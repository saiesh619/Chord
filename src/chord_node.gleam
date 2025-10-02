import chord_msgs.{
  type Msg, type NodeRef, BeginLookups, CheckPredecessor, FindSuccessor,
  FixFingers, FoundSuccessor, GetPredecessor, JoinRing, LookupKey, Notify,
  ReplyPredecessor, Stabilize, Tick,
}
import chord_stats
import chord_util
import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/otp/actor

// ---------- types ----------
pub type Finger {
  Finger(start: Int, node: NodeRef, node_id: Int)
}

pub type State {
  State(
    id: Int,
    m: Int,
    self: NodeRef,
    stats: Subject(chord_stats.Msg),
    succ: NodeRef,
    succ_id: Int,
    pred: Option(NodeRef),
    pred_id: Option(Int),
    fingers: List(Finger),
    kv: dict.Dict(Int, String),
    pending: Int,
  )
}

// ---------- init ----------
pub fn init(
  id: Int,
  m: Int,
  stats: Subject(chord_stats.Msg),
  me: NodeRef,
) -> State {
  // bootstrap: successor = self
  State(id, m, me, stats, me, id, None, None, [], dict.new(), 0)
}

// ---------- update ----------
pub fn update(state: State, msg: Msg) -> actor.Next(State, Msg) {
  case msg {
    // ----- ring join -----
    JoinRing(bootstrap) -> {
      case bootstrap == state.self {
        True -> schedule_maintenance(state)
        False -> {
          actor.send(bootstrap, FindSuccessor(state.id, state.self, 0))
          schedule_maintenance(state)
        }
      }
    }

    // ----- lookups -----
    FindSuccessor(key, reply_to, hops) -> {
      let pred_id = option.unwrap(state.pred_id, state.id)

      case chord_util.in_interval(key, pred_id, state.id, state.m) {
        True -> {
          // I own (pred, self]
          actor.send(reply_to, FoundSuccessor(key, state.self, hops + 1))
          actor.continue(state)
        }
        False -> {
          // Forward to successor (or fingers later)
          let next = closest_preceding(state, key)
          actor.send(next, FindSuccessor(key, reply_to, hops + 1))
          actor.continue(state)
        }
      }
    }

    FoundSuccessor(key, succ_ref, hops) -> {
      case key == state.id {
        True -> {
          // Adopt successor for myself and notify it
          actor.send(succ_ref, Notify(state.self, state.id))
          let st2 =
            State(
              state.id,
              state.m,
              state.self,
              state.stats,
              succ_ref,
              state.succ_id,
              state.pred,
              state.pred_id,
              state.fingers,
              state.kv,
              state.pending,
            )
          actor.continue(st2)
        }
        False -> {
          // Someone else’s lookup completed; record hop count
          actor.send(state.stats, chord_stats.HopCount(hops))
          actor.continue(state)
        }
      }
    }

    // Route KV via lookup path (stub)
    LookupKey(key, _req_id) -> {
      actor.send(state.self, FindSuccessor(key, state.self, 0))
      actor.continue(state)
    }

    // ----- active probing -----
    BeginLookups(num) -> {
      process.send_after(state.self, 1000, Tick)
      actor.continue(State(
        state.id,
        state.m,
        state.self,
        state.stats,
        state.succ,
        state.succ_id,
        state.pred,
        state.pred_id,
        state.fingers,
        state.kv,
        num,
      ))
    }

    Tick -> {
      case state.pending {
        0 -> {
          io.println("Node " <> int.to_string(state.id) <> " finished lookups")
          actor.send(state.stats, chord_stats.RequestDone)
          actor.continue(state)
        }
        n -> {
          let key = chord_util.rand_key(state.m)
          actor.send(state.self, FindSuccessor(key, state.self, 0))
          process.send_after(state.self, 1000, Tick)
          actor.continue(State(
            state.id,
            state.m,
            state.self,
            state.stats,
            state.succ,
            state.succ_id,
            state.pred,
            state.pred_id,
            state.fingers,
            state.kv,
            n - 1,
          ))
        }
      }
    }

    // ----- stabilization -----
    Stabilize -> {
      actor.send(state.succ, GetPredecessor(state.self))
      schedule_maintenance(state)
    }

    GetPredecessor(reply) -> {
      actor.send(reply, ReplyPredecessor(state.pred, state.pred_id))
      actor.continue(state)
    }

    ReplyPredecessor(pred_opt, pred_id_opt) -> {
      let should_adopt = case pred_id_opt {
        Some(xid) ->
          chord_util.in_interval_exclusive(
            xid,
            state.id,
            state.succ_id,
            state.m,
          )
        None -> False
      }

      let st2 = case should_adopt {
        True ->
          case pred_opt {
            Some(x) ->
              case pred_id_opt {
                Some(xid) ->
                  State(
                    state.id,
                    state.m,
                    state.self,
                    state.stats,
                    x,
                    xid,
                    state.pred,
                    state.pred_id,
                    state.fingers,
                    state.kv,
                    state.pending,
                  )
                None -> state
              }
            None -> state
          }
        False -> state
      }

      actor.send(st2.succ, Notify(st2.self, st2.id))
      actor.continue(st2)
    }

    Notify(n, n_id) -> {
      let adopt = case state.pred_id {
        None -> True
        Some(pid) ->
          chord_util.in_interval_exclusive(n_id, pid, state.id, state.m)
      }

      let st2 = case adopt {
        True ->
          State(
            state.id,
            state.m,
            state.self,
            state.stats,
            state.succ,
            state.succ_id,
            Some(n),
            Some(n_id),
            state.fingers,
            state.kv,
            state.pending,
          )
        False -> state
      }

      actor.continue(st2)
    }

    // stubs
    FixFingers(_i) -> actor.continue(state)
    CheckPredecessor -> actor.continue(state)
  }
}

// ---------- helpers ----------
fn schedule_maintenance(state: State) -> actor.Next(State, Msg) {
  process.send_after(state.self, 300, Stabilize)
  process.send_after(state.self, 500, FixFingers(1))
  process.send_after(state.self, 700, CheckPredecessor)
  actor.continue(state)
}

fn closest_preceding(state: State, _key: Int) -> NodeRef {
  // TODO: scan finger table for a closer node; for now successor
  state.succ
}

// ---------- spawner ----------
pub fn start(id: Int, m: Int, stats: Subject(chord_stats.Msg)) -> NodeRef {
  let builder =
    actor.new_with_initialiser(1000, fn(me: NodeRef) {
      let st = init(id, m, stats, me)
      actor.initialised(st)
      |> actor.returning(me)
      // ensure started.data = NodeRef
      |> Ok
    })
    |> actor.on_message(update)

  case actor.start(builder) {
    Ok(started) -> started.data
    // this is NodeRef now
    Error(_) -> panic as "Failed to start Chord node"
  }
}
