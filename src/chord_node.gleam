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
import gleam/list
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
  // bootstrap: successor = self; precompute empty finger starts
  let starts =
    list.map(list.range(1, m), fn(i) { chord_util.finger_start(id, i, m) })
  let empty_fingers = list.map(starts, fn(s) { Finger(s, me, id) })
  State(id, m, me, stats, me, id, None, None, empty_fingers, dict.new(), 0)
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
          actor.send(
            reply_to,
            FoundSuccessor(key, state.self, state.id, hops + 1),
          )
          actor.continue(state)
        }
        False -> {
          let next = closest_preceding(state, key)
          actor.send(next, FindSuccessor(key, reply_to, hops + 1))
          actor.continue(state)
        }
      }
    }

    // NOTE: FoundSuccessor now carries succ_id
    FoundSuccessor(key, succ_ref, succ_id, hops) -> {
      case key == state.id {
        True -> {
          // adopt successor + id, then notify
          actor.send(succ_ref, Notify(state.self, state.id))
          let st2 =
            State(
              state.id,
              state.m,
              state.self,
              state.stats,
              succ_ref,
              succ_id,
              state.pred,
              state.pred_id,
              state.fingers,
              state.kv,
              state.pending,
            )
          actor.continue(st2)
        }

        False -> {
          // if this was for a finger-start, update that finger
          let st2 = case finger_index_for_key(state, key) {
            Some(idx) -> {
              let new_fingers =
                upsert_finger(
                  state.fingers,
                  idx,
                  Finger(key, succ_ref, succ_id),
                )
              State(
                state.id,
                state.m,
                state.self,
                state.stats,
                state.succ,
                state.succ_id,
                state.pred,
                state.pred_id,
                new_fingers,
                state.kv,
                state.pending,
              )
            }
            None -> state
          }

          actor.send(state.stats, chord_stats.HopCount(hops))
          actor.continue(st2)
        }
      }
    }

    // Route KV via lookup path (still stub)
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

    // ----- periodic maintenance -----
    FixFingers(i) -> {
      let start = chord_util.finger_start(state.id, i, state.m)
      actor.send(state.self, FindSuccessor(start, state.self, 0))

      let next_i = case i < state.m {
        True -> i + 1
        False -> 1
      }

      process.send_after(state.self, 500, FixFingers(next_i))
      actor.continue(state)
    }

    CheckPredecessor -> {
      // lightweight liveness probe; if predecessor vanished, clear it
      case state.pred {
        Some(p) -> actor.send(p, GetPredecessor(state.self))
        None -> Nil
      }
      process.send_after(state.self, 700, CheckPredecessor)
      actor.continue(state)
    }
  }
}

// ---------- helpers ----------
fn schedule_maintenance(state: State) -> actor.Next(State, Msg) {
  process.send_after(state.self, 300, Stabilize)
  process.send_after(state.self, 500, FixFingers(1))
  process.send_after(state.self, 700, CheckPredecessor)
  actor.continue(state)
}

fn finger_index_for_key(state: State, key: Int) -> Option(Int) {
  let starts =
    list.map(list.range(1, state.m), fn(i) {
      chord_util.finger_start(state.id, i, state.m)
    })
  let paired = list.zip(list.range(1, state.m), starts)

  case
    list.find(paired, fn(t) {
      let #(i, s) = t
      s == key
    })
  {
    Ok(#(i, _)) -> Some(i)
    Error(_) -> None
  }
}

fn upsert_finger(fingers: List(Finger), idx: Int, f: Finger) -> List(Finger) {
  // idx is 1-based
  list.index_map(fingers, fn(old, i) {
    case i + 1 == idx {
      True -> f
      False -> old
    }
  })
}

fn closest_preceding(state: State, key: Int) -> NodeRef {
  let rev = list.reverse(state.fingers)

  case
    list.find(rev, fn(finger) {
      case finger {
        Finger(_, _, node_id) ->
          chord_util.in_interval_exclusive(node_id, state.id, key, state.m)
      }
    })
  {
    Ok(Finger(_, node, _)) -> node
    Error(_) -> state.succ
  }
}

// ---------- spawner ----------
pub fn start(id: Int, m: Int, stats: Subject(chord_stats.Msg)) -> NodeRef {
  let builder =
    actor.new_with_initialiser(1000, fn(me: NodeRef) {
      let st = init(id, m, stats, me)
      actor.initialised(st)
      |> actor.returning(me)
      |> Ok
    })
    |> actor.on_message(update)

  case actor.start(builder) {
    Ok(started) -> started.data
    Error(_) -> panic as "Failed to start Chord node"
  }
}
