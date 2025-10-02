import chord_msgs.{type NodeRef, BeginLookups, JoinRing}
import chord_node
import chord_stats
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/io
import gleam/list
import gleam/otp/actor

pub type Msg {
  Start
  AllJoined
}

pub type State {
  State(
    num_nodes: Int,
    num_reqs: Int,
    stats: Subject(chord_stats.Msg),
    nodes: List(NodeRef),
    m: Int,
    self: Subject(Msg),
  )
}

pub fn init(
  num_nodes: Int,
  num_reqs: Int,
  stats: Subject(chord_stats.Msg),
) -> State {
  let m_bits = ceil_log2(num_nodes * 2)
  // we’ll set self in initialiser below when wiring builder
  State(num_nodes, num_reqs, stats, [], m_bits, process_subject_placeholder())
}

// We need a placeholder; replaced by actor.new_with_initialiser
fn process_subject_placeholder() -> Subject(Msg) {
  // crash if used before replaced
  panic as "supervisor self not initialised"
}

pub fn update(state: State, msg: Msg) -> actor.Next(State, Msg) {
  case msg {
    Start -> {
      io.println(
        "Supervisor: spawning " <> int.to_string(state.num_nodes) <> " nodes...",
      )

      let bootstrap = chord_node.start(0, state.m, state.stats)
      let nodes1 = [bootstrap]

      let nodes2 =
        list.range(1, state.num_nodes - 1)
        |> list.fold(nodes1, fn(acc, i) {
          let n = chord_node.start(i, state.m, state.stats)
          actor.send(n, JoinRing(bootstrap))
          [n, ..acc]
        })

      // Let ring settle a bit, then start lookups
      process.send_after(state.self, 1200, AllJoined)

      actor.continue(State(
        state.num_nodes,
        state.num_reqs,
        state.stats,
        nodes2,
        state.m,
        state.self,
      ))
    }

    AllJoined -> {
      io.println("Supervisor: starting lookups on all nodes")
      list.each(state.nodes, fn(n) {
        actor.send(n, BeginLookups(state.num_reqs))
      })
      actor.continue(state)
    }
  }
}

// builder
pub fn loop(
  num_nodes: Int,
  num_reqs: Int,
  stats: Subject(chord_stats.Msg),
) -> Subject(Msg) {
  let builder =
    actor.new_with_initialiser(1000, fn(me: Subject(Msg)) {
      let st =
        State(num_nodes, num_reqs, stats, [], ceil_log2(num_nodes * 2), me)
      // we want started.data to be the Subject(Msg), so return `me`
      actor.initialised(st)
      |> actor.returning(me)
      |> Ok
    })
    |> actor.on_message(update)

  case actor.start(builder) {
    Ok(started) -> started.data
    // now this is Subject(Msg)
    Error(_) -> panic as "Could not start supervisor"
  }
}

pub fn ceil_log2(n: Int) -> Int {
  ceil_log2_go(n, 1, 0)
}

fn ceil_log2_go(n: Int, x: Int, bits: Int) -> Int {
  case x < n {
    True -> ceil_log2_go(n, x * 2, bits + 1)
    False -> bits
  }
}
