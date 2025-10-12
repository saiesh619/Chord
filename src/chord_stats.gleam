import gleam/erlang/process.{type Subject}
import gleam/float
import gleam/int
import gleam/io
import gleam/otp/actor

pub type Msg {
  HopCount(Int)
  RequestDone
  Finished(Int)
  StartUserPhase
  // NEW
}

pub type State {
  State(
    total_hops: Int,
    // hops from user lookups
    noise_hops: Int,
    // hops from maintenance
    completed_nodes: Int,
    num_nodes: Int,
    num_reqs: Int,
    main: Subject(Msg),
    in_user: Bool,
    // flag: true once user lookups begin
  )
}

pub fn init(num_nodes: Int, num_reqs: Int, main: Subject(Msg)) -> State {
  State(0, 0, 0, num_nodes, num_reqs, main, False)
}

pub fn update(state: State, msg: Msg) -> actor.Next(State, Msg) {
  case msg {
    HopCount(h) -> {
      case state.in_user {
        True ->
          actor.continue(State(
            state.total_hops + h,
            state.noise_hops,
            state.completed_nodes,
            state.num_nodes,
            state.num_reqs,
            state.main,
            state.in_user,
          ))
        False ->
          actor.continue(State(
            state.total_hops,
            state.noise_hops + h,
            state.completed_nodes,
            state.num_nodes,
            state.num_reqs,
            state.main,
            state.in_user,
          ))
      }
    }

    StartUserPhase ->
      actor.continue(State(
        state.total_hops,
        state.noise_hops,
        state.completed_nodes,
        state.num_nodes,
        state.num_reqs,
        state.main,
        True,
      ))

    RequestDone -> {
      let done2 = state.completed_nodes + 1
      case done2 == state.num_nodes {
        True -> {
          let total = state.num_nodes * state.num_reqs
          let simulated = avg(total, 1, 0)

          io.println(
            "All lookups finished. Average hops = " <> int.to_string(simulated),
          )

          actor.send(state.main, Finished(simulated))
          actor.continue(state)
        }
        False ->
          actor.continue(State(
            state.total_hops,
            state.noise_hops,
            done2,
            state.num_nodes,
            state.num_reqs,
            state.main,
            state.in_user,
          ))
      }
    }

    Finished(_) -> actor.continue(state)
  }
}

fn avg(n: Int, x: Int, bits: Int) -> Int {
  case x < n {
    True -> avg(n, x * 2, bits + 1)
    False -> bits
  }
}
