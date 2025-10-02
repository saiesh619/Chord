import gleam/erlang/process.{type Subject}
import gleam/float
import gleam/int
import gleam/io
import gleam/otp/actor

pub type Msg {
  HopCount(Int)
  RequestDone
  Finished(Float)
}

pub type State {
  State(
    total_hops: Int,
    completed_nodes: Int,
    num_nodes: Int,
    num_reqs: Int,
    main: Subject(Msg),
  )
}

pub fn init(num_nodes: Int, num_reqs: Int, main: Subject(Msg)) -> State {
  State(0, 0, num_nodes, num_reqs, main)
}

pub fn update(state: State, msg: Msg) -> actor.Next(State, Msg) {
  case msg {
    HopCount(h) ->
      actor.continue(State(
        state.total_hops + h,
        state.completed_nodes,
        state.num_nodes,
        state.num_reqs,
        state.main,
      ))

    RequestDone -> {
      let done2 = state.completed_nodes + 1
      case done2 == state.num_nodes {
        True -> {
          let total = state.num_nodes * state.num_reqs
          let avg = case total {
            0 -> 0.0
            _ -> int.to_float(state.total_hops) /. int.to_float(total)
          }

          io.println(
            "All lookups finished. Average hops = " <> float.to_string(avg),
          )
          actor.send(state.main, Finished(avg))
          actor.continue(State(
            state.total_hops,
            done2,
            state.num_nodes,
            state.num_reqs,
            state.main,
          ))
        }
        False ->
          actor.continue(State(
            state.total_hops,
            done2,
            state.num_nodes,
            state.num_reqs,
            state.main,
          ))
      }
    }

    Finished(_) -> actor.continue(state)
  }
}
