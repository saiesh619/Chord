import gleam/erlang/process
import gleam/float
import gleam/int
import gleam/io
import gleam/otp/actor

import chord_stats
import chord_supervisor

pub fn main() {
  let num_nodes = 5
  let num_reqs = 1

  io.println("Starting Chord P2P simulation...")
  io.println(
    "Nodes: "
    <> int.to_string(num_nodes)
    <> " | Requests per node: "
    <> int.to_string(num_reqs),
  )

  let self = process.new_subject()

  let stats_builder =
    actor.new(chord_stats.init(num_nodes, num_reqs, self))
    |> actor.on_message(chord_stats.update)

  let stats_pid = case actor.start(stats_builder) {
    Ok(started) -> started.data
    Error(_) -> panic as "Could not start stats actor"
  }

  let _sup = chord_supervisor.loop(num_nodes, num_reqs, stats_pid)

  case process.receive(self, 30_000) {
    Ok(chord_stats.Finished(avg)) -> {
      io.println("Average hops: " <> float.to_string(avg))
      io.println("Simulation complete.")
    }
    Ok(chord_stats.HopCount(_)) -> {
      io.println("Got HopCount, ignoring…")
    }
    Ok(chord_stats.RequestDone) -> {
      io.println("Got RequestDone, ignoring…")
    }
    Error(_) -> io.println("Timed out waiting for simulation.")
  }
}
