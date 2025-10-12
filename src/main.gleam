import gleam/erlang/process
import gleam/float
import gleam/int
import gleam/io
import gleam/otp/actor

import chord_stats
import chord_supervisor

pub fn main() {
  let num_nodes = 2
  let num_reqs = 1

  io.println("Starting Chord P2P simulation...")
  io.println(
    "Nodes: "
    <> int.to_string(num_nodes)
    <> " | Requests per node: "
    <> int.to_string(num_reqs),
  )

  // mailbox for main
  let self = process.new_subject()

  // start stats actor
  let stats_builder =
    actor.new(chord_stats.init(num_nodes, num_reqs, self))
    |> actor.on_message(chord_stats.update)

  let stats_pid = case actor.start(stats_builder) {
    Ok(started) -> {
      io.println("Stats actor started.")
      started.data
    }
    Error(_) -> panic as "Could not start stats actor"
  }

  // start supervisor
  let sup = chord_supervisor.loop(num_nodes, num_reqs, stats_pid)

  // 🔥 kick things off (supervisor will flip StartUserPhase at the right time)
  actor.send(sup, chord_supervisor.Start)

  // wait for result
  case process.receive(self, 30_000) {
    Ok(chord_stats.Finished(avg)) -> {
      io.println("Average hops: " <> float.to_string(avg))
      io.println("Simulation complete.")
    }
    Ok(_) -> io.println("Got unexpected message, ignoring…")
    Error(_) -> io.println("Timed out waiting for simulation.")
  }
}
