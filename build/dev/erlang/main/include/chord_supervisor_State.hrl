-record(state, {
    num_nodes :: integer(),
    num_reqs :: integer(),
    stats :: gleam@erlang@process:subject(chord_stats:msg()),
    nodes :: list(gleam@erlang@process:subject(chord_msgs:msg())),
    m :: integer(),
    self :: gleam@erlang@process:subject(chord_supervisor:msg())
}).
