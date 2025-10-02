-record(found_successor, {
    key :: integer(),
    succ :: gleam@erlang@process:subject(chord_msgs:msg()),
    hops :: integer()
}).
