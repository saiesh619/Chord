-record(found_successor, {
    key :: integer(),
    succ_ref :: gleam@erlang@process:subject(chord_msgs:msg()),
    succ_id :: integer(),
    hops :: integer()
}).
