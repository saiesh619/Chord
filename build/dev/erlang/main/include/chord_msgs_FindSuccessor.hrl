-record(find_successor, {
    key :: integer(),
    reply_to :: gleam@erlang@process:subject(chord_msgs:msg()),
    hops :: integer()
}).
