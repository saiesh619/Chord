-record(finger, {
    start :: integer(),
    node :: gleam@erlang@process:subject(chord_msgs:msg()),
    node_id :: integer()
}).
