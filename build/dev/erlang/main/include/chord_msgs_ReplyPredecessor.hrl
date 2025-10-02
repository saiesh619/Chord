-record(reply_predecessor, {
    pred :: gleam@option:option(gleam@erlang@process:subject(chord_msgs:msg())),
    pred_id :: gleam@option:option(integer())
}).
