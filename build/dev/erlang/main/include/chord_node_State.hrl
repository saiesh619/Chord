-record(state, {
    id :: integer(),
    m :: integer(),
    self :: gleam@erlang@process:subject(chord_msgs:msg()),
    stats :: gleam@erlang@process:subject(chord_stats:msg()),
    succ :: gleam@erlang@process:subject(chord_msgs:msg()),
    succ_id :: integer(),
    pred :: gleam@option:option(gleam@erlang@process:subject(chord_msgs:msg())),
    pred_id :: gleam@option:option(integer()),
    fingers :: list(chord_node:finger()),
    kv :: gleam@dict:dict(integer(), binary()),
    pending :: integer()
}).
