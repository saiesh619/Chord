-record(state, {
    total_hops :: integer(),
    noise_hops :: integer(),
    completed_nodes :: integer(),
    num_nodes :: integer(),
    num_reqs :: integer(),
    main :: gleam@erlang@process:subject(chord_stats:msg()),
    in_user :: boolean()
}).
