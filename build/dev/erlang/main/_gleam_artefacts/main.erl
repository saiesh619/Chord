-module(main).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/main.gleam").
-export([main/0]).

-file("src/main.gleam", 10).
-spec main() -> nil.
main() ->
    Num_nodes = 5,
    Num_reqs = 1,
    gleam_stdlib:println(<<"Starting Chord P2P simulation..."/utf8>>),
    gleam_stdlib:println(
        <<<<<<"Nodes: "/utf8, (erlang:integer_to_binary(Num_nodes))/binary>>/binary,
                " | Requests per node: "/utf8>>/binary,
            (erlang:integer_to_binary(Num_reqs))/binary>>
    ),
    Self = gleam@erlang@process:new_subject(),
    Stats_builder = begin
        _pipe = gleam@otp@actor:new(chord_stats:init(Num_nodes, Num_reqs, Self)),
        gleam@otp@actor:on_message(_pipe, fun chord_stats:update/2)
    end,
    Stats_pid = case gleam@otp@actor:start(Stats_builder) of
        {ok, Started} ->
            erlang:element(3, Started);

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Could not start stats actor"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"main"/utf8>>,
                    function => <<"main"/utf8>>,
                    line => 30})
    end,
    _ = chord_supervisor:loop(Num_nodes, Num_reqs, Stats_pid),
    case gleam@erlang@process:'receive'(Self, 30000) of
        {ok, {finished, Avg}} ->
            gleam_stdlib:println(
                <<"Average hops: "/utf8,
                    (gleam_stdlib:float_to_string(Avg))/binary>>
            ),
            gleam_stdlib:println(<<"Simulation complete."/utf8>>);

        {ok, {hop_count, _}} ->
            gleam_stdlib:println(<<"Got HopCount, ignoring…"/utf8>>);

        {ok, request_done} ->
            gleam_stdlib:println(<<"Got RequestDone, ignoring…"/utf8>>);

        {error, _} ->
            gleam_stdlib:println(<<"Timed out waiting for simulation."/utf8>>)
    end.
