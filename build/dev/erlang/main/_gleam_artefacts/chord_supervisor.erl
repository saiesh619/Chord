-module(chord_supervisor).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/chord_supervisor.gleam").
-export([update/2, ceil_log2/1, init/3, loop/3]).
-export_type([msg/0, state/0]).

-type msg() :: start | all_joined.

-type state() :: {state,
        integer(),
        integer(),
        gleam@erlang@process:subject(chord_stats:msg()),
        list(gleam@erlang@process:subject(chord_msgs:msg())),
        integer(),
        gleam@erlang@process:subject(msg())}.

-file("src/chord_supervisor.gleam", 37).
-spec process_subject_placeholder() -> gleam@erlang@process:subject(msg()).
process_subject_placeholder() ->
    erlang:error(#{gleam_error => panic,
            message => <<"supervisor self not initialised"/utf8>>,
            file => <<?FILEPATH/utf8>>,
            module => <<"chord_supervisor"/utf8>>,
            function => <<"process_subject_placeholder"/utf8>>,
            line => 39}).

-file("src/chord_supervisor.gleam", 42).
-spec update(state(), msg()) -> gleam@otp@actor:next(state(), msg()).
update(State, Msg) ->
    case Msg of
        start ->
            gleam_stdlib:println(
                <<<<"Supervisor: spawning "/utf8,
                        (erlang:integer_to_binary(erlang:element(2, State)))/binary>>/binary,
                    " nodes..."/utf8>>
            ),
            Bootstrap = chord_node:start(
                0,
                erlang:element(6, State),
                erlang:element(4, State)
            ),
            Nodes1 = [Bootstrap],
            Nodes2 = begin
                _pipe = gleam@list:range(1, erlang:element(2, State) - 1),
                gleam@list:fold(
                    _pipe,
                    Nodes1,
                    fun(Acc, I) ->
                        N = chord_node:start(
                            I,
                            erlang:element(6, State),
                            erlang:element(4, State)
                        ),
                        gleam@otp@actor:send(N, {join_ring, Bootstrap}),
                        [N | Acc]
                    end
                )
            end,
            gleam@erlang@process:send_after(
                erlang:element(7, State),
                1200,
                all_joined
            ),
            gleam@otp@actor:continue(
                {state,
                    erlang:element(2, State),
                    erlang:element(3, State),
                    erlang:element(4, State),
                    Nodes2,
                    erlang:element(6, State),
                    erlang:element(7, State)}
            );

        all_joined ->
            gleam_stdlib:println(
                <<"Supervisor: starting lookups on all nodes"/utf8>>
            ),
            gleam@list:each(
                erlang:element(5, State),
                fun(N@1) ->
                    gleam@otp@actor:send(
                        N@1,
                        {begin_lookups, erlang:element(3, State)}
                    )
                end
            ),
            gleam@otp@actor:continue(State)
    end.

-file("src/chord_supervisor.gleam", 111).
-spec ceil_log2_go(integer(), integer(), integer()) -> integer().
ceil_log2_go(N, X, Bits) ->
    case X < N of
        true ->
            ceil_log2_go(N, X * 2, Bits + 1);

        false ->
            Bits
    end.

-file("src/chord_supervisor.gleam", 107).
-spec ceil_log2(integer()) -> integer().
ceil_log2(N) ->
    ceil_log2_go(N, 1, 0).

-file("src/chord_supervisor.gleam", 26).
-spec init(
    integer(),
    integer(),
    gleam@erlang@process:subject(chord_stats:msg())
) -> state().
init(Num_nodes, Num_reqs, Stats) ->
    M_bits = ceil_log2(Num_nodes * 2),
    {state,
        Num_nodes,
        Num_reqs,
        Stats,
        [],
        M_bits,
        process_subject_placeholder()}.

-file("src/chord_supervisor.gleam", 84).
-spec loop(
    integer(),
    integer(),
    gleam@erlang@process:subject(chord_stats:msg())
) -> gleam@erlang@process:subject(msg()).
loop(Num_nodes, Num_reqs, Stats) ->
    Builder = begin
        _pipe@2 = gleam@otp@actor:new_with_initialiser(
            1000,
            fun(Me) ->
                St = {state,
                    Num_nodes,
                    Num_reqs,
                    Stats,
                    [],
                    ceil_log2(Num_nodes * 2),
                    Me},
                _pipe = gleam@otp@actor:initialised(St),
                _pipe@1 = gleam@otp@actor:returning(_pipe, Me),
                {ok, _pipe@1}
            end
        ),
        gleam@otp@actor:on_message(_pipe@2, fun update/2)
    end,
    case gleam@otp@actor:start(Builder) of
        {ok, Started} ->
            erlang:element(3, Started);

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Could not start supervisor"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"chord_supervisor"/utf8>>,
                    function => <<"loop"/utf8>>,
                    line => 103})
    end.
