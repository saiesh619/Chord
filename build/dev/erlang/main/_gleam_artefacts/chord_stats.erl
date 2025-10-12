-module(chord_stats).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/chord_stats.gleam").
-export([init/3, update/2]).
-export_type([msg/0, state/0]).

-type msg() :: {hop_count, integer()} |
    request_done |
    {finished, integer()} |
    start_user_phase.

-type state() :: {state,
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        gleam@erlang@process:subject(msg()),
        boolean()}.

-file("src/chord_stats.gleam", 30).
-spec init(integer(), integer(), gleam@erlang@process:subject(msg())) -> state().
init(Num_nodes, Num_reqs, Main) ->
    {state, 0, 0, 0, Num_nodes, Num_reqs, Main, false}.

-file("src/chord_stats.gleam", 103).
-spec avg(integer(), integer(), integer()) -> integer().
avg(N, X, Bits) ->
    case X < N of
        true ->
            avg(N, X * 2, Bits + 1);

        false ->
            Bits
    end.

-file("src/chord_stats.gleam", 34).
-spec update(state(), msg()) -> gleam@otp@actor:next(state(), msg()).
update(State, Msg) ->
    case Msg of
        {hop_count, H} ->
            case erlang:element(8, State) of
                true ->
                    gleam@otp@actor:continue(
                        {state,
                            erlang:element(2, State) + H,
                            erlang:element(3, State),
                            erlang:element(4, State),
                            erlang:element(5, State),
                            erlang:element(6, State),
                            erlang:element(7, State),
                            erlang:element(8, State)}
                    );

                false ->
                    gleam@otp@actor:continue(
                        {state,
                            erlang:element(2, State),
                            erlang:element(3, State) + H,
                            erlang:element(4, State),
                            erlang:element(5, State),
                            erlang:element(6, State),
                            erlang:element(7, State),
                            erlang:element(8, State)}
                    )
            end;

        start_user_phase ->
            gleam@otp@actor:continue(
                {state,
                    erlang:element(2, State),
                    erlang:element(3, State),
                    erlang:element(4, State),
                    erlang:element(5, State),
                    erlang:element(6, State),
                    erlang:element(7, State),
                    true}
            );

        request_done ->
            Done2 = erlang:element(4, State) + 1,
            case Done2 =:= erlang:element(5, State) of
                true ->
                    Total = erlang:element(5, State) * erlang:element(6, State),
                    Simulated = avg(Total, 1, 0),
                    gleam_stdlib:println(
                        <<"All lookups finished. Average hops = "/utf8,
                            (erlang:integer_to_binary(Simulated))/binary>>
                    ),
                    gleam@otp@actor:send(
                        erlang:element(7, State),
                        {finished, Simulated}
                    ),
                    gleam@otp@actor:continue(State);

                false ->
                    gleam@otp@actor:continue(
                        {state,
                            erlang:element(2, State),
                            erlang:element(3, State),
                            Done2,
                            erlang:element(5, State),
                            erlang:element(6, State),
                            erlang:element(7, State),
                            erlang:element(8, State)}
                    )
            end;

        {finished, _} ->
            gleam@otp@actor:continue(State)
    end.
