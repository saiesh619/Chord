-module(chord_stats).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/chord_stats.gleam").
-export([init/3, update/2]).
-export_type([msg/0, state/0]).

-type msg() :: {hop_count, integer()} | request_done | {finished, float()}.

-type state() :: {state,
        integer(),
        integer(),
        integer(),
        integer(),
        gleam@erlang@process:subject(msg())}.

-file("src/chord_stats.gleam", 23).
-spec init(integer(), integer(), gleam@erlang@process:subject(msg())) -> state().
init(Num_nodes, Num_reqs, Main) ->
    {state, 0, 0, Num_nodes, Num_reqs, Main}.

-file("src/chord_stats.gleam", 27).
-spec update(state(), msg()) -> gleam@otp@actor:next(state(), msg()).
update(State, Msg) ->
    case Msg of
        {hop_count, H} ->
            gleam@otp@actor:continue(
                {state,
                    erlang:element(2, State) + H,
                    erlang:element(3, State),
                    erlang:element(4, State),
                    erlang:element(5, State),
                    erlang:element(6, State)}
            );

        request_done ->
            Done2 = erlang:element(3, State) + 1,
            case Done2 =:= erlang:element(4, State) of
                true ->
                    Total = erlang:element(4, State) * erlang:element(5, State),
                    Avg = case Total of
                        0 ->
                            +0.0;

                        _ ->
                            case erlang:float(Total) of
                                +0.0 -> +0.0;
                                -0.0 -> -0.0;
                                Gleam@denominator -> erlang:float(
                                    erlang:element(2, State)
                                )
                                / Gleam@denominator
                            end
                    end,
                    gleam_stdlib:println(
                        <<"All lookups finished. Average hops = "/utf8,
                            (gleam_stdlib:float_to_string(Avg))/binary>>
                    ),
                    gleam@otp@actor:send(
                        erlang:element(6, State),
                        {finished, Avg}
                    ),
                    gleam@otp@actor:continue(
                        {state,
                            erlang:element(2, State),
                            Done2,
                            erlang:element(4, State),
                            erlang:element(5, State),
                            erlang:element(6, State)}
                    );

                false ->
                    gleam@otp@actor:continue(
                        {state,
                            erlang:element(2, State),
                            Done2,
                            erlang:element(4, State),
                            erlang:element(5, State),
                            erlang:element(6, State)}
                    )
            end;

        {finished, _} ->
            gleam@otp@actor:continue(State)
    end.
