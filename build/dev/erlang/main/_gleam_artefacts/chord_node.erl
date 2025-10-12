-module(chord_node).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/chord_node.gleam").
-export([init/4, update/2, start/3]).
-export_type([finger/0, state/0]).

-type finger() :: {finger,
        integer(),
        gleam@erlang@process:subject(chord_msgs:msg()),
        integer()}.

-type state() :: {state,
        integer(),
        integer(),
        gleam@erlang@process:subject(chord_msgs:msg()),
        gleam@erlang@process:subject(chord_stats:msg()),
        gleam@erlang@process:subject(chord_msgs:msg()),
        integer(),
        gleam@option:option(gleam@erlang@process:subject(chord_msgs:msg())),
        gleam@option:option(integer()),
        list(finger()),
        gleam@dict:dict(integer(), binary()),
        integer()}.

-file("src/chord_node.gleam", 38).
-spec init(
    integer(),
    integer(),
    gleam@erlang@process:subject(chord_stats:msg()),
    gleam@erlang@process:subject(chord_msgs:msg())
) -> state().
init(Id, M, Stats, Me) ->
    Starts = gleam@list:map(
        gleam@list:range(1, M),
        fun(I) -> chord_util:finger_start(Id, I, M) end
    ),
    Empty_fingers = gleam@list:map(Starts, fun(S) -> {finger, S, Me, Id} end),
    {state, Id, M, Me, Stats, Me, Id, none, none, Empty_fingers, maps:new(), 0}.

-file("src/chord_node.gleam", 300).
-spec schedule_maintenance(state()) -> gleam@otp@actor:next(state(), chord_msgs:msg()).
schedule_maintenance(State) ->
    gleam@erlang@process:send_after(erlang:element(4, State), 300, stabilize),
    gleam@erlang@process:send_after(
        erlang:element(4, State),
        500,
        {fix_fingers, 1}
    ),
    gleam@erlang@process:send_after(
        erlang:element(4, State),
        700,
        check_predecessor
    ),
    gleam@otp@actor:continue(State).

-file("src/chord_node.gleam", 307).
-spec finger_index_for_key(state(), integer()) -> gleam@option:option(integer()).
finger_index_for_key(State, Key) ->
    Starts = gleam@list:map(
        gleam@list:range(1, erlang:element(3, State)),
        fun(I) ->
            chord_util:finger_start(
                erlang:element(2, State),
                I,
                erlang:element(3, State)
            )
        end
    ),
    Paired = gleam@list:zip(
        gleam@list:range(1, erlang:element(3, State)),
        Starts
    ),
    case gleam@list:find(
        Paired,
        fun(T) ->
            {I@1, S} = T,
            S =:= Key
        end
    ) of
        {ok, {I@2, _}} ->
            {some, I@2};

        {error, _} ->
            none
    end.

-file("src/chord_node.gleam", 325).
-spec upsert_finger(list(finger()), integer(), finger()) -> list(finger()).
upsert_finger(Fingers, Idx, F) ->
    gleam@list:index_map(Fingers, fun(Old, I) -> case (I + 1) =:= Idx of
                true ->
                    F;

                false ->
                    Old
            end end).

-file("src/chord_node.gleam", 335).
-spec closest_preceding(state(), integer()) -> gleam@erlang@process:subject(chord_msgs:msg()).
closest_preceding(State, Key) ->
    Rev = lists:reverse(erlang:element(10, State)),
    case gleam@list:find(Rev, fun(Finger) -> case Finger of
                {finger, _, _, Node_id} ->
                    chord_util:in_interval_exclusive(
                        Node_id,
                        erlang:element(2, State),
                        Key,
                        erlang:element(3, State)
                    )
            end end) of
        {ok, {finger, _, Node, _}} ->
            Node;

        {error, _} ->
            erlang:element(6, State)
    end.

-file("src/chord_node.gleam", 52).
-spec update(state(), chord_msgs:msg()) -> gleam@otp@actor:next(state(), chord_msgs:msg()).
update(State, Msg) ->
    case Msg of
        {join_ring, Bootstrap} ->
            case Bootstrap =:= erlang:element(4, State) of
                true ->
                    schedule_maintenance(State);

                false ->
                    gleam@otp@actor:send(
                        Bootstrap,
                        {find_successor,
                            erlang:element(2, State),
                            erlang:element(4, State),
                            0}
                    ),
                    schedule_maintenance(State)
            end;

        {find_successor, Key, Reply_to, Hops} ->
            Pred_id = gleam@option:unwrap(
                erlang:element(9, State),
                erlang:element(2, State)
            ),
            case chord_util:in_interval(
                Key,
                Pred_id,
                erlang:element(2, State),
                erlang:element(3, State)
            ) of
                true ->
                    gleam@otp@actor:send(
                        Reply_to,
                        {found_successor,
                            Key,
                            erlang:element(4, State),
                            erlang:element(2, State),
                            Hops + 1}
                    ),
                    gleam@otp@actor:continue(State);

                false ->
                    Next = closest_preceding(State, Key),
                    gleam@otp@actor:send(
                        Next,
                        {find_successor, Key, Reply_to, Hops + 1}
                    ),
                    gleam@otp@actor:continue(State)
            end;

        {found_successor, Key@1, Succ_ref, Succ_id, Hops@1} ->
            case Key@1 =:= erlang:element(2, State) of
                true ->
                    gleam@otp@actor:send(
                        Succ_ref,
                        {notify,
                            erlang:element(4, State),
                            erlang:element(2, State)}
                    ),
                    St2 = {state,
                        erlang:element(2, State),
                        erlang:element(3, State),
                        erlang:element(4, State),
                        erlang:element(5, State),
                        Succ_ref,
                        Succ_id,
                        erlang:element(8, State),
                        erlang:element(9, State),
                        erlang:element(10, State),
                        erlang:element(11, State),
                        erlang:element(12, State)},
                    gleam@otp@actor:continue(St2);

                false ->
                    St2@1 = case finger_index_for_key(State, Key@1) of
                        {some, Idx} ->
                            New_fingers = upsert_finger(
                                erlang:element(10, State),
                                Idx,
                                {finger, Key@1, Succ_ref, Succ_id}
                            ),
                            {state,
                                erlang:element(2, State),
                                erlang:element(3, State),
                                erlang:element(4, State),
                                erlang:element(5, State),
                                erlang:element(6, State),
                                erlang:element(7, State),
                                erlang:element(8, State),
                                erlang:element(9, State),
                                New_fingers,
                                erlang:element(11, State),
                                erlang:element(12, State)};

                        none ->
                            State
                    end,
                    gleam@otp@actor:send(
                        erlang:element(5, State),
                        {hop_count, Hops@1}
                    ),
                    gleam@otp@actor:continue(St2@1)
            end;

        {lookup_key, Key@2, _} ->
            gleam@otp@actor:send(
                erlang:element(4, State),
                {find_successor, Key@2, erlang:element(4, State), 0}
            ),
            gleam@otp@actor:continue(State);

        {begin_lookups, Num} ->
            gleam@erlang@process:send_after(
                erlang:element(4, State),
                1000,
                tick
            ),
            gleam@otp@actor:continue(
                {state,
                    erlang:element(2, State),
                    erlang:element(3, State),
                    erlang:element(4, State),
                    erlang:element(5, State),
                    erlang:element(6, State),
                    erlang:element(7, State),
                    erlang:element(8, State),
                    erlang:element(9, State),
                    erlang:element(10, State),
                    erlang:element(11, State),
                    Num}
            );

        tick ->
            case erlang:element(12, State) of
                0 ->
                    gleam@otp@actor:send(erlang:element(5, State), request_done),
                    gleam@otp@actor:continue(State);

                N ->
                    Key@3 = chord_util:rand_key(erlang:element(3, State)),
                    gleam@otp@actor:send(
                        erlang:element(4, State),
                        {find_successor, Key@3, erlang:element(4, State), 0}
                    ),
                    gleam@erlang@process:send_after(
                        erlang:element(4, State),
                        1000,
                        tick
                    ),
                    gleam@otp@actor:continue(
                        {state,
                            erlang:element(2, State),
                            erlang:element(3, State),
                            erlang:element(4, State),
                            erlang:element(5, State),
                            erlang:element(6, State),
                            erlang:element(7, State),
                            erlang:element(8, State),
                            erlang:element(9, State),
                            erlang:element(10, State),
                            erlang:element(11, State),
                            N - 1}
                    )
            end;

        stabilize ->
            gleam@otp@actor:send(
                erlang:element(6, State),
                {get_predecessor, erlang:element(4, State)}
            ),
            schedule_maintenance(State);

        {get_predecessor, Reply} ->
            gleam@otp@actor:send(
                Reply,
                {reply_predecessor,
                    erlang:element(8, State),
                    erlang:element(9, State)}
            ),
            gleam@otp@actor:continue(State);

        {reply_predecessor, Pred_opt, Pred_id_opt} ->
            Should_adopt = case Pred_id_opt of
                {some, Xid} ->
                    chord_util:in_interval_exclusive(
                        Xid,
                        erlang:element(2, State),
                        erlang:element(7, State),
                        erlang:element(3, State)
                    );

                none ->
                    false
            end,
            St2@2 = case Should_adopt of
                true ->
                    case Pred_opt of
                        {some, X} ->
                            case Pred_id_opt of
                                {some, Xid@1} ->
                                    {state,
                                        erlang:element(2, State),
                                        erlang:element(3, State),
                                        erlang:element(4, State),
                                        erlang:element(5, State),
                                        X,
                                        Xid@1,
                                        erlang:element(8, State),
                                        erlang:element(9, State),
                                        erlang:element(10, State),
                                        erlang:element(11, State),
                                        erlang:element(12, State)};

                                none ->
                                    State
                            end;

                        none ->
                            State
                    end;

                false ->
                    State
            end,
            gleam@otp@actor:send(
                erlang:element(6, St2@2),
                {notify, erlang:element(4, St2@2), erlang:element(2, St2@2)}
            ),
            gleam@otp@actor:continue(St2@2);

        {notify, N@1, N_id} ->
            Adopt = case erlang:element(9, State) of
                none ->
                    true;

                {some, Pid} ->
                    chord_util:in_interval_exclusive(
                        N_id,
                        Pid,
                        erlang:element(2, State),
                        erlang:element(3, State)
                    )
            end,
            St2@3 = case Adopt of
                true ->
                    {state,
                        erlang:element(2, State),
                        erlang:element(3, State),
                        erlang:element(4, State),
                        erlang:element(5, State),
                        erlang:element(6, State),
                        erlang:element(7, State),
                        {some, N@1},
                        {some, N_id},
                        erlang:element(10, State),
                        erlang:element(11, State),
                        erlang:element(12, State)};

                false ->
                    State
            end,
            gleam@otp@actor:continue(St2@3);

        {fix_fingers, I} ->
            Start = chord_util:finger_start(
                erlang:element(2, State),
                I,
                erlang:element(3, State)
            ),
            gleam@otp@actor:send(
                erlang:element(4, State),
                {find_successor, Start, erlang:element(4, State), 0}
            ),
            Next_i = case I < erlang:element(3, State) of
                true ->
                    I + 1;

                false ->
                    1
            end,
            gleam@erlang@process:send_after(
                erlang:element(4, State),
                500,
                {fix_fingers, Next_i}
            ),
            gleam@otp@actor:continue(State);

        check_predecessor ->
            case erlang:element(8, State) of
                {some, P} ->
                    gleam@otp@actor:send(
                        P,
                        {get_predecessor, erlang:element(4, State)}
                    );

                none ->
                    nil
            end,
            gleam@erlang@process:send_after(
                erlang:element(4, State),
                700,
                check_predecessor
            ),
            gleam@otp@actor:continue(State)
    end.

-file("src/chord_node.gleam", 352).
-spec start(
    integer(),
    integer(),
    gleam@erlang@process:subject(chord_stats:msg())
) -> gleam@erlang@process:subject(chord_msgs:msg()).
start(Id, M, Stats) ->
    Builder = begin
        _pipe@2 = gleam@otp@actor:new_with_initialiser(
            1000,
            fun(Me) ->
                St = init(Id, M, Stats, Me),
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
                    message => <<"Failed to start Chord node"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"chord_node"/utf8>>,
                    function => <<"start"/utf8>>,
                    line => 364})
    end.
