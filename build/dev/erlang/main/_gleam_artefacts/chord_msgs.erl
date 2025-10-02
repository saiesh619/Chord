-module(chord_msgs).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/chord_msgs.gleam").
-export([ipow/2, finger_start/3]).
-export_type([msg/0]).

-type msg() :: {join_ring, gleam@erlang@process:subject(msg())} |
    {find_successor, integer(), gleam@erlang@process:subject(msg()), integer()} |
    {found_successor, integer(), gleam@erlang@process:subject(msg()), integer()} |
    stabilize |
    {get_predecessor, gleam@erlang@process:subject(msg())} |
    {reply_predecessor,
        gleam@option:option(gleam@erlang@process:subject(msg())),
        gleam@option:option(integer())} |
    {notify, gleam@erlang@process:subject(msg()), integer()} |
    {fix_fingers, integer()} |
    check_predecessor |
    {begin_lookups, integer()} |
    {lookup_key, integer(), integer()} |
    tick.

-file("src/chord_msgs.gleam", 26).
-spec ipow(integer(), integer()) -> integer().
ipow(Base, Exp) ->
    case Exp of
        0 ->
            1;

        _ ->
            Base * ipow(Base, Exp - 1)
    end.

-file("src/chord_msgs.gleam", 33).
-spec finger_start(integer(), integer(), integer()) -> integer().
finger_start(N, I, M) ->
    Size_val = ipow(2, M),
    Base_offset = ipow(2, I - 1),
    _ = case Size_val of
        0 -> 0;
        Gleam@denominator -> Base_offset * N rem Gleam@denominator
    end.
