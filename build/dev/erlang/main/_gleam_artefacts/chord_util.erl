-module(chord_util).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/chord_util.gleam").
-export([hash_id/2, finger_start/3, rand_key/1, in_interval/4, in_interval_exclusive/4]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/chord_util.gleam", 13).
?DOC(" Hash an integer into the identifier space [0, 2^m)\n").
-spec hash_id(integer(), integer()) -> integer().
hash_id(X, M) ->
    Size = chord_math:ipow(2, M),
    case Size of
        0 -> 0;
        Gleam@denominator -> gleam@int:absolute_value(X) rem Gleam@denominator
    end.

-file("src/chord_util.gleam", 19).
?DOC(" Compute the start of the i-th finger for node n\n").
-spec finger_start(integer(), integer(), integer()) -> integer().
finger_start(N, I, M) ->
    Size_val = chord_math:ipow(2, M),
    Base_offset = chord_math:ipow(2, I - 1),
    Offset = case Size_val of
        0 -> 0;
        Gleam@denominator -> (N + Base_offset) rem Gleam@denominator
    end,
    Offset.

-file("src/chord_util.gleam", 27).
?DOC(" Pick a random identifier in [0, 2^m)\n").
-spec rand_key(integer()) -> integer().
rand_key(M) ->
    Size = chord_math:ipow(2, M),
    gleam@int:random(Size).

-file("src/chord_util.gleam", 34).
?DOC(" Interval check: inclusive (start, end]\n").
-spec in_interval(integer(), integer(), integer(), integer()) -> boolean().
in_interval(Id, Start, End_, _) ->
    case Start < End_ of
        true ->
            (Id > Start) andalso (Id =< End_);

        false ->
            (Id > Start) orelse (Id =< End_)
    end.

-file("src/chord_util.gleam", 42).
?DOC(" Interval check: exclusive (start, end)\n").
-spec in_interval_exclusive(integer(), integer(), integer(), integer()) -> boolean().
in_interval_exclusive(Id, Start, End_, _) ->
    case Start < End_ of
        true ->
            (Id > Start) andalso (Id < End_);

        false ->
            (Id > Start) orelse (Id < End_)
    end.
