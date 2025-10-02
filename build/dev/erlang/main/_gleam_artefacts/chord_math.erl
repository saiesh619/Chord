-module(chord_math).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/chord_math.gleam").
-export([ipow/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/chord_math.gleam", 2).
?DOC(" Integer exponentiation (safe, recursive)\n").
-spec ipow(integer(), integer()) -> integer().
ipow(Base, Exp) ->
    case Exp of
        0 ->
            1;

        _ ->
            Base * ipow(Base, Exp - 1)
    end.
