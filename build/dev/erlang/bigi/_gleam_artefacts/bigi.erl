-module(bigi).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([zero/0, from_int/1, from_string/1, from_bytes/3, to_int/1, to_string/1, to_bytes/4, compare/2, absolute/1, negate/1, add/2, subtract/2, multiply/2, divide/2, divide_no_zero/2, remainder/2, floor_divide/2, remainder_no_zero/2, modulo/2, modulo_no_zero/2, power/2, decode/1, bitwise_and/2, bitwise_exclusive_or/2, bitwise_not/1, bitwise_or/2, bitwise_shift_left/2, bitwise_shift_right/2, is_odd/1, max/2, min/2, clamp/3, sum/1, product/1, undigits/2, digits/1]).
-export_type([big_int/0, endianness/0, signedness/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type big_int() :: any().

-type endianness() :: little_endian | big_endian.

-type signedness() :: signed | unsigned.

-file("src/bigi.gleam", 26).
?DOC(" Create a big integer representing zero.\n").
-spec zero() -> big_int().
zero() ->
    bigi_ffi:zero().

-file("src/bigi.gleam", 39).
?DOC(
    " Create a big integer from a regular integer.\n"
    "\n"
    " Note that in the JavaScript target, if your integer is bigger than the\n"
    " [maximum safe integer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MAX_SAFE_INTEGER)\n"
    " or smaller than the\n"
    " [minimum safe integer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MIN_SAFE_INTEGER),\n"
    " you may lose precision when operating on it, including when converting it\n"
    " into a big integer (as the JavaScript Number type has already reduced the\n"
    " precision of the value).\n"
).
-spec from_int(integer()) -> big_int().
from_int(Int) ->
    bigi_ffi:from(Int).

-file("src/bigi.gleam", 47).
?DOC(
    " Convert a string into a big integer.\n"
    "\n"
    " If the string does not represent a big integer in base 10, an error is\n"
    " returned. Trailing non-digit content is not allowed.\n"
).
-spec from_string(binary()) -> {ok, big_int()} | {error, nil}.
from_string(Str) ->
    bigi_ffi:from_string(Str).

-file("src/bigi.gleam", 55).
?DOC(
    " Convert raw bytes into a big integer.\n"
    " \n"
    " If the bit array does not contain a whole number of bytes then an error is\n"
    " returned.\n"
).
-spec from_bytes(bitstring(), endianness(), signedness()) -> {ok, big_int()} |
    {error, nil}.
from_bytes(Bytes, Endianness, Signedness) ->
    bigi_ffi:from_bytes(Bytes, Endianness, Signedness).

-file("src/bigi.gleam", 70).
?DOC(
    " Convert a big integer to a regular integer.\n"
    "\n"
    " In Erlang, this cannot fail, as all Erlang integers are big integers. In the\n"
    " JavaScript target, this will fail if the integer is bigger than the\n"
    " [maximum safe integer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MAX_SAFE_INTEGER)\n"
    " or smaller than the\n"
    " [minimum safe integer](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MIN_SAFE_INTEGER).\n"
).
-spec to_int(big_int()) -> {ok, integer()} | {error, nil}.
to_int(Bigint) ->
    bigi_ffi:to(Bigint).

-file("src/bigi.gleam", 75).
?DOC(" Convert the big integer into a simple string - a sequence of digits.\n").
-spec to_string(big_int()) -> binary().
to_string(Bigint) ->
    erlang:integer_to_binary(Bigint).

-file("src/bigi.gleam", 84).
?DOC(
    " Convert a big integer to raw bytes.\n"
    " \n"
    " The size of the returned bit array is specified by `byte_count`, e.g. 8 will\n"
    " return a bit array containing 8 bytes (64 bits). If the big integer doesn't\n"
    " fit in the specified number of bytes then an error is returned.\n"
).
-spec to_bytes(big_int(), endianness(), signedness(), integer()) -> {ok,
        bitstring()} |
    {error, nil}.
to_bytes(Bigint, Endianness, Signedness, Byte_count) ->
    bigi_ffi:to_bytes(Bigint, Endianness, Signedness, Byte_count).

-file("src/bigi.gleam", 95).
?DOC(
    " Compare two big integers, returning an order that denotes if `a` is lower,\n"
    " bigger than, or equal to `b`.\n"
).
-spec compare(big_int(), big_int()) -> gleam@order:order().
compare(A, B) ->
    bigi_ffi:compare(A, B).

-file("src/bigi.gleam", 100).
?DOC(" Get the absolute value of a big integer.\n").
-spec absolute(big_int()) -> big_int().
absolute(Bigint) ->
    erlang:abs(Bigint).

-file("src/bigi.gleam", 105).
?DOC(" Returns the negative of the value provided.\n").
-spec negate(big_int()) -> big_int().
negate(Bigint) ->
    bigi_ffi:negate(Bigint).

-file("src/bigi.gleam", 110).
?DOC(" Add two big integers together.\n").
-spec add(big_int(), big_int()) -> big_int().
add(A, B) ->
    bigi_ffi:add(A, B).

-file("src/bigi.gleam", 115).
?DOC(" Subtract the subtrahend from the minuend.\n").
-spec subtract(big_int(), big_int()) -> big_int().
subtract(A, B) ->
    bigi_ffi:subtract(A, B).

-file("src/bigi.gleam", 120).
?DOC(" Multiply two big integers together.\n").
-spec multiply(big_int(), big_int()) -> big_int().
multiply(A, B) ->
    bigi_ffi:multiply(A, B).

-file("src/bigi.gleam", 127).
?DOC(
    " Divide the dividend with the divisor using integer division.\n"
    "\n"
    " Follows the standard Gleam divide-by-zero rule of 0 when the divisor is 0.\n"
).
-spec divide(big_int(), big_int()) -> big_int().
divide(A, B) ->
    bigi_ffi:divide(A, B).

-file("src/bigi.gleam", 160).
?DOC(
    " Divide the dividend with the divisor using integer division.\n"
    "\n"
    " Returns an error if the divisor is 0.\n"
).
-spec divide_no_zero(big_int(), big_int()) -> {ok, big_int()} | {error, nil}.
divide_no_zero(A, B) ->
    bigi_ffi:divide_no_zero(A, B).

-file("src/bigi.gleam", 171).
?DOC(
    " Divide the dividend with the divisor using integer division and return the\n"
    " remainder.\n"
    "\n"
    " Follows the standard Gleam divide-by-zero rule of 0 when the divisor is 0.\n"
).
-spec remainder(big_int(), big_int()) -> big_int().
remainder(A, B) ->
    bigi_ffi:remainder(A, B).

-file("src/bigi.gleam", 136).
?DOC(
    " Performs a *floored* integer division, which means that the result will\n"
    " always be rounded towards negative infinity.\n"
    "\n"
    " If you want to perform truncated integer division (rounding towards zero),\n"
    " use `divide` or `divide_no_zero` instead.\n"
    "\n"
    " Returns an error if the divisor is 0.\n"
).
-spec floor_divide(big_int(), big_int()) -> {ok, big_int()} | {error, nil}.
floor_divide(Dividend, Divisor) ->
    Z = bigi_ffi:zero(),
    case Divisor =:= Z of
        true ->
            {error, nil};

        false ->
            case bigi_ffi:compare(bigi_ffi:multiply(Dividend, Divisor), Z) of
                lt ->
                    case bigi_ffi:remainder(Dividend, Divisor) /= Z of
                        true ->
                            {ok,
                                bigi_ffi:subtract(
                                    bigi_ffi:divide(Dividend, Divisor),
                                    bigi_ffi:from(1)
                                )};

                        false ->
                            {ok, bigi_ffi:divide(Dividend, Divisor)}
                    end;

                _ ->
                    {ok, bigi_ffi:divide(Dividend, Divisor)}
            end
    end.

-file("src/bigi.gleam", 179).
?DOC(
    " Divide the dividend with the divisor using integer division and return the\n"
    " remainder.\n"
    "\n"
    " Returns an error if the divisor is 0.\n"
).
-spec remainder_no_zero(big_int(), big_int()) -> {ok, big_int()} | {error, nil}.
remainder_no_zero(A, B) ->
    bigi_ffi:remainder_no_zero(A, B).

-file("src/bigi.gleam", 189).
?DOC(
    " Calculate a mathematical modulo operation.\n"
    "\n"
    " Follows the standard Gleam divide-by-zero rule of 0 when the divisor is 0.\n"
).
-spec modulo(big_int(), big_int()) -> big_int().
modulo(A, B) ->
    bigi_ffi:modulo(A, B).

-file("src/bigi.gleam", 196).
?DOC(
    " Calculate a mathematical modulo operation.\n"
    "\n"
    " Returns an error if the divisor is 0.\n"
).
-spec modulo_no_zero(big_int(), big_int()) -> {ok, big_int()} | {error, nil}.
modulo_no_zero(A, B) ->
    bigi_ffi:modulo_no_zero(A, B).

-file("src/bigi.gleam", 206).
?DOC(
    " Raise the base to the exponent.\n"
    "\n"
    " If the exponent is negative, an error is returned.\n"
).
-spec power(big_int(), big_int()) -> {ok, big_int()} | {error, nil}.
power(A, B) ->
    bigi_ffi:power(A, B).

-file("src/bigi.gleam", 219).
?DOC(" Decode a `gleam/dynamic` value into a big integer, if possible.\n").
-spec decode(gleam@dynamic:dynamic_()) -> {ok, big_int()} |
    {error, list(gleam@dynamic@decode:decode_error())}.
decode(Dyn) ->
    bigi_ffi:decode(Dyn).

-file("src/bigi.gleam", 224).
?DOC(" Calculates the bitwise AND of its arguments.\n").
-spec bitwise_and(big_int(), big_int()) -> big_int().
bitwise_and(A, B) ->
    bigi_ffi:bitwise_and(A, B).

-file("src/bigi.gleam", 229).
?DOC(" Calculates the bitwise XOR of its arguments.\n").
-spec bitwise_exclusive_or(big_int(), big_int()) -> big_int().
bitwise_exclusive_or(A, B) ->
    bigi_ffi:bitwise_exclusive_or(A, B).

-file("src/bigi.gleam", 234).
?DOC(" Calculates the bitwise NOT of its argument.\n").
-spec bitwise_not(big_int()) -> big_int().
bitwise_not(Bigint) ->
    bigi_ffi:bitwise_not(Bigint).

-file("src/bigi.gleam", 239).
?DOC(" Calculates the bitwise OR of its arguments.\n").
-spec bitwise_or(big_int(), big_int()) -> big_int().
bitwise_or(A, B) ->
    bigi_ffi:bitwise_or(A, B).

-file("src/bigi.gleam", 244).
?DOC(" Calculates the result of an arithmetic left bitshift by the given amount.\n").
-spec bitwise_shift_left(big_int(), integer()) -> big_int().
bitwise_shift_left(Bigint, Amount) ->
    bigi_ffi:bitwise_shift_left(Bigint, Amount).

-file("src/bigi.gleam", 249).
?DOC(" Calculates the result of an arithmetic right bitshift by the given amount.\n").
-spec bitwise_shift_right(big_int(), integer()) -> big_int().
bitwise_shift_right(Bigint, Amount) ->
    bigi_ffi:bitwise_shift_right(Bigint, Amount).

-file("src/bigi.gleam", 259).
?DOC(" Returns whether the big integer provided is odd.\n").
-spec is_odd(big_int()) -> boolean().
is_odd(Bigint) ->
    bigi_ffi:remainder(Bigint, bigi_ffi:from(2)) /= bigi_ffi:zero().

-file("src/bigi.gleam", 264).
?DOC(" Compares two big integers, returning the larger of the two.\n").
-spec max(big_int(), big_int()) -> big_int().
max(A, B) ->
    case bigi_ffi:compare(A, B) of
        lt ->
            B;

        _ ->
            A
    end.

-file("src/bigi.gleam", 272).
?DOC(" Compares two big integers, returning the smaller of the two.\n").
-spec min(big_int(), big_int()) -> big_int().
min(A, B) ->
    case bigi_ffi:compare(A, B) of
        lt ->
            A;

        _ ->
            B
    end.

-file("src/bigi.gleam", 252).
?DOC(" Restricts a big integer between a lower and upper bound.\n").
-spec clamp(big_int(), big_int(), big_int()) -> big_int().
clamp(Bigint, Min_bound, Max_bound) ->
    _pipe = Bigint,
    _pipe@1 = min(_pipe, Max_bound),
    max(_pipe@1, Min_bound).

-file("src/bigi.gleam", 282).
?DOC(
    " Sums a list of big integers.\n"
    "\n"
    " Returns 0 if the list was empty.\n"
).
-spec sum(list(big_int())) -> big_int().
sum(Bigints) ->
    gleam@list:fold(Bigints, bigi_ffi:zero(), fun bigi_ffi:add/2).

-file("src/bigi.gleam", 289).
?DOC(
    " Multiplies a list of big integers.\n"
    "\n"
    " Returns 1 if the list was empty.\n"
).
-spec product(list(big_int())) -> big_int().
product(Bigints) ->
    gleam@list:fold(Bigints, bigi_ffi:from(1), fun bigi_ffi:multiply/2).

-file("src/bigi.gleam", 296).
?DOC(
    " Joins a list of digits into a single value. Returns an error if the base is\n"
    " less than 2 or if the list contains a digit greater than or equal to the\n"
    " specified base.\n"
).
-spec undigits(list(integer()), integer()) -> {ok, big_int()} | {error, nil}.
undigits(Digits, Base) ->
    case Base < 2 of
        true ->
            {error, nil};

        false ->
            Base@1 = bigi_ffi:from(Base),
            gleam@list:try_fold(
                Digits,
                bigi_ffi:zero(),
                fun(Acc, Digit) ->
                    Digit@1 = bigi_ffi:from(Digit),
                    case bigi_ffi:compare(Digit@1, Base@1) of
                        gt ->
                            {error, nil};

                        eq ->
                            {error, nil};

                        _ ->
                            {ok,
                                bigi_ffi:add(
                                    bigi_ffi:multiply(Acc, Base@1),
                                    Digit@1
                                )}
                    end
                end
            )
    end.

-file("src/bigi.gleam", 312).
-spec get_digit(big_int(), list(integer()), big_int()) -> list(integer()).
get_digit(Bigint, Digits, Divisor) ->
    case bigi_ffi:compare(Bigint, Divisor) of
        lt ->
            _assert_subject = bigi_ffi:to(Bigint),
            {ok, Digit} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"bigi"/utf8>>,
                                function => <<"get_digit"/utf8>>,
                                line => 315})
            end,
            [Digit | Digits];

        _ ->
            _assert_subject@1 = begin
                _pipe = bigi_ffi:remainder(Bigint, Divisor),
                bigi_ffi:to(_pipe)
            end,
            {ok, Digit@1} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@1,
                                module => <<"bigi"/utf8>>,
                                function => <<"get_digit"/utf8>>,
                                line => 319})
            end,
            Digits@1 = [Digit@1 | Digits],
            get_digit(bigi_ffi:divide(Bigint, Divisor), Digits@1, Divisor)
    end.

-file("src/bigi.gleam", 211).
?DOC(
    " Get the digits in a given bigint as a list of integers in base 10.\n"
    "\n"
    " The list is ordered starting from the most significant digit.\n"
).
-spec digits(big_int()) -> list(integer()).
digits(Bigint) ->
    Divisor = bigi_ffi:from(10),
    get_digit(Bigint, [], Divisor).
