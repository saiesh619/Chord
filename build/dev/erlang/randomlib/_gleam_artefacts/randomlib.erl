-module(randomlib).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/randomlib.gleam").
-export([next_bool/1, next_byte/1, next_float/1, byte_iterator/1, float_iterator/1, next_bytes/2, next/2, new/0, with_seed/1, next_int/2, choice/2]).
-export_type([random/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-opaque random() :: {random, integer()}.

-file("src/randomlib.gleam", 214).
-spec ffi_to_n_bits(bigi:big_int(), integer()) -> bigi:big_int().
ffi_to_n_bits(Bi, N) ->
    I@1 = case bigi_ffi:to(Bi) of
        {ok, I} -> I;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"randomlib"/utf8>>,
                        function => <<"ffi_to_n_bits"/utf8>>,
                        line => 215,
                        value => _assert_fail,
                        start => 6497,
                        'end' => 6531,
                        pattern_start => 6508,
                        pattern_end => 6513})
    end,
    Pow@1 = case gleam@int:power(2, erlang:float(N)) of
        {ok, Pow} -> Pow;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"randomlib"/utf8>>,
                        function => <<"ffi_to_n_bits"/utf8>>,
                        line => 216,
                        value => _assert_fail@1,
                        start => 6534,
                        'end' => 6584,
                        pattern_start => 6545,
                        pattern_end => 6552})
    end,
    bigi_ffi:from(erlang:'band'(I@1, erlang:trunc(Pow@1) - 1)).

-file("src/randomlib.gleam", 142).
?DOC(
    " Picks a random seed based on a static seed and the current time in nanosecond\n"
    " the long number is chosen from the initial startup value of Java random\n"
    " using the corrected value from L'Ecuyer, \"Tables of Linear Congruential Generators of\n"
    " Different Sizes and Good Lattice Structure\", 1999\n"
).
-spec init_seed() -> integer().
init_seed() ->
    erlang:'bxor'(3447679086515839964, randomlib_ffi:now() * 1000).

-file("src/randomlib.gleam", 150).
-spec get_next_bits(random(), integer()) -> {integer(), random()}.
get_next_bits(Rnd, Bits) ->
    Next_seed@1 = case bigi_ffi:to(
        ffi_to_n_bits(
            bigi_ffi:add(
                ffi_to_n_bits(
                    bigi_ffi:multiply(
                        bigi_ffi:from(erlang:element(2, Rnd)),
                        bigi_ffi:from(25214903917)
                    ),
                    64
                ),
                bigi_ffi:from(11)
            ),
            48
        )
    ) of
        {ok, Next_seed} -> Next_seed;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"randomlib"/utf8>>,
                        function => <<"get_next_bits"/utf8>>,
                        line => 151,
                        value => _assert_fail,
                        start => 4833,
                        'end' => 5084,
                        pattern_start => 4844,
                        pattern_end => 4857})
    end,
    Next_num = erlang:'bsr'(Next_seed@1, 48 - Bits),
    {Next_num, {random, Next_seed@1}}.

-file("src/randomlib.gleam", 44).
?DOC(
    " Returns a tuple containing a uniformly distributed Bool and \n"
    " the updated random seed\n"
).
-spec next_bool(random()) -> {boolean(), random()}.
next_bool(Rnd) ->
    {Val, Rnd@1} = get_next_bits(Rnd, 1),
    {Val =:= 0, Rnd@1}.

-file("src/randomlib.gleam", 51).
?DOC(
    " Returns a tuple containing a byte (0-255) in Int form and the\n"
    " updated random seed\n"
).
-spec next_byte(random()) -> {integer(), random()}.
next_byte(Rnd) ->
    get_next_bits(Rnd, 8).

-file("src/randomlib.gleam", 64).
?DOC(
    " Returns a tuple containing a uniformly distributed float \n"
    " between 0.0 (inclusive) and 1.0 (exclusive) and the updated random seed\n"
).
-spec next_float(random()) -> {float(), random()}.
next_float(Rnd) ->
    {Val, Rnd@1} = get_next_bits(Rnd, 53),
    Unit@1 = case gleam@float:power(2.0, -53.0) of
        {ok, Unit} -> Unit;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"randomlib"/utf8>>,
                        function => <<"next_float"/utf8>>,
                        line => 66,
                        value => _assert_fail,
                        start => 1968,
                        'end' => 2013,
                        pattern_start => 1979,
                        pattern_end => 1987})
    end,
    {erlang:float(Val) * Unit@1, Rnd@1}.

-file("src/randomlib.gleam", 100).
?DOC(
    " Returns an iterated that generates byte in Int form when iterated\n"
    " Note that the random seed is internally updated but there is no ability\n"
    " to extract the updated seed\n"
).
-spec byte_iterator(random()) -> gleam@yielder:yielder(integer()).
byte_iterator(Rnd) ->
    gleam@yielder:unfold(
        Rnd,
        fun(Acc) ->
            {Next, Rnd@1} = next_byte(Acc),
            {next, Next, Rnd@1}
        end
    ).

-file("src/randomlib.gleam", 110).
?DOC(
    " Returns an iterated that generates uniformly distributed Floats when iterated\n"
    " Note that the random seed is internally updated but there is no ability\n"
    " to extract the updated seed\n"
).
-spec float_iterator(random()) -> gleam@yielder:yielder(float()).
float_iterator(Rnd) ->
    gleam@yielder:unfold(
        Rnd,
        fun(Acc) ->
            {Next, Rnd@1} = next_float(Acc),
            {next, Next, Rnd@1}
        end
    ).

-file("src/randomlib.gleam", 166).
-spec get_next_bytes({list(integer()), random()}, integer()) -> {list(integer()),
    random()}.
get_next_bytes(Acc, N) ->
    case gleam@int:compare(N, 0) of
        eq ->
            Acc;

        lt ->
            Acc;

        gt ->
            {L, Rnd} = Acc,
            {Next, Rnd@1} = get_next_bits(Rnd, 8),
            get_next_bytes({[Next | L], Rnd@1}, N - 1)
    end.

-file("src/randomlib.gleam", 58).
?DOC(
    " Returns a tuple containing a list of n bytes in Int form and\n"
    " the updated random seed\n"
    " Passing n <= 0 will return an empty list\n"
).
-spec next_bytes(random(), integer()) -> {list(integer()), random()}.
next_bytes(Rnd, N) ->
    get_next_bytes({[], Rnd}, N).

-file("src/randomlib.gleam", 177).
-spec get_next(integer(), {bigi:big_int(), random()}) -> {bigi:big_int(),
    random()}.
get_next(Bits, Res) ->
    {Bi, Rnd} = Res,
    case Bits > 48 of
        true ->
            {Next, Rnd@1} = get_next_bits(Rnd, 48),
            get_next(
                Bits - 48,
                {bigi_ffi:add(
                        bigi_ffi:bitwise_shift_left(Bi, 48),
                        bigi_ffi:from(Next)
                    ),
                    Rnd@1}
            );

        false ->
            case Bits of
                0 ->
                    Res;

                Bits@1 ->
                    {Next@1, Rnd@2} = get_next_bits(Rnd, Bits@1),
                    {bigi_ffi:add(
                            bigi_ffi:bitwise_shift_left(Bi, 48),
                            bigi_ffi:from(Next@1)
                        ),
                        Rnd@2}
            end
    end.

-file("src/randomlib.gleam", 38).
?DOC(
    " Returns a tuple containing a BigInt of size n bits and the\n"
    " updated random seed\n"
    " Note that this hasn't been fully tested to generate\n"
    " uniformly distributed values\n"
).
-spec next(random(), integer()) -> {bigi:big_int(), random()}.
next(Rnd, Bits) ->
    get_next(Bits, {bigi_ffi:zero(), Rnd}).

-file("src/randomlib.gleam", 146).
-spec initial_scramble(integer()) -> integer().
initial_scramble(Seed) ->
    erlang:'band'(erlang:'bxor'(Seed, 25214903917), 281474976710655).

-file("src/randomlib.gleam", 25).
?DOC(
    " Creates a new random seed with distinctness set using the\n"
    " current time, nominally in nanoseconds.\n"
    " Please note that the actual timings will be microseconds\n"
    " for erland and milliseconds for javascript, so any new random\n"
    " seeds generated in that same time scale will be the same\n"
).
-spec new() -> random().
new() ->
    {random, initial_scramble(init_seed())}.

-file("src/randomlib.gleam", 30).
?DOC(" Creates a new random seed based on the specified seed provided\n").
-spec with_seed(integer()) -> random().
with_seed(Seed) ->
    {random, initial_scramble(Seed)}.

-file("src/randomlib.gleam", 199).
-spec get_unique_int({integer(), random()}, integer()) -> {integer(), random()}.
get_unique_int(Acc, Limit) ->
    {R, Rnd} = Acc,
    U = R,
    R@2 = case gleam@int:modulo(U, Limit) of
        {ok, R@1} -> R@1;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"randomlib"/utf8>>,
                        function => <<"get_unique_int"/utf8>>,
                        line => 202,
                        value => _assert_fail,
                        start => 6100,
                        'end' => 6139,
                        pattern_start => 6111,
                        pattern_end => 6116})
    end,
    case gleam@int:compare(((U - R@2) + Limit) - 1, 2147483647) of
        gt ->
            get_unique_int(get_next_bits(Rnd, 31), Limit);

        _ ->
            {R@2, Rnd}
    end.

-file("src/randomlib.gleam", 73).
?DOC(
    " Returns a result containing either a tuple containing a uniformly distributed\n"
    " Int (where int is 31 bits unsigned) and the updated random seed or an error\n"
    " containing the random seed (currently not updated)\n"
).
-spec next_int(random(), integer()) -> {ok, {integer(), random()}} |
    {error, random()}.
next_int(Rnd, Limit) ->
    case Limit of
        L when (L =< 0) orelse (L >= 2147483647) ->
            {error, Rnd};

        Limit@1 ->
            {Next, Rnd@1} = get_next_bits(Rnd, 31),
            case gleam@int:compare(erlang:'band'(Limit@1 - 1, Limit@1), 0) of
                eq ->
                    Next@2 = case bigi_ffi:to(
                        bigi_ffi:bitwise_shift_right(
                            bigi_ffi:multiply(
                                bigi_ffi:from(Next),
                                bigi_ffi:from(Limit@1)
                            ),
                            31
                        )
                    ) of
                        {ok, Next@1} -> Next@1;
                        _assert_fail ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        file => <<?FILEPATH/utf8>>,
                                        module => <<"randomlib"/utf8>>,
                                        function => <<"next_int"/utf8>>,
                                        line => 81,
                                        value => _assert_fail,
                                        start => 2563,
                                        'end' => 2739,
                                        pattern_start => 2574,
                                        pattern_end => 2582})
                    end,
                    {ok, {Next@2, Rnd@1}};

                _ ->
                    {Next@3, Rnd@2} = get_unique_int({Next, Rnd@1}, Limit@1),
                    {ok, {Next@3, Rnd@2}}
            end
    end.

-file("src/randomlib.gleam", 120).
?DOC(
    " If non-empty choices list is provided, returns an iterator that performs a uniformly \n"
    " distributed selection from the the items in the list\n"
    " If no choices are passed an Error(Nil) is returned\n"
).
-spec choice(random(), list(NME)) -> {ok, gleam@yielder:yielder(NME)} |
    {error, nil}.
choice(Rnd, Choices) ->
    case Choices of
        [] ->
            {error, nil};

        Choices@1 ->
            Length = erlang:length(Choices@1),
            {ok,
                gleam@yielder:unfold(
                    Rnd,
                    fun(Acc) ->
                        {Next@1, Rnd@2} = case next_int(Acc, Length) of
                            {ok, {Next, Rnd@1}} -> {Next, Rnd@1};
                            _assert_fail ->
                                erlang:error(#{gleam_error => let_assert,
                                            message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                            file => <<?FILEPATH/utf8>>,
                                            module => <<"randomlib"/utf8>>,
                                            function => <<"choice"/utf8>>,
                                            line => 127,
                                            value => _assert_fail,
                                            start => 4060,
                                            'end' => 4111,
                                            pattern_start => 4071,
                                            pattern_end => 4087})
                        end,
                        Next@3 = case gleam@list:first(
                            erlang:element(
                                2,
                                gleam@list:split(Choices@1, Next@1)
                            )
                        ) of
                            {ok, Next@2} -> Next@2;
                            _assert_fail@1 ->
                                erlang:error(#{gleam_error => let_assert,
                                            message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                            file => <<?FILEPATH/utf8>>,
                                            module => <<"randomlib"/utf8>>,
                                            function => <<"choice"/utf8>>,
                                            line => 128,
                                            value => _assert_fail@1,
                                            start => 4122,
                                            'end' => 4183,
                                            pattern_start => 4133,
                                            pattern_end => 4141})
                        end,
                        {next, Next@3, Rnd@2}
                    end
                )}
    end.
