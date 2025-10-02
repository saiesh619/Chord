-module(gleam@list).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/gleam/list.gleam").
-export([length/1, count/2, reverse/1, is_empty/1, contains/2, first/1, rest/1, group/2, filter/2, filter_map/2, map/2, map2/3, map_fold/3, index_map/2, try_map/2, drop/2, take/2, new/0, wrap/1, append/2, prepend/2, flatten/1, flat_map/2, fold/3, fold_right/3, index_fold/3, try_fold/3, fold_until/3, find/2, find_map/2, all/2, any/2, zip/2, strict_zip/2, unzip/1, intersperse/2, unique/1, sort/2, range/2, repeat/2, split/2, split_while/2, key_find/2, key_filter/2, key_pop/2, key_set/3, each/2, try_each/2, partition/2, window/2, window_by_2/1, drop_while/2, take_while/2, chunk/2, sized_chunk/2, reduce/2, scan/3, last/1, combinations/2, combination_pairs/1, transpose/1, interleave/1, shuffle/1, max/2, sample/2, permutations/1]).
-export_type([continue_or_stop/1, sorting/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Lists are an ordered sequence of elements and are one of the most common\n"
    " data types in Gleam.\n"
    "\n"
    " New elements can be added and removed from the front of a list in\n"
    " constant time, while adding and removing from the end requires traversing\n"
    " and copying the whole list, so keep this in mind when designing your\n"
    " programs.\n"
    "\n"
    " There is a dedicated syntax for prefixing to a list:\n"
    "\n"
    " ```gleam\n"
    " let new_list = [1, 2, ..existing_list]\n"
    " ```\n"
    "\n"
    " And a matching syntax for getting the first elements of a list:\n"
    "\n"
    " ```gleam\n"
    " case list {\n"
    "   [first_element, ..rest] -> first_element\n"
    "   _ -> \"this pattern matches when the list is empty\"\n"
    " }\n"
    " ```\n"
    "\n"
).

-type continue_or_stop(YI) :: {continue, YI} | {stop, YI}.

-type sorting() :: ascending | descending.

-file("src/gleam/list.gleam", 60).
-spec length_loop(list(any()), integer()) -> integer().
length_loop(List, Count) ->
    case List of
        [_ | List@1] ->
            length_loop(List@1, Count + 1);

        [] ->
            Count
    end.

-file("src/gleam/list.gleam", 56).
?DOC(
    " Counts the number of elements in a given list.\n"
    "\n"
    " This function has to traverse the list to determine the number of elements,\n"
    " so it runs in linear time.\n"
    "\n"
    " This function is natively implemented by the virtual machine and is highly\n"
    " optimised.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " length([])\n"
    " // -> 0\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " length([1])\n"
    " // -> 1\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " length([1, 2])\n"
    " // -> 2\n"
    " ```\n"
).
-spec length(list(any())) -> integer().
length(List) ->
    erlang:length(List).

-file("src/gleam/list.gleam", 93).
-spec count_loop(list(YP), fun((YP) -> boolean()), integer()) -> integer().
count_loop(List, Predicate, Acc) ->
    case List of
        [] ->
            Acc;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    count_loop(Rest, Predicate, Acc + 1);

                false ->
                    count_loop(Rest, Predicate, Acc)
            end
    end.

-file("src/gleam/list.gleam", 89).
?DOC(
    " Counts the number of elements in a given list satisfying a given predicate.\n"
    "\n"
    " This function has to traverse the list to determine the number of elements,\n"
    " so it runs in linear time.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " count([], fn(a) { a > 0 })\n"
    " // -> 0\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " count([1], fn(a) { a > 0 })\n"
    " // -> 1\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " count([1, 2, 3], int.is_odd)\n"
    " // -> 2\n"
    " ```\n"
).
-spec count(list(YN), fun((YN) -> boolean())) -> integer().
count(List, Predicate) ->
    count_loop(List, Predicate, 0).

-file("src/gleam/list.gleam", 131).
?DOC(
    " Creates a new list from a given list containing the same elements but in the\n"
    " opposite order.\n"
    "\n"
    " This function has to traverse the list to create the new reversed list, so\n"
    " it runs in linear time.\n"
    "\n"
    " This function is natively implemented by the virtual machine and is highly\n"
    " optimised.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " reverse([])\n"
    " // -> []\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " reverse([1])\n"
    " // -> [1]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " reverse([1, 2])\n"
    " // -> [2, 1]\n"
    " ```\n"
).
-spec reverse(list(YR)) -> list(YR).
reverse(List) ->
    lists:reverse(List).

-file("src/gleam/list.gleam", 168).
?DOC(
    " Determines whether or not the list is empty.\n"
    "\n"
    " This function runs in constant time.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " is_empty([])\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " is_empty([1])\n"
    " // -> False\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " is_empty([1, 1])\n"
    " // -> False\n"
    " ```\n"
).
-spec is_empty(list(any())) -> boolean().
is_empty(List) ->
    List =:= [].

-file("src/gleam/list.gleam", 204).
?DOC(
    " Determines whether or not a given element exists within a given list.\n"
    "\n"
    " This function traverses the list to find the element, so it runs in linear\n"
    " time.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [] |> contains(any: 0)\n"
    " // -> False\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " [0] |> contains(any: 0)\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " [1] |> contains(any: 0)\n"
    " // -> False\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " [1, 1] |> contains(any: 0)\n"
    " // -> False\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " [1, 0] |> contains(any: 0)\n"
    " // -> True\n"
    " ```\n"
).
-spec contains(list(AAA), AAA) -> boolean().
contains(List, Elem) ->
    case List of
        [] ->
            false;

        [First | _] when First =:= Elem ->
            true;

        [_ | Rest] ->
            contains(Rest, Elem)
    end.

-file("src/gleam/list.gleam", 231).
?DOC(
    " Gets the first element from the start of the list, if there is one.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " first([])\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " first([0])\n"
    " // -> Ok(0)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " first([1, 2])\n"
    " // -> Ok(1)\n"
    " ```\n"
).
-spec first(list(AAC)) -> {ok, AAC} | {error, nil}.
first(List) ->
    case List of
        [] ->
            {error, nil};

        [First | _] ->
            {ok, First}
    end.

-file("src/gleam/list.gleam", 260).
?DOC(
    " Returns the list minus the first element. If the list is empty, `Error(Nil)` is\n"
    " returned.\n"
    "\n"
    " This function runs in constant time and does not make a copy of the list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " rest([])\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " rest([0])\n"
    " // -> Ok([])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " rest([1, 2])\n"
    " // -> Ok([2])\n"
    " ```\n"
).
-spec rest(list(AAG)) -> {ok, list(AAG)} | {error, nil}.
rest(List) ->
    case List of
        [] ->
            {error, nil};

        [_ | Rest] ->
            {ok, Rest}
    end.

-file("src/gleam/list.gleam", 302).
-spec group_loop(list(AAR), fun((AAR) -> AAT), gleam@dict:dict(AAT, list(AAR))) -> gleam@dict:dict(AAT, list(AAR)).
group_loop(List, To_key, Groups) ->
    case List of
        [] ->
            Groups;

        [First | Rest] ->
            Key = To_key(First),
            Groups@1 = case gleam_stdlib:map_get(Groups, Key) of
                {error, _} ->
                    gleam@dict:insert(Groups, Key, [First]);

                {ok, Existing} ->
                    gleam@dict:insert(Groups, Key, [First | Existing])
            end,
            group_loop(Rest, To_key, Groups@1)
    end.

-file("src/gleam/list.gleam", 298).
?DOC(
    " Groups the elements from the given list by the given key function.\n"
    "\n"
    " Does not preserve the initial value order.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " import gleam/dict\n"
    "\n"
    " [Ok(3), Error(\"Wrong\"), Ok(200), Ok(73)]\n"
    " |> group(by: fn(i) {\n"
    "   case i {\n"
    "     Ok(_) -> \"Successful\"\n"
    "     Error(_) -> \"Failed\"\n"
    "   }\n"
    " })\n"
    " |> dict.to_list\n"
    " // -> [\n"
    " //   #(\"Failed\", [Error(\"Wrong\")]),\n"
    " //   #(\"Successful\", [Ok(73), Ok(200), Ok(3)])\n"
    " // ]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " import gleam/dict\n"
    "\n"
    " group([1,2,3,4,5], by: fn(i) { i - i / 3 * 3 })\n"
    " |> dict.to_list\n"
    " // -> [#(0, [3]), #(1, [4, 1]), #(2, [5, 2])]\n"
    " ```\n"
).
-spec group(list(AAL), fun((AAL) -> AAN)) -> gleam@dict:dict(AAN, list(AAL)).
group(List, Key) ->
    group_loop(List, Key, maps:new()).

-file("src/gleam/list.gleam", 339).
-spec filter_loop(list(ABD), fun((ABD) -> boolean()), list(ABD)) -> list(ABD).
filter_loop(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            New_acc = case Fun(First) of
                true ->
                    [First | Acc];

                false ->
                    Acc
            end,
            filter_loop(Rest, Fun, New_acc)
    end.

-file("src/gleam/list.gleam", 335).
?DOC(
    " Returns a new list containing only the elements from the first list for\n"
    " which the given functions returns `True`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " filter([2, 4, 6, 1], fn(x) { x > 2 })\n"
    " // -> [4, 6]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " filter([2, 4, 6, 1], fn(x) { x > 6 })\n"
    " // -> []\n"
    " ```\n"
).
-spec filter(list(ABA), fun((ABA) -> boolean())) -> list(ABA).
filter(List, Predicate) ->
    filter_loop(List, Predicate, []).

-file("src/gleam/list.gleam", 371).
-spec filter_map_loop(
    list(ABO),
    fun((ABO) -> {ok, ABQ} | {error, any()}),
    list(ABQ)
) -> list(ABQ).
filter_map_loop(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            New_acc = case Fun(First) of
                {ok, First@1} ->
                    [First@1 | Acc];

                {error, _} ->
                    Acc
            end,
            filter_map_loop(Rest, Fun, New_acc)
    end.

-file("src/gleam/list.gleam", 367).
?DOC(
    " Returns a new list containing only the elements from the first list for\n"
    " which the given functions returns `Ok(_)`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " filter_map([2, 4, 6, 1], Error)\n"
    " // -> []\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " filter_map([2, 4, 6, 1], fn(x) { Ok(x + 1) })\n"
    " // -> [3, 5, 7, 2]\n"
    " ```\n"
).
-spec filter_map(list(ABH), fun((ABH) -> {ok, ABJ} | {error, any()})) -> list(ABJ).
filter_map(List, Fun) ->
    filter_map_loop(List, Fun, []).

-file("src/gleam/list.gleam", 402).
-spec map_loop(list(ACA), fun((ACA) -> ACC), list(ACC)) -> list(ACC).
map_loop(List, Fun, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            map_loop(Rest, Fun, [Fun(First) | Acc])
    end.

-file("src/gleam/list.gleam", 398).
?DOC(
    " Returns a new list containing only the elements of the first list after the\n"
    " function has been applied to each one.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " map([2, 4, 6], fn(x) { x * 2 })\n"
    " // -> [4, 8, 12]\n"
    " ```\n"
).
-spec map(list(ABW), fun((ABW) -> ABY)) -> list(ABY).
map(List, Fun) ->
    map_loop(List, Fun, []).

-file("src/gleam/list.gleam", 429).
-spec map2_loop(list(ACL), list(ACN), fun((ACL, ACN) -> ACP), list(ACP)) -> list(ACP).
map2_loop(List1, List2, Fun, Acc) ->
    case {List1, List2} of
        {[], _} ->
            lists:reverse(Acc);

        {_, []} ->
            lists:reverse(Acc);

        {[A | As_], [B | Bs]} ->
            map2_loop(As_, Bs, Fun, [Fun(A, B) | Acc])
    end.

-file("src/gleam/list.gleam", 425).
?DOC(
    " Combines two lists into a single list using the given function.\n"
    "\n"
    " If a list is longer than the other the extra elements are dropped.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " map2([1, 2, 3], [4, 5, 6], fn(x, y) { x + y })\n"
    " // -> [5, 7, 9]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " map2([1, 2], [\"a\", \"b\", \"c\"], fn(i, x) { #(i, x) })\n"
    " // -> [#(1, \"a\"), #(2, \"b\")]\n"
    " ```\n"
).
-spec map2(list(ACF), list(ACH), fun((ACF, ACH) -> ACJ)) -> list(ACJ).
map2(List1, List2, Fun) ->
    map2_loop(List1, List2, Fun, []).

-file("src/gleam/list.gleam", 462).
-spec map_fold_loop(list(ACX), fun((ACZ, ACX) -> {ACZ, ADA}), ACZ, list(ADA)) -> {ACZ,
    list(ADA)}.
map_fold_loop(List, Fun, Acc, List_acc) ->
    case List of
        [] ->
            {Acc, lists:reverse(List_acc)};

        [First | Rest] ->
            {Acc@1, First@1} = Fun(Acc, First),
            map_fold_loop(Rest, Fun, Acc@1, [First@1 | List_acc])
    end.

-file("src/gleam/list.gleam", 454).
?DOC(
    " Similar to `map` but also lets you pass around an accumulated value.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " map_fold(\n"
    "   over: [1, 2, 3],\n"
    "   from: 100,\n"
    "   with: fn(memo, i) { #(memo + i, i * 2) }\n"
    " )\n"
    " // -> #(106, [2, 4, 6])\n"
    " ```\n"
).
-spec map_fold(list(ACS), ACU, fun((ACU, ACS) -> {ACU, ACV})) -> {ACU,
    list(ACV)}.
map_fold(List, Initial, Fun) ->
    map_fold_loop(List, Fun, Initial, []).

-file("src/gleam/list.gleam", 494).
-spec index_map_loop(
    list(ADH),
    fun((ADH, integer()) -> ADJ),
    integer(),
    list(ADJ)
) -> list(ADJ).
index_map_loop(List, Fun, Index, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            Acc@1 = [Fun(First, Index) | Acc],
            index_map_loop(Rest, Fun, Index + 1, Acc@1)
    end.

-file("src/gleam/list.gleam", 490).
?DOC(
    " Returns a new list containing only the elements of the first list after the\n"
    " function has been applied to each one and their index.\n"
    "\n"
    " The index starts at 0, so the first element is 0, the second is 1, and so\n"
    " on.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " index_map([\"a\", \"b\"], fn(x, i) { #(i, x) })\n"
    " // -> [#(0, \"a\"), #(1, \"b\")]\n"
    " ```\n"
).
-spec index_map(list(ADD), fun((ADD, integer()) -> ADF)) -> list(ADF).
index_map(List, Fun) ->
    index_map_loop(List, Fun, 0, []).

-file("src/gleam/list.gleam", 548).
-spec try_map_loop(list(ADV), fun((ADV) -> {ok, ADX} | {error, ADY}), list(ADX)) -> {ok,
        list(ADX)} |
    {error, ADY}.
try_map_loop(List, Fun, Acc) ->
    case List of
        [] ->
            {ok, lists:reverse(Acc)};

        [First | Rest] ->
            case Fun(First) of
                {ok, First@1} ->
                    try_map_loop(Rest, Fun, [First@1 | Acc]);

                {error, Error} ->
                    {error, Error}
            end
    end.

-file("src/gleam/list.gleam", 541).
?DOC(
    " Takes a function that returns a `Result` and applies it to each element in a\n"
    " given list in turn.\n"
    "\n"
    " If the function returns `Ok(new_value)` for all elements in the list then a\n"
    " list of the new values is returned.\n"
    "\n"
    " If the function returns `Error(reason)` for any of the elements then it is\n"
    " returned immediately. None of the elements in the list are processed after\n"
    " one returns an `Error`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " try_map([1, 2, 3], fn(x) { Ok(x + 2) })\n"
    " // -> Ok([3, 4, 5])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " try_map([1, 2, 3], fn(_) { Error(0) })\n"
    " // -> Error(0)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " try_map([[1], [2, 3]], first)\n"
    " // -> Ok([1, 2])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " try_map([[1], [], [2]], first)\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec try_map(list(ADM), fun((ADM) -> {ok, ADO} | {error, ADP})) -> {ok,
        list(ADO)} |
    {error, ADP}.
try_map(List, Fun) ->
    try_map_loop(List, Fun, []).

-file("src/gleam/list.gleam", 583).
?DOC(
    " Returns a list that is the given list with up to the given number of\n"
    " elements removed from the front of the list.\n"
    "\n"
    " If the element has less than the number of elements an empty list is\n"
    " returned.\n"
    "\n"
    " This function runs in linear time but does not copy the list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " drop([1, 2, 3, 4], 2)\n"
    " // -> [3, 4]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " drop([1, 2, 3, 4], 9)\n"
    " // -> []\n"
    " ```\n"
).
-spec drop(list(AEF), integer()) -> list(AEF).
drop(List, N) ->
    case N =< 0 of
        true ->
            List;

        false ->
            case List of
                [] ->
                    [];

                [_ | Rest] ->
                    drop(Rest, N - 1)
            end
    end.

-file("src/gleam/list.gleam", 618).
-spec take_loop(list(AEL), integer(), list(AEL)) -> list(AEL).
take_loop(List, N, Acc) ->
    case N =< 0 of
        true ->
            lists:reverse(Acc);

        false ->
            case List of
                [] ->
                    lists:reverse(Acc);

                [First | Rest] ->
                    take_loop(Rest, N - 1, [First | Acc])
            end
    end.

-file("src/gleam/list.gleam", 614).
?DOC(
    " Returns a list containing the first given number of elements from the given\n"
    " list.\n"
    "\n"
    " If the element has less than the number of elements then the full list is\n"
    " returned.\n"
    "\n"
    " This function runs in linear time.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " take([1, 2, 3, 4], 2)\n"
    " // -> [1, 2]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " take([1, 2, 3, 4], 9)\n"
    " // -> [1, 2, 3, 4]\n"
    " ```\n"
).
-spec take(list(AEI), integer()) -> list(AEI).
take(List, N) ->
    take_loop(List, N, []).

-file("src/gleam/list.gleam", 638).
?DOC(
    " Returns a new empty list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " new()\n"
    " // -> []\n"
    " ```\n"
).
-spec new() -> list(any()).
new() ->
    [].

-file("src/gleam/list.gleam", 658).
?DOC(
    " Returns the given item wrapped in a list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " wrap(1)\n"
    " // -> [1]\n"
    "\n"
    " wrap([\"a\", \"b\", \"c\"])\n"
    " // -> [[\"a\", \"b\", \"c\"]]\n"
    "\n"
    " wrap([[]])\n"
    " // -> [[[]]]\n"
    " ```\n"
).
-spec wrap(AER) -> list(AER).
wrap(Item) ->
    [Item].

-file("src/gleam/list.gleam", 679).
-spec append_loop(list(AEX), list(AEX)) -> list(AEX).
append_loop(First, Second) ->
    case First of
        [] ->
            Second;

        [First@1 | Rest] ->
            append_loop(Rest, [First@1 | Second])
    end.

-file("src/gleam/list.gleam", 675).
?DOC(
    " Joins one list onto the end of another.\n"
    "\n"
    " This function runs in linear time, and it traverses and copies the first\n"
    " list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " append([1, 2], [3])\n"
    " // -> [1, 2, 3]\n"
    " ```\n"
).
-spec append(list(AET), list(AET)) -> list(AET).
append(First, Second) ->
    lists:append(First, Second).

-file("src/gleam/list.gleam", 699).
?DOC(
    " Prefixes an item to a list. This can also be done using the dedicated\n"
    " syntax instead\n"
    "\n"
    " ```gleam\n"
    " let existing_list = [2, 3, 4]\n"
    "\n"
    " [1, ..existing_list]\n"
    " // -> [1, 2, 3, 4]\n"
    "\n"
    " prepend(to: existing_list, this: 1)\n"
    " // -> [1, 2, 3, 4]\n"
    " ```\n"
).
-spec prepend(list(AFB), AFB) -> list(AFB).
prepend(List, Item) ->
    [Item | List].

-file("src/gleam/list.gleam", 720).
-spec flatten_loop(list(list(AFI)), list(AFI)) -> list(AFI).
flatten_loop(Lists, Acc) ->
    case Lists of
        [] ->
            lists:reverse(Acc);

        [List | Further_lists] ->
            flatten_loop(Further_lists, lists:reverse(List, Acc))
    end.

-file("src/gleam/list.gleam", 716).
?DOC(
    " Joins a list of lists into a single list.\n"
    "\n"
    " This function traverses all elements twice on the JavaScript target.\n"
    " This function traverses all elements once on the Erlang target.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " flatten([[1], [2, 3], []])\n"
    " // -> [1, 2, 3]\n"
    " ```\n"
).
-spec flatten(list(list(AFE))) -> list(AFE).
flatten(Lists) ->
    lists:append(Lists).

-file("src/gleam/list.gleam", 737).
?DOC(
    " Maps the list with the given function into a list of lists, and then flattens it.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " flat_map([2, 4, 6], fn(x) { [x, x + 1] })\n"
    " // -> [2, 3, 4, 5, 6, 7]\n"
    " ```\n"
).
-spec flat_map(list(AFN), fun((AFN) -> list(AFP))) -> list(AFP).
flat_map(List, Fun) ->
    lists:append(map(List, Fun)).

-file("src/gleam/list.gleam", 749).
?DOC(
    " Reduces a list of elements into a single value by calling a given function\n"
    " on each element, going from left to right.\n"
    "\n"
    " `fold([1, 2, 3], 0, add)` is the equivalent of\n"
    " `add(add(add(0, 1), 2), 3)`.\n"
    "\n"
    " This function runs in linear time.\n"
).
-spec fold(list(AFS), AFU, fun((AFU, AFS) -> AFU)) -> AFU.
fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [First | Rest] ->
            fold(Rest, Fun(Initial, First), Fun)
    end.

-file("src/gleam/list.gleam", 771).
?DOC(
    " Reduces a list of elements into a single value by calling a given function\n"
    " on each element, going from right to left.\n"
    "\n"
    " `fold_right([1, 2, 3], 0, add)` is the equivalent of\n"
    " `add(add(add(0, 3), 2), 1)`.\n"
    "\n"
    " This function runs in linear time.\n"
    "\n"
    " Unlike `fold` this function is not tail recursive. Where possible use\n"
    " `fold` instead as it will use less memory.\n"
).
-spec fold_right(list(AFV), AFX, fun((AFX, AFV) -> AFX)) -> AFX.
fold_right(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [First | Rest] ->
            Fun(fold_right(Rest, Initial, Fun), First)
    end.

-file("src/gleam/list.gleam", 808).
-spec index_fold_loop(
    list(AGB),
    AGD,
    fun((AGD, AGB, integer()) -> AGD),
    integer()
) -> AGD.
index_fold_loop(Over, Acc, With, Index) ->
    case Over of
        [] ->
            Acc;

        [First | Rest] ->
            index_fold_loop(Rest, With(Acc, First, Index), With, Index + 1)
    end.

-file("src/gleam/list.gleam", 800).
?DOC(
    " Like fold but the folding function also receives the index of the current element.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [\"a\", \"b\", \"c\"]\n"
    " |> index_fold(\"\", fn(acc, item, index) {\n"
    "    acc <> int.to_string(index) <> \":\" <> item <> \" \"\n"
    " })\n"
    " // -> \"0:a 1:b 2:c\"\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " [10, 20, 30]\n"
    " |> index_fold(0, fn(acc, item, index) { acc + item * index })\n"
    " // -> 80\n"
    " ```\n"
).
-spec index_fold(list(AFY), AGA, fun((AGA, AFY, integer()) -> AGA)) -> AGA.
index_fold(List, Initial, Fun) ->
    index_fold_loop(List, Initial, Fun, 0).

-file("src/gleam/list.gleam", 840).
?DOC(
    " A variant of fold that might fail.\n"
    "\n"
    " The folding function should return `Result(accumulator, error)`.\n"
    " If the returned value is `Ok(accumulator)` try_fold will try the next value in the list.\n"
    " If the returned value is `Error(error)` try_fold will stop and return that error.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [1, 2, 3, 4]\n"
    " |> try_fold(0, fn(acc, i) {\n"
    "   case i < 3 {\n"
    "     True -> Ok(acc + i)\n"
    "     False -> Error(Nil)\n"
    "   }\n"
    " })\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec try_fold(list(AGE), AGG, fun((AGG, AGE) -> {ok, AGG} | {error, AGH})) -> {ok,
        AGG} |
    {error, AGH}.
try_fold(List, Initial, Fun) ->
    case List of
        [] ->
            {ok, Initial};

        [First | Rest] ->
            case Fun(Initial, First) of
                {ok, Result} ->
                    try_fold(Rest, Result, Fun);

                {error, _} = Error ->
                    Error
            end
    end.

-file("src/gleam/list.gleam", 879).
?DOC(
    " A variant of fold that allows to stop folding earlier.\n"
    "\n"
    " The folding function should return `ContinueOrStop(accumulator)`.\n"
    " If the returned value is `Continue(accumulator)` fold_until will try the next value in the list.\n"
    " If the returned value is `Stop(accumulator)` fold_until will stop and return that accumulator.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [1, 2, 3, 4]\n"
    " |> fold_until(0, fn(acc, i) {\n"
    "   case i < 3 {\n"
    "     True -> Continue(acc + i)\n"
    "     False -> Stop(acc)\n"
    "   }\n"
    " })\n"
    " // -> 3\n"
    " ```\n"
).
-spec fold_until(list(AGM), AGO, fun((AGO, AGM) -> continue_or_stop(AGO))) -> AGO.
fold_until(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [First | Rest] ->
            case Fun(Initial, First) of
                {continue, Next_accumulator} ->
                    fold_until(Rest, Next_accumulator, Fun);

                {stop, B} ->
                    B
            end
    end.

-file("src/gleam/list.gleam", 916).
?DOC(
    " Finds the first element in a given list for which the given function returns\n"
    " `True`.\n"
    "\n"
    " Returns `Error(Nil)` if no such element is found.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " find([1, 2, 3], fn(x) { x > 2 })\n"
    " // -> Ok(3)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " find([1, 2, 3], fn(x) { x > 4 })\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " find([], fn(_) { True })\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec find(list(AGQ), fun((AGQ) -> boolean())) -> {ok, AGQ} | {error, nil}.
find(List, Is_desired) ->
    case List of
        [] ->
            {error, nil};

        [First | Rest] ->
            case Is_desired(First) of
                true ->
                    {ok, First};

                false ->
                    find(Rest, Is_desired)
            end
    end.

-file("src/gleam/list.gleam", 952).
?DOC(
    " Finds the first element in a given list for which the given function returns\n"
    " `Ok(new_value)`, then returns the wrapped `new_value`.\n"
    "\n"
    " Returns `Error(Nil)` if no such element is found.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " find_map([[], [2], [3]], first)\n"
    " // -> Ok(2)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " find_map([[], []], first)\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " find_map([], first)\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec find_map(list(AGU), fun((AGU) -> {ok, AGW} | {error, any()})) -> {ok, AGW} |
    {error, nil}.
find_map(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [First | Rest] ->
            case Fun(First) of
                {ok, First@1} ->
                    {ok, First@1};

                {error, _} ->
                    find_map(Rest, Fun)
            end
    end.

-file("src/gleam/list.gleam", 987).
?DOC(
    " Returns `True` if the given function returns `True` for all the elements in\n"
    " the given list. If the function returns `False` for any of the elements it\n"
    " immediately returns `False` without checking the rest of the list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " all([], fn(x) { x > 3 })\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " all([4, 5], fn(x) { x > 3 })\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " all([4, 3], fn(x) { x > 3 })\n"
    " // -> False\n"
    " ```\n"
).
-spec all(list(AHC), fun((AHC) -> boolean())) -> boolean().
all(List, Predicate) ->
    case List of
        [] ->
            true;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    all(Rest, Predicate);

                false ->
                    false
            end
    end.

-file("src/gleam/list.gleam", 1024).
?DOC(
    " Returns `True` if the given function returns `True` for any the elements in\n"
    " the given list. If the function returns `True` for any of the elements it\n"
    " immediately returns `True` without checking the rest of the list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " any([], fn(x) { x > 3 })\n"
    " // -> False\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " any([4, 5], fn(x) { x > 3 })\n"
    " // -> True\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " any([4, 3], fn(x) { x > 4 })\n"
    " // -> False\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " any([3, 4], fn(x) { x > 3 })\n"
    " // -> True\n"
    " ```\n"
).
-spec any(list(AHE), fun((AHE) -> boolean())) -> boolean().
any(List, Predicate) ->
    case List of
        [] ->
            false;

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    true;

                false ->
                    any(Rest, Predicate)
            end
    end.

-file("src/gleam/list.gleam", 1066).
-spec zip_loop(list(AHL), list(AHN), list({AHL, AHN})) -> list({AHL, AHN}).
zip_loop(One, Other, Acc) ->
    case {One, Other} of
        {[First_one | Rest_one], [First_other | Rest_other]} ->
            zip_loop(Rest_one, Rest_other, [{First_one, First_other} | Acc]);

        {_, _} ->
            lists:reverse(Acc)
    end.

-file("src/gleam/list.gleam", 1062).
?DOC(
    " Takes two lists and returns a single list of 2-element tuples.\n"
    "\n"
    " If one of the lists is longer than the other, the remaining elements from\n"
    " the longer list are not used.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " zip([], [])\n"
    " // -> []\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " zip([1, 2], [3])\n"
    " // -> [#(1, 3)]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " zip([1], [3, 4])\n"
    " // -> [#(1, 3)]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " zip([1, 2], [3, 4])\n"
    " // -> [#(1, 3), #(2, 4)]\n"
    " ```\n"
).
-spec zip(list(AHG), list(AHI)) -> list({AHG, AHI}).
zip(List, Other) ->
    zip_loop(List, Other, []).

-file("src/gleam/list.gleam", 1107).
-spec strict_zip_loop(list(AHY), list(AIA), list({AHY, AIA})) -> {ok,
        list({AHY, AIA})} |
    {error, nil}.
strict_zip_loop(One, Other, Acc) ->
    case {One, Other} of
        {[], []} ->
            {ok, lists:reverse(Acc)};

        {[], _} ->
            {error, nil};

        {_, []} ->
            {error, nil};

        {[First_one | Rest_one], [First_other | Rest_other]} ->
            strict_zip_loop(
                Rest_one,
                Rest_other,
                [{First_one, First_other} | Acc]
            )
    end.

-file("src/gleam/list.gleam", 1100).
?DOC(
    " Takes two lists and returns a single list of 2-element tuples.\n"
    "\n"
    " If one of the lists is longer than the other, an `Error` is returned.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " strict_zip([], [])\n"
    " // -> Ok([])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " strict_zip([1, 2], [3])\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " strict_zip([1], [3, 4])\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " strict_zip([1, 2], [3, 4])\n"
    " // -> Ok([#(1, 3), #(2, 4)])\n"
    " ```\n"
).
-spec strict_zip(list(AHR), list(AHT)) -> {ok, list({AHR, AHT})} | {error, nil}.
strict_zip(List, Other) ->
    strict_zip_loop(List, Other, []).

-file("src/gleam/list.gleam", 1138).
-spec unzip_loop(list({AIL, AIM}), list(AIL), list(AIM)) -> {list(AIL),
    list(AIM)}.
unzip_loop(Input, One, Other) ->
    case Input of
        [] ->
            {lists:reverse(One), lists:reverse(Other)};

        [{First_one, First_other} | Rest] ->
            unzip_loop(Rest, [First_one | One], [First_other | Other])
    end.

-file("src/gleam/list.gleam", 1134).
?DOC(
    " Takes a single list of 2-element tuples and returns two lists.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " unzip([#(1, 2), #(3, 4)])\n"
    " // -> #([1, 3], [2, 4])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " unzip([])\n"
    " // -> #([], [])\n"
    " ```\n"
).
-spec unzip(list({AIG, AIH})) -> {list(AIG), list(AIH)}.
unzip(Input) ->
    unzip_loop(Input, [], []).

-file("src/gleam/list.gleam", 1173).
-spec intersperse_loop(list(AIV), AIV, list(AIV)) -> list(AIV).
intersperse_loop(List, Separator, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            intersperse_loop(Rest, Separator, [First, Separator | Acc])
    end.

-file("src/gleam/list.gleam", 1166).
?DOC(
    " Inserts a given value between each existing element in a given list.\n"
    "\n"
    " This function runs in linear time and copies the list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " intersperse([1, 1, 1], 2)\n"
    " // -> [1, 2, 1, 2, 1]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " intersperse([], 2)\n"
    " // -> []\n"
    " ```\n"
).
-spec intersperse(list(AIS), AIS) -> list(AIS).
intersperse(List, Elem) ->
    case List of
        [] ->
            List;

        [_] ->
            List;

        [First | Rest] ->
            intersperse_loop(Rest, Elem, [First])
    end.

-file("src/gleam/list.gleam", 1196).
-spec unique_loop(list(AJC), gleam@dict:dict(AJC, nil), list(AJC)) -> list(AJC).
unique_loop(List, Seen, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            case gleam@dict:has_key(Seen, First) of
                true ->
                    unique_loop(Rest, Seen, Acc);

                false ->
                    unique_loop(
                        Rest,
                        gleam@dict:insert(Seen, First, nil),
                        [First | Acc]
                    )
            end
    end.

-file("src/gleam/list.gleam", 1192).
?DOC(
    " Removes any duplicate elements from a given list.\n"
    "\n"
    " This function returns in loglinear time.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " unique([1, 1, 1, 4, 7, 3, 3, 4])\n"
    " // -> [1, 4, 7, 3]\n"
    " ```\n"
).
-spec unique(list(AIZ)) -> list(AIZ).
unique(List) ->
    unique_loop(List, maps:new(), []).

-file("src/gleam/list.gleam", 1282).
?DOC(
    " Given a list it returns slices of it that are locally sorted in ascending\n"
    " order.\n"
    "\n"
    " Imagine you have this list:\n"
    "\n"
    " ```\n"
    "   [1, 2, 3, 2, 1, 0]\n"
    "    ^^^^^^^  ^^^^^^^ This is a slice in descending order\n"
    "    |\n"
    "    | This is a slice that is sorted in ascending order\n"
    " ```\n"
    "\n"
    " So the produced result will contain these two slices, each one sorted in\n"
    " ascending order: `[[1, 2, 3], [0, 1, 2]]`.\n"
    "\n"
    " - `growing` is an accumulator with the current slice being grown\n"
    " - `direction` is the growing direction of the slice being grown, it could\n"
    "   either be ascending or strictly descending\n"
    " - `prev` is the previous element that needs to be added to the growing slice\n"
    "   it is carried around to check whether we have to keep growing the current\n"
    "   slice or not\n"
    " - `acc` is the accumulator containing the slices sorted in ascending order\n"
).
-spec sequences(
    list(AJL),
    fun((AJL, AJL) -> gleam@order:order()),
    list(AJL),
    sorting(),
    AJL,
    list(list(AJL))
) -> list(list(AJL)).
sequences(List, Compare, Growing, Direction, Prev, Acc) ->
    Growing@1 = [Prev | Growing],
    case List of
        [] ->
            case Direction of
                ascending ->
                    [lists:reverse(Growing@1) | Acc];

                descending ->
                    [Growing@1 | Acc]
            end;

        [New | Rest] ->
            case {Compare(Prev, New), Direction} of
                {gt, descending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {lt, ascending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {eq, ascending} ->
                    sequences(Rest, Compare, Growing@1, Direction, New, Acc);

                {gt, ascending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [lists:reverse(Growing@1) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end;

                {lt, descending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [lists:reverse(Growing@1) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end;

                {eq, descending} ->
                    Acc@1 = case Direction of
                        ascending ->
                            [lists:reverse(Growing@1) | Acc];

                        descending ->
                            [Growing@1 | Acc]
                    end,
                    case Rest of
                        [] ->
                            [[New] | Acc@1];

                        [Next | Rest@1] ->
                            Direction@1 = case Compare(New, Next) of
                                lt ->
                                    ascending;

                                eq ->
                                    ascending;

                                gt ->
                                    descending
                            end,
                            sequences(
                                Rest@1,
                                Compare,
                                [New],
                                Direction@1,
                                Next,
                                Acc@1
                            )
                    end
            end
    end.

-file("src/gleam/list.gleam", 1430).
?DOC(
    " Merges two lists sorted in ascending order into a single list sorted in\n"
    " descending order according to the given comparator function.\n"
    "\n"
    " This reversing of the sort order is not avoidable if we want to implement\n"
    " merge as a tail recursive function. We could reverse the accumulator before\n"
    " returning it but that would end up being less efficient; so the merging\n"
    " algorithm has to play around this.\n"
).
-spec merge_ascendings(
    list(AKI),
    list(AKI),
    fun((AKI, AKI) -> gleam@order:order()),
    list(AKI)
) -> list(AKI).
merge_ascendings(List1, List2, Compare, Acc) ->
    case {List1, List2} of
        {[], List} ->
            lists:reverse(List, Acc);

        {List, []} ->
            lists:reverse(List, Acc);

        {[First1 | Rest1], [First2 | Rest2]} ->
            case Compare(First1, First2) of
                lt ->
                    merge_ascendings(Rest1, List2, Compare, [First1 | Acc]);

                gt ->
                    merge_ascendings(List1, Rest2, Compare, [First2 | Acc]);

                eq ->
                    merge_ascendings(List1, Rest2, Compare, [First2 | Acc])
            end
    end.

-file("src/gleam/list.gleam", 1383).
?DOC(
    " Given a list of ascending lists, it merges adjacent pairs into a single\n"
    " descending list, halving their number.\n"
    " It returns a list of the remaining descending lists.\n"
).
-spec merge_ascending_pairs(
    list(list(AJW)),
    fun((AJW, AJW) -> gleam@order:order()),
    list(list(AJW))
) -> list(list(AJW)).
merge_ascending_pairs(Sequences, Compare, Acc) ->
    case Sequences of
        [] ->
            lists:reverse(Acc);

        [Sequence] ->
            lists:reverse([lists:reverse(Sequence) | Acc]);

        [Ascending1, Ascending2 | Rest] ->
            Descending = merge_ascendings(Ascending1, Ascending2, Compare, []),
            merge_ascending_pairs(Rest, Compare, [Descending | Acc])
    end.

-file("src/gleam/list.gleam", 1457).
?DOC(
    " This is exactly the same as merge_ascendings but mirrored: it merges two\n"
    " lists sorted in descending order into a single list sorted in ascending\n"
    " order according to the given comparator function.\n"
    "\n"
    " This reversing of the sort order is not avoidable if we want to implement\n"
    " merge as a tail recursive function. We could reverse the accumulator before\n"
    " returning it but that would end up being less efficient; so the merging\n"
    " algorithm has to play around this.\n"
).
-spec merge_descendings(
    list(AKN),
    list(AKN),
    fun((AKN, AKN) -> gleam@order:order()),
    list(AKN)
) -> list(AKN).
merge_descendings(List1, List2, Compare, Acc) ->
    case {List1, List2} of
        {[], List} ->
            lists:reverse(List, Acc);

        {List, []} ->
            lists:reverse(List, Acc);

        {[First1 | Rest1], [First2 | Rest2]} ->
            case Compare(First1, First2) of
                lt ->
                    merge_descendings(List1, Rest2, Compare, [First2 | Acc]);

                gt ->
                    merge_descendings(Rest1, List2, Compare, [First1 | Acc]);

                eq ->
                    merge_descendings(Rest1, List2, Compare, [First1 | Acc])
            end
    end.

-file("src/gleam/list.gleam", 1405).
?DOC(" This is the same as merge_ascending_pairs but flipped for descending lists.\n").
-spec merge_descending_pairs(
    list(list(AKC)),
    fun((AKC, AKC) -> gleam@order:order()),
    list(list(AKC))
) -> list(list(AKC)).
merge_descending_pairs(Sequences, Compare, Acc) ->
    case Sequences of
        [] ->
            lists:reverse(Acc);

        [Sequence] ->
            lists:reverse([lists:reverse(Sequence) | Acc]);

        [Descending1, Descending2 | Rest] ->
            Ascending = merge_descendings(Descending1, Descending2, Compare, []),
            merge_descending_pairs(Rest, Compare, [Ascending | Acc])
    end.

-file("src/gleam/list.gleam", 1349).
?DOC(
    " Given some some sorted sequences (assumed to be sorted in `direction`) it\n"
    " merges them all together until we're left with just a list sorted in\n"
    " ascending order.\n"
).
-spec merge_all(
    list(list(AJS)),
    sorting(),
    fun((AJS, AJS) -> gleam@order:order())
) -> list(AJS).
merge_all(Sequences, Direction, Compare) ->
    case {Sequences, Direction} of
        {[], _} ->
            [];

        {[Sequence], ascending} ->
            Sequence;

        {[Sequence@1], descending} ->
            lists:reverse(Sequence@1);

        {_, ascending} ->
            Sequences@1 = merge_ascending_pairs(Sequences, Compare, []),
            merge_all(Sequences@1, descending, Compare);

        {_, descending} ->
            Sequences@2 = merge_descending_pairs(Sequences, Compare, []),
            merge_all(Sequences@2, ascending, Compare)
    end.

-file("src/gleam/list.gleam", 1220).
?DOC(
    " Sorts from smallest to largest based upon the ordering specified by a given\n"
    " function.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " import gleam/int\n"
    "\n"
    " sort([4, 3, 6, 5, 4, 1, 2], by: int.compare)\n"
    " // -> [1, 2, 3, 4, 4, 5, 6]\n"
    " ```\n"
).
-spec sort(list(AJI), fun((AJI, AJI) -> gleam@order:order())) -> list(AJI).
sort(List, Compare) ->
    case List of
        [] ->
            [];

        [X] ->
            [X];

        [X@1, Y | Rest] ->
            Direction = case Compare(X@1, Y) of
                lt ->
                    ascending;

                eq ->
                    ascending;

                gt ->
                    descending
            end,
            Sequences = sequences(Rest, Compare, [X@1], Direction, Y, []),
            merge_all(Sequences, ascending, Compare)
    end.

-file("src/gleam/list.gleam", 1497).
-spec range_loop(integer(), integer(), list(integer())) -> list(integer()).
range_loop(Start, Stop, Acc) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            [Stop | Acc];

        gt ->
            range_loop(Start, Stop + 1, [Stop | Acc]);

        lt ->
            range_loop(Start, Stop - 1, [Stop | Acc])
    end.

-file("src/gleam/list.gleam", 1493).
?DOC(
    " Creates a list of ints ranging from a given start and finish.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " range(0, 0)\n"
    " // -> [0]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " range(0, 5)\n"
    " // -> [0, 1, 2, 3, 4, 5]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " range(1, -5)\n"
    " // -> [1, 0, -1, -2, -3, -4, -5]\n"
    " ```\n"
).
-spec range(integer(), integer()) -> list(integer()).
range(Start, Stop) ->
    range_loop(Start, Stop, []).

-file("src/gleam/list.gleam", 1523).
-spec repeat_loop(AKX, integer(), list(AKX)) -> list(AKX).
repeat_loop(Item, Times, Acc) ->
    case Times =< 0 of
        true ->
            Acc;

        false ->
            repeat_loop(Item, Times - 1, [Item | Acc])
    end.

-file("src/gleam/list.gleam", 1519).
?DOC(
    " Builds a list of a given value a given number of times.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " repeat(\"a\", times: 0)\n"
    " // -> []\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " repeat(\"a\", times: 5)\n"
    " // -> [\"a\", \"a\", \"a\", \"a\", \"a\"]\n"
    " ```\n"
).
-spec repeat(AKV, integer()) -> list(AKV).
repeat(A, Times) ->
    repeat_loop(A, Times, []).

-file("src/gleam/list.gleam", 1556).
-spec split_loop(list(ALE), integer(), list(ALE)) -> {list(ALE), list(ALE)}.
split_loop(List, N, Taken) ->
    case N =< 0 of
        true ->
            {lists:reverse(Taken), List};

        false ->
            case List of
                [] ->
                    {lists:reverse(Taken), []};

                [First | Rest] ->
                    split_loop(Rest, N - 1, [First | Taken])
            end
    end.

-file("src/gleam/list.gleam", 1552).
?DOC(
    " Splits a list in two before the given index.\n"
    "\n"
    " If the list is not long enough to have the given index the before list will\n"
    " be the input list, and the after list will be empty.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " split([6, 7, 8, 9], 0)\n"
    " // -> #([], [6, 7, 8, 9])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " split([6, 7, 8, 9], 2)\n"
    " // -> #([6, 7], [8, 9])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " split([6, 7, 8, 9], 4)\n"
    " // -> #([6, 7, 8, 9], [])\n"
    " ```\n"
).
-spec split(list(ALA), integer()) -> {list(ALA), list(ALA)}.
split(List, Index) ->
    split_loop(List, Index, []).

-file("src/gleam/list.gleam", 1592).
-spec split_while_loop(list(ALN), fun((ALN) -> boolean()), list(ALN)) -> {list(ALN),
    list(ALN)}.
split_while_loop(List, F, Acc) ->
    case List of
        [] ->
            {lists:reverse(Acc), []};

        [First | Rest] ->
            case F(First) of
                true ->
                    split_while_loop(Rest, F, [First | Acc]);

                false ->
                    {lists:reverse(Acc), List}
            end
    end.

-file("src/gleam/list.gleam", 1585).
?DOC(
    " Splits a list in two before the first element that a given function returns\n"
    " `False` for.\n"
    "\n"
    " If the function returns `True` for all elements the first list will be the\n"
    " input list, and the second list will be empty.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " split_while([1, 2, 3, 4, 5], fn(x) { x <= 3 })\n"
    " // -> #([1, 2, 3], [4, 5])\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " split_while([1, 2, 3, 4, 5], fn(x) { x <= 5 })\n"
    " // -> #([1, 2, 3, 4, 5], [])\n"
    " ```\n"
).
-spec split_while(list(ALJ), fun((ALJ) -> boolean())) -> {list(ALJ), list(ALJ)}.
split_while(List, Predicate) ->
    split_while_loop(List, Predicate, []).

-file("src/gleam/list.gleam", 1632).
?DOC(
    " Given a list of 2-element tuples, finds the first tuple that has a given\n"
    " key as the first element and returns the second element.\n"
    "\n"
    " If no tuple is found with the given key then `Error(Nil)` is returned.\n"
    "\n"
    " This function may be useful for interacting with Erlang code where lists of\n"
    " tuples are common.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " key_find([#(\"a\", 0), #(\"b\", 1)], \"a\")\n"
    " // -> Ok(0)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " key_find([#(\"a\", 0), #(\"b\", 1)], \"b\")\n"
    " // -> Ok(1)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " key_find([#(\"a\", 0), #(\"b\", 1)], \"c\")\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec key_find(list({ALS, ALT}), ALS) -> {ok, ALT} | {error, nil}.
key_find(Keyword_list, Desired_key) ->
    find_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-file("src/gleam/list.gleam", 1663).
?DOC(
    " Given a list of 2-element tuples, finds all tuples that have a given\n"
    " key as the first element and returns the second element.\n"
    "\n"
    " This function may be useful for interacting with Erlang code where lists of\n"
    " tuples are common.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " key_filter([#(\"a\", 0), #(\"b\", 1), #(\"a\", 2)], \"a\")\n"
    " // -> [0, 2]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " key_filter([#(\"a\", 0), #(\"b\", 1)], \"c\")\n"
    " // -> []\n"
    " ```\n"
).
-spec key_filter(list({ALX, ALY}), ALX) -> list(ALY).
key_filter(Keyword_list, Desired_key) ->
    filter_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-file("src/gleam/list.gleam", 1703).
-spec key_pop_loop(list({AMH, AMI}), AMH, list({AMH, AMI})) -> {ok,
        {AMI, list({AMH, AMI})}} |
    {error, nil}.
key_pop_loop(List, Key, Checked) ->
    case List of
        [] ->
            {error, nil};

        [{K, V} | Rest] when K =:= Key ->
            {ok, {V, lists:reverse(Checked, Rest)}};

        [First | Rest@1] ->
            key_pop_loop(Rest@1, Key, [First | Checked])
    end.

-file("src/gleam/list.gleam", 1699).
?DOC(
    " Given a list of 2-element tuples, finds the first tuple that has a given\n"
    " key as the first element. This function will return the second element\n"
    " of the found tuple and list with tuple removed.\n"
    "\n"
    " If no tuple is found with the given key then `Error(Nil)` is returned.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " key_pop([#(\"a\", 0), #(\"b\", 1)], \"a\")\n"
    " // -> Ok(#(0, [#(\"b\", 1)]))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " key_pop([#(\"a\", 0), #(\"b\", 1)], \"b\")\n"
    " // -> Ok(#(1, [#(\"a\", 0)]))\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " key_pop([#(\"a\", 0), #(\"b\", 1)], \"c\")\n"
    " // -> Error(Nil)\n"
    " ```\n"
).
-spec key_pop(list({AMB, AMC}), AMB) -> {ok, {AMC, list({AMB, AMC})}} |
    {error, nil}.
key_pop(List, Key) ->
    key_pop_loop(List, Key, []).

-file("src/gleam/list.gleam", 1737).
-spec key_set_loop(list({AMS, AMT}), AMS, AMT, list({AMS, AMT})) -> list({AMS,
    AMT}).
key_set_loop(List, Key, Value, Inspected) ->
    case List of
        [{K, _} | Rest] when K =:= Key ->
            lists:reverse(Inspected, [{K, Value} | Rest]);

        [First | Rest@1] ->
            key_set_loop(Rest@1, Key, Value, [First | Inspected]);

        [] ->
            lists:reverse([{Key, Value} | Inspected])
    end.

-file("src/gleam/list.gleam", 1733).
?DOC(
    " Given a list of 2-element tuples, inserts a key and value into the list.\n"
    "\n"
    " If there was already a tuple with the key then it is replaced, otherwise it\n"
    " is added to the end of the list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " key_set([#(5, 0), #(4, 1)], 4, 100)\n"
    " // -> [#(5, 0), #(4, 100)]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " key_set([#(5, 0), #(4, 1)], 1, 100)\n"
    " // -> [#(5, 0), #(4, 1), #(1, 100)]\n"
    " ```\n"
).
-spec key_set(list({AMO, AMP}), AMO, AMP) -> list({AMO, AMP}).
key_set(List, Key, Value) ->
    key_set_loop(List, Key, Value, []).

-file("src/gleam/list.gleam", 1765).
?DOC(
    " Calls a function for each element in a list, discarding the return value.\n"
    "\n"
    " Useful for calling a side effect for every item of a list.\n"
    "\n"
    " ```gleam\n"
    " import gleam/io\n"
    "\n"
    " each([\"1\", \"2\", \"3\"], io.println)\n"
    " // -> Nil\n"
    " // 1\n"
    " // 2\n"
    " // 3\n"
    " ```\n"
).
-spec each(list(AMX), fun((AMX) -> any())) -> nil.
each(List, F) ->
    case List of
        [] ->
            nil;

        [First | Rest] ->
            F(First),
            each(Rest, F)
    end.

-file("src/gleam/list.gleam", 1791).
?DOC(
    " Calls a `Result` returning function for each element in a list, discarding\n"
    " the return value. If the function returns `Error` then the iteration is\n"
    " stopped and the error is returned.\n"
    "\n"
    " Useful for calling a side effect for every item of a list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " try_each(\n"
    "   over: [1, 2, 3],\n"
    "   with: function_that_might_fail,\n"
    " )\n"
    " // -> Ok(Nil)\n"
    " ```\n"
).
-spec try_each(list(ANA), fun((ANA) -> {ok, any()} | {error, AND})) -> {ok, nil} |
    {error, AND}.
try_each(List, Fun) ->
    case List of
        [] ->
            {ok, nil};

        [First | Rest] ->
            case Fun(First) of
                {ok, _} ->
                    try_each(Rest, Fun);

                {error, E} ->
                    {error, E}
            end
    end.

-file("src/gleam/list.gleam", 1824).
-spec partition_loop(list(BGT), fun((BGT) -> boolean()), list(BGT), list(BGT)) -> {list(BGT),
    list(BGT)}.
partition_loop(List, Categorise, Trues, Falses) ->
    case List of
        [] ->
            {lists:reverse(Trues), lists:reverse(Falses)};

        [First | Rest] ->
            case Categorise(First) of
                true ->
                    partition_loop(Rest, Categorise, [First | Trues], Falses);

                false ->
                    partition_loop(Rest, Categorise, Trues, [First | Falses])
            end
    end.

-file("src/gleam/list.gleam", 1817).
?DOC(
    " Partitions a list into a tuple/pair of lists\n"
    " by a given categorisation function.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " import gleam/int\n"
    "\n"
    " [1, 2, 3, 4, 5] |> partition(int.is_odd)\n"
    " // -> #([1, 3, 5], [2, 4])\n"
    " ```\n"
).
-spec partition(list(ANI), fun((ANI) -> boolean())) -> {list(ANI), list(ANI)}.
partition(List, Categorise) ->
    partition_loop(List, Categorise, [], []).

-file("src/gleam/list.gleam", 1904).
-spec window_loop(list(list(AOP)), list(AOP), integer()) -> list(list(AOP)).
window_loop(Acc, List, N) ->
    Window = take(List, N),
    case erlang:length(Window) =:= N of
        true ->
            window_loop([Window | Acc], drop(List, 1), N);

        false ->
            lists:reverse(Acc)
    end.

-file("src/gleam/list.gleam", 1897).
?DOC(
    " Returns a list of sliding windows.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " window([1,2,3,4,5], 3)\n"
    " // -> [[1, 2, 3], [2, 3, 4], [3, 4, 5]]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " window([1, 2], 4)\n"
    " // -> []\n"
    " ```\n"
).
-spec window(list(AOL), integer()) -> list(list(AOL)).
window(List, N) ->
    case N =< 0 of
        true ->
            [];

        false ->
            window_loop([], List, N)
    end.

-file("src/gleam/list.gleam", 1927).
?DOC(
    " Returns a list of tuples containing two contiguous elements.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " window_by_2([1,2,3,4])\n"
    " // -> [#(1, 2), #(2, 3), #(3, 4)]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " window_by_2([1])\n"
    " // -> []\n"
    " ```\n"
).
-spec window_by_2(list(AOV)) -> list({AOV, AOV}).
window_by_2(List) ->
    zip(List, drop(List, 1)).

-file("src/gleam/list.gleam", 1940).
?DOC(
    " Drops the first elements in a given list for which the predicate function returns `True`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " drop_while([1, 2, 3, 4], fn (x) { x < 3 })\n"
    " // -> [3, 4]\n"
    " ```\n"
).
-spec drop_while(list(AOY), fun((AOY) -> boolean())) -> list(AOY).
drop_while(List, Predicate) ->
    case List of
        [] ->
            [];

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    drop_while(Rest, Predicate);

                false ->
                    [First | Rest]
            end
    end.

-file("src/gleam/list.gleam", 1970).
-spec take_while_loop(list(APE), fun((APE) -> boolean()), list(APE)) -> list(APE).
take_while_loop(List, Predicate, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            case Predicate(First) of
                true ->
                    take_while_loop(Rest, Predicate, [First | Acc]);

                false ->
                    lists:reverse(Acc)
            end
    end.

-file("src/gleam/list.gleam", 1963).
?DOC(
    " Takes the first elements in a given list for which the predicate function returns `True`.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " take_while([1, 2, 3, 2, 4], fn (x) { x < 3 })\n"
    " // -> [1, 2]\n"
    " ```\n"
).
-spec take_while(list(APB), fun((APB) -> boolean())) -> list(APB).
take_while(List, Predicate) ->
    take_while_loop(List, Predicate, []).

-file("src/gleam/list.gleam", 2002).
-spec chunk_loop(list(APN), fun((APN) -> APP), APP, list(APN), list(list(APN))) -> list(list(APN)).
chunk_loop(List, F, Previous_key, Current_chunk, Acc) ->
    case List of
        [First | Rest] ->
            Key = F(First),
            case Key =:= Previous_key of
                true ->
                    chunk_loop(Rest, F, Key, [First | Current_chunk], Acc);

                false ->
                    New_acc = [lists:reverse(Current_chunk) | Acc],
                    chunk_loop(Rest, F, Key, [First], New_acc)
            end;

        [] ->
            lists:reverse([lists:reverse(Current_chunk) | Acc])
    end.

-file("src/gleam/list.gleam", 1995).
?DOC(
    " Returns a list of chunks in which\n"
    " the return value of calling `f` on each element is the same.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [1, 2, 2, 3, 4, 4, 6, 7, 7] |> chunk(by: fn(n) { n % 2 })\n"
    " // -> [[1], [2, 2], [3], [4, 4, 6], [7, 7]]\n"
    " ```\n"
).
-spec chunk(list(API), fun((API) -> any())) -> list(list(API)).
chunk(List, F) ->
    case List of
        [] ->
            [];

        [First | Rest] ->
            chunk_loop(Rest, F, F(First), [First], [])
    end.

-file("src/gleam/list.gleam", 2047).
-spec sized_chunk_loop(
    list(APZ),
    integer(),
    integer(),
    list(APZ),
    list(list(APZ))
) -> list(list(APZ)).
sized_chunk_loop(List, Count, Left, Current_chunk, Acc) ->
    case List of
        [] ->
            case Current_chunk of
                [] ->
                    lists:reverse(Acc);

                Remaining ->
                    lists:reverse([lists:reverse(Remaining) | Acc])
            end;

        [First | Rest] ->
            Chunk = [First | Current_chunk],
            case Left > 1 of
                true ->
                    sized_chunk_loop(Rest, Count, Left - 1, Chunk, Acc);

                false ->
                    sized_chunk_loop(
                        Rest,
                        Count,
                        Count,
                        [],
                        [lists:reverse(Chunk) | Acc]
                    )
            end
    end.

-file("src/gleam/list.gleam", 2043).
?DOC(
    " Returns a list of chunks containing `count` elements each.\n"
    "\n"
    " If the last chunk does not have `count` elements, it is instead\n"
    " a partial chunk, with less than `count` elements.\n"
    "\n"
    " For any `count` less than 1 this function behaves as if it was set to 1.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [1, 2, 3, 4, 5, 6] |> sized_chunk(into: 2)\n"
    " // -> [[1, 2], [3, 4], [5, 6]]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " [1, 2, 3, 4, 5, 6, 7, 8] |> sized_chunk(into: 3)\n"
    " // -> [[1, 2, 3], [4, 5, 6], [7, 8]]\n"
    " ```\n"
).
-spec sized_chunk(list(APV), integer()) -> list(list(APV)).
sized_chunk(List, Count) ->
    sized_chunk_loop(List, Count, Count, [], []).

-file("src/gleam/list.gleam", 2091).
?DOC(
    " This function acts similar to fold, but does not take an initial state.\n"
    " Instead, it starts from the first element in the list\n"
    " and combines it with each subsequent element in turn using the given\n"
    " function. The function is called as `fun(accumulator, current_element)`.\n"
    "\n"
    " Returns `Ok` to indicate a successful run, and `Error` if called on an\n"
    " empty list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " [] |> reduce(fn(acc, x) { acc + x })\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " [1, 2, 3, 4, 5] |> reduce(fn(acc, x) { acc + x })\n"
    " // -> Ok(15)\n"
    " ```\n"
).
-spec reduce(list(AQG), fun((AQG, AQG) -> AQG)) -> {ok, AQG} | {error, nil}.
reduce(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [First | Rest] ->
            {ok, fold(Rest, First, Fun)}
    end.

-file("src/gleam/list.gleam", 2115).
-spec scan_loop(list(AQO), AQQ, list(AQQ), fun((AQQ, AQO) -> AQQ)) -> list(AQQ).
scan_loop(List, Accumulator, Accumulated, Fun) ->
    case List of
        [] ->
            lists:reverse(Accumulated);

        [First | Rest] ->
            Next = Fun(Accumulator, First),
            scan_loop(Rest, Next, [Next | Accumulated], Fun)
    end.

-file("src/gleam/list.gleam", 2107).
?DOC(
    " Similar to `fold`, but yields the state of the accumulator at each stage.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " scan(over: [1, 2, 3], from: 100, with: fn(acc, i) { acc + i })\n"
    " // -> [101, 103, 106]\n"
    " ```\n"
).
-spec scan(list(AQK), AQM, fun((AQM, AQK) -> AQM)) -> list(AQM).
scan(List, Initial, Fun) ->
    scan_loop(List, Initial, [], Fun).

-file("src/gleam/list.gleam", 2148).
?DOC(
    " Returns the last element in the given list.\n"
    "\n"
    " Returns `Error(Nil)` if the list is empty.\n"
    "\n"
    " This function runs in linear time.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " last([])\n"
    " // -> Error(Nil)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " last([1, 2, 3, 4, 5])\n"
    " // -> Ok(5)\n"
    " ```\n"
).
-spec last(list(AQT)) -> {ok, AQT} | {error, nil}.
last(List) ->
    case List of
        [] ->
            {error, nil};

        [Last] ->
            {ok, Last};

        [_ | Rest] ->
            last(Rest)
    end.

-file("src/gleam/list.gleam", 2170).
?DOC(
    " Return unique combinations of elements in the list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " combinations([1, 2, 3], 2)\n"
    " // -> [[1, 2], [1, 3], [2, 3]]\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " combinations([1, 2, 3, 4], 3)\n"
    " // -> [[1, 2, 3], [1, 2, 4], [1, 3, 4], [2, 3, 4]]\n"
    " ```\n"
).
-spec combinations(list(AQX), integer()) -> list(list(AQX)).
combinations(Items, N) ->
    case {N, Items} of
        {0, _} ->
            [[]];

        {_, []} ->
            [];

        {_, [First | Rest]} ->
            _pipe = Rest,
            _pipe@1 = combinations(_pipe, N - 1),
            _pipe@2 = map(
                _pipe@1,
                fun(Combination) -> [First | Combination] end
            ),
            _pipe@3 = lists:reverse(_pipe@2),
            fold(_pipe@3, combinations(Rest, N), fun(Acc, C) -> [C | Acc] end)
    end.

-file("src/gleam/list.gleam", 2196).
-spec combination_pairs_loop(list(ARE), list({ARE, ARE})) -> list({ARE, ARE}).
combination_pairs_loop(Items, Acc) ->
    case Items of
        [] ->
            lists:reverse(Acc);

        [First | Rest] ->
            First_combinations = map(Rest, fun(Other) -> {First, Other} end),
            Acc@1 = lists:reverse(First_combinations, Acc),
            combination_pairs_loop(Rest, Acc@1)
    end.

-file("src/gleam/list.gleam", 2192).
?DOC(
    " Return unique pair combinations of elements in the list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " combination_pairs([1, 2, 3])\n"
    " // -> [#(1, 2), #(1, 3), #(2, 3)]\n"
    " ```\n"
).
-spec combination_pairs(list(ARB)) -> list({ARB, ARB}).
combination_pairs(Items) ->
    combination_pairs_loop(Items, []).

-file("src/gleam/list.gleam", 2252).
-spec take_firsts(list(list(ARY)), list(ARY), list(list(ARY))) -> {list(ARY),
    list(list(ARY))}.
take_firsts(Rows, Column, Remaining_rows) ->
    case Rows of
        [] ->
            {lists:reverse(Column), lists:reverse(Remaining_rows)};

        [[] | Rest] ->
            take_firsts(Rest, Column, Remaining_rows);

        [[First | Remaining_row] | Rest_rows] ->
            Remaining_rows@1 = [Remaining_row | Remaining_rows],
            take_firsts(Rest_rows, [First | Column], Remaining_rows@1)
    end.

-file("src/gleam/list.gleam", 2239).
-spec transpose_loop(list(list(ARR)), list(list(ARR))) -> list(list(ARR)).
transpose_loop(Rows, Columns) ->
    case Rows of
        [] ->
            lists:reverse(Columns);

        _ ->
            {Column, Rest} = take_firsts(Rows, [], []),
            case Column of
                [_ | _] ->
                    transpose_loop(Rest, [Column | Columns]);

                [] ->
                    transpose_loop(Rest, Columns)
            end
    end.

-file("src/gleam/list.gleam", 2235).
?DOC(
    " Transpose rows and columns of the list of lists.\n"
    "\n"
    " Notice: This function is not tail recursive,\n"
    " and thus may exceed stack size if called,\n"
    " with large lists (on the JavaScript target).\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " transpose([[1, 2, 3], [101, 102, 103]])\n"
    " // -> [[1, 101], [2, 102], [3, 103]]\n"
    " ```\n"
).
-spec transpose(list(list(ARM))) -> list(list(ARM)).
transpose(List_of_lists) ->
    transpose_loop(List_of_lists, []).

-file("src/gleam/list.gleam", 2216).
?DOC(
    " Make a list alternating the elements from the given lists\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " interleave([[1, 2], [101, 102], [201, 202]])\n"
    " // -> [1, 101, 201, 2, 102, 202]\n"
    " ```\n"
).
-spec interleave(list(list(ARI))) -> list(ARI).
interleave(List) ->
    _pipe = List,
    _pipe@1 = transpose(_pipe),
    lists:append(_pipe@1).

-file("src/gleam/list.gleam", 2285).
-spec shuffle_pair_unwrap_loop(list({float(), ASK}), list(ASK)) -> list(ASK).
shuffle_pair_unwrap_loop(List, Acc) ->
    case List of
        [] ->
            Acc;

        [Elem_pair | Enumerable] ->
            shuffle_pair_unwrap_loop(
                Enumerable,
                [erlang:element(2, Elem_pair) | Acc]
            )
    end.

-file("src/gleam/list.gleam", 2293).
-spec do_shuffle_by_pair_indexes(list({float(), ASO})) -> list({float(), ASO}).
do_shuffle_by_pair_indexes(List_of_pairs) ->
    sort(
        List_of_pairs,
        fun(A_pair, B_pair) ->
            gleam@float:compare(
                erlang:element(1, A_pair),
                erlang:element(1, B_pair)
            )
        end
    ).

-file("src/gleam/list.gleam", 2278).
?DOC(
    " Takes a list, randomly sorts all items and returns the shuffled list.\n"
    "\n"
    " This function uses `float.random` to decide the order of the elements.\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " range(1, 10) |> shuffle()\n"
    " // -> [1, 6, 9, 10, 3, 8, 4, 2, 7, 5]\n"
    " ```\n"
).
-spec shuffle(list(ASH)) -> list(ASH).
shuffle(List) ->
    _pipe = List,
    _pipe@1 = fold(_pipe, [], fun(Acc, A) -> [{rand:uniform(), A} | Acc] end),
    _pipe@2 = do_shuffle_by_pair_indexes(_pipe@1),
    shuffle_pair_unwrap_loop(_pipe@2, []).

-file("src/gleam/list.gleam", 2325).
-spec max_loop(list(ASY), fun((ASY, ASY) -> gleam@order:order()), ASY) -> ASY.
max_loop(List, Compare, Max) ->
    case List of
        [] ->
            Max;

        [First | Rest] ->
            case Compare(First, Max) of
                gt ->
                    max_loop(Rest, Compare, First);

                lt ->
                    max_loop(Rest, Compare, Max);

                eq ->
                    max_loop(Rest, Compare, Max)
            end
    end.

-file("src/gleam/list.gleam", 2315).
?DOC(
    " Takes a list and a comparator, and returns the maximum element in the list\n"
    "\n"
    "\n"
    " ## Example\n"
    "\n"
    " ```gleam\n"
    " range(1, 10) |> list.max(int.compare)\n"
    " // -> Ok(10)\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " [\"a\", \"c\", \"b\"] |> list.max(string.compare)\n"
    " // -> Ok(\"c\")\n"
    " ```\n"
).
-spec max(list(ASR), fun((ASR, ASR) -> gleam@order:order())) -> {ok, ASR} |
    {error, nil}.
max(List, Compare) ->
    case List of
        [] ->
            {error, nil};

        [First | Rest] ->
            {ok, max_loop(Rest, Compare, First)}
    end.

-file("src/gleam/list.gleam", 2406).
-spec build_reservoir_loop(
    list(ATN),
    integer(),
    gleam@dict:dict(integer(), ATN)
) -> {gleam@dict:dict(integer(), ATN), list(ATN)}.
build_reservoir_loop(List, Size, Reservoir) ->
    Reservoir_size = maps:size(Reservoir),
    case Reservoir_size >= Size of
        true ->
            {Reservoir, List};

        false ->
            case List of
                [] ->
                    {Reservoir, []};

                [First | Rest] ->
                    Reservoir@1 = gleam@dict:insert(
                        Reservoir,
                        Reservoir_size,
                        First
                    ),
                    build_reservoir_loop(Rest, Size, Reservoir@1)
            end
    end.

-file("src/gleam/list.gleam", 2402).
?DOC(
    " Builds the initial reservoir used by Algorithm L.\n"
    " This is a dictionary with keys ranging from `0` up to `n - 1` where each\n"
    " value is the corresponding element at that position in `list`.\n"
    "\n"
    " This also returns the remaining elements of `list` that didn't end up in\n"
    " the reservoir.\n"
).
-spec build_reservoir(list(ATI), integer()) -> {gleam@dict:dict(integer(), ATI),
    list(ATI)}.
build_reservoir(List, N) ->
    build_reservoir_loop(List, N, maps:new()).

-file("src/gleam/list.gleam", 2390).
-spec log_random() -> float().
log_random() ->
    Random@1 = case gleam@float:logarithm(
        rand:uniform() + 2.2250738585072014e-308
    ) of
        {ok, Random} -> Random;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"gleam/list"/utf8>>,
                        function => <<"log_random"/utf8>>,
                        line => 2391,
                        value => _assert_fail,
                        start => 56078,
                        'end' => 56149,
                        pattern_start => 56089,
                        pattern_end => 56099})
    end,
    Random@1.

-file("src/gleam/list.gleam", 2367).
-spec sample_loop(
    list(ATC),
    gleam@dict:dict(integer(), ATC),
    integer(),
    float()
) -> gleam@dict:dict(integer(), ATC).
sample_loop(List, Reservoir, N, W) ->
    Skip = begin
        Log@1 = case gleam@float:logarithm(1.0 - W) of
            {ok, Log} -> Log;
            _assert_fail ->
                erlang:error(#{gleam_error => let_assert,
                            message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                            file => <<?FILEPATH/utf8>>,
                            module => <<"gleam/list"/utf8>>,
                            function => <<"sample_loop"/utf8>>,
                            line => 2374,
                            value => _assert_fail,
                            start => 55639,
                            'end' => 55685,
                            pattern_start => 55650,
                            pattern_end => 55657})
        end,
        erlang:round(math:floor(case Log@1 of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator -> log_random() / Gleam@denominator
                end))
    end,
    case drop(List, Skip) of
        [] ->
            Reservoir;

        [First | Rest] ->
            Reservoir@1 = gleam@dict:insert(
                Reservoir,
                gleam@int:random(N),
                First
            ),
            W@1 = W * math:exp(case erlang:float(N) of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator@1 -> log_random() / Gleam@denominator@1
                end),
            sample_loop(Rest, Reservoir@1, N, W@1)
    end.

-file("src/gleam/list.gleam", 2349).
?DOC(
    " Returns a random sample of up to n elements from a list using reservoir\n"
    " sampling via [Algorithm L](https://en.wikipedia.org/wiki/Reservoir_sampling#Optimal:_Algorithm_L).\n"
    " Returns an empty list if the sample size is less than or equal to 0.\n"
    "\n"
    " Order is not random, only selection is.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " reservoir_sample([1, 2, 3, 4, 5], 3)\n"
    " // -> [2, 4, 5]  // A random sample of 3 items\n"
    " ```\n"
).
-spec sample(list(ASZ), integer()) -> list(ASZ).
sample(List, N) ->
    {Reservoir, Rest} = build_reservoir(List, N),
    case gleam@dict:is_empty(Reservoir) of
        true ->
            [];

        false ->
            W = math:exp(case erlang:float(N) of
                    +0.0 -> +0.0;
                    -0.0 -> -0.0;
                    Gleam@denominator -> log_random() / Gleam@denominator
                end),
            maps:values(sample_loop(Rest, Reservoir, N, W))
    end.

-file("src/gleam/list.gleam", 1851).
-spec permutation_zip(list(ANV), list(ANV), list(list(ANV))) -> list(list(ANV)).
permutation_zip(List, Rest, Acc) ->
    case List of
        [] ->
            lists:reverse(Acc);

        [Head | Tail] ->
            permutation_prepend(
                Head,
                permutations(lists:reverse(Rest, Tail)),
                Tail,
                [Head | Rest],
                Acc
            )
    end.

-file("src/gleam/list.gleam", 1869).
-spec permutation_prepend(
    AOC,
    list(list(AOC)),
    list(AOC),
    list(AOC),
    list(list(AOC))
) -> list(list(AOC)).
permutation_prepend(El, Permutations, List_1, List_2, Acc) ->
    case Permutations of
        [] ->
            permutation_zip(List_1, List_2, Acc);

        [Head | Tail] ->
            permutation_prepend(El, Tail, List_1, List_2, [[El | Head] | Acc])
    end.

-file("src/gleam/list.gleam", 1844).
?DOC(
    " Returns all the permutations of a list.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " permutations([1, 2])\n"
    " // -> [[1, 2], [2, 1]]\n"
    " ```\n"
).
-spec permutations(list(ANR)) -> list(list(ANR)).
permutations(List) ->
    case List of
        [] ->
            [[]];

        L ->
            permutation_zip(L, [], [])
    end.
