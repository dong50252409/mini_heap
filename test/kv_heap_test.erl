-module(kv_heap_test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    new(),
    ElemSize = 33,
    MimiHeap = from_list(ElemSize),
    MimiHeap1 = take_elem(MimiHeap),
    MimiHeap2 = add_element(MimiHeap1),
    take_all(ElemSize, MimiHeap2).

new() ->
    MiniHeap = kv_heap:new(),
    assert_mini_heap(MiniHeap).


from_list(ElemSize) ->
    MimiHeap = kv_heap:from_list([{N, N} || N <- lists:seq(ElemSize, 1, -1)]),
    assert_mini_heap(MimiHeap),
    MimiHeap.

take_elem(MimiHeap) ->
    ElemSize = maps:size(element(2, MimiHeap)),
    {Value, MimiHeap1} = kv_heap:take_value(MimiHeap),
    ?assert(Value =:= 1),
    assert_mini_heap(MimiHeap1),
    ?assert(ElemSize - 1 =:= maps:size(element(2, MimiHeap1))),
    MimiHeap1.

add_element(MimiHeap) ->
    ElemSize = maps:size(element(2, MimiHeap)),
    MimiHeap1 = kv_heap:insert(1, 1, MimiHeap),
    assert_mini_heap(MimiHeap1),
    ?assert(ElemSize + 1 =:= maps:size(element(2, MimiHeap1))),
    MimiHeap1.

assert_mini_heap({NextIndex, HeapMap}) ->
    ?assert(is_integer(NextIndex)),
    ?assert(is_map(HeapMap)),
    Size = map_size(HeapMap),
    ?assert(Size + 1 =:= NextIndex).

take_all(ElemSize, MimiHeap) ->
    ?assert(take_all_1(MimiHeap) =:= lists:seq(1, ElemSize)).

take_all_1(MimiHeap) ->
    case kv_heap:take_value(MimiHeap) of
        {Value, MimiHeap1} ->
%%            ?debugFmt("Value:~w MimiHeap:~w~n~n", [Value, MimiHeap1]),
            [Value | take_all_1(MimiHeap1)];
        empty ->
            []
    end.