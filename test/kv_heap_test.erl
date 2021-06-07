-module(kv_heap_test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    new(),
    ElemSize = 33,
    MiniHeap = from_list(ElemSize),
    MiniHeap1 = take_elem(MiniHeap),
    MiniHeap2 = add_element(MiniHeap1),
    size(ElemSize, MiniHeap2),
    take_all(ElemSize, MiniHeap2).

new() ->
    MiniHeap = kv_heap:new(),
    ?assert(is_map(MiniHeap)).


from_list(ElemSize) ->
    MiniHeap = kv_heap:from_list([{N, N} || N <- lists:seq(ElemSize, 1, -1)]),
    ?assert(is_map(MiniHeap)),
    MiniHeap.

take_elem(MiniHeap) ->
    {Value, MiniHeap1} = kv_heap:take_value(MiniHeap),
    ?assert(Value =:= 1),
    ?assert(is_map(MiniHeap1)),
    ?assert(maps:size(MiniHeap) - 1 =:= maps:size(MiniHeap1)),
    MiniHeap1.

add_element(MiniHeap) ->
    MiniHeap1 = kv_heap:insert(1, 1, MiniHeap),
    ?assert(is_map(MiniHeap1)),
    ?assert(maps:size(MiniHeap) + 1 =:= maps:size(MiniHeap1)),
    MiniHeap1.

size(ElemSize, MiniHeap) ->
    ?assert(ElemSize =:= kv_heap:size(MiniHeap)).

take_all(ElemSize, MiniHeap) ->
    ?assert(take_all_1(MiniHeap) =:= lists:seq(1, ElemSize)).

take_all_1(MiniHeap) ->
    case kv_heap:take_value(MiniHeap) of
        {Value, MiniHeap1} ->
%%            ?debugFmt("Value:~w MiniHeap:~w~n~n", [Value, MiniHeap1]),
            [Value | take_all_1(MiniHeap1)];
        empty ->
            []
    end.