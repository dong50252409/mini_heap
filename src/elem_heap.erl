-module(elem_heap).

-export([new/0, add_element/2, take_element/1, size/1, from_list/1, to_list/1]).

-export_type([elem_heap/0]).
-type elem_heap() :: HeapMap :: #{Index :: non_neg_integer() => Elem :: term()}.

%%------------------------------------------------------------------------------
%% @doc 创建一个最小堆
%%------------------------------------------------------------------------------
-spec new() -> elem_heap().
new() ->
    #{}.

%%------------------------------------------------------------------------------
%% @doc 添加一个元素到最小堆中
%%------------------------------------------------------------------------------
-spec add_element(Elem :: term(), MinHeap :: elem_heap()) -> NewMinHeap :: elem_heap().
add_element(Elem, MinHeap) ->
    Index = maps:size(MinHeap) + 1,
    HeapMap1 = MinHeap#{Index => Elem},
    raise_elem(Index, Elem, HeapMap1).

raise_elem(Index, Elem, MinHeap) ->
    ParentIndex = Index bsr 1,
    case MinHeap of
        #{ParentIndex := ParentElem} when ParentElem > Elem ->
            HeapMap1 = MinHeap#{ParentIndex := Elem, Index := ParentElem},
            raise_elem(ParentIndex, Elem, HeapMap1);
        MinHeap ->
            MinHeap
    end.

%%------------------------------------------------------------------------------
%% @doc 取出堆顶的元素
%%------------------------------------------------------------------------------
-spec take_element(MinHeap :: elem_heap()) -> {Elem :: term(), NewMinHeap :: elem_heap()} | empty.
take_element(#{1 := Elem} = MinHeap) ->
    case maps:size(MinHeap) of
        1 ->
            {Elem, #{}};
        TailIndex ->
            {TailElem, HeapMap1} = maps:take(TailIndex, MinHeap),
            HeapMap2 = HeapMap1#{1 := TailElem},
            HeapMap3 = decline_elem(1, TailElem, HeapMap2),
            {Elem, HeapMap3}
    end;
take_element(MinHeap) when map_size(MinHeap) =:= 0 ->
    empty.

decline_elem(Index, Elem, MinHeap) ->
    LeftIndex = Index bsl 1,
    RightIndex = LeftIndex + 1,
    case MinHeap of
        #{LeftIndex := LeftElem, RightIndex := RightElem} when LeftElem < RightElem andalso Elem > LeftElem ->
            HeapMap1 = MinHeap#{Index := LeftElem, LeftIndex := Elem},
            decline_elem(LeftIndex, Elem, HeapMap1);
        #{LeftIndex := LeftElem, RightIndex := RightElem} when LeftElem > RightElem andalso Elem > RightElem ->
            HeapMap1 = MinHeap#{Index := RightElem, RightIndex := Elem},
            decline_elem(RightIndex, Elem, HeapMap1);
        #{LeftIndex := LeftElem} when Elem > LeftElem ->
            HeapMap1 = MinHeap#{Index := LeftElem, LeftIndex := Elem},
            decline_elem(LeftIndex, Elem, HeapMap1);
        _ ->
            MinHeap
    end.

%%------------------------------------------------------------------------------
%% @doc 获取堆的大小
%%------------------------------------------------------------------------------
-spec size(MinHeap :: elem_heap()) -> Size :: non_neg_integer().
size(MinHeap) ->
    maps:size(MinHeap).

%%------------------------------------------------------------------------------
%% @doc 通过列表创建最小堆
%%------------------------------------------------------------------------------
-spec from_list(L :: [Elem :: term()]) -> MinHeap :: elem_heap().
from_list(L) ->
    from_list_1(L, new()).

from_list_1([Elem | T], MinHeap) ->
    from_list_1(T, add_element(Elem, MinHeap));
from_list_1([], MinHeap) ->
    MinHeap.

%%------------------------------------------------------------------------------
%% @doc 将最小堆转为有序列表
%%------------------------------------------------------------------------------
to_list(MinHeap) when map_size(MinHeap) > 0 ->
    {Elem, NewMinHeap} = take_element(MinHeap),
    [Elem | to_list(NewMinHeap)];
to_list(_) ->
    [].