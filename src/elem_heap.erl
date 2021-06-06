-module(elem_heap).

-export([new/0, add_element/2, take_element/1, size/1, from_list/1]).

-export_type([elem_heap/0]).
-type elem_heap() :: {NextIndex :: non_neg_integer(), HeapMap :: map()}.

%%------------------------------------------------------------------------------
%% @doc 创建一个最小堆
%%------------------------------------------------------------------------------
-spec new() -> elem_heap().
new() ->
    {1, #{}}.


%%------------------------------------------------------------------------------
%% @doc 添加一个元素到最小堆中
%%------------------------------------------------------------------------------
-spec add_element(Elem :: term(), MiniHeap :: elem_heap()) -> NewMiniHeap :: elem_heap().
add_element(Elem, {Index, HeapMap}) ->
    HeapMap1 = HeapMap#{Index => Elem},
    HeapMap2 = raise_elem(Index, Elem, HeapMap1),
    {Index + 1, HeapMap2}.

raise_elem(Index, Elem, HeapMap) ->
    ParentIndex = Index div 2,
    case HeapMap of
        #{ParentIndex := ParentElem} when ParentElem > Elem ->
            HeapMap1 = HeapMap#{ParentIndex := Elem, Index := ParentElem},
            raise_elem(ParentIndex, Elem, HeapMap1);
        HeapMap ->
            HeapMap
    end.

%%------------------------------------------------------------------------------
%% @doc 取出堆顶的元素
%%------------------------------------------------------------------------------
-spec take_element(MiniHeap :: elem_heap()) -> {Elem :: term(), NewMiniHeap :: elem_heap()} | empty.
take_element({NextIndex, #{1 := Elem} = HeapMap}) ->
    case NextIndex - 1 of
        1 ->
            {Elem, {1, #{}}};
        TailIndex ->
            {TailElem, HeapMap1} = maps:take(TailIndex, HeapMap),
            HeapMap2 = HeapMap1#{1 := TailElem},
            HeapMap3 = decline_elem(1, TailElem, HeapMap2),
            {Elem, {TailIndex, HeapMap3}}
    end;
take_element({1, HeapMap}) when map_size(HeapMap) =:= 0 ->
    empty.

decline_elem(Index, Elem, HeapMap) ->
    LeftIndex = Index * 2,
    RightIndex = Index * 2 + 1,
    case HeapMap of
        #{LeftIndex := LeftElem, RightIndex := RightElem} when LeftElem < RightElem ->
            HeapMap1 = HeapMap#{Index := LeftElem, LeftIndex := Elem},
            decline_elem(LeftIndex, Elem, HeapMap1);
        #{LeftIndex := LeftElem, RightIndex := RightElem} when LeftElem > RightElem ->
            HeapMap1 = HeapMap#{Index := RightElem, RightIndex := Elem},
            decline_elem(RightIndex, Elem, HeapMap1);
        _ ->
            HeapMap
    end.


%%------------------------------------------------------------------------------
%% @doc 获取堆的大小
%%------------------------------------------------------------------------------
-spec size(MiniHeap :: elem_heap()) -> Size :: non_neg_integer().
size({NextIndex, _HeapMap}) ->
    NextIndex - 1.

%%------------------------------------------------------------------------------
%% @doc 通过列表创建最小堆
%%------------------------------------------------------------------------------
-spec from_list(L :: [Elem :: term()]) -> MiniHeap :: elem_heap().
from_list(L) ->
    from_list_1(L, new()).

from_list_1([Elem | T], MiniHeap) ->
    from_list_1(T, add_element(Elem, MiniHeap));
from_list_1([], MiniHeap) ->
    MiniHeap.
