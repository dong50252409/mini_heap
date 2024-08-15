-module(elem_heap).

-export([new/0, add_element/2, take_element/1, size/1, from_list/1]).

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
-spec add_element(Elem :: term(), MiniHeap :: elem_heap()) -> NewMiniHeap :: elem_heap().
add_element(Elem, MiniHeap) ->
    Index = maps:size(MiniHeap) + 1,
    HeapMap1 = MiniHeap#{Index => Elem},
    raise_elem(Index, Elem, HeapMap1).

raise_elem(Index, Elem, MiniHeap) ->
    ParentIndex = Index bsr 1,
    case MiniHeap of
        #{ParentIndex := ParentElem} when ParentElem > Elem ->
            HeapMap1 = MiniHeap#{ParentIndex := Elem, Index := ParentElem},
            raise_elem(ParentIndex, Elem, HeapMap1);
        MiniHeap ->
            MiniHeap
    end.

%%------------------------------------------------------------------------------
%% @doc 取出堆顶的元素
%%------------------------------------------------------------------------------
-spec take_element(MiniHeap :: elem_heap()) -> {Elem :: term(), NewMiniHeap :: elem_heap()} | empty.
take_element(#{1 := Elem} = MiniHeap) ->
    case maps:size(MiniHeap) of
        1 ->
            {Elem, #{}};
        TailIndex ->
            {TailElem, HeapMap1} = maps:take(TailIndex, MiniHeap),
            HeapMap2 = HeapMap1#{1 := TailElem},
            HeapMap3 = decline_elem(1, TailElem, HeapMap2),
            {Elem, HeapMap3}
    end;
take_element(MiniHeap) when map_size(MiniHeap) =:= 0 ->
    empty.

decline_elem(Index, Elem, MiniHeap) ->
    LeftIndex = Index bsl 1,
    RightIndex = LeftIndex + 1,
    case MiniHeap of
        #{LeftIndex := LeftElem, RightIndex := RightElem} when LeftElem < RightElem andalso Elem > LeftElem ->
            HeapMap1 = MiniHeap#{Index := LeftElem, LeftIndex := Elem},
            decline_elem(LeftIndex, Elem, HeapMap1);
        #{LeftIndex := LeftElem, RightIndex := RightElem} when LeftElem > RightElem andalso Elem > RightElem ->
            HeapMap1 = MiniHeap#{Index := RightElem, RightIndex := Elem},
            decline_elem(RightIndex, Elem, HeapMap1);
        #{LeftIndex := LeftElem} when Elem > LeftElem ->
            HeapMap1 = MiniHeap#{Index := LeftElem, LeftIndex := Elem},
            decline_elem(LeftIndex, Elem, HeapMap1);
        _ ->
            MiniHeap
    end.

%%------------------------------------------------------------------------------
%% @doc 获取堆的大小
%%------------------------------------------------------------------------------
-spec size(MiniHeap :: elem_heap()) -> Size :: non_neg_integer().
size(MiniHeap) ->
    maps:size(MiniHeap).

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
