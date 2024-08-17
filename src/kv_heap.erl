-module(kv_heap).

%% API
-export([new/0, insert/3, take_value/1, size/1, from_list/1]).

-export_type([kv_heap/0]).

-type kv_heap() :: HeapMap :: #{Index :: non_neg_integer() => {Key :: term(), Value :: term()}}.

%%------------------------------------------------------------------------------
%% @doc 创建一个最小堆
%%------------------------------------------------------------------------------
-spec new() -> kv_heap().
new() ->
    #{}.

%%------------------------------------------------------------------------------
%% @doc 添加一个key，value到最小堆中
%%------------------------------------------------------------------------------
-spec insert(Key :: term(), Value :: term(), MinHeap :: kv_heap()) -> NewMinHeap :: kv_heap().
insert(Key, Value, MinHeap) ->
    KV = {Key, Value},
    Index = maps:size(MinHeap) + 1,
    HeapMap1 = MinHeap#{Index => KV},
    raise_kv(Index, KV, HeapMap1).

raise_kv(Index, {K, _} = KV, MinHeap) ->
    ParentIndex = Index bsr 1,
    case MinHeap of
        #{ParentIndex := {PKey, _} = PKV} when PKey > K ->
            HeapMap1 = MinHeap#{ParentIndex := KV, Index := PKV},
            raise_kv(ParentIndex, KV, HeapMap1);
        MinHeap ->
            MinHeap
    end.

%%------------------------------------------------------------------------------
%% @doc 取出堆顶的值
%%------------------------------------------------------------------------------
-spec take_value(MinHeap :: kv_heap()) -> {Value :: term(), NewMinHeap :: kv_heap()} | empty.
take_value(#{1 := {_, Value}} = MinHeap) ->
    case maps:size(MinHeap) of
        1 ->
            {Value, #{}};
        TailIndex ->
            {TailKV, HeapMap1} = maps:take(TailIndex, MinHeap),
            HeapMap2 = HeapMap1#{1 := TailKV},
            HeapMap3 = decline_kv(1, TailKV, HeapMap2),
            {Value, HeapMap3}
    end;
take_value(HeapMap) when map_size(HeapMap) =:= 0 ->
    empty.

decline_kv(Index, KV, MinHeap) ->
    LeftIndex = Index bsl 1,
    RightIndex = LeftIndex + 1,
    case MinHeap of
        #{LeftIndex := {LK, _} = LeftKV, RightIndex := {RK, _}} when LK < RK andalso element(1, KV) > LK ->
            HeapMap1 = MinHeap#{Index := LeftKV, LeftIndex := KV},
            decline_kv(LeftIndex, KV, HeapMap1);
        #{LeftIndex := {LK, _}, RightIndex := {RK, _} = RightKV} when LK > RK andalso element(1, KV) > RK ->
            HeapMap1 = MinHeap#{Index := RightKV, RightIndex := KV},
            decline_kv(RightIndex, KV, HeapMap1);
        #{LeftIndex := {LK, _} = LeftKV} when element(1, KV) > LK ->
            HeapMap1 = MinHeap#{Index := LeftKV, LeftIndex := KV},
            decline_kv(LeftIndex, KV, HeapMap1);
        _ ->
            MinHeap
    end.

%%------------------------------------------------------------------------------
%% @doc 获取堆的大小
%%------------------------------------------------------------------------------
-spec size(MinHeap :: kv_heap()) -> Size :: non_neg_integer().
size(MinHeap) ->
    maps:size(MinHeap).

%%------------------------------------------------------------------------------
%% @doc 通过列表创建最小堆
%%------------------------------------------------------------------------------
-spec from_list(L :: [{K :: term(), V :: term()}]) -> MinHeap :: kv_heap().
from_list(L) ->
    from_list_1(L, new()).

from_list_1([{K, V} | T], MinHeap) ->
    from_list_1(T, insert(K, V, MinHeap));
from_list_1([], MinHeap) ->
    MinHeap.
