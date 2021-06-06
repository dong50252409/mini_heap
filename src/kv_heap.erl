-module(kv_heap).

%% API
-export([new/0, insert/3, take_value/1, size/1, from_list/1]).

-export_type([kv_heap/0]).

-type kv_heap() :: {NextIndex :: non_neg_integer(), HeapMap :: map()}.

%%------------------------------------------------------------------------------
%% @doc 创建一个最小堆
%%------------------------------------------------------------------------------
-spec new() -> kv_heap().
new() ->
    {1, #{}}.

%%------------------------------------------------------------------------------
%% @doc 添加一个key，value到最小堆中
%%------------------------------------------------------------------------------
-spec insert(Key :: term(), Value :: term(), MiniHeap :: kv_heap()) -> NewMiniHeap :: kv_heap().
insert(Key, Value, {Index, HeapMap}) ->
    KV = {Key, Value},
    HeapMap1 = HeapMap#{Index => KV},
    HeapMap2 = raise_kv(Index, KV, HeapMap1),
    {Index + 1, HeapMap2}.

raise_kv(Index, {K, _} = KV, HeapMap) ->
    ParentIndex = Index div 2,
    case HeapMap of
        #{ParentIndex := {PKey, _} = PKV} when PKey > K ->
            HeapMap1 = HeapMap#{ParentIndex := KV, Index := PKV},
            raise_kv(ParentIndex, KV, HeapMap1);
        HeapMap ->
            HeapMap
    end.

%%------------------------------------------------------------------------------
%% @doc 取出堆顶的值
%%------------------------------------------------------------------------------
-spec take_value(MiniHeap :: kv_heap()) -> {Value :: term(), NewMiniHeap :: kv_heap()} | empty.
take_value({NextIndex, #{1 := {_, Value}} = HeapMap}) ->
    case NextIndex - 1 of
        1 ->
            {Value, {1, #{}}};
        TailIndex ->
            {TailKV, HeapMap1} = maps:take(TailIndex, HeapMap),
            HeapMap2 = HeapMap1#{1 := TailKV},
            HeapMap3 = decline_kv(1, TailKV, HeapMap2),
            {Value, {NextIndex - 1, HeapMap3}}
    end;
take_value({1, HeapMap}) when map_size(HeapMap) =:= 0 ->
    empty.

decline_kv(Index, KV, HeapMap) ->
    LeftIndex = Index * 2,
    RightIndex = Index * 2 + 1,
    case HeapMap of
        #{LeftIndex := {LK, _} = LeftKV, RightIndex := {RK, _}} when LK < RK ->
            HeapMap1 = HeapMap#{Index := LeftKV, LeftIndex := KV},
            decline_kv(LeftIndex, KV, HeapMap1);
        #{LeftIndex := {LK, _}, RightIndex := {RK, _} = RightKV} when LK > RK ->
            HeapMap1 = HeapMap#{Index := RightKV, RightIndex := KV},
            decline_kv(RightIndex, KV, HeapMap1);
        _ ->
            HeapMap
    end.

%%------------------------------------------------------------------------------
%% @doc 获取堆的大小
%%------------------------------------------------------------------------------
-spec size(MiniHeap :: kv_heap()) -> Size :: non_neg_integer().
size({NextIndex, _HeapMap}) ->
    NextIndex - 1.

%%------------------------------------------------------------------------------
%% @doc 通过列表创建最小堆
%%------------------------------------------------------------------------------
-spec from_list(L :: [{K :: term(), V :: term()}]) -> MiniHeap :: kv_heap().
from_list(L) ->
    from_list_1(L, new()).

from_list_1([{K, V} | T], MiniHeap) ->
    from_list_1(T, insert(K, V, MiniHeap));
from_list_1([], MiniHeap) ->
    MiniHeap.