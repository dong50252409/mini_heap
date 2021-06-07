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
-spec insert(Key :: term(), Value :: term(), MiniHeap :: kv_heap()) -> NewMiniHeap :: kv_heap().
insert(Key, Value, MiniHeap) ->
    KV = {Key, Value},
    Index = maps:size(MiniHeap) + 1,
    HeapMap1 = MiniHeap#{Index => KV},
    raise_kv(Index, KV, HeapMap1).

raise_kv(Index, {K, _} = KV, MiniHeap) ->
    ParentIndex = Index bsr 1,
    case MiniHeap of
        #{ParentIndex := {PKey, _} = PKV} when PKey > K ->
            HeapMap1 = MiniHeap#{ParentIndex := KV, Index := PKV},
            raise_kv(ParentIndex, KV, HeapMap1);
        MiniHeap ->
            MiniHeap
    end.

%%------------------------------------------------------------------------------
%% @doc 取出堆顶的值
%%------------------------------------------------------------------------------
-spec take_value(MiniHeap :: kv_heap()) -> {Value :: term(), NewMiniHeap :: kv_heap()} | empty.
take_value(#{1 := {_, Value}} = MiniHeap) ->
    case maps:size(MiniHeap) of
        1 ->
            {Value, #{}};
        TailIndex ->
            {TailKV, HeapMap1} = maps:take(TailIndex, MiniHeap),
            HeapMap2 = HeapMap1#{1 := TailKV},
            HeapMap3 = decline_kv(1, TailKV, HeapMap2),
            {Value, HeapMap3}
    end;
take_value(HeapMap) when map_size(HeapMap) =:= 0 ->
    empty.

decline_kv(Index, KV, MiniHeap) ->
    LeftIndex = Index bsl 1,
    RightIndex = (Index bsl 1) + 1,
    case MiniHeap of
        #{LeftIndex := {LK, _} = LeftKV, RightIndex := {RK, _}} when LK < RK ->
            HeapMap1 = MiniHeap#{Index := LeftKV, LeftIndex := KV},
            decline_kv(LeftIndex, KV, HeapMap1);
        #{LeftIndex := {LK, _}, RightIndex := {RK, _} = RightKV} when LK > RK ->
            HeapMap1 = MiniHeap#{Index := RightKV, RightIndex := KV},
            decline_kv(RightIndex, KV, HeapMap1);
        _ ->
            MiniHeap
    end.

%%------------------------------------------------------------------------------
%% @doc 获取堆的大小
%%------------------------------------------------------------------------------
-spec size(MiniHeap :: kv_heap()) -> Size :: non_neg_integer().
size(MiniHeap) ->
    maps:size(MiniHeap).

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
