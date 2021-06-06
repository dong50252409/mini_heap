mini_heap
=====

基于maps结构实现的最小堆，提供elem_heap和kv_heap两种实现

    elem_heap.erl
    new/0               % O(1)
    add_element/2       % O(log n)
    take_element/1      % O(log n)
    size/1              % O(1)
    from_list/1         % O(1)
    
    vk_heap.erl
    new/0               % O(1)
    insert/3            % O(log n)
    take_value/1        % O(log n)
    size/1              % O(1)
    from_list/1         % O(1)

如何使用
-----
`elem_heap.erl`使用例子

    $ rebar3 shell

    Eshell V11.0  (abort with ^G)
    1> L = [79, 62, 73, 90, 66, 32, 58, 39, 26, 19].
    [79, 62, 73, 90, 66, 32, 58, 39, 26, 19]
    2> MiniHeap = elem_heap:from_list(L).
    {11,#{1 => 19,2 => 26,3 => 58,4 => 39,5 => 32,...}}
    7> {Elem, MiniHeap1} = elem_heap:take_element(MiniHeap).
    {19,{10,#{1 => 26,2 => 32,3 => 58,4 => 39,5 => 79,...}}}
    4> Elem.
    19
    5> MiniHeap2 = elem_heap:add_element(50, MiniHeap1).
    {11,#{1 => 26,2 => 32,3 => 58,4 => 39,5 => 50,...}}
    6> {Elem2, MiniHeap3} = elem_heap:take_element(MiniHeap2).
    {26,{10,#{1 => 32,2 => 39,3 => 58,4 => 66,5 => 50,...}}}
    7> Elem2.
    26
    8> elem_heap:size(MiniHeap3).
    9

`kv_heap.erl`使用例子

    $ rebar3 shell

    Eshell V11.0  (abort with ^G)
    1> L = [{79, 1}, {62, 2}, {73, 3}, {90, 4}, {66, 5}, {32, 6}, {58, 7}, {39, 8}, {26, 9}, {19, 10}].
    [{79,1}, {62,2}, {73,3}, {90,4}, {66,5}, {32,6}, {58,7}, {39,8}, {26,9}, {19,10}]
    2> MiniHeap = kv_heap:from_list(L).
    {11,#{1 => {19,10},2 => {26,9},3 => {58,7},4 => {39,8},5 => {32,6},...}}
    3> {Value, MiniHeap1} = kv_heap:take_value(MiniHeap).
    {10,{10,#{1 => {26,9},2 => {32,6},3 => {58,7},4 => {39,8},5 => {79,1},...}}}
    4> Value.
    10
    5> MiniHeap2 = kv_heap:insert(50, 50, MiniHeap1).      
    {11,#{1 => {26,9},2 => {32,6},3 => {58,7},4 => {39,8},5 => {50,50},...}}
    6> {Value2, MiniHeap3} = kv_heap:take_value(MiniHeap2).
    {9,{10,#{1 => {32,6},2 => {39,8},3 => {58,7},4 => {66,5},5 => {50,50},...}}}
    7> Value2.
    9
    8> kv_heap:size(MiniHeap3).
    9




