-module(benchmark).

-export([main/1]).

% 内部合計を計算する関数
calc_inner_sum(U) ->
    % 0からU-1までの余りの合計を計算
    BaseSum = lists:sum(lists:seq(0, U-1)),
    
    % 完全なU個のグループの数と余りを計算
    Quotient = 99999 div U,
    Remainder = 99999 rem U,
    
    % 合計を計算（Erlangのリスト操作を活用）
    Total = BaseSum * Quotient + 
            lists:sum([X rem U || X <- lists:seq(0, Remainder)]),
    
    Total.

main([Arg]) ->
    U = list_to_integer(atom_to_list(Arg)),
    R = rand:uniform(10000) - 1,
    
    % 内部ループの計算を1回だけ行う
    InnerSum = calc_inner_sum(U),
    
    % 配列の初期化を最適化（デフォルト値を使用）
    A = array:new(10000, {default, InnerSum + R}),
    
    io:format("~p~n", [array:get(R, A)]).
