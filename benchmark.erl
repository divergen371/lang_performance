-module(benchmark).
-export([main/1]).

% 内部ループの計算を行う関数
calc_inner_sum(U) ->
    % 0からU-1までの余りの合計を計算
    BaseSum = lists:sum(lists:seq(0, U-1)),
    
    % 完全なU個のグループの数と余りを計算
    Quotient = 99999 div U,
    Remainder = 99999 rem U,
    
    % 合計を計算
    RemainderSum = lists:sum([X rem U || X <- lists:seq(0, Remainder)]),
    
    BaseSum * Quotient + RemainderSum.

% 配列の一部を計算するワーカープロセス
worker(Parent, InnerSum, R, Start, End) ->
    Result = array:from_list([InnerSum + R || _ <- lists:seq(Start, End-1)]),
    Parent ! {self(), Result}.

main([Arg]) ->
    try
        U = list_to_integer(Arg),
        
        % 乱数生成
        <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
        rand:seed(exs1024s, {A,B,C}),
        R = rand:uniform(10000) - 1,
        
        % 内部ループの計算を1回だけ行う
        InnerSum = calc_inner_sum(U),
        
        % プロセッサ数に基づいてワーカー数を決定
        WorkerCount = erlang:system_info(schedulers_online),
        ChunkSize = (10000 + WorkerCount - 1) div WorkerCount,
        
        % ワーカープロセスを起動
        Workers = lists:map(
            fun(N) ->
                Start = N * ChunkSize,
                End = min(Start + ChunkSize, 10000),
                Pid = spawn_link(fun() -> worker(self(), InnerSum, R, Start, End) end),
                {Pid, Start, End}
            end,
            lists:seq(0, WorkerCount-1)
        ),
        
        % 結果を収集
        Array = lists:foldl(
            fun({Pid, Start, End}, Acc) ->
                receive
                    {Pid, Result} ->
                        array:set_range(Start, array:to_list(Result), Acc)
                end
            end,
            array:new(10000),
            Workers
        ),
        
        % 結果を出力
        io:format("~p~n", [array:get(R, Array)]),
        erlang:halt(0)
    catch
        _:_ ->
            io:format(standard_error, "Usage: escript benchmark.erl <number>~n", []),
            erlang:halt(1)
    end;
main(_) ->
    io:format(standard_error, "Usage: escript benchmark.erl <number>~n", []),
    erlang:halt(1).
