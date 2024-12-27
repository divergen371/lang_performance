-module(benchmark).

-export([main/1]).

main([Arg]) ->
    U = list_to_integer(atom_to_list(Arg)),
    R = rand:uniform(10000) - 1,
    A = array:new(10000, {default, 0}),

    FinalA = lists:foldl(
        fun(I, Acc) ->
            InnerA = lists:foldl(
                fun(J, InnerAcc) ->
                    array:set(I, array:get(I, InnerAcc) + J rem U, InnerAcc)
                end,
                Acc,
                lists:seq(0, 99999)
            ),
            array:set(I, array:get(I, InnerA) + R, InnerA)
        end,
        A,
        lists:seq(0, 9999)
    ),

    io:format("~p~n", [array:get(R, FinalA)]).
