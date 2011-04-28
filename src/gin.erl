-module(gin).

-export([
         from_list/1,
         to_list/1,
         fold/3,
         map/2,
         filter/2,
         seq/2,
         seq/3,
         sum/1,
         next/1
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(ITER(Stop, Next),
        case Gin() of
            stop           -> Stop;
            {Val, NextGin} -> Next
        end).

-type gin() :: gin_t(term()).
-type gin_t(Type) :: fun(() -> stop | {Type, gin_t(Type)}).

-spec fold(fun((term(), term()) -> term()), term(), gin()) -> term().
fold(Fun, Init, Gin) ->
    ?ITER(Init, fold(Fun, Fun(Val, Init), NextGin)).

-spec map(fun((term()) -> term()), gin()) -> gin().
map(Fun, Gin) ->
    fun() -> ?ITER(stop, {Fun(Val), map(Fun, NextGin)}) end.

-spec filter(fun((term()) -> boolean()), gin()) -> gin().
filter(Fun, Gin) ->
    fun() ->
            ?ITER(stop,
                  begin
                      NextFilterGin = filter(Fun, NextGin),
                      case Fun(Val) of
                          true -> {Val, NextFilterGin};
                          false -> (NextFilterGin)()
                      end
                  end)
    end.

-spec sum(gin_t(number())) -> number().
sum(Gin) ->
    fold(fun erlang:'+'/2, 0, Gin).

-spec from_list(list()) -> gin().
from_list([Head|Rest]) ->
    fun() -> {Head, from_list(Rest)} end;
from_list([]) ->
    fun() -> stop end.

-spec to_list(gin()) -> list().
to_list(Gin) ->
    Rev = fold(fun(Val, Acc) -> [Val|Acc] end, [], Gin),
    lists:reverse(Rev).

-spec seq(number(), number()) -> gin_t(number()).
seq(Start, Finish) when Start =< Finish ->
    seq(Start, Finish, 1);
seq(Start, Finish) when Start > Finish ->
    seq(Start, Finish, -1).

-spec seq(number(), number(), number()) -> gin_t(number()).
seq(Start, Finish, Incr) when (Incr > 0 andalso Start =< Finish);
                              (Incr < 0 andalso Start >= Finish) ->
    fun() -> {Start, seq(Start+Incr, Finish, Incr)} end;
seq(_Start, _Finish, _Incr) ->
    fun() -> stop end.

-spec next(gin()) -> stop | {term(), gin()}.
next(Gin) ->
    Gin().

%%%

-ifdef(TEST).

list_test() ->
    List = [a,b,c,d,e,f],
    ?assertEqual(List, to_list(from_list(List))).

seq_test() ->
    ?assertEqual(lists:seq(1, 10), to_list(seq(1, 10))).

sum_test() ->
    ?assertEqual(lists:sum(lists:seq(1, 10)), sum(seq(1, 10))).

map_test() ->
    Fun = fun(X) -> X * 2 end,
    ?assertEqual([ Fun(N) || N <- lists:seq(1, 10) ],
                 to_list(map(Fun, seq(1, 10)))).

fold_test() ->
    Fun = fun(X, Acc) -> X * Acc end,
    ?assertEqual(lists:foldl(Fun, 1, lists:seq(1, 10)),
                 fold(Fun, 1, seq(1, 10))).

filter_test() ->
    Fun = fun(X) -> (X rem 2) =:= 0 end,
    ?assertEqual(lists:filter(Fun, lists:seq(1, 10)),
                 to_list(filter(Fun, seq(1, 10)))).

next_test() ->
    Gen = fun() -> {hi, fun() -> stop end} end,
    {First, Next} = next(Gen),
    ?assertEqual(hi, First),
    ?assertEqual(stop, next(Next)).

-endif.
