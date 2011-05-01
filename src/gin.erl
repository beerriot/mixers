%% -------------------------------------------------------------------
%%
%% Copyright (c) 2011 Bryan Fink
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc Function-based generators.
%%
%%      Sometimes building a list simply to have something to iterate
%%      along is annoying, wasteful, or impossible.  For those
%%      situations, consider `gin'.
%%
%%      A gin is a function of zero arity.  When evaluated, it
%%      produces the "next" item, and a new gin (as a 2-tuple `{Item,
%%      Gin}').  When the gin has nothing more to produce, the result
%%      of its evaluation should be the atom `stop'.
%%
%%      The {@link seq/2} function provides a nice example.  Calling
%%      it with the arguments 1 and 3 produces a gin that will
%%      evaluate to 1, 2, and 3, before stopping:
%% ```
%% First       = gin:seq(1, 3).    % create a new gin
%% {1, Second} = gin:next(First).  % get the first value
%% {2, Third}  = gin:next(Second). % get the second value
%% {3, Fourth} = gin:next(Third).  % get the third value
%% stop        = gin:next(Fourth).
%% '''
%%
%%      Remember that gins are functions, so they can do anything:
%%      compute pure values, receive messages, print output, receive
%%      input, etc.  The functions in this module have no side effects
%%      themselves, so it's up to the caller to determine whether they
%%      stay that way.
%%
%%      SICP readers will recognize this construction as "streams"
%%      (a.k.a "delayed lists" or "lazy lists") from section 3.5.
%% [http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-24.html#%_sec_3.5]
-module(gin).

-export([
         from_list/1,
         to_list/1,
         fold/3,
         map/2,
         filter/2,
         foreach/2,
         seq/2,
         seq/3,
         sum/1,
         next/1
        ]).

-ifdef(TEST).
-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-endif.
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(ITER(Stop, Next),
        case Gin() of
            stop           -> Stop;
            {Val, NextGin} -> Next
        end).

-type gin() :: gin_t(term()).
-type gin_t(Type) :: fun(() -> stop | {Type, gin_t(Type)}).

%% @doc Fold over a gin, accumulating a result.  `Fun' is called once
%%      for each item the gin produces, as `Fun(Item, Accumulator)'.
%%      `Init' is passed as the accumulator for the first call.
%%
%%      For example, "sum" might be defined as:
%% ```
%% sum(Gin) ->
%%     fold(fun erlang:'+'/2, 0, Gin).
%% '''
-spec fold(fun((Val::term(), Acc::term()) -> NewAcc::term()), term(), gin())
         -> term().
fold(Fun, Init, Gin) ->
    ?ITER(Init, fold(Fun, Fun(Val, Init), NextGin)).

%% @doc Map over a gin, producing a new gin with each item being the
%%      result of the application `Fun(Item)'.  The input gin is not
%%      evaluated until something evaluates the output gin.
%%
%%      For example, "double" might be implemented as:
%% ```
%% double(Gin) ->
%%    map(fun(I) -> io:format("x"), I*2 end, Gin).
%% '''
%%      But its side-effect behavior would be:
%% ```
%% 1> Single = gin:seq(1,3).
%% #Fun<gin.5.58311043>
%% 2> Double = double(Single).
%% #Fun<gin.0.51198227>
%% 3> gin:to_list(Double).
%% xxx[2,4,6]
%% '''
%%      (Note that the "x" characters printed by the map function did
%%      not appear until the output gin was evaluated by
%%      {@link to_list/1}.)
-spec map(fun((term()) -> term()), gin()) -> gin().
map(Fun, Gin) ->
    fun() -> ?ITER(stop, {Fun(Val), map(Fun, NextGin)}) end.

%% @doc Filter a gin, producing a new gin that only produces those
%%      items for with `Fun(Item)' returns `true'.  The input gin is
%%      not evaluated until something evaluates the output gin.
%%
%%      For example, "evens" might be implemented as:
%% ```
%% evens(Gin) ->
%%     filter(fun(X) -> (X rem 2) =:= 0 end, Gin).
%% '''
%%      See {@link map/2} for an example of the delayed-evaluation
%%      behavior.
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

%% @doc Evaluate `Fun' for each item produced by `Gin'.
%%
%%      For example, sending each item in a gin as a message to
%%      another process might be implemented as:
%% ```
%% carrier(Pid, Gin) ->
%%    foreach(fun(Item) -> Pid ! Item end, Gin).
%% '''
-spec foreach(fun((term()) -> term()), gin()) -> ok.
foreach(Fun, Gin) ->
    ?ITER(ok, begin Fun(Val), foreach(Fun, NextGin) end).

%% @doc Sum the items produced by a gin. This function assumes that
%%      the gin produces only numbers.
%% ```
%% 500000500000 = gin:sum(gin:seq(1, 1000000)).
%% '''
-spec sum(gin_t(number())) -> number().
sum(Gin) ->
    fold(fun erlang:'+'/2, 0, Gin).

%% @doc Create a gin that produces the items from a list, in order.
%%
%%      The similarities can be seen in this snippet:
%% ```
%% [Head|RestList] = List.
%% {Head,RestGin}  = next(from_list(List)).
%% stop            = next(from_list([])).
%% '''
-spec from_list(list()) -> gin().
from_list([Head|Rest]) ->
    fun() -> {Head, from_list(Rest)} end;
from_list([]) ->
    fun() -> stop end.

%% @doc Create a list containing the items produced by a gin, in order.
%%%     ```List = to_list(from_list(List))'''
-spec to_list(gin()) -> list().
to_list(Gin) ->
    Rev = fold(fun(Val, Acc) -> [Val|Acc] end, [], Gin),
    lists:reverse(Rev).

%% @doc Create a gin that produces the numbers from `Start' to
%%      `Finish', incrementing by 1 each time if `Start' is less than
%%      or equal to `Finish', or decrementing by 1 each time if
%%      `Start' is less that `Finish'.
%% ```
%% [1,2,3,4,5] = gin:to_list(gin:seq(1, 5)).
%% [5,4,3,2,1] = gin:to_list(gin:seq(5, 1)).
%% '''
-spec seq(number(), number()) -> gin_t(number()).
seq(Start, Finish) when Start =< Finish ->
    seq(Start, Finish, 1);
seq(Start, Finish) when Start > Finish ->
    seq(Start, Finish, -1).

%% @doc Create a gin that produces a sequence of numbers, starting
%%      with `Start', and incrementing by `Incr', and finishing before
%%      producing any number past `Finish' (where "past" means "less
%%      than" if `Incr' is negative, or "greater than" if `Incr' is
%%      positive).
%% ```
%% [1,11,21,31,41] = gin:to_list(gin:seq(1, 42, 10)).   
%% [1,-9,-19,-29,-39] = gin:to_list(gin:seq(1, -42, -10)).
%% '''
-spec seq(number(), number(), number()) -> gin_t(number()).
seq(Start, Finish, Incr) ->
    %% Start must always be returned
    fun() -> {Start, seq2(Start+Incr, Finish, Incr)} end.
seq2(Start, Finish, Incr) when (Incr > 0 andalso Start =< Finish);
                              (Incr < 0 andalso Start >= Finish) ->
    fun() -> {Start, seq2(Start+Incr, Finish, Incr)} end;
seq2(_Start, _Finish, _Incr) ->
    fun() -> stop end.

%% @doc Get the next value to be produced by a gin, and a handle to
%%      the next invocation of that gin.  The atom `stop' is returned
%%      instead, if the gin had nothing more to produce.  See the example
%%      use with {@link from_list/1}.
-spec next(gin()) -> stop | {term(), gin()}.
next(Gin) ->
    Gin().

%%%

-ifdef(TEST).

%% simple roundtrip through from_list and to_list
list_test() ->
    List = [a,b,c,d,e,f],
    ?assertEqual(List, to_list(from_list(List))).

%% make sure that gin:seq operates like lists:seq
seq_test() ->
    ?assertEqual(lists:seq(1, 10), to_list(seq(1, 10))).

%% make sure that gin:sum operates like lists:sum
sum_test() ->
    ?assertEqual(lists:sum(lists:seq(1, 10)), sum(seq(1, 10))).

%% map should operate similar to a basic list-comp
%% (after gin->list conversion)
map_test() ->
    Fun = fun(X) -> X * 2 end,
    ?assertEqual([ Fun(N) || N <- lists:seq(1, 10) ],
                 to_list(map(Fun, seq(1, 10)))).

%% fold should operate like lists:fold
fold_test() ->
    Fun = fun(X, Acc) -> X * Acc end,
    ?assertEqual(lists:foldl(Fun, 1, lists:seq(1, 10)),
                 fold(Fun, 1, seq(1, 10))).

%% filter should operate like lists:filter
%% (after gin->list conversion)
filter_test() ->
    Fun = fun(X) -> (X rem 2) =:= 0 end,
    ?assertEqual(lists:filter(Fun, lists:seq(1, 10)),
                 to_list(filter(Fun, seq(1, 10)))).

%% make sure next gives the next item and handle, and then stops
next_test() ->
    Gen = fun() -> {hi, fun() -> stop end} end,
    {First, Next} = next(Gen),
    ?assertEqual(hi, First),
    ?assertEqual(stop, next(Next)).

%% make sure foreach hits each item in order by using the side
%% effect of updating a value in the process dict
foreach_test() ->
    Key = '$foreach_test_val',
    Fun = fun(Val) ->
                  put(Key, [Val|get(Key)])
          end,

    put(Key, []),
    ok = lists:foreach(Fun, lists:seq(1, 10)),
    Lists = get(Key),
    
    put(Key, []),
    ok = foreach(Fun, seq(1, 10)),
    Gin = get(Key),
    
    ?assertEqual(Lists, Gin).

-ifdef(EQC).

eqc_test_() ->
    [{Name, ?_assert(eqc:quickcheck(Prop))}
     || {Name, Prop} <- [{"list roundtrip", eqc_list_prop()},
                         {"gin:seq vs lists:seq", eqc_seq_lists_prop()},
                         {"general seq", eqc_seq_prop()},
                         {"seq sum", eqc_sum_prop()},
                         {"map", eqc_map_prop()},
                         {"fold", eqc_fold_prop()},
                         {"filter", eqc_filter_prop()},
                         {"foreach", eqc_foreach_prop()},
                         {"next", eqc_next_prop()}
                        ]].

eqc_list_prop() ->
    ?FORALL(List,
            list(real()),
            begin
                equals(List, to_list(from_list(List)))
            end
           ).

eqc_seq_lists_prop() ->
    ?FORALL({Start, Extent, Step},
            {int(), nat(), ?SUCHTHAT(Step, nat(), Step =/= 0)},
            begin
                conjunction(
                  [{"forward", equals(lists:seq(Start, Start+Extent),
                                      to_list(seq(Start, Start+Extent)))},
                   {"backward", equals(lists:seq(Start+Extent, Start, -1),
                                       to_list(seq(Start+Extent, Start)))},
                   {"forward with step",
                    equals(lists:seq(Start, Start+Extent, Step),
                           to_list(seq(Start, Start+Extent, Step)))},
                   {"backward with step",
                    equals(lists:seq(Start+Extent, Start, -Step),
                           to_list(seq(Start+Extent, Start, -Step)))}])
            end).

eqc_seq_prop() ->
    ?FORALL({Start, End, Step},
            {real(), real(), ?SUCHTHAT(Step, real(), Step =/= 0.0)},
            begin
                RealStep = if (Start =< End andalso Step > 0);
                              (Start >= End andalso Step < 0) ->
                                   Step;
                              true ->
                                   -Step
                           end,
                Gin = seq(Start, End, RealStep),
                {First, Rest} = next(Gin),
                {Final, FailedSteps} =
                    fold(fun(Val, {Prev, Fails}) ->
                                 {Val,
                                  case RealStep-(Val-Prev) of
                                      S when S < 1.0e-9, S > -1.0e-9 ->
                                          Fails;
                                      _LargeDiff ->
                                          [{Prev, Val}|Fails]
                                  end}
                         end,
                         {First, []},
                         Rest),
                conjunction(
                  [{"head", equals(Start, First)},
                   {"lasttail", (RealStep > 0 andalso Final =< End)
                    orelse (RealStep < 0 andalso Final >= End)},
                   {"intervals", equals([], FailedSteps)}])
            end).

eqc_sum_prop() ->
    ?FORALL({Start, Extent},
            {nat(), nat()},
            begin
                End = Start+Extent,
                StartToExtent = if Extent == 0 ->
                                        Start;
                                   true ->
                                       (End*(End+1)/2)
                                            - ((Start-1)*(Start)/2)
                                end,
                equals(StartToExtent,
                       sum(seq(Start, End)))
            end).

eqc_map_prop() ->
    ?FORALL(List,
            list(real()),
            begin
                Map = fun(X) -> X * 2 end,
                equals([ Map(I) || I <- List ],
                       to_list(map(Map, from_list(List))))
            end).

eqc_fold_prop() ->
    ?FORALL(List,
            list(real()),
            begin
                Fold = fun(X, Acc) -> X * Acc end,
                equals(lists:foldl(Fold, 1, List),
                       fold(Fold, 1, from_list(List)))
            end).

eqc_filter_prop() ->
    ?FORALL(List,
            list(int()),
            begin
                Filter = fun(X) -> (X rem 2) =:= 0 end,
                equals([ I || I <- List, Filter(I) ],
                       to_list(filter(Filter, from_list(List))))
            end).

eqc_foreach_prop() ->
    ?FORALL(List,
            list(real()),
            begin
                ListsKey = '$foreach_test_eqc_lists_val',
                GinKey = '$foreach_test_eqc_gin_val',
                Fun = fun(Key) ->
                              fun(Val) ->
                                      put(Key, [Val|get(Key)])
                              end
                      end,

                put(ListsKey, []),
                ok = lists:foreach(Fun(ListsKey), List),
                %% ListsKey holds reverse of List
    
                put(GinKey, []),
                ok = foreach(Fun(GinKey),
                             eqc_foreach_key_popper(ListsKey)),
                %% GinKey now holds List
    
                equals(List, get(GinKey))
            end).

eqc_foreach_key_popper(FromKey) ->
    fun() ->
            case get(FromKey) of
                [Head|Tail] ->
                    put(FromKey, Tail),
                    {Head, eqc_foreach_key_popper(FromKey)};
                [] -> stop
            end
    end.

eqc_next_prop() ->
    ?FORALL(List,
            list(int()),
            begin
                Gin = from_list(List),
                case List of
                    [] ->
                        equals(stop, next(Gin));
                    _ ->
                        {Head, Rest} = next(Gin),
                        conjunction(
                          [{"head", equals(hd(List), Head)},
                           {"fun", is_function(Rest)},
                           {"arity", 0 == proplists:get_value(
                                            arity, erlang:fun_info(Rest))}])
                end
            end).

-endif. % EQC

-endif. % TEST
