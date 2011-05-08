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
%%
%%      Source code may be viewed at
%%      [http://github.com/beerriot/mixers/blob/master/src/gin.erl]
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
         next/1,
         zip/2,
         zip/1,
         zipwitha/2,
         zipwithl/2
        ]).

-ifdef(TEST).

-ifdef(EQC).
  -include_lib("eqc/include/eqc.hrl").
  -define(EP, eqc).
-else.
-ifdef(PROPER).
   %% this module can't use proper's default imports, because state
   %% proper_statem:zip/2 conflicts with the local zip/2
  -define(PROPER_NO_IMPORTS, true).
  -include_lib("proper/include/proper.hrl").
   %% eqc-compatible functions are useful to have imported, though
  -import(proper, [conjunction/1, equals/2]).
  -import(proper_types, [list/1, int/0, nat/0, real/0]).
  -define(EP, proper).
-endif.
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

%% @equiv zip([Gin1, Gin2])
-spec zip(gin(), gin()) -> gin_t({term(), term()}).
zip(Gin1, Gin2) ->
    zip([Gin1, Gin2]).

%% @equiv zipwithl(fun erlang:list_to_tuple/1, Gins)
-spec zip([gin()]) -> gin_t(tuple()).
zip(Gins) ->
    zipwithl(fun erlang:list_to_tuple/1, Gins).

%% @doc Zip N gins together into a new gin. Evaluating the new gin
%%      will evaluate the `Combine' function on a list containing one
%%      evaluation of each component gin, and produce that result.
%% ```
%% Gin1 = gin:from_list([a,b,c]).
%% Gin2 = gin:from_list([1,2,3]).
%% Gin3 = gin:from_list(["x","y","z"]).
%% GinC = gin:zipwithl(fun erlang:list_to_tuple/1, [Gin1, Gin2, Gin3]).
%% { {a,1,"x"}, GinCC } = GinC().
%% { {b,2,"y"}, GinCCC} = GinCC().
%% { {c,3,"z"}, GinCCCC} = GinCCC().
%% stop = GinCCCC().
%% '''
-spec zipwithl(fun(([term()]) -> term()), [gin()]) -> gin_t(tuple()).
zipwithl(Fun, Gins) ->
    zipwith(Fun, Gins, list).

%% @doc Zip N gins together into a new gin. Evaluating the new gin
%%      will apply the `Combine' function to the first evaluation of
%%      each component gin, and produce that result.
%% ```
%% Gin1 = gin:from_list([a,b,c]).
%% Gin2 = gin:from_list([1,2,3]).
%% Gin3 = gin:from_list(["x","y","z"]).
%% GinC = gin:zipwitha(fun(P,Q,R) -> {P, Q, R} end, [Gin1, Gin2, Gin3]).
%% { {a,1,"x"}, GinCC } = GinC().
%% { {b,2,"y"}, GinCCC} = GinCC().
%% { {c,3,"z"}, GinCCCC} = GinCCC().
%% stop = GinCCCC().
%% '''
-spec zipwitha(fun(), [gin()]) -> gin_t(tuple()).
zipwitha(Fun, Gins) ->
    zipwith(Fun, Gins, apply).

%% @doc Internal implementation of {@link zipwithl/2} and {@link
%%      zipwitha/2}.  `Combine' will be evaluated as
%%      `Combine(GinValues)' if `Eval' is the atom `list', or as
%%      `apply(Combine, GinValues)' if `Eval' is the atom `apply'
%%      (where `GinValues' is a list of the results of the first
%%      evaluation of each gin.
-spec zipwith(fun(), Gins::[gin()], list | apply) -> gin_t(tuple()).
zipwith(Combine, [Head|Tail], Eval) ->
    fun() ->
            Start = case Head() of
                        {Val, Next} -> {[Val], [Next]};
                        stop        -> stop
                    end,
            case lists:foldl(fun zipfold/2, Start, Tail) of
                stop ->
                    stop;
                {RevVals, RevNexts} ->
                    {case Eval of
                         list ->
                             Combine(lists:reverse(RevVals));
                         apply ->
                             apply(Combine, lists:reverse(RevVals))
                     end,
                     zipwith(Combine, lists:reverse(RevNexts), Eval)}
            end
    end.

-spec zipfold(gin(), {[term()], [gin()]}) -> {[term()], [gin()]};
             (gin(), stop) -> stop.
zipfold(Gin, {AccVal, AccNext}) ->
    {Val, Next} = Gin(),
    {[Val|AccVal], [Next|AccNext]};
zipfold(Gin, stop) ->
    stop = Gin().

%%%

-ifdef(TEST).

-ifdef(EP).

eqc_test_() ->
    [{Name, ?_assert(?EP:quickcheck(Prop))}
     || {Name, Prop} <- [{"list roundtrip", eqc_list_prop()},
                         {"gin:seq vs lists:seq", eqc_seq_lists_prop()},
                         {"general seq", eqc_seq_prop()},
                         {"seq sum", eqc_sum_prop()},
                         {"map", eqc_map_prop()},
                         {"fold", eqc_fold_prop()},
                         {"filter", eqc_filter_prop()},
                         {"foreach", eqc_foreach_prop()},
                         {"zip", eqc_zip_prop()},
                         {"zipwitha", eqc_zipwitha_prop()},
                         {"next", eqc_next_prop()}
                        ]].

%% roundtripping from_list/to_list should produce its input
eqc_list_prop() ->
    ?FORALL(List,
            list(real()),
            begin
                equals(List, to_list(from_list(List)))
            end
           ).

%% gin:seq/2,3 should work just like list:seq/2,3 for integers
%% (with the exception that lists:seq/2 won't work if Start
%% is greater than end)
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

%% seq should also work for floating point numbers
eqc_seq_prop() ->
    ?FORALL({Start, End, Step},
            {real(), real(), ?SUCHTHAT(Step, real(), Step =/= 0.0)},
            begin
                %% get Step going in the right direction
                RealStep = if (Start =< End andalso Step > 0);
                              (Start >= End andalso Step < 0) ->
                                   Step;
                              true ->
                                   -Step
                           end,

                %% make the gin
                Gin = seq(Start, End, RealStep),

                %% look through the gin
                {First, Rest} = next(Gin),
                {Final, FailedSteps} =
                    fold(fun(Val, {Prev, Fails}) ->
                                 {Val,
                                  %% this test and seq/3 generate
                                  %% their values in different ways,
                                  %% which can produce floating point
                                  %% values that are not exactly
                                  %% equal, but are logically equal
                                  %% for the purposes of seq
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

%% sum should work for any gin(number()), but computing the correct
%% answer for positive integers is so easy that this test just uses
%% them instead
eqc_sum_prop() ->
    ?FORALL({Start, Extent},
            {nat(), nat()},
            begin
                End = Start+Extent,
                StartToExtent = if Extent == 0 ->
                                        Start;
                                   true ->
                                        %% using '/' instead of div
                                        %% will produce a floating
                                        %% point number instead of an
                                        %% integer, which will pass
                                        %% QuickCheck's equals/2, but
                                        %% not PropEr's
                                       (End*(End+1) div 2)
                                            - ((Start-1)*(Start) div 2)
                                end,
                equals(StartToExtent,
                       sum(seq(Start, End)))
            end).

%% map/2 should be basically equivalent to a list comprehension
%% without any filters (after to_list-ing, of course).
eqc_map_prop() ->
    ?FORALL(List,
            list(real()),
            begin
                Map = fun(X) -> X * 2 end,
                equals([ Map(I) || I <- List ],
                       to_list(map(Map, from_list(List))))
            end).

%% fold/3 should be equivalent to lists:foldl/3
eqc_fold_prop() ->
    ?FORALL(List,
            list(real()),
            begin
                Fold = fun(X, Acc) -> X * Acc end,
                equals(lists:foldl(Fold, 1, List),
                       fold(Fold, 1, from_list(List)))
            end).

%% filter/3 should be basically equivalent to a list comprehension,
%% using the function as a filter (after to_list-ing, of course).
eqc_filter_prop() ->
    ?FORALL(List,
            list(int()),
            begin
                Filter = fun(X) -> (X rem 2) =:= 0 end,
                equals([ I || I <- List, Filter(I) ],
                       to_list(filter(Filter, from_list(List))))
            end).

%% foreach/2 and lists:foreach/2 should be basically equivalent
%%
%% This test attempts to test via the side effect of storing data in
%% the process dictionary.  It uses eqc_foreach_key_popper/1 to
%% construct a gin that reads its values from a key in the process
%% dictionary.
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

%% Create a gin that reads its values from a key in the process
%% dictionary.  The key should hold a list of values.  Each time the
%% gin is evaluated, it returns the head of the list, and stores the
%% tail back to the process dictionary.  When the list is empty, the
%% gin stops.
eqc_foreach_key_popper(FromKey) ->
    fun() ->
            case get(FromKey) of
                [Head|Tail] ->
                    put(FromKey, Tail),
                    {Head, eqc_foreach_key_popper(FromKey)};
                [] -> stop
            end
    end.

%% zip/1 creates gins that produce tuples as large as their input list
%% this test effectively tests zipwithl/2, as zip/1 uses it internally
eqc_zip_prop() ->
    ?FORALL({ExtraLists, ListLength},
            {nat(), nat()},
            begin
                Lists = eqc_zip_make_lists(2+ExtraLists, ListLength),
                ZippedGin = case Lists of
                                [A, B] ->
                                    %% take this opportunity to test
                                    %% the zip/2 convenience function
                                    zip(from_list(A), from_list(B));
                                _ ->
                                    zip([ from_list(L) || L <- Lists ])
                            end,
                %% lists:zip/* can't handle more than 3 lists
                ZippedList = eqc_zip_lists(Lists),
                equals(ZippedList, to_list(ZippedGin))
            end).

%% zipwitha/2 is tested by dynamically creating functions that
%% tuple-ize their arguments, producing output identical to
%% list_to_tuple/1, making it easy to reuse components from
%% zip/1,2's test
eqc_zipwitha_prop() ->
    ?FORALL({ExtraLists, ListLength},
            {?SUCHTHAT(X, nat(), X < 10), %% functions have an argument limit
             nat()},
            begin
                Lists = eqc_zip_make_lists(2+ExtraLists, ListLength),
                Fun = eqc_zip_make_apply_fun(2+ExtraLists),
                ZippedGin = zipwitha(Fun, [ from_list(L) || L <- Lists ]),
                ZippedList = eqc_zip_lists(Lists),
                equals(ZippedList, to_list(ZippedGin))
            end).

%% make Count number of lists, each Length element long
eqc_zip_make_lists(Count, Length) ->
    [ [ N*I || N <- lists:seq(1, Length) ]
      || I <- lists:seq(1, Count) ].

%% construct a function that takes N arguments, and produces a tuple
%% containing them in order; for example:
%%    eqc_zip_make_apply_fun(2).
%% should produce:
%%    fun(_1, _2) -> {_1, _2} end.
eqc_zip_make_apply_fun(N) ->
    Args = string:join(
             [ [$_|integer_to_list(A)] || A <- lists:seq(1, N) ],
             ","),
    FunStr = lists:flatten(["fun(", Args, ") -> {", Args, "} end."]),
    %% stolen from riak_kv_mapred_json
    {ok, Tokens, _} = erl_scan:string(FunStr),
    {ok, [Form]} = erl_parse:parse_exprs(Tokens),
    {value, Fun, _} = erl_eval:expr(Form, erl_eval:new_bindings()),
    Fun.

%% zip N lists together (lists:zip can't do more than 3)
eqc_zip_lists([[]|_]) -> [];
eqc_zip_lists(Lists) ->
    {Heads, Tails} = lists:unzip([ {H, T} || [H|T] <- Lists ]),
    [ list_to_tuple(Heads) | eqc_zip_lists(Tails) ].

%% make sure that next/1 produces 'stop' at the end of a gin,
%% or {term(), fun()} in the middle
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

-endif. % EP

-endif. % TEST
