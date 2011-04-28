%% @doc Recursion help for anonymous functions and alternate control.
%%
%%      When you're stuck at the Erlang console, wishing you could
%%      write a recursive anonymous function to debug some data
%%      structure, or you need a different kind of control than
%%      {@link lists} offers, load `mayo'.
%%
%%      For instance, you may want to sum mailbox messages until some
%%      signal:
%% ```
%% sum_until(Marker) ->
%%    mayo:loop(
%%       fun(Sum) ->
%%          receive
%%             Marker -> {stop, Sum};
%%             Number -> {continue, Number+Sum}
%%          end
%%       end,
%%       0).
%% '''
-module(mayo).

-export([
         while/3,
         do/3,
         loop/2
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Evaluate `Fun(Acc)' until `Pred(Acc)' returns false.  `Init'
%%      is passed as `Acc' for the first round, and the result of
%%      `Fun(Acc)' is passed as `Acc' subsequent rounds (similar to
%%      the accumulator of a fold).  The return value is the result of
%%      the last evaluation of `Fun(Acc)', or `Init' if `Pred(Init)'
%%      returned false.
%%
%%      This function is similar to the psuedo code:
%% ```
%% Acc = Init;
%% while( Pred(Acc) ) {
%%    Acc = Fun(Acc);
%% }
%% return Acc;
%% '''
-spec while(fun((term()) -> boolean()),
            fun((term()) -> term()),
            term())
        -> term().
while(Pred, Fun, Init) ->
    case Pred(Init) of
        true ->
            while(Pred, Fun, Fun(Init));
        false ->
            Init
    end.

%% @doc Evaluate `Fun(Acc)' until `Pred(Acc)' returns false.  Similar
%%      to {@link while/3}, except that `Fun' is evaluated before
%%      `Pred', so the final result will be either the last result of
%%      `Fun(Acc)' or the result of `Fun(Init)' (instead of an
%%      unprocessed `Init').
%%
%% For example, "100!" (factorial) might be computed as:
%% ```
%% element(2,
%%         do(fun({N, T}) -> {N-1, N*T} end,
%%            fun({N, _}) -> N > 1 end,
%%            {100, 1})).
%% '''
-spec do(fun((term()) -> boolean()),
         fun((term()) -> term()),
         term())
        -> term().
do(Fun, Pred, Init) ->
    Next = Fun(Init),
    case Pred(Next) of
        true ->
            do(Fun, Pred, Next);
        false ->
            Next
    end.

%% @doc Evaluate `Fun(Acc)' as long as it returns `{continue,
%%      Acc::term()}', and stop when it returns `{stop, Result::term()}',
%%      returning `Result'.
%%
%%      For example, draining a process's mailbox can be as simple as:
%% ```
%% loop(fun(A) ->
%%         receive X -> {continue, [X|A]}
%%         after 0   -> {stop, lists:reverse(A)} end
%%      end,
%%      [])
%% '''
-spec loop(fun((term()) -> {continue | stop, term()}), term()) -> term().
loop(Fun, Init) ->
    case Fun(Init) of
        {continue, Next} ->
            loop(Fun, Next);
        {stop, Result} ->
            Result
    end.

%%%

-ifdef(TEST).

%% spin a process dict value down to zero
while_test() ->
    Key = '$while_test_val',
    Initial = 10,
    put(Key, Initial),
    while(fun(Expected) ->
                  ?assertEqual(get(Key), Expected),
                  get(Key) > 0
          end,
          fun(Expected) ->
                  put(Key, get(Key)-1),
                  Expected-1
          end,
          Initial),
    ?assertEqual(0, get(Key)).

%% spin a process dict value down to zero
do_test() ->
    Key = '$do_test_val',
    Initial = 10,
    put(Key, Initial),
    do(fun(Expected) ->
               put(Key, get(Key)-1),
               Expected-1
       end,
       fun(Expected) ->
               ?assertEqual(get(Key), Expected),
               get(Key) > 0
       end,
       Initial),
    ?assertEqual(0, get(Key)).

%% check that while never runs its fun if Pred(Init) was false
while_never_test() ->
    Key = '$while_never_test',
    put(Key, pass),
    while(fun(_) -> false end,
          fun(_) -> put(Key, fail) end,
          undefined),
    ?assertEqual(pass, get(Key)).

%% check that do runs its fun even if Pred(Init) was true
do_always_test() ->
    Key = '$do_always_test',
    put(Key, fail),
    do(fun(_) -> put(Key, pass) end,
       fun(_) -> false end,
       undefined),
    ?assertEqual(pass, get(Key)).

%% try that mailbox-draining example
loop_test() ->
    loop_test_drain(), %% clear before start
    Msgs = ['$loop_test_1', '$loop_test_2', '$loop_test_3'],
    [ self() ! M || M <- Msgs ],
    Drain = loop_test_drain(),
    ?assertEqual(Msgs, Drain).

loop_test_drain() ->
    loop(fun(A) ->
                 receive X -> {continue, [X|A]}
                 after 0   -> {stop, lists:reverse(A)} end
         end,
         []).

-endif.
