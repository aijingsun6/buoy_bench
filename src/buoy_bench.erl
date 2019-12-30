-module(buoy_bench).
-author('alking').
-define(DEFAULT_TIMEOUT, 5000).
%% API
-export([
  start/0,
  stop/0
]).

-export([bench/5]).

start() ->
  application:start(inets).

stop() ->
  application:stop(inets).


bench(Name, Fun, Threads, Trials, Args) ->
  bench(Name, Fun, Threads, Trials, Args, ?DEFAULT_TIMEOUT).

bench(Name, Fun, Threads, Trials, Args, Timeout) ->
  print_result(Name, repeat_tc(Fun, Threads, Trials, Args, Timeout)).

repeat_tc(Fun, Threads, Trials, Args, _Timeout) when Threads < 2 ->
  % only one thread
  {Time, _} = timer:tc(fun() -> repeat(1, Trials, Fun, Args) end),
  {Time, Trials};
repeat_tc(Fun, Threads, Trials, Args, Timeout) ->
  PID = self(),
  {Time, _} = timer:tc(
    fun() ->
      do_map(Threads, PID, Fun, Trials, Args),
      do_reduce(Threads, Timeout * Trials)
    end),
  {Time, Trials * Threads}.

repeat(_ThreadN, TrialsN, _Fun, _Args) when TrialsN < 1 -> ok;
repeat(ThreadNUM, TrialsNUM, Fun, Args) ->
  case erlang:fun_info(Fun, arity) of
    {arity, 0} -> Fun();
    {arity, 1} -> Fun(Args);
    {arity, 2} -> Fun(Args, ThreadNUM);
    {arity, 3} -> Fun(Args, ThreadNUM, TrialsNUM)
  end,
  repeat(ThreadNUM, TrialsNUM - 1, Fun, Args).

print_result(Name, {Time, Trials}) when Time =/= 0 ->
  io:format("~s: ~.3f us (~.2f per second)~n", [Name, Time / Trials, Trials / (Time / 1000000)]);
print_result(_, _) ->
  ok.

do_map(0, _PID, _F, _Trials, _Args) -> ok;
do_map(N, PID, F, Trials, Args) ->
  erlang:spawn(
    fun() ->
      R = repeat(N, Trials, F, Args),
      PID ! R
    end),
  do_map(N - 1, PID, F, Trials, Args).

do_reduce(0, _Timeout) -> ok;
do_reduce(N, Timeout) ->
  receive
    _ ->
      do_reduce(N - 1, Timeout)
  after Timeout ->
    do_reduce(N - 1, Timeout)
  end.
