-module(httpc_proxy).
-author('alking').

-behaviour(gen_server).

%% API
-export([
  start_link/1,
  request/1,
  request/4
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(SERVER, ?MODULE).
-define(PROFILE_ETS, httpc_proxy_profile_ets).
-define(PROFILE_COUNTER_ETS, httpc_proxy_profile_counter_ets).
-define(PROFILE_COUNTER_KEY, 1).

-record(state, {size}).

start_link(Size) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Size], []).

request(URL) ->
  request(get, {URL, []}, [], []).

request(Method, Request, HTTPOptions, Options) when is_atom(Method) ->
  Profile = select_profile(),
  httpc:request(Method, Request, HTTPOptions, Options, Profile).

init([Size]) ->
  ets:new(?PROFILE_ETS, [public, named_table, set, {read_concurrency, true}]),
  ets:new(?PROFILE_COUNTER_ETS, [public, named_table, set, {read_concurrency, true}]),
  ets:insert(?PROFILE_COUNTER_ETS, {?PROFILE_COUNTER_KEY, 1, Size}),
  lists:foreach(fun(X) -> add_profile(X) end, lists:seq(1, Size)),
  {ok, #state{size = Size}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
profile_name(IDX) ->
  erlang:list_to_atom(lists:concat(["profile_", IDX])).

add_profile(IDX) ->
  Name = profile_name(IDX),
  {ok, PID} = httpc_profile_sup:start_child([{profile, Name}]),
  ets:insert(?PROFILE_ETS, {IDX, Name, PID}).

select_profile() ->
  [{_, _, Max}] = ets:lookup(?PROFILE_COUNTER_ETS, ?PROFILE_COUNTER_KEY),
  IDX = ets:update_counter(?PROFILE_COUNTER_ETS, ?PROFILE_COUNTER_KEY, {2, 1, Max, 1}),
  [{_, Name, _}] = ets:lookup(?PROFILE_ETS, IDX),
  Name.
