-module(buoy_proxy).

-behaviour(gen_server).

%% API
-export([
  start_link/0,
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
-define(DEF_TIMEOUT, 5000).
-define(DEF_POOL_SIZE, 16).
-define(HTTP_VERSION, "HTTP/1.1").
-record(state, {
  pool_size = ?DEF_POOL_SIZE
}).


-record(buoy_resp, {
  state :: body | done,
  body :: undefined | binary(),
  content_length :: undefined | non_neg_integer() | chunked,
  headers :: undefined | [binary()],
  reason :: undefined | binary(),
  status_code :: undefined | 100..505
}).

-record(buoy_url, {
  host,
  hostname,
  path,
  port,
  protocol
}).

start_link() ->
  start_link(?DEF_POOL_SIZE).

start_link(PoolSize) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [PoolSize], []).

request(URL) ->
  request(get, {URL, []}, [], []).

request(Method, Request, HTTPOptions, _Options) when is_atom(Method) ->
  {URL, Headers, Body} =
    case Request of
      {URL0, Headers0} when is_binary(URL0) -> {URL0, Headers0, undefined};
      {URL0, Headers0} when is_list(URL0) -> {erlang:list_to_binary(URL0), Headers0, undefined};
      {URL0, Headers0, ContentType, Body0} when is_binary(URL0) ->
        {URL0, map_header(merge_header(ContentType, Headers0), []), Body0};
      {URL0, Headers0, ContentType, Body0} when is_list(URL0) ->
        {erlang:list_to_binary(URL0), map_header(merge_header(ContentType, Headers0), []), Body0};
      _ -> {error, req_error}
    end,
  Map = maps:new(),
  Map2 = case Body of
           undefined -> Map;
           _ -> maps:put(body, Body, Map)
         end,
  Map3 = case lists:keyfind(timeout, 1, HTTPOptions) of
           {_, Timeout} when is_integer(Timeout) -> maps:put(timeout, Timeout, Map2);
           false -> maps:put(timeout, ?DEF_TIMEOUT, Map2);
           _ -> Map2
         end,
  Map4 = maps:put(headers, Headers, Map3),
  URL2 = buoy_utils:parse_url(URL),
  ensure_pool(URL2),
  request_i(Method, URL2, Map4).

request_i(Method, URL, Map) ->
  case buoy:request(Method, URL, Map) of
    {ok, Resp} ->
      {ok, parse_resp(Resp)};
    Err ->
      Err
  end.

merge_header(ContentType, L) when is_binary(ContentType), ContentType =/= <<>> ->
  case lists:keyfind(<<"Content-Type">>, 1, L) of
    false -> [{<<"Content-Type">>, ContentType} | L];
    {_, _} -> L
  end;
merge_header(ContentType, L) when is_list(ContentType), ContentType =/= [] ->
  case lists:keyfind(<<"Content-Type">>, 1, L) of
    false -> [{<<"Content-Type">>, ContentType} | L];
    {_, _} -> L
  end;
merge_header(_, L) ->
  L.

map_header([], Acc) -> lists:reverse(Acc);
map_header([{A, B} | L], Acc) ->
  A1 = if
         is_binary(A) -> A;
         is_list(A) -> erlang:list_to_binary(A);
         true -> undefined
       end,
  B1 = if
         is_binary(B) -> B;
         is_list(B) -> erlang:list_to_binary(B);
         true -> undefined
       end,
  case is_binary(A1) andalso is_binary(B1) of
    true -> map_header(L, [{A1, B1} | Acc]);
    false -> map_header(L, Acc)
  end;
map_header([_ | L], Acc) ->
  map_header(L, Acc).

parse_resp(#buoy_resp{status_code = StateCode, headers = Headers, body = Body, reason = Reason}) ->
  StateLine = {?HTTP_VERSION, StateCode, Reason},
  Headers2 = parse_headers(Headers, []),
  {StateLine, Headers2, Body}.

parse_headers([], Acc) -> Acc;
parse_headers([E | L], Acc) when is_binary(E) ->
  case binary:split(E, <<": ">>) of
    [A, B] -> parse_headers(L, [{A, B} | Acc]);
    _ ->
      parse_headers(L, Acc)
  end;
parse_headers([_ | L], Acc) ->
  parse_headers(L, Acc).

ensure_pool(#buoy_url{protocol = Protocol, hostname = HostName, port = Port} = URL) ->
  case buoy_pool:lookup(Protocol, HostName, Port) of
    {ok, _} ->
      pass;
    {error, _} ->
      gen_server:call(?SERVER, {init_pool, URL})
  end.

init([PoolSize]) ->
  {ok, _} = buoy_app:start(),
  {ok, #state{pool_size = PoolSize}}.

handle_call({init_pool, URL}, _From, #state{pool_size = PoolSize} = State) ->
  buoy_pool:start(URL, [{pool_size, PoolSize}]),
  {reply, ok, State};
handle_call({set_pool_size, PoolSize}, _From, State) ->
  {reply, ok, State#state{pool_size = PoolSize}};
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