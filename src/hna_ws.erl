-module(hna_ws).

-behaviour(cowboy_websocket).

%% Callbacks for `cowboy_websocket`
-export([init/2, websocket_handle/2, websocket_info/2]).

%% Optional callbacks for `cowboy_websocket`
-export([websocket_init/1]).

init(Req, State) ->
    {ok, IdleTimeout} = application:get_env(hacker_news_aggregator, idle_timeout),
    Opts = #{
        idle_timeout => IdleTimeout
    },
    {cowboy_websocket, Req, State, Opts}.

websocket_init(State) ->
    pg:join(ws_connections, self()),
    Stories = hna_storage:get_all_stories(),
    {[{text, json:encode(Stories)}], State}.

websocket_handle(Msg, State) ->
    logger:warning("Websocket message received: ~p~n", [Msg]),
    {[], State}.

websocket_info({new_stories, Stories}, State) ->
    {[{text, json:encode(Stories)}], State};
websocket_info(_, State) ->
    {[], State}.
