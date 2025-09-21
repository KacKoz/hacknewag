-module(hna_ws).

-behaviour(cowboy_websocket).

%% Callbacks for `cowboy_websocket`
-export([init/2, websocket_handle/2, websocket_info/2]).

%% Optional callbacks for `cowboy_websocket`
-export([websocket_init/1]).

init(Req, State) ->
    {ok, IdleTimeout} = application:get_env(hacker_news_aggregator, idle_timeout),
    Opts = #{
        % We need to increase idle timeout because the default one is less than 5 minutes
        % and the connection would be lost. We could alternatively ping the client periodically
        % but if we had larger numbers of clients it could become a preformence concern.
        idle_timeout => IdleTimeout
    },
    {cowboy_websocket, Req, State, Opts}.

websocket_init(State) ->
    % I decided to use process group to keep track of websocket connections.
    % Each ws connection is a separate process, so we keep their pids in the group
    % and we are able to send them the new data after update without having to implement
    % any custom subscription mechanisms.
    pg:join(ws_connections, self()),
    Stories = hna_storage:get_all_stories(),
    {[{text, json:encode(Stories)}], State}.

websocket_handle(Msg, State) ->
    % I decided to log it as warning since we are not expecting any messages.
    logger:warning("Websocket message received: ~p~n", [Msg]),
    {[], State}.

websocket_info({new_stories, Stories}, State) ->
    % New stories have been received. Send them to the client.
    {[{text, json:encode(Stories)}], State};
websocket_info(_, State) ->
    {[], State}.
