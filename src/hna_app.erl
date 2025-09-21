%%%-------------------------------------------------------------------
%% @doc hacker_news_aggregator public API
%% @end
%%%-------------------------------------------------------------------

-module(hna_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([{'_', [{"/", hna_ws, []}]}]),
    {ok, _} = cowboy:start_clear(hna_ws,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch}}),
    hna_sup:start_link().


stop(_State) ->
    ok.

%% internal functions
