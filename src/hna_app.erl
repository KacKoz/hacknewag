%%%-------------------------------------------------------------------
%% @doc hacker_news_aggregator public API
%% @end
%%%-------------------------------------------------------------------

-module(hna_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    hna_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
