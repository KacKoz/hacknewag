-module(hna_storage).

-behaviour(gen_server).

%% Callbacks for `gen_server`
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, start_link/0]).

-export([get_page/1, get_story/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    erlang:send_after(refresh_interval(), self(), refresh_stories),
    self() ! refresh_stories,
    {ok, []}.

get_page(N) ->
    gen_server:call(?MODULE, {get_page, N}).

get_story(Id) ->
    gen_server:call(?MODULE, {get_story, Id}).

handle_call({get_page, N}, _From, Stories) ->
    PageSize = application:get_env(hacker_news_aggregator, page_size, 10),
    {reply, lists:sublist(Stories, ((N-1)*PageSize)+1, PageSize), Stories};
handle_call({get_story, Id}, _From, Stories) ->
    {reply, lists:keyfind(Id, 1, Stories), Stories}.

handle_cast(_Request, _State) ->
    erlang:error(not_implemented).

handle_info(refresh_stories, _State) ->
    {ok, Stories} = hna_fetcher:get_stories(),
    erlang:send_after(refresh_interval(), self(), refresh_stories),
    {noreply, Stories}.

refresh_interval() ->
    {ok, Interval} = application:get_env(hacker_news_aggregator, refresh_interval),
    Interval.


