-module(hna_storage).

-behaviour(gen_server).

%% Callbacks for `gen_server`
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, start_link/0]).

-export([get_page/1, get_story/1, get_all_stories/0]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_Args) ->
    erlang:send_after(refresh_interval(), self(), refresh_stories),
    self() ! refresh_stories,
    {ok, []}.


get_page(N) ->
    gen_server:call(?MODULE, {get_page, N}).


get_all_stories() ->
    gen_server:call(?MODULE, get_all_stories).


get_story(Id) ->
    gen_server:call(?MODULE, {get_story, Id}).


handle_call({get_page, N}, _From, Stories) ->
    PageSize = application:get_env(hacker_news_aggregator, page_size, 10),
    Page = lists:sublist(Stories, ((N - 1) * PageSize) + 1, PageSize),
    {reply, Page, Stories};
handle_call({get_story, Id}, _From, Stories) ->
    Story = find_story(Id, Stories),
    {reply, Story, Stories};
handle_call(get_all_stories, _From, Stories) ->
    {reply, Stories, Stories}.


handle_cast(_Request, _State) ->
    erlang:error(not_implemented).


handle_info(refresh_stories, _State) ->
    {ok, Stories} = hna_fetcher:get_stories(),
    % Send messages to connected ws clients
    lists:foreach(
        fun(Pid) ->
                Pid ! {new_stories, Stories}
        end,
        pg:get_members(ws_connections)
     ),
    erlang:send_after(refresh_interval(), self(), refresh_stories),
    {noreply, Stories}.


refresh_interval() ->
    {ok, Interval} = application:get_env(hacker_news_aggregator, refresh_interval),
    Interval.

find_story(_Id, []) ->
    not_found;
find_story(Id, [#{<<"id">> := Id} = Story | _]) ->
    Story;
find_story(Id, [_ | Rest]) ->
    find_story(Id, Rest).
