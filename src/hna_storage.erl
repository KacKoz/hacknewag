-module(hna_storage).

-behaviour(gen_server).

%% Callbacks for `gen_server`
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, start_link/0]).

-export([get_page/1, get_story/1, get_all_stories/0]).

-type story() :: #{binary() => binary() | integer()}.
-type story_id() :: non_neg_integer().

-export_type([story/0, story_id/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(any()) -> {ok, any()}.

init(_Args) ->
    erlang:send_after(refresh_interval(), self(), refresh_stories),
    self() ! refresh_stories,
    {ok, []}.

-spec get_page(pos_integer()) -> [story()].

get_page(N) when is_integer(N), N > 0 ->
    gen_server:call(?MODULE, {get_page, N}).

-spec get_all_stories() -> [story()].

get_all_stories() ->
    gen_server:call(?MODULE, get_all_stories).

-spec get_story(story_id()) -> {ok, story()} | {error, not_found}.

get_story(Id) when is_integer(Id) ->
    gen_server:call(?MODULE, {get_story, Id}).

-spec handle_call(Arg, From, Stories) -> Result when
    Arg :: {get_page, non_neg_integer()} | {get_story, hna_storage:story_id()} | get_all_stories,
    From :: {pid(), any()},
    Stories :: [hna_storage:story()],
    Result ::
        {reply, [hna_storage:story()] | {ok, hna_storage:story()} | {error, not_found}, [
            hna_storage:story()
        ]}.

handle_call({get_page, N}, _From, Stories) ->
    PageSize = application:get_env(hacker_news_aggregator, page_size, 10),
    Page = lists:sublist(Stories, ((N - 1) * PageSize) + 1, PageSize),
    {reply, Page, Stories};
handle_call({get_story, Id}, _From, Stories) ->
    {reply, find_story(Id, Stories), Stories};
handle_call(get_all_stories, _From, Stories) ->
    {reply, Stories, Stories}.

handle_cast(Request, State) ->
    logger:warning("Unexpected cast to ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.

-spec handle_info(Arg, Stories) -> Result when
    Arg ::
        refresh_stories
        | {fetched_stories, [hna_storage:story()]}
        | {'DOWN', any(), process, any(), any()},
    Stories :: [hna_storage:story()],
    Result :: {noreply, [hna_storage:story()]}.

handle_info(refresh_stories, State) ->
    Self = self(),
    spawn_monitor(
        fun() ->
            {ok, Stories} = hna_fetcher:get_stories(),
            Self ! {fetched_stories, Stories}
        end
    ),
    {noreply, State};
handle_info({fetched_stories, Stories}, _State) ->
    erlang:send_after(refresh_interval(), self(), refresh_stories),
    % Send messages to connected ws clients
    lists:foreach(
        fun(Pid) ->
            Pid ! {new_stories, Stories}
        end,
        pg:get_members(ws_connections)
    ),
    {noreply, Stories};
handle_info({'DOWN', _Ref, process, _Pid2, Reason}, State) ->
    logger:warning("Stories fetching process failed with reason: ~p~n", [Reason]),
    {noreply, State}.

-spec refresh_interval() -> non_neg_integer().

refresh_interval() ->
    {ok, Interval} = application:get_env(hacker_news_aggregator, refresh_interval),
    Interval.

-spec find_story(story_id(), [story()]) -> {ok, story()} | {error, not_found}.

find_story(_Id, []) ->
    {error, not_found};
find_story(Id, [#{<<"id">> := Id} = Story | _]) ->
    {ok, Story};
find_story(Id, [_ | Rest]) ->
    find_story(Id, Rest).
