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
    % The fetching happens in separate process so we don't block this gen_server.
    Self = self(),
    spawn_monitor(
        fun() ->
            % I was thinking about passing current stories here as an argument, so the
            % fetcher doesn'g have to pull whole story info for the stories we already have.
            % The problem with this is that we wouldn't get updates on number of upvotes for example.
            % It would depend on the buissness requirements for the endpoints.
            {ok, Stories} = hna_fetcher:get_stories(),
            Self ! {fetched_stories, Stories}
        end
    ),
    {noreply, State};
handle_info({fetched_stories, Stories}, _State) ->
    % I was considering in which format to store the stories. I left it as a list
    % because we need to retain the order of the elements for pagination and ws connections.
    % The only drawback of this is that calling /story/id endpoint forces us to search for
    % the story in linear time. For such a small list I think it isn't a problem, however
    % for larger lists and/or very frequent single story calls we could use an redundant
    % associative data structure for faster lookup but the memory usage would be higher.
    erlang:send_after(refresh_interval(), self(), refresh_stories),
    % Send messages to connected ws clients
    lists:foreach(
        fun(Pid) ->
            Pid ! {new_stories, Stories}
        end,
        pg:get_members(ws_connections)
    ),
    {noreply, Stories};
handle_info({'DOWN', _Ref, process, _Pid2, normal}, State) ->
    % Process finished succesfully
    {noreply, State};
handle_info({'DOWN', _Ref, process, _Pid2, Reason}, _State) ->
    % Here we could potentially start the fetching process again. I decided to let it begin after
    % the interval passes again.
    logger:warning("Stories fetching process failed with reason: ~p~n", [Reason]),
    % I change the state to an empty list, because we couldn't get the newest stories from HN.
    % I'm choosing being up to date before being accessible here. It is also consistent with the fact that
    % if the HN api is down we will also set the state to an empty list.
    % This of course could be changed to meet the buisness needs.
    {noreply, []}.

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
