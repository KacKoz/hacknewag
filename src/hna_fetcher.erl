-module(hna_fetcher).

-export([get_stories/0]).

-spec get_stories() -> {ok, [hna_storage:story()]}.

get_stories() ->
    case fetch_stories_ids() of
        {ok, TopIds} ->
            Stories = fetch_whole_stories(TopIds),
            {ok, Stories};
        {error, could_not_fetch} ->
            % In case of error return empty list. I decided to prioritize 
            % consistency over availability of data.
            {ok, []}
    end.

-spec fetch_stories_ids() -> {ok, [hna_storage:story_id()]} | {error, could_not_fetch}.

fetch_stories_ids() ->
    TopStoriesUrl = "https://hacker-news.firebaseio.com/v0/topstories.json",
    StoriesCount = application:get_env(hacker_news_aggregator, top_stories_count, 50),
    case httpc:request(get, {TopStoriesUrl, []}, [], [{body_format, binary}]) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} when is_binary(Body) ->
            AllIds = json:decode(Body),
            {ok, lists:sublist(AllIds, StoriesCount)};
        {error, Error} ->
            logger:error("Could not fetch ids: ~p~n", [Error]),
            {error, could_not_fetch}
    end.

-spec fetch_whole_stories([hna_storage:story_id()]) -> [hna_storage:story()].

fetch_whole_stories(Ids) ->
    ItemUrl = "https://hacker-news.firebaseio.com/v0/item/",
    % Here we could split stories fetching into multiple processes to make it faster.
    lists:reverse(
        lists:foldl(
            fun(Id, Stories) ->
                Url = ItemUrl ++ integer_to_list(Id) ++ ".json",
                case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
                    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} when is_binary(Body) ->
                        StoryMap = json:decode(Body),
                        % Here I decided to remove those fields, since from the perspective
                        % of our app they are not relevant.
                        StoryMapClean = lists:foldl(fun maps:remove/2, StoryMap, [
                            <<"descendants">>, <<"kids">>, <<"type">>
                        ]),
                        [StoryMapClean | Stories];
                    {error, Error} ->
                        logger:error("Could not fetch story with id: ~p. Reason: ~p~n", [Id, Error]),
                        % If only some stories couldn't be fetched, we will return the ones that could be.
                        % Thanks to that we retain consistency with HN without keeping old data or
                        % throwing error all the way. Pagination is also implemented in a way that handles
                        % such cases correctly.
                        Stories
                end
            end,
            [],
            Ids
        )
    ).
