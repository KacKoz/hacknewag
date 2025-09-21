-module(hna_fetcher).

-export([get_stories/0]).

-spec get_stories() -> {ok, [hna_storage:story()]}.

get_stories() ->
    {ok, TopIds} = fetch_stories_ids(),
    Stories = fetch_whole_stories(TopIds),
    {ok, Stories}.

-spec fetch_stories_ids() -> {ok, [hna_storage:story_id()]} | {error, could_not_fetch}.

fetch_stories_ids() ->
    TopStoriesUrl = "https://hacker-news.firebaseio.com/v0/topstories.json",
    StoriesCount = application:get_env(hacker_news_aggregator, top_stories_count, 50),
    case httpc:request(get, {TopStoriesUrl, []}, [], [{body_format, binary}]) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} when is_binary(Body) ->
            AllIds = json:decode(Body),
            {ok, lists:sublist(AllIds, StoriesCount)};
        {error, _Error} ->
            {error, could_not_fetch}
    end.

-spec fetch_whole_stories([hna_storage:story_id()]) -> [hna_storage:story()].

fetch_whole_stories(Ids) ->
    ItemUrl = "https://hacker-news.firebaseio.com/v0/item/",
    lists:reverse(
        lists:foldl(
            fun(Id, Stories) ->
                Url = ItemUrl ++ integer_to_list(Id) ++ ".json",
                case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
                    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
                        StoryMap = json:decode(Body),
                        StoryMapClean = lists:foldl(fun maps:remove/2, StoryMap, [
                            <<"descendants">>, <<"kids">>, <<"type">>
                        ]),
                        [StoryMapClean | Stories];
                    _ ->
                        Stories
                end
            end,
            [],
            Ids
        )
    ).
