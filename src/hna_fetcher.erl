-module(hna_fetcher).

-export([get_stories/0]).


-spec get_stories() -> [{non_neg_integer(), binary()}].

get_stories() ->
    {ok, TopIds} = fetch_stories_ids(),
    Stories = fetch_whole_stories(TopIds),
    {ok, Stories}.


fetch_stories_ids() ->
    TopStoriesUrl = "https://hacker-news.firebaseio.com/v0/topstories.json",
    StoriesCount = application:get_env(hacker_news_aggregator, top_stories_count, 50),
    case httpc:request(get, {TopStoriesUrl, []}, [], [{body_format, binary}]) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            AllIds = json:decode(Body),
            {ok, lists:sublist(AllIds, StoriesCount)};
        {error, _Error} ->
            {error, could_not_fetch}
    end.


fetch_whole_stories(Ids) ->
    ItemUrl = "https://hacker-news.firebaseio.com/v0/item/",
    lists:reverse(
      lists:foldl(
        fun(Id, Stories) ->
                Url = ItemUrl ++ integer_to_list(Id) ++ ".json",
                case httpc:request(get, {Url, []}, [], [{body_format, binary}]) of
                    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = _Res ->
                        StoryMap = json:decode(Body),
                        StoryMapClean = lists:foldl(fun maps:remove/2, StoryMap, [<<"descendants">>, <<"kids">>, <<"type">>]),
                        [{Id, iolist_to_binary(json:encode(StoryMapClean))} | Stories];
                    _ ->
                        Stories
                end
        end,
        [],
        Ids)).
