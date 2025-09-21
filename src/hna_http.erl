-module(hna_http).

-behaviour(cowboy_rest).

%% Callbacks for `cowboy_handler`
-export([init/2]).


init(Req, State) ->
    Path = cowboy_req:path(Req),
    case Path of
        <<"/page/",_/binary>> ->
            page_handler(Req, State);
        <<"/story/",_/binary>> ->
            story_handler(Req, State)
    end.

page_handler(Req0, State) ->
    PageNumBin = cowboy_req:binding('PageNum', Req0),
    PageNum = list_to_integer(binary_to_list(PageNumBin)),
    logger:error("Page: ~p~n", [PageNum]),
    Page = hna_storage:get_page(PageNum),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/json">>
    }, json:encode(Page), Req0),
    {ok, Req, State}.

story_handler(Req0, State) ->
    StoryIdBin = cowboy_req:binding('StoryId', Req0),
    StoryId = list_to_integer(binary_to_list(StoryIdBin)),
    Story = hna_storage:get_story(StoryId),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/json">>
    }, json:encode(Story), Req0),
    {ok, Req, State}.

