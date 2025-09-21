-module(hna_http).

-behaviour(cowboy_rest).

%% Callbacks for `cowboy_handler`
-export([init/2]).


init(Req, State) ->
    Path = cowboy_req:path(Req),
    case Path of
        <<"/page/", _/binary>> ->
            page_handler(Req, State);
        <<"/story/", _/binary>> ->
            story_handler(Req, State)
    end.


page_handler(Req0, State) ->
    PageNum = cowboy_req:binding('PageNum', Req0),
    logger:error("Page: ~p~n", [PageNum]),
    Page = hna_storage:get_page(PageNum),
    Req = cowboy_req:reply(200, #{
          <<"content-type">> => <<"text/json">>
         },
         json:encode(Page),
         Req0
    ),
    {ok, Req, State}.


story_handler(Req0, State) ->
    StoryId = cowboy_req:binding('StoryId', Req0),
    Req =
        case hna_storage:get_story(StoryId) of
            {ok, Story} ->
                cowboy_req:reply(200, #{
                        <<"content-type">> => <<"text/json">>
                    },
                    json:encode(Story),
                    Req0
                );
            {error, not_found} ->
                cowboy_req:reply(404, Req0)
        end,
    {ok, Req, State}.
