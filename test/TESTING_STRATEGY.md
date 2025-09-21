# Tests plan

## hna_fetcher

This module's main task is to fetch data from external API. We don't want our tests to depend on it, so I would mock the httpc:request callback in the unit tests. The tests should include both success and failure path for both endpoints (top stories and single story).
I would test scenarios like:
- Only some stories were fetched successfully.
- Number of stories is less then 50
- request returned error.

## hna_storage

Here we have many different levels of functions to test.
First - a simple function find_story. I would prepare some simple input + edge cases and test the output.
Second - functions that depend on the state (mainly gen_server callbacks). I would test them calling them directly with prepared state, checking how they would behave. Especially I would pay attention to the pagination tests since it is the least trivial one. I would also create a few processes and add them to the ws_connections group, to see if they get the updated stories.
Third - a function depending on external api. We have the refresh_stories message in handle_info. I would test it by running the gen_server, mocking the hna_fetcher:get_stories/0 function and checking if the server state was correctly updated.

## hna_ws

This module is quite simple and depends mostly on functions tested elsewhere. I would create an integration test that would run the handler and check if it sends correct data. In such case I would mock hna_storage interface functions.

## hna_http

This module is similar to the hna_ws in terms of testing.
I would tests scenarios like:
- Check success paths for the requests.
- Expect 404 when requesting non-existing story id.
