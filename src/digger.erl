%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

%% Arguments:
%%   Source:
%%     Amqp Urls
%%     Exchange (+ details)
%%     Routing Key

%%   Destination:
%%     Amqp Url
%%     Exchange (+ details)

%%   Prefetch
%%   Reconnect Delay in seconds

%% Actions:
%%   Create a named shovel for each unique source amqp url
%%   Declare the intermediary shovel queue with a sensible name
%%   Ack Mode should be on_confirm

%% Questions:
%%   Durability, persistent messages ala federation queues

-module(digger).

%% API
-export([main/1]).

%%
%% API
%%

main([]) -> io:fwrite(usage());

main(Configs) -> ok.

%%
%% Private
%%

usage() ->
    io_lib:fwrite("Usage: ~s CONFIG_FILE~n", [?MODULE]).
