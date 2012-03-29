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

-include_lib("amqp_client/include/amqp_client.hrl").

%%
%% API
%%

main([])       -> io:fwrite(usage());
main(_Configs) -> io:fwrite("~s", [render()]).

%%
%% Private
%%

usage() ->
    io_lib:fwrite("Usage: ~s CONFIG_FILE~n", [?MODULE]).

render() ->
    Sources = lookup(sources, test()),
    {ok, D} = config_dtl:render([
        {shovels, [shovel(S, test()) || S <- Sources]}
    ]),
    D.

shovel(Source, Ctx) ->
    Destination = lookup(destination, Ctx),
    Queue = queue_name(Source, Ctx),
    [{source, escape(Source)},
     {destination, escape(Destination)},
     {declarations, [
         exchange_declaration(Ctx)
     ]},
     {queue, escape(Queue)}].

shovel_name(Source, Ctx) ->
    {ok, #amqp_params_network{host = Host}} = amqp_uri:parse(Source),
    string:join([exchange_name(Ctx), Host], ".").

queue_name(Source, Ctx) -> shovel_name(Source, Ctx) ++ ".shovel".

exchange_name(Ctx) -> lookup(name, lookup(exchange, Ctx)).

%% Use partial views for each of the declaration types and format specifically
exchange_declaration(Ctx) ->
    Args = lists:keydelete(name, 1, lookup(exchange, Ctx)),
    {'exchange.declare', [
        {exchange, list_to_binary(exchange_name(Ctx))}
    ] ++ Args}.

test() ->
    [{sources, [
         "amqp://guest:guest@hostfoo",
         "amqp://guest:guest@hostbar"
     ]},
     {destination, "amqp://guest:guest@baz.int"},
     {exchange, [
         {name, "exchangename"},
         {type, <<"direct">>},
         durable
     ]}].

lookup(Key, List) ->
    {Key, Value} = lists:keyfind(Key, 1, List),
    Value.

escape(Str) -> lists:concat(["\"", Str, "\""]).
