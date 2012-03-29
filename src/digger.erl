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

-compile(export_all).

%%
%% API
%%

main([]) ->
    main(["bin/digger.conf"]);
main([Path]) ->
    {ok, Config} = file:consult(Path),
    io:fwrite("~s", [render(Config)]).

%%
%% Private
%%

usage() ->
    io_lib:fwrite("Usage: ~s CONFIG_FILE~n", [?MODULE]).

render([Config]) ->
    {ok, D} = config_dtl:render([
        {shovels, lists:concat([shovels(C) || C <- Config])}
    ]),
    D.

shovels(Ctx) -> [shovel(S, Ctx) || S <- lookup(sources, Ctx)].

shovel(Source, Ctx) ->
    Destination = lookup(destination, Ctx),
    Queue = queue_name(Source, Ctx),
    [{name, shovel_name(Source, Ctx)},
     {source, escape(Source)},
     {destination, escape(Destination)},
     {declarations, [
         exchange_declaration(Ctx),
         queue_declaration(Source, Ctx)
         | binding_declarations(Source, Ctx)
     ]},
     {queue, "<<" ++ escape(Queue) ++ ">>"}].

shovel_name(Source, Ctx) ->
    {ok, #amqp_params_network{host = Host}} = amqp_uri:parse(Source),
    string:join([exchange_name(Ctx), Host], ".").

queue_name(Source, Ctx) -> shovel_name(Source, Ctx) ++ ".shovel".

queue_declaration(Source, Ctx) ->
    {'queue.declare', [
        {queue, bin(queue_name(Source, Ctx))},
        durable
    ]}.

exchange_name(Ctx) -> lookup(name, lookup(exchange, Ctx)).

%% Use partial views for each of the declaration types and format specifically
exchange_declaration(Ctx) ->
    {'exchange.declare', [
        {exchange, bin(exchange_name(Ctx))}
        | lists:keydelete(name, 1, lookup(exchange, Ctx))
    ]}.

binding_declarations(Source, Ctx) ->
    [{'queue.bind', [
        {exchange, bin(exchange_name(Ctx))},
        {queue, bin(queue_name(Source, Ctx))},
        {routing_key, bin(K)}
    ]} || K <- lookup(routing_keys, Ctx)].

lookup(Key, List) ->
    {Key, Value} = lists:keyfind(Key, 1, List),
    Value.

escape(Str) -> lists:concat(["\"", Str, "\""]).

bin(List) when is_list(List)  -> list_to_binary(List);
bin(Atom) when is_atom(Atom)  -> atom_to_binary(Atom, latin1);
bin(Bin)  when is_binary(Bin) -> Bin.
