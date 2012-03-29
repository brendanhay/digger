%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(digger_view).

-include_lib("amqp_client/include/amqp_client.hrl").

%% Defaults
-export([prefetch_count/0,
         ack_mode/0,
         publish_properties/0,
         publish_fields/0,
         reconnect_delay/0]).

%% Overrides
-export([shovels/1]).

%%
%% Defaults
%%

prefetch_count()     -> 1000.

ack_mode()           -> on_confirm.

publish_properties() -> [].

publish_fields()     -> [].

reconnect_delay()    -> 10.

%%
%% Overrides
%%

shovels(Ctx) -> [].

%%
%% Private
%%

shovel(Source, Ctx) ->
    Destination = lookup(destination, Ctx),
    Queue = queue_name(Source, Ctx),
    dict:from_list([
     {name, shovel_name(Source, Ctx)},
     {source, escape(Source)},
     {destination, escape(Destination)},
     %% {declarations, exchange_declaration(Ctx)},
     %% {declarations, [
     %%     exchange_declaration(Ctx),
     %%     queue_declaration(Source, Ctx)
     %%     | binding_declarations(Source, Ctx)
     %% ]},
     {queue, "<<" ++ escape(Queue) ++ ">>"}
    ]).

shovel_name(Source, Ctx) ->
    {ok, #amqp_params_network{host = Host}} = amqp_uri:parse(Source),
    string:join([exchange_name(Ctx), Host], ".").

queue_name(Source, Ctx) -> shovel_name(Source, Ctx) ++ ".shovel".

queue_declaration(Source, Ctx) ->
    [{queue, bin(queue_name(Source, Ctx))},
     durable].

exchange_name(Ctx) -> lookup(name, lookup(exchange, Ctx)).

%% Use partial views for each of the declaration types and format specifically
exchange_declaration(Ctx) ->
    [{exchange, bin(exchange_name(Ctx))}
     | lists:keydelete(name, 1, lookup(exchange, Ctx))].

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
