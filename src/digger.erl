%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(digger).

%% API
-export([main/1]).

-include_lib("amqp_client/include/amqp_client.hrl").

-type context() :: [proplists:propery()].
-type config()  :: [context()].

%%
%% API
%%

-spec main([string()]) -> ok.
%% @doc
main([]) ->
    main(["bin/digger.conf"]);
main([Path]) ->
    {ok, Config} = file:consult(Path),
    io:fwrite("~s", [render(Config)]).

TODO load the resulting string to check erlang syntax

%%
%% Private
%%

-spec usage() -> ok.
%% @private
usage() -> io_lib:fwrite("Usage: ~s CONFIG_FILE~n", [?MODULE]).

-spec render(config()) -> string().
%% @private
render(Config) ->
    {ok, D} = config_dtl:render([
        {shovels, lists:concat([shovels(C) || C <- Config])}
    ]),
    D.

-spec shovels(context()) -> [].
%% @private
shovels(Ctx) -> [shovel(S, Ctx) || S <- lookup(sources, Ctx)].

-spec shovel(string(), context()) -> context().
%% @private
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
     {exchange, [
         {declaration, exchange_declaration(Ctx)},
         {name, "<<" ++ escape(exchange_name(Ctx)) ++ ">>"}
     ]},
     {queue, "<<" ++ escape(Queue) ++ ">>"}].

-spec shovel_name(string(), context()) -> string().
%% @private
shovel_name(Source, Ctx) ->
    {ok, #amqp_params_network{host = Host}} = amqp_uri:parse(Source),
    string:join([exchange_name(Ctx), Host], ".").

-spec queue_name(string(), context()) -> string().
%% @private
queue_name(Source, Ctx) -> shovel_name(Source, Ctx) ++ ".shovel".

-spec queue_declaration(string(), context()) -> string().
%% @private
queue_declaration(Source, Ctx) ->
    sterm({'queue.declare', [
        {queue, bin(queue_name(Source, Ctx))},
        durable
    ]}).

-spec exchange_name(context()) -> string().
%% @private
exchange_name(Ctx) -> lookup(name, lookup(exchange, Ctx)).

-spec exchange_declaration(context()) -> string().
%% @private
exchange_declaration(Ctx) ->
    sterm({'exchange.declare', [
        {exchange, bin(exchange_name(Ctx))}
        | lists:keydelete(name, 1, lookup(exchange, Ctx))
    ]}).

-spec binding_declarations(string(), context()) -> string().
%% @private
binding_declarations(Source, Ctx) ->
    [sterm({'queue.bind', [
        {exchange, bin(exchange_name(Ctx))},
        {queue, bin(queue_name(Source, Ctx))},
        {routing_key, bin(K)}
     ]}) || K <- lookup(routing_keys, Ctx)].

-spec lookup(atom(), [proplists:property()]) -> any().
%% @private
lookup(Key, List) ->
    {Key, Value} = lists:keyfind(Key, 1, List),
    Value.

-spec escape(string()) -> string().
%% @private
escape(Str) -> lists:concat(["\"", Str, "\""]).

-spec bin(list() | atom() | binary()) -> binary().
%% @private
bin(List) when is_list(List)  -> list_to_binary(List);
bin(Atom) when is_atom(Atom)  -> atom_to_binary(Atom, latin1);
bin(Bin)  when is_binary(Bin) -> Bin.

-spec sterm(term()) -> binary().
%% @private
sterm(Term) -> iolist_to_binary(io_lib:fwrite("~600p", [Term])).
