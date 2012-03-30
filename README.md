Digger
===

[![Build Status](https://secure.travis-ci.org/brendanhay/digger.png)](http://travis-ci.org/brendanhay/digger)


<a name="introduction" />

Introduction
------------

Digger is a quick and dirty command-line tool to generate Shovel configurations for connection brokers using a `publish-one/subscribe-many` model for RabbitMQ.

**Input**

````erlang
[
    %% This proplist constitutes many-sources containing the
    %% same exchange and a single-destination
    [{sources, [
         "amqp://guest:guest@hostfoo",
         "amqp://guest:guest@hostbar"
     ]},
     {destination, "amqp://guest:guest@baz.int"},
     {exchange, [
         {name, "exchangename"},
         {type, <<"direct">>},
         durable
     ]},
     {routing_keys, [
         "Track",
         "Tag"
     ]}]
].
````

**Output**

````erlang
{rabbitmq_shovel, [
    {shovels, [
        {exchangename.hostfoo, [
            {sources, [
                {broker, "amqp://guest:guest@hostfoo"},
                {declarations, [
                    {'exchange.declare',[{exchange,<<"exchangename">>},{type,<<"direct">>},durable]},
                    {'queue.declare',[{queue,<<"exchangename.hostfoo.shovel">>},durable]},
                    {'queue.bind',[{exchange,<<"exchangename">>},{queue,<<"exchangename.hostfoo.shovel">>},{routing_key,<<"Track">>}]},
                    {'queue.bind',[{exchange,<<"exchangename">>},{queue,<<"exchangename.hostfoo.shovel">>},{routing_key,<<"Tag">>}]}
                ]}
            ]},
            {destinations, [
                {broker, "amqp://guest:guest@baz.int"},
                {declarations, [
                    {'exchange.declare',[{exchange,<<"exchangename">>},{type,<<"direct">>},durable]}
                ]}
            ]},
            {queue, <<"exchangename.hostfoo.shovel">>},
            {prefetch_count, 1000},
            {ack_mode, on_confirm},
            {publish_properties, []},
            {publish_fields, [{publish_fields,[{exchange,<<"exchangename">>}]}]},
            {reconnect_delay, 10}
        ]},

        {exchangename.hostbar, [
            {sources, [
                {broker, "amqp://guest:guest@hostbar"},
                {declarations, [
                    {'exchange.declare',[{exchange,<<"exchangename">>},{type,<<"direct">>},durable]},
                    {'queue.declare',[{queue,<<"exchangename.hostbar.shovel">>},durable]},
                    {'queue.bind',[{exchange,<<"exchangename">>},{queue,<<"exchangename.hostbar.shovel">>},{routing_key,<<"Track">>}]},
                    {'queue.bind',[{exchange,<<"exchangename">>},{queue,<<"exchangename.hostbar.shovel">>},{routing_key,<<"Tag">>}]}
                ]}
            ]},
            {destinations, [
                {broker, "amqp://guest:guest@baz.int"},
                {declarations, [
                    {'exchange.declare',[{exchange,<<"exchangename">>},{type,<<"direct">>},durable]}
                ]}
            ]},
            {queue, <<"exchangename.hostbar.shovel">>},
            {prefetch_count, 1000},
            {ack_mode, on_confirm},
            {publish_properties, []},
            {publish_fields, [{publish_fields,[{exchange,<<"exchangename">>}]}]},
            {reconnect_delay, 10}
        ]}
    ]}
]}
````

<a name="contribute" />

Contribute
----------

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/digger/issues).


<a name="licence" />

Licence
-------

Digger is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
