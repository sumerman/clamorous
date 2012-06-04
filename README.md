Clamorous
=========

HTTP pub/sub service with complex filtering ability.

Suppose you have an intensive stream of events on the one side 
and bunch of clients on the other. Futhermore, each of those clients is 
interested only in 1/100 of all events. **Clamorous** is designed to 
solve exactly this kind of problems.

__Author:__ Valery Meleshkin ([`valery.meleshkin@gmail.com`](mailto:valery.meleshkin@gmail.com))

Environment
-----------
* Few aggressive publishers.
* Bunch of fastidious subscribers.
* Both types of clients prefer long sessions.
* Data outdates fast or very vast.
* Data may be quite big, so not every field must be indexed.

Features
--------
* Publishing via HTTP and Erlang interfaces.
* HTTP streaming subscription.
* HTTP long-polling subscription.
* Subscription with conjunction of equations on fields and their values.
* Ability to select the fields which should be indexed and may be used 
for subscription.
* In-memory log holds messages for fixed (customizable) period of time 
and allows subscribers to read their stream 
from the point where disconnect happened last time.
* Distributed installation.

HTTP interface
--------------

For now HTTP and JSON are the primal way to interact 
with the service so we start from it.

### Publish

To publish a message you should issue a HTTP POST request 
carrying a JSON object(s) in its BODY.

You may publish one object:  
`$ curl -XPOST http://localhost:8080/clamorous/publish -d "{\"foo\":\"bar\",\"idx\":0}"`
`> {"status":"ok","description":"done"}`

Or an array of objects:  
`$ curl -XPOST http://localhost:8080/clamorous/publish -d\    
"[{\"foo\":\"bar\",\"idx\":0}, {\"foo\":\"bar\",\"idx\":1}]"`   
`> {"status":"ok","description":"done"}`

Anything but an object or an array of objects will be rejected.   
Some or all fields of objects will be indexed just after publishing. 

Actually the session will not be closed and publisher will be able 
to continue pushing messages:

`$ telnet localhost 8080`

	-Trying 127.0.0.1...
	-Connected to localhost.
	-Escape character is '^]'.
	<POST /clamorous/publish HTTP/1.1
	<Host: localhost:8080
	<Content-Length: 22
	<
	<{"foo":"http","omg":2}
	
	>HTTP/1.1 200 OK
	>Access-Control-Allow-Origin: *
	>Variances: Accept
	>Content-Type: application/json
	>Content-Length: 36
	>Date: Sun, 03 Jun 2012 17:20:17 GMT
	>Server: Cowboy
	>Connection: keep-alive
	>
	>{"status":"ok","description":"done"}

	<POST /clamorous/publish HTTP/1.1
	<Host: localhost:8080
	<Content-Length: 22
	<
	<{"foo":"http","omg":3}
	
	>HTTP/1.1 200 OK
	>Access-Control-Allow-Origin: *
	>Variances: Accept
	>Content-Type: application/json
	>Content-Length: 36
	>Date: Sun, 03 Jun 2012 17:20:23 GMT
	>Server: Cowboy
	>Connection: keep-alive
	>
	>{"status":"ok","description":"done"}

	-...

### Subscribe

Subscriptions! The party is because of them!

All kinds of subscriptions share some concepts:

1. *HTTP GET* requests.
2. Sequenced IDs.
3. Fields equations.

An URL of a subscription may look like this:  
`http://host:port/clamorous/subscribe/TYPE/SEQ?F1=V1&F2=V2&...`  
Where **SEQ** may be a magic-number (we will talk about it later) or the word `new`.  
**Fi** should be the name of the field in a JSON which is expected to be equal to **Vi**.  
And subsequent response will look like an array or a stream of objects similar to  
`{"id":SEQ,"data":{"F1":"V1","F1":V1}}`

For example subscription via  
`http://.../subscribe/stream/1338744023790433?username=bar&userid=2`
will generate a stream of objects published after the one with ID 
`1338744023790433` and with field `username`=`bar` and field `userid`=`2`.
Subscription via `http://.../subscribe/stream/new?username=bar&userid=2`
produce the feed with objects published after the subscription request 
only but with the same restrictions applied to the values of the fields.

If you don't want to filter a stream somehow you may issue 
the request without any equations: `http://.../subscribe/stream/new`. 
As you could have already guessed with this kind of request comes the 
stream of all newly published messages.
And `http://.../subscribe/stream/1338744023790433` in turn leads
to the stream of objects published after one with ID `1338744023790433`.

You even are able to issue something like:  
`http://localhost:8080/clamorous/subscribe/stream/0`  
But be ready to wait and receive **ALL** objects stored in the 
history and than all newly published objects.  
**Be very careful with this kind of requests!  
Clamorous is not designed for coping with this constantly!**

With the help of sequenced IDs you can continue a subscription after 
reconnect from the message next to the last received one. As if nothing 
happened. (Actually only if the time passed from disconnection is less 
than max storage time of the history item. Otherwise some messages 
may be dropped).

Let's look closer to the types of subscriptions.

#### Stream

Stream requests are the ones which contain the word `stream` in their URL 
just after the word `subscribe` and subsequent response for them 
is an endless *chunked HTTP response* with `\n` separated JSON objects.

`$ curl http://localhost:8080/clamorous/subscribe/stream/0`

	{"id":1338744719249356,"data":{"foo":"bar","idx":10,"bool":false,"array":[]}}
	{"id":1338744719267724,"data":{"foo":"bar","idx":11,"bool":false,"array":[]}}
	{"id":1338744719280400,"data":{"foo":"bar","idx":12,"bool":false,"array":[]}}
	{"id":1338744719294237,"data":{"foo":"bar","idx":13,"bool":false,"array":[]}}
	...

`$ curl http://localhost:8080/clamorous/subscribe/stream/1338744719249356?foo=bar\&idx=11`

	{"id":1338744719267724,"data":{"foo":"bar","idx":11,"bool":false,"array":[]}}
	{"id":1338750084433716,"data":{"foo":"bar","idx":11,"bool":false,"array":[]}}
	...

#### Long poll

To issue long-polling request you should put the word `wait` 
in the place of request TYPE. In response you (almost) instantly get 
all messages (as an array of JSON objects) published 
after one with given ID or connection hangs until one's arrival.

`$ curl http://localhost:8080/clamorous/subscribe/wait/1338744719280400`

	[{"id":1338750084379212,"data":{"foo":"bar","idx":10,"bool":false,"array":[]}}
	,{"id":1338750084405974,"data":{"foo":"bar","idx":11,"bool":false,"array":[]}}
	,{"id":1338750084419787,"data":{"foo":"bar","idx":12,"bool":false,"array":[]}}
	,{"id":1338750084433716,"data":{"foo":"bar","idx":13,"bool":false,"array":[]}}
	]

`http://.../subscribe/wait/new?...` also may be useful;
it waits of publishing of specified message:

	[{"id":1338750771033455,"data":{"foo":"bar","idx":10,"bool":false,"array":[]}}
	]

#### Get

Get requests are very similar to their long-polling bros, 
but they never wait for anything.

`curl http://localhost:8080/clamorous/subscribe/get/1338744719280400`

	[{"id":1338750084433716,"data":{"foo":"bar","idx":13,"bool":false,"array":[]}}
	...
	,{"id":1338750771184385,"data":{"foo":"bar","idx":13,"bool":false,"array":[]}}
	]

Get request with `SEQ`=`new` is useless since it will always return `[]`.
(At least in the current implementation)

Erlang interface
----------------

For now there is *public* Erlang API only for publishing.  
I hope I'll be able to describe its counterpart in the near future :)

### Publish

TODO

Configuration
-------------

Here is the text of the node's config file with the default values.


	[
		{clamorous, [
			{port,8080},		    % a HTTP port to listen
			{match_fields,[]},
			{discovery,false},      % autodicovery switch
			{discovery_port,19090}, % autodicovery port
			{history_storage_time,{0,10,0}}
		]},
		{sasl, [
			{sasl_error_logger, {file, "log/sasl-error.log"}},
			{errlog_type, error},
			{error_logger_mf_dir, "log/sasl"},      % Log directory
			{error_logger_mf_maxbytes, 10485760},   % 10 MB max file size
			{error_logger_mf_maxfiles, 5}           % 5 files max
		]},
		{lager, [
			{crash_log, "log/crash.log"},
			{handlers, [
				{lager_console_backend, info},
				{lager_file_backend, [
					{"log/error.log", error, 10485760, "$D0", 5},
					{"log/console.log", info, 10485760, "$D0", 5}
				]}
			]}
		]}
	].

Let's take a closer look at the `clamorous` section, 
[`sasl`](http://www.erlang.org/doc/apps/sasl/index.html) and
[`lager`](https://github.com/basho/lager) are not my stuff, 
their docs may be found at the corresponging project pages.


### In-memory cache

To customize the amount of time for which every message would be
available in history you may alter the value of the 
`{history_storage_time,{H,M,S}}` entry, where `H`, `M` and `S` 
is *Hours*, *Minutes* and *Seconds* respectively.

### Match fields and big-documents

For the case when messages are expected to have many fields only
few of which would be used in subscriptions there may be match
fields specification provided by setting the `match_fields` to
the list of field's names, e.g.: `{match_fields,[idx, <<"foo">>, "bar"]}`. 
Subscription filter will behave as if fields, which is not mentioned 
in this list, weren't presented in a published message, but subscriber
will receive the whole message. Empty `match_fields` spec list means
that every field in every object will be indexed.

Build
-----

To build **Clamorous** you need the machine with Erlang R15 installed.  
`make rel` should produce working Erlang-release in the `./rel/clamorous` folder.  

It can be started with `./rel/clamorous/bin/clamorous console` or  
`... clamorous start` (do not forget to stop it :) 
replace `start` with `stop` to do it).
Configs can be found at `./rel/clamorous/etc/app.config`

It also can be copied to another machine and started 
there without separate Erlang installation.


Distributed setup
-----------------

TODO

Performance
-----------

TODO
Load vs get-history vs wipe-history
