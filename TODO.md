- [x] HTTP get and long poll
- [ ] Buddy detection (UDP b-cast)
- [x] Multi-Logger
	- [x] harbinger writes
	- [x] select regarding seq (local if suitable)
	- [x] <s>LOCAL select in worker</s> bad idea
	- [ ] Worker pool for search in log (poolboy?)
	- [ ] Search result pagination (select(LastID, MaxItems))
- [ ] Decouple components and refine ifaces: 
	Subscribtion vs HTTP vs Serialization (json for now)
	- [x] Erlang subscription API: 
	Should hide log-search, harbinger
	and message-order issues behind
	simple stream of messages.
	- [ ] Pluggable front-end (HTTP/TCP/...)
	- [ ] Pluggable serlialization (JSON/Protobuf/...)
- [ ] Binary disk-logger for long term message history
- [ ] Tests and docs
- [ ] ?Fix potential message loss under extreme load?
- [ ] SSL?
- [ ] ?Plugins (mod + opts, prebacked shell mod)?
- [ ] ?Fields rewriting plugin?
- [ ] Internal statistics
- [ ] ?Complex subscr. equations?
