# Features
- Command plugin: joining, parting, loading plugins, etc.
- Github plugin: send message for each commit.
- Quotes plugin: !quote command for saving quotes. Parsing of irssi-like output so people can paste entire lines(?). !quote lastmsgfrom nick to save last message(?).
- Weather plugin: !weather and !forecast. Take a zip code?
- Zippy plugin: spout madness upon seeing uppercase channel messages
- CTCP for.. something or other? http://www.irchelp.org/irchelp/rfc/ctcpspec.html
- Config file and commandline selection of networks/channels/plugins.

# Clean-up, Refactoring
- More abstract plugin interface. Plugin monad? Currently *extremely* boilerplate-y.
- Unify Event and Action types, and/or event and action queues(?).
- Better parseEvent and actionToMsg. Use Parsec or something for parseEvent.

# Flexibility, Expressiveness
- Add timestamps to events.
- Filtering abstraction to allow concise matching/filtering code.
- Dynamic changes to runtime state (networks, channels, plugins). Needed for Command plugin.
- Serializing, loading state(?).

# Robustness
- Auto-reconnect to networks on disconnect.
- Fall back on other servers for given network.
- Try different nick when taken.
