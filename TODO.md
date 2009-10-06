# Features
- Github plugin. Handle per-channel subscriptions and subscription requests.
- RSS plugin.
- Quotes plugin: !quote command for saving quotes. Parsing of irssi-like output so people can paste entire lines(?). !quote lastmsgfrom nick to save last message(?).
- Weather plugin: !weather and !forecast. Take a zip code?
- Zippy plugin: spout madness upon seeing uppercase channel messages
- Dict plugin
- Spell plugin
- CTCP for.. something or other? http://www.irchelp.org/irchelp/rfc/ctcpspec.html
- Config file and commandline selection of networks/channels/plugins.

# Clean-up, Refactoring
- More abstract plugin interface. Plugin monad? Currently *extremely* boilerplate-y.
- Better parseEvent and actionToMsg. Use Parsec or something for parseEvent.
- Plugin-related types are confusing.
- Use hslogger instead of putStrLn/printf.

# Flexibility, Expressiveness
- Filtering abstraction to allow concise matching/filtering code.
- Dynamic network connections.
- Dynamic plugin (un)loading
- Serializing, loading state.
- Per-network nicks. Dynamic nick changing.

# Robustness
- Auto-reconnect to networks on disconnect.
- Fall back on other servers for given network.
- Try different nick when taken.
