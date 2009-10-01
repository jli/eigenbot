# Features
- Command plugin: joining, parting, loading plugins, etc.
- Dynamic configuration (related to Command plugin).
- Serializing, loading state(?).

# Clean-up, Refactoring
- More abstract plugin interface. Plugin monad? Currently very boilerplate-y.
- Abstract interface to event and action queues.
- Unify Event and Action(?).
- Better parseEvent and actionToMsg.

# Flexibility, Expressiveness
- Add timestamps to events.
- Filtering abstraction for plugins to only respond to certain events.

# Robustness
- Auto-reconnect to networks on disconnect.
- Fall back onto other servers for given network.
- Try different nick when taken.
