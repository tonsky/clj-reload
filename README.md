# clj-reload

A smarter way to reload code. It tracks namespace dependencies, unloads and then loads them in the correct topological order.

## Differences from tools.namespace

- [ ] hooks for unload
- [ ] track in-ns dependencies
- [ ] do not reload everything on first reload
- [ ] only reload active namespaces
- [ ] keep defonce
- [ ] keep protocols?
- [ ] keep multimethods
- [ ] ^:clj-reload/keep
- [ ] shorter meta keys
- [ ] reload on file change
- [ ] return list of nses reloaded
- [ ] aliases?
- [ ] excludes in set-refresh-dirs

## License

Copyright © 2024 Nikita Prokopov

Licensed under [MIT](LICENSE).
