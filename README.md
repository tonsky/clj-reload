# clj-reload

A smarter way to reload code. It tracks namespace dependencies, unloads and then loads them in the correct topological order.

## Differences from tools.namespace

Progress:

- [x] Parse namespases
- [x] Support in-ns
- [x] Support standalone `require/use`
- [x] Do not reload everything on first reload
- [x] Support :no-unload
- [x] Support :no-load
- [x] Only reload active namespaces
- [ ] Handle errors during ns unload
- [x] Handle errors during ns load
- [x] Handle errors during ns parse
- [ ] Handle cyclic dependencies
- [x] Handle file deletion
- [x] Handle dependency graph changed
- [ ] hooks for unload
- [ ] keep defonce
- [ ] keep protocols?
- [ ] keep multimethods
- [ ] ^:clj-reload/keep
- [x] shorter meta keys
- [ ] reload on file change
- [ ] return list of nses reloaded
- [ ] ? Try to keep aliases working
- [ ] ? Get dependencies from live ns objects
- [ ] ? Use load to load files, not nses
- [x] Use LispReader instead of tools.reader

## License

Copyright © 2024 Nikita Prokopov

Licensed under [MIT](LICENSE).
