# clj-reload

A smarter way to reload code. It tracks namespace dependencies, unloads and then loads them in the correct topological order.

## Differences from tools.namespace

Progress:

- [x] Parse namespases
- [x] Support in-ns
- [x] Support standalone `require/use`
- [x] Do not reload everything on first reload
- [x] Use LispReader instead of tools.reader
- [x] Support :no-reload
- [x] Support :no-unload
- [x] Only reload active namespaces
- [x] Handle errors during ns load
- [x] Handle errors during ns parse
- [x] Handle cyclic dependencies
- [x] Handle file deletion
- [x] Handle dependency graph changed
- [x] Unload hooks
- [x] Return list of nses reloaded
- [x] after-ns-reload
- [ ] :changed/:loaded/:all
- [ ] keep defonce
- [ ] keep protocols?
- [ ] keep multimethods
- [ ] ^:clj-reload/keep
- [ ] reload on file change
- [ ] ? Try to keep aliases working
- [ ] ? Get dependencies from live ns objects
- [ ] ? Use load to load files, not nses

## License

Copyright © 2024 Nikita Prokopov

Licensed under [MIT](LICENSE).
