![](./.extras/logo.png)

Smarter way to reload Clojure code. Clj-Reload tracks namespace dependencies, unloads namespaces, and then loads them in the correct topological order.

This is only about namespace dependencies within a single project. It has nothing to do with Leiningen, Maven, JAR files, or repositories.

## Dependency

```clojure
io.github.tonsky/clj-reload {:mvn/version "0.9.5"}
```

## The problem

Do you love interactive development? Although Clojure is set up perfectly for that, evaluating buffers one at a time can only get you so far.

Once you start dealing with the state, you get data dependencies, and with them, evaluation order starts to matter, and now you change one line but have to re-eval half of your application to see the change.

But how do you know which half?

## The solution

Clj-reload to the rescue!

Clj-reload scans your source dir, figures out the dependencies, tracks file modification times, and when you are finally ready to reload, it carefully unloads and loads back only the namespaces that you touched and the ones that depend on those. In the correct dependency order, too.

Let’s do a simple example.

a.clj:

```clojure
(ns a
  (:require b))
```

b.clj:

```clojure
(ns b
  (:require c))
```

c.clj:

```clojure
(ns c)
```

Imagine you change something in `b.clj` and want to see these changes in your current REPL. What do you do?

If you call

```clojure
(clj-reload.core/reload)
```

it will notice that

- `b.clj` was changed,
- `a.clj` depends on `b.clj`,
- there’s `c.clj` but it doesn’t depend on `a.clj` or `b.clj` and wasn’t changed.

Then the following will happen:

```
Unloading a
Unloading b
Loading b
Loading a
```

So:

- `c` wasn’t touched — no reason to,
- `b` was reloaded because it was changed,
- `a` was loaded _after_ the new version of `b` was in place. Any dependencies `a` had will now point to the new versions of `b`.

That’s the core proposition of `clj-reload`.

## Usage

Initialize:

```clojure
(require '[clj-reload.core :as reload])

(reload/init
  {:dirs ["src" "dev" "test"]})
```

`:dirs` are relative to the working directory.

Use:

```clojure
(reload/reload)
; => {:unloaded [a b c], :loaded [c b a]}
```

Works best if assigned to a shortcut in your editor.

## Usage: recovering from errors

`reload` can be called multiple times. If reload fails, fix the error and call `reload` again.

Alternatively, you can call `unload` which will unload all changed code and will not try loading it back. Fix the code and call `reload` again to load it back.

## Usage: Return value

`reload` returns a map of namespaces that were reloaded:
   
```clojure
{:unloaded [<symbol> ...]
 :loaded   [<symbol> ...]}
```

By default, `reload` throws if it can’t load a namespace. You can change it to return exception instead:

```clojure
(reload/reload {:throw false})

; => {:unloaded  [a b c]
;     :loaded    [c b]
;     :failed    b
;     :exception <Throwable>}
```

## Usage: Choose what to reload

By default, clj-reload will only reload namespaces that were both:

- Already loaded
- Changed on disk

If you pass `:only :loaded` option to `reload`, it will reload all currently loaded namespaces, no matter if they were changed or not.

If you pass regexp to `:only`, clj-reload will search for matching namespaces and load them. This is useful, for example, if you want to find all tests namespaces:

```
(reload/reload {:only #".*\.-test"})
```

This will reload all changed namespaces, then find and load all unloaded namespaces that match the pattern.

Finally, if you pass `:only :all` option to `reload`, it will reload all namespaces it can find in the specified `:dirs`, no matter whether loaded or changed.

## Usage: Skipping reload

Some namespaces contain state you always want to persist between reloads. E.g. running web-server, UI window, etc. To prevent these namespaces from reloading, add them to `:no-reload` during `init`:

```clojure
(reload/init
  {:dirs ...
   :no-reload '#{user myapp.state ...}})
```

Alternatively, if you want to never unload some namespace, but still reload it (e.g. it contains important state, but only in `defonce`-s), use `:no-unload`:

```clojure
(reload/init
  {:dirs ...
   :no-unload '#{app.main ...}})
```

`:no-reload` implies `:no-unload`.

## Usage: Unload hooks

Sometimes your namespace contains stateful resource that requires proper shutdown before unloading. For example, if you have a running web server defined in a namespace and you unload that namespace, it will just keep running in the background.

To work around that, define an unload hook:

```clojure
(def my-server
  (server/start app {:port 8080}))

(defn before-ns-unload []
  (server/stop my-server))
```

`before-ns-unload` is the default name for the unload hook. If a function with that name exists in a namespace, it will be called before unloading.

You can change the name (or set it to `nil`) during `init`:

```clojure
(reload/init
  {:dirs [...]
   :unload-hook 'my-unload})
```

For symmetry, there’s also `:reload-hook 'after-ns-reload` that triggers after reload.

## Usage: Keeping vars between reloads

One of the main innovations of `clj-reload` is that it can keep selected variables between reloads.

To do so, just add `^:clj-reload/keep` to the form:

```clojure
(ns test)

(defonce x
  (rand-int 1000))

^:clj-reload/keep
(def y
  (rand-int 1000))

^:clj-reload/keep
(defrecord Z [])
```

and then reload:

```clojure
(let [x test/x
      y test/y
      z (test/->Z)]
  
  (reload/reload)
  
  (let [x' test/x
        y' test/y
        z' (test/->Z)]
    (is (= x x'))
    (is (= y y'))
    (is (identical? (class z) (class z')))))
```

Here’s how it works:

- `defonce` works out of the box. No need to do anything.
- `def`/`defn`/`deftype`/`defrecord`/`defprotocol` can be annotated with `^:clj-reload/keep` and can be persisted too.
- Project-specific forms can be added by extending `clj-reload.core/keep-methods` multimethod.

Why is this important? With `tools.namespace` you will structure your code in a way that will work with its reload implementation. For example, you’d probably move persistent state and protocols into separate namespaces, not because logic dictates it, but because reload library will not work otherwise.

`clj-reload` allows you to structure the code the way business logic dictates it, without the need to adapt to developer workflow.

Simply put: the fact that you use `clj-reload` during development does not spill into your production code.

## Usage: init-less workflow

Sometimes it might be useful to integrate `clj-reload` into your system-wide profile or into tool like CIDER to be available in all your projects without explicitly adding it as a dependency.

To support that, `clj-reload`:

- Lets you skip `init`, in which case it’ll initialize with every directory it can find on classpath,
- Supports `:clj-reload/no-reload` and `:clj-reload/no-unload` meta on namespace symbol, like this:

```clojure
(ns ^:clj-reload/no-reload no-reload
  (:require ...))
```

In that case, you can just call `clj-reload.core/reload` and it should work with default settings.

To disable initial `init`, set environment variable:

```sh
CLJ_RELOAD_AUTO_INIT=false clj -M -m ...
```

or system property:

```sh
clj -J-Dclj-reload.auto-init=false -M -m ...
```

## Usage: Reloading custom file types

If you have custom file types, like `*.repl`, you can specify that `clj-reload` should scan and reload them too:

```clojure
(reload/init
  {:dirs  [...]
   :files #".*[.](clj|cljc|repl)"})
```

## Usage: Finding namespaces

Sometimes you just want to know what namespaces are there. Since `clj-reload` does this work already anyways, you can do it, too:

```clojure
(reload/find-namespaces #".*-test")
;; => #{lib.core-test lib.impl-test ...}
```

## Usage: Controlling output

In init, you can set how much `clj-reload` will log during reload. Verbose (default):

```
=> (reload/init {:output :verbose})
=> (reload/reload)

Unloading clojure+.print-test
Unloading clojure+.print
Unloading clojure+.error
Loading clojure+.error
Loading clojure+.print
Loading clojure+.print-test
```

Quieter:

```
=> (reload/init {:output :quieter})
=> (reload/reload)

Reloaded 4 namespaces
```

Quiet (no output):

```
=> (reload/init {:output :quiet})
=> (reload/reload)
```

## Comparison: Evaluating buffer

The simplest way to reload Clojure code is just re-evaluating an entire buffer.

It works for simple cases but fails to account for dependencies. If something depends on your buffer, it won’t see these changes.

The second pitfall is removing/renaming vars or functions. If you had:

```clojure
(def a 1)

(def b (+ a 1))
```

and then change it to just

```clojure
(def b (+ a 1))
```

it will still compile! New code is evaluated “on top” of the old one, without unloading the old one first. The definition of `a` will persist in the namespace and let `b` compile.

It might be really hard to spot these errors during long development sessions.

## Comparison: `(require ... :reload-all)`

Clojure has `:reload` and `:reload-all` options for `require`. They do track upstream dependencies, but that’s about it.

In our original example, if we do

```clojure
(require 'a :reload-all)
```

it will load both `b` and `c`. This is excessive (`b` or `c` might not have changed), doesn’t keep track of downstream dependencies (if we reload `b`, it will not trigger `a`, only `c`) and it also “evals on top”, same as with buffer eval.

## Comparison: tools.namespace

[tools.namespace](https://github.com/clojure/tools.namespace) is a tool originally written by Stuart Sierra to work around the same problems. It’s a fantastic tool and the main inspiration for `clj-reload`. I’ve been using it for years and loving it, until I realized I wanted more.

So the main proposition of both `tools.namespace` and `clj-reload` is the same: they will track file modification times and reload namespaces in the correct topological order.

This is how `clj-reload` is different:

- `tools.namespace` reloads every namespace it can find. `clj-reload` only reloads the ones that were already loaded. This allows you to have broken/experimental/auxiliary files lie around without breaking your workflow [TNS-65](https://clojure.atlassian.net/browse/TNS-65)

- First reload in `tools.namespace` always reloads everything. In `clj-reload`, even the very first reload only reloads files that were actually changed [TNS-62](https://clojure.atlassian.net/browse/TNS-62)

- `clj-reload` supports namespaces split across multiple files (like `core_deftype.clj`, `core_defprint.clj` in Clojure) [TNS-64](https://clojure.atlassian.net/browse/TNS-64)

- `clj-reload` can see dependencies in top-level standalone `require` and `use` forms [TNS-64](https://clojure.atlassian.net/browse/TNS-64)

- `clj-reload` supports load and unload hooks per namespace. Since `tools.namespace` doesn’t report which namespaces it’s going to reload, you always have to rebuild the entire state [TNS-63](https://clojure.atlassian.net/browse/TNS-63)

- `clj-reload` can specify exclusions during configuration, without polluting the source code of those namespaces.

- `clj-reload` can keep individual vars around and restore previous values after reload. E.g. `defonce` doesn’t really work with `tools.namespace`, but it does with `clj-reload`.

- `clj-reload` has 2× smaller codebase and 0 runtime dependencies.

- `clj-reload` doesn’t support ClojureScript. Patches welcome.

## A word of caution

Clj-reload works by removing whole namespaces. Everything in them including vars is gone. This is different from how Clojure REPL usually works, in which calling `def` twice will just override its _value_, keeping the var itself the same.

So if you store a link to a var somewhere, it’ll be pointing to the old version after reload. If you required/aliased a namespace, it’ll be pointing to the old version after reload.

For your code to see new changes, use `(resolve 'full.ns/sym)` instead of just `full.ns/sym` or even `@#'full.ns/sym`, and do not put namespaces you’ll be reloading in `:require` portion of your REPL namespace.

## ClojureScript?

`clj-reload` doesn’t support ClojureScript. Patches are welcome.

## License

Copyright © 2024 Nikita Prokopov

Licensed under [MIT](LICENSE).
