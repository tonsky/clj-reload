# WIP

- Don't re-parse files that didn't change on disk #18

# 0.8.0 - Feb 13, 2025

- Add `CLJ_RELOAD_AUTO_INIT` env variable and `clj-reload.auto-init` system property to optionally disable init-less workflow #16

# 0.7.1 - June 11, 2024

- Omitting `:dirs` will use system classpath
- Add `clj-reload.core/classpath-dirs`

# 0.7.0 - May 4, 2024

- [ BREAKING ] `:only` argument will force load unloaded namespaces, but will not reload unchanged ones
- Added `find-namespaces`

# 0.6.0 - May 3, 2024

- Disabled parallel init/reload via lock #9

# 0.5.0 - Apr 15, 2024

- Added `:files` option for custom file patterns #8 via @danieroux
- Extracted `core/*config*` from `core/*state`

# 0.4.3 - Mar 21, 2024

- Do not report self-reference as a cycle #6
- Parse record ctor syntax #7

# 0.4.2 - Mar 20, 2024

- Speed up topo-sort #5

# 0.4.1 - Mar 7, 2024

- Fixed issues when adding/removing keeps

# 0.4.0 - Mar 4, 2024

- Added `unload`

# 0.3.0 - Feb 29, 2024

- Support passing regexp as `:only` option

# 0.2.0 - Feb 23, 2024

Support optional “init-less” workflow:

- Initialize by default with all dirs on classpath
- Support `:clj-reload/no-reload` and `:clj-reload/no-unload` meta on ns

# 0.1.3 - Feb 21, 2024

- Support namespaces defined in multiple files #3

# 0.1.2 - Feb 20, 2024

- Fixed parsing files with aliased keywords #2
- Support `:as-alias`

# 0.1.1 - Feb 18, 2024

- Support keeping private defs #1
- Throw on unsupported keep forms

# 0.1.0 - Feb 17, 2024

- Initial