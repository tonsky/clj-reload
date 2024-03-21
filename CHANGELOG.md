# 0.4.3 - Mar 21, 2024

- Do not report self-reference as a cycle #6

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