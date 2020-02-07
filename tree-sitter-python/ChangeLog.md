# v0.9.0.1

* fix file path for `node-types.json`

# v0.9.0.0

* remove CodeGen and Template Haskell splice
* move contents of internal library into library and remove internal library
* add `getNodeTypesPath` to provide easier access to `node-types.json` file

# v0.6.0.0

* AST datatypes use a shared `Token` datatype for all anonymous leaves.
* Bumps the lower bound on `tree-sitter` to 0.5.
* AST named sum datatypes are represented as `newtype` wrappers around sums constructed with `:+:`.
* AST named & anonymous sum types are represented as balanced binary trees of `:+:`s instead of right-chained lists.
* Rename the `bytes` field of leaves to `text`.

# v0.5.0.0

* Bumps `tree-sitter` to `^>= 0.4`.

# v0.4.0.0

* Fixes a bug where comments were mistakenly being added to `extraChildren` fields. ([#204](https://github.com/tree-sitter/haskell-tree-sitter/pull/203))
* Bumps to `tree-sitter-0.3.0.0`.

# v0.3.0.0

* Adds annotation fields to all generated data types. ([#181](https://github.com/tree-sitter/haskell-tree-sitter/pull/181))
* Fixes bug where nodes would be inserted into fields in reverse order. ([#195](https://github.com/tree-sitter/haskell-tree-sitter/issues/195))
* Pulled latest tree-sitter grammar.

# v0.2.0.0

* Adds `extraChildren` field to all applicable data types. ([#179](https://github.com/tree-sitter/haskell-tree-sitter/pull/179))

# v0.1.0.0

* Initial release.
