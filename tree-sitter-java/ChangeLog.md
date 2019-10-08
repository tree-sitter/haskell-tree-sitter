# v0.4.0.0

* AST datatypes use a shared `Token` datatype for all anonymous leaves.
* Bumps the lower bound on `tree-sitter` to 0.5.
* AST named sum datatypes are represented as `newtype` wrappers around sums constructed with `:+:`.
* AST named & anonymous sum types are represented as balanced binary trees of `:+:`s instead of right-chained lists.
* Rename the `bytes` field of leaves to `text`.

# v0.3.0.0

* Adds AST datatypes in `TreeSitter.Java.AST` which you can use with `TreeSitter.Unmarshal`.
