### v0.9.0.2

* Remove `fused-effects` dependency.
* Support ghc 8.10.


### v0.9.0.1

* Remove `semantic-source` dependency.

### v0.9.0.0

* Remove CodeGen files `Deserialize`, `GenerateSyntax`, `Unmarshal`, `Token`

### v0.8.0.2

* Updates `tree-sitter` to fix an issue with improperly balanced subtrees leading to slowdowns when copying nodes.
* Neither `ts_tree_cursor_copy_child_nodes` nor `ts_node_copy_child_nodes` is interruptible.


### v0.8.0.1

* `ts_tree_cursor_copy_child_nodes` is interruptible.


### v0.5.0.0

* Use a shared `Token` type for anonymous leaves in generated AST types.
* Allow generated ASTs to override the representation for portions of the AST by defining specialized datatypes. Note that this should be used sparingly to keep the maintenance burden of the AST types low.
* Generate named sum types as `newtype` wrappers around sums constructed with `:+:`.
* Generate named & anonymous sum types as balanced binary trees of `:+:`s instead of right-chained lists.
* Rename the `bytes` field of leaves to `text`.

### v0.4.0.0

* `Unmarshal` has been split into `Unmarshal`, `UnmarshalAnn`, and `UnmarshalField`, with the first newly taking type constructors of kind `* -> *`. `UnmarshalAnn` can be used to unmarshal annotation types relating to the entire node, and `UnmarshalField` can be used to unmarshal fields of zero or more nodes.

* `UnmarshalAnn` instances are provided for the `semantic-source` types `Loc`, `Range`, and `Span`.

* AST datatypes are generated with `GHC.Generics.:+:` for anonymous sums in field positions instead of `Either`s. This makes it possible to define typeclasses over them at kind `* -> *` instead of only at kind `*`.

* AST datatypes receive derived instances of `Foldable`, `Functor`, `Generic`, `Generic1`, and `Traversable`.

### v0.3.0.0

* `Node` has a `nodeIsExtra` field stating whether it was produced via the `extras` rule.

### v0.2.1.0

* Add `TreeSitter.Range` and `TreeSitter.Span`.

### v0.2.0.0

* Add unmarshalling support with `TreeSitter.Unmarshal`.
* Removes pointer-only constructors for bridged C types.

### v0.1.0.0

* Initial release.
