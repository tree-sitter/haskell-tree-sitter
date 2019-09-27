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
