# haskell-tree-sitter

_NOTE: this project used to define Template Haskell splices for auto-generating language-specific, strongly-typed Haskell ASTs from tree-sitter grammars. This code has moved to [`Semantic`](https://github.com/github/semantic/tree/master/semantic-ast)._

### Haskell bindings for tree-sitter

This is a set of Haskell bindings to the [tree-sitter][tree-sitter]
parsing library. tree-sitter is a modern incremental parsing toolkit
with a great many useful features, including:

* Incremental, error-correcting parses: one syntax error in a file
  will not prevent the rest of the file from being parsed.
* Speed: tree-sitter is capable of parsing large files on every
  keystroke of a text editor.
* A GLR algorithm capable of parsing nondeterministic and ambiguous
  grammars.

This package provides interfaces to the official tree-sitter grammars
for Haskell, Java, Go, JSON, TypeScript/JavaScript, PHP, Python, and
Ruby.

The interface is somewhat low-level: if you use this package, you'll
probably want to add a step that munges a given `TreeSitter.Node` into
a more Haskell-amenable data structure.

There are some example executables provided with this project:

* An example of using this library to parse, read, and print out AST nodes
can be found [here](https://github.com/tree-sitter/haskell-tree-sitter/blob/master/languages/haskell/examples/Demo.hs)

To build these executables, pass the `build-examples` flag to your build tool.

[tree-sitter]: https://github.com/tree-sitter/tree-sitter