# tree-sitter

**Please note that this library is currently in flux. The maintainers are revising the core API to allow for generation of Haskell types from a tree-sitter grammar, and the automatic bridging of tree-sitter syntax nodes to these types. If you depend on this, we recommend depending on the commit tagged `legacy`.**

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
can be found [here](https://github.com/tree-sitter/tree-sitter/blob/master/languages/haskell/examples/Demo.hs)
* An example of using this library's experimental API to parse, navigate, read, and print out AST nodes
can be found [here](https://github.com/tree-sitter/tree-sitter/blob/master/languages/haskell/examples/DemoPtr.hs)

To build these executables, pass the `build-examples` flag to your build tool.

[tree-sitter]: https://github.com/tree-sitter/tree-sitter

# Building

1. Run `script/bootstrap` to pull in all submodules.
2. Adjust `stack.yaml`'s `resolver` field to target whatever LTS you want.
3. `stack build` (or `stack build --flag tree-sitter-haskell:build-examples`)
