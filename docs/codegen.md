# CodeGen Documentation

CodeGen is the process for generating language-specific, strongly-typed ASTs for used by [Semantic](https://github.com/github/semantic-code/blob/d9f91a05dc30a61b9ff8c536d75661d417f3c506/design-docs/precise-code-navigation.md). The process depends on two prerequisites for any given language: 1) a [tree-sitter](http://tree-sitter.github.io/tree-sitter/) parser for that language must exist, and 2) a Cabal package for that supported language must also exist in order to provide an interface into tree-sitter's C source. [This](https://github.com/tree-sitter/haskell-tree-sitter/tree/master/tree-sitter-python) is an example of a library for the supported language Python that the remaining documentation will refer to.

During parser generation, tree-sitter produces a JSON file that captures the structure of a particular grammar. This means that for a given language with an existing tree-sitter grammar, we're able to derive datatypes representing surface languages based on that JSON file, and then use those datatypes to create ASTs. The following steps provide a high-level outline of the process:

1. [**Deserialize.**](https://github.com/tree-sitter/haskell-tree-sitter/blob/master/tree-sitter/src/TreeSitter/Deserialize.hs) First, we deserialize the `node-types.json` file for a given language into the desired shape of datatypes via parsing capabilities afforded by the [Aeson](http://hackage.haskell.org/package/aeson) library. There are four distinct types represented in the node-types.json file takes on: sums, products, named leaves and anonymous leaves.
2. [**Generate Syntax.**](https://github.com/tree-sitter/haskell-tree-sitter/blob/master/tree-sitter/src/TreeSitter/GenerateSyntax.hs) We then use Template Haskell to auto-generate language-specific, strongly-typed datatypes that represent various language constructs at compile-time. This API exports the top-level function `astDeclarationsForLanguage` to auto-generate datatypes at compile-time, which is is invoked by a given language [AST](https://github.com/tree-sitter/haskell-tree-sitter/blob/master/tree-sitter-python/TreeSitter/Python/AST.hs) module.
3. [**Unmarshal.**](https://github.com/tree-sitter/haskell-tree-sitter/blob/master/tree-sitter/src/TreeSitter/Unmarshal.hs) Unmarshaling is the process of iterating over tree-sitter’s parse trees using its tree cursor API, and producing Haskell ASTs for the relevant nodes. We parse source code from tree-sitter and unmarshal the data we get to build these ASTs generically. This file exports the top-level function `parseByteString`, which takes source code, a language and produces an AST.

For example, a node-types.json file with a

The following document provides more details on the generated ASTs, APIs and tests.

### Table of Contents
- [Generating ASTs](#Generating-ASTs)
- [Inspecting auto-generated datatypes](#Inspecting-auto-generated-datatypes)
- [Tests](#tests)
- [Additional notes](#Additional-notes)
___

### Generating ASTs

To parse source code and produce ASTs locally:

1. Load the REPL for a given language:

```
cabal new-repl lib:tree-sitter-python
```

2. Set language extensions, `OverloadedStrings` and `TypeApplications`, and import relevant modules, `TreeSitter.Unmarshal`, `TreeSitter.Range` and `TreeSitter.Span`:

```
:seti -XOverloadedStrings
:seti -XTypeApplications

import TreeSitter.Span
import TreeSitter.Range
import TreeSitter.Unmarshal
```

3. You can now call `parseByteString`, passing in the desired language you wish to parse (in this case Python exemplified by `tree_sitter_python`), and the source code (in this case binary addition of two identifiers). Since the function is constrained by `(Unmarshal t, UnmarshalAnn a)`, you can use type applications to provide a top-level node `t`, an entry point into the tree, in addition to a polymorphic annotation `a` used to represent range and span:

```
parseByteString @(TreeSitter.Python.AST.Module (TreeSitter.Range.Range, Span)) tree_sitter_python "(a + b)"
```

This generates the following AST:

```
Right (Module {ann = Range {start = 0, end = 3}, extraChildren = [R1 (SimpleStatement (L1 (R1 (R1 (L1 (ExpressionStatement {ann = Range {start = 0, end = 3}, extraChildren = L1 (L1 (Expression (L1 (L1 (L1 (PrimaryExpression (L1 (L1 (L1 (R1 (BinaryOperator {ann = Range {start = 0, end = 3}, operator = L1 (R1 (R1 (L1 (Token {ann = Range {start = 1, end = 2}})))), left = PrimaryExpression (L1 (R1 (R1 (R1 (R1 (Identifier {ann = Range {start = 0, end = 1}, bytes = "a"})))))), right = PrimaryExpression (L1 (R1 (R1 (R1 (R1 (Identifier {ann = Range {start = 2, end = 3}, bytes = "b"}))))))}))))))))))) :| []}))))))]})
```

### Inspecting auto-generated datatypes

Datatypes are derived from a language and its `node-types.json` file using the GenerateSyntax API. Definition can be viewed in the REPL just as they would for any other datatype, using `:i`:

```
:i TreeSitter.Python.AST.Module
```

This gives us the auto-generated `Module` datatype:

```
data TreeSitter.Python.AST.Module a
  = TreeSitter.Python.AST.Module {TreeSitter.Python.AST.ann :: a,
                                  TreeSitter.Python.AST.extraChildren :: [(GHC.Generics.:+:)
                                                                            TreeSitter.Python.AST.CompoundStatement
                                                                            TreeSitter.Python.AST.SimpleStatement
                                                                            a]}
  	-- Defined at TreeSitter/Python/AST.hs:10:1
instance Show a => Show (TreeSitter.Python.AST.Module a)
  -- Defined at TreeSitter/Python/AST.hs:10:1
instance Ord a => Ord (TreeSitter.Python.AST.Module a)
  -- Defined at TreeSitter/Python/AST.hs:10:1
instance Eq a => Eq (TreeSitter.Python.AST.Module a)
  -- Defined at TreeSitter/Python/AST.hs:10:1
instance Traversable TreeSitter.Python.AST.Module
  -- Defined at TreeSitter/Python/AST.hs:10:1
instance Functor TreeSitter.Python.AST.Module
  -- Defined at TreeSitter/Python/AST.hs:10:1
instance Foldable TreeSitter.Python.AST.Module
  -- Defined at TreeSitter/Python/AST.hs:10:1
instance Unmarshal TreeSitter.Python.AST.Module
  -- Defined at TreeSitter/Python/AST.hs:10:1
instance SymbolMatching TreeSitter.Python.AST.Module
  -- Defined at TreeSitter/Python/AST.hs:10:1
```

### Tests

As of right now, Hedgehog tests are minimal and only in place for the Python library.

To run tests:

`cabal new-test tree-sitter-python`

### Additional notes

- Anonymous leaf types are defined as synonyms for the `Token datatype`
- [GenerateSyntax](https://github.com/tree-sitter/haskell-tree-sitter/blob/master/tree-sitter/src/TreeSitter/GenerateSyntax.hs) provides a way to pre-define certain datatypes for which Template Haskell is not used. Any datatypes among the node types which have already been defined in the module where the splice is run will be skipped, allowing customization of the representation of parts of the tree. Note that this should be used sparingly, as it imposes extra maintenance burden, particularly when the grammar is changed. This may be used to e.g. parse literals into Haskell equivalents (e.g. parsing the textual contents of integer literals into `Integer`s), and may require defining `TS.UnmarshalAnn` or `TS.SymbolMatching` instances for (parts of) the custom datatypes, depending on where and how the datatype occurs in the generated tree, in addition to the usual `Foldable`, `Functor`, etc. instances provided for generated datatypes.
- Range and span info is captured by the parameter `a`
- In [Unmarshal]((https://github.com/tree-sitter/haskell-tree-sitter/blob/master/tree-sitter/src/TreeSitter/Unmarshal.hs)) We have generic and non-generic classes. This is because generic behaviors are different than what we get non-generically, and in the case of ` Maybe`, `[]`—we actually preference doing things non-generically. Since `[]` is a sum, the generic behavior for `:+:` would be invoked and expect that we’d have repetitions represented in the parse tree as right-nested singly-linked lists (ex., `(a (b (c (d…))))`) rather than as just consecutive sibling nodes (ex., `(a b c ...d)`, which is what our trees have). We want to match the latter.
