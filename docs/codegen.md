# CodeGen Documentation

ASTs are generated via the following steps:

1. [**Deserialize.**](https://github.com/tree-sitter/haskell-tree-sitter/blob/master/tree-sitter/src/TreeSitter/Deserialize.hs) First, we deserialize the json file into the desired shape of datatypes, using the Aeson library to parse the JSON.
2. [**Generate Syntax.**](https://github.com/tree-sitter/haskell-tree-sitter/blob/master/tree-sitter/src/TreeSitter/GenerateSyntax.hs) Once we've deserialized the JSON file representing the grammar for a given language, we use Template Haskell to auto-generate datatypes at compile time for each language. This gives us a set of TH-generated, language-specific, strongly-typed datatypes.
3. [**Unmarshal.**](https://github.com/tree-sitter/haskell-tree-sitter/blob/master/tree-sitter/src/TreeSitter/Unmarshal.hs) Iterate over tree-sitter’s parse trees using its tree cursor API and produce Haskell ASTs for the relevant nodes. These ASTs are built generically by unmarshaling the data we get from parsing source code via tree-sitter.

### How to generate ASTs
_TODO: improve the following_

```
cabal new-repl lib:tree-sitter-python

:seti -XOverloadedStrings
:seti -XTypeApplications

import TreeSitter.Span
import TreeSitter.Range
import TreeSitter.Unmarshal


parseByteString @(TreeSitter.Python.AST.Module (TreeSitter.Range.Range, Span)) tree_sitter_python "(a + b)"
```

### How to inspect compile-time generated datatypes
_TODO: elaborate on the following + add examples_

```
:i TreeSitter.Python.AST.Module
```

### Tests
_TODO: fill in testing + improve tests while I'm at it_

## Understanding the API

#### Deserialize

There are four distinct types the node-types.json file takes on: sums, products, named leaves and anonymous leaves. We deserialize these into their respective shapes before using Template Haskell to generate specific datatypes of each shape.

| Datatype | JSON example | TH example |
|----------|--------------|------------|
|sums|||
|products|||
|named leaves|||
|anonymous leaves|||

_TODO:_
- fill in examples, elaborate on this

#### Generate Syntax
_TODO:_
- describe the structure of `syntaxDatatype`, `Grammar` type and call to `astDeclarationsForLanguage`

#### Unmarshal
_TODO:_
- Parse source code and produce AST
- Describe how `parseByteString` works, symbolMatching instances etc.
- Why we have generic vs. non-generic classes: TL;DR because generic behaviors are different than what we get non-generically, and in the case of ` Maybe`, `[]`—we actually preference doing things non-generically. Why? Well, since `[]` is a sum, the generic behavior for `:+:` would be invoked and expect that we’d have repetitions represented in the parse tree as right-nested singly-linked lists (ex., `(a (b (c (d…))))`) rather than as just consecutive sibling nodes (ex., `(a b c ...d)`, which is what our trees have). We want to match the latter.

## CodeGen and Semantic
_TODO:_
- provide brief description of how this fits into our work in Semantic
- provide some context on what changed and motivation for said changes (maybe link to [then-published blog](https://github.com/github/semantic-code/pull/67/files))
- make a better diagram
![image](https://user-images.githubusercontent.com/875834/64924659-fc63e100-d7b4-11e9-9802-dfdf15349b27.png)
