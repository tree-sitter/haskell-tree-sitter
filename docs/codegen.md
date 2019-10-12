# CodeGen Documentation

CodeGen is the process for generating strongly-typed ASTs used by [Semantic](https://github.com/github/semantic-code/blob/d9f91a05dc30a61b9ff8c536d75661d417f3c506/design-docs/precise-code-navigation.md). The process depends on two prerequisites for any given language: 1) a [tree-sitter](http://tree-sitter.github.io/tree-sitter/) parser for that language must exist, and; 2) a Cabal package for that supported language must also exist in order to provide an interface into tree-sitter's C source. [This](https://github.com/tree-sitter/haskell-tree-sitter/tree/master/tree-sitter-python) is an example of a library for the supported language Python that the remaining documentation will refer to.

During parser generation, tree-sitter produces a JSON file that captures the structure of a particular grammar. This means that for a given language with an existing tree-sitter grammar, we're able to derive datatypes representing surface languages based on that JSON file, and then use those datatypes to create ASTs. The following steps provide a high-level outline of the process:

1. [**Deserialize.**](https://github.com/tree-sitter/haskell-tree-sitter/blob/master/tree-sitter/src/TreeSitter/Deserialize.hs) First, we deserialize the `node-types.json` file for a given language into the desired shape of datatypes via parsing capabilities afforded by the [Aeson](http://hackage.haskell.org/package/aeson) library.
2. [**Generate Syntax.**](https://github.com/tree-sitter/haskell-tree-sitter/blob/master/tree-sitter/src/TreeSitter/GenerateSyntax.hs) We then use Template Haskell to auto-generate language-specific, strongly-typed datatypes that represent various language constructs at compile-time.
3. [**Unmarshal.**](https://github.com/tree-sitter/haskell-tree-sitter/blob/master/tree-sitter/src/TreeSitter/Unmarshal.hs) Unmarshaling is the process of iterating over tree-sitter’s parse trees using its tree cursor API, and producing Haskell ASTs for the relevant nodes. We parse source code from tree-sitter and unmarshal the data we get to build these ASTs generically.

The following document provides more details on the generated ASTs, APIs and tests.

### Table of Contents
- [Generating ASTs](#Generating-ASTs)
- [Inspecting auto-generated datatypes](#Inspecting-auto-generated-datatypes)
- [Relevant modules](#Relevant-modules)
  - [Deserialize](#Deserialize)
  - [GenerateSyntax](#GenerateSyntax)
  - [Unmarshal](#unmarshal)
  - [TreeSitter.Language.AST](#treesitter-language-ast)
- [Tests](#tests)

___

## Generating ASTs

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

## Inspecting auto-generated datatypes

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

## Tests

As of right now, Hedgehog tests are minimal and only in place for the Python library.

To run tests:

`cabal new-test tree-sitter-python`

## Relevant modules

### Deserialize

_TODO: add bit about node field names_ https://tree-sitter.github.io/tree-sitter/using-parsers#node-field-names


There are four distinct types represented node-types.json file takes on: sums, products, named leaves and anonymous leaves. We deserialize these into their respective shapes before using Template Haskell to generate specific datatypes of each shape.

| Datatype | JSON example | TH example |
|----------|--------------|------------|
|sum|<code>{<br>"type": "_compound_statement",<br>"named": true,<br>"subtypes": [<br>{"type": "class_definition",<br>"named": true<br>},<br>{<br>"type": "decorated_definition",<br>"named": true<br>},<br>{<br>"type": "for_statement",<br>"named": true<br>},<br>{<br>"type": "function_definition",<br>"named": true<br>},<br>{<br>"type": "if_statement",<br>"named": true<br>},<br>{<br>"type": "try_statement",<br>"named": true<br>},<br>{<br>"type": "while_statement",<br>"named": true<br>},<br>{"type": "with_statement","named": true<br>}<br>]<br>},||
|product|||
|named leaves|||
|anonymous leaves|||

sum

```Haskell
data TreeSitter.Python.AST.CompoundStatement a
  = TreeSitter.Python.AST.ClassDefinitionCompoundStatement (TreeSitter.Python.AST.ClassDefinition
                                                              a)
  | TreeSitter.Python.AST.DecoratedDefinitionCompoundStatement (TreeSitter.Python.AST.DecoratedDefinition
                                                                  a)
  | TreeSitter.Python.AST.ForStatementCompoundStatement (TreeSitter.Python.AST.ForStatement
                                                           a)
  | TreeSitter.Python.AST.FunctionDefinitionCompoundStatement (TreeSitter.Python.AST.FunctionDefinition
                                                                 a)
  | TreeSitter.Python.AST.IfStatementCompoundStatement (TreeSitter.Python.AST.IfStatement
                                                          a)
  | TreeSitter.Python.AST.TryStatementCompoundStatement (TreeSitter.Python.AST.TryStatement
                                                           a)
  | TreeSitter.Python.AST.WhileStatementCompoundStatement (TreeSitter.Python.AST.WhileStatement
                                                             a)
  | TreeSitter.Python.AST.WithStatementCompoundStatement (TreeSitter.Python.AST.WithStatement
                                                            a)
  	-- Defined at TreeSitter/Python/AST.hs:10:1
instance Show a => Show (TreeSitter.Python.AST.CompoundStatement a)
  -- Defined at TreeSitter/Python/AST.hs:10:1
instance Ord a => Ord (TreeSitter.Python.AST.CompoundStatement a)
  -- Defined at TreeSitter/Python/AST.hs:10:1
instance Eq a => Eq (TreeSitter.Python.AST.CompoundStatement a)
  -- Defined at TreeSitter/Python/AST.hs:10:1
instance Traversable TreeSitter.Python.AST.CompoundStatement
  -- Defined at TreeSitter/Python/AST.hs:10:1
instance Functor TreeSitter.Python.AST.CompoundStatement
  -- Defined at TreeSitter/Python/AST.hs:10:1
instance Foldable TreeSitter.Python.AST.CompoundStatement
  -- Defined at TreeSitter/Python/AST.hs:10:1
instance Unmarshal TreeSitter.Python.AST.CompoundStatement
  -- Defined at TreeSitter/Python/AST.hs:10:1
instance SymbolMatching TreeSitter.Python.AST.CompoundStatement
  -- Defined at TreeSitter/Python/AST.hs:10:1```

product

```JSON
{
  "type": "await",
  "named": true,
  "fields": {},
  "children": {
    "multiple": false,
    "required": true,
    "types": [
      {
        "type": "_expression",
        "named": true
      }
    ]
  }
},
```

named leaf

```JSON
{
  "type": "identifier",
  "named": true
}
```


anonymous leaf
```JSON
{
  "type": "lambda",
  "named": false
},
```

### Generate Syntax

GenerateSyntax is our Template Haskell API for generating the datatypes to represent AST nodes. This file:

1. Defines all of the necessary logic required to process node types that correspond to sums, products, leaves and anonymous leaves captured by `Deserialize`.
2. Exports the top-level function `astDeclarationsForLanguage`, which is is invoked by a given language [AST](https://github.com/tree-sitter/haskell-tree-sitter/blob/master/tree-sitter-python/TreeSitter/Python/AST.hs) module.

#### Things to note:
- Anonymous leaf types are defined as synonyms for the `Token datatype`
- Any datatypes among the node types which have already been defined in the module where the splice is run will be skipped, allowing customization of the representation of parts of the tree. Note that this should be used sparingly, as it imposes extra maintenance burden, particularly when the grammar is changed. This may be used to e.g. parse literals into Haskell equivalents (e.g. parsing the textual contents of integer literals into `Integer`s), and may require defining `TS.UnmarshalAnn` or `TS.SymbolMatching` instances for (parts of) the custom datatypes, depending on where and how the datatype occurs in the generated tree, in addition to the usual `Foldable`, `Functor`, etc. instances provided for generated datatypes.
- Range and span info is captured by the parameter `a`

### Unmarshal

_TODO:_
- Parse source code and produce AST
- Describe how `parseByteString` works, symbolMatching instances etc.

Things to note:
- We have generic and non-generic classes. This is because generic behaviors are different than what we get non-generically, and in the case of ` Maybe`, `[]`—we actually preference doing things non-generically. Since `[]` is a sum, the generic behavior for `:+:` would be invoked and expect that we’d have repetitions represented in the parse tree as right-nested singly-linked lists (ex., `(a (b (c (d…))))`) rather than as just consecutive sibling nodes (ex., `(a b c ...d)`, which is what our trees have). We want to match the latter.

## CodeGen and Semantic
_TODO:_
- provide brief description of how this fits into our work in Semantic
- provide some context on what changed and motivation for said changes (maybe link to [then-published blog](https://github.com/github/semantic-code/pull/67/files))
- make a better diagram
![image](https://user-images.githubusercontent.com/875834/64924659-fc63e100-d7b4-11e9-9802-dfdf15349b27.png)
