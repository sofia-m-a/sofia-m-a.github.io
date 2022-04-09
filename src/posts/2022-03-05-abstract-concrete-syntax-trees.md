---
author: "Sofia M.A."
title: "Concrete parsing 1: Abstract concrete syntax trees"
desc: 'A midpoint between abstract and concrete syntax trees'
image: "./images/butterfly.jpg"
keywords: "syntax"
lang: "en"
updated: "2022-03-05T04:08:37Z"
header-includes:
- |
    ```{=latex}
    \usepackage{newunicodechar}
    \newunicodechar{↔}{\ensuremath{\leftrightarrow}}
    ```
---

# Abstract and concrete syntax trees
Commonly, compilers and other language tools use a tree representation of parsed source code. The goal of an *abstract syntax tree* is to represent the structure of the source code in a form that's easy to manipulate. By contrast, a *concrete syntax tree* represents the source code in a way that preserves all information present in the source.

Concrete syntax trees have several advantages:

* we can output the source code underlying the tree exactly as it was input, not some abstracted form
* comments and whitespace are preserved, which can be useful for documentation tools, source formatters, and more
* it's easier to represent erroneous programs, which is useful for error reporting and compiler support for interactive development

# Concrete syntax trees in Haskell
I'm not aware of a concrete syntax tree library for Haskell, but we can get an idea from the Rust libraries [CSTree](https://crates.io/crates/cstree) and [Rowan](https://crates.io/crates/rowan). We can build something similar like this:

```haskell
data CST
    = Node NodeKind [CST]
    | Leaf LeafKind Text
```

Here, a tree consists of `Node`s with a `NodeKind` and a list of children, and the leaves consist of a token of kind `LeafKind` and a span of `Text` recording the actual text of the token.

# Abstract concrete syntax trees
However, this representation isn't so easy to manipulate, and doesn't maintain any of the parsed structure of the tree. For instance, our parser might ensure that an `Add` node has two `Expression` children, but this isn't reflected in the final concrete syntax tree. On ther other hand, an abstract styntax tree would have a `Add Expression Expression` constructor that exactly captures this invariant. 

Of course, this is in tension with one of the goals of a concrete syntax tree: to represent erroneous programs.

However, it's possible to find a middle ground between the two approaches, that preserves the benefits of a concrete syntax tree, but keeps some of the structure of an abstract syntax tree. Here's an example concrete syntax tree type for expressions in a Scheme-like language:

```haskell
data CSTExpr
    = Junk Text
    | Comment Text (Maybe CSTExpr)
    | Whitespace Whitespace (Maybe CSTExpr)

    | Atom Text

    -- literals
    | Bool Bool
    | Number Integer
    | Nil
    | String [Either Text Escape] Bool

    | Quote CSTExpr
    | List [CSTExpr] Whitespace Bool

-- Guaranteed to be a sequence of space characters
newtype Whitespace = Whitespace Text

data Escape
    = SlashN -- \n
    | SlashT -- \t
    | Unicode Int -- '\uXXXX'
    | ...
```

Let's go through this

* First, we have `Junk`, to represent any sequence of characters that cannot possibly begin a valid expression.
* `Comment` keeps the original comment text, together with a possibly following CSTExpr node
* Whitespace keeps a span of whitespace characters and a possibly following Node
* `Atom`, `Bool`, `Number` and `Nil` are straightforward
* For strings, we need to not just store `Text`, but remember which parts of the text correspond to escapes. For instance, "\u0041" and "a" have the same `Text` representation, but one uses an escape code. We also store whether or not the closing quote appears before the end of the file
* `Quote` is straightforward.
* For `List`, we store any trailing whitespace and whether or not the closing ')' appears before the end of the file.

Note that "(a b c)" will be represented by

```haskell
List [
    Atom "a",
    Whitespace (Whitespace " ") (Just $ Atom "b"),
    Whitespace (Whitespace " ") (Just $ Atom "c")
]
(Whitespace mempty) True
```

This is the reason to attach whitespace nodes to their following nodes, so that structure of the list is preserved as much as possible.

# Parsing and printing
To show our ability to roundtrip through this concrete representation, here's a rough sketch of a grammar-like specification

    ";" text "\n" maybe_expr ↔ Comment text maybe_expr
    white=(" " | "\n")* maybe_expr ↔ Whitespace white maybe_expr
    symbolic (symbolic | digit)* ↔ Atom ...
    "#t" ↔ Bool True
    "#f" ↔ Bool False
    (-)? [0-9]+ ↔ Number ...
    "nil" ↔ Nil
    "\"" string_char* "\"" ↔ String ... is-eof
    "'" expr ↔ Quote expr
    "(" expr* whitespace? ")" ↔ List expr whitespace is-eof
    anything_else ↔ Junk anything_else

    [^\]+ ↔ Left ...
    "\n" ↔ Right SlashN
    "\t" ↔ Right SlashT
    "\u" hex hex hex hex ↔ Right (Unicode ...)


# Drawbacks
It can be non-obvious how to elaborate an AST into a CST like the above. In an upcoming post, I hope to show how to build invertible parsers/printer pairs that make it more obvious.