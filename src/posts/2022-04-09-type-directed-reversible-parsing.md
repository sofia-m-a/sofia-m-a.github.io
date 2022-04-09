---
author: "Sofia M.A."
title: "Concrete Parsing 2: Type-directed reversible parsing"
desc: 'A simple method to write reversible parsers'
image: "./images/butterfly.jpg"
keywords: "syntax"
lang: "en"
updated: "2022-04-09T08:17:36Z"
header-includes:
- |
    ```{=latex}
    \usepackage{newunicodechar}
    \newunicodechar{↔}{\ensuremath{\leftrightarrow}}
    ```
---

# Reversible parsing
Following on the previous post, we want to build concrete syntax trees based around a reversible parser-printer pair. Note the printer is not a pretty printer; for that we need an extra formatting step. Here's one approach I've been playing with.

## Syntax
Here's the syntax of a small language we will be working with:

```
program = def*

def = const_def | function_def
const_def = name ':' type '=' expression ';'
function_def = 'define' name '(' param_list? ')' statement

name = [a-zA-Z] [a-zA-Z0-9]*

type = 'int' | 'string' | name | type '[' expression ']'

expression = expression '+' term | expression '-' term | term
term = term '*' factor | term '/' factor | factor
factor = '-' factor | expr2
expr2 = expr2 '[' expression ']' | atom
atom = '(' expression ')' | name | [0-9]+ | '"' escape_or_printable '"'

escape_or_printable = [printable and not \] | '\n' | '\t'

param_list = name ':' type (',' name ':' type)*
statement
    = '{' statement* '}'
    | 'var' name ':' type '=' expression ';'
    | name '=' expression ';'
    | 'while' '(' expression ')' statement
    | 'if' '(' expression ')' statement ('else' statement)?

comment = '#' [not new-line]* new-line
```

## Tokens
First, a boring definition of tokens:

```haskell
data Token
    = TName Text
    | TNumber IntLiteral
    | TString [StringLiteralChunk]
    | TUnrecognized Char -- important for losslessness
    | TWhitespace [Whitespace] -- important for losslessness
    -- punctuation
    | TColon
    | TSemicolon
    | TComma
    | TEquals
    -- brackets
    | TLParen
    | TRParen
    | TLBrace
    | TRBrace
    | TLBracket
    | TRBracket
    -- operators
    | TPlus
    | TMinus
    | TAsterisk
    | TSlash
    -- keywords
    | TDefine
    | TVar
    | TInt
    | TString
    | TWhile
    | TIf
    | TElse

data IntLiteral = IntLiteral { value :: Natural, leadingZeroes :: Word -- important for losslessness }

data StringLiteralChunk
    = JustChar Char
    | SlashN
    | SlashT

data Whitespace
    = Space
    | NewLine
    | Comment Text
```

To go along with this, we want `tokenize :: String ->  [Token]`. Now, here's how we write our parser:

## Type-directed Parsing (and Printing)

```haskell
class BackToTokens a where
    backToTokens :: a -> [Token]

-- a parser has access to the remaining tokens, and can fail
type Parser a = StateT [Token] Maybe a

class Parsable a where
    tryParse :: Parser a
```

```haskell
newtype Program = Program [Def]
    deriving (BackToTokens, Parsable) via (Many Def)
```

Here, we meet our first type combinator, `Many`, which we make use of with `{-# LANGUAGE DerivingVia #-}`.

```haskell
newtype Many a = Many [a]

instance Many a => BackToTokens (Many a) where
    backToTokens = foldMap backToTokens
instance Parsable a => Parsable (Many a) where
    tryParse = many tryParse
```

`many` comes from `Control.Applicative`.

Our next type-level parser combinator will be for exactly a particular token. To make this ergonomic, we'll do some type-level magic with an auxiliary boilerplate class

```haskell
-- {-# LANGUAGE DataKinds #-}

class ToToken (t :: Token) where
    toToken :: Proxy t -> Token
instance ToToken 'TComma where
    toToken _ = TComma
-- a bunch more boilerplate instances omitted. We won't need instances for everything, just what we use
```

Then, we can define the `Exactly` combinator

```haskell
data Exactly (t :: Token) = Exactly

instance ToToken t => BackToTokens (Exactly t) where
  backToTokens Exactly = [toToken (Proxy @t)]

instance ToToken t => Parsable (Exactly t) where
  tryParse = do
      tokens <- get
      case tokens of
          (t:ts) | t == toToken (Proxy @t) -> do
              put ts
              pure Exactly
          _ -> empty
```

Next up, the 'Trivial' combinator

```haskell
data Trivial a = Trivial [Whitespace] a

instance BackToTokens a => BackToTokens (Trivial a) where
    backToTokens (Trivial trivia a) = TWhitespace trivia : backToTokens a
instance Parsable a => Parsable (Trivial a) where
    tryParse = do
        tokens <- get
        case tokens of
            TWhitespace w : rest -> do
                put rest
                Trivial ws a <- tryParse
                pure (Trivial (w:ws) a)
            _ -> Trivial [] <$> tryParse
```

Finally, we need a sequential combinator. We'll call it `:~>`. We could do

```haskell
data (:~>) a b = (:~>) a b
```

but this wouldn't allow us to gracefully fail when `b` isn't present. This is one of the strengths of our concrete syntax approach: returning a representation even when there's parse errors. So, we go with the following:

```haskell
data (:~>) a b = (:~>) a (Maybe b)
  deriving (Show)

infixr 1 :~>

instance (BackToTokens a, BackToTokens b) => BackToTokens (a :~> b) where
  backToTokens (a :~> b) = backToTokens a <> maybe [] backToTokens b

instance (Parsable a, Parsable b) => Parsable (a :~> b) where
  tryParse = (:~>) <$> tryParse <*> optional tryParse
```

Now we're ready for one of our first complex parsers:

```haskell
data Def
    = ConstDef (Name :~> Trivial (Exactly 'TColon) :~> Type' :~> Trivial (Exactly 'TEquals) :~> Expression :~> Trivial (Exactly 'TSemicolon))
    | FunctionDef (Trivial (Exactly 'TDefine) :~> Name :~> Trivial (Exactly 'TLParen) :~> OptionalParamList :~> Trivial (Exactly 'TRParen) :~> Statement)

instance BackToTokens Def where
    backToTokens (ConstDef c) = backToTokens c
    backToTokens (FunctionDef f) = backToTokens f

instance Parsable Def where
    tryParse = do
        token <- peek
        case token of
            Just (TName _) -> ConstDef <$> tryParse
            Just (TDefine) -> FunctionDef <$> tryParse
            _ -> empty
```

That's quite a bit. We relied on the existence of one little helper:

```haskell
peek :: Parser (Maybe Token)
peek = do
    tokens <- get
    case dropWhitespace tokens of
        [] -> pure Nothing
        t : ts -> pure (Just t)
    where
    dropWhitespace (TWhitespace _ : ts) = dropWhitespace ts
    dropWhitespace ts = ts
```

We may as well introduce the abbreviation

```haskell
type Only (t :: Token) = Trivial (Exactly t)
```

too. Now, look at our definition:

```haskell
data Def
    = ConstDef (Name :~> Only 'TColon :~> Type' :~> Only 'TEquals :~> Expression :~> Only 'TSemicolon)
    | FunctionDef (Only 'TDefine :~> Name :~> Only 'TLParen :~> OptionalParamList :~> Only 'TRParen :~> Statement)
```

Writing parsers goes like this:

* When we have a sequence, join the elements with `(:~>)` and use the `(:~>)` instance of `BackToTokens` and `Parsable`
* When we have a choice, peek a token and use that to dispatch to a sequential parser.

Let's write a few more:

```haskell
newtype Name = Name [Whitespace] Text
newtype IntLit = IntLit [WhiteSpace] IntLiteral
newtype StringLit = StringLit [Whitespace] [StringLiteralChunk]
-- instances are just like Trivial and Exactly, above

data Statement
    = SBlock (Only 'TLBrace :~> Many Statement :~> Only TRBrace)
    | SDec (Only 'TVar :~> Name :~> Only 'TColon :~> Type' :~> Only 'TEquals :~> Expression :~> Only 'TSemicolon)
    | SAssign (Name :~> Only 'TEquals :~> Expression :~> Only 'TSemicolon)
    | SWhile (Only 'TWhile :~> Only 'TLParen :~> Expression :~> Only 'TRParen :~> Statement)
    | SIf (Only 'TIf :~> Only 'TLParen :~> Expression :~> Only 'TRParen :~> Statement :~> Only 'TElse :~> Statement)

instance BackToTokens Statement where
    backToTokens (SBlock x) = backToTokens x
    backToTokens (SDec x) = backToTokens x
    backToTokens (SAssign x) = backToTokens x
    backToTokens (SWhile x) = backToTokens x
    backToTokens (SIf x) = backToTokens x

instance Parsable Statement where
    tryParse = do
        token <- peek
        case token of
            Just TLBrace -> SBlock <$> tryParse
            Just TVar -> SDec <$> tryParse
            Just (TName _) -> SAssign <$> tryParse
            Just TWhile -> SWhile <$> tryParse
            Just TIf -> SIf <$> tryParse
```

We can easily transcribe a grammar non-terminal into a Haskell type using these rules. Well, there's a spanner in the mix: consider left-recursion, like in `type`. Here's how we have to handle it:

```haskell
data Type'
    = TyInt (Only 'TInt)
    | TyString (Only 'TString)
    | TyName Name
    | TyArray (Type' :~> Only 'TLBracket :~> Expression :~> Only 'TRBracket)

-- This is getting very familiar
instance BackToTokens Type' where
    backToTokens (TyInt x) = backToTokens x
    backToTokens (TyString x) = backToTokens x
    backToTokens (TyName x) = backToTokens x
    backToTokens (TyArray x) = backToTokens x

instance Parsable Type' where
    tryParse = do
        token <- peek
        base <- case token of
            Just TInt -> TyInt <$> tryParse
            Just TString -> TyString <$> tryParse
            Just (TName _) -> TyName <$> tryParse
            _ -> empty
        parseArrayQuals base
        where
            parseArrayQuals base = do
                token <- peek
                case peek of
                    TLBracket -> do
                        inBracket <- tryParse @(Only 'TLBracket :~> Expression :~> Only 'TRBracket)
                        parseArrayQuals (TyArray (base :~> Just inBracket))
                    _ -> pure base
```

We could abstract this into another combinator, but there's an easier way:

```haskell
data Type' = Type' BaseType (Many (Only 'TLBracket :~> Expression :~> Only 'TRBracket))
data BaseType
    = TyInt (Only 'TInt)
    | TyString (Only 'TString)
    | TyName Name
```

Just use lists!

With this trick in mind, we can fill out the rest of the grammar

```haskell
data Expression = Expression Term [AddOp]
data AddOp = Plus (Only 'TPlus :~> Term) | Minus (Only 'TMinus :~> Term)
data Term = Term Factor [MulOp]
data MulOp = Mul (Only 'TAsterisk :~> Factor) | Div (Only 'TSlash :~> Term)
data Factor = Negate (Only 'TMinus :~> Factor) | FExpr Expr2
data Expr2 = Expr (Atom :~> Many (Only 'TLBracket :~> Expression :~> Only 'TRBracket)))
data Atom
    = AParens (Only 'TLParen :~> Expression :~> Only 'TRParen)
    | AName Name
    | ANumber IntLit
    | AString [StringLiteralChunk]

data OptionalParamList
    = NoParams
    | ParamList (Param :~> Many (Only 'TComma :~> Param))
data Param = Param (Name :~> Only 'TComma :~> Type')
```

## Error recovery
Currently, our parsers handle errors of the kind 'Expected _ here' just fine, but not 'Unexpected _ here'. There is a bit of an art as to where to put 'recovery' nodes. A first attempt would be:

```haskell
data Def
    = ...
    | UnexpectedDef Token

data Statement
    = ...
    | UnexpectedStatement Token
```

The first change is necessary, as all other 'Unexpected' tokens will eventually fall to the top-level parser (for `Def`s) to make sense of. However, allowing unexpected tokens between statements means that we don't break out of parsing a statement just because of something unexpected, which *should* make error-recovery better in a lot of cases. Of course, it's easy to find situations where this doesn't help; I don't know of any precise rules for how to make error-tolerant grammars, beyond intuition and testing.

## Limitations and outlook
The techniques presented here resemble classical LL(1) parsers , with all their limitations. We can add lookahead (imagine a `peek2 :: Parser (Maybe Token, Maybe Token)`) to emulate LL(k) grammars for higher k, but this still closes off certain classes of grammars to us. However, LL(1) is a good class for parsing most programming-language grammars. It's similar to the grammar classes that recursive descent parsers and parser combinators (without clever tricks) can handle, and these are popular parsing frameworks

The specifications are somewhat verbose, although they follow straightforwardly from the grammar. Once we have a few type-level 'parser combinators' and some term-level parser combinators, the verbosity is similar to classical parser combinator libraries.

It's possible to combine `BackToText` and `Parsable` into one class and try to use one set of combinators for both, but I'm not sure it's necessarily worth the extra complexity