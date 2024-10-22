# Haskell Parser Combinator

A very bare bones monadic parser combinator library in Haskell.

1. [The `Parser` Type](##The`Parser`Type)
    1. [Parser Parameters](###ParserParameters)
    1. [Modifying Parsers](###ModifyingParsers)
    2. [Choosing Parsers](###ChoosingParsers)
    3. [Repeating Parsers](###RepeatingParsers)
    4. [Combining Parsers](###CombiningParsers)
    5. [Ignoring Results](###IgnoringResults)
2. [In Case of Errors](##InCaseOfErrors)
3. [Evaluation](##Evaluation) 
4. [Example](##Example)

## The `Parser` Type

The `Parser` type is a function that takes a list of *something* as input, and returns a `ParserResult` type. A
`ParserResult` is either:

- A successful parse: The parsed value, as well as the rest of the input string is returned.
- A failed parse: An error message is returned.

Parsers are ran using the `parse [parser] [input]` function.

```
ghci> parse expr "5 + 5"
Success (10.0, "")
ghci> parse expr "4 + "
Success (4.0, "+ ")
ghci> parse expr "/4"
Failure "Expected numeric char, got /"
```

### Parser Parameters

The full type of a `Parser` is `Parser a b = Parser ([a] -> ParserResult b a)`. The parser
takes a list of `a` as input, and returns a result that is either a `Failure`, containing an error
message, or a `Success`, which contains the parsed result of type `b`, and the remaining elements of
the input list.

### Modifying Parsers

If successful, `Parsers` can have functions applied to them using `fmap`:

```
ghci> add5 = (+5) <$> number
ghci> parse add5 "27"
Success (32.0, "")
```

### Choosing Parsers

Parsers can parse numerous different values using the choice operator `<|>`:
```
ghci> aOrB = char 'a' <|> char 'b'
ghci> parse aOrB "a"
Success ('a',"")
ghci> parse aOrB "b"
Success ('b',"")
ghci> parse aOrB "c"
Failure "Expected b, got c"
```

### Repeating Parsers

Parsers can be repeated in two ways:

- Using `any`: Matching zero or more successful parses
```
ghci> anyA = Parser.any $ char 'a'
ghci> parse anyA "bcdefg"
Success ("","bcdefg")
ghci> parse anyA "abcdefg"
Success ("a","bcdefg")
ghci> parse anyA "aaabcdefg"
Success ("aaa","bcdefg"

```
- Using `some`: Matching one or more successful parses
```
ghci> someA = Parser.some $ char 'a'
ghci> parse someA "bcdefg"
Failure "Expected a, got b"
ghci> parse someA "abcdefg"
Success ("a","bcdefg")
ghci> parse someA "aaabcdefg"
Success ("aaa","bcdefg")
```

The parsers created from using `some` or `any` return a *list* of the return type; `some (char 'a')` has a type of `Parser Char [Char]`.

### Combining Parsers

The `Parser` type is a monad, meaning complex parsers can be made using `bind`. `do` notation makes this more concise.

```
-- Take a Char list as input, parse a Float
add :: Parser Char Float
{- Using bind
add = number >>= \left ->
      char '+' >>
      number >>= \right ->
      return (left + right)
-}
-- Using do
add =
    do
        left <- number
        char '+'
        right <- number
        return (left + right)
```

### Ignoring Results

Using the keep-left and keep-right operators (`<*` and `*>`, respectively), Parsers can be chained, and only keep one result
```
ghci> keepA = char 'a' <* char 'b'
ghci> parse keepA "ab"
Success ('a',"")
ghci> parse keepA "aab"
Failure "Expected b, got a"
ghci> parse keepA "b"
Failure "Expected a, got b"
ghci> keepB = char 'a' *> char 'b'
ghci> parse keepB "ab"
Success ('b',"")
ghci> parse keepB "abb"
Success ('b',"b")
ghci> parse keepB "aab"
Failure "Expected b, got a"
ghci> keepC = char 'a' *> char 'b' *> char 'c'
ghci> parse keepC "abc"
Success ('c',"")
```

## In Case of Errors

In the case of an unconditional error, the `failure` function can be used to create a parser which always fails.
```
ghci> alwaysFail = failure "This can never be a Success"
ghci> parse alwaysFail "test"
Failure "This can never be a Success"
ghci> parse (char 'a' *> alwaysFail) "abc"
Failure "This can never be a Success"
ghci> parse (char 'a' <* alwaysFail) "abc"
Failure "This can never be a Success"
```

## Evaluation

With the `Parser` type being generic, a `Parser` can return a multitude of types. As shown in the example included (see below), input can be evaluated at parse time, or can be used to create other type useful in lexers/compilers.

Run-time evaluation:
```
add :: Parser Char Float
add =
    do
        left <- number
        char '+'
        right <- number
        return (left + right)
```

Parsing into other data types:
```
type Token a = Val a | Add ... -- This can be expanded

add :: (Num a) => Parser Char [Token a]
add =
    do
        left <- number
        char '+'
        right <- number
        return [Val left, Add, Val right]
```

## Example

The `Main.hs` file included contains a basic example of an expression parser. It can be run using `runhaskell`:
```
> cd src/
src/> runhaskell Main.hs
```
