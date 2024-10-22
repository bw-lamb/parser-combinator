module Parser (
    Parser,
    (<|>),
    ParserResult (..),
    parse,
    any,
    some,
    failure,
    item,
    alpha,
    num,
    alphanum,
    char,
    whitespace,
    seqing,
    natural,
    int,
    decimal,
    token,
) where

import Control.Applicative (Alternative, empty, (<|>))
import Text.Printf (printf)
import Prelude hiding (any, some)

data ParserResult b a = Success (b, [a]) | Failure String
    deriving (Show)

-- The Parser type
-- a: The type that we are parsing from. This will be a list of a
-- b: The type we are trying to parse
newtype Parser a b = Parser ([a] -> ParserResult b a)

parse :: Parser a b -> [a] -> ParserResult b a
parse (Parser p) = p

any :: Parser a b -> Parser a [b]
any p =
    Parser
        ( \seq -> case parse p seq of
            Success (res, rem) -> case parse (any p) rem of
                Success (res2, rem2) -> Success (res : res2, rem2)
                Failure _ -> Success ([res], rem)
            Failure _ -> Success ([], seq)
        )

some :: Parser a b -> Parser a [b]
some p =
    Parser
        ( \seq -> case parse p seq of
            Success (res, rem) -> case parse (any p) rem of
                Success (res2, rem2) -> Success (res : res2, rem2)
                Failure _ -> Success ([res], rem)
            Failure e -> Failure e
        )

instance Functor (Parser a) where
    fmap f p =
        Parser
            ( \seq -> case parse p seq of
                Success (res, rem) -> Success (f res, rem)
                Failure e -> Failure e
            )

instance Applicative (Parser a) where
    -- pure :: a -> Parser a
    pure a = Parser (\seq -> Success (a, seq))

    -- (<*>) :: f (a -> b) -> f a -> f b
    (<*>) pf pa =
        Parser
            ( \seq -> case parse pf seq of
                Success (f, rem) -> case parse pa rem of
                    Success (res, rem2) -> Success (f res, rem2)
                    Failure e -> Failure e
                Failure e -> Failure e
            )

    -- Keep the result of the left parser
    -- (*>) :: Parser a -> Parser b -> Parser b
    (*>) left right = left >> right

    -- Keep the result of the right parser
    -- (*>) :: Parser a -> Parser b -> Parser a
    (<*) left right = left >>= \res -> right >> return res

instance Alternative (Parser a) where
    -- empty :: Parser a
    empty = Parser (\seq -> Failure "Empty")

    -- (<|>) :: f a -> f a -> f a
    (<|>) left right =
        Parser
            ( \inp -> case parse left inp of
                Success (parsed, rest) -> Success (parsed, rest)
                Failure _ -> parse right inp
            )

instance Monad (Parser a) where
    -- return :: a -> Parser a
    return = pure

    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) left f =
        Parser
            ( \seq -> case parse left seq of
                Success (res, rem) -> parse (f res) rem
                Failure e -> Failure e
            )

-- Fail always with msg.
failure :: String -> Parser a b
failure msg = Parser (\_ -> Failure msg)

-- Parses the next character in the seqeam indiscriminantly
item :: Parser Char Char
item =
    Parser
        ( \seq -> case seq of
            [] -> parse empty seq
            (h : t) -> Success (h, t)
        )

-- Parses an alphabetic character
alpha :: Parser Char Char
alpha =
    Parser
        ( \seq -> case seq of
            [] -> parse empty seq
            (h : t) ->
                if h `elem` ['a' .. 'z'] ++ ['A' .. 'Z']
                    then Success (h, t)
                    else Failure $ printf "Expected alphabetic char, got %c" h
        )

-- Parses an numeric character
num :: Parser Char Char
num =
    Parser
        ( \seq -> case seq of
            [] -> parse empty seq
            (h : t) ->
                if h `elem` ['0' .. '9']
                    then Success (h, t)
                    else Failure $ printf "Expected numeric char, got %c" h
        )

-- Parses an alphanumeric character
alphanum :: Parser Char Char
alphanum = alpha <|> num

-- Parse a specific character
char :: Char -> Parser Char Char
char c =
    Parser
        ( \seq -> case seq of
            [] -> parse empty seq
            (h : t) ->
                if h == c
                    then Success (h, t)
                    else Failure $ printf "Expected %c, got %c" c h
        )

-- Parse a whitespace character
whitespace :: Parser Char Char
whitespace = char ' ' <|> char '\t' <|> char '\n' <|> char '\r'

-- Parse a specific seqing
seqing :: String -> Parser Char String
seqing = mapM char

-- Parse a natural number
natural :: Parser Char Int
natural =
    Parser
        ( \seq -> case parse (some num) seq of
            Success (res, rem) -> Success (read res :: Int, rem)
            Failure e -> Failure e
        )

-- Parse an integer
int :: Parser Char Int
int =
    do
        char '-'
        v <- natural
        return (-v)
        <|> natural

-- Parse a decimal number
decimal :: Parser Char Float
decimal =
    do
        char '-'
        w <- natural
        char '.'
        f <- natural
        let v = fromIntegral w + (fromIntegral f / magnitude (fromIntegral f))
        return (-v)
        <|> do
            w <- natural
            char '.'
            f <- natural
            let v = fromIntegral w + (fromIntegral f / magnitude (fromIntegral f))
            return v

-- Get the magnitude of a number (how much to divide it by to make it completely fractional. e.g. 1234 -> 0.1234)
magnitude :: Float -> Float
magnitude v
    | v < 10.0 = 10.0
    | otherwise = 10.0 * magnitude (v / 10.0)

-- Wrap a parser so that it will succeed when surrounded by any or no whitespac
token :: Parser Char b -> Parser Char b
token parser = any whitespace *> parser <* any whitespace
