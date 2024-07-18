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
    string,
    natural,
    int,
    decimal,
    token,
) where

import Control.Applicative (Alternative, empty, (<|>))
import Text.Printf (printf)
import Prelude hiding (any, some)

data ParserResult a = Success (a, String) | Failure String
    deriving (Show)

newtype Parser a = Parser (String -> ParserResult a)

parse :: Parser a -> String -> ParserResult a
parse (Parser p) = p

any :: Parser a -> Parser [a]
any p =
    Parser
        ( \str -> case parse p str of
            Success (res, rem) -> case parse (any p) rem of
                Success (res2, rem2) -> Success (res : res2, rem2)
                Failure _ -> Success ([res], rem)
            Failure _ -> Success ([], str)
        )

some :: Parser a -> Parser [a]
some p =
    Parser
        ( \str -> case parse p str of
            Success (res, rem) -> case parse (any p) rem of
                Success (res2, rem2) -> Success (res : res2, rem2)
                Failure _ -> Success ([res], rem)
            Failure e -> Failure e
        )

instance Functor Parser where
    fmap f p =
        Parser
            ( \str -> case parse p str of
                Success (res, rem) -> Success (f res, rem)
                Failure e -> Failure e
            )

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure a = Parser (\str -> Success (a, str))

    -- (<*>) :: f (a -> b) -> f a -> f b
    (<*>) pf pa =
        Parser
            ( \str -> case parse pf str of
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

instance Alternative Parser where
    -- empty :: Parser a
    empty = Parser (\str -> Failure "Empty")

    -- (<|>) :: f a -> f a -> f a
    (<|>) left right =
        Parser
            ( \inp -> case parse left inp of
                Success (parsed, rest) -> Success (parsed, rest)
                Failure _ -> parse right inp
            )

instance Monad Parser where
    -- return :: a -> Parser a
    return = pure

    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) left f =
        Parser
            ( \str -> case parse left str of
                Success (res, rem) -> parse (f res) rem
                Failure e -> Failure e
            )

-- Fail always with msg.
failure :: String -> Parser a
failure msg = Parser (\_ -> Failure msg)

-- Parses the next character in the stream indiscriminantly
item :: Parser Char
item =
    Parser
        ( \str -> case str of
            [] -> parse empty str
            (h : t) -> Success (h, t)
        )

-- Parses an alphabetic character
alpha :: Parser Char
alpha =
    Parser
        ( \str -> case str of
            [] -> parse empty str
            (h : t) ->
                if h `elem` ['a' .. 'z'] ++ ['A' .. 'Z']
                    then Success (h, t)
                    else Failure $ printf "Expected alphabetic char, got %c" h
        )

-- Parses an numeric character
num :: Parser Char
num =
    Parser
        ( \str -> case str of
            [] -> parse empty str
            (h : t) ->
                if h `elem` ['0' .. '9']
                    then Success (h, t)
                    else Failure $ printf "Expected numeric char, got %c" h
        )

-- Parses an alphanumeric character
alphanum :: Parser Char
alphanum = alpha <|> num

-- Parse a specific character
char :: Char -> Parser Char
char c =
    Parser
        ( \str -> case str of
            [] -> parse empty str
            (h : t) ->
                if h == c
                    then Success (h, t)
                    else Failure $ printf "Expected %c, got %c" c h
        )

-- Parse a whitespace character
whitespace :: Parser Char
whitespace = char ' ' <|> char '\t' <|> char '\n' <|> char '\r'

-- Parse a specific string
string :: String -> Parser String
string = mapM char

-- Parse a natural number
natural :: Parser Int
natural =
    Parser
        ( \str -> case parse (some num) str of
            Success (res, rem) -> Success (read res :: Int, rem)
            Failure e -> Failure e
        )

-- Parse an integer
int :: Parser Int
int =
    do
        char '-'
        v <- natural
        return (-v)
        <|> natural

-- Parse a decimal number
decimal :: Parser Float
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
token :: Parser a -> Parser a
token parser = any whitespace *> parser <* any whitespace
