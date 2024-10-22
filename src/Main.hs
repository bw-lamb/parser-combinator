module Main where

import Parser

-- expr ::= term + expr
--          | term - expr
--          | term
--
-- term ::= factor * term
--          | factor / term
--          | factor
--
-- factor ::= ( expr )
--            | variable
--            | number

expr :: Parser Char Float
expr =
    do
        l <- token $ term
        char '+'
        r <- token $ expr
        return (l + r)
        <|> do
            l <- token $ term
            char '-'
            r <- token $ expr
            return (l - r)
        <|> term

term :: Parser Char Float
term =
    do
        l <- token $ factor
        char '*'
        r <- token $ term
        return (l * r)
        <|> do
            l <- token $ factor
            char '/'
            r <- token $ expr
            return (l / r)
        <|> factor

factor :: Parser Char Float
factor =
    do
        token $ char '('
        e <- expr
        token $ char ')'
        return e
        <|> number

number :: Parser Char Float
number = do decimal <|> fromIntegral <$> int

main :: IO ()
main = 
    do
        input <- getLine
        let result = parse expr input
        print result
        main
