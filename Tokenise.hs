{-# LANGUAGE FlexibleContexts #-}
module Tokenise (tokenise,
                 public,
                 equals,
                 plus,
                 minus,
                 asterisk,
                 slash,
                 comma,
                 openParen,
                 closeParen,
                 ident,
                 number,
                 eol) where

import Data.Functor.Identity (Identity)
import Text.Parsec

data Token = Public
           | Equals
           | Plus
           | Minus
           | Asterisk
           | Slash
           | Comma
           | OpenParan
           | CloseParan
           | Ident String
           | Num Integer
           | EOL
           | EOF
    deriving (Eq, Show)

data TokenPos = TokenPos Token SourcePos

tokenise :: String -> Either String [TokenPos]
tokenise input = case parse tokeniser "" input of
                    Right r -> Right r
                    Left e -> Left $ show e

tokeniser =
    do skipWSOrComment
       toks <- many $ do tok <- choice [symStr "public" Public,
                                        sym '=' Equals,
                                        sym '+' Plus,
                                        sym '-' Minus,
                                        sym '*' Asterisk,
                                        sym '/' Slash,
                                        sym ',' Comma,
                                        sym '(' OpenParan,
                                        sym ')' CloseParan,
                                        ident_tok,
                                        number_tok,
                                        sym '\n' EOL]

                         skipWSOrComment
                         return tok
       eof
       return toks

ident_tok = do first <- letter <|> (char '_')
               rest <- many (letter <|> (char '_') <|> digit)
               pos <- getPosition
               return $ (TokenPos (Ident (first:rest)) pos)

number_tok = do num <- many1 digit
                pos <- getPosition
                return $ (TokenPos (Num (read num)) pos)

skipWSOrComment = skipMany (ws <|> comment)
    where ws = oneOf " \t" >> return ()
          comment = try $ do _ <- string "--"
                             _ <- manyTill anyChar endOfLine
                             return ()

symStr str symbol = do _ <- string str
                       pos <- getPosition
                       return $ TokenPos symbol pos

sym :: Stream s m Char => Char -> Token -> ParsecT s u m TokenPos
sym c s = do _ <- char c
             pos <- getPosition
             return $ TokenPos s pos

----

hlToken t = token showToken nextPos testTok
    where testTok (TokenPos x _) = if x == t then Just () else Nothing

public :: Stream s Identity TokenPos => Parsec s u ()
public = hlToken Public

equals :: Stream s Identity TokenPos => Parsec s u ()
equals = hlToken Equals

plus :: Stream s Identity TokenPos => Parsec s u ()
plus = hlToken Plus

minus :: Stream s Identity TokenPos => Parsec s u ()
minus = hlToken Minus

asterisk :: Stream s Identity TokenPos => Parsec s u ()
asterisk = hlToken Asterisk

slash :: Stream s Identity TokenPos => Parsec s u ()
slash = hlToken Slash

comma :: Stream s Identity TokenPos => Parsec s u ()
comma = hlToken Comma

openParen :: Stream s Identity TokenPos => Parsec s u ()
openParen = hlToken OpenParan

closeParen :: Stream s Identity TokenPos => Parsec s u ()
closeParen = hlToken CloseParan

ident :: Stream s Identity TokenPos => Parsec s u String
ident = token showToken nextPos testIdent
    where testIdent (TokenPos (Ident str) _) = Just str
          testIdent _ = Nothing

number :: Stream s Identity TokenPos => Parsec s u Integer
number = token showToken nextPos testNumber
    where testNumber (TokenPos (Num num) _) = Just num
          testNumber _ = Nothing

eol :: Stream s Identity TokenPos => Parsec s u ()
eol = hlToken EOL

showToken (TokenPos t _) = show t
nextPos (TokenPos _ p) = p

instance Show TokenPos where
    show (TokenPos t _) = show t

