
module Parse (parseHL) where

import Text.Parsec

import Ast
import Tokenise
import Util

parseHL :: String -> Either String Module
parseHL input =
    do tokens' <- tokenise input
       case parse moduleP "" tokens' of
        Left e -> Left $ show e
        Right ast -> Right ast

moduleP = do skipMany (eol)
             mb_funcs <- many (justFunc <|> emptyLine)
             eof
             return $ Module (listMaybeToList mb_funcs)
    where justFunc = funcP >>= (return . Just)
          emptyLine = eol >> (return Nothing)

funcP = try $ do vis <- option Private (public >> return Public)
                 fname <- ident
                 fargs <- many ident
                 skip equals
                 fbody <- expr
                 eol
                 return $ Func fname vis fargs fbody

expr = choice [num, var, bopexpr]
    where num = do n <- number
                   return $ Lit32 n
          var = do v <- ident
                   return $ Var v
          bopexpr = do openParen
                       e1 <- expr
                       op <- bop
                       e2 <- expr
                       closeParen
                       return $ BOp op e1 e2
          bop = do choice [plus >> return Add,
                           minus >> return Subtract,
                           asterisk >> return Multiply,
                           slash >> return Divide]

skip p = p >> (return ())

