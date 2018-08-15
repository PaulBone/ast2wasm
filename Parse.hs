
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

expr = do e1 <- expr0
          -- This could be a binary operation, or just a simple expression
          -- below.  This will return e1 if the parser binary operation
          -- parse fails.
          option e1 $ do op <- bop
                         e2 <- expr
                         return $ BOp op e1 e2
    where bop = do choice [plus >> return Add,
                           minus >> return Subtract,
                           asterisk >> return Multiply,
                           slash >> return Divide]

expr0 = choice [num, try call, var, parens_expr]
    where num = do n <- number
                   return $ Lit32 n
          call = do callee <- ident
                    call_args <- between openParen closeParen $ sepBy expr comma
                    return $ Call callee call_args
          var = do v <- ident
                   return $ Var v
          parens_expr = between openParen closeParen expr

skip p = p >> (return ())

