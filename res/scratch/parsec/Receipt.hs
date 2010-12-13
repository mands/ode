module Receipt where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( haskellDef )
import Data.Char( digitToInt )


lexer :: P.TokenParser ()
lexer  = P.makeTokenParser 
         (haskellDef)
         { P.reservedNames   = ["return","total"]
         , P.reservedOpNames = ["*","/","+","-"]
         }

-- For efficiency, we will bind all the used lexical parsers at toplevel.
whiteSpace = P.whiteSpace lexer
lexeme    = P.lexeme lexer
symbol    = P.symbol lexer
natural   = P.natural lexer
parens    = P.parens lexer
semi      = P.semi lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer

receipt :: Parser Bool
receipt = do{ ps <- many produkt
            ; p  <- total
            ; return (sum ps == p)
            }
            
produkt = do{ reserved "return"
            ; p <- price
            ; semi
            ; return (-p)
            }
      <|> do{ identifier
            ; p  <- price
            ; semi
            ; return p
            }
      <?> "produkt"

total   = do{ p <- price
            ; reserved "total"
            ; return p
            }


price   :: Parser Int   -- price in cents         
price   = lexeme (do{ ds1 <- many1 digit
                    ; char '.'
                    ; ds2 <- count 2 digit
                    ; return (convert 0 (ds1 ++ ds2))            
                    })
          <?> "price"
          where
            convert n []     = n
            convert n (d:ds) = convert (10*n + digitToInt d) ds


-- lexical parser runner
runLex :: Show a => Parser a -> String -> IO ()
runLex p input
        = run (do{ whiteSpace
                 ; x <- p
                 ; eof
                 ; return x
                 }) input

-- parser runner
run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x

