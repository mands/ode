module Main where

import Text.ParserCombinators.Parsec
import Expressions
--import Lexical

-- basic
simple :: Parser Char
simple  = letter

-- sequencing using do notation
openClose :: Parser Char
openClose = do{ char '('
              ; char ')'
              }
              
-- choice using <|> operator
parens  :: Parser ()
parens  = do{ char '('
            ; parens
            ; char ')'
            ; parens
            }
        <|> return ()

-- predicitive parsing
testOr  =   string "(a)"
        <|> string "(b)"

testOr1  = do { char '('
              ; string "a)"
                <|> string "b)"
              }
-- backtracking using "try", allows infinite look-ahead without consuming input
testOr2 =   try (string "(a)")
        <|> string "(b)"

-- 
testOr3 =   do{ try (string "(a"); char ')'; return "(a)" }
        <|> string "(b)"
        
-- adding semantics
nesting :: Parser Int
nesting = do{ char '('
            ; n <- nesting
            ; char ')'
            ; m <- nesting
            ; return (max (n+1) m)
            }
        <|> return 0        

-- sequences and separators
word1    :: Parser String
word1    = do{ c  <- letter
            ; do{ cs <- word
                ; return (c:cs)
                }
              <|> return [c]
            }  

word2    :: Parser String
word2    = many1 letter

sentence1    :: Parser [String]
sentence1    = do{ words <- sepBy1 word separator
                ; oneOf ".?!"
                ; return words
                }
                
separator1   :: Parser ()
separator1   = skipMany1 (space <|> char ',')

-- error messages
word    :: Parser String
word    = many1 (letter <?> "") <?> "word"

sentence    :: Parser [String]
sentence    = do{ words <- sepBy1 word separator
                ; oneOf ".?!" <?> "end of sentence"
                ; return words
                }

separator   :: Parser ()
separator   = skipMany1 (space <|> char ',' <?> "")


-- parser runner
run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x
            

