

-- separate scanners - i.e. preprocessed set of tokens
type Token  = (SourcePos,Tok)
data Tok    = Identifier String
            | Reserved   String
            | Symbol     String
            | Price      Int
            deriving Show

scanner :: [Char] -> ([Token],[String])

type MyParser a   = GenParser Token () a

mytoken :: (Tok -> Maybe a) -> MyParser a
mytoken test
  = token showToken posToken testToken
  where
    showToken (pos,tok)   = show tok
    posToken  (pos,tok)   = pos
    testToken (pos,tok)   = test tok

identifier :: MyParser String
identifier 
  = mytoken (\tok -> case tok of 
                       Identifier name -> Just name
                       other           -> Nothing)

reserved :: String -> MyParser ()
reserved name
  = mytoken (\tok -> case tok of
                       Reserved s   | s == name  -> Just ()
                       other        -> Nothing)


-- user state, uses state monad
run input 
  = case runParser parser 0 "" input of
      Right n  -> putStrLn ("counted " ++ show n ++ " identifiers!")
      Left err -> do{ putStr "parse error at "
                    ; print err
                    }

parser :: CharParser Int Int
parser 
  = do{ ...
      ; n <- getState
      ; return n
      }

...

myIdentifier :: CharParser Int String
myIdentifier
  = do{ x <- identifier
      ; updateState (+1)
      ; return x
      }

-- Advanced: Permutation phrases - use for value information/atteibutes
perm0 = permute (f (<$$>) char 'a'
                   (<||>) char 'b'
                   (<||>) char 'c')
      where
        f a b c  = [a,b,c]
        

perm1 :: Parser (String,Char,Char)
perm1 = permute (tuple <$?> ("",many1 (char 'a'))
                       <||> char 'b' 
                       <|?> ('_',char 'c'))
      where
        tuple a b c  = (a,b,c)
        


