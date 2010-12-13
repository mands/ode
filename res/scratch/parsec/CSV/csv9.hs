import System.IO
import System.Environment(getArgs)
import Text.ParserCombinators.Parsec

csvFile = endBy line eol 
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell =
    do  char '"'
        content <- many quotedChar
        char '"' <?> "quote at end of cell"
        return content
        
quotedChar =    noneOf "\""
            <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"    
    <|> string "\r"
    <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

main = 
    do  
        args <- getArgs
        let fileName = head args
        c <- readFile fileName
        case parse csvFile fileName c of
            Left e -> do putStrLn "Error parsing input"
            Right r -> mapM_ print r

