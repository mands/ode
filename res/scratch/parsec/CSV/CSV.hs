import Text.ParserCombinators.Parsec

-- main file - 0 or more lines ([[String]]), each terminated with EOL char
csvFile :: GenParser Char st [[String]]
csvFile = 
    do  result <- many line
        eof
        return result
        
-- each line contains one or more cells, comma separated
line :: GenParser Char st [String]
line = 
    do  result <- cells
        eol
        return result
        
-- builds up a recursive list of cells, i.e. comma separated
cells :: GenParser Char st [String]
cells = 
    do  first <- cellContent
        next <- remainingCells
        return (first : next)
        
-- remaining cells
remainingCells :: GenParser Char st [String]
remainingCells = 
    (char ',' >> cells)
    <|> (return [])
    
-- parse everything until separator/eol
cellContent :: GenParser Char st String
cellContent = many (noneOf ",\n")
    
eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

