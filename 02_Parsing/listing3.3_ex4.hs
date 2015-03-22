import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
-- exercise 4
import Numeric

symbol :: Parser Char
-- exercise 4
--symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

--spaces :: Parser ()
--spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool deriving Show

parseString :: Parser LispVal
parseString = do
                char '"'
-- exercise 2
--                x <- many (noneOf "\"")
                x <- many ( escapedChars <|> noneOf "\"\\" )
                char '"'
                return $ String x

escapedChars :: Parser Char
-- exercise 3
escapedChars = do x <- char '\\' >> oneOf "\\\"nrt"
                  return $ case x of
                    'n' -> '\n'
                    'r' -> '\r'
                    't' -> '\t'
                    _ -> x
--escapedChars = do
--                char '\\'
--                x <- oneOf "\\\""
--                return x

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseNumber :: Parser LispVal
-- exercise 1.1
--parseNumber = liftM (Number . read) $ many1 digit
-- exercise 1.2
--parseNumber = do
--                number <- many1 digit
--                return $ (Number . read) number
-- exercise 4
--parseNumber = many1 digit >>= \number ->
--              return $ (Number . read) number
parseNumber = parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin

parseDigital1 :: Parser LispVal
parseDigital1 = do
                  number <- many1 digit
                  return $ (Number . read) number
parseDigital2 :: Parser LispVal
parseDigital2 = do
                  try $ string "#d"
                  number <- many1 digit
                  return $ (Number . read) number
parseHex :: Parser LispVal
parseHex = do
             try $ string "#x"
             number <- many1 hexDigit
             return $ Number (hex2digRead number)
parseOct :: Parser LispVal
parseOct = do
             try $ string "#o"
             number <- many1 octDigit
             return $ Number (oct2digRead number)
parseBin :: Parser LispVal
parseBin = do
             try $ string "#b"
             number <- many1 (oneOf "10")
             return $ Number (bin2digRead number)
oct2digRead x = fst $ readOct x !! 0
hex2digRead x = fst $ readHex x !! 0
bin2digRead  = bin2digRead' 0
bin2digRead' digint "" = digint
bin2digRead' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                             bin2digRead' old xs

parseBool :: Parser LispVal
parseBool = do
    char '#'
    x <- oneOf "ft"
    return $ case x of
      't'  -> (Bool True)
      'f'  -> (Bool False)
--    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseBool

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"
--readExpr :: String -> String
--readExpr input = case parse (spaces >> symbol) "lisp" input of
--    Left err -> "No match: " ++ show err
--    Right val -> "Found value"
--readExpr input = case parse symbol "lisp" input of
--    Left err -> "No match: " ++ show err
--    Right val -> "Found value"

main :: IO ()
--main = do args <- getArgs
--          putStrLn (readExpr (args !! 0))
main = do
         args <- getArgs
         parseTest parseExpr (args !! 0)
