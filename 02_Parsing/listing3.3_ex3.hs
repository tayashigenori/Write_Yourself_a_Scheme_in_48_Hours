import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

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
parseNumber = many1 digit >>= \number ->
              return $ (Number . read) number

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

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
