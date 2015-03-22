-- exercise 4.3 その３
--import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
-- exercise 4
import Numeric
-- exercise 7
import Data.Ratio
import Data.Complex
-- exercise 4.2
import Data.Array

symbol :: Parser Char
-- exercise 4
--symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

-- exercise 4.3 その３
--spaces :: Parser ()
--spaces = skipMany1 space
spaces1 :: Parser ()
spaces1 = skipMany1 space

-- exercise 4.3 その２
--data LispVal = Atom String
data LispVal = Nil
             | Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
-- exercise 5
--             | Bool Bool deriving Show
             | Bool Bool
-- exercise 6
--             | Character Char deriving Show
             | Character Char
-- exercise 7
--             | Float Double deriving Show
             | Float Double
             | Ratio Rational
-- exercise 4.2
--             | Complex (Complex Double) deriving Show
             | Complex (Complex Double)
             | Vector (Array Int LispVal) deriving Show

parseCharacter :: Parser LispVal
parseCharacter = do
                   try $ string "#\\"
                   value <- try (string "newline" <|> string "space") 
                           <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
                   return $ Character $ case value of
                                          "space" -> ' '
                                          "newline" -> '\n'
                                          otherwise -> (value !! 0)

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

parseFloat :: Parser LispVal
parseFloat = do
               integer <- many1 digit
               char '.'
               decimal <- many1 digit
               return $ Float (fst . head $ readFloat (integer ++ "." ++ decimal))

parseRatio :: Parser LispVal
parseRatio = do
               numerator <- many1 digit
               char '/'
               denominator <- many1 digit
               return $ Ratio ((read numerator) % (read denominator))

parseComplex :: Parser LispVal
parseComplex = do
                 real <- (try parseFloat <|> parseDecimal)
                 char '+' 
                 imaginary <- (try parseFloat <|> parseDecimal)
                 char 'i' 
                 return $ Complex (toDouble real :+ toDouble imaginary)
parseDecimal :: Parser LispVal
parseDecimal = parseDigital1 <|> parseDigital2
toDouble :: LispVal -> Double
toDouble(Float f) = f
toDouble(Number n) = fromIntegral n

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

-- exercise 4.3 その１
--parseList :: Parser LispVal
--parseList = liftM List $ sepBy parseExpr spaces

--parseDottedList :: Parser LispVal
--parseDottedList = do
--    head <- endBy parseExpr spaces
--    tail <- char '.' >> spaces >> parseExpr
--    return $ DottedList head tail

-- exercise 4.3 その２
--parseAnyList :: Parser LispVal
--parseAnyList = do
--  P.char '('
--  optionalSpaces
--  head <- P.sepEndBy parseExpr spaces
--  tail <- (P.char '.' >> spaces >> parseExpr) <|> return (Nil ())
--  optionalSpaces
--  P.char ')'
--  return $ case tail of
--    (Nil ()) -> List head
--    otherwise -> DottedList head tail

-- exercise 4.3 その３
--parseList :: Parser LispVal
--parseList = between beg end parseList1
--            where beg = (char '(' >> skipMany space)
--                  end = (skipMany space >> char ')')
--parseList1 :: Parser LispVal
--parseList1 = do
--               list <- sepEndBy parseExpr spaces
--               datum <- option Nil (char '.' >> spaces >> parseExpr)
--               return $ case datum of
--                  Nil -> List list
--                  val  -> DottedList list val

parseList :: Parser LispVal
parseList = do
              char '(' >> spaces
              head <- parseExpr `sepEndBy` spaces1
              do
                char '.' >> spaces1
                tail <- parseExpr
                spaces >> char ')'
                return $ DottedList head tail
              <|> (spaces >> char ')' >> (return $ List head))

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseVector :: Parser LispVal
parseVector = do
                arrayValues <- sepBy parseExpr spaces
                return $ Vector (listArray (0, (length arrayValues - 1)) arrayValues)

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
-- exercise 5
--        <|> parseNumber
--        <|> parseBool
-- exercise 6
--        <|> try parseFloat
-- exercise 7
--        <|> try parseRatio
--        <|> try parseComplex
--        <|> try parseNumber -- we need the 'try' because 
--        <|> try parseBool -- these can all start with the hash char
--        <|> try parseCharacter
        <|> parseNumber
        <|> parseQuoted
-- exercise 4.2
        <|> try (do string "#("
                    x <- parseVector
                    char ')'
                    return x)
-- exercise 4.3 その１
--        <|> do char '('
--               x <- try parseList <|> parseDottedList
--               char ')'
--               return x
-- exercise 4.3 その２
--        <|> parseAnyList
        <|> parseList
-- exercise 4.1
        <|> parseQuasiQuoted
        <|> parseUnQuote

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
