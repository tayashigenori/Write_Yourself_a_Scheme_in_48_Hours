import Text.ParserCombinators.Parsec
import System.Environment
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex
import Data.Array
import Control.Monad.Error

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space

instance Show LispVal where show = showVal
data LispVal = Nil
             | Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | Vector (Array Int LispVal)

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"

showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


instance Show LispError where show = showError
instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected 
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

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
                x <- many ( escapedChars <|> noneOf "\"\\" )
                char '"'
                return $ String x

escapedChars :: Parser Char
escapedChars = do x <- char '\\' >> oneOf "\\\"nrt"
                  return $ case x of
                    'n' -> '\n'
                    'r' -> '\r'
                    't' -> '\t'
                    _ -> x

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

-- exercise 4.3 その２
parseList :: Parser LispVal
parseList = between beg end parseList1
            where beg = (char '(' >> skipMany space)
                  end = (skipMany space >> char ')')
parseList1 :: Parser LispVal
parseList1 = do
               list <- sepEndBy parseExpr spaces
               datum <- option Nil (char '.' >> spaces >> parseExpr)
               return $ case datum of
                  Nil -> List list
                  val  -> DottedList list val

-- exercise 4.3 その３
--parseList :: Parser LispVal
--parseList = do
--              char '(' >> spaces
--              head <- parseExpr `sepEndBy` spaces1
--              do
--                char '.' >> spaces1
--                tail <- parseExpr
--                spaces >> char ')'
--                return $ DottedList head tail
--              <|> (spaces >> char ')' >> (return $ List head))

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
        <|> parseNumber
        <|> parseQuoted
        <|> try (do string "#("
                    x <- parseVector
                    char ')'
                    return x)
        <|> parseList
        <|> parseQuasiQuoted
        <|> parseUnQuote

--eval :: LispVal -> LispVal
--eval val@(String _) = val
--eval val@(Number _) = val
--eval val@(Bool _) = val
--eval (List [Atom "quote", val]) = val
--eval (List (Atom func : args)) = apply func $ map eval args
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

--apply :: String -> [LispVal] -> LispVal
--apply func args = maybe (Bool False) ($ args) $ lookup func primitives
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)


--primitives :: [(String, [LispVal] -> LispVal)]
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
--              ("remainder", numericBinop rem),
              ("remainder", numericBinop rem)]
--              ("symbol?" , unaryOp symbolp) ,
--              ("string?" , unaryOp stringp) ,
--              ("number?" , unaryOp numberp) ,
--              ("bool?", unaryOp boolp) ,
--              ("list?" , unaryOp listp)]

--numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
--numericBinop op params = Number $ foldl1 op $ map unpackNum params
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

--unpackNum :: LispVal -> Integer
--unpackNum (Number n) = n
--unpackNum _ = 0
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                          if null parsed 
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _)   = Bool True
symbolp _          = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
boolp   (Bool _)   = Bool True
boolp   _          = Bool False
listp   (List _)   = Bool True
listp   (DottedList _ _) = Bool True
listp   _          = Bool False

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s)   = String s
symbol2string _          = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""


--readExpr :: String -> LispVal
--readExpr input = case parse parseExpr "lisp" input of
--    Left err -> String $ "No match: " ++ show err
--    Right val -> val
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val


--main :: IO ()
--main = getArgs >>= print . eval . readExpr . head
main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
