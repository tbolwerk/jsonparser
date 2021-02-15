module Main where
import Control.Applicative
import Data.Char

newtype Parser a = Parser { parse :: String ->  [(a, String)] }

instance Functor Parser where
 fmap f (Parser x) = Parser $ \s -> case x s of
                                      [] -> []
                                      [(a,xs)] ->  [(f a, xs)]

instance Applicative Parser where
 pure x = Parser $ \s -> [(x, s)]
 pmfab <*> pa = Parser $ \s ->  case parse pmfab s of
                                  [] -> []
                                  [(f,s')] -> parse (f <$> pa) s'

instance Monad Parser where
 return = pure
 ma >>= fmb = Parser $ \s -> case parse ma s of
                                  [] -> []
                                  [(a, s')] -> parse (fmb a) s'

instance Alternative Parser where
 empty = Parser $ \s -> []
 a <|> b = Parser $ \s -> case parse a s of
                             [] -> parse b s
                             _       -> parse a s


data Json = JsonNumber Int 
               | JsonString String 
               | JsonObject [(String, Json)] 
               | JsonArray [Json] 
               | JsonBool Bool 
               | JsonNull 
                  deriving Show 


jsonNull :: Parser Json
jsonNull = undefined

jsonParser :: Parser Json
jsonParser = undefined

parseChar :: Parser Char
parseChar = Parser $ \s -> case s of
                              "" -> empty 
                              (x:xs) ->  [(x,xs)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
 x <- parseChar
 if p x then return x else empty

digit :: Parser Char 
digit = satisfy isDigit

lower :: Parser Char
lower = satisfy isLower

upper :: Parser Char
upper = satisfy isUpper

space :: Parser Char
space = satisfy isSpace

alphanum :: Parser Char
alphanum = satisfy isAlphaNum

{- checks for valid characters -} 
char :: Char -> Parser Char
char x = satisfy (==x)
 
isValidChar :: Parser Char
isValidChar =  digit <|> lower <|> upper <|> space <|> alphanum

string :: String -> Parser String
string [] = return []
string (x:xs) = do
 char x
 string xs
 return (x:xs)

whitespace :: Parser ()
whitespace = many (satisfy isSpace) *> pure ()

token :: Parser a -> Parser a
token p = do
 whitespace
 v <- p
 whitespace
 return v

symbol :: String -> Parser String
symbol s = token (string s)

parseValue :: Parser Json
parseValue = parseString <|> parseNumber <|> parseBool <|> parseNull <|> parseArray <|> parseObject
 
parseNull :: Parser Json
parseNull = JsonNull <$ symbol "null"

parseBool :: Parser Json
parseBool = JsonBool <$> ((True <$ symbol "true") <|> (False <$ symbol "false"))

naturalNumber :: Parser Int
naturalNumber = do
 xs <- some digit
 return (read xs)

parseNumber :: Parser Json
parseNumber = JsonNumber <$> naturalNumber 

parseString :: Parser Json
parseString= JsonString <$> stringLiteral 

stringLiteral :: Parser String
stringLiteral = (char '"' *> many isValidChar <* char '"')

parseArray :: Parser Json
parseArray = JsonArray <$> (char '[' *> whitespace *> elements <* whitespace <* char ']')
 where elements = parseElements (whitespace *> char ',' <* whitespace) parseJson  

parseElements :: Parser a -> Parser b -> Parser [b]
parseElements a b = (:) <$> b <*> many (a *> b) <|> pure []

parseObject :: Parser Json
parseObject = JsonObject <$> (char '{' *> whitespace *> recordAndFields  <* whitespace <* char '}')
 where recordAndFields = parseElements (whitespace *> char ',' <* whitespace) pair
       pair            = liftA2 (,) (stringLiteral <* whitespace <* char ':' <* whitespace) parseJson

parseJson :: Parser Json
parseJson = whitespace *> parseValue <* whitespace

parseFile :: FilePath -> Parser a -> IO ()
parseFile fileName parser = do
 content <- readFile fileName
 jsonContent <- return (parse parseJson content)
 putStrLn (show jsonContent)



main :: IO ()
main = do 
 path <- readLn
 parseFile path parseJson







