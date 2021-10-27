module Main where
import Control.Applicative
import Data.Char

data JsonType = JsonBool Bool | JsonString String | JsonMap [(String, JsonType)] | JsonArray [JsonType] | JsonNull | JsonInt Int | JsonFloat Float
 deriving Show

data ParseError = ParseError Int String deriving Show

data Input = Input
             {  loc :: Int,
                inp :: String
             } deriving (Show, Eq)

newtype Parser a = Parser { parse :: Input -> Either ParseError (a, Input) }

instance Functor Parser where
 fmap f x = Parser $ \s -> case parse x s of
                              (Left e) -> (Left e)
                              (Right (a, s')) -> (Right (f a, s'))

instance Applicative Parser where
 pure x = Parser $ \s -> Right (x, s)
 (<*>) f x = Parser $ \s -> case parse f s of
                               (Left e) -> (Left e) 
                               (Right (f,s')) ->  case parse x s' of
                                                   (Left e') -> (Left e')
                                                   (Right (a, s'')) -> (Right (f a, s''))

instance Monad Parser where
 return = pure
 (>>=) ma f = Parser $ \s -> case parse ma s of
                                    (Left e) -> (Left e)
                                    (Right (a, s')) -> parse (f a) s'


instance Alternative Parser where
 empty = Parser $ \s -> Left $ ParseError (loc s) (inp s)
 (<|>) a b = Parser $ \s -> let a' = parse a s in
                              case a' of
                                  (Left _) -> parse b s
                                  _  -> a'

parseChar :: Parser Char
parseChar = Parser $ \s -> case s of
                             (Input loc (x:xs)) -> Right (x,(Input (loc +1) (xs)))
                             (Input loc [])     -> Left $ ParseError (loc) "failed parsing character"

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = let pmc = fmap (\x -> if p x then Just x else Nothing) parseChar
            in do 
               mc  <- pmc
               case mc of
                 Just c -> return c
                 Nothing -> empty

symbol :: Char -> Parser Char
symbol s = satisfy (==s)

parseStringLiteral :: Parser String
parseStringLiteral = symbol '"' *>  many (satisfy (/='"'))  <* symbol '"'

token :: String -> Parser String
token [] = return []
token (x:xs) = do
   x' <- symbol x
   xs' <- token xs
   return (x':xs')

parseBool :: Parser JsonType
parseBool = JsonBool <$> ((True <$ token "true") <|> (False <$ token "false"))

parseString :: Parser JsonType
parseString = JsonString <$> parseStringLiteral

whitespace = many (satisfy isSpace) 

parseJson :: Parser JsonType
parseJson =  parseArray <|> parseMap <|> parseString <|> parseBool <|> parseNumber
parseRecord :: Parser (String, JsonType)
parseRecord =  (,) <$> parseStringLiteral <* whitespace <* symbol ':' <* whitespace <*> parseJson
 
parseMap :: Parser JsonType
parseMap = symbol '{' *>  whitespace *> (JsonMap <$> (many record)) <* whitespace <* symbol '}'
   where record = (do
              whitespace
              symbol ','
              whitespace
              parseRecord)
               <|> (do 
                      whitespace 
                      parseRecord)
digit :: Parser Char
digit = satisfy isDigit

parseFloat :: Parser Float
parseFloat = do
  xs <- some digit
  x  <- symbol '.'
  ys <- some digit
  return (read (xs ++ [x] ++ ys))


parseInt :: Parser Int
parseInt = do
 xs <- some digit
 return (read xs)

parseNumber :: Parser JsonType
parseNumber =  (JsonFloat <$> parseFloat) <|> (JsonInt <$> parseInt) 

parseArray :: Parser JsonType
parseArray = symbol '[' *> whitespace *> (JsonArray <$> (many element)) <* whitespace <* symbol ']'
 where element = (do 
                    whitespace
                    symbol ','
                    whitespace
                    parseJson) <|> (do 
                                     whitespace
                                     parseJson)

printParse :: Show a => FilePath -> Parser a -> IO ()
printParse fileName parser = do
   parsedContent <- parseFile fileName parser
   putStrLn (show parsedContent)

parseFile :: Show a => FilePath -> Parser a -> IO (Either ParseError (a, Input))
parseFile fileName parser = do
   content <- readFile fileName
   return (parse parser (Input 0 content))
   
main :: IO ()
main = do 
 path <- readLn
 printParse path parseJson










