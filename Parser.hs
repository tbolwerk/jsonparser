module Main where
import Control.Applicative
import Data.Char

data JsonType = JsonBool Bool | JsonString String | JsonMap [(String, JsonType)] | JsonArray [JsonType] | JsonNull | JsonInt Int | JsonFloat Float

 deriving Show

newtype Parser a = Parser { parse :: String -> [(a, String)] }

instance Functor Parser where
 fmap f x = Parser $ \s -> case parse x s of
                              [] -> []
                              [(a, s')] -> [(f a, s')]

instance Applicative Parser where
 pure x = Parser $ \s -> [(x, s)]
 (<*>) f x = Parser $ \s -> case parse f s of
                               [] -> []
                               [(f,s')] ->  case parse x s' of
                                                   [] -> []
                                                   [(a, s'')] -> [(f a, s'')]

instance Monad Parser where
 return = pure
 (>>=) ma f = Parser $ \s -> case parse ma s of
                                    [] -> []
                                    [(a, s')] -> parse (f a) s'


instance Alternative Parser where
 empty = Parser $ \s -> []
 (<|>) a b = Parser $ \s -> let a' = parse a s in
                              case a' of
                                  [] -> parse b s
                                  _  -> a'

parseChar :: Parser Char
parseChar = Parser $ \s -> case s of
                             (x:xs) -> [(x,xs)]
                             []     -> empty

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
parseJson =  parseString <|> parseBool <|> parseNumber <|> parseArray <|> parseMap

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
  return (read (xs))


parseInt :: Parser Int
parseInt = do
 xs <- some digit
 return (read xs)

-- TODO: Implement for float, double aswell.
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


parseFile :: Show a => FilePath -> Parser a -> IO ()
parseFile fileName parser = do
   content <- readFile fileName
   parsedContent <- return (parse parser content)
   putStrLn (show parsedContent)


main :: IO ()
main = do 
 path <- readLn
 parseFile path parseMap










