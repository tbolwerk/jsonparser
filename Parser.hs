module Main where
import Control.Applicative
import Data.Char


data JsonType = JsonBool Bool | JsonString String | JsonMap [(String, JsonType)] | JsonArray [JsonType] | JsonNull | JsonInt Int
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

whitespace = many (symbol ' ')

parseJson :: Parser JsonType
parseJson = parseString <|> parseBool <|> parseNumber

parseRecord :: Parser (String, JsonType)
parseRecord =  (,) <$> parseStringLiteral <* whitespace <* symbol ':' <* whitespace <*> parseJson
 
parseMap :: Parser JsonType
parseMap = symbol '{' *>  whitespace *> (JsonMap <$> (many record)) <* whitespace <* symbol '}'
   where record = (do
              whitespace
              symbol ','
              whitespace
              parseRecord)
               <|> parseRecord 

parseNumber :: Parser JsonType
parseNumber = do
          mc <- fmap (\x -> if isDigit x then Just x else Nothing) parseChar
          case mc of
             Just digit -> return (JsonInt (digitToInt digit)) -- TODO: currently only parses one integer
             Nothing    -> empty

parseFile :: Show a => FilePath -> Parser a -> IO ()
parseFile fileName parser = do
   content <- readFile fileName
   parsedContent <- return (parse parser content)
   putStrLn (show parsedContent)


main :: IO ()
main = do 
 --path <- readLn
 let path = "simple.json"
 parseFile path parseMap










