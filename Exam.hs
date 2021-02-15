module Exam where
import Data.List hiding (intersperse)
import Control.Applicative
intersperse :: a -> [a] -> [a]
intersperse x ys@(a:as) | length ys > 1 = a : x : (intersperse x as)
                        | length ys == 1 = [a]
                        | otherwise = []


intersperse' :: a -> [a] -> [a]
intersperse' a xs = take (n*2-1) $ concatMap (\(x,y) -> [x,y]) (zip xs ys) 
 where ys = replicate n a
       n = length xs

intersperse'' :: a -> [a] -> [a]
intersperse'' a xs = tail $ concatMap ( \x -> [a,x] ) xs

allEqual ::Eq a=> [a] -> Bool
allEqual xs = null [ a | a <- xs, b <- xs, a /= b]

allEqual' :: Eq a => [a] -> Bool
allEqual' xs = length (group xs) == 1

inter :: [Char]
inter = intersperse'' ',' "abcdefg"

interspersed :: Eq a => [a] -> Bool
interspersed xs = allEqual' $ map fst $ filter ( \(_,x) -> (mod x 2) == 1) (zip xs [0..])

f1 :: ((a -> a) -> a) -> a
f1 f = f id

f2 :: Either a b -> (a -> b) -> b
f2 (Left x) f = f x
f2 (Right x) _ = x

f3 :: Monad m => m a -> m b -> m (a,b)
f3 ma mb = ma >>= (\a -> mb >>= (\b -> return (a,b)))

f4 :: (a -> c, b -> c -> d) -> (a,b) -> d
f4 (f,g) (a,b) = g b (f a)

f5 :: (a -> b -> a) -> a -> b -> b -> a
f5 f x y z = f (f x y) z


f6 :: [(a,b)] -> [(b,a)]
f6 xs = reverse [(y,x) | (x,y) <- xs]

f7 :: (Foldable f, Num a) => f a -> [a]
f7 = foldMap (\x -> [x + 10, x + 20])

f8 :: Monad m => (Int -> m (Maybe a)) -> m (Maybe a)
f8 f = do
   x <- f 0
   case x of
     Nothing -> f 1
     y       -> return y

type Hash = Int

hashInt :: Int -> Hash
hashInt x = x

class Hashable a where
 hash :: a -> Hash

instance Hashable Int where
 hash = hashInt
instance  (Hashable a, Hashable b) => Hashable (Either a b) where
 hash (Left x) = undefined

data Bit = O | I
 deriving (Eq, Show)
newtype Inflator a = IF { inflate :: [Bit] -> Maybe (a, [Bit]) }

instance Functor Inflator where
 fmap f (IF x) = IF $ \bits -> case x bits of
                                   Nothing -> Nothing
                                   (Just (a,bs)) -> (Just (f a, bs))
                              

instance Applicative Inflator where
 pure x = IF $ \bs -> Just (x,bs)
 (<*>) (IF f) x = IF $ \bs -> case f bs of
                                      Nothing -> Nothing
                                      (Just (f',bs')) -> inflate (fmap f' x) bs'
                                       
 (<*>) (IF f) (IF x) = IF $ \bs -> case f bs of
                                     Nothing -> Nothing
                                     (Just (f',bs')) -> 
                                           case x bs' of
                                              Nothing -> Nothing
                                              (Just (x',bs'')) -> Just (f' x',bs'')


instance Monad Inflator where
 return = pure
 (>>=) (IF a) amb = IF $ \ bits -> case a bits of
                                       Nothing -> Nothing
                                       (Just (a,bits')) -> inflate (amb a) bits'

instance Alternative Inflator where
 empty = IF $ \_ -> Nothing
 a <|> b = IF $ \bits -> case inflate a bits of 
              Nothing -> inflate b bits
              _       -> inflate a bits


data Base = A | T | C | G
 deriving Show

data Tree a = Leaf a | Fork (Tree a) (Tree a)
 deriving Show

compressTree :: Tree Base -> [Bit]
compressTree (Leaf A) = [O,O,O]
compressTree (Leaf C) = [O,O,I]
compressTree (Leaf G) = [O,I,O]
compressTree (Leaf T) = [O,I,I]
compressTree (Fork a b) = [I] ++ compressTree a ++ compressTree b

myTree :: Tree Base
myTree = Fork (Fork (Leaf A) (Leaf T)) (Fork (Leaf C) (Leaf G))

bit :: Bit -> Inflator ()
bit b = IF $ \bits -> case bits of
                         [] -> Nothing
                         (x:xs) -> if b == x then Just ((), xs) else Nothing

base :: Inflator Base
base = IF $ \bits -> case bits of
                       (O:O:xs) -> (Just (A,xs))
                       (O:I:xs) -> (Just (C,xs))
                       (I:O:xs) -> (Just (G,xs))
                       (I:I:xs) -> (Just (T,xs))
                       _        -> Nothing


{- leaf starts with O fork starts with I -}
                   
inflateTree :: Inflator (Tree Base)
inflateTree = (bit O *> pure Leaf <*> base) <|> (bit I *> pure Fork <*> inflateTree <*> inflateTree)

{- use the fusion law to show
 -   foldr k e (xs ++ ys) = foldr k e (foldr k e ys) xs
 -}

{-
 - use the fusion law to show
 -    reverse . concat = foldr (flip (++) . reverse) []
 -    property of reverse = reverse (xs ++ ys) = reverse ys + reverse xs
 -}

{- we first observe the f,g and h
 - f = reverse
 - g = (++)
 - h = flip (++) . reverse
 -}

{- we must show that
 - (1) reverse [] = []
 - (2) reverse (a ++ b) = flip (++) . reverse a (reverse b)
 - (1) by (def. reverse)
 - (2) flip (++) . reverse a (reverse b) = (def. (.)) flip (++) (reverse a) (reverse b) = (def. flip) (++) (reverse a) (reverse b) = (prop. reverse) reverse (a ++ b)
 -}











