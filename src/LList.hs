module LList where

import Prelude hiding (head, tail, length, map, filter, foldl, zip)

data LList a = Cons a (LList a) | LNil deriving (Show, Eq)

head :: LList a -> a
head l = case l of
  LNil -> error "head of an empty list"
  Cons a _ -> a

tail :: LList a -> LList a
tail l = case l of
  LNil -> LNil
  Cons _ a -> a

length :: LList a -> Int
length LNil = 0
length xs = 1 + length (tail xs)

prepend :: a -> LList a -> LList a
a `prepend` l = Cons a l

append :: LList a -> a -> LList a
LNil `append` a = Cons a LNil
(Cons h t) `append` a = Cons h (t `append` a)

add :: Int -> LList a -> a -> LList a
add 0 l a = Cons a l
add _ LNil _ = error "index out of bounds"
add i (Cons h t) a
   | i > 0 = Cons h (add (i - 1) t a)
   | otherwise = error "index out of bounds"

contains :: (Eq a) => a -> LList a -> Bool
contains _ LNil = False
contains a (Cons h t) = h == a || contains a t

get :: Int -> LList a -> Maybe a
get _ LNil = Nothing
get 0 l = Just (head l)
get i l
  | i < 0 = Nothing
  | otherwise = get (i - 1) (tail l)

index :: (Eq a) => a -> LList a -> Int
index a l = indexRec a l 0 where
  indexRec _ LNil _ = -1
  indexRec b (Cons h t) n
    | h == b = n
    | h /= b && t == LNil = -1
    | otherwise = indexRec a t (n + 1)

map :: (a -> b) -> LList a -> LList b
map _ LNil = LNil
map f (Cons a l) = Cons (f a) (map f l)

filter :: (a -> Bool) -> LList a -> LList a
filter _ LNil = LNil
filter f (Cons a l)
  | f a = Cons a (filter f l)
  | otherwise = filter f l

foldl :: (a -> b -> a) -> a -> LList b -> a
foldl _ a LNil = a
foldl f a (Cons h t) = foldl f (f a h) t

sum :: (Num a) => LList a -> a
sum = foldl (+) 0

prod :: (Num a) => LList a -> a
prod = foldl (*) 1

zip :: LList a -> LList b -> LList (a, b)
zip _ LNil = LNil
zip LNil _ = LNil
zip (Cons hx tx) (Cons hy ty) = Cons (hx, hy) (zip tx ty)

zipWithIndex :: LList a -> LList (a, Int)
zipWithIndex LNil = LNil
zipWithIndex l = zipWithN 0 l
  where
    zipWithN _ LNil = LNil
    zipWithN n (Cons h t) = Cons (h, n) (zipWithN (n + 1) t)

foreach :: (a -> ()) -> LList a -> ()
foreach _ LNil = ()
foreach f (Cons a l) = do
  let _ = f a
  foreach f l

reverse :: LList a -> LList a
reverse = foldl (flip Cons) LNil

toList :: LList a -> [a]
toList LNil = []
toList (Cons a l) = a:toList l

fromList :: [a] -> LList a
fromList = foldr Cons LNil