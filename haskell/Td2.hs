module Td2 where

import Data.Ratio
import Data.List

data Prob a = Prob [(a, Ratio Int)]
              deriving (Show)

instance Functor Prob where
  fmap f (Prob l) = Prob $ mapFirst f l

instance Monad Prob where
  return a = sameProbability [a]
  fail _ = Prob []
  Prob a >>= f =  Prob $ concat $ map (applyToOne f) a

applyToOne :: (a -> Prob b) -> (a,Ratio Int) -> [(b,Ratio Int)]
applyToOne f (a,p) = let Prob b = f a
                   in mapSecond (*p) b

-- probability
probability :: (Ord a, Eq a) => a -> Prob a -> Ratio Int
probability val (Prob p) = let Prob p1 = canonize (Prob p)
                           in snd (filter (\x -> val == fst x) p1 !! 0)

-- canonize
canonize :: (Ord a, Eq a) => Prob a -> Prob a
canonize (Prob p) = let part = groupBy (\x y -> fst x == fst y) (sort p)
                  in Prob $ map simplify part

simplify :: [(a, Ratio Int)] -> (a, Ratio Int)
simplify l = (fst $ l !! 0, foldl1 (+) (snd $ unzip l)) 

-- map functions
mapFirst :: (a->c) -> [(a,b)] -> [(c,b)] 
mapFirst f l = pairList (map f (firstList l)) (secondList l)

mapSecond :: (b->c) -> [(a,b)] -> [(a,c)] 
mapSecond f l = pairList (firstList l) (map f (secondList l))

firstList = \l -> map fst l
secondList = \l -> map snd l

pairList :: [a] -> [b] -> [(a,b)]
pairList [] [] = []
pairList (ha:ta) (hb:tb) = (ha,hb) : pairList ta tb
---

sameProbability :: [a] -> Prob a
sameProbability list = let len = length list
                           in Prob $ map (\a -> (a,1%len)) list
--
dice :: Prob Int
dice = sameProbability [1,2,3,4,5,6]

double :: Prob Bool
double = do
  x <- dice
  y <- dice
  return $ x == y

pair :: Prob Int
pair = do
  x <- dice
  y <- dice
  return $ x + y
