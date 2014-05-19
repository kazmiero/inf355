module Td2 where

import Data.Ratio

data Prob a = Prob [(a, Ratio Int)]
              deriving (Show)

instance Functor Prob where
  fmap f (Prob l) = Prob $ mapFirst f l

instance Monad Prob where
  return a = sameProbability [a]
  fail _ = Prob []

-- f : (a -> Prob b)
  Prob a >>= f =  Prob $ concat $ map (applyToOne f) a

applyToOne :: (a -> Prob b) -> (a,Ratio Int) -> [(b,Ratio Int)]
applyToOne f (a,p) = let Prob b = f a
                   in mapSecond (*p) b

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
