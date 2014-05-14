module Peano where

data Peano = Zero
             | Succ Peano

pplus :: Peano -> Peano -> Peano
pplus a Zero = a
pplus a (Succ b) = pplus (Succ a) b

pmoins :: Peano -> Peano -> Peano
pmoins a Zero = a
pmoins Zero b = error "no negatives"
pmoins (Succ a) (Succ b) = pmoins a b  

pfois :: Peano -> Peano -> Peano
pfois a Zero = 0
pfois a (Succ b) = pfois (pplus a a) b

instance Num Peano where
  a + b = pplus a b
  a - b = pmoins a b
  a * b = pfois a b

  signum Zero = Zero
  signum (Succ a) = Succ Zero

  abs a = a

  fromInteger a = if a<0
                     then error "no negatives"
                     else fromInteger 0 = Zero

  fromInteger 0 = Zero
  fromInteger (a+1) = Succ (fromInteger a)
