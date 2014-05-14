module Peano where

data Peano = Zero
             | Succ Peano
               deriving (Show,Read,Eq)

pplus :: Peano -> Peano -> Peano
pplus a Zero = a
pplus a (Succ b) = pplus (Succ a) b

pmoins :: Peano -> Peano -> Peano
pmoins a Zero = a
pmoins Zero _ = error "no negatives"
pmoins (Succ a) (Succ b) = pmoins a b  

pfois :: Peano -> Peano -> Peano
pfois Zero _ = Zero
pfois _ Zero = Zero
pfois a (Succ Zero) = a
pfois a (Succ b) = pplus a (pfois a b) 

instance Num Peano where
  a + b = pplus a b
  a - b = pmoins a b
  a * b = pfois a b

  signum Zero = Zero
  signum (Succ _) = Succ Zero

  abs a = a

  fromInteger 0 = Zero
  fromInteger a = if a<0
                     then error "no negatives"
                     else Succ (fromInteger (a-1))

--instance Show Peano where
--  show Zero = "Z"
--  show (Succ p) = "S(" ++ show p ++ ")"

instance Ord Peano where
  Zero <= _ = True
  _ <= Zero = False
  Succ a <= Succ b = a <= b

instance Enum Peano where
  toEnum a = fromInteger (toInteger a) :: Peano

  fromEnum Zero = 0
  fromEnum (Succ p) = fromEnum p + 1

instance Real Peano where
  toRational Zero = 0
  toRational (Succ p) = toRational p + 1

instance Integral Peano where
  quotRem _ Zero = error "divide by zero"
  quotRem a b = if a<b
                   then (Zero,a)
                   else let (q,r) = quotRem (a-b) b
                        in (Succ q,r)
  divMod = quotRem
  
  toInteger Zero = 0
  toInteger (Succ p) = toInteger p + 1
                
