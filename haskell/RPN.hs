module RPN where

import Prelude hiding (drop)

type Stack = [Int]

type Operator = Stack -> Stack

plus :: Stack -> Stack
plus [] = []
plus [x] = [x]
plus (h : (ht :tt)) = (h+ht : tt) 

moins :: Stack -> Stack
moins [] = []
moins [x] = [x]
moins (h : (ht :tt)) = (ht-h : tt) 

fois :: Stack -> Stack
fois [] = []
fois [x] = [x]
fois (h : (ht :tt)) = (h*ht : tt) 

divi :: Stack -> Stack
divi [] = []
divi [x] = [x]
divi (h : ht : tt) = (div ht h : tt)

dup :: Stack -> Stack
dup [] = []
dup (h : t) = (h:(h :t))

swap :: Stack -> Stack
swap [] = []
swap [x] = [x]

swap (h :(ht : tt)) = (ht: (h :tt))

drop :: Stack -> Stack
drop [] = []
drop (_ : t) = t

depth :: Stack -> Stack
depth l = (length l : l)

genericIndex :: Stack -> Int -> Int
genericIndex [] _ = error "Invalid Input"
genericIndex (h:_) 0 = 0
genericIndex (h:t) n = genericIndex t (n-1)

pick :: Stack -> Stack 
pick (h:t) = (genericIndex t h : t)

parseOp :: String -> Operator
parseOp "+" = plus
parseOp "-" = moins
parseOp "*" = fois
parseOp "/" = divi
parseOp "dup" = dup
parseOp "swap" = swap
parseOp "drop" = drop
parseOp "depth" = depth
parseOp "pick" = pick

eval :: Stack -> [Operator] -> Stack
eval s [] = s
eval s (op : opt) = eval (op s) opt
