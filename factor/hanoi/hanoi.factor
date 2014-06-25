! Copyright (C) 2014 Alexandre Kazmierowski.
! http://factorcode.org/license.txt for BSD license.
USING: kernel math.parser sequences math io ;
IN: hanoi

: move ( a b -- str ) [ number>string ] bi@ " vers " glue ;


: other ( a b -- o )       ! Étant donnés deux plots a et b, renvoie le 3è
    { 1 2 3 } remove remove first ;

: partial ( a b -- a b' )  ! Étant donnés deux plots a et b, renvoie les deux plots a et o
                         ! où o est le résultat de other appliqué sur a et b
    over other ;

: hanoi ( d a n -- )       ! d: plot de départ, a: plot d'arrivée, n: nombre de tours
    dup 0 =
    [ 3drop ]
    [
        3dup 3dup                               ! prepare things ( d a n -- d a n d a n d a n )
        [ partial ] dip 1 - hanoi               ! do hanoi ( d i n-1 -- ) 
        drop move print                         ! print "d to a" ( d a n -- )  
        [ partial swap partial ] dip 1 - hanoi  ! do hanoi ( i a n-1 -- )
    ] if ;
  
