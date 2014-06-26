! Copyright (C) 2014 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: kernel math sequences math.primes math.primes.factors arrays strings fry locals ;
IN: engramme

! ---- Engramme part -------------

: primePower ( prime number -- power )
    swap [ factors ] dip  ! ( {factors} prime )
    '[ 0 [ _ = [ 1 + ] when ] reduce ] call ;
      
:: group-factors-zeros ( n -- factors ) inline
    n group-factors last first primes-upto ! ( n { 2 3 .. biggestPrimeFactor } )
    [ dup [ n primePower ] dip swap 2array ] map ;

: >engramme ( n -- str )
    dup 0 = 
    [ drop "0" ] 
    [
        dup 1 = 
        [ drop "1" ]  
        [
            group-factors-zeros                       ! ( array )
            [ second >engramme ] map                  ! ( power engrammes)
            "" [ append ] reduce                      ! engrammeStr
            "(" swap append ")" append                ! add parenthesis
        ] if
    ] if ;
   
