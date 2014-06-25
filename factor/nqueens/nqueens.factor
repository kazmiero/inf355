! Copyright (C) 2014 Your name.
! See http://factorcode.org/license.txt for BSD license.
USING: backtrack kernel fry sequences sorting math math.combinatorics grouping arrays ;
IN: nqueens

: hcheck? ( solution -- ? ) inline
    '[ _ amb first ] bag-of natural-sort [ < ] monotonic? ;

: vcheck? ( solution -- ? ) inline
    '[ _ amb second ] bag-of natural-sort [ < ] monotonic? ;

: diffcheck? ( diff -- ? )
    dup [ first ] dip second [ = ] [ -1 * = ] 2bi or ;

: dcheck? ( solution -- ? ) inline
    dup '[ _ amb _ amb [ - ] 2map dup [ first 0 = not ] dip diffcheck? and ] bag-of t swap member? not ;

! a potential solution is an array of n queens positions
! example (3 queens) : { { 1 1 } { 2 1 } { 3 3 } }
 : solution? ( solution -- ? ) inline
    dup [ hcheck? ] dip dup [ vcheck? ] dip dcheck? and and ;

! create the array { 0 1 .. n-1 }
: createList ( n -- n list ) inline
    dup iota [ ] map ;

! arguments : n
!             array { 0 1 .. n-1 }
! return : list of the potential n-queens solutions, which are the combinations of n positions on the chessboard
: candidates ( n list -- candidates ) inline
    dup '[ _ amb _ amb 2array ] bag-of swap all-combinations ;

! to launch the n-queens
! use : 
! n createList candidates [ solution? ] filter
! I couldn't make the function work because of an amb error : 
! "Cannot apply "unsafe-amb" to a run-time computed value
! The optimisation is very poor, so it works until n=5
