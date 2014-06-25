! Copyright (C) 2014 Alexandre Kazmierowski.
! See http://factorcode.org/license.txt for BSD license.
USING: tools.test hanoi kernel ;
IN: hanoi.tests

[ t ] [ 1 2 move "1 vers 2" = ] unit-test
[ f ] [ 1 3 move "1vers3" = ] unit-test
