      * Author: Dennis Decker Jensen
      * Date: 5 July 2015
      * Purpose: Calculate length of lattice paths in 20x20 grid
      * Tectonics: cobc -x euler0015.cbl
      * Method:
      *   This is combinatorics. Use formula from 1991.
      *   In a grid from (0, 0) to (a, b) the number of lattice paths
      *   are the binomial coefficient (a+b)Ca.
      *   nCk = n!/(n-k)!k!
      *   There is a simpler algorithm for computing it, but
      *   I chose to eliminate manually the common factors.
      *   For 20x20 grid this becomes 40C20
      *   = 40!/20!20!
      *   = 40*39*...*21/20!
      *   = 37 * 33 * 31 * 29 * 26 * 5 * 2 * 23 * 21
      *   Function Product is not implemented yet in OpenCobol 1.1,
      *   nor in GnuCOBOL 2.0+ as far as I can tell.
       identification division.
       program-id. euler0015.
      ******************************************************************
       data division.
       working-storage section.
       01  lattice-path-count             pic 9(18) comp value 1.
       01  i                              pic 9     comp.
       01  binomial-coefficent-remains.
           05  remains                    pic X(27) values
                                          "37 33 31 29 26 23 21 05 02".
           05  factors redefines remains  pic X(3) occurs 9 times.
       01  factor-num                     pic 9(2)  comp.
      ******************************************************************
       procedure division.
       main-procedure.
           perform varying i from 1 by 1 until i > 9
               move function numval(factors(i)) to factor-num
               multiply factor-num by lattice-path-count
           end-perform
           display "The number of lattice paths in a 20x20 grid is "
               lattice-path-count.
           stop run.
       end program euler0015.
