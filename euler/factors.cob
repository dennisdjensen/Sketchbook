      * Copyright 2015 Dennis Decker Jensen
       program-id. prob0003.
       author. Dennis Decker Jensen.
      * Date: 27 June 2015.
      * Purpose: Calculate prime factors.
      * Tectonics: cobc -x -g -debug -fdebugging-line prob0003
       data division.
       working-storage section.
      *01  target constant as 15485864.
      *01  target constant as 315.
       01  argument pic x(20).
       01  n        pic 9(20).
       01  i        pic 9(11).
       01  factor   pic z(19)9.

       procedure division.
       arguments.
           accept argument from command-line.
           move function numval(argument) to n.
           if function numval(argument) is not positive
               display "Error: Expected number argument > 1" upon syserr
               stop run.
       pre.
      *    move target to n.
           move n to factor.
           display function trim(factor leading)
               ":" space with no advancing.
       main.
           move 2 to i.
           perform factors.
           perform factors varying i from 3 by 2 until i * i > n.
       last-factor.
           if n is greater than 1
               move n to factor
               display function trim(factor leading)
           else
               display space.

           stop run.
       factors.
           perform one-factor until function mod(n, i) is not zero.
       one-factor.
      *    display "i:" space i
      *    display "n:" space n
           move i to factor
           display function trim(factor leading)
                   space with no advancing
           divide n by i giving n.
       end program prob0003.
