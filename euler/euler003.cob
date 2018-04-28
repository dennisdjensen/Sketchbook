      * Copyright 2015 Dennis Decker Jensen
       program-id. prob0003.
       author. Dennis Decker Jensen.
      * Date: 27 June 2015.
      * Purpose: Calculate largest prime factor of 600851475143.
       data division.
       working-storage section.
      * Prime factors 13195 are 5, 7, 13, and 29.
      * Prime factors of 315 are 3, 3, 5 and 7.
      * Prime factors of 600851475143 are ?.
       01  target              pic 9(20) value is 600851475143.
       01  target-edited       pic z(19)9.
       01  n                   pic 9(20).
       01  maybe-n             pic 9(20).
       01  factor-rem          pic 9(20).
       01  i                   pic 9(11).
       01  largest-factor      pic z(10)9 value spaces.

       procedure division.
       let-us-begin.
           move target to n, target-edited.
           display function trim(target-edited leading)
               ":" space with no advancing end-display

           move 2 to i.
           perform find-factor.
           perform find-factor varying i from 3 by 2 until i * i > n.
           if n is greater than 1
               move n to largest-factor.

           display "(largest prime factor) "
                   function trim(largest-factor leading).
           stop run.

       find-factor.
           divide i into n
               giving maybe-n remainder factor-rem
           end-divide
           perform until factor-rem not = zero
               move i to largest-factor
               move maybe-n to n
               divide i into n
                   giving maybe-n remainder factor-rem
               end-divide
           end-perform.
       end program prob0003.
