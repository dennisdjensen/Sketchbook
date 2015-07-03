      * Copyright 2015 Dennis Decker Jensen
       program-id. mod-defect.
      * Author: Dennis Decker Jensen.
      * Date: 27 June 2015.
      * Purpose: Calculate largest prime factor of 600851475143.
       data division.
       working-storage section.
      * Prime factors 13195 are 5, 7, 13, and 29.
      * Prime factors of 315 are 3, 3, 5 and 7.
      * Prime factors of 600851475143 are ?.
       01  target-top          pic 9(20) value 600851475143.
      *01  target              pic 9(20) value 327520965353.
       01  target              pic 9(20) value 327429707349.
       01  target-bottom       pic 9(20) value 327429707278.
       01  n                   pic 9(20).
       01  i                   pic 9(20).

       procedure division.
       let-us-begin.
           move target-bottom to i
           display
               "mod(" i ", 71) = "
               function mod(i, 71)
           end-display
           divide i by 71 giving n end-divide
           display
               i " / 71 = " n
           end-display
           move target to i
           display
               "mod(" i ", 71) = "
               function mod(i, 71)
           end-display
           divide i by 71 giving n end-divide
           display
               i " / 71 = " n
           end-display
           display "This happens on upwards by 71." end-display
           stop run.
           perform varying i from 327429707349 by 71
                   until i > 600851475143
               if function mod(i, 71) not = zero
                   display
                       "mod(" i ", 71) = "
                       function mod(i, 71)
                   end-display
                   divide i by 71 giving n end-divide
                   display
                       i " / 71 = " n
                   end-display
               end-if
           end-perform
           stop run.
       end program mod-defect.
