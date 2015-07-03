      * Author: Dennis Decker Jensen
      * Date: 1 July 2015
      * Purpose: Find the Pythagorean triplet abc, where a+b+c = 1000.
      * Tectonics: cobc -x euler0009.cob
       identification division.
       program-id. euler0009.
      ******************************************************************
       data division.
       working-storage section.
       01  pythagorean-triplet.
           05  side-a                  pic 9999 comp.
           05  side-b                  pic 9999 comp.
           05  side-c                  pic 9999 comp.
       01  pythagorean-triplet-edited.
           05  side-a-edit             pic zz9.
           05  filler                  pic xxxxxx value "**2 * ".
           05  side-b-edit             pic zz9.
           05  filler                  pic xxxxxx value "**2 = ".
           05  side-c-edit             pic zz9.
           05  filler                  pic xxx value "**2".
       01  triplet-product             pic 9(9).
      ******************************************************************
       procedure division.
       find-pythagorean-triplet.
      * Perhaps it would be better to count from 999 by -1 until ...
      *  to avoid the compute steps and the ackward var X from X.
           perform varying side-a from 1 by 1 until side-a > 1000
           compute side-b = side-a + 1
           perform varying side-b from side-b by 1 until side-b > 1000
           compute side-c = side-b + 1
           perform varying side-c from side-c by 1 until side-c > 1000
               if (side-a ** 2) + (side-b ** 2) = (side-c ** 2)
                   if side-a + side-b + side-c = 1000
                       move side-a to side-a-edit
                       move side-b to side-b-edit
                       move side-c to side-c-edit
                       display pythagorean-triplet-edited end-display
                       compute
                           triplet-product = side-a * side-b * side-c
                       end-compute
                       display
                           side-a " * " side-b " * " side-c
                           " = " triplet-product
                       end-display
                       stop run
                   end-if
               end-if
           end-perform
           end-perform
           end-perform.
           stop run.
       end program euler0009.
