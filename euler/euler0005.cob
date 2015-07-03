      * Author: Dennis Decker Jensen
      * Date: 01 July 2015
      * Purpose: Calculate the smallest positive number that
      *          all of the numbers from 1 to 20 can divide
      *          without remainder.
      * Tectonics: cobc -x euler0005.cob
       IDENTIFICATION DIVISION.
       PROGRAM-ID. euler0005.
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  smallest-divisor                        pic 9(20).
       77  i                                       pic 99.
       01  result.
           05  div-result                          pic 9(20).
           05  rem-result                          pic 9(20).
               88 is-divisible                     value zeroes.
      ******************************************************************
       PROCEDURE DIVISION.
       begin-search.
      * Upper bound: 1*2*...*20 = 20! = 2432902008176640000 (19 digits)
      * Lower bound, and stepping value: 2520, which is the smallest
      *  number evenly divisible by all the numbers 1, 2, ..., 10.
           move 2520 to smallest-divisor.
       next-divisor.
           add 2520 to smallest-divisor.
           if smallest-divisor > 2432902008176640000 then stop run.
           if function mod(smallest-divisor, 20) is not zero
               go to next-divisor.
           perform varying i from 1 by 1 until i > 20
               divide i into smallest-divisor
                   giving div-result
                   remainder rem-result
               if not is-divisible
                   exit perform
           end-perform.
           if is-divisible
               display "Smallest divisible number (1-20): "
                   smallest-divisor
           else
               go to next-divisor.
           STOP RUN.
       END PROGRAM euler0005.
