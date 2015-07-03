      * Author: Dennis Decker Jensen
      * Date: 01 July 2015
      * Purpose: Find the 10,000st prime number.
      * Tectonics: cobc -x euler0007.cob
       identification division.
       program-id. euler0007.
      ******************************************************************
       data division.
       working-storage section.
       77  prime-candidate                     pic 9(10) value zeroes.
       01  factor                              pic 9(10).
       01  prime-flag                          pic X.
           88  is-prime                        value "Y"
                                   when set to false "N".
       77  counter                             pic 9(10) value 1.
       77  how-many-primes                     pic 9(10) value 10001.
      ******************************************************************
       procedure division.
       find-primes.
           perform next-prime-candidate varying counter from 1 by 1
                   until counter > how-many-primes.
           stop run.
       next-prime-candidate.
           if prime-candidate = 2
               move 3 to prime-candidate
           else
               add 2 to prime-candidate.
           set is-prime to true.
      *    display "Testing " prime-candidate.
           perform varying factor from 2 by 1
                   until factor * factor > prime-candidate
               if function mod(prime-candidate, factor) is zero
                   set is-prime to false
                   exit perform
               end-if
           end-perform.
           if is-prime
               display "Prime number #" counter ": " prime-candidate
           else
               go to next-prime-candidate.

       end program euler0007.
