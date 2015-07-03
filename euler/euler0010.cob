      * Author: Dennis Decker Jensen
      * Date: 01 July 2015
      * Purpose: Find the sum of all the primes below two million.
      * Tectonics: cobc -x euler0010.cob
       identification division.
       program-id. euler0010.
      ******************************************************************
       data division.
       working-storage section.
       77  prime-candidate                     pic 9(10) value zeroes.
       01  factor                              pic 9(10).
       01  prime-flag                          pic X.
           88  is-prime                        value "Y"
                                   when set to false "N".
       77  counter                             pic 9(10) value 1.
       77  prime-ceiling                       pic 9(10) value 2000000.
       77  sum-of-primes                       pic 9(20) value zero.
      ******************************************************************
       procedure division.
       find-primes.
           perform next-prime-candidate varying counter from 1 by 1
                   until prime-candidate > prime-ceiling.
           subtract prime-candidate from sum-of-primes.
           display "Sum of prime numbers below 2e6: " sum-of-primes.
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
               add prime-candidate to sum-of-primes
           else
               go to next-prime-candidate.

       end program euler0010.
