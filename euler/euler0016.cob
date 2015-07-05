      * Author: Dennis Decker Jensen
      * Date: 5 July 2015
      * Purpose: The digit sum of 2 ** 1000
      * Tectonics: cobc -x euler0016.cob
       identification division.
       program-id. euler0016.
       environment division.
       configuration section.
       input-output section.
       file-control.
           select digit-input
               assign to "power-digits.txt"
               organization is sequential.
      ******************************************************************
       data division.
       file section.
       FD  digit-input.
       01  input-rec   pic X(32768).

       working-storage section.
       01  digit-sum   pic 999999 value is zero usage is computational.
       01  digit       pic 9      usage is computational.
       01  i           pic 9999   comp.
       01  command-status pic s9(9).
      ******************************************************************
       procedure division.
       main-procedure.
           perform calculate-digits through show-result.
           stop run.

       calculate-digits.
           call "SYSTEM"
               using "dc -e '2 1000 ^ p' " &
                     "| sed -e 's/[^0-9]//g' " &
                     "| tr -d '\n' > power-digits.txt"
               returning command-status.
           if command-status not equal zero
               display "Failed to run calculation of power digits."
               stop run.

       read-digits.
           open input digit-input
           read digit-input
           close digit-input.

       digit-summation.
      *    display "Power digits:".
      *    display input-rec.
           perform varying i from 1 by 1
                   until input-rec(i:1) = space or i > 9000
               move input-rec(i:1) to digit
               add digit to digit-sum
           end-perform.

       show-result.
           display "Digit sum: " digit-sum
           stop run.
       end program euler0016.
