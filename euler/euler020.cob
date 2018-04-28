      * Author: Dennis Decker Jensen
      * Date: 5 July 2015
      * Purpose: The digit sum of factorial 100. A copy of euler016.
      * Tectonics: cobc -x euler016.cob
       identification division.
       program-id. euler016.
       environment division.
       configuration section.
       input-output section.
       file-control.
           select digit-input
               assign to "factorial-digits.txt"
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
           perform calculate-digits through clean-up.
           stop run.

       calculate-digits.
           call "SYSTEM"
               using "dc -e '[d1-d1<!*]s! 100l!xp' " &
                     "| sed -e 's/[^0-9]//g' " &
                     "| tr -d '\n' > factorial-digits.txt"
               returning command-status.
           if command-status not equal zero
               display "Failed to run calculation of factorial digits."
               stop run.

       read-digits.
           open input digit-input
           read digit-input
           close digit-input.

       digit-summation.
           perform varying i from 1 by 1
                   until input-rec(i:1) = space or i > 9000
               move input-rec(i:1) to digit
               add digit to digit-sum
           end-perform.

       show-result.
           display "Digit sum: " digit-sum.

       clean-up.
           call "CBL_DELETE_FILE" using "factorial-digits.txt".
       end program euler016.
