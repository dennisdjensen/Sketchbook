      ******************************************************************
      * Author: Dennis Decker Jensen
      * Date: 28 June 2015
      * Purpose: Calculate the sum of multiplums of 3 and 5 under 1000
      * Tectonics: cobc -x euler0001.cob
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. euler0001.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  total-sum               pic 9(6).
       77  counter                 pic 9999.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           perform varying counter from 1 by 1 until counter >= 1000
               if function mod(counter, 3) = zero or
                       function mod(counter, 5) = zero
                   add counter to total-sum end-add
               end-if
           end-perform
           display
               "Sum of multiplums of 3 and 5 under 1000: "
               total-sum
           end-display
           STOP RUN.
       END PROGRAM euler0001.
