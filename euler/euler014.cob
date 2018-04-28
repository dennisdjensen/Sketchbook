      * Author: Dennis Decker Jensen
      * Date: 5 July 2015
      * Purpose: Calculate longest Collatz sequence for number < 1 mio.
      * Tectonics: cobc -x euler014.cob
      * Method: The behaviour is similar to calculation of
      *         a Fibonacci number, i.e. the process grows
      *         exponentially.
      *         Use a table to memoize earlier calculated results.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. euler014.
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  term                            binary-double unsigned.
       01  term-count                      binary-double unsigned.
       01  starting-number                 binary-double unsigned.
       01  collatz-even-test.
           05  collatz-rem                 binary-double unsigned.
           05  collatz-div                 binary-double unsigned.
       01  max-term-count            binary-double unsigned value zero.
       01  starting-number-max-terms binary-double unsigned value zero.
       01  collatz-table.
           05  memo-cur              binary-double unsigned value zero.
           05  memo                        occurs 1 to 1000000 times
                                           depending on memo-cur.
               10 memo-term          binary-double unsigned value zero.
               10 memo-term-count    binary-double unsigned value zero.
      ******************************************************************
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           perform varying starting-number from 1 by 1
                   until starting-number >= 1000000
               move starting-number to term
               move 1 to term-count
               perform until term is equal to 1
                   divide term by 2
                       giving collatz-div
                       remainder collatz-rem
                   end-divide
                   if collatz-rem = 0
                       move collatz-div to term
                   else
                       compute term = term * 3 + 1
                           on size error
                               display
                                   "Too big term on " starting-number
                       end-compute
                   end-if
                   if term <= memo-cur
                       add memo-term-count(term) to term-count
                       exit perform
                   end-if
                   add 1 to term-count
               end-perform
               move starting-number to memo-cur
               move starting-number to memo-term(memo-cur)
               move term-count      to memo-term-count(memo-cur)
               if term-count > max-term-count
                   move term-count to max-term-count
                   move starting-number to starting-number-max-terms
               end-if
           end-perform
           display "Starting number " starting-number-max-terms " has "
                   max-term-count " terms in the Collatz chain.".
           STOP RUN.
       END PROGRAM euler014.
