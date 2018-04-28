      * Author: Dennis Decker Jensen
      * Date: 6 July 2015
      * Purpose: Count the letters of spelled out numbers 1..1000.
      * Tectonics: cobc -x euler017.cob
      * Method: We don't really need to make a human readable number
      *         in text to count the characters, but do it anyway,
      *         because it is good exercise.
      ******************************************************************
       identification division.
       program-id. euler017.
      ******************************************************************
       data division.
       working-storage section.
       01  scores.
           05  score-data              pic X(171) values
                                                     "one      " &
                                                     "two      " &
                                                     "three    " &
                                                     "four     " &
                                                     "five     " &
                                                     "six      " &
                                                     "seven    " &
                                                     "eight    " &
                                                     "nine     " &
                                                     "ten      " &
                                                     "eleven   " &
                                                     "twelve   " &
                                                     "thirteen " &
                                                     "fourteen " &
                                                     "fifteen  " &
                                                     "sixteen  " &
                                                     "seventeen" &
                                                     "eighteen " &
                                                     "nineteen ".
           05  score redefines score-data pic X(9) occurs 19 times.
       01  dekas.
           05  deka-data               pic X(63) values
                                                     "       " &
                                                     "twenty " &
                                                     "thirty " &
                                                     "forty  " &
                                                     "fifty  " &
                                                     "sixty  " &
                                                     "seventy" &
                                                     "eighty " &
                                                     "ninety ".
           05  deka redefines deka-data pic X(7) occurs 9 times.
       01  i                           pic 9999.
       01  j                           pic 99.
       01  number-parts.
           05  ones-num                pic 99.
           05  tens-num                pic 99.
           05  hundreds-num            pic 99.
           05  score-num               pic 99.
       01  spelled-number-parts.
           05  hundreds-txt            pic X(23).
           05  dekas-txt               pic X(10).
           05  scores-txt              pic X(10).
       01  number-txt                  pic X(50).
       01  letters.
           05  letters-txt pic X(26) value "abcdefghijklmnopqrstuvwxyz".
           05  letters-len pic 99.
       01  test-txt        pic X(30) value "one hundred and sixty-one".
       01  letter-count    pic 9(18) packed-decimal value zero.
      ******************************************************************
       procedure division.
       yippie-yay-here-we-go.
           compute letters-len = function length(letters-txt)
           perform varying i from 1 by 1 until i > 1000
               divide i by 100 giving hundreds-num remainder tens-num
               move tens-num to score-num
               divide tens-num by 10 giving tens-num remainder ones-num

               move all spaces to spelled-number-parts
               if hundreds-num = 10
                   move "one thousand" to hundreds-txt
               end-if
               if hundreds-num > 0 and < 10
                   if tens-num > 0 or ones-num > 0
                       move
                           function concatenate(
                            function trim(score(hundreds-num) trailing);
                                       " hundred and ")
                           to hundreds-txt
                   else
                       move
                           function concatenate(
                            function trim(score(hundreds-num) trailing);
                                       " hundred ")
                           to hundreds-txt
                   end-if
               end-if
               if score-num > 0 and < 20
                   move score(score-num) to scores-txt
               end-if
               if score-num >= 20
                   move deka(tens-num) to dekas-txt
                   if ones-num > 0
                       if tens-num = 0
                           move score(ones-num) to scores-txt
                       else
                           move function concatenate("-";
                                                     score(ones-num))
                               to scores-txt
                       end-if
                   end-if
               end-if

               move all spaces to number-txt
               string
                function trim(hundreds-txt) delimited by size
                space                       delimited by size
                function trim(dekas-txt)    delimited by space
                function trim(scores-txt)   delimited by space
                   into number-txt
               end-string
               move function trim(number-txt leading) to number-txt

               perform varying j from 1 by 1 until j > letters-len
                   inspect number-txt
                       tallying letter-count
                       for all letters-txt(j:1)
               end-perform

      *        display i ": " number-txt " = " letter-count end-display
           end-perform
           display "Letter count: " letter-count

           stop run.
       end program euler017.
