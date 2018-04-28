      * Author: Dennis Decker Jensen
      * Date: 7 July 2015
      * Purpose: Calculate maximum path sum
      * Tectonics: cobc -x euler018.cob
      * Method: Dynamic Programming with only 2 "vectors".
      ******************************************************************
       identification division.
       program-id. euler018.
      ******************************************************************
       environment division.
       input-output section.
       file-control.
           select optional triangle-input
               assign to triangle-filepath
               organization is line sequential
               file status is triangle-input-status.
      ******************************************************************
       data division.
       file section.
       fd triangle-input.
       01  triangle-line           pic X(30000).

       working-storage section.
       01  newline constant as 11.
       01  triangle-filepath       pic X(80).
       01  default-triangle-filepath   pic X(80)
                                   value "p018_triangle.txt".
       01  triangle-input-status       pic XX.
           88  triangle-input-OK       value "00".
           88  end-of-triangle-input   value "10".
       01  line-num                    pic 9999 value zero.
       01  maximum-path-sum            pic 9(8) usage comp value zero.
       01  triangle-row-data.
           05  row                     pic z99 occurs 1 to 3000 times.
       01  triangle.
           05  row-len-1               pic 9999 comp value 1.
           05  row-len-2               pic 9999 comp value 1.
           05  row-num-1               pic 9(8) usage comp value zero
                                               occurs 1 to 3000 times.
           05  row-num-2               pic 9(8) usage comp value zero
                                               occurs 1 to 3000 times.
       01  i                           pic 9999.
      ******************************************************************
       procedure division.
       000-main.
           accept triangle-filepath from command-line.
           if triangle-filepath equals spaces
               move default-triangle-filepath to triangle-filepath.
           display "File: " function trim(triangle-filepath trailing).

           open input triangle-input.
           perform 100-calculate-maximum-path-sum.
           close triangle-input.

           perform 200-find-maximum-path-sum.
           display "Triangle " line-num " high, has maximum path sum "
               maximum-path-sum.
           stop run.

       100-calculate-maximum-path-sum.
           perform 110-read-row.
           perform until not triangle-input-OK
      *        perform 140-display-row-2
      * Left side of triangle:
               add row-num-1(1) to row-num-2(1)
                   on size error
                       display "You need a bigger triangle element!"
               end-add
      * Right side of triangle:
               add row-num-1(row-len-1) to row-num-2(row-len-2)
                   on size error
                       display "You need a bigger triangle element!"
               end-add
      * Middle elements of triangle:
               perform varying i from 2 by 1 until i > row-len-2 - 1
                   compute
                       row-num-2(i) = function max(
                           row-num-1(i - 1) + row-num-2(i)
                           row-num-1(i)     + row-num-2(i))
                       on size error
                           display "You need a bigger triangle element!"
                   end-compute
               end-perform
               perform 130-move-row-2-to-row-1
      *        perform 140-display-row-2
               perform 110-read-row
           end-perform.
           if not end-of-triangle-input
               display "Unexpected file status: " triangle-input-status.

       110-read-row.
           read triangle-input
               not at end
                   add 1 to line-num
                   string space triangle-line into triangle-row-data
                   perform 120-move-data-to-row-2.

       120-move-data-to-row-2.
           move line-num to row-len-2.
           perform varying i from 1 by 1 until i > row-len-2
               move row(i) to row-num-2(i)
           end-perform.

       130-move-row-2-to-row-1.
           move row-len-2 to row-len-1.
           perform varying i from 1 by 1 until i > row-len-1
               move row-num-2(i) to row-num-1(i)
           end-perform.

       140-display-row-2.
           display row-len-2 ":" with no advancing.
           perform varying i from 1 by 1 until i > row-len-2
              display space row-num-2(i) with no advancing
           end-perform.
           display space.

       200-find-maximum-path-sum.
      * OpenCobol/GnuCOBOL does not appear to support
      * the notation Function Max(row-num-2(all)).
           perform varying i from 1 by 1 until i > row-len-2
               compute maximum-path-sum =
                   function max(maximum-path-sum; row-num-2(i))
           end-perform.
       end program euler018.
