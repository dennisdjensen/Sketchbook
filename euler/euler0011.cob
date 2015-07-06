      * Author: Dennis Decker Jensen
      * Date: 2 July 2015
      * Purpose: Find largest product of 4 adjacent numbers in a grid.
      * Tectonics: cobc -x euler0011.cob
       identification division.
       program-id. euler0011.
      * What is the greatest product of four adjacent numbers in the
      * same direction (up, down, left, right, or diagonally) in the
      * 20×20 grid?
      *
      * Multiplication is commutative, so we only need checking in
      * one direction: down, right, diag \, and diag /.
      *
      * Diag / (from lower left to upper rigth corner) needs only
      * checking after cell (4, 4), since its length < 4 before that.
       data division.
       working-storage section.
       01  grid-number-table.
           05  grid-cur-row                 pic 99.
           05  grid-cur-col                 pic 99.
           05  grid-max-row                 pic 99 value 20.
           05  grid-max-col                 pic 99 value 20.
           05  grid-data                    pic X(1200) values
       " 08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08" &
       " 49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00" &
       " 81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65" &
       " 52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91" &
       " 22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80" &
       " 24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50" &
       " 32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70" &
       " 67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21" &
       " 24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72" &
       " 21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95" &
       " 78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92" &
       " 16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57" &
       " 86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58" &
       " 19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40" &
       " 04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66" &
       " 88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69" &
       " 04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36" &
       " 20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16" &
       " 20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54" &
       " 01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48".
           05  grid redefines grid-data.
               10  grid-rows                occurs 20 times.
                   15  grid-cols            occurs 20 times.
                       20  grid-cell        pic z99.
       01  product.
           05  adjacent-product             pic 9(8) comp.
           05  max-adjacent-product         pic 9(8) comp value zero.
           05  adjacent-number-cells        occurs 4 times.
               10  number-cell              pic z99.
           05  adjacent-numbers             occurs 4 times.
               10  num                      pic 99 comp.
       01  i                                pic 9  comp.
       procedure division.
       find-largest-product.
           perform varying grid-cur-row from 1 by 1
                   until grid-cur-row > grid-max-row - 4 + 1
               perform varying grid-cur-col from 1 by 1
                       until grid-cur-col > grid-max-col - 4 + 1
diag /*            Diagonal / upwards.
                   if grid-cur-row >= 4
                       perform varying i from 0 by 1 until i > 3
                           move grid-cell(grid-cur-row - i,
                                          grid-cur-col + i)
                             to number-cell(i + 1)
                       end-perform
                       perform check-max-product
                   end-if
diag \*            Diagonal \ downwards.
                   perform varying i from 0 by 1 until i > 3
                       move grid-cell(grid-cur-row + i,
                                      grid-cur-col + i)
                         to number-cell(i + 1)
                   end-perform
                   perform check-max-product
vert  *            Vertical downwards.
                   perform varying i from 0 by 1 until i > 3
                       move grid-cell(grid-cur-row + i,
                                      grid-cur-col)
                         to number-cell(i + 1)
                   end-perform
                   perform check-max-product
hori  *            Horizontal rigthwards.
                   perform varying i from 0 by 1 until i > 3
                       move grid-cell(grid-cur-row,
                                      grid-cur-col + i)
                         to number-cell(i + 1)
                   end-perform
                   perform check-max-product
               end-perform
           end-perform.
           display "Maximum product of 4 adjacent numbers: "
                   max-adjacent-product.
           stop run.

       check-max-product.
           perform varying i from 1 by 1 until i > 4
               move number-cell(i) to num(i)
           end-perform
           move 1 to adjacent-product
           perform varying i from 1 by 1 until i > 4
               multiply num(i) by adjacent-product
           end-perform
           if adjacent-product > max-adjacent-product
               move adjacent-product to max-adjacent-product.
       end program euler0011.
