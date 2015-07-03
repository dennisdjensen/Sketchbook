      * Author: Dennis Decker Jensen
      * Date: 01 July 2015
      * Purpose: Calculate sum and square differences for all numbers
      *          1, 2, ..., 100
      * Tectonics: cobc -x euler0006.cob
       identification division.
       program-id. euler0006.
      ******************************************************************
       data division.
       working-storage section.
       01  sums-and-squares.
           05  ws-sum                      pic 9(8).
           05  ws-sum-squared              pic 9(16).
           05  ws-square-sum               pic 9(8).
           05  ws-difference               pic 9(16).
       01  ws-count                        pic 999.
      ******************************************************************
       procedure division.
       find-differences.
           perform varying ws-count from 1 by 1 until ws-count > 100
               add ws-count to ws-sum
               compute ws-square-sum = ws-square-sum + ws-count ** 2
           end-perform.
           compute ws-sum-squared = ws-sum ** 2.
           compute ws-difference = ws-square-sum - ws-sum-squared.
           display "Sum-square difference: " ws-difference.
           stop run.
       end program euler0006.
