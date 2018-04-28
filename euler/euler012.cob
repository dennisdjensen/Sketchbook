      * Author: Dennis Decker Jensen
      * Date: 27 June 2015.
      * Purpose: Calculate first triangle number
      *          with number of divisors > 500
      * Method: Number of divisors
      *         = the product of exponents + 1 of each prime factor
       program-id. euler012.
       author. Dennis Decker Jensen.
       data division.
       working-storage section.
       01  triangle-num        pic 9(10) usage comp value zero.
       01  triangle-count      pic 9(6)  usage comp.
       01  divisors-count      pic 999   usage comp.
       01  n                   pic 9(10) usage comp.
       01  maybe-n             pic 9(10) usage comp.
       01  factor-rem          pic 9(10) usage comp.
       01  factor-count        pic 9(10) usage comp.
       01  i                   pic 9(10) usage comp.

       procedure division.
       main.
           perform varying triangle-count from 1 by 1
                   until triangle-count > 100000
               add triangle-count to triangle-num
               move triangle-num to n

               move 1 to divisors-count
               move 2 to i
               perform find-divisors-count
               perform find-divisors-count
                   varying i from 3 by 2 until i * i > n
               if n is greater than 1
                    multiply 2 by divisors-count
               end-if
               display triangle-count ": " triangle-num " has "
                       divisors-count " divisors"
               if divisors-count > 500
                   stop run
           end-perform.
           stop run.

       find-divisors-count.
           move zero to factor-count
           divide i into n giving maybe-n remainder factor-rem
           perform until factor-rem not = zero
               add 1 to factor-count
               move maybe-n to n
               divide i into n giving maybe-n remainder factor-rem
           end-perform
           if factor-count > zero
               add 1 to factor-count
               multiply factor-count by divisors-count.
       end program euler012.
