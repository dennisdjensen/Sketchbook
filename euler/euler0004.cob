      * Author: Dennis Decker Jensen
      * Date: 1 July 2015
      * Purpose: Find the largest palindrome of
      *          the product of two 3-digit numbers.
      * Tectonics: cobc -x euler0004.cob
       identification division.
       program-id. euler0004.
       data division.
       working-storage section.
       01  i                       pic 999.
       01  j                       pic 999.
       01  a                       pic 999.
       01  b                       pic 999.
       01  product                 pic 9(6).
       01  max-product             pic 9(6).
       01  palindrome              pic X(6).

       procedure division.
       start-finding-palindrome.
           perform with test after
                   varying i from 999 by -1 until i = 100
                   after   j from   i by -1 until j = 100
               compute product = i * j
               move product to palindrome
               if palindrome = function reverse(palindrome)
                          and product > max-product
                   move product to max-product
                   move i to a
                   move j to b
               end-if
           end-perform.
           display "Largest palindrome: " a " * " b " = " max-product.
           stop run.
       end program euler0004.
