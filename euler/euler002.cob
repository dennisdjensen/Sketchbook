      * Author: Dennis Decker Jensen
      * Date: 30 June 2015
      * Purpose: Calculate the sum of even fibonacci terms <= 4 mio.
      * Tectonics: cobc -x euler002.cob
       identification division.
       program-id. euler002.
       data division.
       working-storage section.
       01  target-term constant as 4000000.
       01  fibonacci-terms.
           05  term1                   pic 9(7).
           05  term2                   pic 9(7).
           05  hold                    pic 9(7).
       01  sum-of-even-terms           pic 9(20).
       01  sum-of-even-terms-edited    pic z(19)9.

       procedure division.
       main-procedure.
           move 1 to term1.
           move 2 to term2.
           perform fibonacci until term1 > target-term.
           move sum-of-even-terms to sum-of-even-terms-edited.
           display "Sum of even fibonacci terms <= " target-term ": "
                   function trim(sum-of-even-terms-edited leading).
           stop run.

       fibonacci.
           if function mod(term1, 2) = zero
               add term1 to sum-of-even-terms.
           move term2 to hold.
           compute term2 = term1 + term2.
           move hold to term1.

       end program euler002.
