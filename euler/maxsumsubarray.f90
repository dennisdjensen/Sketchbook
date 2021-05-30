C Copyright 2021 Dennis Blondell Decker
C Date: 29 May 2021
C Purpose: Maximum Sum Subarray - Kadane's Algorithm
C Tectonics: gfortran -ffixed-form -c maxsumsubarray.f90
C Can handle negative integer sums
C The ISTART and ISTOP are inclusive as always in Fortran

      SUBROUTINE MAXSUMSUBARRAY(NUMBERS,ILEN,ISUM,ISTART,ISTOP)
         DIMENSION NUMBERS(ILEN)

         ISUM=-(2**32-1)
         ISTART=1
         ISTOP=1
         ICURSUM=-(2**32-1)
         DO 10 J=1,ILEN
            IF (ICURSUM.LT.0) THEN
               I=J
               ICURSUM=NUMBERS(J)
            ELSE
               ICURSUM=ICURSUM+NUMBERS(J)
            ENDIF
            IF (ICURSUM.GT.ISUM) THEN
               ISUM=ICURSUM
               ISTART=I
               ISTOP=J
            ENDIF
   10    CONTINUE
      END
