C Copyright 2021 Dennis Decker Jensen
C Date: 24-29 May 2021
C Purpose: Erastothene's prime number sieve
C Tectonics: gfortran -ffixed-form -c primesieve.f90

      SUBROUTINE ERASTOTHENE(SIEVE,NHIGH)
         LOGICAL SIEVE(NHIGH)

         DO 10 I=1,NHIGH
   10       SIEVE(I)=.TRUE.
         SIEVE(1)=.FALSE.
         DO 30 I=2,NHIGH
            IF (SIEVE(I)) THEN
               DO 20 J=I+I,NHIGH,I
   20             SIEVE(J)=.FALSE. 
            ENDIF
   30    CONTINUE
      END
