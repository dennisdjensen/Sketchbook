C Copyright 2021 Dennis Decker Jensen
C Date: 9 May 2021
C Purpose: Prime number test
C Tectonics: gfortran --std=f95 -ffixed-form -c primetest.f
      LOGICAL FUNCTION PRIME(NUM)
         IROOT = INT(SQRT(REAL(NUM)))

         DO 10 I=2,IROOT
            IF (MOD(NUM,I).EQ.0) THEN
               PRIME=.FALSE.
               RETURN
            ENDIF
   10    CONTINUE
         PRIME=.TRUE.
      END
