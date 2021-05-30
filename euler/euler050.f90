C Copyright 2021 Dennis Blondell Decker
C Date: 29 May 2021
C Purpose: Consecutive prime sum
C Tectonics: gfortran --ffixed-form -o euler050 euler050.f90 primesieve.f90

      PROGRAM EULER050
         PARAMETER (MX=1000)
C The gfortran compiler makes the program segmentation fault 
C         PARAMETER (MX=1000000)
         LOGICAL SIEVE(MX)
         INTEGER PRIMES(MX), IARR(MX+1), ICUMARR(MX)
         EQUIVALENCE (IARR(2),ICURARR)
         EXTERNAL ERASTOTHENE

         DATA ICUMARR/MX*0/
         CALL ERASTOTHENE(SIEVE,MX)

         J=0
         DO 10 I=1,MX
            IF (SIEVE(I)) THEN
               J=J+1
               PRIMES(J)=I
            ENDIF
   10    CONTINUE
         MXPRIMES=J

C We treat this problem like a maximum sum subarray problem.
C Use Bentley's quadratic variation of Kandane's Algorithm.
C Instead of the maximum sum, we find the maximum length.
         IARR(1)=0
         DO 20 I=1,MXPRIMES
   20       ICUMARR(I)=ICUMARR(I-1)+PRIMES(I)

C         DO 25 I=1,10
C   25       PRINT *,I,PRIMES(I),ICUMARR(I),ICUMARR(I-1)

         MXSOFAR=0
         K=0
         L=0
         DO 40 I=1,MXPRIMES
            DO 30 J=I,MXPRIMES
               ISUM=ICUMARR(J)-ICUMARR(I-1)
C              ISUM IS SUM OF PRIMES(I:J)
C               IF (I.EQ.1.AND.J.EQ.6)
C     1         PRINT*,ISUM,ICUMARR(J),ICUMARR(I-1),ICUMARR(0),I,J,K,L
               IF (ISUM.LT.MX) THEN
                  IF (SIEVE(ISUM)) THEN
                     IF ((L-K).LT.(J-I)) THEN
                        K=I
                        L=J
                        MXSOFAR=ISUM
                     ENDIF
                  ENDIF
               ENDIF
   30       CONTINUE
   40    CONTINUE

         PRINT 50,MXSOFAR,L-K+1,(PRIMES(I),I=K,L)
   50    FORMAT ('Sum:',I0,' with ',I0,' primes:',1000(I7))
      END
