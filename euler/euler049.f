C Copyright 2021 Dennis Decker Jensen
C Date: 9 May 2021
C Purpose: Find prime permutations
C Tectonics: gfortran --std=f95 -ffixed-form -o euler049 euler049.f primetest.f

      PROGRAM EULER049
         DIMENSION IDIGITS(9)
         EXTERNAL PRIME,PERM
         LOGICAL PRIME,PERM
         NDIG(N,M)=MOD(INT(N/10**(M-1)),10)

         DO 20 I=1000,3339
            J=I+3330
            K=J+3330 
C            PRINT *,I,PRIME(I),J,PRIME(J),K,PRIME(K)
            IF (K.LE.9999 .AND. PRIME(I).AND.PRIME(J).AND.PRIME(K)) THEN
               DO 10 L=1,9
  10              IDIGITS(L)=0
               DO 15 L=1,4
                  IDIGITS(NDIG(I,L))=IDIGITS(NDIG(I,L))+1
                  IDIGITS(NDIG(J,L))=IDIGITS(NDIG(J,L))+1
  15              IDIGITS(NDIG(K,L))=IDIGITS(NDIG(K,L))+1
C               PRINT *,I,J,K,IDIGITS
               DO 16 L=1,9
                  IF (MOD(IDIGITS(L),3).NE.0) GOTO 20
  16           CONTINUE
               PRINT '(3I0,'': '',3(I0,1X))',I,J,K,I,J,K
            ENDIF
  20     CONTINUE
      END
