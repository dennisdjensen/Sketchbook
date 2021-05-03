C Copyright 2021 Dennis Decker Jensen
C Date: 26 April 2021
C Modified: 2 May 2021
C Purpose: Find prime factors in integers
C Tectonics: gfortran --std=f95 -ffixed-form -c primefactors.f

      SUBROUTINE PRIMEFACTORS_TRIALDIV(NUM,IFACS,MXFACS,NFACS)
C https://en.wikipedia.org/wiki/Trial_division
         DIMENSION IFACS(2, MXFACS)

         N=NUM
         DO 90 I=1,MXFACS
            IFACS(1,I)=0
            IFACS(2,I)=0
   90    CONTINUE

         I=1
         IFACS(1,I)=2
  100    IF (MOD(N,2).EQ.0) THEN
            IFACS(2,I)=IFACS(2,I)+1
            N=N/2
            GOTO 100
         ENDIF

         K=3
  300    IF (K*K.GT.N) GOTO 500
            IF (IFACS(2,I).NE.0) THEN
               I=I+1
               IF (I.EQ.MXFACS) THEN
                  PRINT*, 'OVERFLOW: '//
     1                    'INCREASE MXFACS '//
     2                    'TO ACCOMODATE NO PRIME FACTORS'
                  NFACS=0
                  RETURN
               ENDIF
            ENDIF
            IFACS(1,I)=K
  400       IF (MOD(N,K).EQ.0) THEN
               IFACS(2,I)=IFACS(2,I)+1
               N=N/K
               GOTO 400
            ELSE
               K=K+2   
            ENDIF
            GOTO 300
  500    IF (N.NE.1) THEN
C            IF (NUM.EQ.9974) PRINT *,'K',K,'N',N,'F',IFACS
            IF (N.EQ.IFACS(1,I)) THEN
               IFACS(2,I)=IFACS(2,I)+1
            ELSE
               IF (IFACS(2,I).NE.0) THEN
                   I=I+1
                   IF (I.EQ.MXFACS) THEN
                      PRINT *,'OVERFLOW, MISSING LAST FACTOR', N
                   ENDIF
               ENDIF
               IFACS(1,I)=N
               IFACS(2,I)=1
            ENDIF
         ENDIF
         NFACS=I
      END

      SUBROUTINE PRIMEFACTORS_WHEEL(NUM, IFACS, MXFACS, NFACS)
C https://en.wikipedia.org/wiki/Wheel_factorization
         DIMENSION IFACS(2,MXFACS)
         DIMENSION INC(8)
C Wheel:          7, 11, 13, 17, 19, 23, 29, 31
         DATA INC/    4,  2,  4,  2,  4,  6,  2, 6/

         I=1
         J=1
         K=7
         N=NUM

         DO 90 L=1,MXFACS
            IFACS(1,L)=0
            IFACS(2,L)=0
   90    CONTINUE

         DO WHILE (MOD(N,2).EQ.0)
            IFACS(2,I)=IFACS(2,I)+1         
            N=N/2
         ENDDO
         IF (IFACS(2,I).GT.0) THEN
            IFACS(1,I)=2
            I=I+1
         ENDIF

         DO WHILE (MOD(N,3).EQ.0)
            IFACS(2,I)=IFACS(2,I)+1
            N=N/3
         ENDDO
         IF (IFACS(2,I).GT.0) THEN
            IFACS(1,I)=3
            I=I+1
         ENDIF

         DO WHILE (MOD(N,5).EQ.0)
            IFACS(2,I)=IFACS(2,I)+1
            N=N/5
         ENDDO
         IF (IFACS(2,I).GT.0) THEN
            IFACS(1,I)=5
            I=I+1
         ENDIF

         DO WHILE (K*K.LE.N)
C            IF (NUM.EQ.646)
C     1         PRINT *,'K',K,'N',N,'I',I,'F',IFACS
            IF (MOD(N,K).EQ.0) THEN
               IFACS(2,I)=IFACS(2,I)+1
               N=N/K
            ELSE
               IF (IFACS(2,I).GT.0) THEN
                  IFACS(1,I)=K
                  I=I+1
               ENDIF
               K=K+INC(J)
               IF (J.LT.8) THEN
                  J=J+1
               ELSE
                  J=1
               ENDIF
            ENDIF
         ENDDO
         IF (IFACS(2,I).GT.0) THEN
            IFACS(1,I)=K
            I=I+1
         ENDIF
C         PRINT *,'K',K,'N',N,'I',I,'F',IFACS
         IF (N.NE.1) THEN
            IF (N.EQ.IFACS(1,I-1)) THEN
               IFACS(2,I-1)=IFACS(2,I-1)+1
            ELSE
               IFACS(1,I)=N
               IFACS(2,I)=1
               I=I+1
            ENDIF
         ENDIF 
C         PRINT *,'K',K,'N',N,'I',I,'F',IFACS 
         NFACS=I-1
      END
