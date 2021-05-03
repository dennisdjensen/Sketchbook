C Copyright 2021 Dennis Decker Jensen
C Date: 26 April 2021
C Purpose: Find prime factors in integers
C Tectonics: gfortran --std=f95 -ffixed-form -o euler047 euler047.f primefactors.f

      PROGRAM EULER047
         PARAMETER (MX=10,MXDGS=4)
         DIMENSION IFACS(2,MX)
         DIMENSION IRES(MX,MXDGS,2),NUMS(MXDGS),NFACS(MXDGS)

C         CALL PRIMEFACTORS_TRIALDIV(441, IFACS, MX, N)
C         PRINT 50,441,N,(IFACS(1:2,J),J=1,N)

C         CALL PRIMEFACTORS_TRIALDIV(9974, IFACS, MX, N)
C         PRINT 50,9974,N,(IFACS(1:2,J),J=1,N)
C         CALL PRIMEFACTORS_TRIALDIV(9975, IFACS, MX, N)
C         PRINT 50,9975,N,(IFACS(1:2,J),J=1,N)
C         CALL PRIMEFACTORS_TRIALDIV(9976, IFACS, MX, N)
C         PRINT 50,9976,N,(IFACS(1:2,J),J=1,N)
C         STOP

         M=0
         K=0
         DO 20 I=1,2**32-1
C            CALL PRIMEFACTORS_TRIALDIV(I,IFACS,MX,N)
            CALL PRIMEFACTORS_WHEEL(I,IFACS,MX,N)
C            PRINT 50,I,N,(IFACS(1:2,J),J=1,N)
C            IF (N.EQ.MXDGS) PRINT 50,I,N,(IFACS(1:2,J),J=1,N)

            ISP=1
            DO 05 J=1,N
               ISP=ISP*IFACS(1,J)**IFACS(2,J)
   05       CONTINUE
            IF (ISP.NE.I) THEN
               PRINT *, 'WRONG!',I,'.NE.',ISP
               STOP
            ENDIF

            IF (N.EQ.MXDGS .AND. I.EQ.K+1) THEN
               M=M+1
               NUMS(M)=I
               NFACS(M)=N
               DO 10 L=1,N
                  IRES(L,M,1)=IFACS(1,L)
                  IRES(L,M,2)=IFACS(2,L)
   10          CONTINUE
               IF (M.EQ.MXDGS) GOTO 30
            ELSE
               M=0
            ENDIF
            K=I
   20    CONTINUE
   30    DO 40 I=1,M
            PRINT 50, NUMS(I), NFACS(I),
     1                (IRES(J,I,1:2),J=1,NFACS(I))
   40    CONTINUE
   50    FORMAT (I0,' HAS ',I0,' FACTORS: ',4(I0,'**',I0,' '))
      END
