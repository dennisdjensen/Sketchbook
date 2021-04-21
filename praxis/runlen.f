      PROGRAM RUNLEN
C Run Length Encoding, February 9, 2021
C
C Copyright 2021 Dennis Decker Jensen
C
C https://programmingpraxis.com/2021/02/09/run-length-encoding-2/2/
C
C Tectonics: gfortran -o runlen runlen.f
C Execute: ./runlen
C 
      CHARACTER*(*) STR
      PARAMETER (STR='aaaabbbcca')
      EXTERNAL RUNLEN1, RUNLEN2
      CALL RUNLEN1(STR)
      CALL RUNLEN2(STR)
      END

      SUBROUTINE RUNLEN1(STR)
C Uses a small state machine and a direct counter
      CHARACTER*(*) STR
      CHARACTER C

      IF (LEN(STR).EQ.0) THEN
         WRITE (*, *) '[]'
      ELSE
         WRITE (*, *) '['
         N=1
         C=STR(1:1)
         DO 10 I=2,LEN(STR)
            IF (C.EQ.STR(I:I)) THEN
               N=N+1
            ELSE
               WRITE (*,5) C, N
    5          FORMAT (' ''',A1, ''': ',I0)
               N=1
               C=STR(I:I)
            ENDIF
   10    CONTINUE
         WRITE (*,5) C, N
         WRITE (*,*) ']'
      END IF
      END

      SUBROUTINE RUNLEN2(STR)
C Uses straight structured programming and an indirect counter
      CHARACTER*(*) STR

      J=1
      WRITE (*,*) '['
   10 I=J
      J=I+1
      IF (I.GT.LEN(STR)) GO TO 40
   20 IF (J.GT.LEN(STR)) GO TO 30
      IF (STR(I:I).EQ.STR(J:J)) THEN
         J=J+1
         GO TO 20
      ELSE
         WRITE (*,100) STR(I:I), J-I
         GO TO 10
      END IF
   30 WRITE (*,100) STR(I:I), J-I
   40 WRITE (*,*) ']'
  100 FORMAT (' ''',A1, ''': ',I0)
      END
