************************************************************************
*
*     Subroutine PPORDER                          Called by: PPCCTEST
*
*     return the index (IX) to put the censored data in sequential order
*
************************************************************************
      SUBROUTINE PPORDER(CENSFLAG,RESPPCC,NOBSC,IX)
*
*     subroutine arguments
*     
      LOGICAL CENSFLAG(*)
      INTEGER*4 NOBSC,IX(*)
      DOUBLE PRECISION RESPPCC(*)
*
*     local vars
*
      INTEGER*4 I,INDX,L(0:NOBSC),P(0:NOBSC),R(0:NOBSC)
*
*     check to see if we have ordered data to begin with; if sorted, set
*     IX and return
*
      DO 50 I=2,NOBSC
         IX(I) = I
         IF (RESPPCC(I) .LT. RESPPCC(I-1)) GOTO 1
         IF (RESPPCC(I).EQ.RESPPCC(I-1) .AND.
     &      (CENSFLAG(I) .AND. .NOT. CENSFLAG(I-1))) GOTO 1
 50   CONTINUE
      IX(1) = 1
      RETURN
*
*     data is not ordered, set IX for sorted residuals
*
 1    L(1) =  0
      R(1) =  0
      P(1) =  0
      DO 10 I=2,NOBSC
         INDX = 1
         L(I) = 0
         R(I) = 0
 20      IF (RESPPCC(I).GT.RESPPCC(INDX) .OR.
     &      ((RESPPCC(I).EQ.RESPPCC(INDX)) .AND.
     &      (CENSFLAG(INDX).OR.(.NOT. CENSFLAG(I) .AND.
     &      .NOT. CENSFLAG(INDX))))) THEN
            IF(R(INDX) .EQ. 0) THEN
               R(INDX) = I
               P(I) = INDX
               GOTO 10
            ELSE
               INDX = R(INDX)
               GOTO 20
            ENDIF
         ELSE
            IF(L(INDX) .EQ. 0) THEN
               L(INDX) = I
               P(I) = INDX
               GOTO 10
            ELSE
               INDX = L(INDX)
               GOTO 20
            ENDIF
         ENDIF
 10   CONTINUE

      INDX = 1

      DO 40 I=1,NOBSC
 30      IF (L(INDX) .EQ. 0) THEN
            IX(I) = INDX
            P(R(INDX)) = P(INDX)
            L(P(INDX)) = R(INDX)
            INDX = P(INDX)
         ELSE
            INDX = L(INDX)
            GOTO 30
         ENDIF
 40   CONTINUE

      RETURN
      END
