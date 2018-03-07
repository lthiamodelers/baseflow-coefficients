************************************************************************
*
*     Subroutine PPARRANGE                         Called by: PPCCTEST
*
*     arrange the censoring levels so that the plotting positions can be 
*     computed
*
************************************************************************
      SUBROUTINE PPARRANGE(RESPPCC,CENSFLAG,NOBSC,AJ,BJ,CJ,NDL,Y,NUNCEN,
     &                     IX)
*
*     subroutine arguments
*
      LOGICAL CENSFLAG(*)
      INTEGER*4 NDL,NOBSC,NUNCEN,AJ(*),BJ(*),CJ(*),IX(*)
      DOUBLE PRECISION RESPPCC(*),Y(*)
*
*     local vars
*
      INTEGER*4 I,J
      DOUBLE PRECISION CDL,PLUSINF,DL(NDL+2)
      DATA PLUSINF/1.D300/
*
*     aggregate the DL's and construct the sorted Y vector
*
      NUNCEN = 0
      DL(1) = -PLUSINF
      CDL = DL(1)
      J = 1
      DO I=1,NOBSC
         IF(.NOT. CENSFLAG(IX(I))) THEN
            NUNCEN = NUNCEN + 1
            Y(NUNCEN) = RESPPCC(IX(I))
         ELSE
            IF(RESPPCC(IX(I)) .GT. CDL) THEN
               J = J + 1
               DL(J) = RESPPCC(IX(I))
               CDL = RESPPCC(IX(I))
            ENDIF
         ENDIF
      ENDDO
      DL(J+1) = PLUSINF
*
*     accumulate the indexes
*
      DO J=1,(NDL + 1)
         AJ(J) = 0
         BJ(J) = 0
         CJ(J) = 0
         DO I=1, NOBSC
            IF(CENSFLAG(IX(I))) THEN
               IF(RESPPCC(IX(I)) .LE. DL(J)) BJ(J) = BJ(J) + 1
               IF(RESPPCC(IX(I)) .EQ. DL(J)) CJ(J) = CJ(J) + 1
            ELSE
               IF(DL(J).LE.RESPPCC(IX(I)).AND.RESPPCC(IX(I)).LT.DL(J+1))
     &            AJ(J) = AJ(J) + 1
               IF(RESPPCC(IX(I)) .LT. DL(J)) BJ(J) = BJ(J) + 1
            ENDIF
         ENDDO
      ENDDO
      AJ(NDL + 2) = 0
      BJ(NDL + 2) = NOBSC
      CJ(NDL + 2) = 0

      RETURN
      END
