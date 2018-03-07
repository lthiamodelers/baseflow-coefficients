************************************************************************
*
*     Subroutine PPLOT                              Called by: PPCCTEST
*
*     Compute the plotting positions for the uncensored values when some
*     data are censored, use alpha=0.375. Intended only for the PPCC
*     test. Royston (1993), uses the Blom (0.375) unadjusted for
*     censoring for singly censored data.
*
************************************************************************
      SUBROUTINE PPLOT(AJ,BJ,NDL,PP,IPPCCERR)
*
*     subroutine arguments
*
      INTEGER*4 IPPCCERR,NDL,AJ(*),BJ(*)
      DOUBLE PRECISION PP(*)
*
*     local vars
*
      INTEGER*4 I,J,ISUM
      DOUBLE PRECISION PE(NDL+3)
*
      IF(NDL .EQ. 0 .OR. (NDL .EQ. 1 .AND. AJ(1) .EQ. 0)) THEN
         IPPCCERR = 0
         ISUM = 0
         DO J = (BJ(NDL+1)+1),BJ(NDL+2)
            ISUM = ISUM + 1
            PP(ISUM) = (J - .375D0)/(BJ(NDL+2) + 0.25D0)
         ENDDO
      ELSE
         IPPCCERR = -1
         PE(1) = 1.D0
         PE(NDL+3) = 0.D0
         DO J = (NDL+2),2,-1
            PE(J) = PE(J+1)+(1-PE(J+1))*AJ(J)/(AJ(J)+BJ(J))
         ENDDO
         ISUM = 0
         DO I = 1,(NDL+2)
            IF(AJ(I) .GT. 0) THEN
               DO J = 1, AJ(I)
                  PP(ISUM+J) = (1.D0 - PE(I)) + (PE(I) - PE(I+1)) *
     &                           ((J - .375D0)/(AJ(I) + 0.25D0))
               ENDDO
               ISUM = ISUM + AJ(I)
            ENDIF
         ENDDO
      ENDIF

      RETURN
      END
