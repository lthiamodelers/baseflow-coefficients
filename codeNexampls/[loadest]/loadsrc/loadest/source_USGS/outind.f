************************************************************************
*
*     Subroutine OUTIND                            Called by: ESTLOAD
*
*     output individual estimates of load
*
************************************************************************
      SUBROUTINE OUTIND(EDATE,EFLOW,ETIME,NCENS,NOBSE,PLOADAML,PLOADLAD,
     &                  PLOADMLE,NCONST,LDOUT)
*
*     subroutine arguments
*
      INTEGER*4 NCENS,NCONST,NOBSE,LDOUT
      INTEGER*4 ETIME(*)
      DOUBLE PRECISION EFLOW(*),PLOADAML(*),PLOADMLE(*),PLOADLAD(*)
      CHARACTER*8 EDATE(*)
*
*     local var
*
      INTEGER*4 K
*
*     format statements
*
 2000 FORMAT(//,' Individual Load Estimates',//,35X,
     &       'Loads Estimated by:',//,' Date',5X,'Time',3X,'Flow',7X,
     &       'AMLE',8X,'MLE',9X,'LAD',/,1X,61('-'))
 2500 FORMAT(//,' Individual Load Estimates',//,28X,
     &       'Loads Estimated by:',//,' Date',5X,'Time',3X,'Flow',7X,
     &       'AMLE',8X,'MLE',/,1X,49('-'))
 3000 FORMAT(1X,A8,1X,I4,2X,1PE10.3,3(1X,1PE11.4))
*
*     if requested, output estimated daily loads
*
      IF (NCENS .EQ. 0) THEN
         WRITE(LDOUT+2*NCONST,2000)
         DO 10 K = 1,NOBSE
            WRITE(LDOUT+2*NCONST,3000) EDATE(K),ETIME(K),EFLOW(K),
     &                  PLOADAML(K),PLOADMLE(K),PLOADLAD(K)
 10      CONTINUE
      ELSE
         WRITE(LDOUT+2*NCONST,2500)
         DO 20 K = 1,NOBSE
            WRITE(LDOUT+2*NCONST,3000) EDATE(K),ETIME(K),EFLOW(K),
     &                                 PLOADAML(K),PLOADMLE(K)
 20      CONTINUE
      ENDIF

      RETURN
      END





