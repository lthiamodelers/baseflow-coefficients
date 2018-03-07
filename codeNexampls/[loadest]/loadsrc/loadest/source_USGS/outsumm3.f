************************************************************************
*
*     Subroutine OUTSUMM3                            Called by: OUTSUMM
*
*     print summary stats for concentrations estimated by AMLE, MLE,
*     and LAD 
*
*     local vars
*     ----------
*     XMIN      minimum estimated concentration 
*     P10       10th percentile of estimated concentration  
*     P25       25th percentile of estimated concentration  
*     P50       50th percentile of estimated concentration  
*     P75       75th percentile of estimated concentration  
*     P90       90th percentile of estimated concentration  
*     P95       95th percentile of estimated concentration  
*     P99       99th percentile of estimated concentration  
*     XMAX      maximum estimated concentration 
*
************************************************************************
      SUBROUTINE OUTSUMM3(METHSTR,NOBSE,PCONC,CCMAX,CUNITSTR,LDOUT)
*
*     subroutine arguments
*
      INTEGER*4 NOBSE,LDOUT
      DOUBLE PRECISION CCMAX,PCONC(*)
      CHARACTER*4 METHSTR,CUNITSTR
*
*     local vars
*
      DOUBLE PRECISION XMIN,P10,P25,P50,P75,P90,P95,P99,XMAX
*
*     format statements
*
 1400 FORMAT(1X,A4,8(F8.3,1X))
 1500 FORMAT(1X,A4,8(F8.2,1X))
 1600 FORMAT(1X,A4,8(F8.0,1X))

 2000 FORMAT(/,' WARNING: Maximum estimated concentration exceeds ',
     &       'twice the maximum calibration',/,' concentration ',
     &       'of ',F9.3,1X,A4,/)
 2100 FORMAT(/,' WARNING: Maximum estimated concentration exceeds ',
     &       '50000 ',A4,/)
*
*     calculate and print estimated concentration summary stats
*
      CALL PCTL(PCONC,NOBSE,XMIN,P10,P25,P50,P75,P90,P95,P99,XMAX)

      IF (XMIN.LT.(0.1D0).AND.XMAX.LT.(9999.D0)) THEN
         WRITE(LDOUT,1400) METHSTR,XMIN,P25,P50,P75,P90,P95,P99,XMAX
      ELSEIF (XMIN.LT.(1.D0).AND.XMAX.LT.(99999.D0)) THEN
         WRITE(LDOUT,1500) METHSTR,XMIN,P25,P50,P75,P90,P95,P99,XMAX
      ELSE
         WRITE(LDOUT,1600) METHSTR,XMIN,P25,P50,P75,P90,P95,P99,XMAX
      ENDIF
*
*     Print warning message if maximum estimated concentration exceeds
*     twice the maximum observed concentration.  Also print warning
*     message if maximum estimated concentration exceeds 50000
*     concentration units.
*
      IF (XMAX.GT.(2.D0*CCMAX)) WRITE (LDOUT,2000) CCMAX,CUNITSTR
      IF (XMAX .GT. 50000.D0) WRITE(LDOUT,2100) CUNITSTR

      RETURN
      END
