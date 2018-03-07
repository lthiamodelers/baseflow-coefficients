************************************************************************
*
*     Subroutine OUTSUMM2                            Called by: OUTSUMM
*
*     print summary stats for loads estimated by AMLE, MLE, and LAD 
*
*     local vars
*     ----------
*     XMIN      minimum estimated load
*     P10       10th percentile of estimated load 
*     P25       25th percentile of estimated load 
*     P50       50th percentile of estimated load 
*     P75       75th percentile of estimated load 
*     P90       90th percentile of estimated load 
*     P95       95th percentile of estimated load 
*     P99       99th percentile of estimated load 
*     XMAX      maximum estimated load
*
************************************************************************
      SUBROUTINE OUTSUMM2(METHSTR,NOBSE,PLOAD,LDOUT)
*
*     subroutine arguments
*
      INTEGER*4 NOBSE,LDOUT
      DOUBLE PRECISION PLOAD(*)
      CHARACTER*4 METHSTR
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
*
*     calculate and print estimated load summary stats
*
      CALL PCTL(PLOAD,NOBSE,XMIN,P10,P25,P50,P75,P90,P95,P99,XMAX)

      IF (XMIN.LT.(0.1D0).AND.XMAX.LT.(9999.D0)) THEN
         WRITE(LDOUT,1400) METHSTR,XMIN,P25,P50,P75,P90,P95,P99,XMAX
      ELSEIF (XMIN.LT.(1.D0).AND.XMAX.LT.(99999.D0)) THEN
         WRITE(LDOUT,1500) METHSTR,XMIN,P25,P50,P75,P90,P95,P99,XMAX
      ELSE
         WRITE(LDOUT,1600) METHSTR,XMIN,P25,P50,P75,P90,P95,P99,XMAX
      ENDIF

      RETURN
      END
