************************************************************************
*
*     Subroutine OUTSUMMC                Called by: OUTEQN, OUTEQN2
*
*     print summary stats to compare estimates with observations
*
*     local vars
*     ----------
*     XMIN      minimum value
*     P10       10th percentile
*     P25       25th percentile
*     P50       50th percentile
*     P75       75th percentile
*     P90       90th percentile
*     P95       95th percentile
*     P99       99th percentile
*     XMAX      maximum value
*
*     w/ E and O suffix for estimates and observations
*
************************************************************************
      SUBROUTINE OUTSUMMC(NOBSC,EST,OBS,LDOUT)
*
*     subroutine arguments
*
      INTEGER*4 NOBSC,LDOUT
      DOUBLE PRECISION EST(*),OBS(*)
*
*     local vars
*
      DOUBLE PRECISION XMINE,P10E,P25E,P50E,P75E,P90E,P95E,P99E,XMAXE,
     &                 XMINO,P10O,P25O,P50O,P75O,P90O,P95O,P99O,XMAXO
*
*     format statements
*
 1000 FORMAT(1X,A4,3X,8(1PE8.2,1X))
 1100 FORMAT(1X,A7,8(F8.2,1X))
 1200 FORMAT(/,1X,'Est/Obs > 1 indicates overestimation; Est/Obs ',
     &       '< 1 indicates underestimation')
*
*     calculate and print summary stats for estimates, observations, and
*     the ratio of estimated to observed.
*
      CALL PCTL(EST,NOBSC,XMINE,P10E,P25E,P50E,P75E,P90E,P95E,P99E,
     &          XMAXE)
      WRITE(LDOUT,1000) 'Est.',XMINE,P25E,P50E,P75E,P90E,P95E,P99E,XMAXE
      CALL PCTL(OBS,NOBSC,XMINO,P10O,P25O,P50O,P75O,P90O,P95O,P99O,
     &          XMAXO)
      WRITE(LDOUT,1000) 'Obs.',XMINO,P25O,P50O,P75O,P90O,P95O,P99O,XMAXO

      WRITE(LDOUT,1100) 'Est/Obs',XMINE/XMINO,P25E/P25O,P50E/P50O,
     &                  P75E/P75O,P90E/P90O,P95E/P95O,P99E/P99O,
     &                  XMAXE/XMAXO
      WRITE(LDOUT,1200)

      RETURN
      END
