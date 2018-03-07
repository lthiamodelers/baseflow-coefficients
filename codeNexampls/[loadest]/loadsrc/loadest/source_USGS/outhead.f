************************************************************************
*
*     Subroutine OUTHEAD                             Called by: ESTLOAD
*
*     output Part II headings and streamflow summary stats
*
*     local vars
*     ----------
*     P's      percentiles of the flow data for calibration
*     CQMAX    maximum calibration streamflow
*     QMIN     minimum calibration streamflow
*
************************************************************************
      SUBROUTINE OUTHEAD(CFLOW,EDATE,EQSTAT,LUNITSTR,NOBSC,NOBSE,WFLAG,
     &                   LDOUT)
*
*     subroutine arguments
*
      LOGICAL WFLAG
      INTEGER*4 NOBSC,NOBSE,LDOUT
      DOUBLE PRECISION EQSTAT(*),CFLOW(*)
      CHARACTER*(*) LUNITSTR
      CHARACTER*8 EDATE(*)
*     
*     local vars
*
      DOUBLE PRECISION CQMAX,QMIN,P10,P25,P50,P75,P90,P95,P99
*
*     function declation
*
      DOUBLE PRECISION MEAN
*
*     format statements
*
 1000 FORMAT(///,1X,70('-'),//,1X,'Constituent Output File Part IIa:',
     &       ' Estimation (test for extrapolation)',//,17X,'Load ',
     &       'Estimates for ',A8,'-',A8,/,1X,70('-'),////,
     &       ' Streamflow Summary Statistics [cfs]',/,1X,35('-'),//,
     &       ' Data    Mean  Minimum 10th Pct 25th Pct   Median',
     &       ' 75th Pct 90th Pct  Maximum',/,1X,75('-'))
 1010 FORMAT(1X,A4,8(F8.0,1X))
 2000 FORMAT(/,' WARNING: The maximum estimation data set ',
     &       'steamflow exceeds the maximum',/,' calibration data set ',
     &       'streamflow.  Load estimates require extrapolation.',//,
     &       ' Maximum Estimation Streamflow : ',1PE11.4,/,
     &       ' Maximum Calibration Streamflow: ',1PE11.4,//)
 2010 FORMAT(/,' The maximum estimation data set steamflow does not ',
     &       'exceed the maximum',/,' calibration data set streamflow.',
     &       ' No extrapolation is required.',//)
 3000 FORMAT(///,1X,70('-'),//,5X,'Constituent Output File Part IIb:',
     &       ' Estimation (Load Estimates)',//,17X,'Load Estimates for',
     &       1X,A8,'-',A8,/,1X,70('-'),////,' Load Estimates ',A9,/,1X,
     &       24('-'))
 3010 FORMAT(' IMPORTANT WARNING:',//,' As noted in Part Ia, the ',
     &       'calibrated model should not be used for load' /,
     &       ' estimation. ALL LOAD ESTIMATES THAT FOLLOW ARE HIGHLY ',
     &       'UNCERTAIN. See',/,' Part Ia for additional details.',/)
*
*     output Part IIa heading; output streamflow summary stats for
*     calibration and estimation data sets.
*
      WRITE(LDOUT,1000) EDATE(1),EDATE(NOBSE)
      CALL PCTL(CFLOW,NOBSC,QMIN,P10,P25,P50,P75,P90,P95,P99,CQMAX)
      WRITE(LDOUT,1010) 'Cal.',MEAN(CFLOW,NOBSC),QMIN,P10,P25,P50,P75,
     &                  P90,CQMAX
      WRITE(LDOUT,1010) 'Est.',EQSTAT(1),EQSTAT(2),EQSTAT(3),EQSTAT(4),
     &                   EQSTAT(5),EQSTAT(6),EQSTAT(7),EQSTAT(10)
*
*     check to see if maximum flow in estimation data set exceeds the
*     maximum flow of the calibration data set -- if so, issue an
*     extrapolation warning.
*
      IF (EQSTAT(10) .GT. CQMAX) THEN
         WRITE(LDOUT,2000) EQSTAT(10),CQMAX
      ELSE
         WRITE(LDOUT,2010)
      ENDIF
*
*     output Part IIb heading
*
      WRITE(LDOUT,3000) EDATE(1),EDATE(NOBSE),LUNITSTR
*
*     issue warning message if the calibrated load model is questionable
*     (based on the load bias and/or the N-S Index; see OUTEQN)
*
      IF (WFLAG) THEN
         CALL JROGER(LDOUT)
         WRITE(LDOUT,3010)
      ENDIF

      RETURN
      END



