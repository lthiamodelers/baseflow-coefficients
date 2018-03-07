************************************************************************
*
*     Subroutine OUTRES                                Called by: CALIBR
*
*     write residual output file; calculate sums for bias diagnostics
*
************************************************************************
      SUBROUTINE OUTRES(CCONC,CCONCAML,CDATETIM,CENSFLAG,CFLOW,CLOAD,
     &                  CLOADAML,DECTIME,LDOUT,LFACTOR,NCONST,NOBSC,
     &                  RESID,SSECONC,SSELOAD,SSQCOBS,SSQLOBS,SUMCAML,
     &                  SUMCOBS,SUMLAML,SUMLOBS,YHAT,YHATC,YLCAL,YLCALC,
     &                  Z)
*
*     subroutine arguments
*
      LOGICAL CENSFLAG(*)
      INTEGER*4 LDOUT,NCONST,NOBSC
      DOUBLE PRECISION LFACTOR,SSECONC,SSELOAD,SSQCOBS,SSQLOBS,SUMCAML,
     &                 SUMCOBS,SUMLAML,SUMLOBS
      DOUBLE PRECISION CCONC(*),CCONCAML(*),CFLOW(*),CLOAD(*),
     &                 CLOADAML(*),DECTIME(*),RESID(*),YHAT(*),YHATC(*),
     &                 YLCAL(*),YLCALC(*),Z(*)
      CHARACTER*13 CDATETIM(*)
*
*     local var
*
      INTEGER*4 I
*
*     format statements
*
 1000 FORMAT('#',/,'#  Residual output file',/,'#',/,'#  notes',/,
     &       '#  -----',/,'#  DTIME',5X,
     &       'decimal time minus "center" of decimal time',/,
     &       '#  LN(CFLOW) natural log of (uncentered) streamflow',/,
     &       '#  F',9X,'flag indicating observation is censored (C) or',
     &       ' uncensored (U)',/,'#  CCONC     observed concentration ',
     &       'for F=U; 1/2 of the observed concentration for F=C',/,
     &       '#  CCONCAML  estimated concentration',/,
     &       '#  YHATC     estimated natural log of concentration',/,
     &       '#  CLOAD     observed load for F=U; 1/2 of the observed ',
     &       'load for F=C (units dependent on ULFLAG)',/,
     &       '#  CLOADAML  estimated load (units dependent on ULFLAG)',
     &       /,'#  YHAT      estimated natural log of load (where load',
     &       ' is in kg/d)',/,'#  RESID     difference between ',
     &       'observed and estimated values of log load (or log ',
     &       'concentration)',/,'#  Z         z-score for residual',/,
     &       '#',/,'#',/,'#DATE    TIME  DTIME        LN(CFLOW)   F ',
     &       'CCONC       CCONCAML     YHATC       CLOAD       ',
     &       'CLOADAML     YHAT         RESID        Z',/,'#')
 1010 FORMAT(A13,2(1X,1PE12.5),1X,A1,
     &       2(1X,1PE11.5,1X,1PE11.5,1X,1PE12.5),2(1X,1PE12.5))
*
*     initialize sums
*
      SUMCOBS = 0.D0
      SUMCAML = 0.D0
      SUMLOBS = 0.D0
      SUMLAML = 0.D0
      SSQLOBS = 0.D0
      SSQCOBS = 0.D0
      SSELOAD = 0.D0
      SSECONC = 0.D0
*
*     calculate observed concentrations and loads in real space, where
*     loads are in the user-requested units (if the data is censored,
*     calculate the observed values using 1/2 the detection limit).
*
*     output residuals, observed and estimated values, and other
*     quantities for use in making residual plots. 
*
*     calculate sums that are later used to calculate bias diagnostics.
*
      WRITE(LDOUT+NCONST,1000) 

      DO 10 I=1,NOBSC
         IF (CENSFLAG(I)) THEN
            CCONC(I) = 0.5D0*EXP(YLCALC(I))
            CLOAD(I) = 0.5D0*EXP(YLCAL(I))*LFACTOR
            WRITE(LDOUT+NCONST,1010) CDATETIM(I),DECTIME(I),
     &                               LOG(CFLOW(I)),'C',CCONC(I),
     &                               CCONCAML(I),YHATC(I),CLOAD(I),
     &                               CLOADAML(I),YHAT(I),RESID(I),Z(I)
         ELSE
            CCONC(I) = EXP(YLCALC(I))
            CLOAD(I) = EXP(YLCAL(I))*LFACTOR
            WRITE(LDOUT+NCONST,1010) CDATETIM(I),DECTIME(I),
     &                               LOG(CFLOW(I)),'U',CCONC(I),
     &                               CCONCAML(I),YHATC(I),CLOAD(I),
     &                               CLOADAML(I),YHAT(I),RESID(I),Z(I)
         ENDIF

         SUMCOBS = SUMCOBS + CCONC(I)
         SUMCAML = SUMCAML + CCONCAML(I)
         SUMLOBS = SUMLOBS + CLOAD(I)
         SUMLAML = SUMLAML + CLOADAML(I)
         SSQLOBS = SSQLOBS + CLOAD(I)*CLOAD(I)
         SSQCOBS = SSQCOBS + CCONC(I)*CCONC(I)
         SSELOAD = SSELOAD + (CLOAD(I)-CLOADAML(I))**2
         SSECONC = SSECONC + (CCONC(I)-CCONCAML(I))**2

 10   CONTINUE

      RETURN
      END
