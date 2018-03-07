************************************************************************
*
*     Subroutine AMLSTAT                         Called by: CALIBR
*
*     compute AMLE statistics and residuals for the best model
*
*     local vars
*     ----------
*     RESSD    residual standard deviation
*     RESPPCC  residuals for PPCC calculation
*     RESSORT  copy of residuals for use in SORT
*     SUM      sum of YHAT
*     SUMSQ    sum of squares, YHAT
*     SUMSR    sum of squared residuals
*     TAKEN    variable used to assign Z Scores
*
************************************************************************
      SUBROUTINE AMLSTAT(CENSFLAG,IPPCCERR,LDOUT,NOBSC,NOBSCI,NPAR,
     &                   PARAML,PPCC,PLEV,PVAL,RESID,RSQ,SCORR,XLCAL,
     &                   XLIKE,YD,YHAT,YLCAL,YLCAL2,Z)
*
*     subroutine arguments
*
      LOGICAL CENSFLAG(*)
      INTEGER*4 IPPCCERR,LDOUT,NOBSC,NOBSCI,NPAR
      DOUBLE PRECISION PPCC,PLEV,RSQ,SCORR,XLIKE
      DOUBLE PRECISION PARAML(*),PVAL(*),RESID(*),YD(*),YHAT(*),
     &                 YLCAL(*),YLCAL2(*),Z(*)
      DOUBLE PRECISION XLCAL(NOBSCI,*)
*
*     local vars
*
      INTEGER*4 I,K
      DOUBLE PRECISION A,RESSD,SUM,SUMSR,SUMSQ
      DOUBLE PRECISION RESPPCC(NOBSCI),RESSORT(NOBSCI),ZWORK(NOBSCI)
      LOGICAL TAKEN(NOBSCI)
*
*     function declarations
*
      DOUBLE PRECISION CORR,PRED,ZSCORE
*
*     calculate P-values (PVAL) for the AMLE regression coefficients
*
      CALL AMLPVAL(CENSFLAG,LDOUT,NOBSC,NOBSCI,NPAR,PVAL,XLCAL,XLIKE,
     &             YD,YLCAL)
*
*     calculate residuals (observed load - predicted), SUM, and
*     SUMSQ.  For censored observations, use the expected value of
*     observations rather than the observations (5/7/92 Tim Cohn).
*
*     determine RESPPCC, the residual for the PPCC calculation; the
*     detection limit, rather than the expected value, is used to
*     calculate the residual when an observation is censored.
*
*     (Preserve the original YLCAL for use by LADREG and JKNIFE by
*     introducing YLCAL2).  
*
      RESSD = SQRT(PARAML(NPAR+1))
      SUM = 0.D0
      SUMSQ = 0.D0
      DO 10 I=1,NOBSC
         YLCAL2(I) = YLCAL(I)
         YHAT(I) = PRED(NPAR,NOBSCI,I,XLCAL,PARAML)
         IF (CENSFLAG(I)) THEN
            CALL TACIT_CALC((YD(I)-YHAT(I))/RESSD,A)
            YLCAL2(I) = YHAT(I) - RESSD*A
         ENDIF
         RESID(I) = YLCAL2(I) - YHAT(I)
         RESPPCC(I) = YLCAL(I) - YHAT(I)
         SUM = SUM + YHAT(I)
         SUMSQ = SUMSQ + YHAT(I)**2
 10   CONTINUE
*
*     compute R^2 of the load regression and the serial correlation of
*     the residuals
*
      SUMSR = SUMSQ-SUM**2/NOBSC
      RSQ = 100.D0 * SUMSR/(((NOBSC-NPAR)*RESSD**2+SUMSR))
      SCORR = CORR(NOBSC-1,RESID(1),RESID(2))
*
*     make a copy of the residuals vector so that it may be sorted for
*     use in assigning Z-scores, determine Z-scores, and initialize
*     TAKEN
*
      DO 20 I=1,NOBSC
         RESSORT(I) = RESID(I)
         ZWORK(I) = ZSCORE(DBLE(I)/DBLE(NOBSC+1))
         TAKEN(I) = .FALSE.
 20   CONTINUE
*
*     sort residuals
*
      CALL SORT(RESSORT,NOBSC)
*
*     output residuals, Z-scores, predicted values, flow, and decimal
*     time.  Prior to output, assign Z-scores.
* 
*     (Assignment of Z-scores is necessary as Z corresponds to RESSORT,
*     rather than the original vector RESID.  The variable TAKEN is
*     included to make sure that residuals with identical magnitudes
*     are given different Z-scores).
*
      DO 40 I=1,NOBSC
         DO 30 K=1,NOBSC
            IF (RESID(I) .EQ. RESSORT(K) .AND. (.NOT. TAKEN(K))) THEN
               TAKEN(K) = .TRUE.
               Z(I) = ZWORK(K)
               GOTO 40
            ENDIF
 30      CONTINUE
 40   CONTINUE
*
*     compute the probability plot correlation coefficient
*
      CALL PPCCTEST(CENSFLAG,NOBSC,RESPPCC,PPCC,PLEV,IPPCCERR)

      RETURN
      END
