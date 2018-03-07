************************************************************************
*
*     Subroutine AMLSTAT2                         Called by: CALIBR
*
*     compute AMLE statistics for the concentration model
*
*     local vars
*     ----------
*     RESSD    residual standard deviation
*     SUM      sum of YHATC
*     SUMSQ    sum of squares, YHATC
*     SUMSR    sum of squared residuals
*
************************************************************************
      SUBROUTINE AMLSTAT2(CENSFLAG,LDOUT,NOBSC,NOBSCI,NPAR,PARAMLC,
     &                    PVALC,RSQC,XLCAL,XLIKE,YDC,YHATC,YLCALC)
*
*     subroutine arguments
*
      LOGICAL CENSFLAG(*)
      INTEGER*4 NOBSC,NOBSCI,NPAR,LDOUT
      DOUBLE PRECISION RSQC,XLIKE
      DOUBLE PRECISION PARAMLC(*),PVALC(*),YDC(*),YHATC(*),YLCALC(*)
      DOUBLE PRECISION XLCAL(NOBSCI,*)
*
*     local vars
*
      INTEGER*4 I
      DOUBLE PRECISION RESSD,SUM,SUMSR,SUMSQ
*
*     function declarations
*
      DOUBLE PRECISION PRED
*
*     calculate P-values (PVALC) for the AMLE regression coefficients
*
      CALL AMLPVAL(CENSFLAG,LDOUT,NOBSC,NOBSCI,NPAR,PVALC,XLCAL,XLIKE,
     &             YDC,YLCALC)
*
*     calculate SUM, and SUMSQ.  For censored observations, use the
*     expected value of observations rather than the observations
*     (5/7/92 Tim Cohn).
*
      RESSD = SQRT(PARAMLC(NPAR+1))
      SUM = 0.D0
      SUMSQ = 0.D0
      DO 10 I=1,NOBSC
         YHATC(I) = PRED(NPAR,NOBSCI,I,XLCAL,PARAMLC)
         SUM = SUM + YHATC(I)
         SUMSQ = SUMSQ + YHATC(I)**2
 10   CONTINUE
*
*     compute R^2 of the concentration regression
*
      SUMSR = SUMSQ-SUM**2/NOBSC
      RSQC = 100.D0 * SUMSR/(((NOBSC-NPAR)*RESSD**2+SUMSR))

      RETURN
      END
