************************************************************************
*
*     Subroutine PPCCTEST                            Called by: AMLSTAT
*
*     Probability Plot Correlation Coefficient normality test for left-
*     censored data (Royston, 1993). Extended to multiply censored data
*     by Millard in the Environmental Stats for S+ library.  This
*     subroutine and several subordinate subroutines were provided by
*     Dave Lorenz (February, 2013); modified by R. Runkel (March, 2013).
*
*     This procedure is similar to the PPCC test for uncensored data.
*     It computes the correlation coefficient between the uncensored
*     quantiles of the data and the corresponding normal quantiles.
*     The test statistic is the correlation. The attained p-values are
*     empirical and only approximate for multiply-censored data.
*
*     The null hypothesis that the data are normally distributed is
*     rejected if the value of the test statistic is less than the
*     critical value for the number of observations and censoring.
*
*     Note that IPPCCERR is 0 for the singly censored test and -1 for
*     the multiply censored test. The attained p-value for the multiply
*     censored test is only approximate. IPPCCERR > 0 indicates an 
*     error and the results are not valid.
*
************************************************************************
      SUBROUTINE PPCCTEST(CENSFLAG,NOBSC,RESPPCC,PPCC,PLEV,IPPCCERR)
*
*     subroutine arguments
*
      LOGICAL CENSFLAG(*)
      INTEGER*4 IPPCCERR,NOBSC
      DOUBLE PRECISION PPCC,PLEV,RESPPCC(*)
*
*     local vars
*
      INTEGER*4 I,NDL,NUNCEN,NUY,IX(NOBSC)
      INTEGER*4, DIMENSION(:), ALLOCATABLE :: AJ,BJ,CJ
      DOUBLE PRECISION CDL,D,DELTA,MU,QTB2,SIGMA,U,W,Z,ZSMOOTH1,
     &                 ZSMOOTH2,ZSMOOTH3
      DOUBLE PRECISION PP(NOBSC),QQ(NOBSC),Y(NOBSC)
*
*     function declarations
*
      DOUBLE PRECISION CORR,ERFC,QNORM01
*
*     sort the data and count unique detection limits for load
*
      IPPCCERR = 0
      IF(NOBSC .GT. 5000) THEN
         IPPCCERR = 4
         RETURN
      ENDIF
      CALL PPORDER(CENSFLAG,RESPPCC,NOBSC,IX)
      CDL = -1.D99
      NDL = 0
      DO I=1,NOBSC
         IF(CENSFLAG(IX(I)) .AND. RESPPCC(IX(I)) .GT. CDL) THEN
            NDL = NDL + 1
            CDL = RESPPCC(IX(I))
         ENDIF
      ENDDO
*
*     compute the plotting positions and convert to quantiles of the
*     standard normal
*
      ALLOCATE(AJ(NDL+2))
      ALLOCATE(BJ(NDL+2))
      ALLOCATE(CJ(NDL+2))
      CALL PPARRANGE(RESPPCC,CENSFLAG,NOBSC,AJ,BJ,CJ,NDL,Y,NUNCEN,IX)
      CALL PPLOT(AJ,BJ,NDL,PP,IPPCCERR)
*
*     error check on QNORM01 and unique values of Y
*
      NUY = 0
      DO I=1,NUNCEN
         QQ(I) = QNORM01(PP(I))
         IF (QQ(I) .EQ. 0.D0 .AND. PP(I) .NE. 0.5D0) IPPCCERR=3
         IF (I .GT. 1 .AND. Y(I) .GT. Y(I-1)) NUY = NUY + 1
      ENDDO
      IF(NUY .LT. 2) THEN
         IPPCCERR=2
         RETURN
      ENDIF
*
*     compute the correlation and test stat between the uncensored and
*     qnormals
*
      U = LOG(DBLE(NOBSC))
      PPCC = CORR(NUNCEN, QQ, Y)
      W = LOG(1.D0 - PPCC**2)
      Z = (W - (-1.2725D0 + 1.0521D0 * (LOG(U) - U))) /
     &    (1.0308D0 - 0.26758D0 * (LOG(U) + 2.D0/U))
*
*     compute the uncensored (first part of IF) or censored ppcc.test
*     (ELSE), as appropriate.
*
*     For the censored test, compute the linear regression between the
*     smoothed Zs and the actual Z scores for the 0.9, .95, and .99
*     pvalues.  This is directly accomplished by application of the QR
*     decomposition of the matrix of 1s and the Z scores.
*
      IF(NDL .EQ. 0) THEN
         PLEV = ERFC(Z/1.4142135623731D0)/2.D0
      ELSE
         D = 0.76676D0 * U + 0.015814D0 * U**2
         DELTA = -LOG(DBLE(NOBSC - NUNCEN)/DBLE(NOBSC))
         ZSMOOTH1 = 1.2815515655446D0 + D *
     &              (0.164D0 + 0.533D0 * 0.556D0**U)**DELTA
         ZSMOOTH2 = 1.64485362695147D0 + D *
     &              (0.1736D0 + 0.315D0 * 0.622D0**U)**DELTA
         ZSMOOTH3 = 2.32634787404084D0 + D *
     &              (0.256D0 - 0.00635D0 * U)**DELTA
         QTB2 = ((0.625724703684127D0 * ZSMOOTH1) + 0.141396876352102D0
     &           * ZSMOOTH2) - 0.767121580036229D0 * ZSMOOTH3
         MU = (-0.577350269189626D0 * (((-0.577350269189626D0*ZSMOOTH1)
     &         - 0.577350269189626D0*ZSMOOTH2) - 0.577350269189626D0 *
     &         ZSMOOTH3)) + 2.33419583910949D0 * QTB2
         SIGMA = -1.33312711041740D0 * QTB2
         PLEV = ERFC((Z - MU)/SIGMA/1.4142135623731D0)/2.D0
      ENDIF

      RETURN
      END
