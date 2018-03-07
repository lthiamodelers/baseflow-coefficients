************************************************************************
*
*     Subroutine JKNIFE                             Called by: LADLOAD
*
*     Calculate jackknife estimates of mean river load variance using
*     least absolute deviation estimates of rating curve parameters.
*
*     Reference: Crawford (1996)
*
*     local vars
*     ----------
*     BICOR     transformation bias correction factor
*     MTEJK     mean load for each of the jackknife subsamples
*     MTEMEAN
*     NJACK     number of observations in the jackknife subsample
*     PARAM     LAD parameter estimates
*     PRED      predicted value of log load
*     SUM       sum of the squared differences between jackknife
*               estimate of the mean load and the average mean load
*               for all jackknife subsamples
*     XLCAL2    explanatory variables for the jackknife subsample
*     YLJACK    observed load for the jackknife subsample
*
************************************************************************
      SUBROUTINE JKNIFE(NPAR,NOBSC,XLCAL,YLCAL,NUMOBSE,XP,VLAD,NOBSCI,
     &                  NOBSE)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 NPAR,NOBSC,NUMOBSE,NOBSCI,NOBSE
      DOUBLE PRECISION VLAD
      DOUBLE PRECISION YLCAL(*),XLCAL(NOBSCI,*),XP(NOBSE,*)
*
*     local vars
*
      INTEGER*4 I,II,J,JJ,K,NJACK
      DOUBLE PRECISION BICOR,MTEMEAN,PRED,SUM
      DOUBLE PRECISION PARAM(MAXPARMS),MTEJK(NOBSCI),YLJACK(NOBSCI)
      DOUBLE PRECISION XLCAL2(NOBSCI,MAXPARMS)
*
*     function declaration
*
      DOUBLE PRECISION MEAN
*
*     obtain subsample for jackknife
*
      DO 50 I=1,NOBSC
         NJACK = 0
         DO 20 J=1,NOBSC
            IF (J.NE.I) THEN
               NJACK = NJACK + 1
               DO 10 K=1,NPAR
                  XLCAL2(NJACK,K) = XLCAL(J,K)
 10            CONTINUE
               YLJACK(NJACK) = YLCAL(J)
            ENDIF
 20      CONTINUE
*
*        calculate regression model parameters for jackknife sample
*
         CALL LADREG(NPAR,NJACK,XLCAL2,YLJACK,PARAM,BICOR,NOBSCI)
*
*        calculate jackknife estimate of mean load
*
         MTEJK(I) = 0.D0
         DO 40 II=1,NUMOBSE
            PRED = 0.D0	
            DO 30 JJ=1,NPAR
               PRED = PRED + PARAM(JJ)*XP(II,JJ)
 30         CONTINUE
            MTEJK(I) = MTEJK(I) + DEXP(PRED)*BICOR
 40      CONTINUE
         MTEJK(I) = MTEJK(I)/DBLE(NUMOBSE)

 50   CONTINUE
*     
*     sum squared difference between jackknife estimate of the mean
*     load and the average mean load for all jacknife subsamples, then
*     calculate estimated variances.
*
      SUM = 0.D0
      MTEMEAN = MEAN(MTEJK,NOBSC)
      DO 60 I=1,NOBSC
         SUM = SUM + (MTEJK(I)-MTEMEAN)**2
 60   CONTINUE
      VLAD = ((DBLE(NOBSC)-1.D0)/DBLE(NOBSC))*SUM

      RETURN
      END
