************************************************************************
*
*     Function LOGLK                                  Called by: CALIBR
*
*     calculate the log likelihood of Tobit regression (Paarsch, 1984)
*
*     locals vars
*     -----------
*     YHAT    predicted value of the response variable
*
************************************************************************
      DOUBLE PRECISION FUNCTION LOGLK(NPAR,NOBSC,XLCAL,YLCAL,CENSFLAG,
     &                                PARAML,RESSTD,NOBSCI)
*
*     subroutine arguments
*
      INTEGER*4 NPAR,NOBSC,NOBSCI
      LOGICAL CENSFLAG(*)
      DOUBLE PRECISION RESSTD,YLCAL(*),PARAML(*),XLCAL(NOBSCI,*)
*
*     local vars
*
      INTEGER*4 I
      DOUBLE PRECISION YHAT
*
*     function declarations
*
      DOUBLE PRECISION ERF,PRED
*
*     compute the log likelihood due to censored and uncensored data
*
*     if the observation is censored, its contribution to LOGLK is based
*     on the integral of the probability density of a standardized
*     normal variable from X to infinity (the complement of the
*     cumulative distribution function)(Abramowitz and Stegun, 1964, p.
*     297 & 931). If the observation is uncensored, the contribution is
*     based on the probability density function of the standard normal
*     distribution, divided by the residual standard deviation (RESSTD).
*
      LOGLK = 0.D0

      DO 10 I=1,NOBSC
         YHAT = PRED(NPAR,NOBSCI,I,XLCAL,PARAML)
         IF (CENSFLAG(I)) THEN
            LOGLK = LOGLK
     &             + DLOG((1.D0
     &                -ERF((-(YLCAL(I)-YHAT)/RESSTD)/DSQRT(2.D0)))/2.D0)
         ELSE
            LOGLK = LOGLK
     &             + DLOG(((1.D0/DSQRT(2.D0*3.14159265359D0)) *
     &                     DEXP(-(((YLCAL(I)-YHAT)/RESSTD)**2)/2.D0)
     &                                                     /RESSTD))
         ENDIF
 10   CONTINUE

      RETURN
      END
