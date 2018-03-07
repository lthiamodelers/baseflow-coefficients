************************************************************************
*
*     Subroutine LADREG                      Called by: CALIBR, JKNIFE
*
*     Estimate parameters of a censored linear model with NPAR
*     explanatory variables using Powell's Censored Least Absolute
*     Deviation (CLAD) (Powell, 1984).  An iterative linear programming
*     method is used to implement CLAD (Buchinsky, 1994).
*
*     LAD is done via Algorithm 615 from the Association for
*     Computing Machinery (ACM).  Original code obtained from NETLIB
*     TOMS collection.
* 
*     *** This routine is for the special case of no censoring ***
*
*
*     local vars
*     ----------
*     INDX     rows of LU
*     NOBS2    number of observations in the last trimmed sample
*     TOT      current RHS of the dual problem
*     XOBS2    copy of the observed values of the explanatory variables
*              (the last trimmed sample)
*     YOBS2    observed uncensored values of the response variable and
*              the estimated value of the response variable for censored
*              observations (the last trimmed sample)
*
************************************************************************
      SUBROUTINE LADREG(NPAR,NUMOBS,XOBS,YOBS,PARAM,BICOR,NOBSCI)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 NPAR,NOBSCI,NUMOBS
      DOUBLE PRECISION BICOR,PARAM(*),YOBS(*),XOBS(NOBSCI,*)
*
*     local vars
*
      INTEGER*4 I,J,NOBS2
      INTEGER*4 INDX(MAXPARMS)
      DOUBLE PRECISION TOT(MAXPARMS),YOBS2(NOBSCI)
      DOUBLE PRECISION XOBS2(NOBSCI,MAXPARMS)
*
*     function declaration
*
      DOUBLE PRECISION PRED
*
*     create copy of matrix of observed X values for input to L1NORM.
*     Original XOBS is not passed.
*
      NOBS2 = 0
      DO I=1,NUMOBS
         DO J=1,NPAR
            XOBS2(I,J) = XOBS(I,J)
         ENDDO
         YOBS2(I) = YOBS(I)
         NOBS2 = NOBS2 + 1
      ENDDO
* 
*     obtain initial parameter estimates by calculating Least Absolute
*     Deviation of linear model using detection limit for response
*     variable. (original setup included s/r KBEST for selecting best
*     explanatory variable subsets.  By setting the minimum number of
*     variables NPAR, a conventional LAD regression is done here).
*
      DO I=1,NPAR
         INDX(I) = I
         TOT(I) = 0.D0
      ENDDO
      CALL L1NORM(XOBS2,YOBS2,NOBS2,NPAR,PARAM,TOT,INDX,NOBSCI)
*
*     calculate transformation bias correction factor (BICOR).  BICOR is
*     based on Duan'S Nonparametric Smearing Estimator (mean of the
*     exponentiated residuals)
*
      BICOR = 0.D0
      DO I=1,NUMOBS
         BICOR = BICOR + DEXP(YOBS(I)-PRED(NPAR,NOBSCI,I,XOBS,PARAM))
      ENDDO
      BICOR = BICOR/DBLE(NUMOBS)

      RETURN
      END
