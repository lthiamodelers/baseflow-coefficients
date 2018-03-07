************************************************************************
*
*     Subroutine CENTER                               Called by: SETUPC
*
*     compute the 'center' (CENT) of the data (XDATA) such that
*     explanatory variables involving linear and quadratic terms are
*     orthogonal.
*
*     the center is equal to:
*               
*         XBAR + SUM((XDATA-XBAR)^3)/(2*SUM((XDATA-XBAR)^2))
*
*     where
*         
*         XBAR is the mean of XDATA
*
************************************************************************
      SUBROUTINE CENTER(CENT,XDATA,NUM)
*
*     subroutine arguments
*
      INTEGER*4 NUM
      DOUBLE PRECISION CENT,XDATA(*)
*
*     local vars
*
      INTEGER*4 I
      DOUBLE PRECISION XBAR,SECOND,THIRD
*     
*     function declaration
*
      DOUBLE PRECISION MEAN
*
*     compute the mean (XBAR), 2nd, and 3rd moments (SECOND, THIRD)
*
      XBAR = MEAN(XDATA,NUM)
      SECOND = 0.D0
      THIRD = 0.D0
      DO 10 I=1,NUM
         SECOND = SECOND+(XDATA(I)-XBAR)**2
         THIRD = THIRD+(XDATA(I)-XBAR)**3
 10   CONTINUE
      SECOND = SECOND/DBLE(NUM)
      THIRD = THIRD/DBLE(NUM)
*
*     compute the center
*
      CENT = XBAR + THIRD/(2.D0*SECOND)

      RETURN
      END
