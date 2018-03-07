************************************************************************
*
*     Subroutine SETXL3            Called by: AMLLOAD, LADLOAD, MLELOAD,
*                                             SETXL2
*
*     set explanatory variables for seasonal or monthly loads
*
************************************************************************
      SUBROUTINE SETXL3(NOBS,NPAR,XLEST,SXLEST,NOBSE,I)
*
*     subroutine arguments
*
      INTEGER*4 I,NOBS,NOBSE,NPAR
      DOUBLE PRECISION XLEST(NOBSE,*),SXLEST(NOBSE,*)
*
*     local var
*
      INTEGER*4 K
*
*     set explanatory variables for seasonal or monthly loads
*
      NOBS = NOBS + 1
      DO 10 K=1,NPAR
         SXLEST(NOBS,K) = XLEST(I,K)
 10   CONTINUE

      RETURN
      END





