************************************************************************
*
*     Subroutine CALBET                               Called by: L1NORM
*
*     back-solve system of equations.  Routine assumes NPAR > 1.
*     
************************************************************************
      SUBROUTINE CALBET(KKK,PARAM,RHS,NPAR,INDX,LU)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 KKK,NPAR,INDX(*)
      DOUBLE PRECISION PARAM(*),RHS(*),LU(MAXPARMS,*)
*
*     local vars
*
      INTEGER*4 I,II,K,K1,K2,KK
      
      DO 10 I=1,KKK
         K = INDX(I)
         PARAM(K)= 0.D0
   10 CONTINUE

      KKK = KKK+1
      K = INDX(KKK)
      PARAM(K) = RHS(KKK)/LU(K,KKK)

      DO 30 II=KKK+1,NPAR
         K = INDX(II)
         PARAM(K) = RHS(II)
         DO 20 I=KKK,II-1
            KK = INDX(I)
            PARAM(K) = PARAM(K)-LU(KK,II)*PARAM(KK)
 20      CONTINUE
         PARAM(K) = PARAM(K)/LU(K,II)
 30   CONTINUE
      
      DO 50 II=1,NPAR-1
         K1 = NPAR-II
         K = INDX(K1)
         DO 40 I=1,II
            KK = NPAR-I+1
            K2 = INDX(KK)
            PARAM(K) = PARAM(K)-LU(K2,K1)*PARAM(K2)
 40      CONTINUE
 50   CONTINUE

      RETURN
      END
