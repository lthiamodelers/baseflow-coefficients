************************************************************************
*
*     Subroutine CALCPI                               Called by: L1NORM
*
*     forward solve linear system.   Routine assumes NPAR > 1.
*
************************************************************************
      SUBROUTINE CALCPI(KKK,PI,TOT,NPAR,INDX,LU)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 KKK,NPAR,INDX(*)
      DOUBLE PRECISION PI(*),TOT(*),LU(MAXPARMS,*)
*
*     local vars
*
      INTEGER*4 I,II,K1,K2
*
*     initialize PI
*
      DO 10 I=1,KKK
         PI(I) = 0.D0
   10 CONTINUE

      KKK = KKK + 1
      PI(KKK) = TOT(INDX(KKK))

      DO 30 II=KKK+1,NPAR
         PI(II) = TOT(INDX(II))
         DO 20 I=KKK,II-1
            PI(II) = PI(II)-LU(INDX(II),I)*PI(I)
 20      CONTINUE
 30   CONTINUE

      
      PI(NPAR) = PI(NPAR)/LU(INDX(NPAR),NPAR)

      DO 50 II=1,NPAR-1
         K1 = NPAR-II
         
         DO 40 I=1,II
            K2 = NPAR-I+1
            PI(K1) = PI(K1)-LU(INDX(K1),K2)*PI(K2)
 40      CONTINUE
         PI(K1) = PI(K1)/LU(INDX(K1),K1)
 50   CONTINUE

      RETURN
      END
