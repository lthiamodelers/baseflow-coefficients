************************************************************************
*
*     Function EXPON                                Called by: TAC_LOAD
*
*     compute unbiased estimate of EXP(A1*SIGMA+B1*SIGMA^2), assuming
*     S2 is a GAMMA(ALPHA,SIGMA^2*KAPPA) random variable.  Result is
*     exact aside from machine error
*
*     S2       sample estimate of SIGMA^2
*     ALPHA    parameter of GAMMA distribution = (N-1)/2 in std. model
*     KAPPA    parameter of gamma distribution = 2/(N-1) in std. model
*     B1       coefficient in front of SIGMA^2
*     A1       coefficient in front of SIGMA
*
************************************************************************
      DOUBLE PRECISION FUNCTION EXPON(S2,ALPHA,KAPPA,B1,A1)
*
*     subroutine args
*
      DOUBLE PRECISION S2,ALPHA,KAPPA,B1,A1
*
*     local vars
*
      INTEGER*4 K,N,ISTEP
      DOUBLE PRECISION ARG,DELTA,TOL,C(0:1000),SK_UNB(0:1000)
      DATA TOL/1.D-15/
*
*     function declaration
*
      DOUBLE PRECISION DLNGAM
*
*     
*
      ARG = ABS(B1)*S2+ABS(A1)*SQRT(S2)
      N = MIN(998.D0,30.D0+ARG*(5.4-0.2*ARG+ABS(B1)
     &     *(8.28-1.11*ABS(B1)+1.98*ARG)+1.33*ABS(A1)))
  
 10   EXPON = 0.D0
  
      SK_UNB(0) = 1.D0
      SK_UNB(1) = SQRT(S2/KAPPA)
     &            * EXP(DLNGAM(ALPHA)-DLNGAM(ALPHA+0.5D0))
    
      CALL SQUARE(N,B1,A1,C)
  
      IF (ABS(A1).LT.TOL) THEN
         ISTEP = 2
      ELSE
         ISTEP = 1
      ENDIF
  
      DO 20 K=0,N,ISTEP
         DELTA = C(K)*SK_UNB(K)
         EXPON = EXPON + DELTA
         IF (ABS(DELTA).LT.TOL) RETURN
         SK_UNB(K+2) = SK_UNB(K)*(S2/KAPPA)/(ALPHA+0.5D0*K)
 20   CONTINUE
      N = N + 10
      IF (N.LT.998) GOTO 10
*
*     Error: failure to converge in 998 terms
*
      CALL ERROR7(11)

      RETURN
      END
