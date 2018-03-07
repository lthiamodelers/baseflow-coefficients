************************************************************************
*
*     Function MVUEPHI                      Called by: MLELOAD2, MLEVAR
*
*     calculate the function PHI given in Likes (1980)
*
*     local vars
*     ----------
*     ARG1     DLOGAM(K+1)
*     ARG2     DLOGAM(NU2)
*     ARG3     DLOGAM(NU2+K)
*     IK       I-1
*     RK       DBLE(I-1)
*     TOL      convergence criteria (convergence when term < tol)
*     WORK     amount of MVUEPHI contributed by a single term
*
**********************************************************************
      DOUBLE PRECISION FUNCTION MVUEPHI(AW4,NU2)
*
*     subroutine arguments
*
      DOUBLE PRECISION AW4,NU2
*
*     local variables
*
      INTEGER*4 I,IK
      DOUBLE PRECISION ARG1,ARG2,ARG3,RK,TOL,WORK
*
*     function declaration
*
      DOUBLE PRECISION DLOGAM
*
*     define criteria for convergence
*
      PARAMETER (TOL=1.D-9)
*
*     for AW4 equal to zero, PHI equals 1
*
      IF (AW4 .EQ. 0.D0) THEN
         MVUEPHI = 1.D0
         RETURN
      ENDIF
*
*     For AW4 not equal to zero, calculate PHI by iteration.  PHI may
*     be computed using factorial and gamma functions, e.g.:
*
*     WORK = (1.D0/FAC(RK))*(GAMMA(NU2)/GAMMA(NU2+RK))*GAMMA(AW4)**IK
*
*     instead of GAMMA(X) use EXP(LN_GAMMA(X)) to avoid machine
*     overflow.  This allows log GAMMA terms to be added/subtracted
*     rather than multiplying/dividing.  Use EXP(LOGAM(K+1)) for
*     FACTORIAL(K).
*
      MVUEPHI = 0.D0
      DO 10 I=1,200
         IK = I-1
         RK = DBLE(I-1)
         ARG1 = DLOGAM((RK+1.D0))
         ARG2 = DLOGAM(NU2)
         ARG3 = DLOGAM(NU2+RK)
         IF (AW4**IK.GT.2.D0**1000) CALL ERROR7(5)
         WORK = DEXP(-ARG1+ARG2-ARG3)*(AW4**IK)
         MVUEPHI = MVUEPHI+WORK
         IF (I.GT.2.AND.DABS(WORK).LT.TOL) RETURN
 10   CONTINUE
*
*     the function has not converged in 200 iterations; terminate
*     execution
*
      CALL ERROR7(6)

      END
