************************************************************************
*
*     Function MEAN                Called by: CENTER, INPUTE, JKNIFE,
*                                             OUTHEAD
*
*     Calculate the MEAN value of X, where there are N observations
*
*     Local variables
*     ---------------
*     SUMX         sum of X values
*
************************************************************************
      DOUBLE PRECISION FUNCTION MEAN(X,N)
*
*     function arguments
*
      INTEGER*4 N
      DOUBLE PRECISION X(*)
*
*     local variables
*
      INTEGER*4 I
      DOUBLE PRECISION SUMX
*
*     initialize
*
      SUMX = 0.D0
*
*     sum the numbers and calculate mean
*
      DO 10 I=1,N
         SUMX = SUMX+X(I)
 10   CONTINUE
      MEAN = SUMX/DBLE(N)

      RETURN
      END
