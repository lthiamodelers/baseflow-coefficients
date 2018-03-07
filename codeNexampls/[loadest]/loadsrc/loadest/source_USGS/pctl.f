************************************************************************
*
*     Subroutine PCTL
*
*     Called by: INPUTE,OUTHEAD,OUTSUMM2,OUTSUMM3,OUTSUMMC
*
*     calculate the minimum and maximum values, and 10th, 25th, 50th,
*     75th, 90th, 95th, and 99th percentiles
*
*     Local variable
*     --------------
*     X2       copy of X sorted in ascending order
*
************************************************************************
      SUBROUTINE PCTL(X,NUM,XMIN,P10,P25,P50,P75,P90,P95,P99,XMAX)
*
*     subroutine arguments
*
      INTEGER*4 NUM 
      DOUBLE PRECISION XMIN,P10,P25,P50,P75,P90,P95,P99,XMAX
      DOUBLE PRECISION X(*)
*
*     local vars
*
      INTEGER*4 I
      DOUBLE PRECISION X2(NUM)
*
*     function declaration
*
      DOUBLE PRECISION PTILE
*
*     create copy of X and sort [all operations are done on the copy
*     (X2) so that X is passed back to the calling program unaltered]
*
      DO 10 I = 1,NUM
         X2(I) = X(I)
 10   CONTINUE
      CALL SORT(X2,NUM)
*
*     the minimum and maximum values correspond to the first and last
*     elements of X2
*
      XMIN = X2(1)
      XMAX = X2(NUM)
*
*     set percentiles
*
*
      P10 = PTILE(X2,NUM,0.10D0)
      P25 = PTILE(X2,NUM,0.25D0)
      P50 = PTILE(X2,NUM,0.50D0)
      P75 = PTILE(X2,NUM,0.75D0)
      P90 = PTILE(X2,NUM,0.90D0)
      P95 = PTILE(X2,NUM,0.95D0)
      P99 = PTILE(X2,NUM,0.99D0)

      RETURN
      END
