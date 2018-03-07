************************************************************************
*
*     Function PTILE                                    Called by: PCTL
*
*     calculate the requested percentile from a sorted vector
*     (Beyer, 1987 p. 518.)
*
*     local variables
*     ---------------
*     NP       index corresponding to the desired percentile
*     WGHT     weight factor used to interpolate between the two
*              closest values when the vector index of the desired
*              percentile is not an integer
*
************************************************************************
      DOUBLE PRECISION FUNCTION PTILE(X2,NUM,PCTILE)
*
*     subroutine arguments
*
      INTEGER*4 NUM 
      DOUBLE PRECISION PCTILE,X2(*)
*
*     local vars
*
      INTEGER*4 I
      DOUBLE PRECISION NP,WGHT
*
*     For data sorted in ascending order, the Jth percentile is
*     defined as the ((N+1)J/100)th value.  When this quantity is not
*     an integer, the Jth percentile is interpolated from the two
*     closest values.  The weighting factor for interpolation, WGHT,
*     is equal to the noninteger fractional portion of NP. IF statement
*     is required to prevent interpolation past the end of the data set
*     (e.g. the 99 percentile of some small data sets may 
*
      NP = DBLE(NUM+1)*PCTILE
      I = INT(NP)
      WGHT = NP-INT(NP)

      IF (I+1 .LE. NUM) THEN
         PTILE = (1.0D0-WGHT)*X2(I)+WGHT*X2(I+1)
      ELSE
         PTILE = X2(I)
      ENDIF

      RETURN
      END
