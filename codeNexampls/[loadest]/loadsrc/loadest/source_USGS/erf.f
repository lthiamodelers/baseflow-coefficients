************************************************************************
*
*     Function ERF                                     Called by: LOGLK
*
*     Calculate the Error Function using the approximation given in
*     Section 7.1.26 of Abramowitz and Stegun (1964).  The error in the
*     approximation is less than 1.5E-7.
*
*     local vars
*     ----------
*     A1       constant used in approximation
*     A2       constant used in approximation
*     A3       constant used in approximation
*     A4       constant used in approximation
*     A5       constant used in approximation
*     P        constant used in approximation
*     T        quantity used in approximation
*
************************************************************************
      DOUBLE PRECISION FUNCTION ERF(X)
*
*     subroutine argument
*
      DOUBLE PRECISION X
*
*     local vars
*
      DOUBLE PRECISION A1,A2,A3,A4,A5,P,T
      PARAMETER (A1=0.254829592D0,A2=-0.284496736D0,A3=1.421413741D0)
      PARAMETER (A4=-1.453152027D0,A5=1.061405429D0,P=0.3275911D0)
*
*     Approximation is valid for X > 0.0.  If X is negative obtain
*     approximation using symmetry relation: Erf(-X) = -Erf(X).
*
      IF (X.GE.0.D0) THEN
         T=1.D0/(1.D0+P*X)
         ERF=1.D0-((T*(A1+T*(A2+T*(A3+T*(A4+T*A5)))))*DEXP(-(X**2)))
      ELSE
         T=1.D0/(1.D0+P*DABS(X))
         ERF=-(1.D0-((T*(A1+T*(A2+T*(A3+T*(A4+T*A5)))))*DEXP(-(X**2))))
      ENDIF

      RETURN
      END
