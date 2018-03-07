************************************************************************
*
*     Subroutine DHUMSL                              Called by: TACIT_R
*
*     minimize general unconstrained objective function using
*     (analytic) gradient and hessian provided by the calling routine
*
*     based on:
*
*     NIST Guide to Available Math Software.
*     Fullsource for module DHUMSL from package CMLIB.
*     Retrieved from CAMSUN on Sat Aug 30 09:32:07 1997.
*
C        THIS ROUTINE IS LIKE DSUMSL, EXCEPT THAT THE SUBROUTINE PARA-
C     METER CALCG OF DSUMSL (WHICH COMPUTES THE GRADIENT OF THE OBJEC-
C     TIVE FUNCTION) IS REPLACED BY THE SUBROUTINE PARAMETER CALCGH,
C     WHICH COMPUTES BOTH THE GRADIENT AND (LOWER TRIANGLE OF THE)
C     HESSIAN OF THE OBJECTIVE FUNCTION.  THE CALLING SEQUENCE IS...
C             CALL CALCGH(N, X, NF, G, H, UIPARM, URPARM, UFPARM)
C     PARAMETERS N, X, NF, G, UIPARM, URPARM, AND UFPARM ARE THE SAME
C     AS FOR DSUMSL, WHILE H IS AN ARRAY OF LENGTH N*(N+1)/2 IN WHICH
C     CALCGH MUST STORE THE LOWER TRIANGLE OF THE HESSIAN AT X.  START-
C     ING AT H(1), CALCGH MUST STORE THE HESSIAN ENTRIES IN THE ORDER
C     (1,1), (2,1), (2,2), (3,1), (3,2), (3,3), ...
C        THE VALUE PRINTED (BY DITSUM) IN THE COLUMN LABELLED STPPAR
C     IS THE LEVENBERG-MARQUARDT USED IN COMPUTING THE CURRENT STEP.
C     ZERO MEANS A FULL NEWTON STEP.  IF THE SPECIAL CASE DESCRIBED IN
C     REF. 1 IS DETECTED, THEN STPPAR IS NEGATED.  THE VALUE PRINTED
C     IN THE COLUMN LABELLED NPRELDF IS ZERO IF THE CURRENT HESSIAN
C     IS NOT POSITIVE DEFINITE.
C        IT SOMETIMES PROVES WORTHWHILE TO LET D BE DETERMINED FROM THE
C     DIAGONAL OF THE HESSIAN MATRIX BY SETTING IV(DTYPE) = 1 AND
C     V(DINIT) = 0.  THE FOLLOWING IV AND V COMPONENTS ARE RELEVANT...
C
C IV(DTOL)..... IV(59) GIVES THE STARTING SUBSCRIPT IN V OF THE DTOL
C             ARRAY USED WHEN D IS UPDATED.  (IV(DTOL) CAN BE
C             INITIALIZED BY CALLING DHUMSL WITH IV(1) = 13.)
C IV(DTYPE).... IV(16) TELLS HOW THE SCALE VECTOR D SHOULD BE CHOSEN.
C             IV(DTYPE) .LE. 0 MEANS THAT D SHOULD NOT BE UPDATED, AND
C             IV(DTYPE) .GE. 1 MEANS THAT D SHOULD BE UPDATED AS
C             DESCRIBED BELOW WITH V(DFAC).  DEFAULT = 0.
C V(DFAC)..... V(41) AND THE DTOL AND D0 ARRAYS (SEE V(DTINIT) AND
C             V(D0INIT)) ARE USED IN UPDATING THE SCALE VECTOR D WHEN
C             IV(DTYPE) .GT. 0.  (D IS INITIALIZED ACCORDING TO
C             V(DINIT), DESCRIBED IN DSUMSL.)  LET
C                  D1(I) = MAX(SQRT(ABS(H(I,I))), V(DFAC)*D(I)),
C             WHERE H(I,I) IS THE I-TH DIAGONAL ELEMENT OF THE CURRENT
C             HESSIAN.  IF IV(DTYPE) = 1, THEN D(I) IS SET TO D1(I)
C             UNLESS D1(I) .LT. DTOL(I), IN WHICH CASE D(I) IS SET TO
C                  MAX(D0(I), DTOL(I)).
C             IF IV(DTYPE) .GE. 2, THEN D IS UPDATED DURING THE FIRST
C             ITERATION AS FOR IV(DTYPE) = 1 (AFTER ANY INITIALIZATION
C             DUE TO V(DINIT)) AND IS LEFT UNCHANGED THEREAFTER.
C             DEFAULT = 0.6.
C V(DTINIT)... V(39), IF POSITIVE, IS THE VALUE TO WHICH ALL COMPONENTS
C             OF THE DTOL ARRAY (SEE V(DFAC)) ARE INITIALIZED.  IF
C             V(DTINIT) = 0, THEN IT IS ASSUMED THAT THE CALLER HAS
C             STORED DTOL IN V STARTING AT V(IV(DTOL)).
C             DEFAULT = 10**-6.
C V(D0INIT)... V(40), IF POSITIVE, IS THE VALUE TO WHICH ALL COMPONENTS
C             OF THE D0 VECTOR (SEE V(DFAC)) ARE INITIALIZED.  IF
C             V(DFAC) = 0, THEN IT IS ASSUMED THAT THE CALLER HAS
C             STORED D0 IN V STARTING AT V(IV(DTOL)+N).  DEFAULT = 1.0.
C
C  ***  REFERENCE  ***
C
C   GAY, D.M. (1981), COMPUTING OPTIMAL LOCALLY CONSTRAINED STEPS,
C         SIAM J. SCI. STATIST. COMPUT. 2, PP. 186-197.
C
C     CODED BY DAVID M. GAY (WINTER 1980).  REVISED SEPT. 1982.
C     WRITTEN IN CONNECTION WITH RESEARCH SUPPORTED
C     IN PART BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS
C     MCS-7600324 AND MCS-7906671.
*
************************************************************************
      SUBROUTINE DHUMSL(CENSFLAG,D,IV,LV,NOBSC,NOBSCI,NPAR,PARMLE,V,X,
     &                  YD,YLCAL)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      LOGICAL CENSFLAG(*)
      INTEGER*4 LV,NOBSC,NOBSCI,NPAR,IV(*)
      DOUBLE PRECISION D(*),PARMLE(*),V(*),YLCAL(*),YD(*),X(NOBSCI,*)
*
*     local vars
*
      INTEGER*4 I,J,K,G1,H1
      DOUBLE PRECISION FX,HF(MAXPARMS+1,MAXPARMS+1)
*
*
* 
      IV(4) = IV(4) + (NPAR+1)*(NPAR+4)/2
      G1 = 1
      H1 = 1
      IV(1) = 13
      GOTO 20

 10   G1 = IV(28)
      H1 = IV(56)
*
*     DHUMIT -- reverse-communication routine that does DHUMSL
*     algorithm.
*
 20   CALL DHUMIT(D,FX,V(G1),V(H1),IV,LV,NPAR,V,PARMLE)

      IF (IV(1) .LT. 2) THEN
         CALL TACIT_L(PARMLE,FX,NOBSC,NPAR,X,YLCAL,YD,CENSFLAG,NOBSCI)
         IF (IV(6) .LE. 0) IV(2) = 1
         GOTO 20
      ELSEIF (IV(1) .EQ. 2) THEN
         CALL TACIT_G(PARMLE,V(G1),NOBSC,NPAR,X,YLCAL,YD,CENSFLAG,
     &                NOBSCI)
         CALL TACIT_H(PARMLE,HF,NOBSC,NPAR,X,YLCAL,YD,CENSFLAG,NOBSCI)
         K = H1
         DO 40 I=1,NPAR+1
            DO 30 J=1,I
               V(K) = HF(I,J)
               K = K+1
 30         CONTINUE
 40      CONTINUE
         GOTO 20
      ELSEIF (IV(1) .EQ. 14) THEN
         IV(28) = IV(47)
         IV(56) = IV(28) + NPAR + 1 
         IV(47) = IV(56) + (NPAR+1)*(NPAR+2)/2
         GOTO 10
      ELSE
         RETURN
      ENDIF

      END
