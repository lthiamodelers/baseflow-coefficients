************************************************************************
*
*     Subroutine L1NORM                               Called by: LADREG
*
*     solve the initial regression problem with all parameters included
*     in the model
*     
*     local vars
*     ----------
C     IBASE:THE INDEX ARRAY OF COLUMNS OF THE BASIS
C     INEXT:A LOCAL ARRAY FOR THE SORT ROUTINE
C     LU:THE LU DECOMPOSITION OF THE CURRENT BASIS
*  this is from KBEST, not sure its correct....
C     SIGMA:INDICATOR ARRAY TO SPECIFY WHETHER A NONBASIC DUAL VARIABLE
C           IS AT UPPER OR LOWER BOUND(+1 IMPLIES UPPER;-1 IMPLIES
C           LOWER)
************************************************************************
      SUBROUTINE L1NORM(X,Y,N,NPAR,PARAM,TOT,INDX,NOBSCI)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 NPAR,N,NOBSCI,INDX(*)
      DOUBLE PRECISION PARAM(*),TOT(*),Y(*),X(NOBSCI,*)
*
*     local vars
*
      LOGICAL INTL
      INTEGER*4 I,IIN,IOUT,IPT,J,K,KKK,KOUNT,L
      INTEGER*4 INEXT(NOBSCI),IBASE(MAXPARMS)
      DOUBLE PRECISION ACU,AHALF,AONE,BONE,RATIO,RHO,SUBT,SUM,T,TEST,VAL
      DOUBLE PRECISION D(NOBSCI),DELTA(NOBSCI),PI(MAXPARMS),
     &                 PRICE(NOBSCI),RHS(MAXPARMS),SIGMA(NOBSCI)
      DOUBLE PRECISION LU(MAXPARMS,MAXPARMS)
*
*     function declaration
*
      DOUBLE PRECISION D1MACH
*
*     assign values to machine dependent constant (see Fox et al., 1978)
*
      ACU = DSQRT(D1MACH(4))
*
*      INITIAL SETTINGS
*
      AONE = 1.D0 + ACU
      AHALF = 0.5D0 + ACU
      BONE = 1.D0 - ACU
*
*     set up initial LU decomposition
*
      INTL = .TRUE.
      K = 1
      CALL UPDATE(K,X,N,NPAR,INDX,LU,IBASE,INTL,NOBSCI)
      INTL = .FALSE.
      DO 10 I=1,NPAR
         RHS(I) = Y(IBASE(I))
 10   CONTINUE
      KKK = 0
      CALL CALBET(KKK,PARAM,RHS,NPAR,INDX,LU)
*
*     calculate initial D, TOT and SIGMA vectors
*
      DO 30 J=1,N
         VAL = 0.D0
         DO 20 I=1,NPAR
            VAL = VAL+PARAM(I)*X(J,I)
 20      CONTINUE
         D(J) = Y(J)-VAL
         SIGMA(J) = DSIGN(1.D0,D(J))
 30   CONTINUE
      DO 40 J=1,NPAR
         RHS(J) = 0.D0
         KKK = IBASE(J)
         SIGMA(KKK) = 0.D0
 40   CONTINUE
      DO 60 J=1,N
         DO 50 I=1,NPAR
            TOT(I)=TOT(I)-SIGMA(J)*X(J,I)
 50      CONTINUE
 60   CONTINUE
*
*     main iterative loop
*
 70   KKK = 0
      CALL CALCPI(KKK,PI,TOT,NPAR,INDX,LU)
      T = AONE
      K = 0
      DO 80 J=1,NPAR
         IF (DABS(PI(J)).GE.T) THEN
            K = J
            T = DABS(PI(J))
            RHO = -DSIGN(1.D0,PI(J))
         ENDIF
   80 CONTINUE
*
*     if K=0, we're done -- calculate optimal param and sum of absolute
*     deviations and RETURN
*
      IF (K.EQ.0) THEN
         DO 230 I=1,NPAR
            RHS(I) = Y(IBASE(I))
 230     CONTINUE
         KKK = 0
         CALL CALBET(KKK,PARAM,RHS,NPAR,INDX,LU)
         RETURN
      ENDIF
*
*     we're not done - continue iterating
*
      KKK = K-1
      RHS(K) = 1.D0
      CALL CALBET(KKK,PARAM,RHS,NPAR,INDX,LU)
      RHS(K) = 0.D0
      DO 100 I=1,N
         DELTA(I) = 0.D0
         IF (SIGMA(I).NE.0.D0) THEN
            DO 90 J=1,NPAR
               DELTA(I) = DELTA(I)+PARAM(J)*X(I,J)
 90         CONTINUE
            DELTA(I) = RHO*DELTA(I)
         ENDIF
 100  CONTINUE
*
*     perform partial sort of ratios
*
      T = T*0.5D0
      KOUNT = 0
      RATIO = D1MACH(2)
      SUM = AHALF
      SUBT = 0.D0
      DO 160 I=1,N
         IF (DELTA(I)*SIGMA(I).LE.ACU) GOTO 160
         TEST = D(I)/DELTA(I)
         IF (TEST.GE.RATIO) GOTO 160
         SUM = SUM+DABS(DELTA(I))
         IF (SUM-SUBT.LT.T) THEN
*
*           insert I in list
*
            KOUNT = KOUNT+1
            PRICE(KOUNT) = TEST
            INEXT(KOUNT) = I
            GOTO 160
         ENDIF
*
*        update SUM and kick IIN out of the list
*
 110     SUM = SUM-SUBT
         RATIO = TEST
         IPT = 0
         KKK = 0
*
*        identify a new IIN
*
 120     KKK = KKK+1
         IF (KKK.LE.KOUNT) THEN
            IF (PRICE(KKK).GT.RATIO) THEN
               RATIO = PRICE(KKK)
               IPT = KKK
            ENDIF
            GOTO 120
         ENDIF
         IF (IPT.NE.0) THEN
*
*           switch values
*
            KKK = INEXT(IPT)
            SUBT = DABS(DELTA(KKK))
            IF (SUM-SUBT.GE.T) THEN
               PRICE(IPT) = PRICE(KOUNT)
               INEXT(IPT) = INEXT(KOUNT)
               KOUNT = KOUNT-1
               GOTO 110
            ENDIF
            IIN = INEXT(IPT)
            INEXT(IPT) = I
            PRICE(IPT) = TEST
         ELSE
            IIN = I
            SUBT = DABS(DELTA(I))
         ENDIF
  160 CONTINUE
*
*     update basic indicators
*
      DO 180 J=1,KOUNT
         KKK = INEXT(J)
         DO 170 L=1,NPAR
            SUBT = SIGMA(KKK)*X(KKK,L)
            TOT(L) = TOT(L)+SUBT+SUBT
 170     CONTINUE
         SIGMA(KKK) = -SIGMA(KKK)
 180  CONTINUE
      IOUT = IBASE(K)
      DELTA(IOUT) = RHO
      DO 200 L=1,NPAR
         TOT(L) = TOT(L)+RHO*X(IOUT,L)+SIGMA(IIN)*X(IIN,L)
 200  CONTINUE
      SIGMA(IOUT) = -RHO
      IBASE(K) = IIN
      CALL UPDATE(K,X,1,NPAR,INDX,LU,IBASE,INTL,NOBSCI)

      DO 210 J=1,N
         D(J) = D(J)-RATIO*DELTA(J)
 210  CONTINUE
      SIGMA(IIN) = 0.D0
      GOTO 70

      END
