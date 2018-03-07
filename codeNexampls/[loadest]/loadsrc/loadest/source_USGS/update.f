************************************************************************
*
*     Subroutine UPDATE                               Called by: L1NORM
*
*     update LU decomposition matrix
*     
*     local vars
*     ----------
*
************************************************************************
      SUBROUTINE UPDATE(KKK,X,N,NPAR,INDX,LU,IBASE,INTL,NOBSCI)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      LOGICAL INTL
      INTEGER*4 KKK,NPAR,N,NOBSCI,IBASE(*),INDX(*)
      DOUBLE PRECISION X(NOBSCI,*),LU(MAXPARMS,*)
*
*     local vars
*
      INTEGER*4 I,ICOL,II,IROW,ISAVE,K,KK
      DOUBLE PRECISION PIVOT,SUBT
*
*     function declaration
*
      DOUBLE PRECISION D1MACH
*
*     
*
      IROW = 0
      DO 90 II=KKK,NPAR
         IF (.NOT. INTL) THEN
            IROW = IBASE(II)
            GOTO 20
         ENDIF
 10      IROW = IROW+1
         IBASE(II) = IROW
         IF (IROW.GT.N) CALL ERROR7(10)

 20      DO 30 I=1,NPAR            
            LU(I,II) = X(IROW,I)
 30      CONTINUE
*
*        set up representation of incoming row
*
         DO 50 ICOL=1,II-1
            K = INDX(ICOL)
            SUBT = LU(K,II)
            DO 40 I=ICOL+1,NPAR
               K = INDX(I)
               LU(K,II) = LU(K,II)-SUBT*LU(K,ICOL)
 40         CONTINUE
 50      CONTINUE
*
*        find maximum entry
*
*        assign values to machine dependent constants, see Fox et al.
*        (1978)
*
         PIVOT = DSQRT(D1MACH(4))
         KK = 0
         DO 70 I=II,NPAR
            K = INDX(I)
            IF (DABS(LU(K,II)).GT.PIVOT) THEN
               PIVOT = DABS(LU(K,II))
               KK = I
            ENDIF
 70      CONTINUE
         IF (KK.EQ.0) GOTO 10
*
*        switch order
*
         ISAVE = INDX(KK)
         INDX(KK) = INDX(II)
         INDX(II) = ISAVE
*
*        put in columns of LU one at a time
*
         DO 80 I=II+1,NPAR
            K = INDX(I)
            LU(K,II) = LU(K,II)/LU(ISAVE,II)
 80      CONTINUE
 90   CONTINUE

      KKK = IROW

      RETURN
      END
