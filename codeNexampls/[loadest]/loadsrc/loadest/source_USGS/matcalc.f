************************************************************************
*
*     Subroutine MATCALC                             Called by: MLELOAD
*
*     calculate XTX and invert
*
*     local vars
*     ----------
*     XT      transpose of XLCAL
*     XTX     product of XT and the original matrix XLCAL
*
************************************************************************
      SUBROUTINE MATCALC(NPAR,NOBSC,XLCAL,XTXINV,NOBSCI)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 NPAR,NOBSC,NOBSCI
      DOUBLE PRECISION XLCAL(NOBSCI,*),XTXINV(MAXPARMS,*)
*
*     local vars
*
      INTEGER*4 I,K,L
      DOUBLE PRECISION XT(MAXPARMS,NOBSCI),XTX(MAXPARMS,MAXPARMS)
*
*     transpose XLCAL to obtain XT
*
      DO 20 I=1,NOBSC
         DO 10 K=1,NPAR
            XT(K,I) = XLCAL(I,K)
 10      CONTINUE
 20   CONTINUE
*
*     multiply XT and XLCAL to obtain XTX; initialize XTXINV
*
      DO 50 I=1,NPAR
         DO 40 K=1,NPAR
            XTX(I,K) = 0.D0
            DO 30 L=1,NOBSC
                XTX(I,K) =  XTX(I,K) + XT(I,L)*XLCAL(L,K)
 30         CONTINUE
            XTXINV(I,K) = XTX(I,K)
 40      CONTINUE
 50   CONTINUE
*
*     factor and invert matrix XTX producing XTXINV
*
      CALL FACINV(XTXINV,MAXPARMS,NPAR)

      RETURN
      END
