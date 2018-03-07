************************************************************************
*
*     Subroutine LADLOAD2                   Called by: LADLOAD
*
*     estimate loads using the least absolute deviation (LAD) method;
*     bias correction is done by the method of Duan (1983).
*
************************************************************************
      SUBROUTINE LADLOAD2(NPAR,NUMOBSE,XLEST,BICOR,LOAD,PARAM,PLOAD,
     &                    NOBSE)
*
*     subroutine arguments
*
      INTEGER*4 NPAR,NUMOBSE,NOBSE
      DOUBLE PRECISION BICOR,LOAD
      DOUBLE PRECISION PARAM(*),PLOAD(*)
      DOUBLE PRECISION XLEST(NOBSE,*)
*
*     local vars
*
      INTEGER*4 I,K
      DOUBLE PRECISION LTE
*
*     estimate loads
*
      LOAD = 0.D0
      DO 20 I=1,NUMOBSE
         LTE = 0.D0
         DO 10 K=1,NPAR
            LTE = LTE + PARAM(K)*XLEST(I,K)
 10      CONTINUE
         PLOAD(I) = DEXP(LTE)*BICOR
         LOAD = LOAD+PLOAD(I)
 20   CONTINUE
      LOAD = LOAD/DBLE(NUMOBSE)

      RETURN
      END
