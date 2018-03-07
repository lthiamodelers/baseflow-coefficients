************************************************************************
*
*     Subroutine MLELOAD2                            Called by: MLELOAD
*
*
*     Estimate loads using MLE.  Bias correction is done by the
*     method of Bradu and Mundlak(1970). An estimate of the variance
*     of the load is obtained by the method given in Likes (1980).
*
*     The function PHI given by Likes (1980) is used rather the
*     function GM given by Finney (1941) as coded by Cohn et al.
*     (1989). PHI takes longer to compute but can handle larger
*     arguments so there it is less likely that the computation of the
*     variance will fail. Likes (1980) gives the relation:
*
*                                     (DF+1)*A
*                     PHI[A*W,DF]=GM[----------*W]
*                                      2*DF**2
*
*     To use GM rather than PHI see C. Crawford's original LOADEST.
*
*     PHI is denoted MVUEPHI below, to distinguish it from the
*     function PHI used to calculate the probability density
*     function of the standard normal distribution.
*    
*     local vars
*     ----------
*     DF2      degrees of freedom of the regression, divided by 2
*     LTE      log of transport (untransformed value fr rating curve)
*     TE       uncorrected estimated population loads (transport est)
*     V        XO * XTXINV * XT for each population value
*
************************************************************************
      SUBROUTINE MLELOAD2(NPAR,XLEST,NUMOBSE,NOBSC,PARMLE,RVARMLE,
     &                   XTXINV,SEOPT,LOADMLE,VARMLE,PLOADMLE,NOBSE)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 NPAR,NUMOBSE,SEOPT,NOBSC,NOBSE
      DOUBLE PRECISION RVARMLE,LOADMLE,VARMLE
      DOUBLE PRECISION PARMLE(*),PLOADMLE(*)
      DOUBLE PRECISION XLEST(NOBSE,*),XTXINV(MAXPARMS,*)
*
*     local vars
*
      INTEGER*4 I
      DOUBLE PRECISION DF2,LTE(NUMOBSE),TE(NUMOBSE),V(NUMOBSE)
*
*     function declarations
*
      DOUBLE PRECISION MVUEPHI,PRED
*
*     calculate the estimate of the mean load
*
      DF2 = DBLE(NOBSC-NPAR)/2.D0
      LOADMLE = 0.D0
      DO 10 I=1,NUMOBSE
         CALL MATMLT(V(I),XLEST,XTXINV,NPAR,I,I,NOBSE)
         LTE(I) = PRED(NPAR,NOBSE,I,XLEST,PARMLE)
         TE(I) = DEXP(LTE(I))
         PLOADMLE(I) = TE(I)*MVUEPHI(((1.D0-V(I))*DF2*RVARMLE)/2.D0,DF2)
         LOADMLE = LOADMLE + PLOADMLE(I)
 10   CONTINUE
      LOADMLE = LOADMLE/DBLE(NUMOBSE)
*
*     estimate the variance of the mean load
*
      IF ((SEOPT .EQ. 2) .OR. (SEOPT .EQ. 3))
     &   CALL MLEVAR(NPAR,XLEST,NUMOBSE,RVARMLE,XTXINV,VARMLE,DF2,LTE,
     &               TE,V,NOBSE)
      
      RETURN
      END
