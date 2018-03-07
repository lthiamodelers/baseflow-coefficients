************************************************************************
*
*     Subroutine MLEVAR                            Called by: MLELOAD2
*
*
*     Estimate the variance of the mean MLE load using the method given
*     in Likes (1980) and Gilroy et al. (1990).
*
*     This version of TECALC1RC uses the function PHI given by Likes
*     (1980) rather the function GM originally given by Finney (1941) 
*     as coded by Cohn et al. (1989). PHI takes longer to compute
*     but can handle larger arguments so there it is less likely that
*     the computation of the variance will fail. Likes (1980) gives
*     the relation:
*
*                                     (DF+1)*A
*                     PHI[A*W,DF]=GM[----------*W]
*                                      2*DF**2
*
*     To use GM rather than PHI see C. Crawford's original LOADEST2.
*
*     PHI is denoted MVUEPHI below, to distinguish it from the
*     function PHI used to calculate the probability density
*     function of the standard normal distribution.
*    
*     Local variables
*     ---------------
*     DF2      degrees of freedom of the regression, divided by 2
*     LTE      log of transport (untransformed value from rating curve)
*     TE       uncorrected estimated population loads (transport est)
*     V        XO * XTXINV * XT for each population value
*     VIJ      XO * XTXINV * XJT for the Ith and Jth population value
*
************************************************************************
      SUBROUTINE MLEVAR(NPAR,XLEST,NUMOBSE,RVARMLE,XTXINV,VARMLE,DF2,
     &                  LTE,TE,V,NOBSE)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 NPAR,NUMOBSE,NOBSE
      DOUBLE PRECISION RVARMLE,VARMLE,DF2
      DOUBLE PRECISION LTE(*),TE(*),V(*)
      DOUBLE PRECISION XLEST(NOBSE,*),XTXINV(MAXPARMS,*)
*
*     local vars
*
      INTEGER*4 I,K
      DOUBLE PRECISION VIJ,TERM
*
*     function declarations
*
      DOUBLE PRECISION MVUEPHI
*
*     estimate the variance of the mean load.  Variance is equal to the
*     covariance of LDMLE(I) and LDMLE(K) summed over all I and K
*     (I=1,NUMOBSE; K=1,NUMOBSE).  Covariance is given in Gilroy et al.
*     (1990) and is computed as the expected value of
*     (LDMLE(I)*LDMLE(K)) minus the expected value of LDMLE(I) times the
*     expected value of LDMLE(K)
*
      VARMLE = 0.D0
      DO 40 I=1,NUMOBSE
         DO 30 K=1,I
            CALL MATMLT(VIJ,XLEST,XTXINV,NPAR,I,K,NOBSE)
            TERM = (((TE(I)*TE(K)*DEXP((V(I)+V(K)+2.D0*VIJ)
     &           *RVARMLE/2.D0))
     &           * (DEXP((2.D0-V(I)-V(K))*RVARMLE/2.D0))
     &           * (MVUEPHI(((1.D0-V(I))*(1.D0-V(K))*RVARMLE**2)
     &           /4.D0,DF2)))
     &           - (DEXP(LTE(I)+(RVARMLE/2.D0))
     &           * DEXP(LTE(K)+(RVARMLE/2.D0))))
            IF (K.EQ.I) THEN
               VARMLE = VARMLE + TERM
            ELSE
               VARMLE = VARMLE + 2.D0 * TERM
            ENDIF
 30      CONTINUE
 40   CONTINUE
*
*     calculate the variance of mean load
*
      VARMLE = VARMLE/DBLE(NUMOBSE**2)
      
      RETURN
      END
