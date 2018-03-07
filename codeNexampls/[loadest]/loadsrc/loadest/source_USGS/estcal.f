************************************************************************
*
*     Subroutine ESTCAL                            Called by: CALIBR
*  
*     estimate AMLE loads and concentrations corresponding to the
*     observations in the calibration data set
*
*     local vars
*     ----------
*     A1
*     ALPHA
*     AT
*     B
*     B1
*     BIAS     bias of MLE estimates with respect to S**2
*     C
*     CV       covar. of std normals censored at XSI's w.r.t. S**2
*     CV_M     max. likelihood estimates of the covariance matrix
*     GAMMA
*     KAPPA
*     OMEGA
*     PARAML
*     PARMLE
*     SBIAS    bias of MLE estimates w.r.t. S
*     SCV      covar. of standard normals censored at XSI's w.r.t. S
*     STDDEV   standard deviation of the AMLE regression coefficients
*     XC
*     XCX
*     XLIKE    value of the log-likelihood function, AMLE regression
*     XOMEGA
*
************************************************************************
      SUBROUTINE ESTCAL(CCONCAML,CENSFLAG,CFACTOR,CFLOW,CLOADAML,
     &                  LFACTOR,NOBSC,NOBSCI,NPAR,XLCAL,YD,YLCAL)
*     
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      LOGICAL CENSFLAG(*)
      INTEGER*4 NOBSC,NOBSCI,NPAR
      DOUBLE PRECISION CFACTOR,LFACTOR
      DOUBLE PRECISION CCONCAML(*),CFLOW(*),CLOADAML(*),YD(*),YLCAL(*)
      DOUBLE PRECISION XLCAL(NOBSCI,*)
*
*     local vars
*
      INTEGER*4 I,I2,K
      DOUBLE PRECISION ALPHA,AT,KAPPA,XCX,XLIKE,XOMEGA
      DOUBLE PRECISION A1(NOBSCI),B(MAXPARMS),B1(NOBSCI),
     &                 BIAS(MAXPARMS+1),GAMMA(MAXPARMS),OMEGA(MAXPARMS),
     &                 PARAML(MAXPARMS+1),PARMLE(MAXPARMS+1),
     &                 SBIAS(MAXPARMS+1),STDDEV(MAXPARMS+1),XC(MAXPARMS)
      DOUBLE PRECISION C(MAXPARMS,MAXPARMS),CV(MAXPARMS+1,MAXPARMS+1),
     &                 CV_M(MAXPARMS+1,MAXPARMS+1),
     &                 SCV(MAXPARMS+1,MAXPARMS+1)
*
*     function declarations 
*
      DOUBLE PRECISION EXPON
*
*     redo calibration for best model
*
      CALL AMLREG(NOBSC,NPAR,XLCAL,YLCAL,YD,CENSFLAG,XLIKE,STDDEV,
     &            PARMLE,PARAML,BIAS,CV,SBIAS,SCV,CV_M,NOBSCI)
*
*     compute nearly-unbiased AMLE load estimates for the calibration
*     data set (for details, see TAC_LOAD)
*
*     define parameters and OMEGA
*
      DO 10 I=1,NPAR
         GAMMA(I) = SCV(I,NPAR+1)/SCV(NPAR+1,NPAR+1)
         OMEGA(I) = PARMLE(I)-GAMMA(I)*SQRT(PARMLE(NPAR+1))
         B(I) = SBIAS(I)-GAMMA(I)*(1.D0+SBIAS(NPAR+1))
 10   CONTINUE
      DO 30 I=1,NPAR
         DO 20 K=1,NPAR
            C(I,K) = SCV(I,K)-SCV(NPAR+1,NPAR+1)*GAMMA(I)*GAMMA(K)
 20      CONTINUE
 30   CONTINUE
      ALPHA = (1.D0+BIAS(NPAR+1))**2/CV(NPAR+1,NPAR+1)
      KAPPA = (1.D0+BIAS(NPAR+1))/ALPHA
*
*     compute individual loads
*
      DO 90 I=1,NOBSC
         AT = 0.D0
         DO 40 K=1,NPAR
            AT = AT+XLCAL(I,K)*B(K)
 40      CONTINUE
         A1(I) = -AT
         DO 60 I2=1,NPAR
            XC(I2) = 0.D0
            DO 50 K=1,NPAR
               XC(I2) = XC(I2)+XLCAL(I,K)*C(K,I2)
 50         CONTINUE
 60      CONTINUE
         XCX = 0.D0
         DO 70 K=1,NPAR
            XCX = XCX+XC(K)*XLCAL(I,K)
 70      CONTINUE
         B1(I) = (1.D0-XCX)/2.D0
         XOMEGA = 0.D0
         DO 80 K=1,NPAR
            XOMEGA = XOMEGA+XLCAL(I,K)*OMEGA(K)
 80      CONTINUE
*         
*        calculate load in kg/d, back out concentration, and convert
*        load to user-specified units.
*     
         CLOADAML(I) = EXP(XOMEGA) *
     &                 EXPON(PARMLE(NPAR+1),ALPHA,KAPPA,B1(I),A1(I))
         CCONCAML(I) = CLOADAML(I) / (CFLOW(I)*CFACTOR)
         CLOADAML(I) = CLOADAML(I) * LFACTOR

 90   CONTINUE

      RETURN
      END
