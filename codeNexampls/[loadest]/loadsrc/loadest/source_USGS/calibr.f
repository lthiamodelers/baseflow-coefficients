************************************************************************
*
*     Subroutine CALIBR                        Called by: Main program
*
*     calibrate a regression model for estimating loads
*
*
*     local vars
*     ----------
*     AIC      Akaike Information Criterion
*     AICMIN   minimum value of the AIC
*     BESTMOD2 best regression model based on SPPC
*     BIAS     bias of max likelihood estimates w.r.t. S**2
*     CCONC    "observed" concentrations from the calibration data set
*     CCONCAML AMLE conc. estimates corresp to calibration data set
*     CLOAD    "observed" loads from the calibration data set
*     CLOADAML AMLE load estimates corresp to calibration data set
*     CV       covar matrix of std normals censored at dimensionless
*              threshold w.r.t. S**2
*     CV_M     max. likelihood estimates of the covariance matrix
*              (note: CV_M(NPAR+1) refers to covariance w.r.t. S**2)
*     IPPCCERR flag indicating results of PPCC calculations
*     LLK      log likelihood for AMLE regression model
*     LOGLK    function to deterimine LLK
*     PARAMLC  AMLE parameter estimates (concentration regression)
*     PARMLEC  MLE parameter estimates (concentration regression)
*     PLEV     significance level of PPCC test
*     PPCC     probability plot correlation coefficient
*     PVAL     P-values for the AMLE regression coefficients (load)
*     PVALC    P-values for the AMLE regression coefficients (conc.)
*     RESID    residuals from AMLE regression
*     RSQ      R-squared of the AMLE load regression
*     RSQC     R-squared of the AMLE concentration regression
*     SBIAS    bias of max likelihood estimates w.r.t. S
*     SCORR    serial correlation of the AMLE residuals
*     SCV      covar matrix of std normals censored at dimensionless
*              threshold w.r.t. S
*     SPPC     Schwarz Posterior Probability Criterion
*     SPPCMAX  maximum value of the SPPC
*     SSECONC  sum of squared errors, AMLE concentration regression  
*     SSELOAD  sum of squared errors, AMLE load regression  
*     SSQCOBS  sum of squared observed concentrations
*     SSQLOBS  sum of squared observed loads
*     STDDEV   standard deviation of the AMLE regression coefficients
*     SUMCAML  sum of estimated concentrations (AMLE)
*     SUMCOBS  sum of observed concentrations
*     SUMLAML  sum of estimated loads (AMLE)
*     SUMLOBS  sum of observed loads
*     XLIKE    value of the log-likelihood function, AMLE regression
*     YHAT     AMLE est. of log load corresponding to calibration data
*     YHATC    AMLE est. of log conc. corresponding to calibration data
*     Z        Z score for residuals
*
************************************************************************
      SUBROUTINE CALIBR(BCLAD,BESTMOD,CADDL,CDATETIM,CENSFLAG,CFACTOR,
     &                  CFLOW,CUNITSTR,DECTIME,DVNAME,IMODBEG,IMODEND,
     &                  LDOUT,LFACTOR,LQCENT,LUNITSTR,NADDL,NCENS,
     &                  NCONST,NEXPL,NOBSC,NOBSCI,NPAR,PARAML,PARLAD,
     &                  PARMLE,PERIOD,RVARMLE,TRANS,WFLAG,XLCAL,YD,YDC,
     &                  YLCAL,YLCAL2,YLCALC)
*     
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      LOGICAL WFLAG,CENSFLAG(*)
      INTEGER*4 BESTMOD,IMODBEG,IMODEND,LDOUT,NADDL,NCENS,NCONST,NEXPL,
     &          NOBSC,NOBSCI
      INTEGER*4 NPAR(*)
      DOUBLE PRECISION BCLAD,CFACTOR,LFACTOR,LQCENT,RVARMLE
      DOUBLE PRECISION CFLOW(*),DECTIME(*),PARLAD(*),PERIOD(*),YD(*),
     &                 YDC(*),YLCAL(*),YLCALC(*),YLCAL2(*)
      DOUBLE PRECISION CADDL(NADDL,*),PARAML(MAXPARMS+1,*),
     &                 PARMLE(MAXPARMS+1,*),XLCAL(NOBSCI,*)
      CHARACTER*(*) CUNITSTR,LUNITSTR
      CHARACTER*5 DVNAME(*),TRANS(*)
      CHARACTER*13 CDATETIM(*)
*
*     local vars
*
      INTEGER*4 I,BESTMOD2,IPPCCERR
      DOUBLE PRECISION AICMIN,LLK,PPCC,PLEV,RSQ,RSQC,SCORR,SPPCMAX,
     &                 SUMCOBS,SUMCAML,SUMLOBS,SUMLAML,SSQLOBS,SSQCOBS,
     &                 SSELOAD,SSECONC
      DOUBLE PRECISION AIC(NMODELS),BIAS(MAXPARMS+1),CCONC(NOBSC),
     &                 CCONCAML(NOBSC),CLOAD(NOBSC),CLOADAML(NOBSC),
     &                 PARAMLC(MAXPARMS+1),PARMLEC(MAXPARMS+1),
     &                 PVAL(MAXPARMS),PVALC(MAXPARMS),RESID(NOBSCI),
     &                 SBIAS(MAXPARMS+1),SPPC(NMODELS),XLIKE(NMODELS),
     &                 YHAT(NOBSCI),YHATC(NOBSCI),Z(NOBSCI)
      DOUBLE PRECISION CV(MAXPARMS+1,MAXPARMS+1),
     &                 CV_M(MAXPARMS+1,MAXPARMS+1),
     &                 SCV(MAXPARMS+1,MAXPARMS+1),
     &                 STDDEV(MAXPARMS+1,NMODELS)
*
*     function declaration
*
      DOUBLE PRECISION LOGLK
*
*     Calculate regression coefficients using Adjusted Maximum
*     Likelihood Estimation (AMLE).
*
*     For automated model selection, loop through the available
*     models (from IMODBEG to IMODEND); for user-selected models,
*     IMODBEG=IMODEND, and the loop is only executed once (see
*     additional comments in SETUPC).
*
*     Start by initializing the vector of explanatory variables, XLCAL
*     (S/R SETXL). Then deterimine the regression coefficients for
*     Maximum Likelihood estimation (PARMLE) and AMLE (PARAML)
*     (S/R AMLREG).  Using the AMLE results, calculate the log
*     likelihood (function LOGLK), the Akaike Information Criterion
*     (AIC), and the Schwarz Posterior Probability Criterion (SPPC).
*
*     While going through the loop, deterimine the best model based
*     on the Akaike Information Criteria (set BESTMOD equal to the
*     model with the lowest value of AIC) and the Schwarz Posterior
*     Probability Criteria (set BESTMOD2 equal to the model with the
*     highest value of SPPC). (Reference: Judge et al., 1988,
*     p. 848-849.)
*
      AICMIN = 9999.D99
      SPPCMAX = -9999.D99

      DO 30 I=IMODBEG,IMODEND
         CALL SETXL(I,NOBSC,LQCENT,CADDL,CFLOW,DECTIME,PERIOD,XLCAL,
     &              NOBSCI,NEXPL,DVNAME,TRANS,NADDL)
         CALL AMLREG(NOBSC,NPAR(I),XLCAL,YLCAL,YD,CENSFLAG,XLIKE(I),
     &               STDDEV(1,I),PARMLE(1,I),PARAML(1,I),BIAS,CV,
     &               SBIAS,SCV,CV_M,NOBSCI)
         LLK = LOGLK(NPAR(I),NOBSC,XLCAL,YLCAL,CENSFLAG,PARAML(1,I),
     &               DSQRT(PARAML(NPAR(I)+1,I)),NOBSCI)
         AIC(I) = -(2.D0/DBLE(NOBSC))
     &            * LLK+(2.D0*DBLE(NPAR(I))/DBLE(NOBSC))
         SPPC(I) = LLK-(0.5D0*DBLE(NPAR(I))*DLOG(DBLE(NOBSC)))
         IF (AIC(I).LT.AICMIN) THEN
            BESTMOD = I
            AICMIN = AIC(I)
         ENDIF
         IF (SPPC(I).GT.SPPCMAX) THEN
            BESTMOD2 = I
            SPPCMAX = SPPC(I)
         ENDIF
 30   CONTINUE
*
*     reinitialize the vector of explanatory variables, XLCAL, so
*     it corresponds to BESTMOD (prior to this call they correspond
*     to IMODEND due to loop above).  This is redundant for the case
*     of a user-selected model (in which case BESTMOD=IMODBEG=IMODEND
*     =MODNO).
*
      CALL SETXL(BESTMOD,NOBSC,LQCENT,CADDL,CFLOW,DECTIME,PERIOD,
     &           XLCAL,NOBSCI,NEXPL,DVNAME,TRANS,NADDL)
*
*     estimate AMLE loads and concentrations corresponding to the
*     observations in the calibration data set
*
      CALL ESTCAL(CCONCAML,CENSFLAG,CFACTOR,CFLOW,CLOADAML,LFACTOR,
     &            NOBSC,NOBSCI,NPAR(BESTMOD),XLCAL,YD,YLCAL)
*
*     calculate AMLE statistics and residuals for the best model
*
      CALL AMLSTAT(CENSFLAG,IPPCCERR,LDOUT,NOBSC,NOBSCI,NPAR(BESTMOD),
     &             PARAML(1,BESTMOD),PPCC,PLEV,PVAL,RESID,RSQ,SCORR,
     &             XLCAL,XLIKE(BESTMOD),YD,YHAT,YLCAL,YLCAL2,Z)
*
*     calculate the MLE residual variance associated with the best model
*
*     The mean square error of the regression (PARMLE(NPAR+1)) is a
*     biased estimate of the residual variance. Compute the residual
*     variance (RVARMLE) by adjusting the mean square error for the
*     degrees of freedom as suggested in Aitken (1981).
*
      RVARMLE = PARMLE(NPAR(BESTMOD)+1,BESTMOD)
     &       * (DBLE(NOBSC-NCENS)/DBLE(NOBSC-NCENS-NPAR(BESTMOD)))
*
*     Since maximum likelihood parameter estimates can be biased in the
*     presence of non-normally distributed residuals, repeat regression
*     using the least absolute deviation method (LAD).  LAD is resistant
*     to both non-normality and heteroscedasticity of the residuals, but
*     it is not as efficient as the maximum-likelihood method when the
*     residuals are normally distributed.  LAD is only included when the
*     calibration dataset is uncensored.
*
      IF (NCENS .EQ. 0)
     &   CALL LADREG(NPAR(BESTMOD),NOBSC,XLCAL,YLCAL,PARLAD,BCLAD,
     &               NOBSCI)
*
*     Calculate AMLE regression coefficients for a concentration
*     model with the same model form as BESTMOD.
*
      CALL AMLREG(NOBSC,NPAR(BESTMOD),XLCAL,YLCALC,YDC,CENSFLAG,
     &            XLIKE(BESTMOD),STDDEV(1,BESTMOD),PARMLEC,PARAMLC,
     &            BIAS,CV,SBIAS,SCV,CV_M,NOBSCI)
*
*     calculate AMLE statistics for concentration regression
*
      CALL AMLSTAT2(CENSFLAG,LDOUT,NOBSC,NOBSCI,NPAR(BESTMOD),PARAMLC,
     &              PVALC,RSQC,XLCAL,XLIKE(BESTMOD),YDC,YHATC,YLCALC)
*
*     write residual output file and calculate sums for bias diagnostics
*
      CALL OUTRES(CCONC,CCONCAML,CDATETIM,CENSFLAG,CFLOW,CLOAD,CLOADAML,
     &            DECTIME,LDOUT,LFACTOR,NCONST,NOBSC,RESID,SSECONC,
     &            SSELOAD,SSQCOBS,SSQLOBS,SUMCAML,SUMCOBS,SUMLAML,
     &            SUMLOBS,YHAT,YHATC,YLCAL,YLCALC,Z)
*
*     output regression equation (load) and bias diagnostics
*
      CALL OUTEQN(AIC,BESTMOD,BESTMOD2,CLOAD,CLOADAML,DVNAME,IMODBEG,
     &            IMODEND,IPPCCERR,LDOUT,LUNITSTR,NCENS,NEXPL,NOBSC,
     &            NOBSCI,NPAR(BESTMOD),PARAML,PARLAD,PARMLE,PPCC,PLEV,
     &            PVAL,RSQ,RVARMLE,SCORR,SPPC,SSELOAD,SSQLOBS,STDDEV,
     &            SUMLAML,SUMLOBS,TRANS,WFLAG,XLCAL)
*
*     output regression equation (concentration) and bias diagnostics
*
      CALL OUTEQN2(BESTMOD,CCONC,CCONCAML,CUNITSTR,DVNAME,LDOUT,NEXPL,
     &             NOBSC,NPAR(BESTMOD),PARAMLC,PVALC,RSQC,SSECONC,
     &             SSQCOBS,STDDEV,SUMCAML,SUMCOBS,TRANS)

      RETURN
      END
