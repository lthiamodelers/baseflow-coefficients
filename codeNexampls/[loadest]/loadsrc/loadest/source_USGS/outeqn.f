************************************************************************
*
*     Subroutine OUTEQN                              Called by: CALIBR
*
*     print out load regression equation
*
************************************************************************
      SUBROUTINE OUTEQN(AIC,BESTMOD,BESTMOD2,CLOAD,CLOADAML,DVNAME,
     &                  IMODBEG,IMODEND,IPPCCERR,LDOUT,LUNITSTR,NCENS,
     &                  NEXPL,NOBSC,NOBSCI,NPAR,PARAML,PARLAD,PARMLE,
     &                  PPCC,PLEV,PVAL,RSQ,RVARMLE,SCORR,SPPC,SSELOAD,
     &                  SSQLOBS,STDDEV,SUMLAML,SUMLOBS,TRANS,WFLAG,
     &                  XLCAL)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      LOGICAL WFLAG
      INTEGER*4 BESTMOD,BESTMOD2,IPPCCERR,IMODBEG,IMODEND,LDOUT,NCENS,
     &          NEXPL,NOBSC,NOBSCI,NPAR
      DOUBLE PRECISION PPCC,PLEV,RSQ,RVARMLE,SCORR,SSELOAD,SSQLOBS,
     &                 SUMLAML,SUMLOBS
      DOUBLE PRECISION AIC(*),CLOAD(*),CLOADAML(*),PARLAD(*),PVAL(*),
     &                 SPPC(*)
      DOUBLE PRECISION PARMLE(MAXPARMS+1,*),PARAML(MAXPARMS+1,*),
     &                 STDDEV(MAXPARMS+1,*),XLCAL(NOBSCI,*)
      CHARACTER*(*) LUNITSTR
      CHARACTER*5 DVNAME(*),TRANS(*)
*
*     local vars
*
      INTEGER*4 I,K
      CHARACTER*2 COEFSTR(MAXPARMS) /'a0','a1','a2','a3','a4','a5','a6'/
*
*     function declaration
*
      DOUBLE PRECISION CORR
*
*     format statements
*
 1000 FORMAT(//,' Model Evaluation Criteria Based on AMLE Results',/,
     &       1X,47('-'),//,' Model #',5X,'AIC',11X,'SPPC',/,1X,
     &       34('-'))
 1100 FORMAT(1X,I2,2(8X,F8.3))
 1200 FORMAT(/,' Model #',I2,' selected',/)
 1300 FORMAT(//,' Akaike Information Criterion (AIC) and ',
     &       'Schwarz Posterior Probability',/,' Criteria (SPPC) ',
     &       'did not select same best fit model.  Model #',I2,/,
     &       ' selected on basis of AIC. (Model #',I2,
     &       ' would have been selected based on SPPC)',/)
 2000 FORMAT(/,' Selected Model:',/,1X,15('-'),/)
 2001 FORMAT(' Ln(Load) = a0 + a1 LnQ')
 2002 FORMAT(' Ln(Load) = a0 + a1 LnQ + a2 LnQ^2')
 2003 FORMAT(' Ln(Load) = a0 + a1 LnQ + a2 dtime')
 2004 FORMAT(' Ln(Load) = a0 + a1 LnQ + a2 Sin(2 pi dtime) + a3 ',
     &       'Cos(2 pi dtime)')
 2005 FORMAT(' Ln(Load) = a0 + a1 LnQ + a2 LnQ^2 + a3 ',
     &       'dtime')
 2006 FORMAT(' Ln(Load) = a0 + a1 LnQ + a2 LnQ^2 + a3 ',
     &       'Sin(2 pi dtime) + a4 Cos(2 pi dtime)')
 2007 FORMAT(' Ln(Load) = a0 + a1 LnQ + a2 Sin(2 pi dtime) + a3 ',
     &       'Cos(2 pi dtime) + a4 dtime')
 2008 FORMAT(' Ln(Load) = a0 + a1 LnQ + a2 LnQ^2 + a3 ',
     &       'Sin(2 pi dtime) + a4 Cos(2 pi dtime)',//,12X,'+ a5 dtime')
 2009 FORMAT(' Ln(Load) = a0 + a1 LnQ + a2 LnQ^2 + a3 Sin(2 pi dtime)',
     &       ' + a4 Cos(2 pi dtime)',//,12X,'+ a5 dtime ',
     &       '+ a6 dtime^2')
 2010 FORMAT(' Ln(Load) = a0 + a1 per + a2 LnQ + a3 ',
     &       'LnQ*per')
 2011 FORMAT(' Ln(Load) = a0 + a1 per + a2 LnQ + a3 ',
     &       'LnQ*per + a4 LnQ^2',//,12X,'+ a5 ',
     &       'LnQ^2*per')
 2012 FORMAT(' Ln(Load) =   a0')
 2020 FORMAT(12X,'+ a',I1,1X,A5)
 2021 FORMAT(12X,'+ a',I1,1X,A5,'^2')
 2022 FORMAT(12X,'+ a',I1,1X,'sqrt(',A5,')')
 2023 FORMAT(12X,'+ a',I1,1X,'LnQ')
 2024 FORMAT(12X,'+ a',I1,1X,'ln(',A5,')')
 2025 FORMAT(12X,'+ a',I1,1X,'LnQ^2')
 2026 FORMAT(12X,'+ a',I1,1X,'ln(',A5,')^2')
 2027 FORMAT(12X,'+ a',I1,1X,'sin(2*Pi*',A5,')')
 2028 FORMAT(12X,'+ a',I1,1X,'sin(4*Pi*',A5,')')
 2029 FORMAT(12X,'+ a',I1,1X,'sin(6*Pi*',A5,')')
 2030 FORMAT(12X,'+ a',I1,1X,'cos(2*Pi*',A5,')')
 2031 FORMAT(12X,'+ a',I1,1X,'cos(4*Pi*',A5,')')
 2032 FORMAT(12X,'+ a',I1,1X,'cos(6*Pi*',A5,')')
 2100 FORMAT(/' where:',/,7X,'Load  = constituent load [kg/d]',/,7X,
     &       'LnQ   = Ln(Q) - center of Ln(Q)')
 2110 FORMAT(7X,'dtime = decimal time - center of decimal time')
 2120 FORMAT(7X,'per   = period; 1 or 0 depending on defined period')
 3000 FORMAT(//,7X,'Model Coefficients',//,7(8X,A2))
 3100 FORMAT(7X,7(A10))
 3200 FORMAT(1X,A4,1X,7(F8.4,2X))
 4000 FORMAT(//,' AMLE Regression Statistics',/,1X,26('-'),/,
     &       ' R-Squared [%]                  : ',F5.2,/,
     &       ' Residual Variance              : ',F6.4,/,
     &       ' Serial Correlation of Residuals: ',F6.4)
 4100 FORMAT(' Prob. Plot Corr. Coeff. (PPCC) : ',F6.4,/,
     &       ' Significance Level of PPCC Test: ',1PE9.3)
 4200 FORMAT(/,' The calibration data set includes loads that are ',
     &       'censored ',/,' at multiple levels. The PPCC significance',
     &       ' level is approximate.')
 4300 FORMAT(/,' Constant uncensored values; Probability Plot ',
     &       'Correlation Coefficient not calculated')
 4400 FORMAT(/,' Error in calculation of Q-normal; Probability Plot ',
     &       'Correlation Coefficient not calculated')
 4500 FORMAT(/,' More than 5000 observations; Probability Plot ',
     &       'Correlation Coefficient not calculated')
 5000 FORMAT(//,' Coeff.',4X,'Std.Dev.',4X,'t-ratio',6X,'P Value',/,1X,
     &       44('-'))
 5100 FORMAT(1X,A2,8X,0PF6.4,6X,0PF7.2,6X,1PE9.3)
 5200 FORMAT(1X,A2,8X,0PF6.4,6X,0PF7.2,6X,'See WARNING')
 5300 FORMAT(//,' Correlation Between Explanatory Variables',/,1X,
     &       41('-'),//,7X,'Explanatory variable corresponding to:',
     &       //,7(8X,A2))
 5400 FORMAT(//,' Additional Regression Statistics',/,1X,32('-'),/,
     &       ' MLE Residual Variance:',1X,F6.4,/)
 5500 FORMAT(//,' Comparison of Observed and Estimated Loads',/,
     &       1X,42('-'),
     &       /,'   The summary statistics and bias diagnostics ',
     &         'presented below are based',
     &       /,' on a comparison of observed and estimated loads for ',
     &         'all dates/times within ',
     &       /,' the calibration data set. Although this comparison ',
     &         'does not directly ',
     &       /,' address errors in load estimation for unsampled ',
     &         'dates/times, large ',
     &       /,' discrepancies between observed and estimated loads ',
     &         'are indicative of a ',
     &       /,' poor model fit. Additional details and warnings are ',
     &         'provided below.',
     &       //,' Note: The comparison that follows uses a ',
     &         'concentration equal to 1/2 the',
     &       /,' detection limit when an observation ',
     &         'is censored. The summary stats and',
     &       /,' bias diagnostics are therefore slightly inaccurate ',
     &         'for censored data sets.')
 5600 FORMAT(//,' Summary Stats: Est. and Obs. Loads in ',A9,/,1X,
     &       50('-'),/,17X,'25th',14X,'75th',5X,'90th',5X,'95th',5X,
     &       '99th',/,8X,'Min.',5X,'Pct',6X,'Med.',5X,'Pct',
     &       3(6X,'Pct'),6X,'Max.',/,8X,71('-'))
 5700 FORMAT(//,' Bias Diagnostics',/,1X,16('-'),/,' Bp [%] ',F9.3,
     &       /,' PLR',4X,F9.3,/,' E',6X,F9.3,//,' where:',//,4X,
     &       'Bp',T9,'Load Bias in Percent',/,T9,'Positive '
     &       '(negative) values indicate over (under) ',
     &       'estimation.',/,T9,'***The model should not be ',
     &       'used when the + or - bias exceeds 25%***',
     &       //,4X,'PLR',T9,'Partial Load Ratio',/,T9,'Sum of',
     &       ' estimated loads divided by sum of observed loads.',/,T9,
     &       'Values > 1 indicate overestimation; values < 1 indicate ',
     &       'underestimation.',/,T9,'PLR = (Bp + 100) / 100',//,4X,
     &       'E',T9,'Nash Sutcliffe Efficiency Index',/,T9,'E ranges',
     &       ' from -infinity to 1.0',/,T9,'E = 1; a perfect',
     &       ' fit to observed data.',/,T9,'E = 0; model ',
     &       'estimates are as accurate as the mean of observed ',
     &       'data.',/,T9,'E < 0; the observed mean is a',
     &       ' better estimate than the model estimates.',/)
 5800 FORMAT(' IMPORTANT WARNING:',//,' Load Bias (Bp) Exceeds + or',
     &       ' - 25%',/,' THE CALIBRATED MODEL SHOULD NOT BE USED FOR ',
     &       'LOAD ESTIMATION',//)
 5900 FORMAT(' IMPORTANT WARNING:',//,' Nash Sutcliffe Efficiency ',
     &       'Index (E) is less than zero.',/,' The observed mean is ',
     &       'a better estimate than the model estimates.',/,' THE ',
     &       'CALIBRATED MODEL SHOULD NOT BE USED FOR LOAD ESTIMATION',
     &       //)
 6000 FORMAT(/,' NOTE: Additional information on model calibration is ',
     &       'included in the',/,7X,'residual output file. LOADEST ',
     &       'users should conduct a thorough',/,7X,'residuals ',
     &       'analysis using the data contained therein (checks for',/,
     &       7X,'heteroscedasticity and non-normality).  Example ',
     &       'residual plots',/,7X,'are shown in Figures 7, 8, 9, and',
     &       ' 17 of the LOADEST documentation',/,7X,
     &       '(Runkel et al., 2004).')
*
*     output model selection criteria and the selected model number;
*     include a warning message if the 2 different criteria used for
*     determining the best model do not select the same best model.
*     If the regression model is user defined (MODNO=99), BESTMOD will
*     equal NMODELS, as a user defined model is the last model - output
*     '99' rather than BESTMOD in this case.
*
      WRITE(LDOUT,1000)
      DO 10 I=IMODBEG,IMODEND
         IF (BESTMOD .EQ. NMODELS) THEN
            WRITE(LDOUT,1100) 99,AIC(I),SPPC(I)
         ELSE
            WRITE(LDOUT,1100) I,AIC(I),SPPC(I)
         ENDIF
 10   CONTINUE

      IF (BESTMOD.EQ.BESTMOD2) THEN
         IF (BESTMOD .EQ. NMODELS) THEN
            WRITE(LDOUT,1200) 99
         ELSE
            WRITE(LDOUT,1200) BESTMOD
         ENDIF
      ELSE
         WRITE(LDOUT,1300) BESTMOD,BESTMOD2
      ENDIF
*
*     output model equation
*
      WRITE(LDOUT,2000)
      IF (BESTMOD.EQ.1) THEN
         WRITE(LDOUT,2001)
         WRITE(LDOUT,2100)
      ELSEIF (BESTMOD.EQ.2) THEN
         WRITE(LDOUT,2002)
         WRITE(LDOUT,2100)
      ELSEIF (BESTMOD.EQ.3) THEN
         WRITE(LDOUT,2003)
         WRITE(LDOUT,2100)
         WRITE(LDOUT,2110)
      ELSEIF (BESTMOD.EQ.4) THEN
         WRITE(LDOUT,2004)
         WRITE(LDOUT,2100)
         WRITE(LDOUT,2110)
      ELSEIF (BESTMOD.EQ.5) THEN
         WRITE(LDOUT,2005)
         WRITE(LDOUT,2100)
         WRITE(LDOUT,2110)
      ELSEIF (BESTMOD.EQ.6) THEN
         WRITE(LDOUT,2006)
         WRITE(LDOUT,2100)
         WRITE(LDOUT,2110)
      ELSEIF (BESTMOD.EQ.7) THEN
         WRITE(LDOUT,2007)
         WRITE(LDOUT,2100)
         WRITE(LDOUT,2110)
      ELSEIF (BESTMOD.EQ.8) THEN
         WRITE(LDOUT,2008)
         WRITE(LDOUT,2100)
         WRITE(LDOUT,2110)
      ELSEIF (BESTMOD.EQ.9) THEN
         WRITE(LDOUT,2009)
         WRITE(LDOUT,2100)
         WRITE(LDOUT,2110)
      ELSEIF (BESTMOD.EQ.10) THEN
         WRITE(LDOUT,2010)
         WRITE(LDOUT,2100)
         WRITE(LDOUT,2120)
      ELSEIF (BESTMOD.EQ.11) THEN
         WRITE(LDOUT,2011)
         WRITE(LDOUT,2100)
         WRITE(LDOUT,2120)
      ELSEIF (BESTMOD.EQ.NMODELS) THEN
         WRITE(LDOUT,2012)
         DO 20 I=1,NEXPL
            IF (TRANS(I) .EQ. 'NONE') THEN
               WRITE(LDOUT,2020) I,DVNAME(I)
            ELSEIF(TRANS(I) .EQ. 'SQ') THEN 
               WRITE(LDOUT,2021) I,DVNAME(I)
            ELSEIF(TRANS(I) .EQ. 'SQRT') THEN 
               WRITE(LDOUT,2022) I,DVNAME(I)
            ELSEIF(TRANS(I) .EQ. 'LN') THEN 
               IF (DVNAME(I) .EQ. 'Q') THEN
                  WRITE(LDOUT,2023) I
               ELSE
                  WRITE(LDOUT,2024) I,DVNAME(I)
               ENDIF
            ELSEIF(TRANS(I) .EQ. 'LNSQ') THEN 
               IF (DVNAME(I) .EQ. 'Q') THEN
                  WRITE(LDOUT,2025) I
               ELSE
                  WRITE(LDOUT,2026) I,DVNAME(I)
               ENDIF
            ELSEIF(TRANS(I) .EQ. 'SIN2P') THEN 
               WRITE(LDOUT,2027) I,DVNAME(I)
            ELSEIF(TRANS(I) .EQ. 'SIN4P') THEN 
               WRITE(LDOUT,2028) I,DVNAME(I)
            ELSEIF(TRANS(I) .EQ. 'SIN6P') THEN 
               WRITE(LDOUT,2029) I,DVNAME(I)
            ELSEIF(TRANS(I) .EQ. 'COS2P') THEN 
               WRITE(LDOUT,2030) I,DVNAME(I)
            ELSEIF(TRANS(I) .EQ. 'COS4P') THEN 
               WRITE(LDOUT,2031) I,DVNAME(I)
            ELSEIF(TRANS(I) .EQ. 'COS6P') THEN
               WRITE(LDOUT,2032) I,DVNAME(I)
            ENDIF
 20      CONTINUE
         WRITE(LDOUT,2100)
         WRITE(LDOUT,2110)
      ENDIF

      WRITE(LDOUT,3000) (COEFSTR(I),I=1,NPAR)
      WRITE(LDOUT,3100) ('----------',I=1,NPAR)
      WRITE(LDOUT,3200) 'AMLE',(PARAML(I,BESTMOD),I=1,NPAR)
      WRITE(LDOUT,3200) 'MLE ',(PARMLE(I,BESTMOD),I=1,NPAR)
      IF (NCENS .EQ. 0)
     &   WRITE(LDOUT,3200) 'LAD ',(PARLAD(I),I=1,NPAR)

*
*     output AMLE regression statistics
*
      WRITE(LDOUT,4000) RSQ,PARAML(NPAR+1,BESTMOD),SCORR

      IF (IPPCCERR .EQ. 0) THEN
         WRITE(LDOUT,4100) PPCC,PLEV
      ELSEIF (IPPCCERR .EQ. -1) THEN
         WRITE(LDOUT,4100) PPCC,PLEV
         WRITE(LDOUT,4200)
      ELSEIF (IPPCCERR .EQ. 2) THEN
         WRITE(LDOUT,4300)
      ELSEIF (IPPCCERR .EQ. 3) THEN
         WRITE(LDOUT,4400)
      ELSEIF (IPPCCERR .EQ. 4) THEN
         WRITE(LDOUT,4500)
      ENDIF
*
*     output AMLE regression statistics for model coefficients
*
      WRITE(LDOUT,5000)
      DO 30 I=1,NPAR
         IF (PVAL(I) .LT. 9999999.99) THEN
            WRITE(LDOUT,5100) COEFSTR(I),STDDEV(I,BESTMOD),
     &                        PARAML(I,BESTMOD)/STDDEV(I,BESTMOD),
     &                        PVAL(I)
         ELSE
            WRITE(LDOUT,5200) COEFSTR(I),STDDEV(I,BESTMOD),
     &                        PARAML(I,BESTMOD)/STDDEV(I,BESTMOD)
         ENDIF
 30   CONTINUE
*
*     output correlations among explanatory variables
*
      IF (NPAR .GT. 2) THEN
         WRITE(LDOUT,5300) (COEFSTR(I),I=2,NPAR-1)
         WRITE(LDOUT,3100) ('----------',I=2,NPAR-1)
         DO 40 I=3,NPAR
            WRITE(LDOUT,3200) COEFSTR(I),(CORR(NOBSC,XLCAL(1,I),
     &                           XLCAL(1,K)),K=2,I-1)
 40      CONTINUE
      ENDIF
*
*     output the MLE residual variance
*
      WRITE(LDOUT,5400) RVARMLE
*
*     start a new section w/i Constituent Output File Part Ia; this new
*     section compares observed and estimated loads on the days/times
*     for which observations are available (the calibration data set).
*
      WRITE(LDOUT,5500)
*
*     print summary statistics for estimated loads
*
      WRITE(LDOUT,5600) LUNITSTR
      CALL OUTSUMMC(NOBSC,CLOADAML,CLOAD,LDOUT)
*
*     print bias diagnostics, including:
*
*     Bp   Load Bias in Percent (email from Bob Hirsch, 16 Jan 2013)
*     PLR  Partial Load Ratio (Stenback et al., 2011)
*     E    Nash-Sutcliffe Efficiency Index (Nash & Sutcliffe, 1970)
*
*     Issue WARNINGS if |Bp| > 25% or  E < 0
*
      WRITE(LDOUT,5700) 100.D0*(SUMLAML/SUMLOBS - 1.D0),
     &                  SUMLAML/SUMLOBS,
     &                  1.D0-SSELOAD/(SSQLOBS - SUMLOBS*SUMLOBS/NOBSC)

      WFLAG = .FALSE.

      IF (ABS(100.D0*(SUMLAML/SUMLOBS-1.D0)).GT.25.D0) THEN
         WFLAG = .TRUE.
         CALL JROGER(LDOUT)
         WRITE(LDOUT,5800)
      ENDIF

      IF (1.D0-SSELOAD/(SSQLOBS-SUMLOBS*SUMLOBS/NOBSC).LT.0.D0) THEN
         WFLAG = .TRUE.
         CALL JROGER(LDOUT)
         WRITE(LDOUT,5900)
      ENDIF
*
*     write final note in regard to residual analyses
*
      WRITE(LDOUT,6000)

      RETURN
      END
