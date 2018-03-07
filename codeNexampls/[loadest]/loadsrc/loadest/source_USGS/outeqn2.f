************************************************************************
*
*     Subroutine OUTEQN2                              Called by: CALIBR
*
*     print out concentration regression equation
*
************************************************************************
      SUBROUTINE OUTEQN2(BESTMOD,CCONC,CCONCAML,CUNITSTR,DVNAME,LDOUT,
     &                   NEXPL,NOBSC,NPAR,PARAMLC,PVALC,RSQC,SSECONC,
     &                   SSQCOBS,STDDEV,SUMCAML,SUMCOBS,TRANS)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 BESTMOD,LDOUT,NEXPL,NOBSC,NPAR
      DOUBLE PRECISION RSQC,SSECONC,SSQCOBS,SUMCAML,SUMCOBS
      DOUBLE PRECISION CCONC(*),CCONCAML(*),PARAMLC(*),PVALC(*)
      DOUBLE PRECISION STDDEV(MAXPARMS+1,*)
      CHARACTER*(*) CUNITSTR
      CHARACTER*5 DVNAME(*),TRANS(*)
*
*     local vars
*
      INTEGER*4 I
      CHARACTER*2 COEFSTR(MAXPARMS) /'a0','a1','a2','a3','a4','a5','a6'/
*
*     format statements
*
 1000 FORMAT(///,1X,70('-'),//,1X,'Constituent Output File Part Ib: ',
     &       'Calibration (Concentration Regression)',//,1X,70('-'),/)
 1200 FORMAT(//,' AMLE Regression Statistics',/,1X,26('-'))
 1400 FORMAT(/,' Model #',I2,' was selected for the load regression ',
     &       '(PART Ia) and is used here:',/)
 2001 FORMAT(' Ln(Conc) = a0 + a1 LnQ')
 2002 FORMAT(' Ln(Conc) = a0 + a1 LnQ + a2 LnQ^2')
 2003 FORMAT(' Ln(Conc) = a0 + a1 LnQ + a2 dtime')
 2004 FORMAT(' Ln(Conc) = a0 + a1 LnQ + a2 Sin(2 pi dtime) + a3 ',
     &       'Cos(2 pi dtime)')
 2005 FORMAT(' Ln(Conc) = a0 + a1 LnQ + a2 LnQ^2 + a3 ',
     &       'dtime')
 2006 FORMAT(' Ln(Conc) = a0 + a1 LnQ + a2 LnQ^2 + a3 ',
     &       'Sin(2 pi dtime) + a4 Cos(2 pi dtime)')
 2007 FORMAT(' Ln(Conc) = a0 + a1 LnQ + a2 Sin( pi dtime) + a3 ',
     &       'Cos(2 pi dtime) + a4 dtime')
 2008 FORMAT(' Ln(Conc) = a0 + a1 LnQ + a2 LnQ^2 + a3 ',
     &       'Sin(2 pi dtime) + a4 Cos(2 pi dtime)',//,12X,'+ a5 dtime')
 2009 FORMAT(' Ln(Conc) = a0 + a1 LnQ + a2 LnQ^2 + a3 Sin(2 pi dtime)',
     &       ' + a4 Cos(2 pi dtime)',//,12X,'+ a5 dtime ',
     &       '+ a6 dtime^2')
 2010 FORMAT(' Ln(Conc) = a0 + a1 per + a2 LnQ + a3 ',
     &       'LnQ*per')
 2011 FORMAT(' Ln(Conc) = a0 + a1 per + a2 LnQ + a3 ',
     &       'LnQ*per + a4 LnQ^2',//,12X,'+ a5 ',
     &       'LnQ^2*per')
 2012 FORMAT(' Ln(Conc) =   a0')
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
 2100 FORMAT(/' where:',/,7X,'Conc  = constituent concentration',/,7X,
     &       'LnQ   = Ln(Q) - center of Ln(Q)')
 2110 FORMAT(7X,'dtime = decimal time - center of decimal time')
 2120 FORMAT(7X,'per   = period; 1 or 0 depending on defined period')
 3000 FORMAT(//,' Concentration Regression Results',/,1X,32('-'),/,
     &       ' R-Squared [%]                  : ',F5.2,/,
     &       ' Residual Variance              : ',F6.4,//,
     &       ' Coeff.',4X,'Value',9X,'Std.Dev.',5X,'t-ratio',5X,
     &       'P Value',/,1X,58('-'))
 3100 FORMAT(1X,A2,7X,0PF7.4,8X,0PF6.4,5X,0PF7.2,7X,1PE9.3)
 3200 FORMAT(1X,A2,7X,0PF7.4,8X,0PF6.4,5X,0PF7.2,7X,'See WARNING')
 4600 FORMAT(//,' Comparison of Observed and Estimated Concentrations',
     &       /,1X,51('-'),
     &       /,'   The summary statistics and bias diagnostics ',
     &         'presented below are',
     &       /,' based on a comparison of observed and estimated ',
     &         'concentrations for',
     &       /,' all dates/times within the calibration data set. ',
     &         'Although this'
     &       /,' comparison does not directly address errors in ',
     &         'concentration estimation',
     &       /,' for unsampled dates/times, large discrepancies',
     &         ' between observed and',
     &       /,' estimated concentrations are indicative of a poor ',
     &         'model fit. Additional',
     &       /,' details and warnings are ',
     &         'provided below.',
     &       //,' Note: The comparison that follows uses a ',
     &         'concentration equal to 1/2 the',
     &       /,' detection limit when an observation ',
     &         'is censored. The summary stats and',
     &       /,' bias diagnostics are therefore slightly inaccurate ',
     &         'for censored data sets.')
 4700 FORMAT(//,' Summary Stats: Est. and Obs. Concentrations in ',A9,/,
     &       1X,59('-'),/,17X,'25th',14X,'75th',5X,'90th',5X,'95th',5X,
     &       '99th',/,8X,'Min.',5X,'Pct',6X,'Med.',5X,'Pct',
     &       3(6X,'Pct'),6X,'Max.',/,7X,72('-'))
 4800 FORMAT(//,' Bias Diagnostics',/,1X,16('-'),/,' Bp [%] ',F9.3,
     &       /,' PCR',4X,F9.3,/,' E',6X,F9.3,//,' where:',//,4X,
     &       'Bp',T9,'Concentration Bias in Percent',/,T9,'Positive '
     &       '(negative) values indicate over (under) ',
     &       'estimation.',/,T9,'***The model should not be ',
     &       'used when the + or - bias exceeds 25%***',
     &       //,4X,'PCR',T9,'Partial Concentration Ratio',/,T9,'Sum of',
     &       ' est. concentrations divided by sum of obs. ',
     &       'concentrations.',/,T9,'Values > 1 indicate over',
     &       'estimation; values < 1 indicate underestimation.',/,T9,
     &       'PCR = (Bp + 100) / 100',//,4X,'E',T9,
     &       'Nash Sutcliffe Efficiency Index',/,T9,'E ranges',
     &       ' from -infinity to 1.0',/,T9,'E = 1; a perfect',
     &       ' fit to observed data.',/,T9,'E = 0; model ',
     &       'estimates are as accurate as the mean of observed ',
     &       'data.',/,T9,'E < 0; the observed mean is a',
     &       ' better estimate than the model estimates.',/)
 4900 FORMAT(' IMPORTANT WARNING:',//,' Concentration Bias (Bp) ',
     &       'Exceeds + or - 25%',/,' THE CALIBRATED MODEL SHOULD NOT ',
     &       'BE USED FOR CONCENTRATION ESTIMATION',//)
 5000 FORMAT(' IMPORTANT WARNING:',//,' Nash Sutcliffe Efficiency ',
     &       'Index (E) is less than zero.',/,' The observed mean is ',
     &       'a better estimate than the model estimates.',/,' THE ',
     &       'CALIBRATED MODEL SHOULD NOT BE USED FOR CONCENTRATION ',
     &       'ESTIMATION',//)
 5100 FORMAT(/,' NOTE: Additional information on model calibration is ',
     &       'included in the',/,7X,'residual output file. LOADEST ',
     &       'users should conduct a thorough',/,7X,'residuals ',
     &       'analysis using the data contained therein (checks for',/,
     &       7X,'heteroscedasticity and non-normality).  Example ',
     &       'residual plots',/,7X,'are shown in Figures 7, 8, 9, and',
     &       ' 17 of the LOADEST documentation',/,7X,
     &       '(Runkel et al., 2004).')
*
*     output selected model number; If the regression model is user
*     defined (MODNO=99), BESTMOD will equal NMODELS, as a user defined
*     model is the last model - output '99' rather than BESTMOD in this
*     case.
*
      WRITE(LDOUT,1000)
      WRITE(LDOUT,1200)
      IF (BESTMOD .EQ. NMODELS) THEN
         WRITE(LDOUT,1400) 99
      ELSE
         WRITE(LDOUT,1400) BESTMOD
      ENDIF
*
*     output model equation
*
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
*
*     output AMLE regression statistics
*
      WRITE(LDOUT,3000) RSQC,PARAMLC(NPAR+1)

      DO 30 I=1,NPAR
         IF (PVALC(I) .LT. 9999999.99) THEN
            WRITE(LDOUT,3100) COEFSTR(I),PARAMLC(I),STDDEV(I,BESTMOD),
     &                        PARAMLC(I)/STDDEV(I,BESTMOD),PVALC(I)
         ELSE
            WRITE(LDOUT,3200) COEFSTR(I),PARAMLC(I),STDDEV(I,BESTMOD),
     &                           PARAMLC(I)/STDDEV(I,BESTMOD)
         ENDIF
 30   CONTINUE
*
*     start a new section w/i Constituent Output File Part Ib; this new
*     section compares observed and estimated concentrations on the
*     days/times for which observations are available (the calibration
*     data set).
*
      WRITE(LDOUT,4600)
*
*     print summary statistics for estimated concentrations
*
      WRITE(LDOUT,4700) CUNITSTR
      CALL OUTSUMMC(NOBSC,CCONCAML,CCONC,LDOUT)
*
*     print bias diagnostics, including:
*
*     Bp   Concentration Bias in % (email from B Hirsch, 16Jan2013)
*     PCR  Partial Concentration Ratio (Stenback et al., 2011)
*     E    Nash-Sutcliffe Efficiency Index (Nash & Sutcliffe, 1970)
*
*     Issue WARNINGS if |Bp| > 25% or  E < 0
*
      WRITE(LDOUT,4800) 100.D0*(SUMCAML/SUMCOBS - 1.D0),
     &                  SUMCAML/SUMCOBS,
     &                  1.D0-SSECONC/(SSQCOBS - SUMCOBS*SUMCOBS/NOBSC)

      IF (ABS(100.D0*(SUMCAML/SUMCOBS-1.D0)).GT.25.D0) THEN
         CALL JROGER(LDOUT)
         WRITE(LDOUT,4900)
      ENDIF

      IF (1.D0-SSECONC/(SSQCOBS - SUMCOBS*SUMCOBS/NOBSC).LT.0.D0) THEN
         CALL JROGER(LDOUT)
         WRITE(LDOUT,5000)
      ENDIF
*
*     write final note in regard to residual analyses
*
      WRITE(LDOUT,5100)

      RETURN
      END
