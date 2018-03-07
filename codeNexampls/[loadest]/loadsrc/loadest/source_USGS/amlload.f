************************************************************************
*
*     Subroutine AMLLOAD                            Called by: ESTLOAD
*
*     calculate AMLE loads for entire estimation period, user-defined
*     seasons, and individual months
*
*     local vars
*     ----------
*     LOADAML  mean AMLE load (entire period, season, or indiv. month)
*     LOW95    lower 95% confidence limit for LOADAML
*     MONSTR   string denoting month of year
*     NOBS     no. of observations in season or month
*     OLDMONTH tracking variable used to determine when month ends
*     OLDYEAR  tracking variable used to determine when month ends
*     SEOPT2   standard error option for use by AMLLOAD2
*     SEP      standard error of prediction for LOADAML
*     SXLEST   subset of XLEST for seasonal or monthly loads
*     TSTR     string denoting time period covered by load estimate
*     UP95     upper 95% confidence limit for LOADAML
*     VARAML   variance of LOADAML
*
************************************************************************
      SUBROUTINE AMLLOAD(CENSFLAG,EDATE,LDOPT,LDOUT,LFACTOR,NOBSC,
     &                   NOBSCI,NOBSE,NPAR,NSEAS,PARAML,PLOADAML,SBEG,
     &                   SEND,SEOPT,XLCAL,XLEST,YD,YLCAL2)
*     
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      LOGICAL CENSFLAG(*)
      INTEGER*4 LDOPT,LDOUT,NOBSC,NOBSCI,NOBSE,NPAR,NSEAS,SEOPT
      DOUBLE PRECISION LFACTOR,PLOADAML(*),PARAML(*),YD(*),YLCAL2(*)
      DOUBLE PRECISION XLCAL(NOBSCI,*),XLEST(NOBSE,*)
      CHARACTER*4 SBEG(*),SEND(*)
      CHARACTER*8 EDATE(*)
*
*     local vars
*
      INTEGER*4 I,IMO,IYR,NOBS,OLDMONTH,OLDYEAR,SEOPT2
      DOUBLE PRECISION LOADAML,LOW95,SEP,UP95,VARAML
      DOUBLE PRECISION SXLEST(NOBSE,MAXPARMS)
      CHARACTER*13 TSTR
      CHARACTER*4 MONSTR(12) /'Jan.','Feb.','Mar.','Apr.','May ','June',
     &                        'July','Aug.','Sep.','Oct.','Nov.','Dec.'/
*
*     local var, not used but needed in CALLS
*
      DOUBLE PRECISION PLOAD(NOBSE)
*
*     function declaration
*
      INTEGER*4 GETINT
*
*     format statements
*
 1000 FORMAT(//,T15,'AMLE Load Estimates',/,T15,19('-'),//,T34,
     &       '95% Conf.Intervals',/,T26,'Mean',T34,18('-'),3X,
     &       'Std Error',3X,'Standard',/,T18,'N',T26,'Load',T36,'Lower',
     &       T47,'Upper',T54,'Prediction',T70,'Error',/,T15,60('-'))
 2000 FORMAT('Season',I3,'    ')
 3000 FORMAT(A4,1X,I4)
 4000 FORMAT(//,'Note: A linear approximation has been used to ',
     &       'calculate the AMLE standard',/,6X,'error (SEOPT equals ',
     &       I1,').  More accurate estimates of the standard',/,6X,
     &       'error, standard error of prediction, and the confidence',
     &       ' interval may',/,6X,'be obtained with SEOPT equal to 3.',
     &       /)
*
*     output heading for AMLE load table
*
      WRITE(LDOUT,1000)
*
*     estimate AMLE loads for the entire estimation period; output mean
*     load estimate with confidence interval and standard errors
*
      SEOPT2 = SEOPT
      CALL AMLLOAD2(NOBSC,NPAR,YLCAL2,YD,PARAML,XLCAL,NOBSE,XLEST,
     &              CENSFLAG,SEOPT2,LOADAML,VARAML,LOW95,UP95,SEP,
     &              PLOADAML,NOBSCI,NOBSE)
      CALL OUTAML('Est. Period  ',LFACTOR,LOADAML,LOW95,NOBSE,SEP,UP95,
     &            VARAML,LDOUT)
*
*     develop seasonal AMLE load estimates.  Begin by selecting the
*     "observations" of the explanatory variables that fall within the
*     current season (SETXL2), then calculate loads (AMLLOAD2) and
*     output results (OUTAML).
*
      DO 10 I=1,NSEAS
         SEOPT2 = SEOPT
         WRITE(TSTR,2000) I
         CALL SETXL2(EDATE,NOBSE,NOBS,NPAR,SBEG(I),SEND(I),XLEST,SXLEST)
         CALL AMLLOAD2(NOBSC,NPAR,YLCAL2,YD,PARAML,XLCAL,NOBS,SXLEST,
     &                 CENSFLAG,SEOPT2,LOADAML,VARAML,LOW95,UP95,SEP,
     &                 PLOAD,NOBSCI,NOBSE)
         CALL OUTAML(TSTR,LFACTOR,LOADAML,LOW95,NOBS,SEP,UP95,VARAML,
     &               LDOUT)
 10   CONTINUE
*
*     develop monthly AMLE load estimates
*
      IF ((LDOPT .EQ. 2) .OR. (LDOPT .EQ. 3)) THEN
         NOBS = 0
         OLDMONTH = GETINT(EDATE(1),5,6)
         OLDYEAR = GETINT(EDATE(1),1,4)
         DO 20 I=1,NOBSE
            IF ((GETINT(EDATE(I),5,6).EQ.OLDMONTH) .AND.
     &          (GETINT(EDATE(I),1,4).EQ.OLDYEAR)) THEN
               CALL SETXL3(NOBS,NPAR,XLEST,SXLEST,NOBSE,I)
            ELSE
               SEOPT2 = SEOPT
               IMO = GETINT(EDATE(I-1),5,6)
               IYR = GETINT(EDATE(I-1),1,4)
               WRITE(TSTR,3000) MONSTR(IMO),IYR
               CALL AMLLOAD2(NOBSC,NPAR,YLCAL2,YD,PARAML,XLCAL,NOBS,
     &                       SXLEST,CENSFLAG,SEOPT2,LOADAML,VARAML,
     &                       LOW95,UP95,SEP,PLOAD,NOBSCI,NOBSE)
               CALL OUTAML(TSTR,LFACTOR,LOADAML,LOW95,NOBS,SEP,UP95,
     &                     VARAML,LDOUT)
               OLDMONTH = GETINT(EDATE(I),5,6)
               OLDYEAR = GETINT(EDATE(I),1,4)
               NOBS = 0
               CALL SETXL3(NOBS,NPAR,XLEST,SXLEST,NOBSE,I)
            ENDIF
 20      CONTINUE
         SEOPT2 = SEOPT
         IMO = GETINT(EDATE(I-1),5,6)
         IYR = GETINT(EDATE(I-1),1,4)
         WRITE(TSTR,3000) MONSTR(IMO),IYR
         CALL AMLLOAD2(NOBSC,NPAR,YLCAL2,YD,PARAML,XLCAL,NOBS,SXLEST,
     &                 CENSFLAG,SEOPT2,LOADAML,VARAML,LOW95,UP95,SEP,
     &                 PLOAD,NOBSCI,NOBSE)
         CALL OUTAML(TSTR,LFACTOR,LOADAML,LOW95,NOBS,SEP,UP95,VARAML,
     &               LDOUT)
      ENDIF
*
*     include a note if a linear approximation has been used to compute
*     the standard error
*
      IF (SEOPT .NE. 3) WRITE(LDOUT,4000) SEOPT

      RETURN
      END
