************************************************************************
*
*     Subroutine MLELOAD                            Called by: ESTLOAD
*
*     calculate MLE loads for entire estimation period, user-defined
*     seasons, and individual months
*
*     local vars
*     ----------
*     LOADMLE  mean MLE load (entire period, season, or indiv. month)
*     MONSTR   string denoting month of year
*     NOBS     no. of observations used to estimate the seasonal load
*     OLDMONTH tracking variable used to determine when month ends
*     OLDYEAR  tracking variable used to determine when month ends
*     SXLEST   subset of XLEST for seasonal or monthly loads
*     TSTR     string denoting time period covered by load estimate
*     VARMLE   variance of LOADMLE
*     XTXINV   inverse of X X-Transpose X from the regression equation
*
************************************************************************
      SUBROUTINE MLELOAD(EDATE,LDOPT,LDOUT,LFACTOR,NOBSC,NOBSCI,NOBSE,
     &                   NPAR,NSEAS,PARMLE,PLOADMLE,RVARMLE,SBEG,SEND,
     &                   SEOPT,XLCAL,XLEST)
*     
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 LDOPT,LDOUT,NOBSC,NOBSCI,NOBSE,NPAR,NSEAS,SEOPT
      DOUBLE PRECISION LFACTOR,RVARMLE
      DOUBLE PRECISION PLOADMLE(*),PARMLE(*)
      DOUBLE PRECISION XLCAL(NOBSCI,*),XLEST(NOBSE,*)
      CHARACTER*4 SBEG(*),SEND(*)
      CHARACTER*8 EDATE(*)
*
*     local vars
*
      INTEGER*4 I,IMO,IYR,NOBS,OLDMONTH,OLDYEAR
      DOUBLE PRECISION LOADMLE,VARMLE
      DOUBLE PRECISION SXLEST(NOBSE,MAXPARMS),
     &                 XTXINV(MAXPARMS,MAXPARMS)
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
 1000 FORMAT(//,T15,'MLE Load Estimates',/,T15,18('-'),//,T26,'Mean',
     &       T33,'Standard',/,T18,'N',T26,'Load',T36,'Error',/,T15,
     &       26('-'))
 2000 FORMAT(//,T15,'MLE Load Estimates',/,T15,18('-'),//,T26,'Mean',/,
     &       T18,'N',T26,'Load',/,T15,15('-'))
 3000 FORMAT('Season',I3,'    ')
 4000 FORMAT(A4,1X,I4)
*
*     output heading for MLE load table
*
      IF (SEOPT .NE. 1) THEN
         WRITE(LDOUT,1000)
      ELSE
         WRITE(LDOUT,2000)
      ENDIF
*
*     estimate MLE loads for the entire estimation period; output mean
*     load and standard error
*
      CALL MATCALC(NPAR,NOBSC,XLCAL,XTXINV,NOBSCI)
      CALL MLELOAD2(NPAR,XLEST,NOBSE,NOBSC,PARMLE,RVARMLE,XTXINV,SEOPT,
     &              LOADMLE,VARMLE,PLOADMLE,NOBSE)
      CALL OUTEST('Est. Period  ',LFACTOR,LOADMLE,NOBSE,SEOPT,VARMLE,
     &            LDOUT)
*
*     develop seasonal MLE load estimates.  Begin by selecting the
*     "observations" of the explanatory variables that fall within the
*     current season (SETXL2), then calculate loads (MLELOAD2) and
*     output results (OUTEST).
*
      DO 10 I=1,NSEAS
         WRITE(TSTR,3000) I
         CALL SETXL2(EDATE,NOBSE,NOBS,NPAR,SBEG(I),SEND(I),XLEST,SXLEST)
         CALL MLELOAD2(NPAR,SXLEST,NOBS,NOBSC,PARMLE,RVARMLE,XTXINV,
     &                 SEOPT,LOADMLE,VARMLE,PLOAD,NOBSE)
         CALL OUTEST(TSTR,LFACTOR,LOADMLE,NOBS,SEOPT,VARMLE,LDOUT)
 10   CONTINUE
*
*     develop monthly MLE load estimates
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
               IMO = GETINT(EDATE(I-1),5,6)
               IYR = GETINT(EDATE(I-1),1,4)
               WRITE(TSTR,4000) MONSTR(IMO),IYR
               CALL MLELOAD2(NPAR,SXLEST,NOBS,NOBSC,PARMLE,RVARMLE,
     &                       XTXINV,SEOPT,LOADMLE,VARMLE,PLOAD,NOBSE)
               CALL OUTEST(TSTR,LFACTOR,LOADMLE,NOBS,SEOPT,VARMLE,LDOUT)
               OLDMONTH = GETINT(EDATE(I),5,6)
               OLDYEAR = GETINT(EDATE(I),1,4)
               NOBS = 0
               CALL SETXL3(NOBS,NPAR,XLEST,SXLEST,NOBSE,I)
            ENDIF
 20      CONTINUE
         IMO = GETINT(EDATE(I-1),5,6)
         IYR = GETINT(EDATE(I-1),1,4)
         WRITE(TSTR,4000) MONSTR(IMO),IYR
         CALL MLELOAD2(NPAR,SXLEST,NOBS,NOBSC,PARMLE,RVARMLE,XTXINV,
     &                 SEOPT,LOADMLE,VARMLE,PLOAD,NOBSE)
         CALL OUTEST(TSTR,LFACTOR,LOADMLE,NOBS,SEOPT,VARMLE,LDOUT)
      ENDIF

      RETURN
      END
