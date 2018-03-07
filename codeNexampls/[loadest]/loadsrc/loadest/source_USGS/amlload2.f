************************************************************************
*
*     Subroutine AMLLOAD2                      Called by: AMLLOAD
*
*     estimate AMLE loads
*
*     local vars
*     ----------
*     BIAS     bias of MLE estimates with respect to S**2
*     B_AML
*     B_MLE
*     CV       covar. of std normals censored at XSI's w.r.t. S**2
*     CV_M
*     SBIAS    bias of MLE estimates w.r.t. S
*     SCV      covar. of standard normals censored at XSI's w.r.t. S
*     STDDEV
*     XLIKE
*     XLOADSUM sum of estimated loads
*     XLOADVAR variance of sum of estimated loads
*
************************************************************************
      SUBROUTINE AMLLOAD2(NOBSC,NPAR,YLCAL2,YD,PARAML,XLCAL,NUMOBSE,
     &                    XLESTM,CENSFLAG,SEOPT2,LOADAML,VARAML,LOW95,
     &                    UP95,SEP,PLDAML,NOBSCI,NOBSE)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine args
*
      LOGICAL CENSFLAG(*)
      INTEGER*4 NOBSC,NPAR,NUMOBSE,NOBSCI,NOBSE,SEOPT2
      DOUBLE PRECISION LOADAML,VARAML,LOW95,UP95,SEP
      DOUBLE PRECISION YLCAL2(*),YD(*),PARAML(*),PLDAML(*)
      DOUBLE PRECISION XLCAL(NOBSCI,*),XLESTM(NOBSE,*)
*
*     local vars
*
      DOUBLE PRECISION XLOADSUM,XLOADVAR,XLIKE
      DOUBLE PRECISION B_MLE(MAXPARMS+1),B_AML(MAXPARMS+1),
     &                 STDDEV(MAXPARMS+1),BIAS(MAXPARMS+1),
     &                 SBIAS(MAXPARMS+1)
      DOUBLE PRECISION CV(MAXPARMS+1,MAXPARMS+1),
     &                 SCV(MAXPARMS+1,MAXPARMS+1),
     &                 CV_M(MAXPARMS+1,MAXPARMS+1)
*
*     save for when SEOPT2 = 0
*
      SAVE B_MLE,BIAS,SBIAS,CV,SCV,CV_M
*
*     execute the first part of the 'IF' if the user has requested the
*     exact variance (SEOPT=SEOPT2=3).  Execute the second part of
*     the 'IF' if the user has requested the approximate variance
*     (SEOPT=SEOPT2=1 or 2) and this is the first time through this
*     routine.   Execute the third part of the 'IF' if the user has
*     requested the approximate variance (SEOPT=1 or 2) and this is
*     a repeat trip through this routine (SEOPT2=0).
*
      IF (SEOPT2.EQ.3) THEN
         CALL AMLREG(NOBSC,NPAR,XLCAL,YLCAL2,YD,CENSFLAG,XLIKE,STDDEV,
     &               B_MLE,B_AML,BIAS,CV,SBIAS,SCV,CV_M,NOBSCI)
         CALL TAC_LOAD(NPAR,B_MLE,BIAS,CV,SBIAS,SCV,NUMOBSE,XLESTM,
     &                 PLDAML,XLOADSUM,XLOADVAR,NOBSE)
      ELSEIF ((SEOPT2.EQ.1) .OR. (SEOPT2.EQ.2)) THEN
         CALL AMLREG(NOBSC,NPAR,XLCAL,YLCAL2,YD,CENSFLAG,XLIKE,STDDEV,
     &              B_MLE,B_AML,BIAS,CV,SBIAS,SCV,CV_M,NOBSCI)
         CALL TACIT_LOADS_NC(NPAR,NUMOBSE,XLESTM,PLDAML,XLOADSUM,
     &                       XLOADVAR,B_MLE,BIAS,SBIAS,CV,SCV,CV_M,
     &                       NOBSE)
         SEOPT2 = 0
      ELSE
         CALL TACIT_LOADS_NC(NPAR,NUMOBSE,XLESTM,PLDAML,XLOADSUM,
     &                       XLOADVAR,B_MLE,BIAS,SBIAS,CV,SCV,CV_M,
     &                       NOBSE)
      ENDIF
*
*     compute load and standard error of the load
*
      LOADAML = XLOADSUM/NUMOBSE
      VARAML = XLOADVAR/(NUMOBSE*NUMOBSE)
*
*     compute standard error of prediction assuming errors
*     uncorrelated lag 1 (approx correct for mid-size rivers -- see
*     Tim Cohn 1989 notes)
*     
      CALL LOADSEP(NUMOBSE,PLDAML,PARAML(NPAR+1),XLOADVAR,SEP)
*
*     compute approx 95% confidence interval around true load
*     assuming a 2-P Lognormal distribution
*
      CALL LOAD95CI(LOADAML,SEP,LOW95,UP95)

      RETURN
      END
