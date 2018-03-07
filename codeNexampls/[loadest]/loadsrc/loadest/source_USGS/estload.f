************************************************************************
*
*     Subroutine ESTLOAD                        Called by: Main program
*
*     given the calibrated regression models developed in CALIBR,
*     estimate loads
*
*     local vars
*     ----------
*     PLOADAML individual AMLE load estimate (P is for 'population')
*     PLOADLAD individual LAD load estimate (P is for 'population')
*     PLOADMLE individual MLE load estimate (P is for 'population')
*     XLEST    explanatory variables for estimating mean load 
*
************************************************************************
      SUBROUTINE ESTLOAD(BCLAD,BESTMOD,CCMAX,CENSFLAG,CFACTOR,CFLOW,
     &                   CUNITSTR,DECTIME2,DVNAME,EADDL,EDATE,EFLOW,
     &                   EQSTAT,ETIME,LDOPT,LDOUT,LFACTOR,LQCENT,
     &                   LUNITSTR,NADDL,NCENS,NCONST,NEXPL,NOBSC,NOBSCI,
     &                   NOBSE,NPAR,NSEAS,PARAML,PARLAD,PARMLE,PERIOD2,
     &                   PRTOPT,RVARMLE,SBEG,SEND,SEOPT,TRANS,WFLAG,
     &                   XLCAL,YD,YLCAL,YLCAL2)
*     
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      LOGICAL WFLAG,CENSFLAG(*)
      INTEGER*4 BESTMOD,LDOPT,LDOUT,NADDL,NCENS,NCONST,NEXPL,NOBSC,
     &          NOBSCI,NOBSE,NPAR,NSEAS,PRTOPT,SEOPT
      INTEGER*4 ETIME(*)
      DOUBLE PRECISION BCLAD,CCMAX,CFACTOR,LFACTOR,LQCENT,RVARMLE
      DOUBLE PRECISION CFLOW(*),DECTIME2(*),EFLOW(*),EQSTAT(*),
     &                 PARAML(*),PARLAD(*),PARMLE(*),PERIOD2(*),YD(*),
     &                 YLCAL(*),YLCAL2(*)
      DOUBLE PRECISION EADDL(NADDL,*),XLCAL(NOBSCI,*)
      CHARACTER*(*) CUNITSTR,LUNITSTR
      CHARACTER*4 SBEG(*),SEND(*)
      CHARACTER*5 DVNAME(*),TRANS(*)
      CHARACTER*8 EDATE(*)
*
*     local vars
*
      DOUBLE PRECISION PLOADAML(NOBSE),PLOADLAD(NOBSE),PLOADMLE(NOBSE)
      DOUBLE PRECISION XLEST(NOBSE,MAXPARMS)
*
*     output heading for estimated loads
*
      CALL OUTHEAD(CFLOW,EDATE,EQSTAT,LUNITSTR,NOBSC,NOBSE,WFLAG,LDOUT)
*
*     generate explanatory variables for the best model
*
      CALL SETXL(BESTMOD,NOBSE,LQCENT,EADDL,EFLOW,DECTIME2,PERIOD2,
     &           XLEST,NOBSE,NEXPL,DVNAME,TRANS,NADDL)
*           
*     estimate loads using AMLE
*
      CALL AMLLOAD(CENSFLAG,EDATE,LDOPT,LDOUT,LFACTOR,NOBSC,NOBSCI,
     &             NOBSE,NPAR,NSEAS,PARAML,PLOADAML,SBEG,SEND,SEOPT,
     &             XLCAL,XLEST,YD,YLCAL2)
*           
*     estimate loads using MLE
*
      CALL MLELOAD(EDATE,LDOPT,LDOUT,LFACTOR,NOBSC,NOBSCI,NOBSE,NPAR,
     &             NSEAS,PARMLE,PLOADMLE,RVARMLE,SBEG,SEND,SEOPT,XLCAL,
     &             XLEST)
*
*     estimate loads using LAD
*
      IF (NCENS .EQ. 0)
     &   CALL LADLOAD(BCLAD,EDATE,LDOPT,LFACTOR,NOBSC,NOBSE,NPAR,NSEAS,
     &                PARLAD,SBEG,SEOPT,SEND,XLCAL,XLEST,YLCAL,PLOADLAD,
     &                NOBSCI,LDOUT)
*
*     output summary statistics for estimated loads and concentrations
*
      CALL OUTSUMM(CCMAX,CFACTOR,CUNITSTR,EFLOW,LFACTOR,LUNITSTR,NCENS,
     &             NOBSE,PLOADAML,PLOADLAD,PLOADMLE,LDOUT)
*
*     if requested, output individual load estimates
*
      IF (PRTOPT.EQ.1)
     &   CALL OUTIND(EDATE,EFLOW,ETIME,NCENS,NOBSE,PLOADAML,PLOADLAD,
     &               PLOADMLE,NCONST,LDOUT)

      RETURN
      END
