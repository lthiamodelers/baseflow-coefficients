************************************************************************
*
*     Subroutine INIT3                          Called by: Main program
*
*     finish reading the header file, then read the calibration and
*     estimation files.
*
*     local vars (automatic arrays)
*     -----------------------------
*     BYEAR    beginning year of calibration data
*     DTCENT   "center" of decimal time
*     IYR      year in yyyy format
*     LNQ      log of calibration flow
*
************************************************************************
      SUBROUTINE INIT3(CENSFLAG,IMODBEG,IMODEND,MODNO,NADDL,NCONST,
     &                 NOBSCI,NOBSE,PBMON,PEMON,PRTOPT,ETIME,LDOUT,
     &                 NCENS,NOBSC,CCMAX,CFACTOR,EQSTAT,LFACTOR,LQCENT,
     &                 EFLOW,PERIOD2,CFLOW,DECTIME,DECTIME2,EADDL,
     &                 PERIOD,YD,YDC,YLCAL,YLCALC,CADDL,CUNITSTR,EDATE,
     &                 LUNITSTR,CDATETIM,TITLE)
*
*     logical devices
*
      INCLUDE 'lda.inc'
*
*     subroutine arguments
*
      LOGICAL CENSFLAG(NOBSCI,*)
      INTEGER*4 IMODBEG,IMODEND,MODNO,NADDL,NCONST,NOBSCI,NOBSE,PBMON,
     &          PEMON,PRTOPT
      INTEGER*4 ETIME(*),LDOUT(*),NCENS(*),NOBSC(*)
      DOUBLE PRECISION CCMAX(*),CFACTOR(*),EQSTAT(*),LFACTOR(*),
     &                 LQCENT(*),EFLOW(*),PERIOD2(*)
      DOUBLE PRECISION CFLOW(NOBSCI,*),DECTIME(NOBSCI,*),
     &                 DECTIME2(NOBSE,*),EADDL(NADDL,*),
     &                 PERIOD(NOBSCI,*),YD(NOBSCI,*),YDC(NOBSCI,*),
     &                 YLCAL(NOBSCI,*),YLCALC(NOBSCI,*)
      DOUBLE PRECISION CADDL(NADDL,NOBSCI,*)
      CHARACTER*4 CUNITSTR(*)
      CHARACTER*8 EDATE(*)
      CHARACTER*9 LUNITSTR(*)
      CHARACTER*13 CDATETIM(NOBSCI,*)
      CHARACTER*80 TITLE
*
*     local vars
*
      INTEGER*4 I,J,BYEAR(NCONST),IYR(NCONST)
      DOUBLE PRECISION DTCENT(NCONST),LNQ(NOBSCI,NCONST)
      CHARACTER*45 CNAME(NCONST)
*
*     format statement
*
 2000 FORMAT(///,1X,70('-'),//,10X,'Echo Output File Part V: ',
     &       'Other Warnings and Error Messages',//,1X,70('-'),//)
*
*     initialize the logical device for the constituent output files
*
      J = 21 
      DO 10 I = 1,NCONST*3
         LDOUT(I) = J
         J = J + 1
 10   CONTINUE
*
*     finish reading the header file
*
      CALL INPUTH4(CFACTOR,CNAME,CUNITSTR,LFACTOR,LUNITSTR,NCONST)
*
*     read the calibration data
*
      CALL INPUTC(CENSFLAG,MODNO,NADDL,NCONST,NOBSCI,PBMON,PEMON,BYEAR,
     &            IYR,NCENS,NOBSC,CCMAX,CFACTOR,CFLOW,DECTIME,LNQ,
     &            PERIOD,YD,YDC,YLCAL,YLCALC,CADDL,CDATETIM,CNAME)
      CALL SETUPC(IMODBEG,IMODEND,MODNO,NCONST,NOBSCI,PRTOPT,BYEAR,IYR,
     &            LDOUT,NCENS,NOBSC,DTCENT,LQCENT,DECTIME,LNQ,CNAME,
     &            TITLE)
*
*     read the estimation data
*
      CALL INPUTE(DECTIME2,DTCENT,EADDL,EDATE,EFLOW,EQSTAT,ETIME,MODNO,
     &            NADDL,NCONST,NOBSE,PBMON,PEMON,PERIOD2)
*
*     close input files 
*
      CLOSE(LDHEAD)
      CLOSE(LDCAL)
      CLOSE(LDEST)
      CLOSE(LDCTRL)
*
*     write heading for last part of echo.out
*
      WRITE(LDECHO,2000)

      RETURN
      END
