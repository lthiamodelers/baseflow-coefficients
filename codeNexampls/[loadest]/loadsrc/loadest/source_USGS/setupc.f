************************************************************************
*
*     Subroutine SETUPC                         Called by: INIT3
*
*     Using the calibration data set, compute the maximum observed
*     sample concentrations, the "center" of log streamflow, and the
*     center of decimal time.  Set the loop indices for automated model
*     selection.
*
************************************************************************
      SUBROUTINE SETUPC(IMODBEG,IMODEND,MODNO,NCONST,NOBSCI,PRTOPT,
     &                  BYEAR,IYR,LDOUT,NCENS,NOBSC,DTCENT,LQCENT,
     &                  DECTIME,LNQ,CNAME,TITLE)
*
*     dimensional parameters and logical devices
*
      INCLUDE 'fmodules.inc'
      INCLUDE 'lda.inc'
*
*     subroutine arguments
*
      INTEGER*4 IMODBEG,IMODEND,MODNO,NCONST,NOBSCI,PRTOPT
      INTEGER*4 BYEAR(*),IYR(*),LDOUT(*),NCENS(*),NOBSC(*)
      DOUBLE PRECISION DTCENT(*),LQCENT(*)
      DOUBLE PRECISION DECTIME(NOBSCI,*),LNQ(NOBSCI,*)
      CHARACTER*45 CNAME(*)
      CHARACTER*80 TITLE
*
*     local vars
*
      INTEGER*4 I,J
      CHARACTER*49 OUTFILE,OUTFILE2,OUTFILE3
*
*     format statements
*
 3000 FORMAT(///,1X,70('-'),//,10X,'Echo Output File Part III: ',
     &       'Constituent Output Files',//,1X,70('-'),///,
     &       ' Constituent',25X,'File Name',/,1X,60('-'))
 3100 FORMAT(1X,A35,1X,A48)
 3200 FORMAT(1X,A80,//,' Constituent: ',A45)
 4000 FORMAT(/,1X,70('-'),//,5X,'Constituent Output File Part Ia: ',
     &       'Calibration (Load Regression)',//,1X,70('-'),/)
 4100 FORMAT(' Number of Observations           : ',I5)
 4200 FORMAT(' Number of Uncensored Observations: ',I5)
 4300 FORMAT(' "center" of Decimal Time         :   ',F8.3)
 4400 FORMAT(' "center" of Ln(Q)                :  ',F8.4)
 4500 FORMAT(' Period of record                 :    ',I4,'-',I4)
 4600 FORMAT(//' There is insufficient concentration data for ',
     &       'calibration (12 or more',/,' nonzero observations ',
     &       'are required, 7 or more of which must be uncensored.)',//,
     &       ' Number of Nonzero Observations   : ',I5,/,
     &       ' Number of Uncensored Observations: ',I5,//)
*
*     Set the loop indices for automated model selection.  If MODNO
*     equals 0, automated model selection is conducted by looping 
*     through the first 9 models in CALIBR.  If a specific model
*     is specified by the user (MODNO not equal to 0), set the indices
*     so that the loop in CALIBR is executed only once
*     (IMODBEG=IMODEND).  Because the user defined model is MODNO=99,
*     IMODBEG & END are set to NMODELS (its the last model) rather
*     than the MODNO - this insures that NPAR and other vars dimensioned
*     to NMODELS are accessed properly.
*
      IF (MODNO .EQ. 0) THEN
         IMODBEG = 1
         IMODEND = 9
      ELSEIF (MODNO .EQ. 99) THEN
         IMODBEG = NMODELS
         IMODEND = NMODELS
      ELSE
         IMODBEG = MODNO
         IMODEND = MODNO
      ENDIF
*
*
*     loop through constituents
*
      WRITE(LDECHO,3000)

      DO 20 J=1,NCONST
*
*        create a filenames for main output, residuals, and individual
*        loads based on the user-specified constituent name (CNAME),
*        open constituent output file, and output info
*
         CALL MKFNAME(OUTFILE,OUTFILE2,OUTFILE3,CNAME(J))
         OPEN (UNIT=LDOUT(J),FILE=OUTFILE)
         OPEN (UNIT=LDOUT(J+NCONST),FILE=OUTFILE2)
         IF (PRTOPT.EQ.1) OPEN (UNIT=LDOUT(J+2*NCONST),FILE=OUTFILE3)
         WRITE(LDECHO,3100) CNAME(J),OUTFILE
         CALL HEADING(LDOUT(J))
         WRITE(LDOUT(J),3200) TITLE,CNAME(J)
*
*        determine the "center" for log streamflow (LQCENT) and
*        decimal time (DTCENT).   LQCENT and DTCENT are subracted from
*        log(streamflow) and decimal time to minimize collinearity
*        between linear and quadratic terms (e.g. log streamflow and
*        log streamflow squared)(Helsel and Hirsch 1992, p.306).  Note
*        that LQCENT and DTCENT are constituent specific since
*        constituents can have different number of observations due to
*        missing values.
*
         CALL CENTER(LQCENT(J),LNQ(1,J),NOBSC(J))
         CALL CENTER(DTCENT(J),DECTIME(1,J),NOBSC(J))
*
*        output calibration information; omit center of decimal time
*        for models that do not use DECTIME (MODNO=1,2,10 or 11)
*
         WRITE(LDOUT(J),4000)
         WRITE(LDOUT(J),4100) NOBSC(J)
         WRITE(LDOUT(J),4200) NOBSC(J)-NCENS(J)
         IF (MODNO .EQ. 0 .OR. MODNO .EQ. 99 .OR.
     &      (MODNO .GT. 2 .AND. MODNO .LT. 10))
     &      WRITE(LDOUT(J),4300) DTCENT(J)
*
*        Test to make sure there is enough data for calibration (12
*        or more nonzero observations, 7 or more of which are
*        uncensored).
*
*        If data is insufficient, print message and set NOBSC to 0
*        such that calibration and load estimation routines are not
*        called.
*
*        If data is sufficient, output 'center' of log streamflow
*        (LQCENT) and period of record.
*
         IF (NOBSC(J).LT.12 .OR. NOBSC(J)-NCENS(J) .LT. 7 ) THEN
            WRITE (LDOUT(J),4600) NOBSC(J),NOBSC(J)-NCENS(J)
            NOBSC(J) = 0
         ELSE
            WRITE(LDOUT(J),4400) LQCENT(J)
            WRITE(LDOUT(J),4500) BYEAR(J),IYR(J)
         ENDIF
*
*        adjust the decimal time by subtracting out the center of
*        decimal time (DTCENT).
*
         DO 10 I=1,NOBSC(J)
            DECTIME(I,J) = DECTIME(I,J) - DTCENT(J)
 10      CONTINUE

 20   CONTINUE

      RETURN
      END
