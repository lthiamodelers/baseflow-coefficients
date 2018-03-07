************************************************************************
*
*     Subroutine INPUTC                             Called by: INIT3
*
*     input the calibration data set and compute the maximum observed
*     sample concentrations
*
*
*     local vars (input vars defined in main.f)
*     -----------------------------------------
*     CADDLI   additional data variables on input
*     CFLAGI   censored flag (CENSFLAG) on input
*     CFLOWI   stream flow on input
*     DLIM     detection limit
*
************************************************************************
      SUBROUTINE INPUTC(CENSFLAG,MODNO,NADDL,NCONST,NOBSCI,PBMON,PEMON,
     &                  BYEAR,IYR,NCENS,NOBSC,CCMAX,CFACTOR,CFLOW,
     &                  DECTIME,LNQ,PERIOD,YD,YDC,YLCAL,YLCALC,CADDL,
     &                  CDATETIM,CNAME)
*
*     logical devices
*
      INCLUDE 'lda.inc'
*
*     subroutine arguments
*
      LOGICAL CENSFLAG(NOBSCI,*)
      INTEGER*4 MODNO,NADDL,NCONST,NOBSCI,PBMON,PEMON,BYEAR(*),IYR(*)
      DOUBLE PRECISION CFACTOR(*)
      DOUBLE PRECISION CFLOW(NOBSCI,*),DECTIME(NOBSCI,*),LNQ(NOBSCI,*),
     &                 PERIOD(NOBSCI,*),YD(NOBSCI,*),YDC(NOBSCI,*),
     &                 YLCAL(NOBSCI,*),YLCALC(NOBSCI,*)
      DOUBLE PRECISION CADDL(NADDL,NOBSCI,*)
      CHARACTER*13 CDATETIM(NOBSCI,*)
      CHARACTER*45 CNAME(*)
*
*     subroutine args that must be explicitely declared due to init
*
      INTEGER*4 NCENS(NCONST),NOBSC(NCONST)
      DOUBLE PRECISION CCMAX(NCONST)
*
*     local vars
*
      LOGICAL CFLAGI(NOBSCI,NCONST)
      INTEGER*4 I,J,K,CDATE(NOBSCI),CTIME(NOBSCI)
      DOUBLE PRECISION CFLOWI(NOBSCI)
      DOUBLE PRECISION CADDLI(NOBSCI,NADDL),CCONC(NOBSCI,NCONST),
     &                 DLIM(NOBSCI,NCONST)
      CHARACTER*1 NOTE(NOBSCI,NCONST)




*
*     output format statements
*
 2000 FORMAT(///,1X,59('-'),//,24X,'Echo Output File Part II: ',/,14X,
     &       'Reading from the Calibration File',//,1X,59('-'))
 2100 FORMAT(//,' Data Variables:',//,' Date',6X,'Time',4X,
     &       'Streamflow',5X,6(A4,I1,10X))
 2200 FORMAT(1X,27('--'),6(A10),5('-'))
 2300 FORMAT(1X,I8,2X,I4,3X,7(1PE11.4,4X))
 2400 FORMAT(////,' Constituent: ',A45,//,T35,'Detection',/,' Date',6X,
     &       'Time',4X,'Concentr.',6X,'Limit',/,1X,49('-'))
 2500 FORMAT(1X,I8,2X,I4,3X,1PE11.4,A1,3X,1PE11.4)
 2600 FORMAT(1X,I8,2X,I4,3X,1PE11.4,A1)
 2700 FORMAT(//,' * = Censored Observation',/,
     &       ' ^ = Missing Observation',/,
     &       ' # = uncensored observation is less than the',
     &       ' detection limit that',/,5X,'was initially ',
     &       'assigned (assigned detection limit makes it look',/,5X,
     &       'as if the observation is censored, when in fact it is ',
     &       'not).',/,5X,'Assigned detection limit reset to 1.E-25.',
     &       //)
*
*     initialize arrays
*
      NOBSC = 0
      NCENS = 0 
      CCMAX = 0.D0
*
*     read calibration data and deterimine the detection limit, DLIM,
*     associated with each observation
*
      CALL DLIMIT(CADDLI,CCONC,CDATE,CFLAGI,CFLOWI,CTIME,DLIM,NADDL,
     &            NCONST,NOBSCI,NOTE)
*
*     echo observed values of the data variables; make sure all of the
*     observed streamflow values are greater than 0.0
*
      WRITE(LDECHO,2000)
      WRITE(LDECHO,2100) ('Addl',I,I=1,NADDL)
      WRITE(LDECHO,2200) ('----------',I=1,NADDL)
      DO 10 I=1,NOBSCI
         WRITE(LDECHO,2300) CDATE(I),CTIME(I),CFLOWI(I),
     &                      (CADDLI(I,J),J=1,NADDL)
         IF (CFLOWI(I) .LE. 0.D0) CALL ERROR3(2,CFLOWI(I))
 10   CONTINUE
*
*     read and echo calibration data.  If the constituent
*     concentration is greater than zero:
*
*     1) increment the counter for the number of nonmissing
*        observations, NOBSC.
*
*     2) set the PERIOD for models 10 and 11 (S/R GETDTIME)
*
*     3) calculate decimal time, DECTIME, in fractional years
*
*     4) set the streamflow (CFLOW), log streamflow (LNQ), string w/
*        the calibration date/time (CDATETIM), and additional data
*        variables (CADDL)
*
*     5) check to see if the data is censored.  If so, set the censored
*        flag (CENSFLAG, a logical variable used in IFs), increment
*        counter for number of censored observations (NCENS), and set
*        the value of dependent variable (log load, YLCAL in kg/day)
*        based on the detection limit.  If data is not censored, set
*        the censored flag accordingly and set YLCAL based on the
*        user-specified concentration.
*
*     6) Set the censoring threshold, YD
*
*     7) Calculate the maximum calibration concentration (CCMAX)
*
*     Use of CENSFLAG and YD as described in 5 & 6 above was necessary
*     due to the different ways in which LOADEST2 (C.Crawford's code)
*     and ESTIMATOR (Tim Cohn's code) handled censored data.
*
*     some of the steps listed above also apply to YDC and YLCALC, the
*     concentration counterparts of YD and YLCAL.
*
      DO 40 I=1,NOBSCI
         DO 30 J=1,NCONST
            IF (CCONC(I,J).GT.(0.D0)) THEN
               NOBSC(J) = NOBSC(J) + 1
               CALL GETDTIME(CDATE(I),IYR(J),CTIME(I),
     &                       DECTIME(NOBSC(J),J),PERIOD(NOBSC(J),J),
     &                       PBMON,PEMON,MODNO)
               IF (NOBSC(J) .EQ. 1) THEN
                  BYEAR(J) = IYR(J)
               ELSEIF (IYR(J) .LT. BYEAR(J)) THEN
                  BYEAR(J) = IYR(J)
               ENDIF
               CFLOW(NOBSC(J),J) = CFLOWI(I)
               LNQ(NOBSC(J),J) = DLOG(CFLOWI(I))
               WRITE(CDATETIM(NOBSC(J),J),'(I8,1X,I4)')
     &              CDATE(I),CTIME(I)
               DO 20 K=1,NADDL
                  CADDL(K,NOBSC(J),J) = CADDLI(I,K)
 20            CONTINUE
               CENSFLAG(NOBSC(J),J) = CFLAGI(I,J)
               IF (.NOT. CENSFLAG(NOBSC(J),J)) THEN
                  YLCAL(NOBSC(J),J) = DLOG(CFLOWI(I)*CCONC(I,J)
     &                                              *CFACTOR(J))
                  YLCALC(NOBSC(J),J) = DLOG(CCONC(I,J))
               ELSE
                  NCENS(J) = NCENS(J) + 1
                  YLCAL(NOBSC(J),J) = DLOG(CFLOWI(I)*DLIM(I,J)
     &                                             *CFACTOR(J))
                  YLCALC(NOBSC(J),J) = DLOG(DLIM(I,J))
               ENDIF
               YD(NOBSC(J),J) = DLOG(CFLOWI(I)*DLIM(I,J)*CFACTOR(J))
               YDC(NOBSC(J),J) = DLOG(DLIM(I,J))
               IF (CCONC(I,J) .GT. CCMAX(J)) CCMAX(J)= CCONC(I,J)
            ENDIF
 30      CONTINUE
 40   CONTINUE
*
*     echo observed concentrations and associated detection limits
*     (omit detection limit for missing observations, as it is not
*     applicable).      
*
      DO 60 J=1,NCONST
         WRITE(LDECHO,2400) CNAME(J)
         DO 50 I=1,NOBSCI
            IF (CCONC(I,J) .GT. 0.D0) THEN
               WRITE(LDECHO,2500) CDATE(I),CTIME(I),CCONC(I,J),
     &                            NOTE(I,J),DLIM(I,J)
            ELSE
               WRITE(LDECHO,2600) CDATE(I),CTIME(I),CCONC(I,J),NOTE(I,J)
            ENDIF
 50      CONTINUE
 60   CONTINUE
      WRITE(LDECHO,2700)

      RETURN
      END
