************************************************************************
*
*     Subroutine INPUTE                               Called by: INIT3
*
*     read observations of the data variable(s) from the estimation file
*     to use in the regression equations
*
*     local vars
*     ----------
*     EDATEI   integer representation of EDATE
*     IYR      year in yyyy format
*     NPD      number of observations input for a given day
*     OLDDATE  equal to EDATE(I-1)
*
************************************************************************
      SUBROUTINE INPUTE(DECTIME2,DTCENT,EADDL,EDATE,EFLOW,EQSTAT,ETIME,
     &                  MODNO,NADDL,NCONST,NOBSE,PBMON,PEMON,PERIOD2)
*
*     logical devices
*
      INCLUDE 'lda.inc'
*
*     subroutine arguments
*
      INTEGER*4 MODNO,NADDL,NCONST,NOBSE,PBMON,PEMON,ETIME(*)
      DOUBLE PRECISION EQSTAT(*),DTCENT(*),EFLOW(*),PERIOD2(*)
      DOUBLE PRECISION EADDL(NADDL,*),DECTIME2(NOBSE,*)
      CHARACTER*8 EDATE(*)
*
*     local vars
*
      INTEGER*4 I,J,K,EDATEI,IYR,NOBSPD,NPD
      DOUBLE PRECISION DTIMETMP
      CHARACTER*8 OLDDATE
      CHARACTER*40 FILE
      CHARACTER*500 BUFFER
*
*     function declaration
*
      DOUBLE PRECISION MEAN
*
*     input format statements
*
 1000 FORMAT(A40)
 1100 FORMAT(I5)
*
*     Output Format Statements
*
 2000 FORMAT(///,1X,59('-'),//,24X,'Echo Output File Part IV:',/,14X,
     &       'Reading from the Estimation File',//,1X,
     &       59('-'),//)
 2100 FORMAT(1X,'No. of Observations of the Data Variables per Day',
     &       ' (NOBSPD):',I5,//,1X,
     &       '(The remaining lines in the Estimation File are ',
     &       'not echoed unless',/,' an error occurs)',//)
 2200 FORMAT(1X,'Error on line number',I5,' of the Estimation ',
     &       'File (excluding # lines)',//,1X,'Date',7X,'Time',
     &       3X,'Flow',10X,'Additional I.V.s (NADDL>0)',/,1X,60('-'),/,
     &       1X,A8,3X,I4,7(3X,1PE11.4))
*
*     write echo.out Part IV
*
      WRITE(LDECHO,2000)
*
*     read the number of observations per day
*
      CALL GETLINE(LDEST,BUFFER)
      READ (BUFFER,1100) NOBSPD
      WRITE (LDECHO,2100) NOBSPD
      IF (NOBSPD .GT. 24) CALL ERROR1(11,NOBSPD)
*
*     read through the estimation file and perform the following error
*     tests:
*
*     - flow values must be greater than zero
*     - times on given date must be in ascending order
*     - there must be NOBSPD values per day in the estimation data set
*
      NPD = 0

      DO 20 I=1,NOBSE
         CALL GETLINE(LDEST,BUFFER)         
         READ(BUFFER,*,END=30) EDATE(I),ETIME(I),EFLOW(I),
     &                         (EADDL(K,I),K=1,NADDL)
         IF (EFLOW(I) .LE. 0.D0) THEN
            WRITE(LDECHO,2200) I+1,EDATE(I),ETIME(I),EFLOW(I),
     &                         (EADDL(K,I),K=1,NADDL)
            CALL ERROR3(1,EFLOW(I))
         ENDIF

         IF (I.EQ.1) OLDDATE = EDATE(1)

         IF (EDATE(I) .EQ. OLDDATE) THEN
            NPD = NPD + 1
            IF (I.NE.1 .AND. ETIME(I).LE.ETIME(I-1)) THEN
               WRITE(LDECHO,2200) I+1,EDATE(I),ETIME(I),EFLOW(I),
     &                            (EADDL(K,I),K=1,NADDL)
               CALL ERROR4(1,EDATE(I),ETIME(I),ETIME(I-1))
            ENDIF
         ELSE
            IF  (NPD .NE. NOBSPD) THEN
               WRITE(LDECHO,2200) I,EDATE(I-1),ETIME(I-1),EFLOW(I-1),
     &                            (EADDL(K,I-1),K=1,NADDL)
               CALL ERROR6(1,EDATE(I-1),NPD,NOBSPD)
            ENDIF
            OLDDATE = EDATE(I)
            NPD = 1
         ENDIF
*
*        calculate decimal time (DECTIME2, in fractional years) and 
*        set PERIOD for the estimation data set.
*
*        DECTIME2 is adjusted by subtracting out the center of decimal
*        time (DTCENT).  This adjustment minimizes the colinearity
*        between decimal time and decimal time squared.
*
         READ(EDATE(I),*) EDATEI
         CALL GETDTIME(EDATEI,IYR,ETIME(I),DTIMETMP,PERIOD2(I),PBMON,
     &                 PEMON,MODNO)
         DO 10 J=1,NCONST
            DECTIME2(I,J) = DTIMETMP - DTCENT(J)
 10      CONTINUE

 20   CONTINUE
*
*     final check for last day of estimation file
*
      IF (NPD .NE. NOBSPD) THEN
         WRITE(LDECHO,2200) I,EDATE(I-1),ETIME(I-1),EFLOW(I-1),
     &                      (EADDL(K,I-1),K=1,NADDL)
         CALL ERROR6(1,EDATE(I-1),NPD,NOBSPD)
      ENDIF
*
*     calculate flow summary stats for the estimation data set (these
*     stats are later output in OUTHEAD)
*
      EQSTAT(1) = MEAN(EFLOW,NOBSE)
      CALL PCTL(EFLOW,NOBSE,EQSTAT(2),EQSTAT(3),EQSTAT(4),EQSTAT(5),
     &          EQSTAT(6),EQSTAT(7),EQSTAT(8),EQSTAT(9),EQSTAT(10))

      RETURN
*
*     end of estimation file (or input line) reached; print error
*     message and terminate execution
*
 30   CALL ERROR1(15,I+1)

      END
