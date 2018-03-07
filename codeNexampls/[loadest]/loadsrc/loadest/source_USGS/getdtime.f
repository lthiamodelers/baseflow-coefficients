************************************************************************
*
*     Subroutine GETDTIME                      Called by: INPUTC,INPUTE
*
*     compute decimal time in fractional years, DECTIME, and set the
*     PERIOD for models 10 and 11
*
*     local variables
*     ---------------
*     IDA      day of month (1 - 31)
*     IMI      number of minutes since midnight
*     IMO      month of year (1-12)
*     NDAY     julian date
*     IDAYS    julian date of the last day of the previous month
*
************************************************************************
      SUBROUTINE GETDTIME(DATE,IYR,TIME,DECTIME,PERIOD,PBMON,PEMON,
     &                    MODNO)
*
*     subroutine arguments
*
      INTEGER*4 DATE,IYR,TIME,PBMON,PEMON,MODNO
      DOUBLE PRECISION DECTIME,PERIOD
*
*     local vars
*
      INTEGER*4 IDA,IMI,IMO,LEAP,NDAY,IDAYS(12)
      DATA IDAYS / 0,31,59,90,120,151,181,212,243,273,304,334 /
*
*     determine year, month, day of month, and minutes since midnight
*
      IYR = DATE/10000
      IMO = (DATE-IYR*10000)/100
      IDA = DATE-(IYR*10000)-(IMO*100)
      IMI = ((TIME/100)*60)+MOD(TIME,100)
*
*     error checks: 1) day of month must be 1-31; 2) month must be
*     1-12; 3) year must be > 1582.
*
      IF ((IDA.LT.1).OR.(IDA.GT.31)) CALL ERROR1(8,DATE)
      IF ((IMO.LT.1).OR.(IMO.GT.12)) CALL ERROR1(9,DATE)
      IF (IYR.LE.1582) CALL ERROR1(10,DATE)
*
*     For models 10 or 11, set PERIOD if the observation falls within
*     the range of PBMON AND PEMON
*
      IF (((MODNO.EQ.10).OR.(MODNO.EQ.11)).AND.
     &    (IMO.GE.PBMON.AND.IMO.LE.PEMON)) THEN
         PERIOD = 1.D0
      ELSE
         PERIOD = 0.D0
      ENDIF
*
*     determine if its a leap year
*
      LEAP = 0
      IF (MOD(IYR,4).EQ.0) LEAP = 1
      IF ((MOD(IYR,100).EQ.0).AND.(MOD(IYR,400).NE.0)) LEAP = 0
*
*     compute the julian date (works for dates after 1582)
*
      NDAY = IDAYS(IMO)+IDA
      IF (IMO.GE.3) NDAY = NDAY + LEAP
*
*     calculate DECTIME
*
      IF (LEAP.EQ.1) THEN
         DECTIME = DBLE(IYR)+(DBLE(NDAY)-
     &        1.D0+(DBLE(IMI)/1440.D0))/366.D0
      ELSE
         DECTIME = DBLE(IYR)+(DBLE(NDAY) -
     &        1.D0+(DBLE(IMI)/1440.D0))/365.D0
      ENDIF

      RETURN
      END


