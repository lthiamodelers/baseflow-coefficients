************************************************************************
*
*     Subroutine INPUTH                                  Called by: INIT
*
*     read from header file: TITLE, PRTOPT, SEOPT, LDOPT, NSEAS
*
************************************************************************
      SUBROUTINE INPUTH(TITLE,LDOPT,PRTOPT,SEOPT,NSEAS)
*
*     logical devices
*
      INCLUDE 'lda.inc'
*
*     subroutine arguments
*
      INTEGER*4 LDOPT,NSEAS,PRTOPT,SEOPT
      CHARACTER*80 TITLE
*
*     local vars
*
      CHARACTER*40 FILE
      CHARACTER*500 BUFFER
*
*     input format statements
*
 1000 FORMAT(A40)
 1010 FORMAT(A80)
 1020 FORMAT(I5)
*
*     Output Format Statements
*
 2000 FORMAT(///,1X,A80,//,1X,59('-'),//,4X,'Echo Output File Part I: ',
     &       'Reading from the Header File',//,1X,59('-'),//)
 2010 FORMAT(1X,'Estimated Values Print Option (PRTOPT):',I5)
 2020 FORMAT(1X,'Standard Error Option (SEOPT)         :',I5)
 2030 FORMAT(1X,'Load Option (LDOPT)                   :',I5,//)
 2040 FORMAT(/,' Loads for user-defined seasons are requested.',//,
     &       ' User-defined Seasons',/,1X,20('-'),/,
     &       ' Number of User-defined Seasons (NSEAS):',I5,//,T17,
     &       'Begin Date',T39,'End Date',/,' Season',T19,
     &       '(SBEG)',T40,'(SEND)',/,1X,47('-'))
*
*     read the simulation title and write heading to echo.out
*
      CALL GETLINE(LDHEAD,BUFFER)
      READ (BUFFER,1010) TITLE
      WRITE (LDECHO,2000) TITLE
*
*     read the estimated values print option
*
      CALL GETLINE(LDHEAD,BUFFER)
      READ (BUFFER,1020) PRTOPT
      WRITE (LDECHO,2010) PRTOPT
      IF ((PRTOPT .NE. 0).AND.(PRTOPT .NE. 1)) CALL ERROR1(1,PRTOPT)
*
*     read the standard error option
*
      CALL GETLINE(LDHEAD,BUFFER)
      READ (BUFFER,1020) SEOPT
      WRITE (LDECHO,2020) SEOPT
      IF ((SEOPT .LT. 1).OR.(SEOPT .GT. 3)) CALL ERROR1(2,SEOPT)
*
*     read the load option
*
      CALL GETLINE(LDHEAD,BUFFER)
      READ (BUFFER,1020) LDOPT
      WRITE (LDECHO,2030) LDOPT
      IF ((LDOPT .LT. 0).OR.(LDOPT .GT. 3)) CALL ERROR1(13,LDOPT)
*
*     If seasonal loads are requested (LDOPT = 1 or 3), read
*     the number of seasons (NSEAS) and the begin and end date
*     associated with each season (SBEG and SEND).  Output user-
*     defined seasons.
*
      NSEAS = 0
      IF ((LDOPT.EQ.1).OR.(LDOPT.EQ.3)) THEN
         CALL GETLINE(LDHEAD,BUFFER)
         READ(BUFFER,1020) NSEAS
         WRITE(LDECHO,2040) NSEAS
      ENDIF

      RETURN
      END
