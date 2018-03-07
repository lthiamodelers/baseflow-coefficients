************************************************************************
*
*     Subroutine INPUTH3                               Called by: INIT2
*
*     input MODNO, NADDL, NEXPL, PEMON, PBMON, DVNAME, and TRANS
*
************************************************************************
      SUBROUTINE INPUTH3(MODNO,NADDL,NCONST,NEXPL,PEMON,PBMON,DVNAME,
     &                   TRANS)
*
*     logical devices
*
      INCLUDE 'lda.inc'
*
*     subroutine arguments
*
      INTEGER*4 MODNO,NADDL,NCONST,NEXPL,PEMON,PBMON
      CHARACTER*5 DVNAME(*),TRANS(*)
*
*     local vars
*
      CHARACTER*500 BUFFER
*
*     input format statements
*
 1000 FORMAT(2I5)
*
*     output format statements
*
 2000 FORMAT(/,' Model Number 0 was selected. Regression model is',
     &       ' selected based on',/,' Akaike Information Criteria.',//)
 2100 FORMAT(/,' Model Number 99 was selected. User-defined ',
     &       'regression model is used.',//,' User-defined Model',/,1X,
     &       18('-'))
 2200 FORMAT(/,' Model Number ',I2,' was selected. Regression ',
     &       'Model Number ',I2,' is used.',//)
 2300 FORMAT(//,' Beginning Month of Period (PBMON): ',I2,/,
     &       ' Ending Month of Period (PEMON)   : ',I2)
 2400 FORMAT(///,1X,'Number of Constituents (NCONST):',I5)
*
*     read model number 
*
      CALL GETLINE(LDHEAD,BUFFER)
      READ (BUFFER,1000) MODNO
      IF ((MODNO .LT. 0) .OR.
     &   ((MODNO .GT. 11).AND.(MODNO .NE. 99))) CALL ERROR1(3,MODNO)
*
*     output statement indicating whether regression model is (a) 
*     automatically selected from the set of predefined models
*     (MODNO=0), (b) user defined (MODNO=99), or (c) one of the
*     predefined models (MODNO=1-11).
*
      IF (MODNO.EQ.0) THEN
         WRITE (LDECHO,2000) 
      ELSEIF (MODNO.EQ.99) THEN
         WRITE (LDECHO,2100) 
      ELSE
         WRITE (LDECHO,2200) MODNO,MODNO
      ENDIF
*
*     for models 10 and 11, read beginning and ending months of period;
*     call error routines for invalid months
*
      IF ((MODNO .EQ. 10) .OR. (MODNO .EQ. 11)) THEN
         CALL GETLINE(LDHEAD,BUFFER)
         READ(BUFFER,1000) PBMON,PEMON
         WRITE(LDECHO,2300) PBMON,PEMON
         IF ((PBMON.LT.1).OR.(PBMON.GT.12)) CALL ERROR1(4,PBMON)
         IF ((PEMON.LT.1).OR.(PEMON.GT.12)) CALL ERROR1(5,PEMON)
         IF (PEMON.LT.PBMON) CALL ERROR2(1,PBMON,PEMON)
      ENDIF
*
*     if appropriate (MODNO=99), call input routine for user-defined
*     regression model.  If the model is not user defined (MODNO not 
*     equal to 99), there are no additional data variables (NADDL=0)
*     as the predefined models use only flow and time.
*
      IF (MODNO .EQ. 99) THEN
         CALL INPUTH3B(NADDL,NEXPL,DVNAME,TRANS)
      ELSE
         NADDL = 0
      ENDIF
*
*     read the number of constituents
*
      CALL GETLINE(LDHEAD,BUFFER)
      READ (BUFFER,1000) NCONST
      WRITE (LDECHO,2400) NCONST

      RETURN
      END

