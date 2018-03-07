************************************************************************
*
*     Subroutine ERROR2         Called by: INPUTH3, INPUTH3B, SETNPAR
*
*     print input error messages
*
************************************************************************
      SUBROUTINE ERROR2(NUMBER,PARAM1,PARAM2)
*
*     logical devices
*
      INCLUDE 'lda.inc'
*
*     subroutine arguments
*
      INTEGER*4 NUMBER,PARAM1,PARAM2
*
*     output format statements
*
 1000 FORMAT(//,2X,
     &'Error: The Ending Month of the period must be greater',/,9X,
     &'than or equal to the Beginning Month',//,10X,
     &'Beginning Month of Period (PBMON): ',I5,/,10X,
     &'Ending Month of Period (PEMON)   : ',I5,//)
 2000 FORMAT(//,2X,
     &'Error: The number of additional data variables ',
     &'specified exceeds the',/,10X,'maximum number of 9.',//,2X,
     &'Decrease the number of additional variables.',,//,10X,
     &'Number of Additional Data Variables (NADDL):',I5,//)
 6000 FORMAT(//,2X,
     &'Programming Error: The number of parameters exceeds the ',
     &'maximum.',/,2X,'Contact program author.',//,10X,
     &'Number of Parameters : ',I5,/,10X,
     &'Maximum number allowed (MAXPARMS): ',I5,//)
 7000 FORMAT(//,2X,
     &'Error: The number of explanatory variables specified exceeds',
     &/,9X,'the maximum.',//,2X,'Decrease the number of explanatory ',
     &'variables or increase the maximum.',/,2X,
     &'To increase the maximum number, change MAXPARMS and recompile',
     &/,2X,'(note: regression models with more ',
     &'than 6 explanatory variables are rare).',//,10X,
     &'Number of Explanatory Variables (NEXPL): ',I5,/,10X,
     &'Maximum number allowed (MAXPARMS-1)    : ',I5,//)
*
*     print error message and terminate execution
*
      IF (NUMBER .EQ. 1) THEN
         WRITE(*,1000) PARAM1,PARAM2
         WRITE(LDECHO,1000) PARAM1,PARAM2
      ELSEIF (NUMBER .EQ. 2) THEN
         WRITE(*,2000) PARAM1
         WRITE(LDECHO,2000) PARAM1
      ELSEIF (NUMBER .EQ. 6) THEN
         WRITE(*,6000) PARAM1,PARAM2
         WRITE(LDECHO,6000) PARAM1,PARAM2
      ELSEIF (NUMBER .EQ. 7) THEN
         WRITE(*,7000) PARAM1,PARAM2
         WRITE(LDECHO,7000) PARAM1,PARAM2
      ENDIF

      WRITE(*,*) '  **** Fatal Input Error, See file echo.out ****'
      STOP ' '

      END
