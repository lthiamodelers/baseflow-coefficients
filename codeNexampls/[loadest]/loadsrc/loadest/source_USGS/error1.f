************************************************************************
*
*     Subroutine ERROR1          Called by: DLIMIT,GETDTIME,INPUT1,
*                                           INPUT2,INPUT2B,INPUT3,INPUTE
*     print input error messages
*
************************************************************************
      SUBROUTINE ERROR1(NUMBER,IPARAM)
*
*     logical devices
*
      INCLUDE 'lda.inc'
*
*     subroutine arguments
*
      INTEGER*4 NUMBER,IPARAM
*
*     output format statements
*
 100  FORMAT(//,2X,'Error: The Estimated Values Print Option must ',
     &       'equal 0 or 1.',//,2X,'Estimated Values Print Option ',
     &       'specified (PRTOPT) : ',I5,//)
 200  FORMAT(//,2X,'Error: The Standard Error Option must equal ',
     &       '1, 2, or 3.',//,2X,'Standard Error Option ',
     &       'specified (SEOPT) : ',I5,//)
 300  FORMAT(//,2X,'Error: The Model Number must be between 0 and 11',
     &       ' (inclusive), or equal 99',//,2X,'Model Number ',
     &       'specified (MODNO) : ',I5,//)
 400  FORMAT(//,2X,'Error: The Beginning Month of the period must be',
     &       ' between 1 and 12 (inclusive)',//,2X,'Beginning Month ',
     &       'specified (PBMON):',I5,//)
 500  FORMAT(//,2X,'Error: The Ending Month of the period must be',
     &       ' between 1 and 12 (inclusive)',//,2X,'Ending Month ',
     &       'specified (PEMON):',I5,//)
 600  FORMAT(//,2X,'Error: Concentration Unit Flag must equal 1 or ',
     &       '2.',//,2X,
     &       'Concentration Unit Flag specified (UCFLAG) : ',I5,//)
 700  FORMAT(//,2X,'Error: Load Unit Flag must equal 1, 2, 3 or ',
     &       '4.',//,2X,'Load Unit Flag specified (ULFLAG):',I5,//)
 800  FORMAT(//,2X,'Error: Day must be between 1 and 31 ',
     &       '(inclusive)',//,2X,
     &       'Date specified (CDATE or EDATE; yyyymmdd format) : ',I8,
     &       //)
 900  FORMAT(//,2X,'Error: Month must be between 1 and 12 ',
     &       '(inclusive)',//,2X,
     &       'Date specified (CDATE or EDATE; yyyymmdd format) : ',I8,
     &       //)
 1000 FORMAT(//,2X,'Error: Year must be later than 1582',//,2X,
     &       'Date specified (CDATE or EDATE; yyyymmdd format) : ',I8,
     &       //)
 1100 FORMAT(//,2X,'Error: The Number of Observations of the ',
     &       'Data Variables per day must be',/,9X,
     &       '24 or less.',//,2X,'Number of Observations ',
     &       'specified (NOBSPD):',I5,//)
 1200 FORMAT(//,2X,'Error: One or more explanatory variables must be ',
     &       'present',/,9X,'in the user-defined model.',//,2X,
     &       'Number of Explanatory Variables specified (NEXPL):',I5,//)
 1300 FORMAT(//,2X,'Error: The Load Option must equal 0, 1, 2, or 3.',
     &       //,2X,'Load Option specified (LDOPT) : ',I5,//)
 1400 FORMAT(//,2X,'Error on line number',I4,' of the Calibration ',
     &       'File (excluding # lines)',//,2X,'Common reasons for ',
     &       'this error are:',//,4X,'1) extraneous lines in the ',
     &       'Calibration File (e.g. extra lines at end)',/,4X,
     &       '2) input line does not have all of the required fields',
     &       //)
 1500 FORMAT(//,2X,'Error on line number',I4,' of the Estimation ',
     &       'File (excluding # lines)',//,2X,'Common reasons for ',
     &       'this error are:',//,4X,'1) extraneous lines in the ',
     &       'Estimation File (e.g. extra lines at end)',/,4X,
     &       '2) input line does not have all of the required fields',
     &       //)
*
*     print error message and terminate execution
*
      IF (NUMBER .EQ. 1) THEN
         WRITE(*,100) IPARAM
         WRITE(LDECHO,100) IPARAM
      ELSEIF (NUMBER .EQ. 2) THEN
         WRITE(*,200) IPARAM
         WRITE(LDECHO,200) IPARAM
      ELSEIF (NUMBER .EQ. 3) THEN
         WRITE(*,300) IPARAM
         WRITE(LDECHO,300) IPARAM
      ELSEIF (NUMBER .EQ. 4) THEN
         WRITE(*,400) IPARAM
         WRITE(LDECHO,400) IPARAM
      ELSEIF (NUMBER .EQ. 5) THEN
         WRITE(*,500) IPARAM
         WRITE(LDECHO,500) IPARAM
      ELSEIF (NUMBER .EQ. 6) THEN
         WRITE(*,600) IPARAM
         WRITE(LDECHO,600) IPARAM
      ELSEIF (NUMBER .EQ. 7) THEN
         WRITE(*,700) IPARAM
         WRITE(LDECHO,700) IPARAM
      ELSEIF (NUMBER .EQ. 8) THEN
         WRITE(*,800) IPARAM
         WRITE(LDECHO,800) IPARAM
      ELSEIF (NUMBER .EQ. 9) THEN
         WRITE(*,900) IPARAM
         WRITE(LDECHO,900) IPARAM
      ELSEIF (NUMBER .EQ. 10) THEN
         WRITE(*,1000) IPARAM
         WRITE(LDECHO,1000) IPARAM
      ELSEIF (NUMBER .EQ. 11) THEN
         WRITE(*,1100) IPARAM
         WRITE(LDECHO,1100) IPARAM
      ELSEIF (NUMBER .EQ. 12) THEN
         WRITE(*,1200) IPARAM
         WRITE(LDECHO,1200) IPARAM
      ELSEIF (NUMBER .EQ. 13) THEN
         WRITE(*,1300) IPARAM
         WRITE(LDECHO,1300) IPARAM
      ELSEIF (NUMBER .EQ. 14) THEN
         WRITE(*,1400) IPARAM
         WRITE(LDECHO,1400) IPARAM
      ELSEIF (NUMBER .EQ. 15) THEN
         WRITE(*,1500) IPARAM
         WRITE(LDECHO,1500) IPARAM
      ENDIF

      WRITE(*,*) '  **** Fatal Input Error, See file echo.out ****'
      WRITE(*,*)
      STOP ' '

      END


