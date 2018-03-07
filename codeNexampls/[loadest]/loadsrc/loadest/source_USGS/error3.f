************************************************************************
*
*     Subroutine ERROR3                       Called by: INPUTC, INPUTE
*                                                
*     print error messages
*
************************************************************************
      SUBROUTINE ERROR3(NUMBER,PARAM1)
*
*     logical devices
*
      INCLUDE 'lda.inc'
*
*     subroutine arguments
*
      INTEGER*4 NUMBER
      DOUBLE PRECISION PARAM1
*
*     output format statements
*
 1000 FORMAT(//,2X,
     &'Error in Estimation File: Streamflow values must be greater ',
     &'than zero.',
     &//,2X,'Streamflow Value Specified: ',1PE16.8,//)
 2000 FORMAT(//,2X,
     &'Error in Calibration File: Streamflow values must be greater ',
     &'than zero.',
     &//,2X,'Streamflow Value Specified: ',1PE16.8,//)
*
*     print error message and terminate execution
*
      IF (NUMBER .EQ. 1) THEN
         WRITE(*,1000) PARAM1
         WRITE(LDECHO,1000) PARAM1
      ELSEIF (NUMBER .EQ. 2) THEN
         WRITE(*,2000) PARAM1
         WRITE(LDECHO,2000) PARAM1
      ENDIF

      WRITE(*,*) '  **** Fatal Input Error, See file echo.out ****'
      STOP ' '

      END
