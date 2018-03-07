**********************************************************************
*
*     Subroutine ERROR4                      Called by: INPUTE
*
*     print input error messages
*
**********************************************************************
      SUBROUTINE ERROR4(NUMBER,STRVAR,PARAM1,PARAM2)
*
*     logical devices
*
      INCLUDE 'lda.inc'
*
*     subroutine arguments
*
      INTEGER*4 NUMBER,PARAM1,PARAM2
      CHARACTER*(*) STRVAR
*
*     output format statements
*
 1000 FORMAT(//,2X,
     &'Error in Estimation File: Time on a given date must be in ',
     &'ascending order',//,10X,
     &'Current Date (EDATE)      :',A8,/,10X,
     &'Current Time (ETIME)      :',I5,/,10X,
     &'Previous Time (ETIME(j-1)):',I5,//)
*
*     print error message and terminate execution
*
      IF (NUMBER .EQ. 1) THEN
         WRITE(*,1000) STRVAR,PARAM1,PARAM2
         WRITE(LDECHO,1000) STRVAR,PARAM1,PARAM2
      ENDIF

      WRITE(*,*) '  **** Fatal Input Error, See file echo.out ****'
      STOP ' '

      END

