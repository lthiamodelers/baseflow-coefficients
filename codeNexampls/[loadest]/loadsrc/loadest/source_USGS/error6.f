************************************************************************
*
*     Subroutine ERROR6                              Called by: INPUTE
*
*     print input error messages
*
************************************************************************
      SUBROUTINE ERROR6(NUMBER,STRVAR,PARAM1,PARAM2)
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
     &'Error in Estimation File: Incorrect number of observations on ',
     & A8,//,10X,'Number of observations on ',A8,14X,':',I5,/,10X,                   : ',A8,/,10X,
     & 'Required number of observations per day (NOBSPD):',I5,//)
*
*     print error message and terminate execution
*
      IF (NUMBER .EQ. 1) THEN
         WRITE(*,1000) STRVAR,STRVAR,PARAM1,PARAM2
         WRITE(LDECHO,1000) STRVAR,STRVAR,PARAM1,PARAM2
      ENDIF

      WRITE(*,*) '  **** Fatal Input Error, See file echo.out ****'
      STOP

      END

