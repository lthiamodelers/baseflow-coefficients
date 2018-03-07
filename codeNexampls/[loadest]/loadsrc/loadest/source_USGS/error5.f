************************************************************************
*
*     Subroutine ERROR5                      Called by: DLIMIT, INPUT2B
*
*     print input error messages
*
************************************************************************
      SUBROUTINE ERROR5(NUMBER,STRVAR)
*
*     dimensional parameters and logical devices
*
      INCLUDE 'lda.inc'
*
*     subroutine arguments
*
      INTEGER*4 NUMBER
      CHARACTER*(*) STRVAR
*
*     output format statements
*
 1000 FORMAT(//,2X,"Error: The constituent concentration cannot be ",
     &       "both missing (negative)",/,9X,"and censored ('<').",//,2X,
     &       "Constituent concentration specified (CCONC): ",A30,//)
 2000 FORMAT(//,2X,"Error: Invalid Data Variable Name.  Valid names ",
     &       "include Q,",/,10X,"DTIME, and ADDLi, where i is ",
     &       "less than or equal to NADDL.",
     &       //,2X,"Data Variable Name specified (DVNAME): ",A5,//)
 3000 FORMAT(//,2X,"Error: Invalid Transformation Code.  Valid codes ",
     &       "include NONE, SQ, SQRT, ",/,9X,"LN, LNSQ, SIN2P, ",
     &       "SIN4P, SIN6P, COS2P, COS4P, and COS6P.",//,2X,
     &       "Transformation code specified (TRANS): ",A5,//)
*
*     print error message and terminate execution
*
      IF (NUMBER .EQ. 1) THEN
         WRITE(*,1000) STRVAR
         WRITE(LDECHO,1000) STRVAR
      ELSEIF (NUMBER .EQ. 2) THEN
         WRITE(*,2000) STRVAR
         WRITE(LDECHO,2000) STRVAR
      ELSEIF (NUMBER .EQ. 3) THEN
         WRITE(*,3000) STRVAR
         WRITE(LDECHO,3000) STRVAR
      ENDIF

      WRITE(*,*) '  **** Fatal Input Error, See file echo.out ****'
      WRITE(*,*)
      STOP ' '

      END


