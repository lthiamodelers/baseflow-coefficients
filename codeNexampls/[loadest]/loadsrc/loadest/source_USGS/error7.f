************************************************************************
*
*     Subroutine ERROR7               Called by: see format stmts below
*
*     print error messages
*
************************************************************************
      SUBROUTINE ERROR7(NUMBER)
*
*     logical devices
*
      INCLUDE 'lda.inc'
*
*     subroutine arguments
*
      INTEGER*4 NUMBER
*
*     output format statements
*
 100  FORMAT(//,2X,"Error in subroutine DCSEVL: number of terms < 1",/)
 200  FORMAT(//,2X,"Error in subroutine DCSEVL: number of terms > ",
     &       "1000",/)
 300  FORMAT(//,2X,"Error in subroutine DCSEVL: X outside the ",
     &       "interval (-1,+1)",/)
 400  FORMAT(//,2X,"Error in subroutine INITDS: series too short for ",
     &       "specified accuracy",/)
 500  FORMAT(//,2X,"Error in subroutine MVUEPHI: argument AW is too ",
     &       "large",/)
 600  FORMAT(//,2X,"Error in subroutine MVUEPHI: convergence failure",/)
 1000 FORMAT(//,2X,"Error in subroutine UPDATE: matrix does not have ",
     &       "full row rank",/)
 1100 FORMAT(//,2X,"Error in function EXPON: failure to converge",/)
 1200 FORMAT(//,2X,"Error in function EXPVAR: failure to converge",/)
 1300 FORMAT(//,2X,"Error in subroutine FACINV: matrix is singular",/)
*
*     print error message and terminate execution
*
      IF (NUMBER .EQ. 1) THEN
         WRITE(*,100)
         WRITE(LDECHO,100)
      ELSEIF (NUMBER .EQ. 2) THEN
         WRITE(*,200)
         WRITE(LDECHO,200)
      ELSEIF (NUMBER .EQ. 3) THEN
         WRITE(*,300)
         WRITE(LDECHO,300) 
      ELSEIF (NUMBER .EQ. 4) THEN
         WRITE(*,400)
         WRITE(LDECHO,400) 
      ELSEIF (NUMBER .EQ. 5) THEN
         WRITE(*,500)
         WRITE(LDECHO,500) 
      ELSEIF (NUMBER .EQ. 6) THEN
         WRITE(*,600)
         WRITE(LDECHO,600) 
      ELSEIF (NUMBER .EQ. 10) THEN
         WRITE(*,1000)
         WRITE(LDECHO,1000) 
      ELSEIF (NUMBER .EQ. 11) THEN
         WRITE(*,1100)
         WRITE(LDECHO,1100) 
      ELSEIF (NUMBER .EQ. 12) THEN
         WRITE(*,1200)
         WRITE(LDECHO,1200) 
      ELSEIF (NUMBER .EQ. 13) THEN
         WRITE(*,1300)
         WRITE(LDECHO,1300) 
      ENDIF

      WRITE(*,*) '  **** Fatal Error, See file echo.out ****'
      WRITE(*,*)
      STOP ' '

      END


