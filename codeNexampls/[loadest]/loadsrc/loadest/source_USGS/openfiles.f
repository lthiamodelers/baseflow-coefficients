************************************************************************
*
*     Subroutine OPENFILES                               Called by: INIT
*
*     open echo.out and all the input files; determine the length of the
*     calibration and estimation files.
*
************************************************************************
      SUBROUTINE OPENFILES(NOBSCI,NOBSE)
*
*     logical devices
*
      INCLUDE 'lda.inc'
*
*     subroutine arguments
*
      INTEGER*4 NOBSCI,NOBSE
*
*     local vars
*
      CHARACTER*40 FILE
      CHARACTER*500 BUFFER
*
*     input format statement
*
 1000 FORMAT(A40)
*
*     output format statement
*
 2000 FORMAT(1X,A17,1X,A40)
*
*     open output file for run information and input echoing and output
*     heading
*
      OPEN(UNIT=LDECHO,FILE='echo.out',STATUS='UNKNOWN')
      CALL HEADING(LDECHO)
*
*     open the control file
*
      CALL OPENIN(LDCTRL,'control.inp')
*
*     determine the header file and open
*
      CALL GETLINE(LDCTRL,BUFFER)
      READ (BUFFER,1000) FILE
      CALL OPENIN(LDHEAD,FILE)
      WRITE(LDECHO,2000) 'HEADER FILE     :',FILE
*
*     determine the calibration file and open
*
      CALL GETLINE(LDCTRL,BUFFER)
      READ(BUFFER,1000) FILE
      CALL OPENIN(LDCAL,FILE)
      WRITE(LDECHO,2000) 'CALIBRATION FILE:',FILE
*
*     determine the number of observations in the calibration data set
*
      CALL CLINES(LDCAL,NOBSCI)
*
*     determine the estimation file and open
*
      CALL GETLINE(LDCTRL,BUFFER)
      READ (BUFFER,1000) FILE
      CALL OPENIN(LDEST,FILE)
      WRITE(LDECHO,2000) 'ESTIMATION FILE :',FILE
*
*     determine the number of observations of the data variable(s) in
*     the estimation data set by counting the lines in the estimation
*     file (subtract 1 for the line specifying NOBSPD)
*
      CALL CLINES(LDEST,NOBSE)
      NOBSE = NOBSE - 1

      RETURN
      END
