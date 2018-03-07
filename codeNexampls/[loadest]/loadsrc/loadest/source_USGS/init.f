************************************************************************
*
*     Subroutine INIT                          Called by: Main program
*
*     initialize, open files, determine the length of the calibration
*     and estimation files, and read the first part of the header file
*
************************************************************************
      SUBROUTINE INIT(LDOPT,NOBSCI,NOBSE,NSEAS,PRTOPT,SEOPT,TITLE)
*
*     subroutine args
*
      INTEGER*4 LDOPT,NOBSCI,NOBSE,NSEAS,PRTOPT,SEOPT
      CHARACTER*80 TITLE
*
*     initialize Logical Device Assignments (LDAs)
*
      CALL LDAINIT
*
*     open echo.out and all the input files; determine the length of the
*     calibration and estimation files.
*
      CALL OPENFILES(NOBSCI,NOBSE)
*
*     read the title, print option, standard error option, etc. from the
*     header file.
*
      CALL INPUTH(TITLE,LDOPT,PRTOPT,SEOPT,NSEAS)

      RETURN
      END
