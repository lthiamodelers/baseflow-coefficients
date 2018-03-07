************************************************************************
*
*     Subroutine CLINES                            Called by: OPENFILES
*
*     read from the input file designated by LDUNIT, counting the
*     number of lines (excluding comments).  This allows for the
*     determination of NOBSCI & NOBSE.
*
************************************************************************
      SUBROUTINE CLINES(LDUNIT,NUM)
*
*     subroutine arguments
*
      INTEGER*4 LDUNIT,NUM
*
*     local variable
*
      CHARACTER*(500) BUFFER
*
*     format statement
*
 1000 FORMAT(A500)
*
*     determine the number of lines in file (excluding comments);
*     rewind input file so its ready for the actual input process
*
      NUM = 0
 10   READ (LDUNIT,1000,END=20) BUFFER
      IF (INDEX(BUFFER,'#') .NE. 1) NUM = NUM + 1
      GOTO 10
 20   REWIND(LDUNIT)

      RETURN
      END
