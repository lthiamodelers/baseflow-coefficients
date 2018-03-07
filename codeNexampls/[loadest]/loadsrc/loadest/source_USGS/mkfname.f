************************************************************************
*
*     Subroutine MKFNAME                              Called by: SETUPC
*
*     create an output file name using the constituent name, CNAME
*
************************************************************************
      SUBROUTINE MKFNAME(OUTFILE,OUTFILE2,OUTFILE3,CNAME)
*
*     subroutine arguments
*
      CHARACTER*(*) CNAME
      CHARACTER*(*) OUTFILE,OUTFILE2,OUTFILE3
*
*     local vars
*
      INTEGER*4 I,J,LASTCHAR
*
*     initialize
*
      OUTFILE = CNAME
      LASTCHAR = 1
*
*     determine the last non-space character in the constituent name
*
      DO 10 I = 1,45
         IF (OUTFILE(I:I) .NE. ' ') LASTCHAR = I
 10   CONTINUE
*
*     parse out embedded spaces from constituent name (DOWHILE there
*     are embedded spaces)
*
 20   J = INDEX(OUTFILE,' ') 
      IF (J.LT.LASTCHAR) THEN
         DO 30 I=J,LASTCHAR
            OUTFILE(I:I) = OUTFILE(I+1:I+1)
 30      CONTINUE
         LASTCHAR = LASTCHAR - 1
         GOTO 20
      ENDIF
*
*     append suffix to filename
*
      OUTFILE2 = OUTFILE
      OUTFILE3 = OUTFILE
      OUTFILE(LASTCHAR+1:LASTCHAR+4) = '.out'
      OUTFILE2(LASTCHAR+1:LASTCHAR+4) = '.res'
      OUTFILE3(LASTCHAR+1:LASTCHAR+4) = '.ind'

      RETURN
      END

