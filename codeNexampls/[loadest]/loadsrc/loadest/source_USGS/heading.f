************************************************************************
*
*     Subroutine HEADING                   Called by: OPENFILES, SETUPC
*
*     print model info heading to specified output file 
*
************************************************************************
      SUBROUTINE HEADING(LDNUM)
*
*     subroutine arguments
*
      INTEGER*4 LDNUM
*
*     format statement
*
 1000 FORMAT(//,37X,'LOADEST',/,22X,
     &       'A Program to Estimate Constituent Loads',/,16X,
     &       'U.S. Geological Survey, Version: MOD48 (March 2013)',/,
     &        16X,49('-'),/)
*
*     write heading to file
*
      WRITE(LDNUM,1000)

      RETURN
      END
