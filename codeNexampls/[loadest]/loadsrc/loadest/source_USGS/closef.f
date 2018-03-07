************************************************************************
*
*     Subroutine CLOSEF                         Called by: Main Program
*
*     close the echo output file and the constituent output files 
*
************************************************************************
      SUBROUTINE CLOSEF(NCONST,LDOUT)
*
*     logical devices
*
      INCLUDE 'lda.inc'
*
*     subroutine argument
*
      INTEGER*4 NCONST,LDOUT(*)
*
*     local var
*
      INTEGER*4 I
*
*     close the echo output file
*
      CLOSE (UNIT=LDECHO)
*
*     close the constituent output files
*
      DO 10 I = 1,NCONST
         CLOSE(LDOUT(I))
 10   CONTINUE

      RETURN
      END
