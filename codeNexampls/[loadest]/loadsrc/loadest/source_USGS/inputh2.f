************************************************************************
*
*     Subroutine INPUTH2                                Called by: INIT2
*
*     input SBEG and SEND
*
************************************************************************
      SUBROUTINE INPUTH2(SBEG,SEND,NSEAS)
*
*     logical devices
*
      INCLUDE 'lda.inc'
*
*     subroutine arguments
*
      INTEGER*4 NSEAS
      CHARACTER*4 SBEG(*),SEND(*)
*
*     local vars
*
      INTEGER*4 I
      CHARACTER*500 BUFFER
*
*     output format statement
*
 2000 FORMAT(1X,I3,T20,A4,T41,A4)
*
*     If seasonal loads are requested (NSEAS > 0), read the the
*     begin and end dates (SBEG, SEND) for each season and output.
*
      DO 10 I=1,NSEAS
         CALL GETLINE(LDHEAD,BUFFER)
         READ (BUFFER,*) SBEG(I),SEND(I)
         WRITE(LDECHO,2000) I,SBEG(I),SEND(I)
 10   CONTINUE

      RETURN
      END
