************************************************************************
*
*     Subroutine INIT2                          Called by: Main program
*
*     continue reading from the header file
*
************************************************************************
      SUBROUTINE INIT2(MODNO,NADDL,NCONST,NEXPL,NSEAS,PBMON,PEMON,NPAR,
     &                 SBEG,SEND,DVNAME,TRANS)
*
*     subroutine arguments
*
      INTEGER*4 MODNO,NADDL,NCONST,NEXPL,NSEAS,PBMON,PEMON,NPAR(*)
      CHARACTER*4 SBEG(*),SEND(*)
      CHARACTER*5 DVNAME(*),TRANS(*)
*
*     input season definition
*
      IF(NSEAS .GT. 0) CALL INPUTH2(SBEG,SEND,NSEAS)
*
*     read model information and constituents
*
      CALL INPUTH3(MODNO,NADDL,NCONST,NEXPL,PEMON,PBMON,DVNAME,TRANS)
*
*     set number of model parameters 
*
      CALL SETNPAR(MODNO,NEXPL,NPAR)

      RETURN
      END
