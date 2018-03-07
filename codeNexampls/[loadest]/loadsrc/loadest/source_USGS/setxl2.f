************************************************************************
*
*     Subroutine SETXL2    Called by: AMLLOAD, LADLOAD, MLELOAD
*
*     set explanatory variables for seasonal loads
*
************************************************************************
      SUBROUTINE SETXL2(EDATE,NOBSE,NOBS,NPAR,SBEG,SEND,XLEST,SXLEST)
*
*     subroutine arguments
*
      INTEGER*4 NOBSE,NOBS,NPAR
      DOUBLE PRECISION XLEST(NOBSE,*),SXLEST(NOBSE,*)
      CHARACTER*4 SBEG,SEND
      CHARACTER*8 EDATE(*)
*
*     local vars
*
      INTEGER*4 I
*
*     function declaration
*
      INTEGER*4 GETINT
*
*     search through the "observations" of the explanatory variables and
*     select those for the current season (set SXLEST based on XLEST if
*     EDATE falls within the current season.) The first set of IF stmts
*     is for the case where the ending date is greater than the starting
*     date - e.g. if loads are requested for Feb-May; the second set of
*     IF stmts is for when the end date is less than the starting date -
*     e.g. when loads are requested for Dec-Feb.
*
      NOBS = 0
      DO 10 I = 1,NOBSE
         IF (GETINT(SEND,1,4).GT.GETINT(SBEG,1,4)) THEN
            IF ((GETINT(EDATE(I),5,8).GE.GETINT(SBEG,1,4)) .AND.
     &         (GETINT(EDATE(I),5,8).LE.GETINT(SEND,1,4))) THEN
               CALL SETXL3(NOBS,NPAR,XLEST,SXLEST,NOBSE,I)
            ENDIF
         ELSE
            IF ((GETINT(EDATE(I),5,8).LE.GETINT(SEND,1,4)) .OR.
     &         (GETINT(EDATE(I),5,8).GE.GETINT(SBEG,1,4))) THEN
               CALL SETXL3(NOBS,NPAR,XLEST,SXLEST,NOBSE,I)
            ENDIF
         ENDIF
 10   CONTINUE

      RETURN
      END





