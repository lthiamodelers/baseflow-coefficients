************************************************************************
*
*     Subroutine OUTEST            Called by: LADLOAD, MLELOAD
*
*     output mean loads (based on MLE or LAD) with standard errors for
*     entire estimation period, a user-defined season, or an individual
*     month
*
************************************************************************
      SUBROUTINE OUTEST(TSTR,LFACTOR,LOAD,N,SEOPT,VAR,LDOUT)
*
*     subroutine arguments
*
      INTEGER*4 N,SEOPT,LDOUT
      DOUBLE PRECISION LFACTOR,LOAD,VAR
      CHARACTER*(*) TSTR
*
*     format statements
*
 1021 FORMAT(A13,I5,T20,1PE10.3,T31,1PE10.3)
 1031 FORMAT(A13,I5,T20,F10.2,T31,F10.2)
 1041 FORMAT(A13,I5,T20,F10.0,T31,F10.0)
*
*     output results
*
      IF (SEOPT .NE. 1) THEN
         IF (LOAD*LFACTOR .LT. 0.1 .OR. LOAD*LFACTOR .GT. 1.D7) THEN
            WRITE(LDOUT,1021) TSTR,N,LOAD*LFACTOR,DSQRT(VAR)*LFACTOR
         ELSEIF (LOAD*LFACTOR .LT. 1.D3) THEN
            WRITE(LDOUT,1031) TSTR,N,LOAD*LFACTOR,DSQRT(VAR)*LFACTOR
         ELSE
            WRITE(LDOUT,1041) TSTR,N,LOAD*LFACTOR,DSQRT(VAR)*LFACTOR
         ENDIF
      ELSE
         IF (LOAD*LFACTOR .LT. 0.1 .OR. LOAD*LFACTOR .GT. 1.D7) THEN
            WRITE(LDOUT,1021) TSTR,N,LOAD*LFACTOR
         ELSEIF (LOAD*LFACTOR .LT. 1.D3) THEN
            WRITE(LDOUT,1031) TSTR,N,LOAD*LFACTOR
         ELSE
            WRITE(LDOUT,1041) TSTR,N,LOAD*LFACTOR
         ENDIF
      ENDIF

      RETURN
      END


