************************************************************************
*
*     Subroutine OUTAML                              Called by: AMLLOAD
*
*     output mean AMLE loads with confidence intervals and standard
*     errors for entire estimation period, a user-defined season, or an
*     individual month
*
************************************************************************
      SUBROUTINE OUTAML(TSTR,LFACTOR,LOAD,L95,N,SP,U95,VAR,LDOUT)
*
*     subroutine arguments
*
      INTEGER*4 N,LDOUT
      DOUBLE PRECISION LFACTOR,LOAD,L95,U95,SP,VAR
      CHARACTER*(*) TSTR
*
*     format statements
*
 1021 FORMAT(A13,I5,T20,1PE10.3,T31,1PE10.3,T42,1PE10.3,T54,1PE10.3,T65,
     &       1PE10.3)
 1031 FORMAT(A13,I5,T20,F10.2,T31,F10.2,T42,F10.2,T54,F10.2,T65,F10.2)
 1041 FORMAT(A13,I5,T20,F10.0,T31,F10.0,T42,F10.0,T54,F10.0,T65,F10.0)
*
*     output results
*
      IF (U95*LFACTOR .LT. 0.1 .OR. U95*LFACTOR .GT. 1.D7) THEN
         WRITE(LDOUT,1021) TSTR,N,LOAD*LFACTOR,L95*LFACTOR,U95*LFACTOR,
     &                     SP*LFACTOR,DSQRT(VAR)*LFACTOR
      ELSEIF (U95*LFACTOR .LT. 1.D3) THEN
         WRITE(LDOUT,1031) TSTR,N,LOAD*LFACTOR,L95*LFACTOR,U95*LFACTOR,
     &                     SP*LFACTOR,DSQRT(VAR)*LFACTOR
      ELSE
         WRITE(LDOUT,1041) TSTR,N,LOAD*LFACTOR,L95*LFACTOR,U95*LFACTOR,
     &                     SP*LFACTOR,DSQRT(VAR)*LFACTOR
      ENDIF

      RETURN
      END


