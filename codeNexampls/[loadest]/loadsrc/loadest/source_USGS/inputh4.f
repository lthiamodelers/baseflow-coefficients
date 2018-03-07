************************************************************************
*
*     Subroutine INPUTH4                               Called by: INIT3
*
*     input UCFLAG, ULFLAG, CNAME
*
************************************************************************
      SUBROUTINE INPUTH4(CFACTOR,CNAME,CUNITSTR,LFACTOR,LUNITSTR,NCONST)
*
*     logical devices
*
      INCLUDE 'lda.inc'
*
*     subroutine arguments
*
      INTEGER*4 NCONST
      DOUBLE PRECISION CFACTOR(*),LFACTOR(*)
      CHARACTER*4 CUNITSTR(*)
      CHARACTER*9 LUNITSTR(*)
      CHARACTER*45 CNAME(*)
*
*     local vars
*
      INTEGER*4 J,UCFLAG,ULFLAG
      CHARACTER*8 CUNIT(2) / 'mg/L (1)','ug/L (2)' /
      CHARACTER*9 LUNIT(4) /'kg/d (1)','g/d (2)','lb/d (3)','ton/d (4)'/
      CHARACTER*500 BUFFER
*
*     input format statements
*
 1000 FORMAT(A45,I5,I5)
*
*     output format statements
*
 2000 FORMAT(/,T47,'Conc.',T64,'Load',/,1X,'Constituent',34X,
     &       'Units (Flag)',T64,'Units (Flag)',/,1X,74('-'))
 2100 FORMAT(1X,A45,A8,T64,A9)
*
*     Read name and unit flags for each constituent.  For valid unit
*     flags, set conversion factors (CFACTOR and LFACTOR) and unit
*     strings (CUNITSTR and LUNITSTR).  Call error routines if unit
*     flags are invalid.
*
      WRITE(LDECHO,2000)
      DO 10 J=1,NCONST
         CALL GETLINE(LDHEAD,BUFFER)
         READ (BUFFER,1000) CNAME(J),UCFLAG,ULFLAG
         WRITE (LDECHO,2100) CNAME(J),CUNIT(UCFLAG),LUNIT(ULFLAG)
         IF (UCFLAG .EQ. 1) THEN
            CFACTOR(J) = 86400.D-3 * 0.3048D0 * 0.3048D0 * 0.3048D0
            CUNITSTR(J) = 'MG/L'
         ELSEIF (UCFLAG .EQ. 2) THEN
            CFACTOR(J) = 86400.D-6 * 0.3048D0 * 0.3048D0 * 0.3048D0
            CUNITSTR(J) = 'UG/L'
         ELSE
            CALL ERROR1(6,UCFLAG)
         ENDIF
         IF (ULFLAG .EQ. 1) THEN
            LFACTOR(J) = 1.D0
            LUNITSTR(J) =  '[KG/DAY]'
         ELSEIF (ULFLAG .EQ. 2) THEN
            LFACTOR(J) = 1000.D0
            LUNITSTR(J) = '[G/DAY]'
         ELSEIF (ULFLAG .EQ. 3) THEN
            LFACTOR(J) = 2.2046D0
            LUNITSTR(J) = '[LBS/DAY]'
         ELSEIF (ULFLAG .EQ. 4) THEN
            LFACTOR(J) = 2.2046D0/2000.D0
            LUNITSTR(J) = '[TON/DAY]'
         ELSE
            CALL ERROR1(7,ULFLAG)
         ENDIF
 10   CONTINUE

      RETURN
      END
