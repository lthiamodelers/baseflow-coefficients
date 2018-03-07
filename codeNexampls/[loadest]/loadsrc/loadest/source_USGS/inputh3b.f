************************************************************************
*
*     Subroutine INPUTH3B                            Called by: INPUTH3
*
*     For a user defined regression model (MODNO=99), determine the
*     number of additional data variables (NADDL) and the number of
*     explanatory variables (NEXPL).  For each explanatory variable,
*     determine corresponding data variable (DVNAME) and transformation
*     (TRANS).
*
*     locals vars
*     -----------
*     DVNAMEI  DVNAME on input
*     TRANSI   TRANS on input
*
************************************************************************
      SUBROUTINE INPUTH3B(NADDL,NEXPL,DVNAME,TRANS)
*
*     dimensional parameters and logical devices
*
      INCLUDE 'fmodules.inc'
      INCLUDE 'lda.inc'
*
*     subroutine arguments
*
      INTEGER*4 NADDL,NEXPL
      CHARACTER*5 DVNAME(*),TRANS(*)
*
*     local vars
*
      INTEGER*4 I,IADDL
      CHARACTER*5 DVNAMEI,TRANSI
      CHARACTER*500 BUFFER
*
*     input format statements
*
 1000 FORMAT(2I5)
 1100 FORMAT(I1)
*
*     output format statements
*
 2000 FORMAT(' Number of Additional Data Variables (NADDL):',I5)
 2100 FORMAT(' Number of Explanatory Variables (NEXPL)    :',I5)
 2200 FORMAT(/,' Data',T25,'Transformation',T46,'Explanatory',
     &       /,' Variable (DVNAME)',T25,'Code (TRANS)',T46,'Variable',
     &       /,1X,59('-'))
 2300 FORMAT(1X,A5,T25,A5,T46,A5)
 2301 FORMAT(1X,A5,T25,A5,T46,A5,'^2')
 2302 FORMAT(1X,A5,T25,A5,T46,'sqrt(',A5,')')
 2303 FORMAT(1X,A5,T25,A5,T46,'ln(',A5,')')
 2304 FORMAT(1X,A5,T25,A5,T46,'ln(',A5,')^2')
 2305 FORMAT(1X,A5,T25,A5,T46,'sin(2*Pi*',A5,')')
 2306 FORMAT(1X,A5,T25,A5,T46,'sin(4*Pi*',A5,')')
 2307 FORMAT(1X,A5,T25,A5,T46,'sin(6*Pi*',A5,')')
 2308 FORMAT(1X,A5,T25,A5,T46,'cos(2*Pi*',A5,')')
 2309 FORMAT(1X,A5,T25,A5,T46,'cos(4*Pi*',A5,')')
 2310 FORMAT(1X,A5,T25,A5,T46,'cos(6*Pi*',A5,')')
 2400 FORMAT(/,' *** WARNING: Sin and Cos transformations are normally',
     &       ' ***',/,5X,
     &       'applied to the time variable only (DTIME).  A',/,5X,
     &       'transformation code of ',A5,' has been specified ',/,5X,
     &       'for ',A5,/)
*
*     read the number of additional data variables, NADDL
*
*     NADDL should never exceed 9 due to the way in which the ADDLX
*     independent variable names are formatted (a single digit is
*     assumed to follow 'ADDL'; see FORMAT statement for reading IADDL
*     in INPUT2).  This is probably not a problem -- its hard to
*     envision applications with more than 3 or 4 addional variables,
*     let alone more than 9.
* 
      CALL GETLINE(LDHEAD,BUFFER)
      READ(BUFFER,1000) NADDL
      WRITE(LDECHO,2000) NADDL
      IF (NADDL.GT.9) CALL ERROR2(2,NADDL,9)
*
*     read the number of explanatory variables, NEXPL.  There must
*     be at least one explanatory variable, and the total number of
*     explanatory variables cannot exceed the maximum number of
*     parameters minus 1 (in general, the number of parameters equals 
*     1 constant plus NEXPL explanatory variables).
*
      CALL GETLINE(LDHEAD,BUFFER)
      READ(BUFFER,1000) NEXPL
      WRITE(LDECHO,2100) NEXPL
      IF (NEXPL.LT.1) CALL ERROR1(12,NEXPL)
      IF (NEXPL.GT.MAXPARMS-1) CALL ERROR2(7,NEXPL,MAXPARMS-1)
*
*     for each explanatory variable, read the name of the data variable
*     (DVNAME) and the transformation code (TRANS).
*
      WRITE(LDECHO,2200)

      DO 10 I=1,NEXPL
         CALL GETLINE(LDHEAD,BUFFER)
         READ (BUFFER,*) DVNAMEI,TRANSI
*
*        Echo DVNAME and TRANS; print error if valid transformation code
*        not found.
*
         IF (TRANSI .EQ. 'NONE') THEN
            WRITE(LDECHO,2300) DVNAMEI,TRANSI,DVNAMEI
         ELSEIF(TRANSI .EQ. 'SQ') THEN 
            WRITE(LDECHO,2301) DVNAMEI,TRANSI,DVNAMEI
         ELSEIF(TRANSI .EQ. 'SQRT') THEN 
            WRITE(LDECHO,2302) DVNAMEI,TRANSI,DVNAMEI
         ELSEIF(TRANSI .EQ. 'LN') THEN 
            WRITE(LDECHO,2303) DVNAMEI,TRANSI,DVNAMEI
         ELSEIF(TRANSI .EQ. 'LNSQ') THEN 
            WRITE(LDECHO,2304) DVNAMEI,TRANSI,DVNAMEI
         ELSEIF(TRANSI .EQ. 'SIN2P') THEN 
            WRITE(LDECHO,2305) DVNAMEI,TRANSI,DVNAMEI
         ELSEIF(TRANSI .EQ. 'SIN4P') THEN 
            WRITE(LDECHO,2306) DVNAMEI,TRANSI,DVNAMEI
         ELSEIF(TRANSI .EQ. 'SIN6P') THEN 
            WRITE(LDECHO,2307) DVNAMEI,TRANSI,DVNAMEI
         ELSEIF(TRANSI .EQ. 'COS2P') THEN 
            WRITE(LDECHO,2308) DVNAMEI,TRANSI,DVNAMEI
         ELSEIF(TRANSI .EQ. 'COS4P') THEN 
            WRITE(LDECHO,2309) DVNAMEI,TRANSI,DVNAMEI
         ELSEIF(TRANSI .EQ. 'COS6P') THEN
            WRITE(LDECHO,2310) DVNAMEI,TRANSI,DVNAMEI
         ELSE
            WRITE(LDECHO,2300) DVNAMEI,TRANSI
            CALL ERROR5(3,TRANSI)
         ENDIF
*
*        make sure data variable names are valid.  Valid names include
*        Q, DTIME, and ADDLi, where i is less than or equal to NADDL.
*
         IF (DVNAMEI(1:4) .EQ. 'ADDL') THEN
            READ(DVNAMEI(5:5),1100) IADDL
            IF (IADDL .GT. NADDL) CALL ERROR5(2,DVNAMEI)
         ELSEIF (DVNAMEI.NE.'Q' .AND. DVNAMEI.NE.'DTIME') THEN
            CALL ERROR5(2,DVNAMEI)
         ENDIF
*         
*        Sin and Cos transformations are normally applied to temporal
*        terms only.  Issue a non-fatal warning if sin or cos
*        transformations are applied to anything other than DTIME
*        
         IF ((TRANSI(1:3) .EQ. 'SIN' .OR. TRANSI(1:3) .EQ. 'COS')
     &        .AND. (DVNAMEI .NE. 'DTIME'))
     &        WRITE(LDECHO,2400) TRANSI,DVNAMEI
*
*        now that error checking is done, copy strings to vector vars
*
         DVNAME(I) = DVNAMEI
         TRANS(I) = TRANSI
 10   CONTINUE

      RETURN
      END
