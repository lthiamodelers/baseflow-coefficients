************************************************************************
*
*     Subroutine SETXL                       Called by: CALIBR, ESTLOAD 
*
*     set explanatory variables (XLCAL for calibration, XLEST for
*     estimation)
*
************************************************************************
      SUBROUTINE SETXL(NMOD,NUMOBS,LQCENT,ADDL,FLOW,DECTIM,PER,XL,LDIM,
     &                 NEXPL,DVNAME,TRANS,NADDL)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 NMOD,NUMOBS,LDIM,NEXPL,NADDL
      DOUBLE PRECISION LQCENT
      DOUBLE PRECISION DECTIM(*),FLOW(*),XL(LDIM,*),PER(*)
      DOUBLE PRECISION ADDL(NADDL,*)
      CHARACTER*5 DVNAME(*),TRANS(*)
*
*     local vars
*
      INTEGER*4 I,J
      DOUBLE PRECISION PI
*
*     function declaration
*
      DOUBLE PRECISION TRANSF
*     
*     define PI 
*     
      PARAMETER (PI=3.1415926536D0)
*
*     generate explanatory variables for one of the 12 models. Note
*     that the user-defined model is always the last model
*     (NMOD=NMODELS, see SETNPAR & SETUPC).
*
      IF (NMOD .EQ. 1) THEN
         DO 10 I=1,NUMOBS
            XL(I,1) = 1.D0
            XL(I,2) = DLOG(FLOW(I)) - LQCENT
 10      CONTINUE
      ELSEIF (NMOD .EQ. 2) THEN
         DO 20 I=1,NUMOBS
            XL(I,1) = 1.D0
            XL(I,2) = DLOG(FLOW(I)) - LQCENT
            XL(I,3) = XL(I,2)**2
 20      CONTINUE
      ELSEIF (NMOD .EQ. 3) THEN
         DO 30 I=1,NUMOBS
            XL(I,1) = 1.D0
            XL(I,2) = DLOG(FLOW(I)) - LQCENT
            XL(I,3) = DECTIM(I)
 30      CONTINUE
      ELSEIF (NMOD .EQ. 4) THEN
         DO 40 I=1,NUMOBS
            XL(I,1) = 1.D0
            XL(I,2) = DLOG(FLOW(I)) - LQCENT
            XL(I,3) = DSIN(2.D0*PI*DECTIM(I))
            XL(I,4) = DCOS(2.D0*PI*DECTIM(I))
 40      CONTINUE
      ELSEIF (NMOD .EQ. 5) THEN
         DO 50 I=1,NUMOBS
            XL(I,1) = 1.D0
            XL(I,2) = DLOG(FLOW(I)) - LQCENT
            XL(I,3) = XL(I,2)**2
            XL(I,4) = DECTIM(I)
 50      CONTINUE
      ELSEIF (NMOD .EQ. 6) THEN
         DO 60 I=1,NUMOBS
            XL(I,1) = 1.D0
            XL(I,2) = DLOG(FLOW(I)) - LQCENT
            XL(I,3) =  XL(I,2)**2
            XL(I,4) = DSIN(2.D0*PI*DECTIM(I))
            XL(I,5) = DCOS(2.D0*PI*DECTIM(I))
 60      CONTINUE
      ELSEIF (NMOD .EQ. 7) THEN
         DO 70 I=1,NUMOBS
            XL(I,1) = 1.D0
            XL(I,2) = DLOG(FLOW(I)) - LQCENT
            XL(I,3) = DSIN(2.D0*PI*DECTIM(I))
            XL(I,4) = DCOS(2.D0*PI*DECTIM(I))
            XL(I,5) = DECTIM(I)
 70      CONTINUE
      ELSEIF (NMOD .EQ. 8) THEN
         DO 80 I=1,NUMOBS
            XL(I,1) = 1.D0
            XL(I,2) = DLOG(FLOW(I)) - LQCENT
            XL(I,3) = XL(I,2)**2
            XL(I,4) = DSIN(2.D0*PI*DECTIM(I))
            XL(I,5) = DCOS(2.D0*PI*DECTIM(I))
            XL(I,6) = DECTIM(I)
 80      CONTINUE
      ELSEIF (NMOD .EQ. 9) THEN
         DO 90 I=1,NUMOBS
            XL(I,1) = 1.D0
            XL(I,2) = DLOG(FLOW(I)) - LQCENT
            XL(I,3) = XL(I,2)**2
            XL(I,4) = DSIN(2.D0*PI*DECTIM(I))
            XL(I,5) = DCOS(2.D0*PI*DECTIM(I))
            XL(I,6) = DECTIM(I)
            XL(I,7) = DECTIM(I)*DECTIM(I)
 90      CONTINUE
      ELSEIF (NMOD .EQ. 10) THEN
         DO 100 I=1,NUMOBS
            XL(I,1) = 1.D0
            XL(I,2) = PER(I)
            XL(I,3) = DLOG(FLOW(I)) - LQCENT
            XL(I,4) = XL(I,3)*PER(I)
 100      CONTINUE
      ELSEIF (NMOD .EQ. 11) THEN
         DO 110 I=1,NUMOBS
            XL(I,1) = 1.D0
            XL(I,2) = PER(I)
            XL(I,3) = DLOG(FLOW(I)) - LQCENT
            XL(I,4) = XL(I,3)*PER(I)
            XL(I,5) = XL(I,3)**2
            XL(I,6) = XL(I,5) * PER(I)
 110     CONTINUE
      ELSEIF (NMOD .EQ. NMODELS) THEN
         DO 130 I=1,NUMOBS
            XL(I,1) = 1.D0
            DO 120 J=2,NEXPL+1
               XL(I,J) = TRANSF(DVNAME(J-1),TRANS(J-1),DECTIM(I),
     &                          FLOW(I),ADDL(1,I),LQCENT)
 120        CONTINUE
 130     CONTINUE
      ENDIF

      RETURN
      END






