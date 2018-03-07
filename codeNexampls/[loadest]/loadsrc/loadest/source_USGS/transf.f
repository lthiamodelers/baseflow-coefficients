************************************************************************
*
*     Function TRANSF                                  Called by: SETXL
*
*     create an observation of an explanatory variable using the 
*     observed value of the data variable (FLOW, DECTIM, or ADDLi, as
*     determined from DVNAME) and the requested transformation (TRANS)
*
*     local vars
*     ----------
*     IADDL   index denoting the additional data variable.  (when the
*             data variable is an 'additional data variable', ADDLI,
*             IADDL tells us which one it is)
*
*     XVAL    observed value of the data variable
*
************************************************************************
      DOUBLE PRECISION FUNCTION TRANSF(DVNAME,TRANS,DECTIM,FLOW,ADDLI,
     &                                 LQCENT)
*
*     function arguments
*
      CHARACTER*(*) DVNAME,TRANS
      DOUBLE PRECISION DECTIM,FLOW,LQCENT
      DOUBLE PRECISION ADDLI(*)
*
*     local var
*
      INTEGER*4 IADDL
      DOUBLE PRECISION XVAL
      DOUBLE PRECISION PI
*     
*     define PI 
*     
      PARAMETER (PI=3.1415926536D0)
*
*     format statement for determining the i in ADDLi
*
 1000 FORMAT(I1)
*
*     using DVNAME, determine the data variable (FLOW, DECTIM, or ADDLI)
*     that's used to create the explanatory variable.  Then set XVAL
*     equal to the observation of the data variable.
*
*     note that valid DVNAMEs are checked on input -- therefore if the
*     ELSE statement is reached, DVNAME is one of the additional
*     data variables ADDLi (where i is less than or equal to NADDL).  I
*     (IADDL) is determined by reading the 5th character in DVNAME.
*
      IF (DVNAME .EQ. 'Q') THEN
         XVAL = FLOW
      ELSEIF (DVNAME .EQ. 'DTIME') THEN
         XVAL = DECTIM
      ELSE
         READ(DVNAME(5:5),1000) IADDL 
         XVAL = ADDLI(IADDL)
      ENDIF
*
*     XVAL is now equal to the observation of the data variable. Perform
*     the requested transformation and return the value as TRANSF.
*
*     Note that if the data variable is 'Q', the log transformations
*     include the log Q adjustment such that the user-defined model is
*     consistent with the predefined models.  DECTIM has already been
*     adjusted (the decimal time adjustment) so no special treatment is
*     needed here.
*
      IF (TRANS .EQ. 'NONE') THEN
         TRANSF = XVAL
      ELSEIF(TRANS .EQ. 'SQ') THEN 
         TRANSF = XVAL*XVAL
      ELSEIF(TRANS .EQ. 'SQRT') THEN 
         TRANSF = SQRT(XVAL)
      ELSEIF(TRANS .EQ. 'LN') THEN 
         IF (DVNAME .EQ. 'Q') THEN
            TRANSF = DLOG(XVAL) - LQCENT
         ELSE
            TRANSF = DLOG(XVAL)
         ENDIF
      ELSEIF(TRANS .EQ. 'LNSQ') THEN 
         IF (DVNAME .EQ. 'Q') THEN
            TRANSF = (DLOG(XVAL) - LQCENT)**2
         ELSE
            TRANSF = DLOG(XVAL)**2
         ENDIF
      ELSEIF(TRANS .EQ. 'SIN2P') THEN 
         TRANSF = DSIN(2.D0*PI*XVAL)
      ELSEIF(TRANS .EQ. 'SIN4P') THEN 
         TRANSF = DSIN(4.D0*PI*XVAL)
      ELSEIF(TRANS .EQ. 'SIN6P') THEN 
         TRANSF = DSIN(6.D0*PI*XVAL)
      ELSEIF(TRANS .EQ. 'COS2P') THEN 
         TRANSF = DCOS(2.D0*PI*XVAL)
      ELSEIF(TRANS .EQ. 'COS4P') THEN 
         TRANSF = DCOS(4.D0*PI*XVAL)
      ELSEIF(TRANS .EQ. 'COS6P') THEN
         TRANSF = DCOS(6.D0*PI*XVAL)
      ENDIF

      RETURN
      END
