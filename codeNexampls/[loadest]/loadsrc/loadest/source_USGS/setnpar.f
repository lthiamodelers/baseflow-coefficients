************************************************************************
*
*     Subroutine SETNPAR                              Called by: INIT2
*
*     set the number of parameters in the various regression models
*
************************************************************************
      SUBROUTINE SETNPAR(MODNO,NEXPL,NPAR)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 MODNO,NEXPL,NPAR(*)
*
*     local vars
*
      INTEGER*4 I
*
*     set the number of parameters for each predefined regression model
*   
      NPAR(1) = 2
      NPAR(2) = 3
      NPAR(3) = 3
      NPAR(4) = 4
      NPAR(5) = 4
      NPAR(6) = 5
      NPAR(7) = 5
      NPAR(8) = 6
      NPAR(9) = 7
      NPAR(10) = 4
      NPAR(11) = 6
*
*     set the number of parameters for the user-defined regression
*     model.  The user defined model is always the last model (see
*     also setting of IMODBEG,IMODEND in SETUPC)
*   
      IF (MODNO .EQ. 99) THEN
         NPAR(NMODELS) = NEXPL + 1
      ELSE
         NPAR(NMODELS) = 0
      ENDIF
*
*     check to make sure that the number of parameters for each model,
*     NPAR, doesn't exceed MAXPARMS.  Because NPAR is set explicitly
*     above (and NEXPL has its own error test), this error call is due
*     to a programming error and is not the fault of the user.
*
      DO 10 I=1,NMODELS
         IF (NPAR(I) .GT. MAXPARMS) CALL ERROR2(6,NPAR(I),MAXPARMS)
 10   CONTINUE

      RETURN
      END
