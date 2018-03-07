************************************************************************
*
*     Subroutine LDAINIT                                Called by: INIT
*
*     initialize Logical Device Assignments (LDAs)
*
************************************************************************
      SUBROUTINE LDAINIT
*
*     logical devices
*
      INCLUDE 'lda.inc'
*
*     local variables
*
      INTEGER*4 I,J
*
*     assign input unit numbers
*
      LDCTRL = 10 
      LDHEAD = 11
      LDCAL = 12
      LDEST = 13
*
*     assign ouput unit numbers
*
      LDECHO = 20 

      RETURN
      END




