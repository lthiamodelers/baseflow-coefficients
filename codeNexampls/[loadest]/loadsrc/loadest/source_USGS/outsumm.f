************************************************************************
*
*     Subroutine OUTSUMM                             Called by: ESTLOAD
*
*     print summary statistics for estimated loads and concentrations
*
*     local vars
*     ----------
*     PCONCAML  estimated concentration, AMLE estimation
*     PCONCMLE  estimated concentration, MLE estimation
*     PCONCLAD  estimated concentration, LAD estimation
*
************************************************************************
      SUBROUTINE OUTSUMM(CCMAX,CFACTOR,CUNITSTR,EFLOW,LFACTOR,LUNITSTR,
     &                   NCENS,NOBSE,PLOADAML,PLOADLAD,PLOADMLE,LDOUT)
*
*     subroutine arguments
*
      INTEGER*4 NCENS,NOBSE,LDOUT
      DOUBLE PRECISION CCMAX,CFACTOR,LFACTOR
      DOUBLE PRECISION EFLOW(*),PLOADAML(*),PLOADLAD(*),PLOADMLE(*)
      CHARACTER*(*) CUNITSTR,LUNITSTR
*
*     local vars
*
      INTEGER*4 I
      DOUBLE PRECISION PCONCAML(NOBSE),PCONCMLE(NOBSE),PCONCLAD(NOBSE)
*
*     format statements
*
 1000 FORMAT(///,' Summary Statistics - Estimated Loads ',A9,/,1X,
     &       46('-'))
 2000 FORMAT(/,18X,'25th',14X,'75th',5X,'90th',5X,'95th',5X,'99th',/,9X,
     &      'Min.',6X,'Pct',5X,'Med.',4(6X,'Pct'),5X,'Max.',/,6X,
     &      70('-'))
 3000 FORMAT(///,' Summary Statistics - Estimated Concentrations [',
     &       A4,']',/,1X,52('-'))
*
*     back out estimated concentrations from estimated loads while
*     estimated loads are still in kg/d.
*     
      DO 10 I=1,NOBSE
         PCONCAML(I) = PLOADAML(I)/(EFLOW(I)*CFACTOR)
         PCONCMLE(I) = PLOADMLE(I)/(EFLOW(I)*CFACTOR)
         PCONCLAD(I) = PLOADLAD(I)/(EFLOW(I)*CFACTOR)
 10   CONTINUE
*
*     convert estimated loads from kg/day to requested units
* 
      DO 20 I=1,NOBSE
         PLOADAML(I) = PLOADAML(I)*LFACTOR
         PLOADMLE(I) = PLOADMLE(I)*LFACTOR
         PLOADLAD(I) = PLOADLAD(I)*LFACTOR
 20   CONTINUE
*
*     print summary statistics for estimated loads
*
      WRITE(LDOUT,1000) LUNITSTR
      WRITE(LDOUT,2000) 
      CALL OUTSUMM2('AMLE',NOBSE,PLOADAML,LDOUT)
      CALL OUTSUMM2('MLE ',NOBSE,PLOADMLE,LDOUT)
      IF (NCENS .EQ. 0) CALL OUTSUMM2('LAD ',NOBSE,PLOADLAD,LDOUT)
*
*     print summary statistics for estimated concentration
*
      WRITE(LDOUT,3000) CUNITSTR
      WRITE(LDOUT,2000) 
      CALL OUTSUMM3('AMLE',NOBSE,PCONCAML,CCMAX,CUNITSTR,LDOUT)
      CALL OUTSUMM3('MLE ',NOBSE,PCONCMLE,CCMAX,CUNITSTR,LDOUT)
      IF (NCENS .EQ. 0)
     &   CALL OUTSUMM3('LAD ',NOBSE,PCONCLAD,CCMAX,CUNITSTR,LDOUT)

      RETURN
      END



