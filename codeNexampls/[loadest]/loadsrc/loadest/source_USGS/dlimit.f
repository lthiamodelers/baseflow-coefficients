************************************************************************
*
*     Subroutine DLIMIT                               Called by: INPUTC
*
*     read through the calibration data set and set the detection limit
*     for each observation.  For the case of AMLE, a detection limit is
*     needed for each and every observation, even if a given observation
*     is uncensored.  Rather than require the user to enter the
*     detection limit for each observation/constituent, this subroutine
*     adopts the approach taken in Tim Cohn's ESTIMATOR 2002 code
*     (S/R THRESH_REPAIR).  Using that approach, censored observations
*     are preceded by a '<' sign -- the concentration that follows is
*     the detection limit for that specific observation.  The detection
*     limit for the censored observation is then used as the detection
*     limit for subsequent uncensored observations.  When another
*     censored observation is read, a new detection limit takes effect
*     for subsequent uncensored observations.
*    
*     For example, consider 20 observations - 10 from lab A with a
*     detection limit of 0.02 and 10 from lab B with a detection limit
*     of 0.01; results from both labs yield 1 censored observation. The
*     first ten lines in the calibration file would include the 10
*     observations from Lab A, with line one used to specify the
*     censored observation.  The concentration following the '<' sign
*     on line one would then be used as the detection limit for
*     observations 1-10.  Observations from Lab B would occupy lines
*     11-20 of the calibration file, with line 11 used to specify the
*     censored observation.  The concentration following the '<' sign
*     on line 11 would then be used as the detection limit for
*     observations 11-20.
*
*     There are several problems with this approach:
*
*     1) for large data sets, it may be difficult for the user to group
*     the observations into subsets as in the example above.  If the
*     observations are not grouped into subsets or the subsets do not
*     have the censored observations as the first records, incorrect
*     detection limits may be applied to the uncensored data.
*
*     2) for the case of multiple constituents, it may be impossible to
*     place the censored observation within each subset at the top of
*     the subset.  Consider constituents Y and Z, each analyzed at Lab
*     B, and each with one censored observation.  The censored
*     observations occur on different dates.  In this case its not
*     possible to have both observations as the first line in the subset
*     from Lab B. (Note: the user could work around this problem by
*     doing seperate runs for each constituent.)
*
*     3) When a subset of observations is completely uncensored, the
*     detection limit from another subset will be applied.  Consider 
*     the example above, but with the 10 observations from Lab B being
*     completely uncensored.  In this case, the detection limit from
*     Lab A will be used for all 20 observations.
*
*     4) When all of the observations are uncensored (no '<' signs for
*     a given constituent), there is no way for the user to specify
*     the detection limit(s).  In this case, a default value of 1.E-25
*     is used for all observations (consistent with ESTIMATOR).
*
*     Despite these problems, the approach is satisfactory for most
*     applications: "the estimates are not very sensitive to the precise
*     value of the censoring threshold for the above-threshold values."
*     (T. Cohn, written communication, 5 Nov 2002).
*     
*
*     local vars (input vars defined in main.f)
*     -----------------------------------------
*     CCONCSTR constituent concentration, CCONC, as a character string
*     IDLIMIT  index (observation #) at which new detection limit takes
*              effect
*     NUMDLIMS potential number of different detection limits
*              (= number of censored observations, NCENS, on input)
*
************************************************************************
      SUBROUTINE DLIMIT(CADDLI,CCONC,CDATE,CFLAGI,CFLOWI,CTIME,DLIM,
     &                  NADDL,NCONST,NOBSCI,NOTE)
*
*     logical devices
*
      INCLUDE 'lda.inc'
*
*     subroutine arguments
*
      LOGICAL CFLAGI(NOBSCI,*)
      INTEGER*4 NADDL,NCONST,NOBSCI,CDATE(*),CTIME(*)
      DOUBLE PRECISION CFLOWI(*)
      DOUBLE PRECISION CADDLI(NOBSCI,*),CCONC(NOBSCI,*),DLIM(NOBSCI,*)
      CHARACTER*1 NOTE(NOBSCI,*)
*
*     local vars
*
      INTEGER*4 I,J,K
      INTEGER*4 NUMDLIMS(NCONST)
      INTEGER*4 IDLIMIT(NOBSCI+1,NCONST)
      CHARACTER*30 STRTMP,CCONCSTR(NCONST)
      CHARACTER*500 BUFFER
*
*     initialize
*
      DO 10 J=1,NCONST
         NUMDLIMS(J) = 0
 10   CONTINUE
*
*     read through the calibration data set, placing the observed
*     concentrations in the character variable CCONCSTR.  For each
*     observation of constituent concentration, check to see if the
*     observation is censored (censored observations are immediately
*     preceded by a '<').  If the observation is censored, increment
*     the counter for the number of detection limits (NUMDLIMS) and
*     set an index (IDLIMIT) that indicates which record has the
*     censored observation.  For both censored and uncensored data,
*     convert the character representation of concentration (CCONCSTR)
*     to a double precision value (CCONC)(for censored observations,
*     the '<' is stripped away), set the censored flag, and set the
*     string used in echo.out (NOTE, where '*' and '^' denote censored
*     and missing observations).
*
      DO 30 I=1,NOBSCI
         CALL GETLINE(LDCAL,BUFFER)
         READ(BUFFER,*,END=100) CDATE(I),CTIME(I),CFLOWI(I),
     &                          (CADDLI(I,K),K=1,NADDL),
     &                          (CCONCSTR(J),J=1,NCONST)
         DO 20 J=1,NCONST
            IF (INDEX(CCONCSTR(J),'<') .EQ. 1) THEN
               NUMDLIMS(J) = NUMDLIMS(J) + 1
               IDLIMIT(NUMDLIMS(J),J) = I
               STRTMP = CCONCSTR(J)
               READ(STRTMP(2:30),*) CCONC(I,J)
               IF (CCONC(I,J).LE.0.D0) CALL ERROR5(1,CCONCSTR(J))
               CFLAGI(I,J) = .TRUE.
               NOTE(I,J) = '*'
            ELSE
               READ(CCONCSTR(J),*) CCONC(I,J)
               CFLAGI(I,J) = .FALSE.
               IF (CCONC(I,J).LE.0.D0) THEN 
                  NOTE(I,J) = '^'
               ELSE
                  NOTE(I,J) = ' '
               ENDIF
            ENDIF
 20      CONTINUE
 30   CONTINUE
*
*     now that the calibration data set has been read, detection limits
*     may be assigned to each observation.  If there are no censored
*     values for a given constituent, simply set the detection limit
*     using a default value of 1.D-25 (loop 40).
*
*     For constituents with censored values, use the concentrations
*     associated with the '<' as the detection limit.  The first
*     detection limit applies to observation 1 and all subsequent
*     uncensored observations until the next censored observation is
*     reached (loop 50).  The second detection limit applies from the
*     second censored observation and all subsequent uncensored
*     observations until the next censored observation is reached,
*     etcetera (loops 60 and 70).  As noted above, this assignment of
*     detection limits is far from perfect -- as a result, some of
*     the uncensored observations may fall below the initially
*     assigned detection limits.  When this happens, reset the
*     detection limit to 1.D-25 (loop 80).
*
      DO 90 J=1,NCONST
         IDLIMIT(NUMDLIMS(J)+1,J) = NOBSCI
         
         IF (NUMDLIMS(J).EQ.0) THEN
            DO 40 I=1,NOBSCI
               DLIM(I,J) = 1.D-25
 40         CONTINUE
         ELSE
            DO 50 I=1,IDLIMIT(2,J)
               DLIM(I,J) = CCONC(IDLIMIT(1,J),J)
 50         CONTINUE
            DO 70 K=2,NUMDLIMS(J)
               DO 60 I=IDLIMIT(K,J),IDLIMIT(K+1,J)
                  DLIM(I,J) = CCONC(IDLIMIT(K,J),J)
 60            CONTINUE
 70         CONTINUE
            DO 80 I=1,NOBSCI
               IF (CCONC(I,J).GT.0.D0.AND.CCONC(I,J).LT.DLIM(I,J)) THEN
                  DLIM(I,J) = 1.D-25
                  NOTE(I,J) = '#'
               ENDIF
 80         CONTINUE
         ENDIF
 90   CONTINUE

      RETURN
*
*     end of calibration file (or input line) reached; print error
*     message and terminate execution
*
 100  CALL ERROR1(14,I)

      END
