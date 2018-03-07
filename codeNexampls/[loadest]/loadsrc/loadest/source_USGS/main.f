************************************************************************
*
*     LOADEST: Load Estimation
*     ------------------------
*     LOADEST is a program to estimate constituent loads by the rating
*     curve method.  Daily or unit streamflow data are used with the
*     rating curve to obtain the estimated loads.
*
*     The LOADEST software, data, and documentation are made available
*     by the U.S. Geological Survey (USGS) to be used in the public
*     interest and the advancement of science. You may, without any
*     fee or cost, use, copy, modify, or distribute this software,
*     and any derivative works thereof, and its supporting
*     documentation, SUBJECT TO the USGS software User's Rights Notice
*     (http://water.usgs.gov/software/software_notice.html)
*
************************************************************************
*
*     Version: MOD48                                      March 2013
*
************************************************************************
*
*     Reference
*     ---------
*     Runkel, R.L., C.G. Crawford, and T.A. Cohn, 2004, Load Estimator
*       (LOADEST): A FORTRAN Program for Estimating Constituent Loads in
*       Streams and Rivers, U.S. Geological Survey Techniques and
*       Methods, Book 4, Chapter A5, 75 p.
*
*     Homepage:  http://water.usgs.gov/software/loadest/
*
*
*     Authors
*     -------
*     Rob Runkel
*     Charlie Crawford
*     Tim Cohn
*     Dave Lorenz
*     U.S. Geological Survey
*
*     Address
*     -------
*     Rob Runkel
*     U.S. Geological Survey
*     3215 Marine Street, Suite E-127
*     Boulder, CO 80303
*
*     email: runkel@usgs.gov
*     https://profile.usgs.gov/runkel
*
************************************************************************
*
*     Related References
*     ------------------
*     Abramowitz, M. and Stegun, I.A., 1964, Handbook of mathematical
*        functions: U.S. Department of Commerce, National Bureau of
*        Standards, Applied Mathematics Series 55.
*
*     Aitken, M., 1981, A note on the regression analysis of
*        censored data: Technometrics, v. 23, no. 2, p. 161-163.
*
*     Beyer, W.H., 1987, Standard mathematical tables, 28th Ed.:
*        Boca Raton, CRC Press.
*
*     Bradu, D., and Mundlak, Y., 1970, Estimation in lognormal
*        linear models: Journal of the American Statistical
*        Association, v. 65, no. 329, p. 198-211.
*
*     Buchinsky, M., 1994, Changes in the U.S. wage structure 1963-1987:
*        Applications of Quantile Regression: Econometrica, v. 62,
*        no. 2, p. 405-458.
*
*     Cohn, T.A., 1988, Adjusted maximum likelihood estimation of the
*         moments of lognormal populations from type I censored samples:
*         U.S. Geological Survey Open-File Report 88-350, 34 p.
*
*     Cohn, T.A., Delong, L.L., Gilroy, E.J., Hirsch, R.M., and Wells,
*        D.K., 1989, Estimating constituent loads: Water Resources
*       Research, v. 25, p. 937-942.
*
*     Crawford, C.G., 1996, Estimating mean constituent loads in
*        rivers by the rating-curve and flow-duration, rating-curve
*        methods: Bloomington, Indiana, Indiana University, Ph.d.
*        Dissertation, 245 P.
*
*     Duan, N., 1983, Smearing estimate -- a nonparametric
*        retransformation method: Journal of the American Statistical
*        Association, v. 78, p. 605-610.
*
*     Finney, D.J., 1941, On the distribution of a variate whose
*        logarithm is normally distributed: Supplement to the Journal
*        of the Royal Statistical Society, v. 7, p. 155-161.
*
*     Fox P.A., Hall A.D., Schryer N.L., 1978, Framework for a portable
*        library, ACM Transactions on Mathematical Software, v. 4, no.
*        2, p. 177-188.
*
*     Gilroy, E.J., Hirsch, R.M., and Cohn, T.A., 1990, Mean square
*        error of regression-based constituent transport estimates:
*        Water Resources Research, v. 26, p. 2069-2077.
*
*     Helsel, D.R., and Hirsch, R.M., 1992, Statistical Methods in
*        Water Resources: New York, Elsevier.
*
*     Judge, G.G., Hill, R.C., Griffiths, W.E., Lutkepohl, H., and Lee,
*        T.C., 1988, Introduction to the theory and practice of
*        econometrics (2d ed.): New York, John Wiley, 1024 p.
*
*     Likes, J., 1980, Variance of the MVUE for lognormal variance:
*        Tecnometrics, v. 22, no. 2, p. 253-258.
*
*     Nash, J.E., and Sutcliffe, J.V., 1970, River flow forecasting
*        through conceptual models part I -- A discussion of principles:
*        Journal of Hydrology, v. 10, no. 3, p. 282-290.
*
*     Nelder, J.A., and Mean, R., 1965, A simplex method for function
*        minimization: Computer Journal, v.7, p.308-313.
*
*     O'Neill, R., 1985, Function minimization using a simplex
*        procedure, in Griffiths, P., and Hill, I.D.,Eds., Applied
*        Statistics Algorithms: Chicestor, Ellis Horwood Limited, p.
*        79-87.
*
*     Paarsch, H.J., 1984, A Monte Carlo comparison of estimators for
*        censored regression models: Journal of Econometrics, v. 24,
*        p. 197-213.
*
*     Powell, J.L., 1984, Least absolute deviations estimation for the
*        censored regression model: J. of Econometrics, v. 25,
*        p. 303-325.
*
*     Royston, P., 1993, A toolkit for testing for non-normality in
*        complete and censored samples: J. of the Royal Statistical
*        Society. Series D (The Statistician), v. 42, No. 1, p. 37-43.
*  
*     Shenton, L.R., and Bowman, K.O., 1977, Maximum likelihood
*        estimation in small samples: London, Griffin.
*
*     Stenback, G.A., Crumpton, W.G., Schilling, K.E., and Helmers,
*        M.J., 2011, Rating curve estimation of nutrient loads in Iowa
*        rivers: Journal of Hydrology, v. 396, p. 158-169.
*
************************************************************************
*
*     DICTIONARY - INPUT VARIABLES
*
************************************************************************
*
*     Required Input Variables
*     ------------------------
*     TITLE      run title
*     PRTOPT     estimated values print option
*     SEOPT      standard error option
*     LDOPT      load option
*     MODNO      model number
*     NCONST     no. of constituents
*
*     Season Definition (Required for LDOPT = 1 or 3)
*     -----------------------------------------------
*     NSEAS      no. of user-defined seasons
*     SBEG       beginning of season, in MMDD format       
*     SEND       end of season, in MMDD format
*
*     Period Definition (Required for MODNO = 10 or 11)
*     ------------------------------------------------
*     PBMON      beginning month of period
*     PEMON      ending month of period
*     
*     User-defined Models (Required for MODNO = 99)
*     ---------------------------------------------
*     NADDL      no. of additional data variables
*     NEXPL      no. of explanatory variables
*     DVNAME     name of data variable used to develop explanatory var.
*     TRANS      transformation code used to create explanatory variable
*
*     Required Constituent Variables (One value per constituent)
*     ----------------------------------------------------------
*     CNAME      constituent name
*     UCFLAG     unit flag, concentration
*     ULFLAG     unit flag, load
*
*     Required Calibration Data (Calibration File)
*     --------------------------------------------
*     CDATE    date of observation (yyyymmdd format)
*     CTIME    time of observation
*     CFLOW    stream flow at CTIME on CDATE
*     CADDL    additional data variables at CTIME on CDATE
*     CCONC    constituent concentration at CTIME on CDATE
*
*     Required Estimation Data (Estimation File)
*     ------------------------------------------
*     NOBSPD   no. of observations of the data variables per day
*     EDATE    date of observation (yyyymmdd format)
*     ETIME    time of observation 
*     EFLOW    stream flow at ETIME on EDATE
*     EADDL    additional data variables at ETIME on EDATE
*
************************************************************************
*
*     DICTIONARY - PROGRAM VARIABLES
*
*     Acronyms used below (and in internal doc of subroutines):
*     --------------------------------------------------------
*     MLE      maximum likelihood estimation
*     AIC      Akaike Information Criteria
*     AMLE     adjusted maximum likelihood estimation
*     LAD      least absolute deviation method
*
************************************************************************
*
*     Program variables
*     -----------------
*     BCLAD    transformation bias correction factor for LAD 
*     BESTMOD  best regression model (selected by AIC or the user)
*     CCMAX    maximum calibration concentration
*     CDATETIM string to hold CDATE and CTIME of calibration data
*     CENSFLAG indicator variable identifying censored observations
*     CFACTOR  conversion factor from cfs*concentration to kg/day
*     DECTIME  decimal time in fractional years (1 July 1978 = 1978.5)
*     DECTIME2 decimal time in fractional yrs (1 July 1978 = 1978.5)
*     EQSTAT   estimation streamflow stats (mean,min,percentiles,max)
*     IMODBEG  beginning model number for looping through poss. models
*     IMODEND  ending model number for looping through poss. models
*     LFACTOR  load conversion factor for going from kg/d to other units
*     LQCENT   "center" of ln(Q)
*     NCENS    no. of censored observations
*     NOBSC    no. of nonmissing observations for calibration
*     NOBSCI   no. of observations in the calibration data set on input
*     NOBSE    no. of observations of the data variable(s) for est.
*     NPAR     no. of parameters in the regression model
*     PARAML   AMLE parameter estimates 
*     PARLAD   LAD parameter estimates
*     PARMLE   MLE parameter estimates 
*     PERIOD   indicates if an observation is between PBMON and PEMON
*     PERIOD2   indicates if an observation is between PBMON and PEMON
*     RVARMLE  residual variance for MLE calibration
*     XLCAL    explanatory variables for calibrating model
*     WFLAG    flag indicated status of warning message for calibration
*     YD       censoring threshold (load)
*     YDC      censoring threshold (concentration)
*     YLCAL    observed log load for calibrating model
*     YLCAL2   YLCAL (uncens.obs.) or exp. val. of YLCAL (cens.obs.)
*     YLCALC   observed log concentration for calibrating model
*
************************************************************************
*
*                      INCLUDE FILES
*
************************************************************************
*   
*     Maximum Dimensions and Number of Models 
*     (set via PARAMETER statements in fmodules.inc)
*     ----------------------------------------------------------------
*     MAXPARMS   max. # of model parameters including intercept
*     NMODELS    number of possible regression models
*
*     Logical Devices (w/ the exception of LDOUT, defined in lda.inc)
*     ---------------------------------------------------------------
*     LDCTRL     input control information (I/O filenames)
*     LDHEAD     input file for user-specified options and parameters
*     LDCAL      input file for calibration data
*     LDEST      input file for estimation data
*     LDECHO     output file for date and time, echo input parameters
*     LDOUT      output files for regression models
*
************************************************************************
      PROGRAM LOADEST
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     local vars
*
      INTEGER*4 J,STATUS
*
*     other vars
*
      LOGICAL WFLAG
      INTEGER*4 BESTMOD,IMODBEG,IMODEND,LDOPT,MODNO,NADDL,NCONST,NEXPL,
     &          NOBSCI,NOBSE,NSEAS,PBMON,PEMON,PRTOPT,SEOPT
      INTEGER*4 NPAR(NMODELS)
      DOUBLE PRECISION BCLAD,RVARMLE
      DOUBLE PRECISION EQSTAT(10),PARLAD(MAXPARMS)
      DOUBLE PRECISION PARAML(MAXPARMS+1,NMODELS),
     &                 PARMLE(MAXPARMS+1,NMODELS)
      CHARACTER*5 DVNAME(MAXPARMS-1),TRANS(MAXPARMS-1)
      CHARACTER*80 TITLE
*
*     vars w/ dynamic allocation
*
      LOGICAL,ALLOCATABLE :: CENSFLAG(:,:)
      CHARACTER*4,ALLOCATABLE,DIMENSION(:) :: CUNITSTR,SBEG,SEND
      CHARACTER*8,ALLOCATABLE :: EDATE(:)
      CHARACTER*9,ALLOCATABLE :: LUNITSTR(:)
      CHARACTER*13,ALLOCATABLE :: CDATETIM(:,:)
      INTEGER*4,ALLOCATABLE,DIMENSION(:) :: ETIME,LDOUT,NCENS,NOBSC
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:) ::
     &       CCMAX,CFACTOR,EFLOW,LFACTOR,LQCENT,PERIOD2,YLCAL2
      DOUBLE PRECISION,ALLOCATABLE,DIMENSION(:,:) ::
     &       CFLOW,DECTIME,DECTIME2,EADDL,PERIOD,XLCAL,YD,YDC,
     &       YLCAL,YLCALC
      DOUBLE PRECISION,ALLOCATABLE :: CADDL(:,:,:)
*
*     format statements
*
 1000 FORMAT(/,2X,'Beginning Calibration for Constituent #',I2)
 2000 FORMAT(2X,'Beginning Load Estimation for Constituent #',I2,/)
 3000 FORMAT(2X,'LOADEST Execution Complete',//)
*
*     initialize, open files, determine the length of the calibration
*     and estimation files, and read the first part of the header file
*
      CALL INIT(LDOPT,NOBSCI,NOBSE,NSEAS,PRTOPT,SEOPT,TITLE)
*
*     allocate dynamic arrays
*
      ALLOCATE (SBEG(NSEAS),SEND(NSEAS), STAT=STATUS)
      IF (STATUS .NE. 0) STOP 'Memory Allocation Error: SBEG, SEND'
      ALLOCATE (YLCAL2(NOBSCI),STAT=STATUS)
      IF (STATUS .NE. 0) STOP 'Memory Allocation Error: YLCAL2'
      ALLOCATE (XLCAL(NOBSCI,MAXPARMS), STAT=STATUS)
      IF (STATUS .NE. 0) STOP 'Memory Allocation Error: XLCAL'
      ALLOCATE(EDATE(NOBSE),ETIME(NOBSE),EFLOW(NOBSE),PERIOD2(NOBSE),
     &         STAT=STATUS)
      IF (STATUS .NE. 0)
     &     STOP 'Memory Allocation Error: EDATE,ETIME,EFLOW,PERIOD2'
*
*     read model and contituent information from the header file
*
      CALL INIT2(MODNO,NADDL,NCONST,NEXPL,NSEAS,PBMON,PEMON,NPAR,SBEG,
     &           SEND,DVNAME,TRANS)
*
*     allocate dynamic arrays
*
      ALLOCATE(CCMAX(NCONST),CFACTOR(NCONST),CUNITSTR(NCONST),
     &         LFACTOR(NCONST),LQCENT(NCONST),LUNITSTR(NCONST),
     &         NCENS(NCONST),NOBSC(NCONST),STAT=STATUS)
      IF (STATUS .NE. 0)
     &     STOP 'Memory Allocation Error: CCMAX,CFACTOR,,...'
      ALLOCATE(LDOUT(NCONST*3),STAT=STATUS)
      IF (STATUS .NE. 0) STOP 'Memory Allocation Error:  LDOUT'
      ALLOCATE(CENSFLAG(NOBSCI,NCONST),CFLOW(NOBSCI,NCONST),
     &         CDATETIM(NOBSCI,NCONST),DECTIME(NOBSCI,NCONST),
     &         PERIOD(NOBSCI,NCONST),YD(NOBSCI,NCONST),
     &         YDC(NOBSCI,NCONST),YLCAL(NOBSCI,NCONST),
     &         YLCALC(NOBSCI,NCONST),STAT=STATUS)
      IF (STATUS .NE. 0)
     &     STOP 'Memory Allocation Error: CENSFLAG,CFLOW,CDATETIM,...'
      ALLOCATE(CADDL(NADDL,NOBSCI,NCONST),STAT=STATUS)
      IF (STATUS .NE. 0) STOP 'Memory Allocation Error: CADDL'
      ALLOCATE(DECTIME2(NOBSE,NCONST),EADDL(NADDL,NOBSE),STAT=STATUS)
      IF (STATUS .NE. 0) STOP 'Memory Allocation Error: DECTIME2,EADDL'
*
*     finish reading the header file, then read the calibration and
*     estimation files.
*
      CALL INIT3(CENSFLAG,IMODBEG,IMODEND,MODNO,NADDL,NCONST,NOBSCI,
     &           NOBSE,PBMON,PEMON,PRTOPT,ETIME,LDOUT,NCENS,NOBSC,CCMAX,
     &           CFACTOR,EQSTAT,LFACTOR,LQCENT,EFLOW,PERIOD2,CFLOW,
     &           DECTIME,DECTIME2,EADDL,PERIOD,YD,YDC,YLCAL,YLCALC,
     &           CADDL,CUNITSTR,EDATE,LUNITSTR,CDATETIM,TITLE)
*
*     loop through calibration and estimation steps for each
*     constituent.  (skip computations for a given constituent if
*     there was insufficient data for calibration {NOBSC=0})
*
      DO 10 J=1,NCONST
         IF (NOBSC(J) .GT. 0) THEN
            WRITE(*,1000) J
            CALL CALIBR(BCLAD,BESTMOD,CADDL(1,1,J),CDATETIM(1,J),
     &                  CENSFLAG(1,J),CFACTOR(J),CFLOW(1,J),CUNITSTR(J),
     &                  DECTIME(1,J),DVNAME,IMODBEG,IMODEND,LDOUT(J),
     &                  LFACTOR(J),LQCENT(J),LUNITSTR(J),NADDL,NCENS(J),
     &                  NCONST,NEXPL,NOBSC(J),NOBSCI,NPAR,PARAML,PARLAD,
     &                  PARMLE,PERIOD(1,J),RVARMLE,TRANS,WFLAG,XLCAL,
     &                  YD(1,J),YDC(1,J),YLCAL(1,J),YLCAL2,YLCALC(1,J))
            WRITE(*,2000) J
            CALL ESTLOAD(BCLAD,BESTMOD,CCMAX(J),CENSFLAG(1,J),
     &                   CFACTOR(J),CFLOW(1,J),CUNITSTR(J),
     &                   DECTIME2(1,J),DVNAME,EADDL,EDATE,EFLOW,EQSTAT,
     &                   ETIME,LDOPT,LDOUT(J),LFACTOR(J),LQCENT(J),
     &                   LUNITSTR(J),NADDL,NCENS(J),NCONST,NEXPL,
     &                   NOBSC(J),NOBSCI,NOBSE,NPAR(BESTMOD),NSEAS,
     &                   PARAML(1,BESTMOD),PARLAD,PARMLE(1,BESTMOD),
     &                   PERIOD2,PRTOPT,RVARMLE,SBEG,SEND,SEOPT,TRANS,
     &                   WFLAG,XLCAL,YD(1,J),YLCAL(1,J),YLCAL2)
         ENDIF
 10   CONTINUE
*
*     close files
*
      CALL CLOSEF(NCONST,LDOUT)
*
*     deallocate memory & write final message
*
      DEALLOCATE(CADDL,CCMAX,CENSFLAG,CFACTOR,CFLOW,CUNITSTR,DECTIME,
     &           DECTIME2,EADDL,EDATE,EFLOW,ETIME,LDOUT,LFACTOR,LQCENT,
     &           LUNITSTR,NCENS,NOBSC,PERIOD,PERIOD2,SBEG,SEND,XLCAL,YD,
     &           YDC,YLCAL,YLCAL2,YLCALC)

      WRITE(*,3000)

      END
