      SUBROUTINE SETUP(NEQ,TSTART,YSTART,TEND,TOL,THRES,METHOD,TASK,
     &                 ERRASS,HSTART,WORK,LENWRK,MESAGE)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C  If you are not familiar with the code SETUP and how it is used in
C  conjunction with UT or CT to solve initial value problems, you should study
C  the document file rksuite.doc carefully before attempting to use the code.
C  The following "Brief Reminder" is intended only to remind you of the
C  meaning, type, and size requirements of the arguments.
C
C  The environmental parameters OUTCH, MCHEPS, and DWARF are used in the
C  following description.  To find out their values
C
C       CALL ENVIRN(OUTCH,MCHEPS,DWARF)
C
C  INPUT VARIABLES
C
C     NEQ       - INTEGER
C                 The number of differential equations in the system.
C                 Constraint: NEQ >= 1
C     TSTART    - DOUBLE PRECISION
C                 The initial value of the independent variable.
C     YSTART(*) - DOUBLE PRECISION array of length NEQ
C                 The vector of initial values of the solution components.
C     TEND      - DOUBLE PRECISION
C                 The integration proceeds from TSTART in the direction of
C                 TEND. You cannot go past TEND.
C                 Constraint: TEND must be clearly distinguishable from TSTART
C                 in the precision available.
C     TOL       - DOUBLE PRECISION
C                 The relative error tolerance.
C                 Constraint: 0.01D0 >= TOL >= 10*MCHEPS
C     THRES(*)  - DOUBLE PRECISION array of length NEQ
C                 THRES(L) is the threshold for the Ith solution component.
C                 Constraint: THRES(L) >= SQRT(DWARF)
C     METHOD    - INTEGER
C                 Specifies which Runge-Kutta pair is to be used.
C                  = 1 - use the (2,3) pair
C                  = 2 - use the (4,5) pair
C                  = 3 - use the (7,8) pair
C     TASK      - CHARACTER*(*)
C                 Only the first character of TASK is significant.
C                 TASK(1:1) = `U' or `u' - UT is to be used
C                           = `C' or `c' - CT is to be used
C                 Constraint: TASK(1:1) = `U'or `u' or`C' or `c'
C     ERRASS    - LOGICAL
C                 = .FALSE. - do not attempt to assess the true error.
C                 = .TRUE.  - assess the true error. Costs roughly twice
C                             as much as the integration with METHODs 2 and
C                             3, and three times with METHOD = 1.
C     HSTART    - DOUBLE PRECISION
C                 0.0D0     - select automatically the first step size.
C                 non-zero  - try HSTART for the first step.
C
C  WORKSPACE
C
C     WORK(*) - DOUBLE PRECISION array of length LENWRK
C               Do not alter the contents of this array after calling SETUP.
C
C  INPUT VARIABLES
C
C     LENWRK  - INTEGER
C               Length of WORK(*): How big LENWRK must be depends
C               on the task and how it is to be solved.
C
C               LENWRK = 32*NEQ is sufficient for all cases. 
C
C               If storage is a problem, the least storage possible 
C               in the various cases is:
C
C                 If TASK = `U' or `u', then
C                   if ERRASS = .FALSE. and
C                     METHOD = 1, LENWRK must be at least 10*NEQ
C                            = 2                          20*NEQ
C                            = 3                          16*NEQ
C                   if ERRASS = .TRUE. and
C                     METHOD = 1, LENWRK must be at least 15*NEQ
C                            = 2                          32*NEQ
C                            = 3                          21*NEQ
C
C                 If TASK = `C' or `c', then
C                   if ERRASS = .FALSE. and
C                     METHOD = 1, LENWRK must be at least 10*NEQ
C                            = 2                          14*NEQ
C                            = 3                          16*NEQ
C                   if ERRASS = .TRUE. and
C                     METHOD = 1, LENWRK must be at least 15*NEQ
C                            = 2                          26*NEQ
C                            = 3                          21*NEQ
C
C                 Warning:  To exploit the interpolation capability
C                 of METHODs 1 and 2, you have to call INTRP.  This
C                 subroutine requires working storage in addition to
C                 that specified here.
C
C     MESAGE    - LOGICAL
C                 Specifies whether you want informative messages written to
C                 the standard output channel OUTCH.
C                 = .TRUE.   - provide messages
C                 = .FALSE.  - do not provide messages
C
C  In the event of a "catastrophic" failure to call SETUP correctly, the
C  nature of the catastrophe is reported on the standard output channel,
C  regardless of the value of MESAGE.  Unless special provision was made
C  in advance (see rksuite.doc), the computation then comes to a STOP.
C
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C     .. Scalar Arguments ..
      DOUBLE PRECISION  HSTART, TEND, TOL, TSTART
      INTEGER           LENWRK, METHOD, NEQ
      LOGICAL           ERRASS, MESAGE
      CHARACTER*(*)     TASK
C     .. Array Arguments ..
      DOUBLE PRECISION  THRES(*), WORK(*), YSTART(*)
C     .. Common Block for Problem Definition ..
      DOUBLE PRECISION  TSTRT, TND, DIR, HSTRT, TOLR
      INTEGER           NEQN
      COMMON /RKCOM1/   TSTRT, TND, DIR, HSTRT, TOLR, NEQN
      SAVE   /RKCOM1/
C     .. Common Block to hold Problem Status ..
      DOUBLE PRECISION  T, H, TOLD, HOLD
      INTEGER           NFCN, SVNFCN, OKSTP, FLSTP
      LOGICAL           FIRST, LAST
      COMMON /RKCOM2/   T, H, TOLD, HOLD, NFCN, SVNFCN, OKSTP, FLSTP,
     &                  FIRST, LAST
      SAVE   /RKCOM2/
C     .. Common Block for General Workspace Pointers ..
      INTEGER           PRTHRS, PRERST, PRWT, PRYOLD, PRSCR, PRY, PRYP,
     &                  PRSTGS, PRINTP, LNINTP
      COMMON /RKCOM3/   PRTHRS, PRERST, PRWT, PRYOLD, PRSCR, PRY, PRYP,
     &                  PRSTGS, PRINTP, LNINTP
      SAVE   /RKCOM3/
C     .. Common Block to hold Formula Definitions ..
      DOUBLE PRECISION  A(13,13), B(13), C(13), BHAT(13), R(11,6),
     &                  E(7)
      INTEGER           PTR(13), NSTAGE, METHD, MINTP
      LOGICAL           INTP
      COMMON /RKCOM4/   A, B, C, BHAT, R, E, PTR, NSTAGE, METHD,
     &                  MINTP, INTP
      SAVE   /RKCOM4/
C     .. Common Block to hold Formula Characterisitcs ..
      DOUBLE PRECISION  TOOSML, COST, SAFETY, EXPON, STBRAD, TANANG,
     &                  RS, RS1, RS2, RS3, RS4
      INTEGER           ORDER, LSTSTG, MAXTRY, NSEC
      LOGICAL           FSAL
      COMMON /RKCOM5/   TOOSML, COST, SAFETY, EXPON, STBRAD, TANANG,
     &                  RS, RS1, RS2, RS3, RS4, ORDER, LSTSTG, MAXTRY,
     &                  NSEC, FSAL
      SAVE   /RKCOM5/
C     .. Common Block for Global Error Assessment ..
      DOUBLE PRECISION  MAXERR, LOCMAX
      INTEGER           GNFCN, PRZSTG, PRZY, PRZYP, PRZERS, PRZERR,
     &                  PRZYNU
      LOGICAL           ERASON, ERASFL
      COMMON /RKCOM6/   MAXERR, LOCMAX, GNFCN, PRZSTG, PRZY, PRZYP,
     &                  PRZERS, PRZERR, PRZYNU, ERASON, ERASFL
      SAVE   /RKCOM6/
C     .. Common Block for Environment Parameters ..
      DOUBLE PRECISION  MCHEPS, DWARF, RNDOFF, SQRRMC, CUBRMC, TINY
      INTEGER           OUTCH
      COMMON /RKCOM7/   MCHEPS, DWARF, RNDOFF, SQRRMC, CUBRMC, TINY,
     &                  OUTCH
      SAVE   /RKCOM7/
C     .. Common Block for Integrator Options ..
      LOGICAL           MSG, UTASK
      COMMON /RKCOM8/   MSG, UTASK
      SAVE   /RKCOM8/
C     .. Common Block for Error Message ..
      CHARACTER*80      REC(10)
      COMMON /RKCOM9/   REC
      SAVE   /RKCOM9/
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='SETUP')
      INTEGER           MINUS1
      LOGICAL           TELL
      PARAMETER         (MINUS1=-1,TELL=.FALSE.)
      DOUBLE PRECISION  ONE, ZERO, PT01
      PARAMETER         (ONE=1.0D+0,ZERO=0.0D+0,PT01=0.01D+0)
C     .. Local Scalars ..
      DOUBLE PRECISION  HMIN
      INTEGER           FLAG, FREEPR, IER, L, LINTPL, LREQ, NREC, VECSTG
      LOGICAL           LEGALT, REQSTG
      CHARACTER         TASK1
C     .. External Subroutines ..
      EXTERNAL          CONST, MCONST, RKMSG, RKSIT
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, MAX, SIGN
C     .. Executable Statements ..
C
C  Clear previous flag values of subprograms in the suite.
C
      IER = MINUS1
      CALL RKSIT(TELL,SRNAME,IER)
C
      IER = 1
      NREC = 0
C
C  Fetch output channel and machine constants; initialise common
C  block /RKCOM7/
C
      CALL MCONST(METHOD)
C
C  Check for valid input of trivial arguments
      TASK1 = TASK(1:1)
      LEGALT = TASK1 .EQ. 'U' .OR. TASK1 .EQ. 'u' .OR.
     &         TASK1 .EQ. 'C' .OR. TASK1 .EQ. 'c'
      IF (.NOT.LEGALT) THEN
         IER = 911
         NREC = 3
         WRITE (REC,'(A/A,A,A/A)')
     &' ** You have set the first character of ',
     &' ** TASK to be ''',TASK1,'''. It must be one of ',
     &' ** ''U'',''u'',''C'' or ''c''.'
      ELSE IF (NEQ.LT.1) THEN
         IER = 911
         NREC = 1
         WRITE (REC,'(A,I6,A)')
     &' ** You have set NEQ = ',NEQ,' which is less than 1.'
      ELSE IF (METHOD.LT.1 .OR. METHOD.GT.3) THEN
         IER = 911
         NREC = 1
         WRITE (REC,'(A,I6,A)')
     &' ** You have set METHOD = ',METHOD,' which is not 1, 2, or 3.'
      ELSE IF (TSTART.EQ.TEND) THEN
         IER = 911
         NREC = 1
         WRITE (REC,'(A,D13.5,A)')
     &' ** You have set TSTART = TEND = ',TSTART,'.'
      ELSE IF ((TOL.GT.PT01) .OR. (TOL.LT.RNDOFF)) THEN
         IER = 911
         NREC = 2
         WRITE (REC,'(A,D13.5,A/A,D13.5,A)')
     &' ** You have set TOL = ',TOL,' which is not permitted. The',
     &' ** range of permitted values is (',RNDOFF,',0.01D0).'
      ELSE
         L = 1
   20    CONTINUE
         IF (THRES(L).LT.TINY) THEN
            IER = 911
            NREC = 2
            WRITE (REC,'(A,I6,A,D13.5,A/A,D13.5,A)')
     &' ** You have set THRES(',L,') to be ',THRES(L),' which is ',
     &' ** less than the permitted minimum,',TINY,'.'
         END IF
         L = L + 1
         IF (IER.NE.911 .AND. L.LE.NEQ) GO TO 20
      END IF
C
C  Return if error detected
C
      IF (IER.NE.1) GO TO 80
C
C  Set formula definitions and characteristics by means of arguments
C  in the call list and COMMON blocks /RKCOM4/ and /RKCOM5/
C
      CALL CONST(METHOD,VECSTG,REQSTG,LINTPL)
C
C  Set options in /RKCOM8/
      UTASK = TASK1 .EQ. 'U' .OR. TASK1 .EQ. 'u'
      MSG = MESAGE
C
C  Initialise problem status in /RKCOM1/ and /RKCOM2/
      NEQN = NEQ
      TSTRT = TSTART
      TND = TEND
      T = TSTART
      TOLD = TSTART
      DIR = SIGN(ONE,TEND-TSTART)
C
C  In CT the first step taken will have magnitude H.  If HSTRT = ABS(HSTART)
C  is not equal to zero, H = HSTRT.  If HSTRT is equal to zero, the code is
C  to find an on-scale initial step size H.  To start this process, H is set
C  here to an upper bound on the first step size that reflects the scale of
C  the independent variable.  UT has some additional information, namely the
C  first output point, that is used to refine this bound in UT when UTASK
C  is .TRUE..  If HSTRT is not zero, but it is either too big or too small,
C  the input HSTART is ignored and HSTRT is set to zero to activate the
C  automatic determination of an on-scale initial step size.
C
      HSTRT = ABS(HSTART)
      HMIN = MAX(TINY,TOOSML*MAX(ABS(TSTART),ABS(TEND)))
      IF (HSTRT.GT.ABS(TEND-TSTART) .OR. HSTRT.LT.HMIN) HSTRT = ZERO
      IF (HSTRT.EQ.ZERO) THEN
         H = MAX(ABS(TEND-TSTART)/RS3,HMIN)
      ELSE
         H = HSTRT
      END IF
      HOLD = ZERO
      TOLR = TOL
      NFCN = 0
      SVNFCN = 0
      OKSTP = 0
      FLSTP = 0
      FIRST = .TRUE.
      LAST = .FALSE.
C
C  WORK(*) is partioned into a number of arrays using pointers. These
C  pointers are set in /RKCOM3/.
      PRTHRS = 1
C                           the threshold values
      PRERST = PRTHRS + NEQ
C                           the error estimates
      PRWT = PRERST + NEQ
C                           the weights used in the local error test
      PRYOLD = PRWT + NEQ
C                           the previous value of the solution
      PRSCR = PRYOLD + NEQ
C                           scratch array used for the higher order
C                           approximate solution and for the previous 
C                           value of the derivative of the solution
      PRY = PRSCR + NEQ
C                           the dependent variables
      PRYP = PRY + NEQ
C                           the derivatives
      PRSTGS = PRYP + NEQ
C                           intermediate stages held in an internal
C                           array STAGES(NEQ,VECSTG)
C
      FREEPR = PRSTGS + VECSTG*NEQ
C
C  Allocate storage for interpolation if the TASK = `U' or `u' was
C  specified. INTP and LINTPL returned by CONST indicate whether there
C  is an interpolation scheme associated with the pair and how much
C  storage is required.
C
      PRINTP = 1
      LNINTP = 1
      IF (UTASK) THEN
         IF (INTP) THEN
            LNINTP = LINTPL*NEQ
            IF (REQSTG) THEN
               PRINTP = FREEPR
               FREEPR = PRINTP + LNINTP
            ELSE
               PRINTP = PRSTGS
               FREEPR = MAX(PRINTP+VECSTG*NEQ,PRINTP+LNINTP)
            END IF
         END IF
      END IF
C
C  Initialise state and allocate storage for global error assessment
C  using /RKCOM6/
      GNFCN = 0
      MAXERR = ZERO
      LOCMAX = TSTART
      ERASON = ERRASS
      ERASFL = .FALSE.
      IF (ERRASS) THEN
C
C  Storage is required for the stages of a secondary integration. The
C  stages of the primary intergration can only be overwritten in the
C  cases where there is no interpolant or the interpolant does not
C  require information about the stages (e.g. METHOD 3 and METHOD 1,
C  respectively).
         IF (.NOT.REQSTG) THEN
            PRZSTG = PRSTGS
         ELSE
            PRZSTG = FREEPR
            FREEPR = PRZSTG + VECSTG*NEQ
         END IF
         PRZY = FREEPR
         PRZYP = PRZY + NEQ
         PRZERS = PRZYP + NEQ
         PRZERR = PRZERS + NEQ
         PRZYNU = PRZERR + NEQ
         FREEPR = PRZYNU + NEQ
      ELSE
         PRZSTG = 1
         PRZY = 1
         PRZYP = 1
         PRZERS = 1
         PRZERR = 1
         PRZYNU = 1
      END IF
C
      LREQ = FREEPR - 1
C
C  Check for enough workspace and suitable range of integration
C
      IF (LENWRK.LT.LREQ) THEN
         IER = 911
         NREC = 2
         WRITE (REC,'(A/A,I6,A,I6,A)')
     &' ** You have not supplied enough workspace. You gave LENWRK ',
     &' ** as', LENWRK, ', but it must be at least ',LREQ,'.'
      ELSE
         HMIN = MAX(TINY,TOOSML*MAX(ABS(TSTART),ABS(TEND)))
         IF (ABS(TEND-TSTART).LT.HMIN) THEN
            IER = 911
            NREC = 4
            WRITE (REC,'(A/A/A,D13.5/A,D13.5,A)')
     &' ** You have set values for TEND and TSTART that are not ',
     &' ** clearly distinguishable for the method and the precision ',
     &' ** of the computer being used. ABS(TEND-TSTART) is ',
     &ABS(TEND-TSTART),
     &' ** but should be at least ',HMIN,'.'
         END IF
      END IF
C
C  Return if error detected
C
      IF (IER.NE.1) GO TO 80
C
C  Initialize elements of the workspace
      DO 40 L = 1, NEQ
         WORK(PRTHRS-1+L) = THRES(L)
         WORK(PRY-1+L) = YSTART(L)
   40 CONTINUE
C
C  Initialize the global error to zero when ERRASS = .TRUE.
      IF (ERRASS) THEN
         DO 60 L = 1, NEQ
            WORK(PRZERR-1+L) = ZERO
   60    CONTINUE
      END IF
C
   80 CONTINUE
C
      CALL RKMSG(IER,SRNAME,NREC,FLAG)
C
      RETURN
      END
      SUBROUTINE UT(F,TWANT,TGOT,YGOT,YPGOT,YMAX,WORK,UFLAG)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C  If you are not familiar with the code UT and how it is used in
C  conjunction with SETUP to solve initial value problems, you should study
C  the document file rksuite.doc carefully before proceeding further.  The
C  following "Brief Reminder" is intended only to remind you of the meaning,
C  type, and size requirements of the arguments.
C
C  NAME DECLARED IN AN EXTERNAL STATEMENT IN THE CALLING PROGRAM:
C
C     F         - name of the subroutine for evaluating the differential
C                 equations.
C
C  The subroutine F must have the form
C
C  SUBROUTINE F(T,Y,YP)
C  DOUBLE PRECISION T,Y(*),YP(*)
C     Given input values of the independent variable T and the solution
C     components Y(*), for each L = 1,2,...,NEQ evaluate the differential
C     equation for the derivative of the Ith solution component and place the
C     value in YP(L).  Do not alter the input values of T and Y(*).
C  RETURN
C  END
C
C  INPUT VARIABLE
C
C     TWANT     - DOUBLE PRECISION
C                 The next value of the independent variable where a
C                 solution is desired.
C
C                 Constraints: TWANT must lie between the previous value
C                 of TGOT (TSTART on the first call) and TEND. TWANT can be
C                 equal to TEND, but it must be clearly distinguishable from
C                 the previous value of TGOT (TSTART on the first call) in 
C                 the precision available.
C
C  OUTPUT VARIABLES
C
C     TGOT      - DOUBLE PRECISION
C                 A solution has been computed at this value of the
C                 independent variable.
C     YGOT(*)   - DOUBLE PRECISION array of length NEQ
C                 Approximation to the true solution at TGOT. Do not alter
C                 the contents of this array
C     YPGOT(*)  - DOUBLE PRECISION array of length NEQ
C                 Approximation to the first derivative of the true
C                 solution at TGOT.
C     YMAX(*)   - DOUBLE PRECISION array of length NEQ
C                 YMAX(L) is the largest magnitude of YGOT(L) computed at any
C                 time in the integration from TSTART to TGOT. Do not alter
C                 the contents of this array.
C
C  WORKSPACE
C
C     WORK(*)   - DOUBLE PRECISION array as used in SETUP
C                 Do not alter the contents of this array.
C
C  OUTPUT VARIABLE
C
C     UFLAG     - INTEGER
C
C                       SUCCESS.  TGOT = TWANT.
C                 = 1 - Complete success.
C
C                       "SOFT" FAILURES
C                 = 2 - Warning:  You are using METHOD = 3 inefficiently
C                       by computing answers at many values of TWANT.  If
C                       you really need answers at so many specific points,
C                       it would be more efficient to compute them with
C                       METHOD = 2.  To do this you would need to restart
C                       from TGOT, YGOT(*) by a call to SETUP.  If you wish
C                       to continue as you are, you may.
C                 = 3 - Warning:  A considerable amount of work has been
C                       expended.  If you wish to continue on to TWANT, just
C                       call UT again.
C                 = 4 - Warning:  It appears that this problem is "stiff".
C                       You really should change to another code that is
C                       intended for such problems, but if you insist, you can
C                       continue with UT by calling it again.
C
C                       "HARD" FAILURES
C                 = 5 - You are asking for too much accuracy. You cannot
C                       continue integrating this problem.
C                 = 6 - The global error assessment may not be reliable beyond
C                       the current point in the integration.  You cannot
C                       continue integrating this problem.
C
C                       "CATASTROPHIC" FAILURES
C                 = 911 - The nature of the catastrophe is reported on
C                         the standard output channel. Unless special
C                         provision was made in advance (see rksuite.doc),
C                         the computation then comes to a STOP.
C
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C     .. Scalar Arguments ..
      DOUBLE PRECISION  TGOT, TWANT
      INTEGER           UFLAG
C     .. Array Arguments ..
      DOUBLE PRECISION  WORK(*), YGOT(*), YMAX(*), YPGOT(*)
C     .. Subroutine Arguments ..
      EXTERNAL          F
C     .. Common Block for Problem Definition ..
      DOUBLE PRECISION  TSTRT, TND, DIR, HSTRT, TOLR
      INTEGER           NEQN
      COMMON /RKCOM1/   TSTRT, TND, DIR, HSTRT, TOLR, NEQN
      SAVE   /RKCOM1/
C     .. Common Block to hold Problem Status ..
      DOUBLE PRECISION  T, H, TOLD, HOLD
      INTEGER           NFCN, SVNFCN, OKSTP, FLSTP
      LOGICAL           FIRST, LAST
      COMMON /RKCOM2/   T, H, TOLD, HOLD, NFCN, SVNFCN, OKSTP, FLSTP,
     &                  FIRST, LAST
      SAVE   /RKCOM2/
C     .. Common Block for General Workspace Pointers ..
      INTEGER           PRTHRS, PRERST, PRWT, PRYOLD, PRSCR, PRY, PRYP,
     &                  PRSTGS, PRINTP, LNINTP
      COMMON /RKCOM3/   PRTHRS, PRERST, PRWT, PRYOLD, PRSCR, PRY, PRYP,
     &                  PRSTGS, PRINTP, LNINTP
      SAVE   /RKCOM3/
C     .. Common Block to hold Formula Definitions ..
      DOUBLE PRECISION  A(13,13), B(13), C(13), BHAT(13), R(11,6),
     &                  E(7)
      INTEGER           PTR(13), NSTAGE, METHD, MINTP
      LOGICAL           INTP
      COMMON /RKCOM4/   A, B, C, BHAT, R, E, PTR, NSTAGE, METHD,
     &                  MINTP, INTP
      SAVE   /RKCOM4/
C     .. Common Block to hold Formula Characterisitcs ..
      DOUBLE PRECISION  TOOSML, COST, SAFETY, EXPON, STBRAD, TANANG,
     &                  RS, RS1, RS2, RS3, RS4
      INTEGER           ORDER, LSTSTG, MAXTRY, NSEC
      LOGICAL           FSAL
      COMMON /RKCOM5/   TOOSML, COST, SAFETY, EXPON, STBRAD, TANANG,
     &                  RS, RS1, RS2, RS3, RS4, ORDER, LSTSTG, MAXTRY,
     &                  NSEC, FSAL
      SAVE   /RKCOM5/
C     .. Common Block for Environment Parameters ..
      DOUBLE PRECISION  MCHEPS, DWARF, RNDOFF, SQRRMC, CUBRMC, TINY
      INTEGER           OUTCH
      COMMON /RKCOM7/   MCHEPS, DWARF, RNDOFF, SQRRMC, CUBRMC, TINY,
     &                  OUTCH
      SAVE   /RKCOM7/
C     .. Common Block for Integrator Options ..
      LOGICAL           MSG, UTASK
      COMMON /RKCOM8/   MSG, UTASK
      SAVE   /RKCOM8/
C     .. Common Block for Error Message ..
      CHARACTER*80      REC(10)
      COMMON /RKCOM9/   REC
      SAVE   /RKCOM9/
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='UT')
      LOGICAL           ASK, TELL
      PARAMETER         (ASK=.TRUE.,TELL=.FALSE.)
      INTEGER           MINUS1, MINUS2
      PARAMETER         (MINUS1=-1,MINUS2=-2)
      DOUBLE PRECISION  ZERO
      PARAMETER         (ZERO=0.0D+0)
C     .. Local Scalars ..
      DOUBLE PRECISION  HMIN, TLAST, TNOW, UTEND
      INTEGER           CFLAG, IER, L, NREC, STATE
      LOGICAL           BADERR, GOBACK
C     .. External Subroutines ..
      EXTERNAL          CHKFL, CT, INTRP, RESET, RKMSG, RKSIT
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, MAX, MIN
C     .. Save statement ..
      SAVE              UTEND, TLAST
C     .. Executable Statements ..
      IER = 1
      NREC = 0
      GOBACK = .FALSE.
      BADERR = .FALSE.
C
C  Is it permissible to call UT?
C
      CALL RKSIT(ASK,'SETUP',STATE)
      IF (STATE.EQ.911) THEN
         IER = 912
         NREC = 1
         WRITE (REC,'(A)')
     &' ** A catastrophic error has already been detected elsewhere.'
         GO TO 100
      END IF
      IF (STATE.EQ.MINUS1) THEN
         IER = 911
         NREC = 1
         WRITE (REC,'(A)')
     &' ** You have not called SETUP, so you cannot use UT.'
         GO TO 100
      END IF
      IF (.NOT.UTASK) THEN
         IER = 911
         NREC = 2
         WRITE (REC,'(A/A)')
     &' ** You have called UT after you specified in SETUP that ',
     &' ** you were going to use CT. This is not permitted.'
         GO TO 100
      END IF
      CALL RKSIT(ASK,SRNAME,STATE)
      IF (STATE.EQ.5 .OR. STATE.EQ.6) THEN
         IER = 911
         NREC = 1
         WRITE (REC,'(A/A)')
     &' ** This routine has already returned with a hard failure.',
     &' ** You must call SETUP to start another problem.'
         GO TO 100
      END IF
      STATE = MINUS2
      CALL RKSIT(TELL,SRNAME,STATE)
C
      IF (FIRST) THEN
C
C  First call.
C
C  A value of TND is specified in SETUP. When INTP = .FALSE., as with
C  METHD = 3, output is obtained at the specified TWANT by resetting TND
C  to TWANT.  At this point, before the integration gets started, this can
C  be done with a simple assignment.  Later it is done with a call to RESET.
C  The original TND is SAVEd as a local variable UTEND.
C
         UTEND = TND
         IF (.NOT.INTP) TND = TWANT
C
C  The last TGOT returned is SAVEd in the variable TLAST.  T (a variable
C  passed through the common block RKCOM2) records how far the integration
C  has advanced towards the specified TND.  When output is obtained by
C  interpolation, the integration goes past the TGOT returned (T is closer
C  to the specified TND than TGOT).  Initialize these variables and YMAX(*).
         TLAST = TSTRT
         TGOT = TSTRT
         DO 20 L = 1, NEQN
            YMAX(L) = ABS(WORK(PRY-1+L))
   20    CONTINUE
C
C  If the code is to find an on-scale initial step size H, a bound was placed
C  on H in SETUP.  Here the first output point is used to refine this bound.
         IF (HSTRT.EQ.ZERO) THEN
            H = MIN(ABS(H),ABS(TWANT-TSTRT))
            HMIN = MAX(TINY,TOOSML*MAX(ABS(TSTRT),ABS(TND)))
            H = MAX(H,HMIN)
         END IF
C
      ELSE
C
C  Subsequent call.
C
         IF (TLAST.EQ.UTEND) THEN
            IER = 911
            NREC = 3
            WRITE (REC,'(A/A/A)')
     &' ** You have called UT after reaching TEND. (Your last    ',
     &' ** call to UT resulted in TGOT = TEND.)  To start a new ',
     &' ** problem, you will need to call SETUP.'
            GO TO 100
         END IF
C
      END IF
C
C  Check for valid TWANT.
C
      IF (DIR*(TWANT-TLAST).LE.ZERO) THEN
         IER = 911
         NREC = 4
         WRITE (REC,'(A/A/A/A)')
     &' ** You have made a call to UT with a TWANT that does   ',
     &' ** not lie between the previous value of TGOT (TSTART  ',
     &' ** on the first call) and TEND. This is not permitted. ',
     &' ** Check your program carefully.'
         GO TO 100
      END IF
      IF (DIR*(TWANT-UTEND).GT.ZERO) THEN
         HMIN = MAX(TINY,TOOSML*MAX(ABS(TWANT),ABS(UTEND)))
         IF (ABS(TWANT-UTEND).LT.HMIN) THEN
            IER = 911
            NREC = 5
            WRITE (REC,'(A/A/A/A)')
     &' ** You have made a call to UT with a TWANT that does      ',
     &' ** not lie between the previous value of TGOT (TSTART on  ',
     &' ** the first call) and TEND. This is not permitted. TWANT ',
     &' ** is very close to TEND, so you may have meant to set    ',
     &' ** it to be TEND exactly.  Check your program carefully.  '
         ELSE
            IER = 911
            NREC = 4
            WRITE (REC,'(A/A/A/A)')
     &' ** You have made a call to UT with a TWANT that does   ',
     &' ** not lie between the previous value of TGOT (TSTART  ',
     &' ** on the first call) and TEND. This is not permitted. ',
     &' ** Check your program carefully.'
         END IF
         GO TO 100
      END IF
      IF (.NOT.INTP) THEN
         HMIN = MAX(TINY,TOOSML*MAX(ABS(TLAST),ABS(TWANT)))
         IF (ABS(TWANT-TLAST).LT.HMIN) THEN
            IER = 911
            NREC = 4
            WRITE (REC,'(A/A/A/A,D13.5,A)')
     &' ** You have made a call to UT with a TWANT that is not ',
     &' ** sufficiently different from the last value of TGOT  ',
     &' ** (TSTART on the first call).  When using METHOD = 3, ',
     &' ** it must differ by at least ',HMIN,'.'
            GO TO 100
         END IF
C
C  We have a valid TWANT. There is no interpolation with this METHD and
C  therefore we step to TWANT exactly by resetting TND with a call to RESET.
C  On the first step this matter is handled differently as explained above.
C
         IF (.NOT.FIRST) THEN
            CALL RESET(TWANT)
            CALL CHKFL(ASK,BADERR)
            IF (BADERR) GO TO 100
         END IF
      END IF
C
C  Process output, decide whether to take another step.
C
   40 CONTINUE
C
      IF (INTP) THEN
C
C  Interpolation is possible with this METHD.  The integration has
C  already reached T. If this is past TWANT, GOBACK is set .TRUE. and
C  the answers are obtained by interpolation.
C
         GOBACK = DIR*(T-TWANT) .GE. ZERO
         IF (GOBACK) THEN
            CALL INTRP(TWANT,'Both solution and derivative',NEQN,YGOT,
     &                 YPGOT,F,WORK,WORK(PRINTP),LNINTP)
            CALL CHKFL(ASK,BADERR)
            IF (BADERR) GO TO 100
            TGOT = TWANT
         END IF
      ELSE
C
C  Interpolation is not possible with this METHD, so output is obtained
C  by integrating to TWANT = TND.  Both YGOT(*) and YPGOT(*) are then 
C  already loaded with the solution at TWANT by CT.
C
         GOBACK = T .EQ. TWANT
         IF (GOBACK) TGOT = TWANT
      END IF
C
C  Updating of YMAX(*) is done here to account for the fact that when
C  interpolation is done, the integration goes past TGOT.  Note that YGOT(*)
C  is not defined until CT is called.  YMAX(*) was initialized at TSTRT
C  from values stored in WORK(*), so only needs to be updated for T
C  different from TSTRT.
      IF (T.NE.TSTRT) THEN
         DO 60 L = 1, NEQN
            YMAX(L) = MAX(YMAX(L),ABS(YGOT(L)))
   60    CONTINUE
      END IF
C
C  If done, go to the exit point.
      IF (GOBACK) GO TO 100
C
C  Take a step with CT in the direction of TND.  On exit, the solution is
C  advanced to TNOW.  The way CT is written, the approximate solution at
C  TNOW is available in both YGOT(*) and in WORK(*).  If output is obtained by
C  stepping to the end (TNOW = TWANT = TND), YGOT(*) can be returned directly.
C  If output is obtained by interpolation, the subroutine INTRP that does this
C  uses the values in WORK(*) for its computations and places the approximate
C  solution at TWANT in the array YGOT(*) for return to the calling program.
C  The approximate derivative is handled in the same way. TNOW is output from
C  CT and is actually a copy of T declared above in a common block.
C
      CALL CT(F,TNOW,YGOT,YPGOT,WORK,CFLAG)
      IER = CFLAG
C
C  A successful step by CT is indicated by CFLAG = 1 or = 2.
      IF (CFLAG.EQ.1) THEN
         GO TO 40
      ELSE IF (CFLAG.EQ.2) THEN
C
C  Supplement the warning message written in CT.
         NREC = 3
         WRITE (REC,'(A/A/A)')
     &' ** The last message was produced on a call to CT from UT.  ',
     &' ** In UT the appropriate action is to change to METHOD = 2,',
     &' ** or, if insufficient memory is available, to METHOD = 1. '
      ELSE IF (CFLAG.LE.6) THEN
         NREC = 1
         WRITE (REC,'(A)')
     &' ** The last message was produced on a call to CT from UT.'
      ELSE
         BADERR = .TRUE.
      END IF
      TGOT = T
C
C  Update YMAX(*) before the return.
      DO 80 L = 1, NEQN
         YMAX(L) = MAX(YMAX(L),ABS(YGOT(L)))
   80 CONTINUE
C
C  Exit point for UT.
C
  100 CONTINUE
C
      IF (BADERR) THEN
         IER = 911
         NREC = 4
         WRITE (REC,'(A/A/A/A)')
     &' ** An internal call by UT to a subroutine resulted in an  ',
     &' ** error that should not happen.  Check your program      ',
     &' ** carefully for array sizes, correct number of arguments,',
     &' ** type mismatches ... .'
      END IF
C
      TLAST = TGOT
C
C  All exits are done here after a call to RKMSG to report
C  what happened and set UFLAG.
C
      CALL RKMSG(IER,SRNAME,NREC,UFLAG)
C
      RETURN
      END
      SUBROUTINE STAT(TOTFCN,STPCST,WASTE,STPSOK,HNEXT)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C  If you are not familiar with the code STAT and how it is used in
C  conjunction with the integrators CT and UT, you should study the 
C  document file rksuite.doc carefully before attempting to use the code. 
C  The following "Brief Reminder" is intended only to remind you of the 
C  meaning, type, and size requirements of the arguments.
C
C  STAT is called to obtain some details about the integration.
C
C  OUTPUT VARIABLES
C
C     TOTFCN    - INTEGER
C                 Total number of calls to F in the integration so far --
C                 a measure of the cost of the integration.
C     STPCST    - INTEGER
C                 Cost of a typical step with this METHOD measured in
C                 calls to F.
C     WASTE     - DOUBLE PRECISION
C                 The number of attempted steps that failed to meet the 
C                 local error requirement divided by the total number of 
C                 steps attempted so far in the integration.
C     STPSOK    - INTEGER
C                 The number of accepted steps.
C     HNEXT     - DOUBLE PRECISION
C                 The step size the integrator plans to use for the next step.
C
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C     .. Scalar Arguments ..
      DOUBLE PRECISION  HNEXT, WASTE
      INTEGER           STPCST, STPSOK, TOTFCN
C     .. Common Block to hold Problem Status ..
      DOUBLE PRECISION  T, H, TOLD, HOLD
      INTEGER           NFCN, SVNFCN, OKSTP, FLSTP
      LOGICAL           FIRST, LAST
      COMMON /RKCOM2/   T, H, TOLD, HOLD, NFCN, SVNFCN, OKSTP, FLSTP,
     &                  FIRST, LAST
      SAVE   /RKCOM2/
C     .. Common Block to hold Formula Characterisitcs ..
      DOUBLE PRECISION  TOOSML, COST, SAFETY, EXPON, STBRAD, TANANG,
     &                  RS, RS1, RS2, RS3, RS4
      INTEGER           ORDER, LSTSTG, MAXTRY, NSEC
      LOGICAL           FSAL
      COMMON /RKCOM5/   TOOSML, COST, SAFETY, EXPON, STBRAD, TANANG,
     &                  RS, RS1, RS2, RS3, RS4, ORDER, LSTSTG, MAXTRY,
     &                  NSEC, FSAL
      SAVE   /RKCOM5/
C     .. Common Block for Global Error Assessment ..
      DOUBLE PRECISION  MAXERR, LOCMAX
      INTEGER           GNFCN, PRZSTG, PRZY, PRZYP, PRZERS, PRZERR,
     &                  PRZYNU
      LOGICAL           ERASON, ERASFL
      COMMON /RKCOM6/   MAXERR, LOCMAX, GNFCN, PRZSTG, PRZY, PRZYP,
     &                  PRZERS, PRZERR, PRZYNU, ERASON, ERASFL
      SAVE   /RKCOM6/
C     .. Common Block for Integrator Options ..
      LOGICAL           MSG, UTASK
      COMMON /RKCOM8/   MSG, UTASK
      SAVE   /RKCOM8/
C     .. Common Block for Error Message ..
      CHARACTER*80      REC(10)
      COMMON /RKCOM9/   REC
      SAVE   /RKCOM9/
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='STAT')
      LOGICAL           ASK
      INTEGER           MINUS1, MINUS2
      PARAMETER         (ASK=.TRUE.,MINUS1=-1,MINUS2=-2)
      DOUBLE PRECISION  ZERO
      PARAMETER         (ZERO=0.0D+0)
C     .. Local Scalars ..
      INTEGER           FLAG, IER, NREC, STATE
C     .. External Subroutines ..
      EXTERNAL          RKMSG, RKSIT
C     .. Intrinsic Functions ..
      INTRINSIC         DBLE
C     .. Executable Statements ..
C
      IER = 1
      NREC = 0
C
C  Is it permissible to call STAT?
C
      CALL RKSIT(ASK,SRNAME,STATE)
      IF (STATE.EQ.911) THEN
         IER = 912
         NREC = 1
         WRITE (REC,'(A)')
     &' ** A catastrophic error has already been detected elsewhere.'
         GO TO 20
      END IF
      IF (STATE.EQ.MINUS2) THEN
         IER = 911
         NREC = 3
         WRITE (REC,'(A/A/A)')
     &' ** You have already made a call to STAT after a hard   ',
     &' ** failure was reported from the integrator. You cannot',
     &' ** call STAT again.'
         GO TO 20
      END IF
      CALL RKSIT(ASK,'CT',STATE)
      IF (STATE.EQ.MINUS1) THEN
         IER = 911
         NREC = 1
         IF (UTASK) THEN
            WRITE (REC,'(A)')
     &' ** You have not called UT, so you cannot use STAT.'
         ELSE
            WRITE (REC,'(A)')
     &' ** You have not called CT, so you cannot use STAT.'
         END IF
         GO TO 20
      END IF
C
C  Set flag so that the routine can only be called once after a hard 
C  failure from the integrator.
      IF (STATE.EQ.5 .OR. STATE.EQ.6) IER = MINUS2
C
      TOTFCN = SVNFCN + NFCN
      IF (ERASON) TOTFCN = TOTFCN + GNFCN
      STPCST = COST
      STPSOK = OKSTP
      IF (OKSTP.LE.1) THEN
         WASTE = ZERO
      ELSE
         WASTE = DBLE(FLSTP)/DBLE(FLSTP+OKSTP)
      END IF
      HNEXT = H
C
   20 CONTINUE
C
      CALL RKMSG(IER,SRNAME,NREC,FLAG)
C
      RETURN
      END
      SUBROUTINE GLBERR(RMSERR,ERRMAX,TERRMX,WORK)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C  If you are not familiar with the code GLBERR and how it is used in
C  conjunction with UT and CT to solve initial value problems, you should
C  study the document file rksuite.doc carefully before attempting to use 
C  the code.  The following "Brief Reminder" is intended only to remind you 
C  of the meaning, type, and size requirements of the arguments.
C
C  If ERRASS was set .TRUE. in the call to SETUP, then after any call to UT
C  or CT to advance the integration to TNOW or TWANT, the subroutine GLBERR
C  may be called to obtain an assessment of the true error of the integration.
C  At each step and for each solution component Y(L), a more accurate "true"
C  solution YT(L), an average magnitude "size(L)" of its size, and its error
C                abs(Y(L) - YT(L))/max("size(L)",THRES(L))
C  are computed.  The assessment returned in RMSERR(L) is the RMS (root-mean-
C  square) average of the error in the Lth solution component over all steps
C  of the integration from TSTART through TNOW.
C
C  OUTPUT VARIABLES
C
C     RMSERR(*) - DOUBLE PRECISION array of length NEQ
C                 RMSERR(L) approximates the RMS average of the true error 
C                 of the numerical solution for the Ith solution component,
C                 L = 1,2,...,NEQ.  The average is taken over all steps from
C                 TSTART to TNOW.
C     ERRMAX    - DOUBLE PRECISION
C                 The maximum (approximate) true error taken over all
C                 solution components and all steps from TSTART to TNOW.
C     TERRMX    - DOUBLE PRECISION
C                 First value of the independent variable where the
C                 (approximate) true error attains the maximum value ERRMAX.
C
C  WORKSPACE
C
C     WORK(*)   - DOUBLE PRECISION array as used in SETUP and UT or CT
C                 Do not alter the contents of this array.
C
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C     .. Scalar Arguments ..
      DOUBLE PRECISION  ERRMAX, TERRMX
C     .. Array Arguments ..
      DOUBLE PRECISION  RMSERR(*), WORK(*)
C     .. Common Block for Problem Definition ..
      DOUBLE PRECISION  TSTRT, TND, DIR, HSTRT, TOLR
      INTEGER           NEQN
      COMMON /RKCOM1/   TSTRT, TND, DIR, HSTRT, TOLR, NEQN
      SAVE   /RKCOM1/
C     .. Common Block to hold Problem Status ..
      DOUBLE PRECISION  T, H, TOLD, HOLD
      INTEGER           NFCN, SVNFCN, OKSTP, FLSTP
      LOGICAL           FIRST, LAST
      COMMON /RKCOM2/   T, H, TOLD, HOLD, NFCN, SVNFCN, OKSTP, FLSTP,
     &                  FIRST, LAST
      SAVE   /RKCOM2/
C     .. Common Block for Global Error Assessment ..
      DOUBLE PRECISION  MAXERR, LOCMAX
      INTEGER           GNFCN, PRZSTG, PRZY, PRZYP, PRZERS, PRZERR,
     &                  PRZYNU
      LOGICAL           ERASON, ERASFL
      COMMON /RKCOM6/   MAXERR, LOCMAX, GNFCN, PRZSTG, PRZY, PRZYP,
     &                  PRZERS, PRZERR, PRZYNU, ERASON, ERASFL
      SAVE   /RKCOM6/
C     .. Common Block for Integrator Options ..
      LOGICAL           MSG, UTASK
      COMMON /RKCOM8/   MSG, UTASK
      SAVE   /RKCOM8/
C     .. Common Block for Error Message ..
      CHARACTER*80      REC(10)
      COMMON /RKCOM9/   REC
      SAVE   /RKCOM9/
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='GLBERR')
      LOGICAL           ASK
      PARAMETER         (ASK=.TRUE.)
      INTEGER           MINUS1, MINUS2
      PARAMETER         (MINUS1=-1,MINUS2=-2)
C     .. Local Scalars ..
      INTEGER           FLAG, IER, L, NREC, STATE
C     .. External Subroutines ..
      EXTERNAL          RKMSG, RKSIT
C     .. Intrinsic Functions ..
      INTRINSIC         SQRT
C     .. Executable Statements ..
C
      IER = 1
      NREC = 0
C
C  Is it permissible to call GLBERR?
C
      CALL RKSIT(ASK,SRNAME,STATE)
      IF (STATE.EQ.911) THEN
         IER = 912
         NREC = 1
         WRITE (REC,'(A)')
     &' ** A catastrophic error has already been detected elsewhere.'
         GO TO 40
      END IF
      IF (STATE.EQ.MINUS2) THEN
         IER = 911
         NREC = 3
         WRITE (REC,'(A/A/A)')
     &' ** You have already made a call to GLBERR after a hard ',
     &' ** failure was reported from the integrator. You cannot',
     &' ** call GLBERR again.'
         GO TO 40
      END IF
      CALL RKSIT(ASK,'CT',STATE)
      IF (STATE.EQ.MINUS1) THEN
         IER = 911
         NREC = 1
         IF (UTASK) THEN
            WRITE (REC,'(A)')
     &' ** You have not yet called UT, so you cannot call GLBERR.'
         ELSE
            WRITE (REC,'(A)')
     &' ** You have not yet called CT, so you cannot call GLBERR.'
         END IF
         GO TO 40
      END IF
C
C  Set flag so that the routine can only be called once after a hard 
C  failure from the integrator.
      IF (STATE.EQ.5 .OR. STATE.EQ.6) IER = MINUS2
C
C  Check that ERRASS was set properly for error assessment in SETUP.
C
      IF (.NOT.ERASON) THEN
         IER = 911
         NREC = 3
         WRITE (REC,'(A/A/A)')
     &' ** No error assessment is available since you did not ',
     &' ** ask for it in your call to the routine SETUP.',
     &' ** Check your program carefully.'
         GO TO 40
      END IF
C
C  Check to see if the integrator has not actually taken a step.
C
      IF (OKSTP.EQ.0) THEN
         IER = 911
         NREC = 3
         WRITE (REC,'(A/A/A)')
     &' ** The integrator has not actually taken any successful ',
     &' ** steps.  You cannot call GLBERR in this circumstance. ',
     &' ** Check your program carefully.'
         GO TO 40
      END IF
C
C  Compute RMS error and set output variables.
C
      ERRMAX = MAXERR
      TERRMX = LOCMAX
      DO 20 L = 1, NEQN
         RMSERR(L) = SQRT(WORK(PRZERR-1+L)/OKSTP)
   20 CONTINUE
C
   40 CONTINUE
C
      CALL RKMSG(IER,SRNAME,NREC,FLAG)
C
      RETURN
      END
      SUBROUTINE CT(F,TNOW,YNOW,YPNOW,WORK,CFLAG)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C  If you are not familiar with the code CT and how it is used in
C  conjunction with SETUP to solve initial value problems, you should study
C  the document file rksuite.doc carefully before attempting to use the code.
C  The following "Brief Reminder" is intended only to remind you of the
C  meaning, type, and size requirements of the arguments.
C
C  NAME DECLARED IN AN EXTERNAL STATEMENT IN THE CALLING PROGRAM:
C
C     F         - name of the subroutine for evaluating the differential
C                 equations.
C
C  The subroutine F must have the form
C
C  SUBROUTINE F(T,Y,YP)
C  DOUBLE PRECISION T,Y(*),YP(*)
C     Using the input values of the independent variable T and the solution
C     components Y(*), for each L = 1,2,...,NEQ evaluate the differential
C     equation for the derivative of the Lth solution component and place the
C     value in YP(L).  Do not alter the input values of T and Y(*).
C  RETURN
C  END
C
C  OUTPUT VARIABLES
C
C     TNOW      - DOUBLE PRECISION
C                 Current value of the independent variable.
C     YNOW(*)   - DOUBLE PRECISION array of length NEQ
C                 Approximation to the true solution at TNOW.
C     YPNOW(*)  - DOUBLE PRECISION array of length NEQ
C                 Approximation to the first derivative of the
C                 true solution at TNOW.
C
C  WORKSPACE
C
C     WORK(*)   - DOUBLE PRECISION array as used in SETUP
C                 Do not alter the contents of this array.
C
C  OUTPUT VARIABLE
C
C     CFLAG     - INTEGER
C
C                       SUCCESS.  A STEP WAS TAKEN TO TNOW.
C                 = 1 - Complete success.
C
C                       "SOFT" FAILURES
C                 = 2 - Warning:  You have obtained an answer by integrating
C                       to TEND (TNOW = TEND).  You have done this at least
C                       100 times, and monitoring of the computation reveals
C                       that this way of getting output has degraded the
C                       efficiency of the code. If you really need answers at
C                       so many specific points, it would be more efficient to
C                       get them with INTRP.  (If METHOD = 3, you would need
C                       to change METHOD and restart from TNOW, YNOW(*) by a
C                       call to SETUP.)  If you wish to continue as you are,
C                       you may.
C                 = 3 - Warning:  A considerable amount of work has been
C                       expended. To continue the integration, just call
C                       CT again.
C                 = 4 - Warning:  It appears that this problem is "stiff".
C                       You really should change to another code that is
C                       intended for such problems, but if you insist, you 
C                       can continue with CT by calling it again.
C
C                       "HARD" FAILURES
C                 = 5 - You are asking for too much accuracy. You cannot
C                       continue integrating this problem.
C                 = 6 - The global error assessment may not be reliable beyond
C                       the current point in the integration.  You cannot
C                       continue integrating this problem.
C
C                       "CATASTROPHIC" FAILURES
C                 = 911 - The nature of the catastrophe is reported on
C                         the standard output channel. Unless special
C                         provision was made in advance (see rksuite.doc),
C                         the computation then comes to a STOP.
C
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C     .. Scalar Arguments ..
      DOUBLE PRECISION  TNOW
      INTEGER           CFLAG
C     .. Array Arguments ..
      DOUBLE PRECISION  WORK(*), YNOW(*), YPNOW(*)
C     .. Subroutine Arguments ..
      EXTERNAL          F
C     .. Common Block for Problem Definition ..
      DOUBLE PRECISION  TSTRT, TND, DIR, HSTRT, TOLR
      INTEGER           NEQN
      COMMON /RKCOM1/   TSTRT, TND, DIR, HSTRT, TOLR, NEQN
      SAVE   /RKCOM1/
C     .. Common Block to hold Problem Status ..
      DOUBLE PRECISION  T, H, TOLD, HOLD
      INTEGER           NFCN, SVNFCN, OKSTP, FLSTP
      LOGICAL           FIRST, LAST
      COMMON /RKCOM2/   T, H, TOLD, HOLD, NFCN, SVNFCN, OKSTP, FLSTP,
     &                  FIRST, LAST
      SAVE   /RKCOM2/
C     .. Common Block for General Workspace Pointers ..
      INTEGER           PRTHRS, PRERST, PRWT, PRYOLD, PRSCR, PRY, PRYP,
     &                  PRSTGS, PRINTP, LNINTP
      COMMON /RKCOM3/   PRTHRS, PRERST, PRWT, PRYOLD, PRSCR, PRY, PRYP,
     &                  PRSTGS, PRINTP, LNINTP
      SAVE   /RKCOM3/
C     .. Common Block to hold Formula Characterisitcs ..
      DOUBLE PRECISION  TOOSML, COST, SAFETY, EXPON, STBRAD, TANANG,
     &                  RS, RS1, RS2, RS3, RS4
      INTEGER           ORDER, LSTSTG, MAXTRY, NSEC
      LOGICAL           FSAL
      COMMON /RKCOM5/   TOOSML, COST, SAFETY, EXPON, STBRAD, TANANG,
     &                  RS, RS1, RS2, RS3, RS4, ORDER, LSTSTG, MAXTRY,
     &                  NSEC, FSAL
      SAVE   /RKCOM5/
C     .. Common Block for Global Error Assessment ..
      DOUBLE PRECISION  MAXERR, LOCMAX
      INTEGER           GNFCN, PRZSTG, PRZY, PRZYP, PRZERS, PRZERR,
     &                  PRZYNU
      LOGICAL           ERASON, ERASFL
      COMMON /RKCOM6/   MAXERR, LOCMAX, GNFCN, PRZSTG, PRZY, PRZYP,
     &                  PRZERS, PRZERR, PRZYNU, ERASON, ERASFL
      SAVE   /RKCOM6/
C     .. Common Block for Environment Parameters ..
      DOUBLE PRECISION  MCHEPS, DWARF, RNDOFF, SQRRMC, CUBRMC, TINY
      INTEGER           OUTCH
      COMMON /RKCOM7/   MCHEPS, DWARF, RNDOFF, SQRRMC, CUBRMC, TINY,
     &                  OUTCH
      SAVE   /RKCOM7/
C     .. Common Block for Integrator Options ..
      LOGICAL           MSG, UTASK
      COMMON /RKCOM8/   MSG, UTASK
      SAVE   /RKCOM8/
C     .. Common Block for Error Message ..
      CHARACTER*80      REC(10)
      COMMON /RKCOM9/   REC
      SAVE   /RKCOM9/
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='CT')
      LOGICAL           ASK, TELL
      PARAMETER         (ASK=.TRUE.,TELL=.FALSE.)
      INTEGER           MINUS1, MINUS2
      PARAMETER         (MINUS1=-1,MINUS2=-2)
      INTEGER           MAXFCN
      PARAMETER         (MAXFCN=5000)
      DOUBLE PRECISION  ZERO, PT1, PT9, ONE, TWO, HUNDRD
      PARAMETER         (ZERO=0.0D+0,PT1=0.1D+0,PT9=0.9D+0,ONE=1.0D+0,
     &                  TWO=2.0D+0,HUNDRD=100.0D+0)
C     .. Local Scalars ..
      DOUBLE PRECISION  ALPHA, BETA, ERR, ERROLD, HAVG, HMIN, HTRY, TAU,
     &                  TEMP1, TEMP2, YPNORM
      INTEGER           IER, JFLSTP, L, NREC, NTEND, POINT, STATE, YNEW,
     &                  YPOLD
      LOGICAL           CHKEFF, FAILED, MAIN, PHASE1, PHASE2, PHASE3,
     &                  TOOMCH
C     .. External Subroutines ..
      EXTERNAL          RKMSG, RKSIT, STEP, STIFF, TRUERR
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, MAX, MIN, SIGN
C     .. Save statement ..
      SAVE              JFLSTP, NTEND, ERROLD, HAVG, PHASE2, YNEW,
     &                  YPOLD, CHKEFF
C     .. Executable Statements ..
C
      IER = 1
      NREC = 0
C
C  Is it permissible to call CT?
C
      CALL RKSIT(ASK,'SETUP',STATE)
      IF (STATE.EQ.911) THEN
         IER = 912
         NREC = 1
         WRITE (REC,'(A)')
     &' ** A catastrophic error has already been detected elsewhere.'
         GO TO 180
      END IF
      IF (STATE.EQ.MINUS1) THEN
         IER = 911
         NREC = 1
         WRITE (REC,'(A)')
     &' ** You have not called SETUP, so you cannot use CT.'
         GO TO 180
      END IF
      IF (UTASK) THEN
         CALL RKSIT(ASK,'UT',STATE)
         IF (STATE.NE.MINUS2) THEN
            IER = 911
            NREC = 2
            WRITE (REC,'(A/A)')
     &' ** You have called CT after you specified in SETUP that ',
     &' ** you were going to use UT. This is not permitted.'
            UTASK = .FALSE.
            GO TO 180
         END IF
      END IF
      CALL RKSIT(ASK,SRNAME,STATE)
      IF (STATE.EQ.5 .OR. STATE.EQ.6) THEN
         IER = 911
         NREC = 3
         WRITE (REC,'(A/A)')
     &' ** CT has already returned with a flag value of 5 or 6.',
     &' ** You cannot continue integrating this problem. You must ',
     &' ** call SETUP to start another problem.'
         GO TO 180
      END IF
C
      IF (FIRST) THEN
C
C  First call in an integration -- initialize everything.
C
         CHKEFF = .FALSE.
         NTEND = 0
         JFLSTP = 0
C
C  A scratch area of WORK(*) starting at PRSCR is used to hold two
C  arrays in this subroutine: the higher order approximate solution at
C  the end of a step and the approximate derivative of the solution at
C  the end of the last step. To make this clear, local pointers YNEW and
C  YPOLD are used.
         YNEW = PRSCR
         YPOLD = PRSCR
C
C  For this first step T was initialized to TSTRT in SETUP and the
C  starting values YSTART(*) were loaded into the area of WORK(*) reserved
C  for the current solution approximation starting at location PRY. The
C  derivative is now computed and stored in WORK(*) starting at PRYP.
C  Subsequently these arrays are copied to the output vectors YNOW(*)
C  and YPNOW(*).
         CALL F(T,WORK(PRY),WORK(PRYP))
         NFCN = NFCN + 1
         DO 20 L = 1, NEQN
            YNOW(L) = WORK(PRY-1+L)
            YPNOW(L) = WORK(PRYP-1+L)
   20    CONTINUE
C
C  Set dependent variables for error assessment.
         IF (ERASON) THEN
            DO 40 L = 1, NEQN
               WORK(PRZY-1+L) = YNOW(L)
               WORK(PRZYP-1+L) = YPNOW(L)
   40       CONTINUE
         END IF
C
C  The weights for the control of the error depend on the size of the
C  solution at the beginning and at the end of the step. On the first
C  step we do not have all this information. Whilst determining the
C  initial step size we initialize the weight vector to the larger of
C  abs(Y(L)) and the threshold for this component.
         DO 60 L = 1, NEQN
            WORK(PRWT-1+L) = MAX(ABS(YNOW(L)),WORK(PRTHRS-1+L))
   60    CONTINUE
C
C  If HSTRT is equal to zero, the code is to find an on-scale initial step
C  size H.  CT has an elaborate scheme of three phases for finding such an H,
C  and some preparations are made earlier.  In SETUP an upper bound is placed
C  on H that reflects the scale of the independent variable. When UTASK is
C  .TRUE., UT refines this bound using the first output point.  Here in CT
C  PHASE1 applies a rule of thumb based on the error control, the order of the
C  the formula, and the size of the initial slope to get a crude approximation
C  to an on-scale H.  PHASE2 may reduce H in the course of taking the first
C  step.  PHASE3 repeatedly adjusts H and retakes the first step until H is
C  on scale.
C
C  A guess for the magnitude of the first step size H can be provided to SETUP
C  as HSTART.  If it is too big or too small, it is ignored and the automatic
C  determination of an on-scale initial step size is activated.  If it is
C  acceptable, H is set to HSTART in SETUP.  Even when H is supplied to CT,
C  PHASE3 of the scheme for finding an on-scale initial step size is made
C  active so that the code can deal with a bad guess.
C
         PHASE1 = HSTRT .EQ. ZERO
         PHASE2 = PHASE1
         PHASE3 = .TRUE.
         IF (PHASE1) THEN
            H = ABS(H)
            YPNORM = ZERO
            DO 80 L = 1, NEQN
               IF (ABS(YNOW(L)).NE.ZERO) THEN
                  YPNORM = MAX(YPNORM,ABS(YPNOW(L))/WORK(PRWT-1+L))
               END IF
   80       CONTINUE
            TAU = TOLR**EXPON
            IF (H*YPNORM.GT.TAU) H = TAU/YPNORM
            HMIN = MAX(TINY,TOOSML*MAX(ABS(TSTRT),ABS(TND)))
            H = DIR*MAX(H,HMIN)
            PHASE1 = .FALSE.
         END IF
C
      ELSE
C
C Continuation call
C
         IF (LAST) THEN
            IER = 911
            NREC = 3
            WRITE (REC,'(A,D13.5,A/A/A)')
     &' ** You have already reached TEND ( = ', TND, ').',
     &' ** To integrate further with the same problem you must ',
     &' ** call the routine RESET with a new value of TEND.'
            GO TO 180
         END IF
      END IF
C
C  Begin computation of a step here.
C
      FAILED = .FALSE.
C
  100 CONTINUE
      H = SIGN(ABS(H),DIR)
C
C  Reduce the step size if necessary so that the code will not step
C  past TND.  "Look ahead" to prevent unnecessarily small step sizes.
      LAST = DIR*((T+H)-TND) .GE. ZERO
      IF (LAST) THEN
         H = TND - T
      ELSE IF (DIR*((T+TWO*H)-TND).GE.ZERO) THEN
         H = (TND-T)/TWO
      END IF
C
C  When the integrator is at T and attempts a step of H, the function
C  defining the differential equations will be evaluated at a number of
C  arguments between T and T+H.  If H is too small, these arguments cannot
C  be clearly distinguished in the precision available.
C
      HMIN = MAX(TINY,TOOSML*MAX(ABS(T),ABS(T+H)))
      IF (ABS(H).LT.HMIN) THEN
         IER = 5
         NREC = 3
         WRITE (REC,'(A/A,D13.5,A,D13.5/A)')
     &' ** In order to satisfy your error requirements CT would ',
     &' ** have to use a step size of ',H,' at TNOW = ',T,
     &' ** This is too small for the machine precision.'
         GO TO 180
      END IF
C
C  Monitor the impact of output on the efficiency of the integration.
C
      IF (CHKEFF) THEN
         NTEND = NTEND + 1
         IF (NTEND.GE.100 .AND. NTEND.GE.OKSTP/3) THEN
            IER = 2
            NREC = 6
            WRITE (REC,'(A/A/A/A/A/A)')
     &' ** More than 100 output points have been obtained by ',
     &' ** integrating to TEND.  They have been sufficiently close ',
     &' ** to one another that the efficiency of the integration has ',
     &' ** been degraded. It would probably be (much) more efficient ',
     &' ** to obtain output by interpolating with INTRP (after ',
     &' ** changing to METHOD=2 if you are using METHOD = 3).'
            NTEND = 0
            GO TO 180
         END IF
      END IF
C
C  Check for stiffness and for too much work.  Stiffness can be
C  checked only after a successful step.
C
      IF (.NOT.FAILED) THEN
C
C  Check for too much work.
         TOOMCH = NFCN .GT. MAXFCN
         IF (TOOMCH) THEN
            IER = 3
            NREC = 3
           WRITE (REC,'(A,I6,A/A/A)')
     &' ** Approximately ',MAXFCN,' function evaluations have been ',
     &' ** used to compute the solution since the integration ',
     &' ** started or since this message was last printed.'
C
C  After this warning message, NFCN is reset to permit the integration
C  to continue.  The total number of function evaluations in the primary
C  integration is SVNFCN + NFCN.
            SVNFCN = SVNFCN + NFCN
            NFCN = 0
         END IF
C
C  Check for stiffness.  NREC is passed on to STIFF because when
C  TOOMCH = .TRUE. and stiffness is diagnosed, the message about too
C  much work is augmented inside STIFF to explain that it is due to
C  stiffness.
         CALL STIFF(F,HAVG,JFLSTP,TOOMCH,MAXFCN,WORK,IER,NREC)
C
         IF (IER.NE.1) GO TO 180
      END IF
C
C  Take a step.  Whilst finding an on-scale H (PHASE2 = .TRUE.), the input
C  value of H might be reduced (repeatedly), but it will not be reduced
C  below HMIN.  The local error is estimated, a weight vector is formed,
C  and a weighted maximum norm, ERR, of the local error is returned.
C  The variable MAIN is input as .TRUE. to tell STEP that this is the
C  primary, or "main", integration.
C
C  H resides in the common block /RKCOM2/ which is used by both CT and STEP;
C  since it may be changed inside STEP, a local copy is made to ensure
C  portability of the code.
C
      MAIN = .TRUE.
      HTRY = H
      CALL STEP(F,NEQN,T,WORK(PRY),WORK(PRYP),WORK(PRSTGS),TOLR,HTRY,
     &          WORK(PRWT),WORK(YNEW),WORK(PRERST),ERR,MAIN,HMIN,
     &          WORK(PRTHRS),PHASE2)
      H = HTRY
C
C  Compare the norm of the local error to the tolerance.
C
      IF (ERR.GT.TOLR) THEN
C
C  Failed step.  Reduce the step size and try again.
C
C  First step:  Terminate PHASE3 of the search for an on-scale step size.
C               The step size is not on scale, so ERR may not be accurate;
C               reduce H by a fixed factor.  Failed attempts to take the
C               first step are not counted.
C  Later step:  Use ERR to compute an "optimal" reduction of H.  More than
C               one failure indicates a difficulty with the problem and an
C               ERR that may not be accurate, so reduce H by a fixed factor.
C
         IF (FIRST) THEN
            PHASE3 = .FALSE.
            ALPHA = RS1
         ELSE
            FLSTP = FLSTP + 1
            JFLSTP = JFLSTP + 1
            IF (FAILED) THEN
               ALPHA = RS1
            ELSE
               ALPHA = SAFETY*(TOLR/ERR)**EXPON
               ALPHA = MAX(ALPHA,RS1)
            END IF
         END IF
         H = ALPHA*H
         FAILED = .TRUE.
         GO TO 100
      END IF
C
C  Successful step.
C
C  Predict a step size appropriate for the next step.  After the first
C  step the prediction can be refined using an idea of H.A. Watts that
C  takes account of how well the prediction worked on the previous step.
      BETA = (ERR/TOLR)**EXPON
      IF (.NOT.FIRST) THEN
         TEMP1 = (ERR**EXPON)/H
         TEMP2 = (ERROLD**EXPON)/HOLD
         IF (TEMP1.LT.TEMP2*HUNDRD .AND. TEMP2.LT.TEMP1*HUNDRD) THEN
            BETA = BETA*(TEMP1/TEMP2)
         END IF
      END IF
      ALPHA = RS3
      IF (SAFETY.LT.BETA*ALPHA) ALPHA = SAFETY/BETA
C
C  On the first step a search is made for an on-scale step size.  PHASE2
C  of the scheme comes to an end here because a step size has been found
C  that is both successful and has a credible local error estimate. Except
C  in the special case that the first step is also the last, the step is
C  repeated in PHASE3 as long as an increase greater than RS2 appears
C  possible.  An increase as big as RS3 is permitted.  A step failure
C  terminates PHASE3.
C
      IF (FIRST) THEN
         PHASE2 = .FALSE.
         PHASE3 = PHASE3 .AND. .NOT. LAST .AND. (ALPHA.GT.RS2)
         IF (PHASE3) THEN
            H = ALPHA*H
            GO TO 100
         END IF
      END IF
C
C  After getting on scale, step size changes are more restricted.
      ALPHA = MIN(ALPHA,RS)
      IF (FAILED) ALPHA = MIN(ALPHA,ONE)
      ALPHA = MAX(ALPHA,RS1)
      HOLD = H
      H = ALPHA*H
C
C  For the diagnosis of stiffness, an average accepted step size, HAVG,
C  must be computed and SAVEd.
      IF (FIRST) THEN
         HAVG = HOLD
      ELSE
         HAVG = PT9*HAVG + PT1*HOLD
      END IF
C
      FIRST = .FALSE.
      ERROLD = ERR
      TOLD = T
C
C  Take care that T is set to precisely TND when the end of the
C  integration is reached.
      IF (LAST) THEN
         T = TND
      ELSE
         T = T + HOLD
      END IF
C
C  Increment counter on accepted steps.  Note that successful steps
C  that are repeated whilst getting on scale are not counted.
      OKSTP = OKSTP + 1
C
C  Advance the current solution and its derivative.  (Stored in WORK(*)
C  with the first location being PRY and PRYP, respectively.)  Update the
C  previous solution and its derivative.  (Stored in WORK(*) with the first
C  location being PRYOLD and YPOLD, respectively.)  Note that the previous
C  derivative will overwrite YNEW(*).
C
      DO 120 L = 1, NEQN
         WORK(PRYOLD-1+L) = WORK(PRY-1+L)
         WORK(PRY-1+L) = WORK(YNEW-1+L)
         WORK(YPOLD-1+L) = WORK(PRYP-1+L)
  120 CONTINUE
C
      IF (FSAL) THEN
C
C  When FSAL = .TRUE., YP(*) is the last stage of the step.
         POINT = PRSTGS + (LSTSTG-1)*NEQN
         DO 140 L = 1, NEQN
            WORK(PRYP-1+L) = WORK(POINT-1+L)
  140    CONTINUE
      ELSE
C
C  Call F to evaluate YP(*).
         CALL F(T,WORK(PRY),WORK(PRYP))
         NFCN = NFCN + 1
      END IF
C
C  If global error assessment is desired, advance the secondary
C  integration from TOLD to T.
C
      IF (ERASON) THEN
         CALL TRUERR(F,NEQN,WORK(PRY),TOLR,WORK(PRWT),WORK(PRZY),
     &               WORK(PRZYP),WORK(PRZERR),WORK(PRZYNU),WORK(PRZERS),
     &               WORK(PRZSTG),IER)
         IF (IER.EQ.6) THEN
C
C  The global error estimating procedure has broken down. Treat it as a
C  failed step. The solution and derivative are reset to their values at
C  the beginning of the step since the last valid error assessment refers
C  to them.
            OKSTP = OKSTP - 1
            ERASFL = .TRUE.
            LAST = .FALSE.
            T = TOLD
            H = HOLD
            DO 160 L = 1, NEQN
               WORK(PRY-1+L) = WORK(PRYOLD-1+L)
               WORK(PRYP-1+L) = WORK(YPOLD-1+L)
  160       CONTINUE
            IF (OKSTP.GT.1) THEN
               NREC = 2
               WRITE (REC,'(A/A,D13.5,A)')
     &' ** The global error assessment may not be reliable for T past ',
     &' ** TNOW = ',T,'.  The integration is being terminated.'
            ELSE
               NREC = 2
               WRITE (REC,'(A/A)')
     &' ** The global error assessment algorithm failed at the start',
     &' ** the integration.  The integration is being terminated.'
            END IF
            GO TO 180
         END IF
      END IF
C
C
C  Exit point for CT
C
  180 CONTINUE
C
C  Set the output variables and flag that interpolation is permitted
C
      IF (IER.LT.911) THEN
         TNOW = T
         LAST = TNOW .EQ. TND
         CHKEFF = LAST
         DO 200 L = 1, NEQN
            YNOW(L) = WORK(PRY-1+L)
            YPNOW(L) = WORK(PRYP-1+L)
  200    CONTINUE
         IF (IER.EQ.1) THEN
            STATE = MINUS2
            CALL RKSIT(TELL,'INTRP',STATE)
         END IF
      END IF
C
C  Call RKMSG to report what happened and set CFLAG.
C
      CALL RKMSG(IER,SRNAME,NREC,CFLAG)
C
      RETURN
      END
      SUBROUTINE INTRP(TWANT,REQEST,NWANT,YWANT,YPWANT,F,WORK,WRKINT,
     &                 LENINT)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C  If you are not familiar with the code INTRP and how it is used in
C  conjunction with CT to solve initial value problems, you should study the
C  document file rksuite.doc carefully before attempting to use the code. The
C  following "Brief Reminder" is intended only to remind you of the meaning,
C  type, and size requirements of the arguments.
C
C  When integrating with METHOD = 1 or 2, answers may be obtained inexpensively
C  between steps by interpolation. INTRP is called after a step by CT from a
C  previous value of T, called TOLD below, to the current value of T to get
C  an answer at TWANT. You can specify any value of TWANT you wish, but
C  specifying a value outside the interval [TOLD,T] is likely to yield
C  answers with unsatisfactory accuracy.
C
C  INPUT VARIABLE
C
C     TWANT     - DOUBLE PRECISION
C                 The value of the independent variable where a solution
C                 is desired.
C
C  The interpolant is to be evaluated at TWANT to approximate the solution
C  and/or its first derivative there.  There are three cases:
C
C  INPUT VARIABLE
C
C     REQEST    - CHARACTER*(*)
C                 Only the first character of REQEST is significant.
C                 REQEST(1:1) = `S' or `s'- compute approximate `S'olution 
C                                           only.
C                             = `D' or `d'- compute approximate first 
C                                           `D'erivative of the solution only.
C                             = `B' or `b'- compute `B'oth approximate solution
C                                           and first derivative.
C                 Constraint: REQEST(1:1) must be `S',`s',`D',`d',`B' or `b'.
C
C  If you intend to interpolate at many points, you should arrange for the
C  the interesting components to be the first ones because the code
C  approximates only the first NWANT components.
C
C  INPUT VARIABLE
C
C     NWANT     - INTEGER
C                 Only the first NWANT components of the answer are to be
C                 computed.
C                 Constraint:  NEQ >= NWANT >= 1
C
C  OUTPUT VARIABLES
C
C     YWANT(*)  - DOUBLE PRECISION array of length NWANT
C                 Approximation to the first NWANT components of the true
C                 solution at TWANT when REQESTed.
C     YPWANT(*) - DOUBLE PRECISION array of length NWANT
C                 Approximation to the first NWANT components of the first
C                 derivative of the true solution at TWANT when REQESTed.
C
C  NAME DECLARED IN AN EXTERNAL STATEMENT IN THE PROGRAM CALLING INTRP:
C
C     F         - name of the subroutine for evaluating the differential
C                 equations as provided to CT.
C
C  WORKSPACE
C
C     WORK(*)   - DOUBLE PRECISION array as used in SETUP and CT
C                 Do not alter the contents of this array.
C
C     WRKINT(*) - DOUBLE PRECISION array of length LENINT
C                 Do not alter the contents of this array.
C
C     LENINT    - INTEGER
C                 Length of WRKINT. If
C                 METHOD = 1, LENINT must be at least 1
C                        = 2, LENINT must be at least NEQ+MAX(NEQ,5*NWANT)
C                        = 3--CANNOT BE USED WITH THIS SUBROUTINE
C
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C     .. Scalar Arguments ..
      DOUBLE PRECISION  TWANT
      INTEGER           LENINT, NWANT
      CHARACTER*(*)     REQEST
C     .. Array Arguments ..
      DOUBLE PRECISION  WORK(*), WRKINT(*), YPWANT(*), YWANT(*)
C     .. Subroutine Arguments ..
      EXTERNAL          F
C     .. Common Block for Problem Definition ..
      DOUBLE PRECISION  TSTRT, TND, DIR, HSTRT, TOLR
      INTEGER           NEQN
      COMMON /RKCOM1/   TSTRT, TND, DIR, HSTRT, TOLR, NEQN
      SAVE   /RKCOM1/
C     .. Common Block for General Workspace Pointers ..
      INTEGER           PRTHRS, PRERST, PRWT, PRYOLD, PRSCR, PRY, PRYP,
     &                  PRSTGS, PRINTP, LNINTP
      COMMON /RKCOM3/   PRTHRS, PRERST, PRWT, PRYOLD, PRSCR, PRY, PRYP,
     &                  PRSTGS, PRINTP, LNINTP
      SAVE   /RKCOM3/
C     .. Common Block to hold Formula Definitions ..
      DOUBLE PRECISION  A(13,13), B(13), C(13), BHAT(13), R(11,6),
     &                  E(7)
      INTEGER           PTR(13), NSTAGE, METHD, MINTP
      LOGICAL           INTP
      COMMON /RKCOM4/   A, B, C, BHAT, R, E, PTR, NSTAGE, METHD,
     &                  MINTP, INTP
      SAVE   /RKCOM4/
C     .. Common Block for Integrator Options ..
      LOGICAL           MSG, UTASK
      COMMON /RKCOM8/   MSG, UTASK
      SAVE   /RKCOM8/
C     .. Common Block for Error Message ..
      CHARACTER*80      REC(10)
      COMMON /RKCOM9/   REC
      SAVE   /RKCOM9/
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='INTRP')
      LOGICAL           ASK
      INTEGER           PLUS1, MINUS1, MINUS2
      PARAMETER         (ASK=.TRUE.,PLUS1=1,MINUS1=-1,MINUS2=-2)
C     .. Local Scalars ..
      INTEGER           FLAG, ICHK, IER, NREC, NWNTSV, STARTP, STATE,
     &                  STATE1
      LOGICAL           ININTP, LEGALR
      CHARACTER         REQST1
C     .. External Subroutines ..
      EXTERNAL          EVALI, FORMI, RKMSG, RKSIT
C     .. Intrinsic Functions ..
      INTRINSIC         MAX
C     .. Save statement ..
      SAVE              NWNTSV, ININTP, STARTP
C     .. Data statements ..
      DATA              NWNTSV/MINUS1/
C     .. Executable Statements ..
C
      IER = 1
      NREC = 0
C
C  Is it permissible to call INTRP?
C
      CALL RKSIT(ASK,'CT',STATE)
      IF (STATE.EQ.911) THEN
         IER = 912
         NREC = 1
         WRITE (REC,'(A)')
     &' ** A catastrophic error has already been detected elsewhere.'
         GO TO 20
      END IF
      IF (UTASK) THEN
         CALL RKSIT(ASK,'UT',STATE1)
         IF (STATE1.NE.MINUS2) THEN
            IER = 911
            NREC = 2
            WRITE (REC,'(A/A)')
     &' ** You have called INTRP after you specified to SETUP ',
     &' ** that you were going to use UT. This is not permitted.'
            GO TO 20
         END IF
      END IF
      IF (STATE.EQ.MINUS1) THEN
         IER = 911
         NREC = 1
         WRITE (REC,'(A)')
     &' ** You have not called CT, so you cannot use INTRP.'
         GO TO 20
      END IF
      IF (STATE.GT.PLUS1) THEN
         IER = 911
         NREC = 2
         WRITE (REC,'(A/A)')
     &' ** CT has returned with a flag value greater than 1.',
     &' ** You cannot call INTRP in this circumstance.'
         GO TO 20
      END IF
C
C  Check input
C
      REQST1 = REQEST(1:1)
      LEGALR = REQST1 .EQ. 'S' .OR. REQST1 .EQ. 's' .OR.
     &         REQST1 .EQ. 'D' .OR. REQST1 .EQ. 'd' .OR.
     &         REQST1 .EQ. 'B' .OR. REQST1 .EQ. 'b'
      IF (.NOT.LEGALR) THEN
         IER = 911
         NREC = 3
         WRITE (REC,'(A/A,A,A/A)')
     &' ** You have set the first character of ',
     &' ** REQEST to be ''',REQST1,'''. It must be one of ',
     &' ** ''S'',''s'',''D'',''d'',''B'' or ''b''.'
         GO TO 20
      END IF
C
      IF (NWANT.GT.NEQN) THEN
         IER = 911
         NREC = 3
         WRITE (REC,'(A,I6,A/A,I6,A/A)')
     &' ** You have specified the value of NWANT to be ',NWANT,'. This',
     &' ** is greater than ',NEQN,', which is the number of equations ',
     &' ** in the system being integrated.'
         GO TO 20
      ELSE IF (NWANT.LT.1) THEN
         IER = 911
         NREC = 3
         WRITE (REC,'(A,I6,A/A/A)')
     &' ** You have specified the value of NWANT to be ',NWANT,', but ',
     &' ** this is less than 1. You cannot interpolate a zero or ',
     &' ** negative number of components.'
         GO TO 20
      END IF
C
      IF (METHD.EQ.1) THEN
         IF (LENINT.LT.1) THEN
            IER = 911
            NREC = 2
            WRITE (REC,'(A,I6,A/A)')
     &' ** You have specified LENINT to be ',LENINT,'.',
     &' ** This is too small. LENINT must be at least 1.'
            GO TO 20
         END IF
         STARTP = 1
      ELSE IF (METHD.EQ.2) THEN
         ICHK = NEQN + MAX(NEQN,5*NWANT)
         IF (LENINT.LT.ICHK) THEN
            IER = 911
            NREC = 3
            WRITE (REC,'(A,I6,A/A/A,I6,A)')
     &' ** You have specified LENINT to be ',LENINT,'. This is too',
     &' ** small. NINT must be at least NEQ + MAX(NEQ, 5*NWANT) ',
     &' ** which is ', ICHK,'.'
            GO TO 20
         END IF
         STARTP = NEQN + 1
      ELSE IF (METHD.EQ.3) THEN
         IER = 911
         NREC = 5
         WRITE (REC,'(A/A/A/A/A)')
     &' ** You have been using CT with METHOD = 3 to integrate your  ',
     &' ** equations. You have just called INTRP, but interpolation  ',
     &' ** is not available for this METHOD. Either use METHOD = 2,  ',
     &' ** for which interpolation is available, or use RESET to make',
     &' ** CT step exactly to the points where you want output.'
         GO TO 20
      END IF
C
C  Has the interpolant been initialised for this step?
C
      CALL RKSIT(ASK,SRNAME,STATE)
      ININTP = STATE .NE. MINUS2
C
C  Some initialization must be done before interpolation is possible.
C  To reduce the overhead, the interpolating polynomial is formed for
C  the first NWANT components.  In the unusual circumstance that NWANT
C  is changed while still interpolating within the span of the current
C  step, the scheme must be reinitialized to accomodate the additional
C  components.
C
      IF (.NOT.ININTP .OR. NWANT.NE.NWNTSV) THEN
C
C  At present the derivative of the solution at the previous step, YPOLD(*),
C  is stored in the scratch array area starting at PRSCR. In the case of
C  METHD = 1 we can overwrite the stages.
C
         IF (METHD.EQ.1) THEN
            CALL FORMI(F,NEQN,NWANT,WORK(PRY),WORK(PRYP),WORK(PRYOLD),
     &                 WORK(PRSCR),WORK(PRSTGS),.NOT.ININTP,
     &                 WORK(PRSTGS),WORK(PRSTGS))
         ELSE
            CALL FORMI(F,NEQN,NWANT,WORK(PRY),WORK(PRYP),WORK(PRYOLD),
     &                 WORK(PRSCR),WORK(PRSTGS),.NOT.ININTP,WRKINT,
     &                 WRKINT(STARTP))
         END IF
C
C  Set markers to show that interpolation has been initialized for
C  NWANT components.
         NWNTSV = NWANT
         ININTP = .TRUE.
      END IF
C
C  The actual evaluation of the interpolating polynomial and/or its first
C  derivative is done in EVALI.
C
      IF (METHD.EQ.1) THEN
         CALL EVALI(WORK(PRY),WORK(PRYP),WORK(PRSTGS),TWANT,REQEST,
     &              NWANT,YWANT,YPWANT)
      ELSE
         CALL EVALI(WORK(PRY),WORK(PRYP),WRKINT(STARTP),TWANT,REQEST,
     &              NWANT,YWANT,YPWANT)
      END IF
C
   20 CONTINUE
C
      CALL RKMSG(IER,SRNAME,NREC,FLAG)
C
      RETURN
      END
      SUBROUTINE RESET(TENDNU)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C  If you are not familiar with the code RESET and how it is used in
C  conjunction with CT to solve initial value problems, you should study the
C  document file rksuite.doc carefully before attempting to use the code. The
C  following "Brief Reminder" is intended only to remind you of the meaning,
C  type, and size requirements of the arguments.
C
C  The integration using CT proceeds from TSTART in the direction of TEND, and
C  is now at TNOW.  To reset TEND to a new value TENDNU, just call RESET with
C  TENDNU as the argument.  You must continue integrating in the same
C  direction, so the sign of (TENDNU - TNOW) must be the same as the sign of
C  (TEND - TSTART). To change direction you must restart by a call to SETUP.
C
C  INPUT VARIABLE
C
C     TENDNU    - DOUBLE PRECISION
C                 The new value of TEND.
C                 Constraint: TENDNU and TNOW must be clearly distinguishable
C                 in the precision used.  The sign of (TENDNU - TNOW) must be
C                 the same as the sign of (TEND - TSTART).
C
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C     .. Scalar Arguments ..
      DOUBLE PRECISION  TENDNU
C     .. Common Block for Problem Definition ..
      DOUBLE PRECISION  TSTRT, TND, DIR, HSTRT, TOLR
      INTEGER           NEQN
      COMMON /RKCOM1/   TSTRT, TND, DIR, HSTRT, TOLR, NEQN
      SAVE   /RKCOM1/
C     .. Common Block to hold Problem Status ..
      DOUBLE PRECISION  T, H, TOLD, HOLD
      INTEGER           NFCN, SVNFCN, OKSTP, FLSTP
      LOGICAL           FIRST, LAST
      COMMON /RKCOM2/   T, H, TOLD, HOLD, NFCN, SVNFCN, OKSTP, FLSTP,
     &                  FIRST, LAST
      SAVE   /RKCOM2/
C     .. Common Block to hold Formula Characterisitcs ..
      DOUBLE PRECISION  TOOSML, COST, SAFETY, EXPON, STBRAD, TANANG,
     &                  RS, RS1, RS2, RS3, RS4
      INTEGER           ORDER, LSTSTG, MAXTRY, NSEC
      LOGICAL           FSAL
      COMMON /RKCOM5/   TOOSML, COST, SAFETY, EXPON, STBRAD, TANANG,
     &                  RS, RS1, RS2, RS3, RS4, ORDER, LSTSTG, MAXTRY,
     &                  NSEC, FSAL
      SAVE   /RKCOM5/
C     .. Common Block for Environment Parameters ..
      DOUBLE PRECISION  MCHEPS, DWARF, RNDOFF, SQRRMC, CUBRMC, TINY
      INTEGER           OUTCH
      COMMON /RKCOM7/   MCHEPS, DWARF, RNDOFF, SQRRMC, CUBRMC, TINY,
     &                  OUTCH
      SAVE   /RKCOM7/
C     .. Common Block for Integrator Options ..
      LOGICAL           MSG, UTASK
      COMMON /RKCOM8/   MSG, UTASK
      SAVE   /RKCOM8/
C     .. Common Block for Error Message ..
      CHARACTER*80      REC(10)
      COMMON /RKCOM9/   REC
      SAVE   /RKCOM9/
C     .. Parameters ..
      CHARACTER*6       SRNAME
      PARAMETER         (SRNAME='RESET')
      LOGICAL           ASK
      INTEGER           MINUS1, MINUS2
      PARAMETER         (ASK=.TRUE.,MINUS1=-1,MINUS2=-2)
      DOUBLE PRECISION  ZERO
      PARAMETER         (ZERO=0.0D+0)
C     .. Local Scalars ..
      DOUBLE PRECISION  HMIN, TDIFF
      INTEGER           FLAG, IER, NREC, STATE, STATE1
C     .. External Subroutines ..
      EXTERNAL          RKMSG, RKSIT
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, MAX
C     .. Executable Statements ..
      IER = 1
      NREC = 0
C
C  Is it permissible to call RESET?
C
      CALL RKSIT(ASK,'CT',STATE)
      IF (STATE.EQ.911) THEN
         IER = 912
         NREC = 1
         WRITE (REC,'(A)')
     &' ** A catastrophic error has already been detected elsewhere.'
         GO TO 20
      END IF
      IF (UTASK) THEN
         CALL RKSIT(ASK,'UT',STATE1)
         IF (STATE1.NE.MINUS2) THEN
            IER = 911
            NREC = 2
            WRITE (REC,'(A/A)')
     &' ** You have called RESET after you specified to SETUP that ',
     &' ** you were going to use UT. This is not permitted.'
            GO TO 20
         END IF
      END IF
      IF (STATE.EQ.MINUS1) THEN
         IER = 911
         NREC = 1
         WRITE (REC,'(A)')
     &' ** You have not called CT, so you cannot use RESET.'
         GO TO 20
      END IF
      IF (STATE.EQ.5 .OR. STATE.EQ.6) THEN
         IER = 911
         NREC = 2
         WRITE (REC,'(A,I1,A/A)')
     &' ** CT has returned with CFLAG =  ',STATE,'.',
     &' ** You cannot call RESET in this circumstance.'
         GO TO 20
      END IF
C
C  Check value of TENDNU
C
      IF (DIR.GT.ZERO .AND. TENDNU.LE.T) THEN
         IER = 911
         NREC = 4
         WRITE (REC,'(A/A,D13.5/A,D13.5,A/A)')
     &' ** Integration is proceeding in the positive direction. The ',
     &' ** current value for the independent variable is ',T,
     &' ** and you have set TENDNU = ',TENDNU,'.  TENDNU must be ',
     &' ** greater than T.'
      ELSE IF (DIR.LT.ZERO .AND. TENDNU.GE.T) THEN
         IER = 911
         NREC = 4
         WRITE (REC,'(A/A,D13.5/A,D13.5,A/A)')
     &' ** Integration is proceeding in the negative direction. The ',
     &' ** current value for the independent variable is ',T,
     &' ** and you have set TENDNU = ',TENDNU,'.  TENDNU must be ',
     &' ** less than T.'
      ELSE
         HMIN = MAX(TINY,TOOSML*MAX(ABS(T),ABS(TENDNU)))
         TDIFF = ABS(TENDNU-T)
         IF (TDIFF.LT.HMIN) THEN
            IER = 911
            NREC = 4
            WRITE (REC,'(A,D13.5,A/A,D13.5,A/A/A,D13.5,A)')
     &' ** The current value of the independent variable T is ',T,'.',
     &' ** The TENDNU you supplied has ABS(TENDNU-T) = ',TDIFF,'.',
     &' ** For the METHOD and the precision of the computer being ',
     &' ** used, this difference must be at least ',HMIN,'.'
         END IF
      END IF
      IF (IER.EQ.911) GO TO 20
C
      TND = TENDNU
      LAST = .FALSE.
C
   20 CONTINUE
C
      CALL RKMSG(IER,SRNAME,NREC,FLAG)
C
      RETURN
      END
      SUBROUTINE MCONST(METHOD)
C************************************************
C**** NOT A DESIGNATED USER-CALLABLE ROUTINE ****
C************************************************
C
C  Purpose:   Sets machine-dependent global quantities
C
C  Common:    Initializes:    /RKCOM7/ OUTCH, MCHEPS, DWARF, RNDOFF,
C                                      SQRRMC, CUBRMC, TINY
C             Reads:          none
C             Alters:         none
C
C  Comments:
C  =========
C  OUTCH, MCHEPS, DWARF are pure environmental parameters with values
C  obtained from a call to ENVIRN. The other quantities set depend on
C  the environmental parameters, the implementation, and, possibly,
C  METHOD. At present the METHODs implemented in the RK suite do not
C  influence the values of these quantities.
C  OUTCH  - Standard output channel
C  MCHEPS - Largest positive number such that 1.0D0 + MCHEPS = 1.0D0
C  DWARF  - Smallest positive number
C  RNDOFF - 10 times MCHEPS
C  SQRRMC - Square root of MCHEPS
C  CUBRMC - Cube root of MCHEPS
C  TINY   - Square root of DWARF
C
C     .. Scalar Arguments ..
      INTEGER           METHOD
C     .. Common Block for Environment Parameters ..
      DOUBLE PRECISION  MCHEPS, DWARF, RNDOFF, SQRRMC, CUBRMC, TINY
      INTEGER           OUTCH
      COMMON /RKCOM7/   MCHEPS, DWARF, RNDOFF, SQRRMC, CUBRMC, TINY,
     &                  OUTCH
      SAVE   /RKCOM7/
C     .. Parameters ..
      DOUBLE PRECISION  TEN, THIRD
      PARAMETER         (TEN=10.0D+0,THIRD=1.0D+0/3.0D+0)
C     .. External Subroutines ..
      EXTERNAL          ENVIRN
C     .. Intrinsic Functions ..
      INTRINSIC         SQRT
C     .. Executable Statements ..
C
      CALL ENVIRN(OUTCH,MCHEPS,DWARF)
C
      RNDOFF = TEN*MCHEPS
      SQRRMC = SQRT(MCHEPS)
      CUBRMC = MCHEPS**THIRD
      TINY = SQRT(DWARF)
C
      RETURN
      END
      SUBROUTINE ENVIRN(OUTCH,MCHEPS,DWARF)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C  The RK suite requires some environmental parameters that are provided by
C  this subroutine.  The values provided with the distribution codes are those
C  appropriate to the IEEE standard.  They must be altered, if necessary, to
C  those appropriate to the computing system you are using before calling the 
C  codes of the suite.  
C
C        ================================================================
C        ================================================================
C        TO MAKE SURE THAT THESE MACHINE AND INSTALLATION DEPENDENT 
C        QUANTITIES ARE SPECIFIED PROPERLY, THE DISTRIBUTION VERSION 
C        WRITES A MESSAGE ABOUT THE MATTER TO THE STANDARD OUTPUT CHANNEL
C        AND TERMINATES THE RUN.  THE VALUES PROVIDED IN THE DISTRIBUTION
C        VERSION SHOULD BE ALTERED, IF NECESSARY, AND THE "WRITE" AND 
C        "STOP" STATEMENTS COMMENTED OUT.
C        ================================================================
C        ================================================================
C
C  OUTPUT VARIABLES 
C
C     OUTCH     - INTEGER
C                 Standard output channel
C     MCHEPS    - DOUBLE PRECISION
C                 MCHEPS is the largest positive number such that
C                 1.0D0 + MCHEPS = 1.0D0. 
C     DWARF     - DOUBLE PRECISION
C                 DWARF is the smallest positive number.
C
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C     .. Scalar Arguments ..  
      INTEGER           OUTCH
      DOUBLE PRECISION  DWARF, MCHEPS
C     .. Executable Statements ..      
C
C  The following six statements are to be Commented out after verification that
C  the machine and installation dependent quantities are specified correctly.
C  If you pass copies of RKSUITE on to others, please give them the whole
C  distribution version of RKSUITE, and in particular, give them a version 
C  of ENVIRN that does not have the following six statements Commented out.
      WRITE(*,*) ' Before using RKSUITE, you must verify that the  '
      WRITE(*,*) ' machine- and installation-dependent quantities  '
      WRITE(*,*) ' specified in the subroutine ENVIRN are correct, '
      WRITE(*,*) ' and then Comment these WRITE statements and the '
      WRITE(*,*) ' STOP statement out of ENVIRN.                   '
      STOP
C
C  The following values are appropriate to IEEE arithmetic with the typical
C  standard output channel.
C
      OUTCH = 6
      MCHEPS = 1.11D-16
      DWARF = 2.23D-308
C      
C------------------------------------------------------------------------------
C  If you have the routines D1MACH and I1MACH on your system, you could
C  replace the preceding statements by the following ones to obtain the 
C  appropriate machine dependent numbers. The routines D1MACH and I1MACH 
C  are public domain software.  They are available from NETLIB.
C      .. Scalar Arguments ..  
C      INTEGER           OUTCH
C      DOUBLE PRECISION  DWARF, MCHEPS
C      .. External Functions ..
C      INTEGER           I1MACH
C      DOUBLE PRECISION  D1MACH
C      .. Executable Statements ..
C
C      OUTCH = I1MACH(2)
C      MCHEPS = D1MACH(3)
C      DWARF = D1MACH(1)
C
C  If you have the NAG Fortran Library available on your system, you could 
C  replace the preceding statements by the following ones to obtain the 
C  appropriate machine dependent numbers.
C
C      .. Scalar Arguments ..  
C      INTEGER           OUTCH
C      DOUBLE PRECISION  DWARF, MCHEPS
C      .. External Functions ..
C      DOUBLE PRECISION  X02AJF, X02AMF
C      .. Executable Statements ..
C
C      CALL X04AAF(0,OUTCH)
C      MCHEPS = X02AJF()
C      DWARF = X02AMF()
C
C  If you have the IMSL MATH/LIBRARY available on your system, you could
C  replace the preceding statements by the following ones to obtain the
C  appropriate machine dependent numbers.
C
C      .. Scalar Arguments ..  
C      INTEGER           OUTCH
C      DOUBLE PRECISION  DWARF, MCHEPS
C      .. External Functions ..
C      DOUBLE PRECISION  DMACH
C      .. Executable Statements ..
C
C      CALL UMACH(2,OUTCH)
C      MCHEPS = DMACH(4)
C      DWARF = DMACH(1)
C------------------------------------------------------------------------------
C
      RETURN
      END
      SUBROUTINE CONST(METHOD,VECSTG,REQSTG,LINTPL)
C************************************************
C**** NOT A DESIGNATED USER-CALLABLE ROUTINE ****
*************************************************
C
C  Purpose:   Set formula definitions and formula characteristics for
C             selected method. Return storage requirements for the
C             selected method.
C
C  Input:     METHOD
C  Output:    VECSTG, REQSTG, LINTPL
C
C  Common:    Initializes:    /RKCOM4/ A(*,*), B(*), C(*), BHAT(*), R(*),
C                                      E(*), PTR(*), NSTAGE, METHD, INTP, MINTP
C                             /RKCOM5/ TOOSML, COST, SAFETY, EXPON, STBRAD,
C                                      TANANG, RS, RS1, RS2, RS3, RS4, ORDER,
C                                      LSTSTG, MAXTRY, NSEC, FSAL
C             Reads:          /RKCOM7/ RNDOFF
C             Alters:         none
C
C  Comments:
C  =========
C  Runge-Kutta formula pairs are described by a set of coefficients
C  and by the setting of a number of parameters that describe the
C  characteristics of the pair.  The input variable METHD indicates
C  which of the three pairs available is to be set. In the case of
C  METHD = 2 additional coefficients are defined that make interpolation
C  of the results possible and provide an additional error estimator.
C  VECSTG is the number of columns of workspace required to compute the
C  stages of a METHD. For interpolation purposes the routine returns via
C  COMMON the logical variable INTP indicating whether interpolation is
C  possible and via the call list:
C  REQSTG - whether the stages are required to form the
C           interpolant (set .FALSE. if INTP=.FALSE.)
C  LINTPL - the number of extra columns of storage required for use
C           with UT (set 0 if INTP=.FALSE.)
C
C  Quantities set in common blocks:
C  METHD - copy of METHOD
C  A, B, C, BHAT - coefficients of the selected method
C  R      - extra coefficents for interpolation with METHD = 2
C  E      - extra coefficients for additional local error estimate
C           with METHD = 2
C  PTR    - vector of pointers indicating how individual stages are to
C           be stored.  With it zero coefficients of the formulas can
C           be exploited to reduce the storage required
C  NSTAGE - number of stages for the specified METHD
C  INTP   - indicates whether there is an associated interpolant
C           (depending on the method, the user may have to supply
C           extra workspace)
C  MINTP  - the degree of the interpolating polynomial, if one exists
C  FSAL   - indicates whether the last stage of a step can be used as
C           the first stage of the following step
C  LSTSTG - pointer to location of last stage for use with FSAL=.TRUE.
C  ORDER  - the lower order of the pair of Runge-Kutta formulas that
C           constitute a METHD
C  TANANG, 
C  STBRAD - the stability region of the formula used to advance
C           the integration is approximated by a sector in the left half
C           complex plane.  TANANG is the tangent of the interior angle
C           of the sector and STBRAD is the radius of the sector.
C  COST   - cost of a successful step in function evaluations
C  MAXTRY - limit on the number of iterations in the stiffness check. As
C           set, no more than 24 function evaluations are made in the check.
C  NSEC   - each step of size H in the primary integration corresponds to
C           NSEC steps of size H/NSEC in the secondary integration when
C           global error assessment is done.
C  EXPON  - used to adjust the step size; this code implements an error
C           per step control for which EXPON = 1/(ORDER + 1).
C  SAFETY - quantity used in selecting the step size
C  TOOSML - quantity used to determine when a step size is too small for
C           the precision available
C  RS, RS1,
C  RS2, RS3,
C  RS4    - quantities used in determining the maximum and minimum change
C           change in step size (set independently of METHD)
C
C  Further comments on SAFETY:
C ============================
C  The code estimates the largest step size that will yield the specified
C  accuracy.  About half the time this step size would result in a local
C  error that is a little too large, and the step would be rejected.  For
C  this reason a SAFETY factor is used to reduce the "optimal" value to one
C  more likely to succeed.  Unfortunately, there is some art in choosing this
C  value. The more expensive a failed step, the smaller one is inclined to 
C  take this factor. However, a small factor means that if the prediction were
C  good, more accuracy than desired would be obtained and the behavior of the
C  error would then be irregular.  The more stringent the tolerance, the better
C  the prediction, except near limiting precision. Thus the general range of 
C  tolerances expected influences the choice of SAFETY.
C
C     .. Scalar Arguments ..
      INTEGER           LINTPL, METHOD, VECSTG
      LOGICAL           REQSTG
C     .. Common Block to hold Formula Definitions ..
      DOUBLE PRECISION  A(13,13), B(13), C(13), BHAT(13), R(11,6),
     &                  E(7)
      INTEGER           PTR(13), NSTAGE, METHD, MINTP
      LOGICAL           INTP
      COMMON /RKCOM4/   A, B, C, BHAT, R, E, PTR, NSTAGE, METHD,
     &                  MINTP, INTP
      SAVE   /RKCOM4/
C     .. Common Block to hold Formula Characterisitcs ..
      DOUBLE PRECISION  TOOSML, COST, SAFETY, EXPON, STBRAD, TANANG,
     &                  RS, RS1, RS2, RS3, RS4
      INTEGER           ORDER, LSTSTG, MAXTRY, NSEC
      LOGICAL           FSAL
      COMMON /RKCOM5/   TOOSML, COST, SAFETY, EXPON, STBRAD, TANANG,
     &                  RS, RS1, RS2, RS3, RS4, ORDER, LSTSTG, MAXTRY,
     &                  NSEC, FSAL
      SAVE   /RKCOM5/
C     .. Common Block for Environment Parameters ..
      DOUBLE PRECISION  MCHEPS, DWARF, RNDOFF, SQRRMC, CUBRMC, TINY
      INTEGER           OUTCH
      COMMON /RKCOM7/   MCHEPS, DWARF, RNDOFF, SQRRMC, CUBRMC, TINY,
     &                  OUTCH
      SAVE   /RKCOM7/
C     .. Parameters ..
      DOUBLE PRECISION  ONE, ZERO, TWO, FIFTY, FIVEPC
      PARAMETER         (ONE=1.0D+0,ZERO=0.0D+0,TWO=2.0D+0,FIFTY=50.D+0,
     &                  FIVEPC=0.05D+0)
C     .. Local Scalars ..
      DOUBLE PRECISION  CDIFF, DIFF
      INTEGER           I, J
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DBLE, INT, MAX, MIN
C     .. Executable Statements ..
C
      METHD = METHOD
C
      GO TO (20,40,100) METHD
C
C  METHD = 1.
C    This pair is from "A 3(2) Pair of Runge-Kutta Formulas" by P. Bogacki
C    and L.F. Shampine, Appl. Math. Lett., 2, pp. 321-325, 1989.  The authors
C    are grateful to P. Bogacki for his assistance in implementing the pair.
C
   20 CONTINUE
      NSTAGE = 4
      FSAL = .TRUE.
      ORDER = 2
      TANANG = 8.9D0
      STBRAD = 2.3D0
      SAFETY = 0.8D0
      INTP = .TRUE.
      MINTP = 3
      REQSTG = .FALSE.
      LINTPL = 2
      NSEC = 3
C
      PTR(1) = 0
      PTR(2) = 1
      PTR(3) = 2
      PTR(4) = 3
C
      A(2,1) = 1.D0/2.D0
      A(3,1) = 0.D0
      A(3,2) = 3.D0/4.D0
      A(4,1) = 2.D0/9.D0
      A(4,2) = 1.D0/3.D0
      A(4,3) = 4.D0/9.D0
C
C  The coefficients BHAT(*) refer to the formula used to advance the
C  integration, here the one of order 3.  The coefficients B(*) refer
C  to the other formula, here the one of order 2. For this pair, BHAT(*)
C  is not needed since FSAL = .TRUE.
C
      B(1) = 7.D0/24.D0
      B(2) = 1.D0/4.D0
      B(3) = 1.D0/3.D0
      B(4) = 1.D0/8.D0
C
      C(1) = 0.D0
      C(2) = 1.D0/2.D0
      C(3) = 3.D0/4.D0
      C(4) = 1.D0
C
      GO TO 120
C
C  METHD = 2
C    This pair is from "An Efficient Runge-Kutta (4,5) Pair" by P. Bogacki
C    and L.F. Shampine, Rept. 89-20, Math. Dept., Southern Methodist
C    University, Dallas, Texas, USA, 1989.  The authors are grateful to
C    P. Bogacki for his assistance in implementing the pair.  Shampine and
C    Bogacki subsequently modified the formula to enhance the reliability of
C    the pair.  The original fourth order formula is used in an estimate of
C    the local error.  If the step fails, the computation is broken off.  If
C    the step is acceptable, the first evaluation of the next step is done,
C    i.e., the pair is implemented as FSAL and the local error of the step
C    is again estimated with a fourth order formula using the additional data.
C    The step must succeed with both estimators to be accepted.  When the
C    second estimate is formed, it is used for the subsequent adjustment of
C    the step size because it is of higher quality.  The two fourth order
C    formulas are well matched to leading order, and only exceptionally do
C    the estimators disagree -- problems with discontinuous coefficients are
C    handled more reliably by using two estimators as is global error
C    estimation.
C
   40 CONTINUE
      NSTAGE = 8
      FSAL = .TRUE.
      ORDER = 4
      TANANG = 5.2D0
      STBRAD = 3.9D0
      SAFETY = 0.8D0
      INTP = .TRUE.
      REQSTG = .TRUE.
      MINTP = 6
      LINTPL = 6
      NSEC = 2
C
      PTR(1) = 0
      PTR(2) = 1
      PTR(3) = 2
      PTR(4) = 3
      PTR(5) = 4
      PTR(6) = 5
      PTR(7) = 6
      PTR(8) = 7
C
      A(2,1) = 1.D0/6.D0
      A(3,1) = 2.D0/27.D0
      A(3,2) = 4.D0/27.D0
      A(4,1) = 183.D0/1372.D0
      A(4,2) = -162.D0/343.D0
      A(4,3) = 1053.D0/1372.D0
      A(5,1) = 68.D0/297.D0
      A(5,2) = -4.D0/11.D0
      A(5,3) = 42.D0/143.D0
      A(5,4) = 1960.D0/3861.D0
      A(6,1) = 597.D0/22528.D0
      A(6,2) = 81.D0/352.D0
      A(6,3) = 63099.D0/585728.D0
      A(6,4) = 58653.D0/366080.D0
      A(6,5) = 4617.D0/20480.D0
      A(7,1) = 174197.D0/959244.D0
      A(7,2) = -30942.D0/79937.D0
      A(7,3) = 8152137.D0/19744439.D0
      A(7,4) = 666106.D0/1039181.D0
      A(7,5) = -29421.D0/29068.D0
      A(7,6) = 482048.D0/414219.D0
      A(8,1) = 587.D0/8064.D0
      A(8,2) = 0.D0
      A(8,3) = 4440339.D0/15491840.D0
      A(8,4) = 24353.D0/124800.D0
      A(8,5) = 387.D0/44800.D0
      A(8,6) = 2152.D0/5985.D0
      A(8,7) = 7267.D0/94080.D0
C
C  The coefficients B(*) refer to the formula of order 4.
C
      B(1) = 2479.D0/34992.D0
      B(2) = 0.D0
      B(3) = 123.D0/416.D0
      B(4) = 612941.D0/3411720.D0
      B(5) = 43.D0/1440.D0
      B(6) = 2272.D0/6561.D0
      B(7) = 79937.D0/1113912.D0
      B(8) = 3293.D0/556956.D0
C
C  The coefficients E(*) refer to an estimate of the local error based on
C  the first formula of order 4.  It is the difference of the fifth order
C  result, here located in A(8,*), and the fourth order result.  By
C  construction both E(2) and E(7) are zero.
C
      E(1) = -3.D0/1280.D0
      E(2) = 0.D0
      E(3) = 6561.D0/632320.D0
      E(4) = -343.D0/20800.D0
      E(5) = 243.D0/12800.D0
      E(6) = -1.D0/95.D0
      E(7) = 0.D0
C
      C(1) = 0.D0
      C(2) = 1.D0/6.D0
      C(3) = 2.D0/9.D0
      C(4) = 3.D0/7.D0
      C(5) = 2.D0/3.D0
      C(6) = 3.D0/4.D0
      C(7) = 1.D0
      C(8) = 1.D0
C
C  To do interpolation with this pair, some extra stages have to be computed.
C  The following additional A(*,*) and C(*) coefficients are for this purpose.
C  In addition there is an array R(*,*) that plays a role for interpolation
C  analogous to that of BHAT(*) for the basic step.
C
      C(9) = 1.D0/2.D0
      C(10) = 5.D0/6.D0
      C(11) = 1.D0/9.D0
C
      A(9,1) = 455.D0/6144.D0
      A(10,1) = -837888343715.D0/13176988637184.D0
      A(11,1) = 98719073263.D0/1551965184000.D0
      A(9,2) = 0.D0
      A(10,2) = 30409415.D0/52955362.D0
      A(11,2) = 1307.D0/123552.D0
      A(9,3) = 10256301.D0/35409920.D0
      A(10,3) = -48321525963.D0/759168069632.D0
      A(11,3) = 4632066559387.D0/70181753241600.D0
      A(9,4) = 2307361.D0/17971200.D0
      A(10,4) = 8530738453321.D0/197654829557760.D0
      A(11,4) = 7828594302389.D0/382182512025600.D0
      A(9,5) = -387.D0/102400.D0
      A(10,5) = 1361640523001.D0/1626788720640.D0
      A(11,5) = 40763687.D0/11070259200.D0
      A(9,6) = 73.D0/5130.D0
      A(10,6) = -13143060689.D0/38604458898.D0
      A(11,6) = 34872732407.D0/224610586200.D0
      A(9,7) = -7267.D0/215040.D0
      A(10,7) = 18700221969.D0/379584034816.D0
      A(11,7) = -2561897.D0/30105600.D0
      A(9,8) = 1.D0/32.D0
      A(10,8) = -5831595.D0/847285792.D0
      A(11,8) = 1.D0/10.D0
      A(10,9) = -5183640.D0/26477681.D0
      A(11,9) = -1.D0/10.D0
      A(11,10) = -1403317093.D0/11371610250.D0
C
      DO 60 I = 1, 11
         R(I,1) = 0.D0
   60 CONTINUE
      DO 80 I = 1, 6
         R(2,I) = 0.D0
   80 CONTINUE
      R(1,6) = -12134338393.D0/1050809760.D0
      R(1,5) = -1620741229.D0/50038560.D0
      R(1,4) = -2048058893.D0/59875200.D0
      R(1,3) = -87098480009.D0/5254048800.D0
      R(1,2) = -11513270273.D0/3502699200.D0
C
      R(3,6) = -33197340367.D0/1218433216.D0
      R(3,5) = -539868024987.D0/6092166080.D0
      R(3,4) = -39991188681.D0/374902528.D0
      R(3,3) = -69509738227.D0/1218433216.D0
      R(3,2) = -29327744613.D0/2436866432.D0
C
      R(4,6) = -284800997201.D0/19905339168.D0
      R(4,5) = -7896875450471.D0/165877826400.D0
      R(4,4) = -333945812879.D0/5671036800.D0
      R(4,3) = -16209923456237.D0/497633479200.D0
      R(4,2) = -2382590741699.D0/331755652800.D0
C
      R(5,6) = -540919.D0/741312.D0
      R(5,5) = -103626067.D0/43243200.D0
      R(5,4) = -633779.D0/211200.D0
      R(5,3) = -32406787.D0/18532800.D0
      R(5,2) = -36591193.D0/86486400.D0
C
      R(6,6) = 7157998304.D0/374350977.D0
      R(6,5) = 30405842464.D0/623918295.D0
      R(6,4) = 183022264.D0/5332635.D0
      R(6,3) = -3357024032.D0/1871754885.D0
      R(6,2) = -611586736.D0/89131185.D0
C
      R(7,6) = -138073.D0/9408.D0
      R(7,5) = -719433.D0/15680.D0
      R(7,4) = -1620541.D0/31360.D0
      R(7,3) = -385151.D0/15680.D0
      R(7,2) = -65403.D0/15680.D0
C
      R(8,6) = 1245.D0/64.D0
      R(8,5) = 3991.D0/64.D0
      R(8,4) = 4715.D0/64.D0
      R(8,3) = 2501.D0/64.D0
      R(8,2) = 149.D0/16.D0
      R(8,1) = 1.D0
C
      R(9,6) = 55.D0/3.D0
      R(9,5) = 71.D0
      R(9,4) = 103.D0
      R(9,3) = 199.D0/3.D0
      R(9,2) = 16.0D0
C
      R(10,6) = -1774004627.D0/75810735.D0
      R(10,5) = -1774004627.D0/25270245.D0
      R(10,4) = -26477681.D0/359975.D0
      R(10,3) = -11411880511.D0/379053675.D0
      R(10,2) = -423642896.D0/126351225.D0
C
      R(11,6) = 35.D0
      R(11,5) = 105.D0
      R(11,4) = 117.D0
      R(11,3) = 59.D0
      R(11,2) = 12.D0
C
      GO TO 120
C
C  METHD = 3
C    This pair is from "High Order Embedded Runge-Kutta Formulae" by P.J.
C    Prince and J.R. Dormand, J. Comp. Appl. Math.,7, pp. 67-75, 1981.  The
C    authors are grateful to P. Prince and J. Dormand for their assistance in
C    implementing the pair.
C
  100 CONTINUE
      NSTAGE = 13
      FSAL = .FALSE.
      ORDER = 7
      TANANG = 11.0D0
      STBRAD = 5.2D0
      SAFETY = 0.8D0
      INTP = .FALSE.
      REQSTG = .FALSE.
      MINTP = 0
      LINTPL = 0
      NSEC = 2
C
      PTR(1) = 0
      PTR(2) = 1
      PTR(3) = 2
      PTR(4) = 1
      PTR(5) = 3
      PTR(6) = 2
      PTR(7) = 4
      PTR(8) = 5
      PTR(9) = 6
      PTR(10) = 7
      PTR(11) = 8
      PTR(12) = 9
      PTR(13) = 1
C
      A(2,1) = 5.55555555555555555555555555556D-2
      A(3,1) = 2.08333333333333333333333333333D-2
      A(3,2) = 6.25D-2
      A(4,1) = 3.125D-2
      A(4,2) = 0.D0
      A(4,3) = 9.375D-2
      A(5,1) = 3.125D-1
      A(5,2) = 0.D0
      A(5,3) = -1.171875D0
      A(5,4) = 1.171875D0
      A(6,1) = 3.75D-2
      A(6,2) = 0.D0
      A(6,3) = 0.D0
      A(6,4) = 1.875D-1
      A(6,5) = 1.5D-1
      A(7,1) = 4.79101371111111111111111111111D-2
      A(7,2) = 0.D0
      A(7,3) = 0.0D0
      A(7,4) = 1.12248712777777777777777777778D-1
      A(7,5) = -2.55056737777777777777777777778D-2
      A(7,6) = 1.28468238888888888888888888889D-2
      A(8,1) = 1.6917989787292281181431107136D-2
      A(8,2) = 0.D0
      A(8,3) = 0.D0
      A(8,4) = 3.87848278486043169526545744159D-1
      A(8,5) = 3.59773698515003278967008896348D-2
      A(8,6) = 1.96970214215666060156715256072D-1
      A(8,7) = -1.72713852340501838761392997002D-1
      A(9,1) = 6.90957533591923006485645489846D-2
      A(9,2) = 0.D0
      A(9,3) = 0.D0
      A(9,4) = -6.34247976728854151882807874972D-1
      A(9,5) = -1.61197575224604080366876923982D-1
      A(9,6) = 1.38650309458825255419866950133D-1
      A(9,7) = 9.4092861403575626972423968413D-1
      A(9,8) = 2.11636326481943981855372117132D-1
      A(10,1) = 1.83556996839045385489806023537D-1
      A(10,2) = 0.D0
      A(10,3) = 0.D0
      A(10,4) = -2.46876808431559245274431575997D0
      A(10,5) = -2.91286887816300456388002572804D-1
      A(10,6) = -2.6473020233117375688439799466D-2
      A(10,7) = 2.84783876419280044916451825422D0
      A(10,8) = 2.81387331469849792539403641827D-1
      A(10,9) = 1.23744899863314657627030212664D-1
      A(11,1) = -1.21542481739588805916051052503D0
      A(11,2) = 0.D0
      A(11,3) = 0.D0
      A(11,4) = 1.66726086659457724322804132886D1
      A(11,5) = 9.15741828416817960595718650451D-1
      A(11,6) = -6.05660580435747094755450554309D0
      A(11,7) = -1.60035735941561781118417064101D1
      A(11,8) = 1.4849303086297662557545391898D1
      A(11,9) = -1.33715757352898493182930413962D1
      A(11,10) = 5.13418264817963793317325361166D0
      A(12,1) = 2.58860916438264283815730932232D-1
      A(12,2) = 0.D0
      A(12,3) = 0.D0
      A(12,4) = -4.77448578548920511231011750971D0
      A(12,5) = -4.3509301377703250944070041181D-1
      A(12,6) = -3.04948333207224150956051286631D0
      A(12,7) = 5.57792003993609911742367663447D0
      A(12,8) = 6.15583158986104009733868912669D0
      A(12,9) = -5.06210458673693837007740643391D0
      A(12,10) = 2.19392617318067906127491429047D0
      A(12,11) = 1.34627998659334941535726237887D-1
      A(13,1) = 8.22427599626507477963168204773D-1
      A(13,2) = 0.D0
      A(13,3) = 0.D0
      A(13,4) = -1.16586732572776642839765530355D1
      A(13,5) = -7.57622116690936195881116154088D-1
      A(13,6) = 7.13973588159581527978269282765D-1
      A(13,7) = 1.20757749868900567395661704486D1
      A(13,8) = -2.12765911392040265639082085897D0
      A(13,9) = 1.99016620704895541832807169835D0
      A(13,10) = -2.34286471544040292660294691857D-1
      A(13,11) = 1.7589857770794226507310510589D-1
      A(13,12) = 0.D0
C
C  The coefficients BHAT(*) refer to the formula used to advance the
C  integration, here the one of order 8.  The coefficients B(*) refer
C  to the other formula, here the one of order 7.
C
      BHAT(1) = 4.17474911415302462220859284685D-2
      BHAT(2) = 0.D0
      BHAT(3) = 0.D0
      BHAT(4) = 0.D0
      BHAT(5) = 0.D0
      BHAT(6) = -5.54523286112393089615218946547D-2
      BHAT(7) = 2.39312807201180097046747354249D-1
      BHAT(8) = 7.0351066940344302305804641089D-1
      BHAT(9) = -7.59759613814460929884487677085D-1
      BHAT(10) = 6.60563030922286341461378594838D-1
      BHAT(11) = 1.58187482510123335529614838601D-1
      BHAT(12) = -2.38109538752862804471863555306D-1
      BHAT(13) = 2.5D-1
C
      B(1) = 2.9553213676353496981964883112D-2
      B(2) = 0.D0
      B(3) = 0.D0
      B(4) = 0.D0
      B(5) = 0.D0
      B(6) = -8.28606276487797039766805612689D-1
      B(7) = 3.11240900051118327929913751627D-1
      B(8) = 2.46734519059988698196468570407D0
      B(9) = -2.54694165184190873912738007542D0
      B(10) = 1.44354858367677524030187495069D0
      B(11) = 7.94155958811272872713019541622D-2
      B(12) = 4.44444444444444444444444444445D-2
      B(13) = 0.D0
C
      C(1) = 0.D0
      C(2) = 5.55555555555555555555555555556D-2
      C(3) = 8.33333333333333333333333333334D-2
      C(4) = 1.25D-1
      C(5) = 3.125D-1
      C(6) = 3.75D-1
      C(7) = 1.475D-1
      C(8) = 4.65D-1
      C(9) = 5.64865451382259575398358501426D-1
      C(10) = 6.5D-1
      C(11) = 9.24656277640504446745013574318D-1
      C(12) = 1.D0
      C(13) = C(12)
C
      GO TO 120
C
C  The definitions of all pairs come here for the calculation of
C  LSTSTG, RS1, RS2, RS3, RS4, COST, MAXTRY, EXPON, TOOSML, and VECSTG.
C
  120 CONTINUE
      LSTSTG = PTR(NSTAGE)
      IF (FSAL) THEN
         COST = DBLE(NSTAGE-1)
      ELSE
         COST = DBLE(NSTAGE)
      END IF
C
C  MAXTRY - limit on the number of iterations of a computation made in
C  diagnosing stiffness.  There are at most Q = 3 function calls per
C  iteration. MAXTRY is determined so that  Q*MAXTRY <= 5% of the cost of
C  50 steps and 1 <= MAXTRY <= 8. This limits the number of calls to FCN
C  in each diagnosis of stiffness to 24 calls.
C
      MAXTRY = MIN(8,MAX(1,INT(FIVEPC*COST*FIFTY)))
C
      EXPON = ONE/(ORDER+ONE)
C
C     In calculating CDIFF it is assumed that there will be a non-zero
C     difference |C(I) - C(J)| less than one. If C(I) = C(J) for any I not
C     equal to J, they should be made precisely equal by assignment.
C
      CDIFF = ONE
      DO 160 I = 1, NSTAGE - 1
         DO 140 J = I + 1, NSTAGE
            DIFF = ABS(C(I)-C(J))
            IF (DIFF.NE.ZERO) CDIFF = MIN(CDIFF,DIFF)
  140    CONTINUE
  160 CONTINUE
      TOOSML = RNDOFF/CDIFF
C
C  Determine the number of columns needed in STAGES(1:NEQ,*) (this will be
C  at most NSTAGE-1 since the first stage is held in a separate array).
C  The PTR array contains the column positions of the stages.
C
      VECSTG = 0
      DO 180 I = 2, NSTAGE
         VECSTG = MAX(PTR(I),VECSTG)
  180 CONTINUE
C
      RS = TWO
      RS1 = ONE/RS
      RS2 = RS**2
      RS3 = RS**3
      RS4 = ONE/RS3
C
      RETURN
      END
      SUBROUTINE FORMI(F,NEQ,NWANT,Y,YP,YOLD,YPOLD,STAGES,CALSTG,
     &                 XSTAGE,P)
C************************************************
C**** NOT A DESIGNATED USER-CALLABLE ROUTINE ****
C************************************************
C
C  Purpose:    Forms an interpolating polynomial for use with
C              METHDs 1 or 2.
C
C  Input:      NEQ, NWANT, T, Y(*), YP(*), HOLD, YOLD(*), YPOLD(*),
C              STAGES(NEQ,*), CALSTG
C  Output:     P(*), XSTAGE(NEQ)
C  External:   F
C
C  Common:     Initializes:    none
C              Reads:          /RKCOM4/ A(*,*), C(*), R(*), METHD, MINTP
C                              /RKCOM2/ T, TOLD, HOLD
C              Alters:         /RKCOM2/ NFCN
C
C  Comments:
C  =========
C  The integration has reached T with a step HOLD from TOLD = T-HOLD.
C  Y(*),YP(*) and YOLD(*),YPOLD(*) approximate the solution and its
C  derivative at T and TOLD respectively.  STAGES(NEQ,*) holds the stages
C  computed in taking this step. In the case of METHD = 2 it is necessary 
C  to compute some more stages in this subroutine. CALSTG indicates whether
C  or not the extra stages need to be computed. A(*,*) and C(*) are used in 
C  computing these stages. The extra stages are stored in STAGES(NEQ,*) and 
C  XSTAGE(*).  The coefficients of the interpolating polynomials for the first
C  NWANT components of the solution are returned in the array P(*). The 
C  polynomial is of degree MINTP = 3 for METHD = 1 and of degree MINTP = 6 
C  for METHD = 2. The vector R(*) is used for workspace when METHD = 2.
C
C     .. Scalar Arguments ..
      INTEGER           NEQ, NWANT
      LOGICAL           CALSTG
C     .. Array Arguments ..
      DOUBLE PRECISION  P(*), STAGES(NEQ,*), XSTAGE(*), Y(*), YOLD(*),
     &                  YP(*), YPOLD(*)
C     .. Subroutine Arguments ..
      EXTERNAL          F
C     .. Common Block to hold Problem Status ..
      DOUBLE PRECISION  T, H, TOLD, HOLD
      INTEGER           NFCN, SVNFCN, OKSTP, FLSTP
      LOGICAL           FIRST, LAST
      COMMON /RKCOM2/   T, H, TOLD, HOLD, NFCN, SVNFCN, OKSTP, FLSTP,
     &                  FIRST, LAST
      SAVE   /RKCOM2/
C     .. Common Block to hold Formula Definitions ..
      DOUBLE PRECISION  A(13,13), B(13), C(13), BHAT(13), R(11,6),
     &                  E(7)
      INTEGER           PTR(13), NSTAGE, METHD, MINTP
      LOGICAL           INTP
      COMMON /RKCOM4/   A, B, C, BHAT, R, E, PTR, NSTAGE, METHD,
     &                  MINTP, INTP
      SAVE   /RKCOM4/
C     .. Local Scalars ..
      DOUBLE PRECISION  D1, D2, D3, D4, HYP, HYPOLD
      INTEGER           I, J, K, L
C     .. Executable Statements ..
C
      IF (METHD.EQ.1) THEN
C
C  METHD = 1.  Use the cubic Hermite interpolant that is is fully
C  specified by the values and slopes at the two ends of the step.
C
         DO 20 L = 1, NWANT
            D1 = Y(L) - YOLD(L)
            HYP = HOLD*YP(L)
            HYPOLD = HOLD*YPOLD(L)
            D2 = HYP - D1
            D3 = D1 - HYPOLD
            D4 = D2 - D3
            P(L) = D2 + D4
            P(NWANT+L) = D4
   20    CONTINUE
C
      ELSE
C
C  METHD = 2.
C
         IF (CALSTG) THEN
C
C  Compute the extra stages needed for interpolation using the facts that
C       1. Stage 1 is YPOLD(*).
C       2. Stage i (i>1) is stored in STAGES(1:NEQ,i).
C       3. This pair is FSAL, i.e. STAGES(1:NEQ,7)=YP(1:NEQ), which frees
C          up STAGES(1:NEQ,7) for use by stage 9.
C       4. XSTAGE(1:NEQ) is used for stage 10.
C       5. The coefficient of stage 2 in the interpolant is always 0, so
C          STAGES(1:NEQ,1) is used for stage 11.
C  The vector P(1:NEQ) is used as workspace for computing the stages.
C
            DO 180 I = 9, 11
               DO 40 L = 1, NEQ
                  P(L) = A(I,1)*YPOLD(L)
   40          CONTINUE
               DO 140 J = 2, I - 1
                  IF (J.LE.7) THEN
                     DO 60 L = 1, NEQ
                        P(L) = P(L) + A(I,J)*STAGES(L,J-1)
   60                CONTINUE
                  ELSE IF (J.EQ.8) THEN
                     DO 80 L = 1, NEQ
                        P(L) = P(L) + A(I,J)*YP(L)
   80                CONTINUE
                  ELSE IF (J.EQ.9) THEN
                     DO 100 L = 1, NEQ
                        P(L) = P(L) + A(I,J)*STAGES(L,7)
  100                CONTINUE
                  ELSE IF (J.EQ.10) THEN
                     DO 120 L = 1, NEQ
                        P(L) = P(L) + A(I,J)*XSTAGE(L)
  120                CONTINUE
                  END IF
  140          CONTINUE
               DO 160 L = 1, NEQ
                  P(L) = YOLD(L) + HOLD*P(L)
  160          CONTINUE
               IF (I.EQ.9) THEN
                  CALL F(TOLD+C(I)*HOLD,P,STAGES(1,7))
                  NFCN = NFCN + 1
               ELSE IF (I.EQ.10) THEN
                  CALL F(TOLD+C(I)*HOLD,P,XSTAGE)
                  NFCN = NFCN + 1
               ELSE
                  CALL F(TOLD+C(I)*HOLD,P,STAGES(1,1))
                  NFCN = NFCN + 1
               END IF
  180       CONTINUE
         END IF
C
C  Form the coefficients of the interpolating polynomial in its shifted
C  and scaled form.  The transformation from the form in which the
C  polynomial is derived can be somewhat ill-conditioned.  The terms 
C  are grouped so as to minimize the errors of the transformation.
C
C  Coefficient of SIGMA**6
         K = 4*NWANT
         DO 200 L = 1, NWANT
            P(K+L) = R(5,6)*STAGES(L,4) +
     &               ((R(10,6)*XSTAGE(L)+R(8,6)*YP(L))+
     &               (R(7,6)*STAGES(L,6)+R(6,6)*STAGES(L,5))) +
     &               ((R(4,6)*STAGES(L,3)+R(9,6)*STAGES(L,7))+
     &               (R(3,6)*STAGES(L,2)+R(11,6)*STAGES(L,1))+
     &               R(1,6)*YPOLD(L))
  200    CONTINUE
C
C  Coefficient of SIGMA**5
         K = 3*NWANT
         DO 220 L = 1, NWANT
            P(K+L) = (R(10,5)*XSTAGE(L)+R(9,5)*STAGES(L,7)) +
     &               ((R(7,5)*STAGES(L,6)+R(6,5)*STAGES(L,5))+
     &               R(5,5)*STAGES(L,4)) + ((R(4,5)*STAGES(L,3)+
     &               R(8,5)*YP(L))+(R(3,5)*STAGES(L,2)+R(11,5)*
     &               STAGES(L,1))+R(1,5)*YPOLD(L))
  220    CONTINUE
C
C  Coefficient of SIGMA**4
         K = 2*NWANT
         DO 240 L = 1, NWANT
            P(K+L) = ((R(4,4)*STAGES(L,3)+R(8,4)*YP(L))+
     &               (R(7,4)*STAGES(L,6)+R(6,4)*STAGES(L,5))+
     &               R(5,4)*STAGES(L,4)) + ((R(10,4)*XSTAGE(L)+
     &               R(9,4)*STAGES(L,7))+(R(3,4)*STAGES(L,2)+
     &               R(11,4)*STAGES(L,1))+R(1,4)*YPOLD(L))
  240    CONTINUE
C
C  Coefficient of SIGMA**3
         K = NWANT
         DO 260 L = 1, NWANT
            P(K+L) = R(5,3)*STAGES(L,4) + R(6,3)*STAGES(L,5) +
     &               ((R(3,3)*STAGES(L,2)+R(9,3)*STAGES(L,7))+
     &               (R(10,3)*XSTAGE(L)+R(8,3)*YP(L))+R(1,3)*
     &               YPOLD(L))+((R(4,3)*STAGES(L,3)+R(11,3)*
     &               STAGES(L,1))+R(7,3)*STAGES(L,6))
  260    CONTINUE
C
C  Coefficient of SIGMA**2
C
         DO 280 L = 1, NWANT
            P(L) = R(5,2)*STAGES(L,4) + ((R(6,2)*STAGES(L,5)+
     &             R(8,2)*YP(L))+R(1,2)*YPOLD(L)) +
     &             ((R(3,2)*STAGES(L,2)+R(9,2)*STAGES(L,7))+
     &             R(10,2)*XSTAGE(L)) + ((R(4,2)*STAGES(L,3)+
     &             R(11,2)*STAGES(L,1))+R(7,2)*STAGES(L,6))
  280    CONTINUE
C
C  Scale all the coefficients by the step size.
C
         DO 300 L = 1, NWANT*(MINTP-1)
            P(L) = HOLD*P(L)
  300    CONTINUE
C
      END IF
C
      RETURN
      END
      SUBROUTINE EVALI(Y,YP,P,TWANT,REQEST,NWANT,YWANT,YPWANT)
C************************************************
C**** NOT A DESIGNATED USER-CALLABLE ROUTINE ****
*************************************************
C
C  Purpose:    Evaluation of an interpolating polynomial and/or its
C              first derivative.
C
C  Input:      Y(*), YP(*), P(NWANT,*), TWANT, REQEST, NWANT
C  Output:     YWANT(*), YPWANT(*)
C
C  Common:     Initializes:    none
C              Reads:          /RKCOM2/ HOLD, T
C                              /RKCOM4/ MINTP
C              Alters:         none
C
C  Comments:
C  =========
C  The interpolant is evaluated at TWANT to approximate the solution,
C  YWANT, and/or its first derivative there, YPWANT. Only the first
C  NWANT components of the answer are computed. There are three cases
C  that are indicated by the first character of REQEST:
C    REQEST(1:1) = `S' or `s'- compute approximate `S'olution only.
C                = `D' or `d'- compute approximate first `D'erivative
C                              of the solution only.
C                = `B' or `b'- compute `B'oth approximate solution and
C                              first derivative.
C  The coefficents of the polynomial are contained in Y(*), YP(*) and
C  P(NWANT,*).
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION  TWANT
      INTEGER           NWANT
      CHARACTER*(*)     REQEST
C     .. Array Arguments ..
      DOUBLE PRECISION  P(NWANT,*), Y(*), YP(*), YPWANT(*), YWANT(*)
C     .. Common Block to hold Problem Status ..
      DOUBLE PRECISION  T, H, TOLD, HOLD
      INTEGER           NFCN, SVNFCN, OKSTP, FLSTP
      LOGICAL           FIRST, LAST
      COMMON /RKCOM2/   T, H, TOLD, HOLD, NFCN, SVNFCN, OKSTP, FLSTP,
     &                  FIRST, LAST
      SAVE   /RKCOM2/
C     .. Common Block to hold Formula Definitions ..
      DOUBLE PRECISION  A(13,13), B(13), C(13), BHAT(13), R(11,6),
     &                  E(7)
      INTEGER           PTR(13), NSTAGE, METHD, MINTP
      LOGICAL           INTP
      COMMON /RKCOM4/   A, B, C, BHAT, R, E, PTR, NSTAGE, METHD,
     &                  MINTP, INTP
      SAVE   /RKCOM4/
C     .. Local Scalars ..
      DOUBLE PRECISION  SIGMA
      INTEGER           K, L
      CHARACTER         REQST1
C     .. Executable Statements ..
C
C  Evaluate the interpolating polynomial of degree MINTP in terms of the
C  shifted and scaled independent variable SIGMA.
C
      SIGMA = (TWANT-T)/HOLD
C
      REQST1 = REQEST(1:1)
      IF (REQST1.EQ.'S' .OR. REQST1.EQ.'s' .OR. 
     &    REQST1.EQ.'B' .OR. REQST1.EQ.'b') THEN
C
         DO 20 L = 1, NWANT
            YWANT(L) = P(L,MINTP-1)*SIGMA
   20    CONTINUE
         DO 60 K = MINTP - 2, 1, -1
            DO 40 L = 1, NWANT
               YWANT(L) = (YWANT(L)+P(L,K))*SIGMA
   40       CONTINUE
   60    CONTINUE
         DO 80 L = 1, NWANT
            YWANT(L) = (YWANT(L)+HOLD*YP(L))*SIGMA + Y(L)
   80    CONTINUE
      END IF
C
C  Evaluate the derivative of the interpolating polynomial.
C
      IF (REQST1.EQ.'D' .OR. REQST1.EQ.'d' .OR. 
     &    REQST1.EQ.'B' .OR. REQST1.EQ.'b') THEN
C
C  The derivative of the interpolating polynomial with respect to TWANT 
C  is the derivative with respect to S divided by HOLD.
C
         DO 100 L = 1, NWANT
            YPWANT(L) = MINTP*P(L,MINTP-1)*SIGMA
  100    CONTINUE
         DO 140 K = MINTP - 1, 2, -1
            DO 120 L = 1, NWANT
               YPWANT(L) = (YPWANT(L)+K*P(L,K-1))*SIGMA
  120       CONTINUE
  140    CONTINUE
         DO 160 L = 1, NWANT
            YPWANT(L) = (YPWANT(L)+HOLD*YP(L))/HOLD
  160    CONTINUE
      END IF
C
      RETURN
      END
      SUBROUTINE RKMSG(IER,SRNAME,NREC,FLAG)
C************************************************
C**** NOT A DESIGNATED USER-CALLABLE ROUTINE ****
C************************************************
C
C  Purpose:      To process error messages and terminate the program
C                in the event of a "catastrophic" failure.
C
C  Input:        IER, SRNAME, NREC
C  Output:       FLAG
C
C  Common:       Initializes:    none
C                Reads:          /RKCOM7/ OUTCH
C                                /RKCOM8/ MSG, UTASK
C                                /RKCOM9/ REC
C                Alters:         none
C
C  Comments:
C  =========
C  The output variable FLAG is assigned the value of the input variable IER.
C
C  IER = -2  reports a successful call of the subroutine SRNAME and
C            indicates that special action is to be taken elsewhere
C            in the suite.  FLAG is set and a return is effected.
C
C  IER = 1   reports a successful call of the subroutine SRNAME.  FLAG
C            is set and a return is effected.
C
C  1 < IER < 911 and MSG = .TRUE.: a message of NREC records contained in
C            the array REC(*) is written to the standard output channel, 
C            OUTCH.  FLAG is set and a return is effected.
C
C  IER = 911 reports a "catastrophic" error was detected in SRNAME.  A
C            message is written to OUTCH regardless of the value of MSG and
C            normally the execution of the program is terminated.  The
C            execution is not terminated if the error is the result of an
C            indirect call to CT, RESET, or INTRP through UT (UTASK = .TRUE.).
C            Termination can be prevented by using the subroutine SOFTFL.
C
C  IER = 912 reports that a "catastrophic" error was detected earlier and
C            termination was prevented, but the user has failed to take
C            appropriate remedial action.  Execution is terminated.
C
C     .. Scalar Arguments ..
      INTEGER           FLAG, IER, NREC
      CHARACTER*(*)     SRNAME
C     .. Common Block for Environment Parameters ..
      DOUBLE PRECISION  MCHEPS, DWARF, RNDOFF, SQRRMC, CUBRMC, TINY
      INTEGER           OUTCH
      COMMON /RKCOM7/   MCHEPS, DWARF, RNDOFF, SQRRMC, CUBRMC, TINY,
     &                  OUTCH
      SAVE   /RKCOM7/
C     .. Common Block for Integrator Options ..
      LOGICAL           MSG, UTASK
      COMMON /RKCOM8/   MSG, UTASK
      SAVE   /RKCOM8/
C     .. Common Block for Error Message ..
      CHARACTER*80      REC(10)
      COMMON /RKCOM9/   REC
      SAVE   /RKCOM9/
C     .. Parameters ..
      INTEGER           PLUS1
      LOGICAL           ASK, TELL
      PARAMETER         (PLUS1=1,ASK=.TRUE.,TELL=.FALSE.)
C     .. Local Scalars ..
      INTEGER           I
      LOGICAL           BADERR, OK, ON, UTCALL
C     .. External Subroutines ..
      EXTERNAL          CHKFL, RKSIT, SOFTFL
C     .. Executable Statements ..
C
C  Check where the call came from - if it is an indirect call from UT,
C  the run is not STOPped.
      UTCALL = (SRNAME.EQ.'RESET' .OR. SRNAME.EQ.'CT' .OR.
     &          SRNAME.EQ.'INTRP') .AND. UTASK
C
C  Check if can continue with integrator.
      OK = (SRNAME.EQ.'CT' .OR. SRNAME.EQ.'UT') .AND.
     &     (IER.EQ.2 .OR. IER.EQ.3 .OR. IER.EQ.4)
C
C  Check if program termination has been overridden.
      CALL SOFTFL(ASK,ON)
C
      IF ((MSG.AND.IER.GT.PLUS1) .OR. IER.GE.911) THEN
         WRITE (OUTCH,'(/A)') ' **'
         WRITE (OUTCH,'(A)') (REC(I),I=1,NREC)
         IF (IER.GE.911) THEN
            WRITE (OUTCH,'(A/A,A,A/A/)') 
     &' **',
     &' ** Catastrophic error detected in ', SRNAME, '.', 
     &' **'
            IF ((.NOT.(UTCALL.OR.ON).AND.IER.EQ.911) .OR.
     &          IER.EQ.912) THEN
               WRITE (OUTCH,'(A/A/A)') 
     &' **',
     &' ** Execution of your program is being terminated.',
     &' **'
               STOP
            END IF
         ELSE IF (OK) THEN
            WRITE (OUTCH,'(A/A,A,A,I2,A/A/A)') 
     &' **',
     &' ** Warning from routine ', SRNAME, ' with flag set ',IER, '.',
     &' ** You can continue integrating this problem.',
     &' **'
         ELSE
            WRITE (OUTCH,'(A/A,A,A,I2,A/A/A)') 
     &' **',
     &' ** Warning from routine ', SRNAME, ' with flag set ',IER, '.',
     &' ** You cannot continue integrating this problem.', 
     &' **'
         END IF
      END IF
      DO 20 I = NREC + 1, 10
         REC(I) = ' '
   20 CONTINUE
      FLAG = IER
C
C  TELL RKSIT the status of the routine associated with SRNAME
      CALL RKSIT(TELL,SRNAME,FLAG)
C
C  Indicate that a catastrophic error has been detected
      BADERR = FLAG .GE. 911
      CALL CHKFL(TELL,BADERR)
C
      RETURN
C
      END
      SUBROUTINE RKSIT(ASK,SRNAME,STATE)
C************************************************
C**** NOT A DESIGNATED USER-CALLABLE ROUTINE ****
C************************************************
C
C  Purpose:      To save or enquire about the status of each
C                subprogram in the suite.
C
C  Input:        ASK, SRNAME
C  Input/output: STATE
C
C
C  Comments:
C  =========
C  SRNAME indicates which routine is of interest in the call to RKSIT.
C
C  If ASK=.FALSE., then the value of STATE (which as used is the error
C  flag) for the routine SRNAME is saved internally. This value of STATE
C  is usually positive.  There are two exceptions:
C     1. SRNAME='SETUP' and STATE=-1 indicates a completely new problem,
C        so all SAVEd states are cleared.
C     2. STATE=-2 is used by some routines in the suite to indicate
C        circumstances which require special action.
C
C  If ASK=.TRUE., then RKSIT first checks to see if there were any
C  catastrophic errors, that is, a SAVEd state has value 911. This should
C  happen only when the user has overridden program termination in the event
C  of catastrophic failures from routines in the package but has failed to
C  take appropriate action. If this is the case, then RKSIT returns a value
C  of STATE = 911 which forces a termination of execution inside RKMSG. If 
C  no catastrophic errors are flagged, then STATE returns the saved state 
C  value for the routine specified by SRNAME.
C
C     .. Scalar Arguments ..
      INTEGER           STATE
      LOGICAL           ASK
      CHARACTER*(*)     SRNAME
C     .. Parameters ..
      INTEGER           STATES, MINUS1
      PARAMETER         (STATES=7,MINUS1=-1)
C     .. Local Scalars ..
      INTEGER           I, NAME
C     .. Local Arrays ..
      INTEGER           SVSTA(STATES)
C     .. Save statement ..
      SAVE              SVSTA
C     .. Data statements ..
      DATA              SVSTA/STATES*MINUS1/
C     .. Executable Statements ..
C
      IF (SRNAME.EQ.'SETUP') THEN
         NAME = 1
      ELSE IF (SRNAME.EQ.'UT') THEN
         NAME = 2
      ELSE IF (SRNAME.EQ.'STAT') THEN
         NAME = 3
      ELSE IF (SRNAME.EQ.'GLBERR') THEN
         NAME = 4
      ELSE IF (SRNAME.EQ.'CT') THEN
         NAME = 5
      ELSE IF (SRNAME.EQ.'INTRP') THEN
         NAME = 6
      ELSE IF (SRNAME.EQ.'RESET') THEN
         NAME = 7
      ELSE
         NAME = 0
      END IF
C
C  (Re)initialize if SETUP is telling RKSIT to do so.
      IF (.NOT.ASK .AND. NAME.EQ.1 .AND. STATE.EQ.MINUS1) THEN
         DO 20 I = 1, STATES
            SVSTA(I) = MINUS1
   20    CONTINUE
         GO TO 60
      END IF
C
C  Check for 911 on exit from a previous call.
      IF (ASK) THEN
         DO 40 I = 1, STATES
            IF (SVSTA(I).EQ.911) THEN
               STATE = 911
               GO TO 60
            END IF
   40    CONTINUE
      END IF
C
      IF (ASK) THEN
         STATE = SVSTA(NAME)
      ELSE
         SVSTA(NAME) = STATE
      END IF
C
   60 CONTINUE
C
      RETURN
      END
      SUBROUTINE TRUERR(F,NEQ,Y,TOL,WEIGHT,ZY,ZYP,ZERROR,ZYNEW,ZERRES,
     &                  ZSTAGE,IER)
C************************************************
C**** NOT A DESIGNATED USER-CALLABLE ROUTINE ****
C************************************************
C
C  Purpose:      Compute a running RMS measure of the true (global) error
C                for a general Runge-Kutta pair.
C
C
C  Input:        NEQ, Y(*), TOL, WEIGHT(*),
C  Input/output: ZY(*), ZYP(*), ZERROR(*)
C  Workspace:    ZYNEW(*), ZERRES(*), ZSTAGE(NEQ,*)
C  Output:       IER
C  External:     F
C
C  Common:       Initializes:    none
C                Reads:          /RKCOM2/ T, HOLD
C                                /RKCOM5/ TOOSML, ORDER, NSEC
C                                /RKCOM7/ TINY
C                Alters:         /RKCOM6/ MAXERR, LOCMAX, GNFCN
C
C  Comments:
C  =========
C  A secondary integration is performed using a fraction of the step size 
C  of the primary integration. ZY(*) and ZYP(*) are the approximate solution
C  and first derivative of this secondary integration. ZERRES(*) contains the 
C  error estimates for the secondary integration. ZYNEW(*) and ZSTAGE(*,*) are
C  workspace for taking a step. The error assessment is computed using the
C  difference of the primary and secondary solutions at the primary
C  integration points as an estimate of the true error there.  The weights 
C  used are those of the error test of the primary integration. This error 
C  assessment is maintained in the vector ZERROR(*).  MAXERR and LOCMAX 
C  contain the maximum contribution to the assessment and its location,
C  respectively.  The number of calls to F is counted by GNFCN.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION  TOL
      INTEGER           IER, NEQ
C     .. Array Arguments ..
      DOUBLE PRECISION  WEIGHT(*), Y(*), ZERRES(*), ZERROR(*),
     &                  ZSTAGE(NEQ,*), ZY(*), ZYNEW(*), ZYP(*)
C     .. Subroutine Arguments ..
      EXTERNAL          F
C     .. Common Block to hold Problem Status ..
      DOUBLE PRECISION  T, H, TOLD, HOLD
      INTEGER           NFCN, SVNFCN, OKSTP, FLSTP
      LOGICAL           FIRST, LAST
      COMMON /RKCOM2/   T, H, TOLD, HOLD, NFCN, SVNFCN, OKSTP, FLSTP,
     &                  FIRST, LAST
      SAVE   /RKCOM2/
C     .. Common Block to hold Formula Characterisitcs ..
      DOUBLE PRECISION  TOOSML, COST, SAFETY, EXPON, STBRAD, TANANG,
     &                  RS, RS1, RS2, RS3, RS4
      INTEGER           ORDER, LSTSTG, MAXTRY, NSEC
      LOGICAL           FSAL
      COMMON /RKCOM5/   TOOSML, COST, SAFETY, EXPON, STBRAD, TANANG,
     &                  RS, RS1, RS2, RS3, RS4, ORDER, LSTSTG, MAXTRY,
     &                  NSEC, FSAL
      SAVE   /RKCOM5/
C     .. Common Block for Global Error Assessment ..
      DOUBLE PRECISION  MAXERR, LOCMAX
      INTEGER           GNFCN, PRZSTG, PRZY, PRZYP, PRZERS, PRZERR,
     &                  PRZYNU
      LOGICAL           ERASON, ERASFL
      COMMON /RKCOM6/   MAXERR, LOCMAX, GNFCN, PRZSTG, PRZY, PRZYP,
     &                  PRZERS, PRZERR, PRZYNU, ERASON, ERASFL
      SAVE   /RKCOM6/
C     .. Common Block for Environment Parameters ..
      DOUBLE PRECISION  MCHEPS, DWARF, RNDOFF, SQRRMC, CUBRMC, TINY
      INTEGER           OUTCH
      COMMON /RKCOM7/   MCHEPS, DWARF, RNDOFF, SQRRMC, CUBRMC, TINY,
     &                  OUTCH
      SAVE   /RKCOM7/
C     .. Parameters ..
      DOUBLE PRECISION  PT1, TEN, DUMMY
      PARAMETER         (PT1=0.1D0,TEN=10.0D0,DUMMY=1.0D0)
C     .. Local Scalars ..
      DOUBLE PRECISION  DIFF, ERRMAX, HMIN, HSEC, MXERLC, TSEC, ZLERR,
     &                  ZTEST1, ZTEST2
      INTEGER           ISTEP, L, LEVEL
      LOGICAL           LDUMMY, MAIN
C     .. Local Arrays ..
      DOUBLE PRECISION  DUMARR(1)
C     .. External Subroutines ..
      EXTERNAL          STEP
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DBLE, MAX
C     .. Executable Statements ..
      TSEC = T - HOLD
      HSEC = HOLD/DBLE(NSEC)
      HMIN = MAX(TINY,TOOSML*MAX(ABS(TSEC),ABS(T)))
      IF (ABS(HSEC).LT.HMIN) THEN
         IER = 6
         GO TO 120
      END IF
      ZTEST1 = TOL/DBLE(NSEC)
      ZTEST2 = TOL/TEN
      LEVEL = 0
C
C  The subroutine STEP is used to take a step.  In its use in the primary
C  integration provision is made for getting on scale in the first step.
C  In this situation only the subroutine might reduce the step size.  By
C  setting MAIN = .FALSE., the subroutine will take a step of the size input.
C  In this use of the subroutine, all items of the call list appearing after
C  MAIN are dummy variables.
C
C  Perform secondary integration.
      MAIN = .FALSE.
      LDUMMY = .FALSE.
      DO 60 ISTEP = 1, NSEC
C
C  Take a step.
         CALL STEP(F,NEQ,TSEC,ZY,ZYP,ZSTAGE,ZTEST1,HSEC,WEIGHT,ZYNEW,
     &             ZERRES,ZLERR,MAIN,DUMMY,DUMARR,LDUMMY)
C
C  The primary integration is using a step size of HUSED and the secondary
C  integration is using the smaller step size HSEC = HUSED/NSEC.  If steps
C  of this size were taken from the same starting point and the asymptotic
C  behavior were evident, the smaller step size would result in a local error
C  that is considerably smaller, namely by a factor of 1/(NSEC**(ORDER+1)).
C  If the two approximate solutions are close and TOLR is neither too large nor
C  too small, this should be approximately true.  The step size is chosen in
C  the primary integration so that the local error ERR is no larger than TOLR.
C  The local error, ZLERR, of the secondary integration is compared to TOLR in
C  an attempt to diagnose a secondary integration that is not rather more
C  accurate than the primary integration.
C
         IF (ZLERR.GE.ZTEST1) THEN
            LEVEL = 2
         ELSE IF (ZLERR.GT.ZTEST2) THEN
            LEVEL = LEVEL + 1
         END IF
         IF (LEVEL.GE.2) THEN
            IER = 6
            GO TO 120
         END IF
C
C  Advance TSEC and the dependent variables ZY(*) and ZYP(*).
         TSEC = T - DBLE(NSEC-ISTEP)*HSEC
         DO 20 L = 1, NEQ
            ZY(L) = ZYNEW(L)
   20    CONTINUE
C
         IF (FSAL) THEN
C
C  When FSAL = .TRUE., the derivative ZYP(*) is the last stage of the step.
            DO 40 L = 1, NEQ
               ZYP(L) = ZSTAGE(L,LSTSTG)
   40       CONTINUE
         ELSE
C
C  Call F to evaluate ZYP(*).
            CALL F(TSEC,ZY,ZYP)
            GNFCN = GNFCN + 1
         END IF
C
   60 CONTINUE
C
C  Update the maximum error seen, MAXERR, and its location, LOCMAX.
C  Use local variables ERRMAX and MXERLC.
C
      ERRMAX = MAXERR
      MXERLC = LOCMAX
      DO 80 L = 1, NEQ
         DIFF = ABS(ZY(L)-Y(L))/WEIGHT(L)
         IF (DIFF.GT.ERRMAX) THEN
            ERRMAX = DIFF
            MXERLC = T
         END IF
   80 CONTINUE
C
C  If the global error is greater than 0.1D0, the solutions have diverged so
C  far that comparing them may not provide a reliable estimate of the global
C  error. The test is made before ZERROR(*) and MAXERR, LCMXER are updated so
C  that on a failure, they refer to the last reliable results.
C
      IF (ERRMAX.GT.PT1) THEN
         IER = 6
         GO TO 120
      ELSE
         MAXERR = ERRMAX
         LOCMAX = MXERLC
         DO 100 L = 1, NEQ
            DIFF = ABS(ZY(L)-Y(L))/WEIGHT(L)
            ZERROR(L) = ZERROR(L) + DIFF**2
  100    CONTINUE
         IER = 1
      END IF
C
C  Exit point for TRUERR
  120 CONTINUE
C
      RETURN
      END
      SUBROUTINE STEP(F,NEQ,TNOW,Y,YP,STAGES,TOL,HTRY,WEIGHT,YNEW,
     &                ERREST,ERR,MAIN,HMIN,THRES,PHASE2)
C************************************************
C**** NOT A DESIGNATED USER-CALLABLE ROUTINE ****
C************************************************
C
C  Purpose:      To compute a step of an explicit Runge-Kutta
C                method and estimate the local error of the step.
C
C  Input:        NEQ, TNOW, Y(*), YP(*), TOL, MAIN, HMIN, THRES(*)
C  Input/output: HTRY, PHASE2, LAST, WEIGHT(*)
C  Output:       STAGES(NEQ,*), YNEW(*), ERREST(*), ERR
C
C  Common:       Initializes:    none
C                Reads:          /RKCOM1/ TND
C                                /RKCOM2/ LAST
C                                /RKCOM4/ A, B, C, BHAT, PTR, NSTAGE, METHD
C                                /RKCOM5/ FSAL
C                Alters:         /RKCOM2/ NFCN, LAST
C                                /RKCOM6/ GNFCN
C
C  Comments:
C  =========
C  From an approximate solution Y(*) at TNOW and first derivative there,
C  YP(*) = F(TNOW,Y,YP), a step is taken to get an approximation YNEW(*)
C  at TNOW + HTRY. The Runge-Kutta method and how it is used are defined
C  by A, B, C, BHAT, PTR, NSTAGE, METHD and FSAL. Intermediate stages
C  of the method are stored in the array STAGES(NEQ,*). The error in
C  each solution component is estimated and returned in ERREST(*). A
C  weighted maximum norm of the local error, ERR, is formed. For some
C  methods an intermediate error estimate can be computed before completion
C  of the step (see routine STEPB); if the estimate is greater than the
C  specified tolerance TOL, the computation of the step is terminated.
C
C  When global error estimation is desired, two integrations are done.
C  The usual integration is referred to as the "primary", or "main",
C  integration (MAIN=.TRUE.).  For global error estimation another,
C  "secondary" integration (MAIN=.FALSE.) is carried out with a smaller
C  step size.  The weight vector WEIGHT(*) used in computing ERR is
C  determined by the main integration.  Thus this argument is output when
C  MAIN = .TRUE. and input when MAIN = .FALSE..
C
C  When taking the first step in an integration, the logical variable
C  PHASE2 may be input as .TRUE. and if the first step is the whole of
C  the range of integration, then LAST will be .TRUE.. When PHASE2=.TRUE.,
C  the first three stages are monitored to help assure that the step
C  size H is small enough for the integration to be stable and for the
C  estimate of the error of the step to be credible. Calls are made to
C  the subroutine STEPA for this purpose. If necessary, H will be
C  reduced in STEPA (and LAST altered accordingly) and the step retried
C  in STEP until an acceptable value is found.
C
C  In the primary integration the number of calls to F is counted by
C  NFCN, and in the secondary integration, by GNFCN.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION  ERR, HMIN, HTRY, TNOW, TOL
      INTEGER           NEQ
      LOGICAL           MAIN, PHASE2
C     .. Array Arguments ..
      DOUBLE PRECISION  ERREST(*), STAGES(NEQ,*), THRES(*), WEIGHT(*),
     &                  Y(*), YNEW(*), YP(*)
C     .. Subroutine Arguments ..
      EXTERNAL          F
C     .. Common Block for Problem Definition ..
      DOUBLE PRECISION  TSTRT, TND, DIR, HSTRT, TOLR
      INTEGER           NEQN
      COMMON /RKCOM1/   TSTRT, TND, DIR, HSTRT, TOLR, NEQN
      SAVE   /RKCOM1/
C     .. Common Block to hold Problem Status ..
      DOUBLE PRECISION  T, H, TOLD, HOLD
      INTEGER           NFCN, SVNFCN, OKSTP, FLSTP
      LOGICAL           FIRST, LAST
      COMMON /RKCOM2/   T, H, TOLD, HOLD, NFCN, SVNFCN, OKSTP, FLSTP,
     &                  FIRST, LAST
      SAVE   /RKCOM2/
C     .. Common Block to hold Formula Definitions ..
      DOUBLE PRECISION  A(13,13), B(13), C(13), BHAT(13), R(11,6),
     &                  E(7)
      INTEGER           PTR(13), NSTAGE, METHD, MINTP
      LOGICAL           INTP
      COMMON /RKCOM4/   A, B, C, BHAT, R, E, PTR, NSTAGE, METHD,
     &                  MINTP, INTP
      SAVE   /RKCOM4/
C     .. Common Block to hold Formula Characterisitcs ..
      DOUBLE PRECISION  TOOSML, COST, SAFETY, EXPON, STBRAD, TANANG,
     &                  RS, RS1, RS2, RS3, RS4
      INTEGER           ORDER, LSTSTG, MAXTRY, NSEC
      LOGICAL           FSAL
      COMMON /RKCOM5/   TOOSML, COST, SAFETY, EXPON, STBRAD, TANANG,
     &                  RS, RS1, RS2, RS3, RS4, ORDER, LSTSTG, MAXTRY,
     &                  NSEC, FSAL
      SAVE   /RKCOM5/
C     .. Common Block for Global Error Assessment ..
      DOUBLE PRECISION  MAXERR, LOCMAX
      INTEGER           GNFCN, PRZSTG, PRZY, PRZYP, PRZERS, PRZERR,
     &                  PRZYNU
      LOGICAL           ERASON, ERASFL
      COMMON /RKCOM6/   MAXERR, LOCMAX, GNFCN, PRZSTG, PRZY, PRZYP,
     &                  PRZERS, PRZERR, PRZYNU, ERASON, ERASFL
      SAVE   /RKCOM6/
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, HALF, ONE
      PARAMETER         (ZERO=0.0D0,HALF=0.5D0,ONE=1.0D0)
C     .. Local Scalars ..
      DOUBLE PRECISION  AVGY, TSTG
      INTEGER           I, J, L
      LOGICAL           CUTBAK
C     .. External Subroutines ..
      EXTERNAL          STEPA, STEPB
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, MAX, SIGN
C     .. Executable Statements ..
C
C  Many of the following loops over L = 1, NEQ have constant array values
C  inside. The code is written with clarity in mind.  Any optimizing
C  compiler will identify these occurrences and take appropriate action.
C  A check for zero multipliers has been included so as to prevent
C  needless computation resulting from the storing of zero coefficients
C  in the arrays for the sake of clarity.  The array ERREST(*) is used
C  for working storage in this computation.
C
   20 CONTINUE
      IF (MAIN) THEN
         IF (PHASE2) THEN
C
C  Initialize weights for measuring the local error.
            DO 40 L = 1, NEQ
               WEIGHT(L) = MAX(THRES(L),ABS(Y(L)))
   40       CONTINUE
         END IF
      END IF
C
      DO 140 I = 2, NSTAGE
         DO 100 J = 1, I - 1
            IF (J.EQ.1) THEN
               DO 60 L = 1, NEQ
                  ERREST(L) = A(I,1)*YP(L)
   60          CONTINUE
            ELSE
               IF (A(I,J).NE.ZERO) THEN
                  DO 80 L = 1, NEQ
                     ERREST(L) = ERREST(L) + A(I,J)*STAGES(L,PTR(J))
   80             CONTINUE
               END IF
            END IF
  100    CONTINUE
         DO 120 L = 1, NEQ
            YNEW(L) = Y(L) + HTRY*ERREST(L)
  120    CONTINUE
C
C  METHD = 2 is special in that an estimate of the local error can be
C  formed before the step is completed.  If the step is a failure,
C  return immediately.  Otherwise, complete the step and compute a more
C  accurate error estimate.
         IF (METHD.EQ.2 .AND. I.EQ.7) THEN
            CALL STEPB(NEQ,Y,YP,HTRY,YNEW,STAGES,THRES,ERR,MAIN,WEIGHT)
            IF (ERR.GT.TOL) RETURN
         END IF
C
         TSTG = TNOW + C(I)*HTRY
         IF (MAIN .AND. LAST .AND. C(I).EQ.ONE) TSTG = TND
         CALL F(TSTG,YNEW,STAGES(1,PTR(I)))
C
C  Increment the counter for the number of function evaluations
C  depending on whether the primary or secondary integration is taking
C  place.
         IF (MAIN) THEN
            NFCN = NFCN + 1
         ELSE
            GNFCN = GNFCN + 1
         END IF
C
C----------------------------------------------------------------------
C  When PHASE2 is .TRUE. we are in the second phase of the automatic
C  selection of the initial step size.  The results of the first three
C  stages are monitored in the subroutine STEPA for evidence that H is
C  too large -- instability and/or an unreliable estimate of the error
C  of the step is then possible.  When the subroutine believes H to be
C  too large, it returns CUTBAK = .TRUE. and a suitably reduced H for
C  another try.
C
         IF (MAIN) THEN
            IF (PHASE2) THEN
               IF (I.LE.3 .AND. ABS(HTRY).GT.HMIN) THEN
                  CALL STEPA(TNOW,Y,YP,TSTG,YNEW,STAGES(1,PTR(I)),
     &                       HTRY,WEIGHT,CUTBAK)
                  IF (CUTBAK) THEN
                     LAST = .FALSE.
C
C  Make sure that STEPA does not reduce the step size below the
C  minimum. If it does, reset H to HMIN and deactivate PHASE2.
                     IF (ABS(HTRY).LE.HMIN) THEN
                        HTRY = SIGN(HMIN,HTRY)
                        PHASE2 = .FALSE.
                     END IF
                     GO TO 20
                  END IF
               END IF
            END IF
         END IF
C----------------------------------------------------------------------
C
  140 CONTINUE
C
C  Some formulas are constructed so that the last stage represents
C  the result of the step (FSAL=.TRUE.), hence if the step is acceptable,
C  it will be the first stage for the next step. When FSAL=.FALSE., we
C  have to complete the computation of the step.
C
      IF (.NOT.FSAL) THEN
         DO 200 I = 1, NSTAGE
            IF (I.EQ.1) THEN
               DO 160 L = 1, NEQ
                  ERREST(L) = BHAT(1)*YP(L)
  160          CONTINUE
            ELSE
               IF (BHAT(I).NE.ZERO) THEN
                  DO 180 L = 1, NEQ
                     ERREST(L) = ERREST(L) + BHAT(I)*STAGES(L,PTR(I))
  180             CONTINUE
               END IF
            END IF
  200    CONTINUE
         DO 220 L = 1, NEQ
            YNEW(L) = Y(L) + HTRY*ERREST(L)
  220    CONTINUE
      END IF
C
C  Form an estimate of the error in the lower order formula by comparing
C  it to the higher order formula of the pair. ERREST(*) has been used
C  as working storage above.  The higher order approximation has been
C  formed as YNEW(*) = Y(*) + HTRY*ERREST(*) where ERREST(*) is a linear
C  combination of the stages of the formula. The lower order result also
C  has the form Y(*) plus HTRY times a different linear combination of
C  the stages. Hence, this different linear combination of stages for
C  the lower order formula can just be subtracted from the combination
C  stored in ERREST(*) to produce the errors. The result is then
C  multiplied by HTRY to obtain the error estimate.
C
      DO 280 I = 1, NSTAGE
         IF (I.EQ.1 .AND. B(1).NE.ZERO) THEN
            DO 240 L = 1, NEQ
               ERREST(L) = ERREST(L) - B(1)*YP(L)
  240       CONTINUE
         ELSE
            IF (B(I).NE.ZERO) THEN
               DO 260 L = 1, NEQ
                  ERREST(L) = ERREST(L) - B(I)*STAGES(L,PTR(I))
  260          CONTINUE
            END IF
         END IF
  280 CONTINUE
      DO 300 L = 1, NEQ
         ERREST(L) = HTRY*ERREST(L)
  300 CONTINUE
C
C  The error in a solution component is measured relative to a weight
C  that is the larger of a threshold and the size of the solution over
C  the step.  Using the magnitude of a solution component at both ends
C  of the step in the definition of "size" increases the robustness of
C  the test. When global error estimation is specified, the weight
C  vector WEIGHT(*) is defined by the primary integration and is then
C  used in the secondary integration.
C
      IF (MAIN) THEN
         DO 320 L = 1, NEQ
            AVGY = HALF*(ABS(Y(L))+ABS(YNEW(L)))
            WEIGHT(L) = MAX(AVGY,THRES(L))
  320    CONTINUE
      END IF
C
      ERR = ZERO
      DO 340 L = 1, NEQ
         ERR = MAX(ERR,ABS(ERREST(L)/WEIGHT(L)))
  340 CONTINUE   
C
      RETURN
      END
      SUBROUTINE STEPA(TNOW,Y,YP,TSTG,YSTG,YPSTG,HTRY,WEIGHT,CUTBAK)
C************************************************
C**** NOT A DESIGNATED USER-CALLABLE ROUTINE ****
C************************************************
C
C  Purpose:      To calculate an "on-scale" step size for phase 2 of
C                the initial step size computation.
C
C  Input:        TNOW, Y(*), YP(*), TSTG, YSTG(*), YPSTG(*)
C  Input/output: HTRY, WEIGHT
C  Output:       CUTBAK
C
C  Common:       Initializes:    none
C                Reads:          /RKCOM1/ TND, NEQ
C                                /RKCOM5/ STBRAD, RS1, RS4
C                                /RKCOM7/ RNDOFF
C                Alters:         none
C
C  Comments:
C  =========
C  This subroutine is used during the first three stages of the first step.
C  A Lipschitz constant L for the differential equation in autonomous form
C  is approximated, and the product abs(HTRY)*L is compared to an approximate
C  radius, STBRAD, of the stability region of the method. The step size is 
C  reduced as necessary, within a range specified by the step size control 
C  parameters RS1 and RS4, to assure stability and give some confidence in 
C  the error estimator.  If HTRY is reduced, CUTBAK is set .TRUE..
C
C  Y(*) and YP(*) contain the solution and its derivative at TNOW and
C  similarly YSTG(*) and YPSTG(*) contain approximations at TSTG.
C
C  Normally the weights used in the control of the error depend on the
C  size of the solution at the beginning and at the end of the step, but
C  at this time we do not have a solution at the end of the step.  Each
C  stage YSTG(*) of the Runge - Kutta process represents a low order
C  approximation to the solution at TSTG.  Because the initial value of
C  WEIGHT(*) provided in the first phase of the scheme is based only on
C  the solution at T and THRES(*), it is continually updated in STEPA to
C  account for the size of the solution throughout the step as revealed
C  by the intermediate stages YSTG(*). Inside this subroutine only, the
C  differential equation is converted to autonomous form. After the
C  conversion, the end of the interval of integration, TND, is used
C  to define a suitable weight for the independent variable.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION  HTRY, TNOW, TSTG
      LOGICAL           CUTBAK
C     .. Array Arguments ..
      DOUBLE PRECISION  WEIGHT(*), Y(*), YP(*), YPSTG(*), YSTG(*)
C     .. Common Block for Problem Definition ..
      DOUBLE PRECISION  TSTRT, TND, DIR, HSTRT, TOLR
      INTEGER           NEQN
      COMMON /RKCOM1/   TSTRT, TND, DIR, HSTRT, TOLR, NEQN
      SAVE   /RKCOM1/
C     .. Common Block to hold Formula Characterisitcs ..
      DOUBLE PRECISION  TOOSML, COST, SAFETY, EXPON, STBRAD, TANANG,
     &                  RS, RS1, RS2, RS3, RS4
      INTEGER           ORDER, LSTSTG, MAXTRY, NSEC
      LOGICAL           FSAL
      COMMON /RKCOM5/   TOOSML, COST, SAFETY, EXPON, STBRAD, TANANG,
     &                  RS, RS1, RS2, RS3, RS4, ORDER, LSTSTG, MAXTRY,
     &                  NSEC, FSAL
      SAVE   /RKCOM5/
C     .. Common Block for Environment Parameters ..
      DOUBLE PRECISION  MCHEPS, DWARF, RNDOFF, SQRRMC, CUBRMC, TINY
      INTEGER           OUTCH
      COMMON /RKCOM7/   MCHEPS, DWARF, RNDOFF, SQRRMC, CUBRMC, TINY,
     &                  OUTCH
      SAVE   /RKCOM7/
C     .. Parameters ..
      DOUBLE PRECISION  ZERO
      PARAMETER         (ZERO=0.0D0)
C     .. Local Scalars ..
      DOUBLE PRECISION  ARGDIF, FDIFF, SCL, TDIFF, TWT, WT, YNRM, YSTGNM
      INTEGER           L
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, MAX, MIN
C     .. Executable Statements ..
C
C  Update the weights to account for the current intermediate solution
C  approximation YSTG(*).  Compute the sizes of Y(*) and YSTG(*) in the
C  new norm.  The size of the Lipschitz constant is assessed by a difference
C  in the arguments Y(*), YSTG(*) and a difference in the function evaluated
C  at these arguments.
C
      YNRM = ZERO
      YSTGNM = ZERO
      ARGDIF = ZERO
      FDIFF = ZERO
      DO 20 L = 1, NEQN
         WT = MAX(WEIGHT(L),ABS(YSTG(L)))
         WEIGHT(L) = WT
         YNRM = MAX(YNRM,ABS(Y(L))/WT)
         YSTGNM = MAX(YSTGNM,ABS(YSTG(L))/WT)
         ARGDIF = MAX(ARGDIF,ABS(YSTG(L)-Y(L))/WT)
         FDIFF = MAX(FDIFF,ABS(YPSTG(L)-YP(L))/WT)
   20 CONTINUE
C
C  The transformation of the equation to autonomous form is done
C  implicitly.  The difference of the arguments must take into account
C  the difference between the values of the independent variable T and
C  TSTG. The difference of the corresponding component of the function
C  is zero because of the way the standard transformation is done.
C
      TDIFF = TSTG - TNOW
      TWT = ABS(TND-TNOW)
      YNRM = MAX(YNRM,ABS(TNOW)/TWT)
      YSTGNM = MAX(YSTGNM,ABS(TSTG)/TWT)
      ARGDIF = MAX(ARGDIF,ABS(TDIFF)/TWT)
C
C  The ratio FDIFF/ARGDIF is a lower bound for, and an approximation to, a
C  Lipschitz constant L for the differential equation written in autonomous
C  form.  First we must ask if the difference ARGDIF is significant in the 
C  precision available.  If it appears to be, we insist that abs(HTRY)*L be 
C  less than an approximate radius, STBRAD, of the stability region of the
C  method.  This is more stringent than necessary for stability, possibly a
C  lot more stringent, but the aim is to get an HTRY small enough that the
C  error estimate for the step is credible.  The reduction is required to be
C  at least as much as the step control parameter RS1. It is necessary to 
C  limit the reduction of HTRY at any one time because we may be misled in 
C  the size of the reduction that is appropriate due to nonlinearity of the 
C  differential equation and to inaccurate weights caused by HTRY much too 
C  large.  The reduction is not permitted to be more than the step control 
C  parameter RS4.
C
      CUTBAK = .FALSE.
      IF (ARGDIF.GT.RNDOFF*MAX(YNRM,YSTGNM)) THEN
         IF ((ABS(HTRY)*FDIFF).GT.(STBRAD*ARGDIF)) THEN
            SCL = (STBRAD*ARGDIF)/(ABS(HTRY)*FDIFF)
            SCL = MIN(SCL,RS1)
            SCL = MAX(SCL,RS4)
            HTRY = SCL*HTRY
            CUTBAK = .TRUE.
         END IF
      END IF
C
      RETURN
      END
      SUBROUTINE STEPB(NEQ,Y,YP,H,YNEW,STAGES,THRES,ERR,MAIN,WEIGHT)
C************************************************
C**** NOT A DESIGNATED USER-CALLABLE ROUTINE ****
C************************************************
C
C  Purpose:      To compute an error estimate for METHD = 2 prior
C                to completing the step.
C
C  Input:        NEQ, Y(*), YP(*), H, STAGES(NEQ,*), THRES(*), MAIN,
C                WEIGHT(*)
C  Output:       ERR
C
C  Common:       Initializes:    none
C                Reads:          /RKCOM4/ E, PTR
C                Alters:         none
C
C  Comments:
C  =========
C  If global error assessment is taking place, then MAIN = .FALSE. and
C  the weight vector generated by the primary integration is used.  The
C  error estimate is a linear combination (with coefficients in E(*))
C  of the stages stored in STAGES(*,*) (located by PTR(*)).
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION  ERR, H
      INTEGER           NEQ
      LOGICAL           MAIN
C     .. Array Arguments ..
      DOUBLE PRECISION  STAGES(NEQ,*), THRES(*), WEIGHT(*), Y(*),
     &                  YNEW(*), YP(*)
C     .. Common Block to hold Formula Definitions ..
      DOUBLE PRECISION  A(13,13), B(13), C(13), BHAT(13), R(11,6),
     &                  E(7)
      INTEGER           PTR(13), NSTAGE, METHD, MINTP
      LOGICAL           INTP
      COMMON /RKCOM4/   A, B, C, BHAT, R, E, PTR, NSTAGE, METHD,
     &                  MINTP, INTP
      SAVE   /RKCOM4/
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, HALF
      PARAMETER         (ZERO=0.0D0,HALF=0.5D0)
C     .. Local Scalars ..
      DOUBLE PRECISION  AVGY, SUM, WT
      INTEGER           INDEX, L
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, MAX
C     .. Executable Statements ..
C
      ERR = ZERO
      DO 40 L = 1, NEQ
C
C  Estimate the local error of component L. The coding makes use of
C  E(2) = 0.0D0 and E(7) = 0.0D0.
C
         SUM = E(1)*YP(L)
         DO 20 INDEX = 3, 6
            SUM = SUM + E(INDEX)*STAGES(L,PTR(INDEX))
   20    CONTINUE
C
C  The local error is H*SUM.  A weighted maximum norm of SUM is formed 
C  and then the factor of H is taken into account.
C  
         IF (MAIN) THEN
            AVGY = HALF*(ABS(Y(L))+ABS(YNEW(L)))
            WT = MAX(AVGY,THRES(L))
         ELSE
            WT = WEIGHT(L)
         END IF
C
         ERR = MAX(ERR,ABS(SUM/WT))
   40 CONTINUE
      ERR = ABS(H)*ERR
C
      RETURN
      END
      SUBROUTINE STIFF(F,HAVG,JFLSTP,TOOMCH,MAXFCN,WORK,IER,NREC)
C************************************************
C**** NOT A DESIGNATED USER-CALLABLE ROUTINE ****
C************************************************
C
C  Purpose:      Diagnose stiffness.  This depends on two things: whether
C                the step size is being restricted on grounds of stability
C                and whether the integration to TND can be completed in no
C                more than MAXFCN function evaluations.
C
C  Input:        HAVG, TOOMCH, MAXFCN, WORK(*)
C  Input/output: JFLSTP
C  Output:       IER, NREC
C  Workspace:    WORK(*)
C  External:     F
C
C  Common:       Initializes:    /RKCOM9/ REC
C                Reads:          /RKCOM1/ TND, NEQN
C                                /RKCOM2/ T, H, NFCN, SVNFCN, OKSTP
C                                /RKCOM3/ PRY, PRYP, PRTHRS, PRWT, PRSCR,
C                                         PRSTGS, PRYOLD
C                                /RKCOM5/ COST
C                Alters:         /RKCOM2/ NFCN
C                                /RKCOM9/ REC
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION  HAVG
      INTEGER           IER, JFLSTP, MAXFCN, NREC
      LOGICAL           TOOMCH
C     .. Array Arguments ..
      DOUBLE PRECISION  WORK(*)
C     .. Subroutine Arguments ..
      EXTERNAL          F
C     .. Common Block for Problem Definition ..
      DOUBLE PRECISION  TSTRT, TND, DIR, HSTRT, TOLR
      INTEGER           NEQN
      COMMON /RKCOM1/   TSTRT, TND, DIR, HSTRT, TOLR, NEQN
      SAVE   /RKCOM1/
C     .. Common Block to hold Problem Status ..
      DOUBLE PRECISION  T, H, TOLD, HOLD
      INTEGER           NFCN, SVNFCN, OKSTP, FLSTP
      LOGICAL           FIRST, LAST
      COMMON /RKCOM2/   T, H, TOLD, HOLD, NFCN, SVNFCN, OKSTP, FLSTP,
     &                  FIRST, LAST
      SAVE   /RKCOM2/
C     .. Common Block for General Workspace Pointers ..
      INTEGER           PRTHRS, PRERST, PRWT, PRYOLD, PRSCR, PRY, PRYP,
     &                  PRSTGS, PRINTP, LNINTP
      COMMON /RKCOM3/   PRTHRS, PRERST, PRWT, PRYOLD, PRSCR, PRY, PRYP,
     &                  PRSTGS, PRINTP, LNINTP
      SAVE   /RKCOM3/
C     .. Common Block to hold Formula Characterisitcs ..
      DOUBLE PRECISION  TOOSML, COST, SAFETY, EXPON, STBRAD, TANANG,
     &                  RS, RS1, RS2, RS3, RS4
      INTEGER           ORDER, LSTSTG, MAXTRY, NSEC
      LOGICAL           FSAL
      COMMON /RKCOM5/   TOOSML, COST, SAFETY, EXPON, STBRAD, TANANG,
     &                  RS, RS1, RS2, RS3, RS4, ORDER, LSTSTG, MAXTRY,
     &                  NSEC, FSAL
      SAVE   /RKCOM5/
C     .. Common Block for Error Message ..
      CHARACTER*80      REC(10)
      COMMON /RKCOM9/   REC
      SAVE   /RKCOM9/
C     .. Parameters ..
      DOUBLE PRECISION  HALF
      PARAMETER         (HALF=0.5D0)
C     .. Local Scalars ..
      DOUBLE PRECISION  AVGY, XTRAWK
      INTEGER           L
      LOGICAL           LOTSFL, STIF, UNSURE
C     .. External Subroutines ..
      EXTERNAL          STIFFA
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, DBLE, MAX, MOD
C     .. Executable Statements ..
C
      IF (MOD(OKSTP-10,40).EQ.0) THEN
         LOTSFL = JFLSTP .GE. 10
         JFLSTP = 0
      ELSE
         LOTSFL = .FALSE.
      END IF
C
C  If either too much work has been done or there are lots of failed steps,
C  test for stiffness.
C
      IF (TOOMCH .OR. LOTSFL) THEN
C
C  Regenerate weight vector
         DO 20 L = 1, NEQN
            AVGY = HALF*(ABS(WORK(PRY-1+L))+ABS(WORK(PRYOLD-1+L)))
            WORK(PRWT-1+L) = MAX(AVGY,WORK(PRTHRS-1+L))
   20    CONTINUE
C
C  STIFFA determines whether the problem is STIFF. In some circumstances it
C  is UNSURE.  The decision depends on two things: whether the step size is
C  being restricted on grounds of stability and whether the integration to
C  TND can be completed in no more than MAXFCN function evaluations.  The
C  last four arguments of STIFFA are vectors of length NEQN used for working
C  storage.  Some storage in WORK(*) reserved for the stages (there are a
C  minimum of three such vectors reserved for the METHDs implemented) and
C  the scratch vector starting at PRSCR are used for this purpose.
C
         CALL STIFFA(F,T,WORK(PRY),H,HAVG,TND,MAXFCN,WORK(PRWT),
     &               WORK(PRYP),WORK(PRERST),UNSURE,STIF,WORK(PRSTGS),
     &               WORK(PRSTGS+NEQN),WORK(PRSTGS+2*NEQN),WORK(PRSCR))
         IF (.NOT.UNSURE) THEN
            IF (STIF) THEN
C
C  Predict how much eXTRA WorK will be needed to reach TND.
               XTRAWK = (COST*ABS((TND-T)/HAVG))/DBLE(SVNFCN+NFCN)
               IER = 4
               WRITE (REC(NREC+1),'(A)')
     &' ** Your problem has been diagnosed as stiff.  If the '
               WRITE (REC(NREC+2),'(A,D13.5)')
     &' ** situation persists, it will cost roughly ', XTRAWK
               WRITE (REC(NREC+3),'(A)')
     &' ** times as much to reach TEND as it has cost to reach TNOW.'
               WRITE (REC(NREC+4),'(A)')
     &' ** You should probably change to a code intended for '
               WRITE (REC(NREC+5),'(A)')
     &' ** stiff problems. '
               NREC = NREC + 5
            END IF
         END IF
      END IF
C
      RETURN
      END
      SUBROUTINE STIFFA(F,X,Y,HNOW,HAVG,XEND,MAXFCN,WT,FXY,V0,UNSURE,
     &                  STIF,V1,V2,V3,VTEMP)
C************************************************
C**** NOT A DESIGNATED USER-CALLABLE ROUTINE ****
C************************************************
C
C  External:     F
C  Input:        X, Y(*), HNOW, HAVG, XEND, MAXFCN, WT(*), FXY(*)
C  Input/Output  V0(*)
C  Output:       UNSURE, STIF
C  Workspace:    V1(*), V2(*), V3(*), VTEMP(*)
C
C  Common:       Initializes:    none
C                Reads:          /RKCOM1/ TND, NEQN
C                                /RKCOM5/ COST, STBRAD, TANANG
C                                /RKCOM7/ SQRRMC, CUBRMC
C                Alters:         none
C
C  STIFFA diagnoses stiffness for an explicit Runge-Kutta code.  When it
C  is called, either many step failures have been observed, or a lot of
C  work has been done.
C
C  The NEQ equations of the problem are defined by the subroutine F(X,Y,YP).
C  When STIFFA is called, the integration has reached X where the approximate
C  solution is Y(*).  The vector FXY(*) is defined by a call of F(X,Y,FXY).
C  It is an input argument because it is usually available from the integrator.
C
C  The last successful step was of size HNOW, and an average step size is
C  HAVG.  A weighted norm is used to measure the local error with the error
C  in solution component L divided by the positive weight WT(L) provided in 
C  the vector WT(*).
C
C  Explicit Runge - Kutta codes estimate the local error of Y(*) by
C  forming the difference of two approximate solutions.  This difference
C  must be provided in the vector V0(*).  When this difference is too
C  small to be significant, STIFFA will replace it with a "random" vector.
C
C  STIF is set .TRUE. when the average step size appears to be restricted
C  on grounds of stability.  In certain cases the variable UNSURE is set 
C  .TRUE.; the value of STIF is then not defined.
C
C  The stability region of the explicit Runge-Kutta formula is described
C  by quantities TANANG and STBRAD that are communicated by the setup routine
C  via COMMON.  Stability regions often change sharply near the imaginary
C  axis so that it is difficult to classify the stiffness of a problem with
C  eigenvalues of a local Jacobian that are "near" the imaginary axis.  For
C  this reason,  we consider only points Z in the upper left half complex
C  plane for which TAN( IMAG(Z)/( - RE(Z))) <= TANANG. Eigenvalues outside
C  this region are one reason for the code being UNSURE.  The stability
C  region is approximated by the intersection of a disk with this sector.
C  The radius of this disk is called STBRAD.
C
C  Working storage must be provided via the four vectors V1(*),V2(*),
C  V3(*),VTEMP(*).  These vectors must be of length at least NEQ.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION  HAVG, HNOW, X, XEND
      INTEGER           MAXFCN
      LOGICAL           STIF, UNSURE
C     .. Array Arguments ..
      DOUBLE PRECISION  FXY(*), V0(*), V1(*), V2(*), V3(*), VTEMP(*),
     &                  WT(*), Y(*)
C     .. Subroutine Arguments ..
      EXTERNAL          F
C     .. Common Block for Problem Definition ..
      DOUBLE PRECISION  TSTRT, TND, DIR, HSTRT, TOLR
      INTEGER           NEQN
      COMMON /RKCOM1/   TSTRT, TND, DIR, HSTRT, TOLR, NEQN
      SAVE   /RKCOM1/
C     .. Common Block to hold Formula Characterisitcs ..
      DOUBLE PRECISION  TOOSML, COST, SAFETY, EXPON, STBRAD, TANANG,
     &                  RS, RS1, RS2, RS3, RS4
      INTEGER           ORDER, LSTSTG, MAXTRY, NSEC
      LOGICAL           FSAL
      COMMON /RKCOM5/   TOOSML, COST, SAFETY, EXPON, STBRAD, TANANG,
     &                  RS, RS1, RS2, RS3, RS4, ORDER, LSTSTG, MAXTRY,
     &                  NSEC, FSAL
      SAVE   /RKCOM5/
C     .. Common Block for Environment Parameters ..
      DOUBLE PRECISION  MCHEPS, DWARF, RNDOFF, SQRRMC, CUBRMC, TINY
      INTEGER           OUTCH
      COMMON /RKCOM7/   MCHEPS, DWARF, RNDOFF, SQRRMC, CUBRMC, TINY,
     &                  OUTCH
      SAVE   /RKCOM7/
C     .. Parameters ..
      DOUBLE PRECISION  LARGE
      PARAMETER         (LARGE=1.0D+10)
      DOUBLE PRECISION  ZERO, P001, P9, ONE, TWO, FIVE, FIFTH
      PARAMETER         (ZERO=0.0D+0,P001=0.001D+0,P9=0.9D+0,ONE=1.0D+0,
     &                  TWO=2.0D+0,FIVE=5.0D+0,FIFTH=0.2D+0)
C     .. Local Scalars ..
      DOUBLE PRECISION  ALPHA1, ALPHA2, BETA1, BETA2, D1, D2, DET1,
     &                  DET2, DIST, RES2, RHO, RHO2, ROLD, SCALE, V0NRM,
     &                  V0V0, V0V1, V0V2, V1V1, V1V2, V1V3, V2V2, V2V3,
     &                  V3NRM, V3V3, XTRFCN, YNRM
      INTEGER           L, NTRY
      LOGICAL           ROOTRE
C     .. Local Arrays ..
      DOUBLE PRECISION  R1(2), R2(2), ROOT1(2), ROOT2(2)
C     .. External Functions ..
      DOUBLE PRECISION  DOTPRD
      EXTERNAL          DOTPRD
C     .. External Subroutines ..
      EXTERNAL          STIFFB, STIFFC, STIFFD
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, MIN, SQRT
C     .. Executable Statements ..
C
C  If the current step size differs substantially from the average,
C  the problem is not stiff.
C
      IF (ABS(HNOW/HAVG).GT.FIVE .OR. ABS(HNOW/HAVG).LT.FIFTH) THEN
         STIF = .FALSE.
         UNSURE = .FALSE.
         RETURN
      ELSE
         UNSURE = .TRUE.
      END IF
C
C  The average step size is used to predict the cost in function evaluations
C  of finishing the integration to XEND.  If this cost is no more than MAXFCN,
C  the problem is declared not stiff: If the step size is being restricted on
C  grounds of stability, it will stay close to HAVG.  The prediction will
C  then be good, but the cost is too low to consider the problem stiff.  If
C  the step size is not close to HAVG, the problem is not stiff.  Either way
C  there is no point to testing for a step size restriction due to stability.
C
      XTRFCN = COST*ABS((XEND-X)/HAVG)
      IF (XTRFCN.LE.MAXFCN) THEN
         STIF = .FALSE.
         UNSURE = .FALSE.
         RETURN
      ELSE
         UNSURE = .TRUE.
      END IF
C
C  There have been many step failures or a lot of work has been done.  Now 
C  we must determine if this is due to the stability characteristics of the
C  formula.  This is done by calculating the dominant eigenvalues of the
C  local Jacobian and then testing whether HAVG corresponds to being on the
C  boundary of the stability region.
C
C  The size of Y(*) provides scale information needed to approximate
C  the Jacobian by differences.
C
      YNRM = SQRT(DOTPRD(Y,Y,WT,NEQN))
      SCALE = YNRM*SQRRMC
      IF (SCALE.EQ.ZERO) THEN
C
C  Degenerate case.  Y(*) is (almost) the zero vector so the scale is not 
C  defined.  The input vector V0(*) is the difference between Y(*) and a 
C  lower order approximation to the solution that is within the error 
C  tolerance.  When Y(*) vanishes, V0(*) is itself an acceptable approximate
C  solution, so we take SCALE from it, if this is possible.
C
         YNRM = SQRT(DOTPRD(V0,V0,WT,NEQN))
         SCALE = YNRM*SQRRMC
         IF (SCALE.EQ.ZERO) THEN
            UNSURE = .TRUE.
            RETURN
         END IF
      END IF
C
      V0V0 = DOTPRD(V0,V0,WT,NEQN)
      IF (V0V0.EQ.ZERO) THEN
C
C  Degenerate case.  V0(*) is (almost) the zero vector so cannot
C  be used to define a direction for an increment to Y(*).  Try a
C  "random" direction.
C
         DO 20 L = 1, NEQN
            V0(L) = ONE
   20    CONTINUE
         V0V0 = DOTPRD(V0,V0,WT,NEQN)
      END IF
      V0NRM = SQRT(V0V0)
      DO 40 L = 1, NEQN
         V0(L) = V0(L)/V0NRM
   40 CONTINUE
      V0V0 = ONE
C
C  Use a nonlinear power method to estimate the two dominant eigenvalues.
C  V0(*) is often very rich in the two associated eigenvectors.  For this 
C  reason the computation is organized with the expectation that a minimal 
C  number of iterations will suffice.  Indeed, it is necessary to recognize 
C  a kind of degeneracy when there is a dominant real eigenvalue.  The
C  subroutine STIFFB does this.  In the first try, NTRY = 1, a Rayleigh 
C  quotient for such an eigenvalue is initialized as ROLD.  After each 
C  iteration, REROOT computes a new Rayleigh quotient and tests whether the
C  two approximations agree to one tenth of one per cent and the eigenvalue,
C  eigenvector pair satisfy a stringent test on the residual.  ROOTRE = .TRUE.
C  signals that a single dominant real root has been found.
C
      NTRY = 1
   60 CONTINUE
C
      CALL STIFFD(V0,HAVG,X,Y,F,FXY,WT,SCALE,V0V0,V1,V1V1,VTEMP)
C
C  The quantity SQRT(V1V1/V0V0) is a lower bound for the product of HAVG
C  and a Lipschitz constant.  If it should be LARGE, stiffness is not
C  restricting the step size to the stability region.  The principle is
C  clear enough, but the real reason for this test is to recognize an
C  extremely inaccurate computation of V1V1 due to finite precision
C  arithmetic in certain degenerate circumstances.
C
      IF (SQRT(V1V1).GT.LARGE*SQRT(V0V0)) THEN
         UNSURE = .TRUE.
         RETURN
      END IF
C
      V0V1 = DOTPRD(V0,V1,WT,NEQN)
      IF (NTRY.EQ.1) THEN
         ROLD = V0V1/V0V0
C
C  This is the first Rayleigh quotient approximating the product of HAVG
C  and a dominant real eigenvalue.  If it should be very small, the
C  problem is not stiff.  It is important to test for this possibility so
C  as to prevent underflow and degeneracies in the subsequent iteration.
C
         IF (ABS(ROLD).LT.CUBRMC) THEN
            UNSURE = .FALSE.
            STIF = .FALSE.
            RETURN
         END IF
      ELSE
         CALL STIFFB(V1V1,V0V1,V0V0,ROLD,RHO,ROOT1,ROOT2,ROOTRE)
         IF (ROOTRE) GO TO 100
      END IF
      CALL STIFFD(V1,HAVG,X,Y,F,FXY,WT,SCALE,V1V1,V2,V2V2,VTEMP)
      V0V2 = DOTPRD(V0,V2,WT,NEQN)
      V1V2 = DOTPRD(V1,V2,WT,NEQN)
      CALL STIFFB(V2V2,V1V2,V1V1,ROLD,RHO,ROOT1,ROOT2,ROOTRE)
      IF (ROOTRE) GO TO 100
C
C  Fit a quadratic in the eigenvalue to the three successive iterates
C  V0(*),V1(*),V2(*) of the power method to get a first approximation to
C  a pair of eigenvalues.  A test made earlier in STIFFB implies that
C  the quantity DET1 here will not be too small.
C
      DET1 = V0V0*V1V1 - V0V1**2
      ALPHA1 = (-V0V0*V1V2+V0V1*V0V2)/DET1
      BETA1 = (V0V1*V1V2-V1V1*V0V2)/DET1
C
C  Iterate again to get V3, test again for degeneracy, and then fit a
C  quadratic to V1(*),V2(*),V3(*) to get a second approximation to a pair
C  of eigenvalues.
C
      CALL STIFFD(V2,HAVG,X,Y,F,FXY,WT,SCALE,V2V2,V3,V3V3,VTEMP)
      V1V3 = DOTPRD(V1,V3,WT,NEQN)
      V2V3 = DOTPRD(V2,V3,WT,NEQN)
      CALL STIFFB(V3V3,V2V3,V2V2,ROLD,RHO,ROOT1,ROOT2,ROOTRE)
      IF (ROOTRE) GO TO 100
      DET2 = V1V1*V2V2 - V1V2**2
      ALPHA2 = (-V1V1*V2V3+V1V2*V1V3)/DET2
      BETA2 = (V1V2*V2V3-V2V2*V1V3)/DET2
C
C  First test the residual of the quadratic fit to see if we might
C  have determined a pair of eigenvalues.
C
      RES2 = ABS(V3V3+V2V2*ALPHA2**2+V1V1*BETA2**2+TWO*V2V3*ALPHA2+
     &       TWO*V1V3*BETA2+TWO*V1V2*ALPHA2*BETA2)
      IF (RES2.LE.V3V3*P001**2) THEN
C
C  Calculate the two approximate pairs of eigenvalues.
C
         CALL STIFFC(ALPHA1,BETA1,R1,R2)
         CALL STIFFC(ALPHA2,BETA2,ROOT1,ROOT2)
C
C  The test for convergence is done on the larger root of the second
C  approximation.  It is complicated by the fact that one pair of roots 
C  might be real and the other complex.  First calculate the spectral 
C  radius RHO of HAVG*J as the magnitude of ROOT1.  Then see if one of 
C  the roots R1,R2 is within one per cent of ROOT1.  A subdominant root 
C  may be very poorly approximated if its magnitude is much smaller than 
C  RHO -- this does not matter in our use of these eigenvalues.
C
         RHO = SQRT(ROOT1(1)**2+ROOT1(2)**2)
         D1 = (ROOT1(1)-R1(1))**2 + (ROOT1(2)-R1(2))**2
         D2 = (ROOT1(1)-R2(1))**2 + (ROOT1(2)-R2(2))**2
         DIST = SQRT(MIN(D1,D2))
         IF (DIST.LE.P001*RHO) GO TO 100
      END IF
C
C  Do not have convergence yet.  Because the iterations are cheap, and
C  because the convergence criterion is stringent, we are willing to try
C  a few iterations.
C
      IF (NTRY.LT.MAXTRY) THEN
         NTRY = NTRY + 1
         V3NRM = SQRT(V3V3)
         DO 80 L = 1, NEQN
            V0(L) = V3(L)/V3NRM
   80    CONTINUE
         V0V0 = ONE
         GO TO 60
      ELSE
         UNSURE = .TRUE.
         RETURN
      END IF
C
C                        **************
C
C  We now have the dominant eigenvalues.  Decide if the average step
C  size is being restricted on grounds of stability.  Check the real
C  parts of the eigenvalues.  First see if the dominant eigenvalue is
C  in the left half plane -- there won't be a stability restriction
C  unless it is. If there is another eigenvalue of comparable magnitude
C  with a positive real part, the problem is not stiff. If the dominant
C  eigenvalue is too close to the imaginary axis, we cannot diagnose
C  stiffness.
C
  100 CONTINUE
      IF (ROOT1(1).GT.ZERO) THEN
         STIF = .FALSE.
         UNSURE = .FALSE.
         RETURN
      END IF
      RHO2 = SQRT(ROOT2(1)**2+ROOT2(2)**2)
      IF (RHO2.GE.P9*RHO .AND. ROOT2(1).GT.ZERO) THEN
         STIF = .FALSE.
         UNSURE = .FALSE.
         RETURN
      END IF
      IF (ABS(ROOT1(2)).GT.ABS(ROOT1(1))*TANANG) THEN
         UNSURE = .TRUE.
         RETURN
      END IF
C
C  If the average step size corresponds to being well within the
C  stability region, the step size is not being restricted because
C  of stability.
C
      STIF = RHO .GE. P9*STBRAD
      UNSURE = .FALSE.
      RETURN
      END
      SUBROUTINE STIFFB(V1V1,V0V1,V0V0,ROLD,RHO,ROOT1,ROOT2,ROOTRE)
C************************************************
C**** NOT A DESIGNATED USER-CALLABLE ROUTINE ****
C************************************************
C
C  Input:        V1V1, V0V1, V0V0
C  Input/output: ROLD
C  Output:       RHO, ROOT1(*),ROOT2(*),ROOTRE
C
C  Decide if the iteration has degenerated because of a strongly
C  dominant real eigenvalue.  Have just computed the latest iterate.
C  V1V1 is its dot product with itself, V0V1 is the dot product
C  of the previous iterate with the current one, and V0V0 is the
C  dot product of the previous iterate with itself.  ROLD is a
C  previous Rayleigh quotient approximating a dominant real
C  eigenvalue.  It must be computed directly the first time the
C  subroutine is called.  It is updated each call to STIFFB, hence
C  is available for subsequent calls.
C
C  If there is a strongly dominant real eigenvalue, ROOTRE is set
C  .TRUE., ROOT1(*) returns the eigenvalue, RHO returns the magnitude
C  of the eigenvalue, and ROOT2(*) is set to zero.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION  RHO, ROLD, V0V0, V0V1, V1V1
      LOGICAL           ROOTRE
C     .. Array Arguments ..
      DOUBLE PRECISION  ROOT1(2), ROOT2(2)
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, P001
      PARAMETER         (ZERO=0.0D+0,P001=0.001D+0)
C     .. Local Scalars ..
      DOUBLE PRECISION  DET, R, RES
C     .. Intrinsic Functions ..
      INTRINSIC         ABS
C     .. Executable Statements ..
C
      R = V0V1/V0V0
      RHO = ABS(R)
      DET = V0V0*V1V1 - V0V1**2
      RES = ABS(DET/V0V0)
      ROOTRE = DET .EQ. ZERO .OR. (RES.LE.V1V1*P001**2 .AND.
     &         ABS(R-ROLD).LE.P001*RHO)
      IF (ROOTRE) THEN
         ROOT1(1) = R
         ROOT1(2) = ZERO
         ROOT2(1) = ZERO
         ROOT2(2) = ZERO
      END IF
      ROLD = R
C
      RETURN
      END
      SUBROUTINE STIFFC(ALPHA,BETA,R1,R2)
C************************************************
C**** NOT A DESIGNATED USER-CALLABLE ROUTINE ****
C************************************************
C
C  Input:  ALPHA, BETA
C  Output: R1(*), R2(*)
C
C  This subroutine computes the two complex roots R1 and R2 of
C  the quadratic equation X**2 + ALPHA*X + BETA = 0.  The magnitude
C  of R1 is greater than or equal to the magnitude of R2. R1 and R2 are
C  returned as vectors of two components with the first being the real
C  part of the complex number and the second being the imaginary part.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION  ALPHA, BETA
C     .. Array Arguments ..
      DOUBLE PRECISION  R1(2), R2(2)
C     .. Parameters ..
      DOUBLE PRECISION  ZERO, TWO
      PARAMETER         (ZERO=0.0D+0,TWO=2.0D+0)
C     .. Local Scalars ..
      DOUBLE PRECISION  DISC, SQDISC, TEMP
C     .. Intrinsic Functions ..
      INTRINSIC         ABS, SQRT
C     .. Executable Statements ..
      TEMP = ALPHA/TWO
      DISC = TEMP**2 - BETA
      IF (DISC.EQ.ZERO) THEN
C
C  Double root.
C
         R1(1) = -TEMP
         R1(2) = ZERO
         R2(1) = R1(1)
         R2(2) = R1(2)
         RETURN
      END IF
C
      SQDISC = SQRT(ABS(DISC))
      IF (DISC.LT.ZERO) THEN
C
C  Complex conjugate roots.
C
         R1(1) = -TEMP
         R1(2) = SQDISC
         R2(1) = R1(1)
         R2(2) = -R1(2)
      ELSE
C
C  Real pair of roots.  Calculate the bigger one in R1(1).
C
         IF (TEMP.GT.ZERO) THEN
            R1(1) = -TEMP - SQDISC
         ELSE
            R1(1) = -TEMP + SQDISC
         END IF
         R1(2) = ZERO
         R2(1) = BETA/R1(1)
         R2(2) = ZERO
      END IF
C
      RETURN
      END
      SUBROUTINE STIFFD(V,HAVG,X,Y,F,FXY,WT,SCALE,VDOTV,Z,ZDOTZ,VTEMP)
C************************************************
C**** NOT A DESIGNATED USER-CALLABLE ROUTINE ****
C************************************************
C
C  External:     F
C  Input:        V(*), HAVG, X, Y(*), FXY(*), WT(*), SCALE, VDOTV,
C  Output:       Z(*), ZDOTZ
C  Workspace:    VTEMP(*)
C
C  For an input vector V(*) of length NEQ, this subroutine computes a vector
C  Z(*) that approximates the product HAVG*J*V where HAVG is an input scalar 
C  and J is the Jacobian matrix of a function F evaluated at the input 
C  arguments (X,Y(*)).  This function is defined by a subroutine of the form
C  F(T,U,F) that when given T and U(*), returns the value of the function in 
C  F(*).  The input vector FXY(*) is defined by F(X,Y,FXY).  Scaling is a 
C  delicate matter.  A weighted Euclidean norm is used with the (positive) 
C  weights provided in WT(*).  The input scalar SCALE is the square root of 
C  the unit roundoff times the norm of Y(*).  The square of the norm of the
C  input vector V(*) is input as VDOTV.  The routine outputs the square of
C  the norm of the output vector Z(*) as ZDOTZ.  The subroutine calls the
C  DOUBLE PRECISION FUNCTION DOTPRD(U,V,WT,NEQ) to compute the dot (inner)
C  product.  The vector VTEMP(*) is used for working storage.
C
C     .. Scalar Arguments ..
      DOUBLE PRECISION  HAVG, SCALE, VDOTV, X, ZDOTZ
C     .. Array Arguments ..
      DOUBLE PRECISION  FXY(*), V(*), VTEMP(*), WT(*), Y(*), Z(*)
C     .. Subroutine Arguments ..
      EXTERNAL          F
C     .. Common Block for Problem Definition ..
      DOUBLE PRECISION  TSTRT, TND, DIR, HSTRT, TOLR
      INTEGER           NEQN
      COMMON /RKCOM1/   TSTRT, TND, DIR, HSTRT, TOLR, NEQN
      SAVE   /RKCOM1/
C     .. Common Block to hold Problem Status ..
      DOUBLE PRECISION  T, H, TOLD, HOLD
      INTEGER           NFCN, SVNFCN, OKSTP, FLSTP
      LOGICAL           FIRST, LAST
      COMMON /RKCOM2/   T, H, TOLD, HOLD, NFCN, SVNFCN, OKSTP, FLSTP,
     &                  FIRST, LAST
      SAVE   /RKCOM2/
C     .. Local Scalars ..
      DOUBLE PRECISION  TEMP1, TEMP2
      INTEGER           L
C     .. External Functions ..
      DOUBLE PRECISION  DOTPRD
      EXTERNAL          DOTPRD
C     .. Intrinsic Functions ..
      INTRINSIC         SQRT
C     .. Executable Statements ..
C
C  Scale V(*) so that it can be used as an increment to Y(*)
C  for an accurate difference approximation to the Jacobian.
C
      TEMP1 = SCALE/SQRT(VDOTV)
      DO 20 L = 1, NEQN
         VTEMP(L) = Y(L) + TEMP1*V(L)
   20 CONTINUE
C
      CALL F(X,VTEMP,Z)
      NFCN = NFCN + 1
C
C  Form the difference approximation.  At the same time undo
C  the scaling of V(*) and introduce the factor of HAVG.
C
      TEMP2 = HAVG/TEMP1
      DO 40 L = 1, NEQN
         Z(L) = TEMP2*(Z(L)-FXY(L))
   40 CONTINUE
C
      ZDOTZ = DOTPRD(Z,Z,WT,NEQN)
C
      RETURN
      END
      DOUBLE PRECISION FUNCTION DOTPRD(U,V,WT,NEQ)
C************************************************
C**** NOT A DESIGNATED USER-CALLABLE ROUTINE ****
C************************************************
C
C  Purpose:   To compute a weighted Euclidean dot (inner) product of
C             two vectors.
C
C  Input:     U(*), V(*), WT(*), NEQ
C  Output:    the result DOTPRD is returned via the subprogram name
C
C  Comments:
C  =========
C  The vectors U(*), V(*), and WT(*) are of length NEQ. The components
C  of WT(*) are weights that must be non-zero.
C
C     .. Scalar Arguments ..
      INTEGER           NEQ
C     .. Array Arguments ..
      DOUBLE PRECISION  U(*), V(*), WT(*)
C     .. Parameters ..
      DOUBLE PRECISION  ZERO
      PARAMETER         (ZERO=0.0D0)
C     .. Local Scalars ..
      DOUBLE PRECISION  SUM
      INTEGER           L
C     .. Executable Statements ..
C
      SUM = ZERO
      DO 20 L = 1, NEQ
         SUM = SUM + (U(L)/WT(L))*(V(L)/WT(L))
   20 CONTINUE
C
      DOTPRD = SUM
C
      RETURN
      END
      SUBROUTINE SOFTFL(ASK,ON)
C
C  Purpose:      To prevent a program STOP after a "catastrophic"
C                failure when using a routine from RKSUITE.
C
C  Input:        ASK
C  Input/output: ON
C
C  Comments:
C  =========
C  When a "catastrophic" failure is detected, the default action of
C  RKSUITE is to write an explanation to the standard output channel,
C  OUTCH, and STOP.  This subroutine can be used to prevent the STOP and
C  so allow the main program to continue.  To do this, you call SOFTFL with
C  ASK = .FALSE. and ON = .TRUE.  You must then call the subroutine CHKFL
C  after every call to a user-callable routine in RKSUITE to check whether
C  a catastrophic error occurred and take appropriate action if it did.  Of
C  course, you may call SETUP at any time to start a new problem, but calling
C  any other user-callable routine in RKSUITE after a catastrophic error will
C  lead to a STOP (even when "soft failure" has been set "on").
C
C  When ON is set by a call to SOFTFL with ASK = .FALSE., the value of ON
C  is SAVEd.  The subroutine RKMSG in RKSUITE calls SOFTFL with ASK = .TRUE.
C  to find out the SAVEd value of ON.
C
C     .. Scalar Arguments ..
      LOGICAL           ASK, ON
C     .. Local Scalars ..
      LOGICAL           SOFT
C     .. Save statement ..
      SAVE              SOFT
C     .. Data statements ..
      DATA              SOFT/.FALSE./
C     .. Executable Statements ..
C
      IF (ASK) THEN
         ON = SOFT
      ELSE
         SOFT = ON
      END IF
C
      RETURN
      END
      SUBROUTINE CHKFL(ASK,ERROR)
C
C  Purpose:      Enquiry routine used in conjunction with SOFTFL.
C                Reports whether a "catastrophic" error was detected.
C
C  Input:        ASK
C  Input/output: ERROR
C
C  Comments:
C  =========
C  When a "catastrophic" failure is detected, the default action of
C  RKSUITE is to write an explanation to the standard output channel,
C  OUTCH, and STOP.  SOFTFL can be used to prevent the STOP and so
C  allow the main program to continue.  It is then necessary to call
C  CHKFL with ASK = .TRUE. after every call to a user-callable routine 
C  in RKSUITE to check whether a catastrophic error occurred and take 
C  appropriate action if it did.  If there was a catastrophic error, 
C  ERROR is returned .TRUE.  Of course, you may call SETUP at any time 
C  to start a new problem, but calling any other user-callable routine 
C  in RKSUITE after a catastrophic error will lead to a STOP (even when
C  "soft failure" has been set "on").
C
C  When a catastrophic failure (IER = 911) is detected in one of
C  the routines in RKSUITE, it calls CHKFL with ASK = .FALSE. and
C  ERROR = .TRUE.  This value of ERROR is SAVEd.
C
C     .. Scalar Arguments ..
      LOGICAL           ASK, ERROR
C     .. Local Scalars ..
      LOGICAL           SAVERR
C     .. Save statement ..
      SAVE              SAVERR
C     .. Data statements ..
      DATA              SAVERR/.FALSE./
C     .. Executable Statements ..
C
      IF (ASK) THEN
         ERROR = SAVERR
      ELSE
         SAVERR = ERROR
      END IF
C
      RETURN
      END
