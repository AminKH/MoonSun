
module SOFA

      implicit none

      contains

      SUBROUTINE iau_A2TF ( NDP, ANGLE, SIGN, IHMSF )
!*+
!*  - - - - - - - - -
!*   i a u _ A 2 T F
!*  - - - - - - - - -
!*
!*  Decompose radians into hours, minutes, seconds, fraction.
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  vector/matrix support routine.
!*
!*  Given:
!*     NDP       i        resolution (Note 1)
!*     ANGLE     d        angle in radians
!*
!*  Returned:
!*     SIGN      c        '+' or '-'
!*     IHMSF     i(4)     hours, minutes, seconds, fraction
!*
!*  Called:
!*     iau_D2TF     decompose days to hms
!*
!*  Notes:
!*
!*  1) NDP is interpreted as follows:
!*
!*     NDP         resolution
!*      :      ...0000 00 00
!*     -7         1000 00 00
!*     -6          100 00 00
!*     -5           10 00 00
!*     -4            1 00 00
!*     -3            0 10 00
!*     -2            0 01 00
!*     -1            0 00 10
!*      0            0 00 01
!*      1            0 00 00.1
!*      2            0 00 00.01
!*      3            0 00 00.001
!*      :            0 00 00.000...
!*
!*  2) The largest useful value for NDP is determined by the size
!*     of ANGLE, the format of DOUBLE PRECISION floating-point numbers
!*     on the target platform, and the risk of overflowing IHMSF(4).
!*     On a typical platform, for ANGLE up to 2pi, the available
!*     floating-point precision might correspond to NDP=12.  However,
!*     the practical limit is typically NDP=9, set by the capacity of
!*     a 32-bit IHMSF(4).
!*
!*  3) The absolute value of ANGLE may exceed 2pi.  In cases where it
!*     does not, it is up to the caller to test for and handle the
!*     case where ANGLE is very nearly 2pi and rounds up to 24 hours,
!*     by testing for IHMSF(1)=24 and setting IHMSF(1-4) to zero.
!*
!*  This revision:  2000 November 25
!*
!*  SOFA release 2019-07-22
!*
!*  Copyright (C) 2019 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER NDP
      DOUBLE PRECISION ANGLE
      CHARACTER SIGN*(*)
      INTEGER IHMSF(4)

!*  2Pi
      DOUBLE PRECISION D2PI
      PARAMETER ( D2PI = 6.283185307179586476925287D0 )

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Scale then use days to h,m,s routine.
      CALL iau_D2TF ( NDP, ANGLE/D2PI, SIGN, IHMSF )

!*  Finished.

!*+----------------------------------------------------------------------
      end

      SUBROUTINE iau_A2AF ( NDP, ANGLE, SIGN, IDMSF )
!*+
!*  - - - - - - - - -
!*   i a u _ A 2 A F
!*  - - - - - - - - -
!*
!*  Decompose radians into degrees, arcminutes, arcseconds, fraction.
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  vector/matrix support routine.
!*
!*  Given:
!*     NDP       i        resolution (Note 1)
!*     ANGLE     d        angle in radians
!*
!*  Returned:
!*     SIGN      c        '+' or '-'
!*     IDMSF     i(4)     degrees, arcminutes, arcseconds, fraction
!*
!*  Called:
!*     iau_D2TF     decompose days to hms
!*
!*  Notes:
!*
!*  1) NDP is interpreted as follows:
!*
!*     NDP         resolution
!*      :      ...0000 00 00
!*     -7         1000 00 00
!*     -6          100 00 00
!*     -5           10 00 00
!*     -4            1 00 00
!*     -3            0 10 00
!*     -2            0 01 00
!*     -1            0 00 10
!*      0            0 00 01
!*      1            0 00 00.1
!*      2            0 00 00.01
!*      3            0 00 00.001
!*      :            0 00 00.000...
!*
!*  2) The largest positive useful value for NDP is determined by the
!*     size of ANGLE, the format of DOUBLE PRECISION floating-point
!*     numbers on the target platform, and the risk of overflowing
!*     IDMSF(4).  On a typical platform, for ANGLE up to 2pi, the
!*     available floating-point precision might correspond to NDP=12.
!*     However, the practical limit is typically NDP=9, set by the
!*     capacity of a 32-bit IDMSF(4).
!*
!*  3) The absolute value of ANGLE may exceed 2pi.  In cases where it
!*     does not, it is up to the caller to test for and handle the
!*     case where ANGLE is very nearly 2pi and rounds up to 360 degrees,
!*     by testing for IDMSF(1)=360 and setting IDMSF(1-4) to zero.
!*
!*  This revision:  2007 December 3
!*
!*  SOFA release 2019-07-22
!*
!*  Copyright (C) 2019 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER NDP
      DOUBLE PRECISION ANGLE
      CHARACTER SIGN*(*)
      INTEGER IDMSF(4)

!*  2Pi
      DOUBLE PRECISION D2PI
      PARAMETER ( D2PI = 6.283185307179586476925287D0 )

!*  Hours to degrees * radians to turns
      DOUBLE PRECISION F
      PARAMETER ( F = 15D0/D2PI )

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Scale then use days to h,m,s routine.
      CALL iau_D2TF ( NDP, ANGLE*F, SIGN, IDMSF )

!*  Finished.

      end

!*+----------------------------------------------------------------------

      SUBROUTINE iau_D2TF ( NDP, DAYS, SIGN, IHMSF )
!*+
!*  - - - - - - - - -
!*   i a u _ D 2 T F
!*  - - - - - - - - -
!*
!*  Decompose days to hours, minutes, seconds, fraction.
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  vector/matrix support routine.
!*
!*  Given:
!*     NDP       i        resolution (Note 1)
!*     DAYS      d        interval in days
!*
!*  Returned:
!*     SIGN      c        '+' or '-'
!*     IHMSF     i(4)     hours, minutes, seconds, fraction
!*
!*  Notes:
!*
!*  1) NDP is interpreted as follows:
!*
!*     NDP         resolution
!*      :      ...0000 00 00
!*     -7         1000 00 00
!*     -6          100 00 00
!*     -5           10 00 00
!*     -4            1 00 00
!*     -3            0 10 00
!*     -2            0 01 00
!*     -1            0 00 10
!*      0            0 00 01
!*      1            0 00 00.1
!*      2            0 00 00.01
!*      3            0 00 00.001
!*      :            0 00 00.000...
!*
!*  2) The largest positive useful value for NDP is determined by the
!*     size of DAYS, the format of DOUBLE PRECISION floating-point
!*     numbers on the target platform, and the risk of overflowing
!*     IHMSF(4).  On a typical platform, for DAYS up to 1D0, the
!*     available floating-point precision might correspond to NDP=12.
!*     However, the practical limit is typically NDP=9, set by the
!*     capacity of a 32-bit IHMSF(4).
!*
!*  3) The absolute value of DAYS may exceed 1D0.  In cases where it
!*     does not, it is up to the caller to test for and handle the
!*     case where DAYS is very nearly 1D0 and rounds up to 24 hours,
!*     by testing for IHMSF(1)=24 and setting IHMSF(1-4) to zero.
!*
!*  This revision:  2005 August 26
!*
!*  SOFA release 2019-07-22
!*
!*  Copyright (C) 2019 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER NDP
      DOUBLE PRECISION DAYS
      CHARACTER SIGN*(*)
      INTEGER IHMSF(4)

!*  Days to seconds
      DOUBLE PRECISION D2S
      PARAMETER ( D2S = 86400D0 )

      INTEGER NRS, N
      DOUBLE PRECISION RS, RM, RH, A, AH, AM, AS, AF

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Handle sign.
      IF ( DAYS .GE. 0D0 ) THEN
         SIGN = '+'
      ELSE
         SIGN = '-'
      END IF

!*  Interval in seconds.
      A = D2S * ABS(DAYS)

!*  Pre-round if resolution coarser than 1 second (then pretend NDP=1).
      IF ( NDP .LT. 0 ) THEN
         NRS = 1
         DO 1 N=1,-NDP
            IF ( N.EQ.2 .OR. N.EQ.4 ) THEN
               NRS = NRS * 6
            ELSE
               NRS = NRS * 10
            END IF
 1       CONTINUE
         RS = DBLE(NRS)
         A = RS * ANINT(A/RS)
      END IF

!*  Express the unit of each field in resolution units.
      NRS = 1
      DO 2 N=1,NDP
         NRS = NRS * 10
 2    CONTINUE
      RS = DBLE(NRS)
      RM = RS * 60D0
      RH = RM * 60D0

!*  Round the interval and express in resolution units.
      A = ANINT(RS*A)

!*  Break into fields.
      AH = AINT(A/RH)
      A = A - AH*RH
      AM = AINT(A/RM)
      A = A - AM*RM
      AS = AINT(A/RS)
      AF = A - AS*RS

!*  Return results.
      IHMSF(1) = NINT(AH)
      IHMSF(2) = NINT(AM)
      IHMSF(3) = NINT(AS)
      IHMSF(4) = NINT(AF)

!*  Finished.

      end
!*+----------------------------------------------------------------------

      SUBROUTINE iau_TAITT ( TAI1, TAI2, TT1, TT2, J )
!*+
!*  - - - - - - - - - -
!*   i a u _ T A I T T
!*  - - - - - - - - - -
!*
!*  Time scale transformation:  International Atomic Time, TAI, to
!*  Terrestrial Time, TT.
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  canonical.
!*
!*  Given:
!*     TAI1,TAI2    d      TAI as a 2-part Julian Date
!*
!*  Returned:
!*     TT1,TT2      d      TT as a 2-part Julian Date
!*     J            i      status:  0 = OK
!*
!*  Note:
!*
!*     TAI1+TAI2 is Julian Date, apportioned in any convenient way
!*     between the two arguments, for example where TAI1 is the Julian
!*     Day Number and TAI2 is the fraction of a day.  The returned
!*     TT1,TT2 follow suit.
!*
!*  References:
!*
!*     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
!*     IERS Technical Note No. 32, BKG (2004)
!*
!*     Explanatory Supplement to the Astronomical Almanac,
!*     P. Kenneth Seidelmann (ed), University Science Books (1992)
!*
!*  This revision:  2010 April 16
!*
!*  SOFA release 2018-01-30
!*
!*  Copyright (C) 2018 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE
      DOUBLE PRECISION TAI1, TAI2, TT1, TT2
      INTEGER J

!*  TT minus TAI (days).
      DOUBLE PRECISION DTAT
      PARAMETER ( DTAT = 32.184D0/86400D0 )

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Result, safeguarding precision.
      IF ( TAI1.GT.TAI2 ) THEN
         TT1 = TAI1
         TT2 = TAI2 + DTAT
      ELSE
         TT1 = TAI1 + DTAT
         TT2 = TAI2
      END IF

!*  Status (always OK).
      J = 0

      end

      SUBROUTINE iau_UTCTAI ( UTC1, UTC2, TAI1, TAI2, J )
!*+
!*  - - - - - - - - - - -
!*   i a u _ U T C T A I
!*  - - - - - - - - - - -
!*
!*  Time scale transformation:  Coordinated Universal Time, UTC, to
!*  International Atomic Time, TAI.
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  canonical.
!*
!*  Given:
!*     UTC1,UTC2    d      UTC as a 2-part quasi Julian Date (Notes 1-4)
!*
!*  Returned:
!*     TAI1,TAI2    d      TAI as a 2-part Julian Date (Note 5)
!*     J            i      status: +1 = dubious year (Note 3)
!*                                  0 = OK
!*                                 -1 = unacceptable date
!*
!*  Notes:
!*
!*  1) UTC1+UTC2 is quasi Julian Date (see Note 2), apportioned in any
!*     convenient way between the two arguments, for example where UTC1
!*     is the Julian Day Number and UTC2 is the fraction of a day.
!*
!*  2) JD cannot unambiguously represent UTC during a leap second unless
!*     special measures are taken.  The convention in the present routine
!*     is that the JD day represents UTC days whether the length is
!*     86399, 86400 or 86401 SI seconds.  In the 1960-1972 era there were
!*     smaller jumps (in either direction) each time the linear UTC(TAI)
!*     expression was changed, and these "mini-leaps" are also included
!*     in the SOFA convention.
!*
!*  3) The warning status "dubious year" flags UTCs that predate the
!*     introduction of the time scale or that are too far in the future
!*     to be trusted.  See iau_DAT for further details.
!*
!*  4) The routine iau_DTF2D converts from calendar date and time of day
!*     into 2-part Julian Date, and in the case of UTC implements the
!*     leap-second-ambiguity convention described above.
!*
!*  5) The returned TAI1,TAI2 are such that their sum is the TAI Julian
!*     Date.
!*
!*  Called:
!*     iau_JD2CAL   JD to Gregorian calendar
!*     iau_DAT      delta(AT) = TAI-UTC
!*     iau_CAL2JD   Gregorian calendar to JD
!*
!*  References:
!*
!*     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
!*     IERS Technical Note No. 32, BKG (2004)
!*
!*     Explanatory Supplement to the Astronomical Almanac,
!*     P. Kenneth Seidelmann (ed), University Science Books (1992)
!*
!*  This revision:  2013 July 26
!*
!*  SOFA release 2018-01-30
!*
!*  Copyright (C) 2018 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE
      DOUBLE PRECISION UTC1, UTC2, TAI1, TAI2
      INTEGER J

!*  Days to seconds
      DOUBLE PRECISION D2S
      PARAMETER ( D2S = 86400D0 )

      LOGICAL BIG1
      INTEGER IY, IM, ID, JS, IYT, IMT, IDT
      DOUBLE PRECISION U1, U2, FD, DAT0, DAT12, W, DAT24, DLOD, DLEAP,Z1, Z2, A2

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Put the two parts of the UTC into big-first order.
      BIG1 = UTC1.GE.UTC2
      IF ( BIG1 ) THEN
         U1 = UTC1
         U2 = UTC2
      ELSE
         U1 = UTC2
         U2 = UTC1
      END IF

!*  Get TAI-UTC at 0h today.
      CALL iau_JD2CAL ( U1, U2, IY, IM, ID, FD, JS )
      IF ( JS.NE.0 ) GO TO 9
      CALL iau_DAT ( IY, IM, ID, 0D0, DAT0, JS )
      IF ( JS.LT.0 ) GO TO 9

!*  Get TAI-UTC at 12h today (to detect drift).
      CALL iau_DAT ( IY, IM, ID, 0.5D0, DAT12, JS )
      IF ( JS.LT.0 ) GO TO 9

!*  Get TAI-UTC at 0h tomorrow (to detect jumps).
      CALL iau_JD2CAL ( U1+1.5D0, U2-FD, IYT, IMT, IDT, W, JS )
      IF ( JS.NE.0 ) GO TO 9
      CALL iau_DAT ( IYT, IMT, IDT, 0D0, DAT24, JS )
      IF ( JS.LT.0 ) GO TO 9

!*  Separate TAI-UTC change into per-day (DLOD) and any jump (DLEAP).
      DLOD = 2D0 * ( DAT12 - DAT0 )
      DLEAP = DAT24 - ( DAT0 + DLOD )

!*  Remove any scaling applied to spread leap into preceding day.
      FD = FD * (D2S+DLEAP)/D2S

!*  Scale from (pre-1972) UTC seconds to SI seconds.
      FD = FD * (D2S+DLOD)/D2S

!*  Today's calendar date to 2-part JD.
      CALL iau_CAL2JD ( IY, IM, ID, Z1, Z2, JS )
      IF ( JS.NE.0 ) GO TO 9

!*  Assemble the TAI result, preserving the UTC split and order.
      A2 = Z1 - U1
      A2 = ( A2 + Z2 ) + ( FD + DAT0/D2S )
      IF ( BIG1 ) THEN
         TAI1 = U1
         TAI2 = A2
      ELSE
         TAI1 = A2
         TAI2 = U1
      END IF

!*  Status.
 9    CONTINUE
      J = JS

      end

      SUBROUTINE iau_D2DTF ( SCALE, NDP, D1, D2, IY, IM, ID, IHMSF, J )
!*+
!*  - - - - - - - - - -
!*   i a u _ D 2 D T F
!*  - - - - - - - - - -
!*
!*  Format for output a 2-part Julian Date (or in the case of UTC a
!*  quasi-JD form that includes special provision for leap seconds).
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  support routine.
!*
!*  Given:
!*     SCALE      c!*(*)  time scale ID (Note 1)
!*     NDP        i      resolution (Note 2)
!*     D1,D2      d      time as a 2-part Julian Date (Notes 3,4)
!*
!*  Returned:
!*     IY,IM,ID   i      year, month, day in Gregorian calendar (Note 5)
!*     IHMSF      i(4)   hours, minutes, seconds, fraction (Note 1)
!*     J          i      status: +1 = dubious year (Note 5)
!*                                0 = OK
!*                               -1 = unacceptable date (Note 6)
!*
!*  Notes:
!*
!*  1) SCALE identifies the time scale.  Only the value 'UTC' (in upper
!*     case) is significant, and enables handling of leap seconds (see
!*     Note 4).
!*
!*  2) NDP is the number of decimal places in the seconds field, and can
!*     have negative as well as positive values, such as:
!*
!*     NDP         resolution
!*     -4            1 00 00
!*     -3            0 10 00
!*     -2            0 01 00
!*     -1            0 00 10
!*      0            0 00 01
!*      1            0 00 00.1
!*      2            0 00 00.01
!*      3            0 00 00.001
!*
!*     The limits are platform dependent, but a safe range is -5 to +9.
!*
!*  3) D1+D2 is Julian Date, apportioned in any convenient way between
!*     the two arguments, for example where D1 is the Julian Day Number
!*     and D2 is the fraction of a day.  In the case of UTC, where the
!*     use of JD is problematical, special conventions apply:  see the
!*     next note.
!*
!*  4) JD cannot unambiguously represent UTC during a leap second unless
!*     special measures are taken.  The SOFA internal convention is that
!*     the quasi-JD day represents UTC days whether the length is 86399,
!*     86400 or 86401 SI seconds.  In the 1960-1972 era there were
!*     smaller jumps (in either direction) each time the linear UTC(TAI)
!*     expression was changed, and these "mini-leaps" are also included
!*     in the SOFA convention.
!*
!*  5) The warning status "dubious year" flags UTCs that predate the
!*     introduction of the time scale or that are too far in the future
!*     to be trusted.  See iau_DAT for further details.
!*
!*  6) For calendar conventions and limitations, see iau_CAL2JD.
!*
!*  Called:
!*     iau_JD2CAL   JD to Gregorian calendar
!*     iau_D2TF     decompose days to hms
!*     iau_DAT      delta(AT) = TAI-UTC
!*
!*  This revision:  2014 February 15
!*
!*  SOFA release 2018-01-30
!*
!*  Copyright (C) 2018 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE
      CHARACTER*(*) SCALE
      INTEGER NDP
      DOUBLE PRECISION D1, D2
      INTEGER IY, IM, ID, IHMSF(4), J

!*  Days to seconds
      DOUBLE PRECISION D2S
      PARAMETER ( D2S = 86400D0 )

      LOGICAL LEAP
      CHARACTER S
      INTEGER IY1, IM1, ID1, JS, IY2, IM2, ID2, IHMSF1(4), I
      DOUBLE PRECISION A1, B1, FD, DAT0, DAT12, W, DAT24, DLEAP

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  The two-part JD.
      A1 = D1
      B1 = D2

!*  Provisional calendar date.
      CALL iau_JD2CAL ( A1, B1, IY1, IM1, ID1, FD, JS )
      IF ( JS.NE.0 ) GO TO 9

!*  Is this a leap second day?
      LEAP = .FALSE.
      IF ( SCALE.EQ.'UTC' ) THEN

!*     TAI-UTC at 0h today.
         CALL iau_DAT ( IY1, IM1, ID1, 0D0, DAT0, JS )
         IF ( JS.LT.0 ) GO TO 9

!*     TAI-UTC at 12h today (to detect drift).
         CALL iau_DAT ( IY1, IM1, ID1, 0.5D0, DAT12, JS )
         IF ( JS.LT.0 ) GO TO 9

!*     TAI-UTC at 0h tomorrow (to detect jumps).
         CALL iau_JD2CAL ( A1+1.5D0, B1-FD, IY2, IM2, ID2, W, JS )
         IF ( JS.NE.0 ) GO TO 9
         CALL iau_DAT ( IY2, IM2, ID2, 0D0, DAT24, JS )
         IF ( JS.LT.0 ) GO TO 9

!*     Any sudden change in TAI-UTC (seconds).
         DLEAP = DAT24 - ( 2D0 * DAT12 - DAT0 )

!*     If leap second day, scale the fraction of a day into SI.
         LEAP = DLEAP.NE.0D0
         IF ( LEAP ) FD = FD + FD*DLEAP/D2S

      END IF

!*  Provisional time of day.
      CALL iau_D2TF ( NDP, FD, S, IHMSF1 )

!*  Has the (rounded) time gone past 24h?
      IF ( IHMSF1(1).GT.23 ) THEN

!*     Yes.  We probably need tomorrow's calendar date.
         CALL iau_JD2CAL ( A1+1.5D0, B1-FD, IY2, IM2, ID2, W, JS )
         IF ( JS.LT.0 ) GO TO 9

!*     Is today a leap second day?
         IF ( .NOT. LEAP ) THEN

!*        No.  Use 0h tomorrow.
            IY1 = IY2
            IM1 = IM2
            ID1 = ID2
            IHMSF1(1) = 0
            IHMSF1(2) = 0
            IHMSF1(3) = 0

         ELSE

!*        Yes.  Are we past the leap second itself?
            IF ( IHMSF1(3).GT.0 ) THEN

!*           Yes.  Use tomorrow but allow for the leap second.
               IY1 = IY2
               IM1 = IM2
               ID1 = ID2
               IHMSF1(1) = 0
               IHMSF1(2) = 0
               IHMSF1(3) = 0

            ELSE

!*           No.  Use 23 59 60... today.
               IHMSF1(1) = 23
               IHMSF1(2) = 59
               IHMSF1(3) = 60
            END IF

!*        If rounding to 10s or coarser always go up to new day.
            IF ( NDP.LT.0 .AND. IHMSF1(3).EQ.60 ) THEN
               IY1 = IY2
               IM1 = IM2
               ID1 = ID2
               IHMSF1(1) = 0
               IHMSF1(2) = 0
               IHMSF1(3) = 0
            END IF
         END IF
      END IF

!*  Results.
      IY = IY1
      IM = IM1
      ID = ID1
      DO 2 I=1,4
         IHMSF(I) = IHMSF1(I)
 2    CONTINUE

!*  Status.
 9    CONTINUE
      J = JS

      end

      SUBROUTINE iau_JD2CAL ( DJ1, DJ2, IY, IM, ID, FD, J )
!*+
!*  - - - - - - - - - - -
!*   i a u _ J D 2 C A L
!*  - - - - - - - - - - -
!*
!*  Julian Date to Gregorian year, month, day, and fraction of a day.
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  support routine.
!*
!*  Given:
!*     DJ1,DJ2     d     Julian Date (Notes 1, 2)
!*
!*  Returned:
!*     IY          i     year
!*     IM          i     month
!*     ID          i     day
!*     FD          d     fraction of day
!*     J           i     status:
!*                           0 = OK
!*                          -1 = unacceptable date (Note 1)
!*
!*  Notes:
!*
!*  1) The earliest valid date is -68569.5 (-4900 March 1).  The
!*     largest value accepted is 10^9.
!*
!*  2) The Julian Date is apportioned in any convenient way between
!*     the arguments DJ1 and DJ2.  For example, JD=2450123.7 could
!*     be expressed in any of these ways, among others:
!*
!*             DJ1            DJ2
!*
!*         2450123.7D0        0D0        (JD method)
!*          2451545D0      -1421.3D0     (J2000 method)
!*         2400000.5D0     50123.2D0     (MJD method)
!*         2450123.5D0       0.2D0       (date & time method)
!*
!*  3) In early eras the conversion is from the "Proleptic Gregorian
!*     Calendar";  no account is taken of the date(s) of adoption of
!*     the Gregorian Calendar, nor is the AD/BC numbering convention
!*     observed.
!*
!*  Reference:
!*
!*     Explanatory Supplement to the Astronomical Almanac,
!*     P. Kenneth Seidelmann (ed), University Science Books (1992),
!*     Section 12.92 (p604).
!*
!*  This revision:  2017 January 12
!*
!*  SOFA release 2018-01-30
!*
!*  Copyright (C) 2018 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DJ1, DJ2
      INTEGER IY, IM, ID
      DOUBLE PRECISION FD
      INTEGER J

!*  Minimum and maximum allowed JD
      DOUBLE PRECISION DJMIN, DJMAX
      PARAMETER ( DJMIN = -68569.5D0, DJMAX = 1D9 )

      INTEGER JD, L, N, I, K
      DOUBLE PRECISION DJ, D1, D2, F1, F2, F, D

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Check if date is acceptable.
      DJ = DJ1 + DJ2
      IF ( DJ.LT.DJMIN .OR. DJ.GT.DJMAX ) THEN
         J = -1
      ELSE
         J = 0

!*     Copy the date, big then small, and re-align to midnight.
         IF ( DJ1 .GE. DJ2 ) THEN
            D1 = DJ1
            D2 = DJ2
         ELSE
            D1 = DJ2
            D2 = DJ1
         END IF
         D2 = D2 - 0.5D0

!*     Separate day and fraction.
         F1 = MOD(D1,1D0)
         F2 = MOD(D2,1D0)
         F = MOD(F1+F2,1D0)
         IF ( F .LT. 0D0 ) F = F+1D0
         D = ANINT(D1-F1) + ANINT(D2-F2) + ANINT(F1+F2-F)
         JD = NINT(D) + 1

!*     Express day in Gregorian calendar.
         L = JD + 68569
         N = ( 4*L ) / 146097
         L = L - ( 146097*N + 3 ) / 4
         I = ( 4000 * (L+1) ) / 1461001
         L = L - ( 1461*I ) / 4 + 31
         K = ( 80*L ) / 2447
         ID = L - ( 2447*K ) / 80
         L = K / 11
         IM = K + 2 - 12*L
         IY = 100 * ( N-49 ) + I + L

         FD = F
      END IF

      end

      SUBROUTINE iau_DAT ( IY, IM, ID, FD, DELTAT, J )
!*+
!*  - - - - - - - -
!*   i a u _ D A T
!*  - - - - - - - -
!*
!*  For a given UTC date, calculate Delta(AT) = TAI-UTC.
!*
!*     :------------------------------------------:
!*     :                                          :
!*     :                 IMPORTANT                :
!*     :                                          :
!*     :  A new version of this routine must be   :
!*     :  produced whenever a new leap second is  :
!*     :  announced.  There are five items to     :
!*     :  change on each such occasion:           :
!*     :                                          :
!*     :  1) The parameter NDAT must be           :
!*     :     increased by 1.                      :
!*     :                                          :
!*     :  2) The set of DATA statements that      :
!*     :     initialize the arrays IDAT and       :
!*     :     DATS must be extended by one line.   :
!*     :                                          :
!*     :  3) The parameter IYV must be set to     :
!*     :     the current year.                    :
!*     :                                          :
!*     :  4) The "Latest leap second" comment     :
!*     :     below must be set to the new leap    :
!*     :     second date.                         :
!*     :                                          :
!*     :  5) The "This revision" comment, later,  :
!*     :     must be set to the current date.     :
!*     :                                          :
!*     :  Change (3) must also be carried out     :
!*     :  whenever the routine is re-issued,      :
!*     :  even if no leap seconds have been       :
!*     :  added.                                  :
!*     :                                          :
!*     :  Latest leap second:  2016 December 31   :
!*     :                                          :
!*     :__________________________________________:
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  user-replaceable support routine.
!*
!*  Given:
!*     IY       i     UTC:  year (Notes 1 and 2)
!*     IM       i           month (Note 2)
!*     ID       i           day (Notes 2 and 3)
!*     FD       d           fraction of day (Note 4)
!*
!*  Returned:
!*     DELTAT   d     TAI minus UTC, seconds
!*     J        i     status (Note 5):
!*                       1 = dubious year (Note 1)
!*                       0 = OK
!*                      -1 = bad year
!*                      -2 = bad month
!*                      -3 = bad day (Note 3)
!*                      -4 = bad fraction (Note 4)
!*                      -5 = internal error (Note 5)
!*
!*  Notes:
!*
!*  1) UTC began at 1960 January 1.0 (JD 2436934.5) and it is improper
!*     to call the routine with an earlier date.  If this is attempted,
!*     zero is returned together with a warning status.
!*
!*     Because leap seconds cannot, in principle, be predicted in
!*     advance, a reliable check for dates beyond the valid range is
!*     impossible.  To guard against gross errors, a year five or more
!*     after the release year of the present routine (see parameter IYV)
!*     is considered dubious.  In this case a warning status is returned
!*     but the result is computed in the normal way.
!*
!*     For both too-early and too-late years, the warning status is J=+1.
!*     This is distinct from the error status J=-1, which signifies a
!*     year so early that JD could not be computed.
!*
!*  2) If the specified date is for a day which ends with a leap second,
!*     the TAI-UTC value returned is for the period leading up to the
!*     leap second.  If the date is for a day which begins as a leap
!*     second ends, the TAI-UTC returned is for the period following the
!*     leap second.
!*
!*  3) The day number must be in the normal calendar range, for example
!*     1 through 30 for April.  The "almanac" convention of allowing
!*     such dates as January 0 and December 32 is not supported in this
!*     routine, in order to avoid confusion near leap seconds.
!*
!*  4) The fraction of day is used only for dates before the introduction
!*     of leap seconds, the first of which occurred at the end of 1971.
!*     It is tested for validity (0 to 1 is the valid range) even if not
!*     used;  if invalid, zero is used and status J=-4 is returned.  For
!*     many applications, setting FD to zero is acceptable;  the
!*     resulting error is always less than 3 ms (and occurs only
!*     pre-1972).
!*
!*  5) The status value returned in the case where there are multiple
!*     errors refers to the first error detected.  For example, if the
!*     month and day are 13 and 32 respectively, J=-2 (bad month) will be
!*     returned.  The "internal error" status refers to a case that is
!*     impossible but causes some compilers to issue a warning.
!*
!*  6) In cases where a valid result is not available, zero is returned.
!*
!*  References:
!*
!*  1) For dates from 1961 January 1 onwards, the expressions from the
!*     file ftp://maia.usno.navy.mil/ser7/tai-utc.dat are used.
!*
!*  2) The 5ms timestep at 1961 January 1 is taken from 2.58.1 (p87) of
!*     the 1992 Explanatory Supplement.
!*
!*  Called:
!*     iau_CAL2JD   Gregorian calendar to JD
!*
!*  This revision:  2019 July 5
!*
!*  SOFA release 2019-07-22
!*
!*  Copyright (C) 2019 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER IY, IM, ID
      DOUBLE PRECISION FD, DELTAT
      INTEGER J

!*  Release year for this version of iau_DAT
      INTEGER IYV
      PARAMETER ( IYV = 2019 )

!*  Number of Delta(AT) changes (increase by 1 for each new leap second)
      INTEGER NDAT
      PARAMETER ( NDAT = 42 )

!*  Number of Delta(AT) expressions before leap seconds were introduced
      INTEGER NERA1
      PARAMETER ( NERA1 = 14 )

!*  Dates (year, month) on which new Delta(AT) came into force
      INTEGER IDAT(2,NDAT)

!*  New Delta(AT) which came into force on the given dates
      DOUBLE PRECISION DATS(NDAT)

!*  Reference dates (MJD) and drift rates (s/day), pre leap seconds
      DOUBLE PRECISION DRIFT(2,NERA1)

!*  Miscellaneous local variables
      LOGICAL MORE
      INTEGER JS, M, N, IS
      DOUBLE PRECISION DA, DJM0, DJM

!*  Dates, Delta(AT)s, reference dates, and drift rates
      DATA ((IDAT(M,N),M=1,2),DATS(N),(DRIFT(M,N),M=1,2),N=1,14)&
       / 1960,  1,  1.4178180D0, 37300D0, 0.001296D0,&
         1961,  1,  1.4228180D0, 37300D0, 0.001296D0,&
         1961,  8,  1.3728180D0, 37300D0, 0.001296D0,&
         1962,  1,  1.8458580D0, 37665D0, 0.0011232D0,&
         1963, 11,  1.9458580D0, 37665D0, 0.0011232D0,&
         1964,  1,  3.2401300D0, 38761D0, 0.001296D0,&
         1964,  4,  3.3401300D0, 38761D0, 0.001296D0,&
         1964,  9,  3.4401300D0, 38761D0, 0.001296D0,&
         1965,  1,  3.5401300D0, 38761D0, 0.001296D0,&
         1965,  3,  3.6401300D0, 38761D0, 0.001296D0,&
         1965,  7,  3.7401300D0, 38761D0, 0.001296D0,&
         1965,  9,  3.8401300D0, 38761D0, 0.001296D0,&
         1966,  1,  4.3131700D0, 39126D0, 0.002592D0,&
         1968,  2,  4.2131700D0, 39126D0, 0.002592D0 /

!*  Dates and Delta(AT)s
      DATA ((IDAT(M,N),M=1,2),DATS(N),N=15,30)&
       / 1972,  1, 10D0,&
         1972,  7, 11D0,&
         1973,  1, 12D0,&
         1974,  1, 13D0,&
         1975,  1, 14D0,&
         1976,  1, 15D0,&
         1977,  1, 16D0,&
         1978,  1, 17D0,&
         1979,  1, 18D0,&
         1980,  1, 19D0,&
         1981,  7, 20D0,&
         1982,  7, 21D0,&
         1983,  7, 22D0,&
         1985,  7, 23D0,&
         1988,  1, 24D0,&
         1990,  1, 25D0 /

      DATA ((IDAT(M,N),M=1,2),DATS(N),N=31,NDAT)&
       / 1991,  1, 26D0,&
         1992,  7, 27D0,&
         1993,  7, 28D0,&
         1994,  7, 29D0,&
         1996,  1, 30D0,&
         1997,  7, 31D0,&
         1999,  1, 32D0,&
         2006,  1, 33D0,&
         2009,  1, 34D0,&
         2012,  7, 35D0,&
         2015,  7, 36D0,&
         2017,  1, 37D0 /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Initialize the result to zero and the status to OK.
      DA = 0D0
      JS = 0

!*  If invalid fraction of a day, set error status and give up.
      IF ( FD.LT.0D0 .OR. FD.GT.1D0 ) THEN
         JS = -4
         GO TO 9000
      END IF

!*  Convert the date into an MJD.
      CALL iau_CAL2JD ( IY, IM, ID, DJM0, DJM, JS )

!*  If invalid year, month, or day, give up.
      IF ( JS .LT. 0 ) GO TO 9000

!*  If pre-UTC year, set warning status and give up.
      IF ( IY .LT. IDAT(1,1) ) THEN
         JS = 1
         GO TO 9000
      END IF

!*  If suspiciously late year, set warning status but proceed.
      IF ( IY .GT. IYV+5 ) JS = 1

!*  Combine year and month.
      M = 12*IY+IM

!*  Find the most recent table entry.
      IS = 0
      MORE = .TRUE.
      DO 1 N=NDAT,1,-1
         IF ( MORE ) THEN
            IS = N
            MORE = M .LT. ( 12*IDAT(1,N) + IDAT(2,N) )
         END IF
 1    CONTINUE

!*  Prevent underflow warnings.
      IF ( IS .LT. 1 ) THEN
         JS = -5
         GO TO 9000
      END IF

!*  Get the Delta(AT).
      DA = DATS(IS)

!*  If pre-1972, adjust for drift.
      IF ( IS .LE. NERA1 ) DA = DA +( DJM + FD - DRIFT(1,IS) ) * DRIFT(2,IS)

!*  Return the Delta(AT) value and the status.
 9000 CONTINUE
      DELTAT = DA
      J = JS

!*  Finished.
      end
!*+----------------------------------------------------------------------

      SUBROUTINE iau_CAL2JD ( IY, IM, ID, DJM0, DJM, J )
!*+
!*  - - - - - - - - - - -
!*   i a u _ C A L 2 J D
!*  - - - - - - - - - - -
!*
!*  Gregorian Calendar to Julian Date.
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  support routine.
!*
!*  Given:
!*     IY,IM,ID    i     year, month, day in Gregorian calendar (Note 1)
!*
!*  Returned:
!*     DJM0        d     MJD zero-point: always 2400000.5
!*     DJM         d     Modified Julian Date for 0 hrs
!*     J           i     status:
!*                           0 = OK

!!*                          -1 = bad year   (Note 3: JD not computed)
!*                          -2 = bad month  (JD not computed)
!*                          -3 = bad day    (JD computed)
!*
!*  Notes:
!*
!*  1) The algorithm used is valid from -4800 March 1, but this
!*     implementation rejects dates before -4799 January 1.
!*
!*  2) The Julian Date is returned in two pieces, in the usual SOFA
!*     manner, which is designed to preserve time resolution.  The
!*     Julian Date is available as a single number by adding DJM0 and
!*     DJM.
!*
!*  3) In early eras the conversion is from the "Proleptic Gregorian
!*     Calendar";  no account is taken of the date(s) of adoption of
!*     the Gregorian Calendar, nor is the AD/BC numbering convention
!*     observed.
!*
!*  Reference:
!*
!*     Explanatory Supplement to the Astronomical Almanac,
!*     P. Kenneth Seidelmann (ed), University Science Books (1992),
!*     Section 12.92 (p604).
!*
!*  This revision:  2014 November 7
!*
!*  SOFA release 2018-01-30
!*
!*  Copyright (C) 2018 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER IY, IM, ID
      DOUBLE PRECISION DJM0, DJM

      INTEGER J, NDAYS, MY, IYPMY

!*  Earliest year allowed (4800BC)
      INTEGER IYMIN
      PARAMETER ( IYMIN = -4799 )

!*  Month lengths in days
      INTEGER MTAB(12)
      DATA MTAB / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Preset status.
      J = 0

!*  Validate year.
      IF ( IY.LT.IYMIN ) THEN
         J = -1
      ELSE

!*     Validate month.
         IF ( IM.GE.1 .AND. IM.LE.12 ) THEN

!*        Days in current month.
            NDAYS = MTAB(IM)

!*        Allow for leap year.
            IF ( IM .EQ. 2 ) THEN
               IF ( MOD(IY,4) .EQ. 0 ) NDAYS = 29
               IF ( MOD(IY,100).EQ.0 .AND. MOD(IY,400).NE.0 ) NDAYS = 28
            END IF

!*        Validate day.
            IF ( ID.LT.1 .OR. ID.GT.NDAYS ) J = -3

!*        Result.
            MY = ( IM - 14 ) / 12
            IYPMY = IY + MY
            DJM0 = 2400000.5D0
            DJM = DBLE( ( 1461 * ( IYPMY + 4800 ) ) / 4&
                     + (  367 * ( IM-2 - 12*MY ) ) / 12&
                     - (    3 * ( ( IYPMY + 4900 ) / 100 ) ) / 4&
                     + ID - 2432076)

!*        Bad month
         ELSE
            J = -2
         END IF
      END IF
      END


      SUBROUTINE iau_DTF2D ( SCALE, IY, IM, ID, IHR, IMN, SEC, D1, D2, J )
!*+
!*  - - - - - - - - - -
!*   i a u _ D T F 2 D
!*  - - - - - - - - - -
!*
!*  Encode date and time fields into 2-part Julian Date (or in the case
!*  of UTC a quasi-JD form that includes special provision for leap
!*  seconds).
!*
!*  This routine is part of the International Astronomical Union's
!*  SOFA (Standards of Fundamental Astronomy) software collection.
!*
!*  Status:  support routine.
!*
!*  Given:
!*     SCALE      c!*(*)  time scale ID (Note 1)
!*     IY,IM,ID   i      year, month, day in Gregorian calendar (Note 2)
!*     IHR,IMN    i      hour, minute
!*     SEC        d      seconds
!*
!*  Returned:
!*     D1,D2      d      2-part Julian Date (Notes 3,4)
!*     J          i      status: +3 = both of next two
!*                               +2 = time is after end of day (Note 5)
!*                               +1 = dubious year (Note 6)
!*                                0 = OK
!*                               -1 = bad year
!*                               -2 = bad month
!*                               -3 = bad day
!*                               -4 = bad hour
!*                               -5 = bad minute
!*                               -6 = bad second (<0)
!*
!*  Notes:
!*
!*  1) SCALE identifies the time scale.  Only the value 'UTC' (in upper
!*     case) is significant, and enables handling of leap seconds (see
!*     Note 4).
!*
!*  2) For calendar conventions and limitations, see iau_CAL2JD.
!*
!*  3) The sum of the results, D1+D2, is Julian Date, where normally D1
!*     is the Julian Day Number and D2 is the fraction of a day.  In the
!*     case of UTC, where the use of JD is problematical, special
!*     conventions apply:  see the next note.
!*
!*  4) JD cannot unambiguously represent UTC during a leap second unless
!*     special measures are taken.  The SOFA internal convention is that
!*     the quasi-JD day represents UTC days whether the length is 86399,
!*     86400 or 86401 SI seconds.  In the 1960-1972 era there were
!*     smaller jumps (in either direction) each time the linear UTC(TAI)
!*     expression was changed, and these "mini-leaps" are also included
!*     in the SOFA convention.
!*
!*  5) The warning status "time is after end of day" usually means that
!*     the SEC argument is greater than 60D0.  However, in a day ending
!*     in a leap second the limit changes to 61D0 (or 59D0 in the case of
!*     a negative leap second).
!*
!*  6) The warning status "dubious year" flags UTCs that predate the
!*     introduction of the time scale or that are too far in the future
!*     to be trusted.  See iau_DAT for further details.
!*
!*  7) Only in the case of continuous and regular time scales (TAI, TT,
!*     TCG, TCB and TDB) is the result D1+D2 a Julian Date, strictly
!*     speaking.  In the other cases (UT1 and UTC) the result must be
!*     used with circumspection;  in particular the difference between
!*     two such results cannot be interpreted as a precise time
!*     interval.
!*
!*  Called:
!*     iau_CAL2JD   Gregorian calendar to JD
!*     iau_DAT      delta(AT) = TAI-UTC
!*     iau_JD2CAL   JD to Gregorian calendar
!*
!*  This revision:  2013 July 26
!*
!*  SOFA release 2019-07-22
!*
!*  Copyright (C) 2019 IAU SOFA Board.  See notes at end.
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE
      CHARACTER*(*) SCALE
      INTEGER IY, IM, ID, IHR, IMN
      DOUBLE PRECISION SEC, D1, D2
      INTEGER J

!*  Days to seconds
      DOUBLE PRECISION D2S
      PARAMETER ( D2S = 86400D0 )

      INTEGER JS, IY2, IM2, ID2
      DOUBLE PRECISION DJ, W, DAY, SECLIM, DAT0, DAT12, DAT24,DLEAP, TIME

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Today's Julian Day Number.
      CALL iau_CAL2JD ( IY, IM, ID, DJ, W, JS )
      IF ( JS.NE.0 ) GO TO 9
      DJ = DJ + W

!*  Day length and final minute length in seconds (provisional).
      DAY = D2S
      SECLIM = 60D0

!*  Deal with the UTC leap second case.
      IF ( SCALE.EQ.'UTC' ) THEN

!*     TAI-UTC at 0h today.
         CALL iau_DAT ( IY, IM, ID, 0D0, DAT0, JS )
         IF ( JS.LT.0 ) GO TO 9

!*     TAI-UTC at 12h today (to detect drift).
         CALL iau_DAT ( IY, IM, ID, 0.5D0, DAT12, JS )
         IF ( JS.LT.0 ) GO TO 9

!*     TAI-UTC at 0h tomorrow (to detect jumps).
         CALL iau_JD2CAL ( DJ, 1.5D0, IY2, IM2, ID2, W, JS )
         IF ( JS.NE.0 ) GO TO 9
         CALL iau_DAT ( IY2, IM2, ID2, 0D0, DAT24, JS )
         IF ( JS.LT.0 ) GO TO 9

!*     Any sudden change in TAI-UTC between today and tomorrow.
         DLEAP = DAT24 - ( 2D0 * DAT12 - DAT0 )

!*     If leap second day, correct the day and final minute lengths.
         DAY = DAY + DLEAP
         IF ( IHR.EQ.23 .AND. IMN.EQ.59 ) SECLIM = SECLIM + DLEAP

!*     End of UTC-specific actions.
      END IF

!*  Validate the time.
      IF ( IHR.GE.0 .AND. IHR.LE.23 ) THEN
         IF ( IMN.GE.0 .AND. IMN.LE.59 ) THEN
            IF ( SEC.GE.0D0 ) THEN
               IF ( SEC.GE.SECLIM ) THEN
                  JS = JS + 2
               END IF
            ELSE
               JS = -6
            END IF
         ELSE
            JS = -5
         END IF
      ELSE
         JS = -4
      END IF
      IF ( JS.LT.0 ) GO TO 9

!*  The time in days.
      TIME = (60D0*DBLE(60*IHR+IMN)+SEC) / DAY

!*  Return the date and time.
      D1 = DJ
      D2 = TIME

!*  Return the status.
 9    CONTINUE
      J = JS

!*  Finished.
      end

end module
