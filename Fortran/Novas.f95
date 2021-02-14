
Module NOVAS

    implicit none

    real(kind=8), parameter ::  PI = 3.141592653589793238462643D0
      ! Degree to radian coefficient    3.14159265358979323846264338327950288419716939937510582
    real(kind=8), parameter ::  DEGRAD = PI/180.D0

    contains

   SUBROUTINE JULDAT (I,M,K,H,TJD)
!      This code is from NOVAS package
!*     THIS SUBROUTINE COMPUTES JULIAN DATE, GIVEN CALENDAR DATE AND
!*     TIME.  INPUT CALENDAR DATE MUST BE GREGORIAN.  INPUT TIME VALUE
!*     CAN BE IN ANY UT-LIKE TIME SCALE (UTC, UT1, TT, ETC.) - OUTPUT
!*     JULIAN DATE WILL HAVE SAME BASIS.  ALGORITHM BY FLIEGEL AND
!*     VAN FLANDERN.
!*
!*          I      = YEAR (IN)
!*          M      = MONTH NUMBER (IN)
!*          K      = DAY OF MONTH (IN)
!*          H      = UT HOURS (IN)
!*          TJD    = JULIAN DATE (OUT)
!*
!*
      real(kind=8) :: H , TJD , JD
      integer      ::I , M , K

!*     JD=JULIAN DAY NO FOR DAY BEGINNING AT GREENWICH NOON ON GIVEN DATE
      JD = K-32075+1461*(I+4800+(M-14)/12)/4+367*(M-2-(M-14)/12*12)/12 &
          -3*((I+4900+(M-14)/12)/100)/4

      TJD = JD - 0.5D0 + H/24.D0

      RETURN
END subroutine

!----------------------------------------------------------------------

      SUBROUTINE RCALDAT (TJD,I,M,K,H)
!*
!*     THIS SUBROUTINE COMPUTES CALENDAR DATE AND TIME, GIVEN JULIAN
!*     DATE.  INPUT JULIAN DATE CAN BE BASED ON ANY UT-LIKE TIME SCALE
!*     (UTC, UT1, TT, ETC.) - OUTPUT TIME VALUE WILL HAVE SAME BASIS.
!*     OUTPUT CALENDAR DATE WILL BE GREGORIAN.  ALGORITHM BY FLIEGEL AND
!*     VAN FLANDERN.
!*
!*          TJD    = JULIAN DATE (IN)
!*          I      = YEAR (OUT)
!*          M      = MONTH NUMBER (OUT)
!*          K      = DAY OF MONTH (OUT)
!*          H      = UT HOURS (OUT)
!*
!*
      real(kind=8) :: TJD,H,DJD,DMOD
      integer :: I,M,K,L,N,JD

      DJD = TJD + 0.5D0
      H = DMOD (DJD,1.D0) * 24.D0
      JD = int(DJD)
!*     JD=JULIAN DAY NO FOR DAY BEGINNING AT GREENWICH NOON ON GIVEN DATE
      L = JD + 68569
      N = 4*L/146097
      L = L - (146097*N+3)/4
!*     I=YEAR, M=MONTH, K=DAY
      I = 4000*(L+1)/1461001
      L = L - 1461*I/4 + 31
      M = 80*L/2447
      K = L - 2447*M/80
      L = M / 11
      M = M + 2 - 12*L
      I = 100*(N-49) + I + L

      RETURN
      END subroutine

    SUBROUTINE ASTCON (NAME,FACTOR,CONST)
!*  This code is from NOVAS package
!     THIS SUBROUTINE SUPPLIES THE VALUES OF ASTRONOMICAL CONSTANTS.
!*
!*         NAME   = NAME OF CONSTANT WHOSE VALUE IS DESIRED (IN)
!*                  'C'         SPEED OF LIGHT IN METERS/SECOND
!*                  'C(AU/DAY)' SPEED OF LIGHT IN AU/DAY
!*                  'AU'        LENGTH OF ASTRONOMICAL UNIT IN METERS
!*                  'AU(SEC)'   LENGTH OF ASTRONOMICAL UNIT IN SECONDS
!*                  'GS'        HELIOCENTRIC GRAVITATIONAL CONSTANT
!*                                 IN METERS**3/SECOND**2
!*                  'GE'        GEOCENTRIC GRAVITATIONAL CONSTANT
!*                                 IN METERS**3/SECOND**2
!*                  'ERAD'      EQUATORIAL RADIUS OF EARTH IN METERS
!*                  'F'         FLATTENING FACTOR OF EARTH
!*                  'ANGVEL'    NOMINAL MEAN ROTATIONAL ANGULAR VELOCITY
!*                                 OF EARTH IN RADIANS/SECOND
!*                  'MASS_SUN'  RECIPROCAL MASS OF THE SUN
!*                  'MASS_EAR'  RECIPROCAL MASS OF THE EARTH
!*                  'MASS_MOO'  RECIPROCAL MASS OF THE MOON
!*                  'MASS_MER'  RECIPROCAL MASS OF MERCURY
!*                      :             :      :        :
!*                  'MASS_PLU'  RECIPROCAL MASS OF PLUTO
!*         FACTOR = FACTOR BY WHICH CONSTANT VALUE IS TO BE MULTIPLIED
!*                  (IN)
!*         CONST  = CONSTANT VALUE AFTER MULTIPLICATION BY FACTOR (OUT)
!*
!*
      real(kind=8) :: FACTOR,CONST,C,AUSEC
      CHARACTER NAME*(*)

!*     NOTE:  THESE CONSTANT VALUES ARE BASED ON THE TDB SECOND WHERE
!*     APPLICABLE.

!*     SPEED OF LIGHT IN METERS/SECOND IS A DEFINING PHYSICAL CONSTANT
      DATA C / 299792458.D0 /

!*     LIGHT-TIME FOR ONE ASTRONOMICAL UNIT IN SECONDS, FROM DE-405
      DATA AUSEC / 499.0047838061D0 /

!*     SPEED OF LIGHT IN METERS/SECOND
      IF ( NAME == 'C' ) THEN
          CONST = C

!*     SPEED OF LIGHT IN AU/DAY
      ELSE IF ( NAME == 'C(AU/DAY)' ) THEN
          CONST = 86400.D0 / AUSEC

!*     LENGTH OF ASTRONOMICAL UNIT IN METERS
      ELSE IF ( NAME == 'AU' ) THEN
          CONST = AUSEC * C

!*     LENGTH OF ASTRONOMICAL UNIT IN SECONDS
      ELSE IF ( NAME == 'AU(SEC)' ) THEN
          CONST = AUSEC

!*     HELIOCENTRIC GRAVITATIONAL CONSTANT IN METERS**3/SECOND**2, FROM
!*     DE-405
      ELSE IF ( NAME == 'GS' ) THEN
          CONST = 1.32712440017987D20

!*     GEOCENTRIC GRAVITATIONAL CONSTANT IN METERS**3/SECOND**2, FROM
!*     DE-405
      ELSE IF ( NAME == 'GE' ) THEN
          CONST = 3.98600433D14

!*     EQUATORIAL RADIUS OF EARTH IN METERS, FROM IERS CONVENTIONS (2003)
      ELSE IF ( NAME == 'ERAD' ) THEN
          CONST = 6378136.6D0

!*     FLATTENING FACTOR OF EARTH, FROM IERS CONVENTIONS (2003)
      ELSE IF ( NAME == 'F' ) THEN
          CONST = 1.D0 / 298.25642D0

!*     NOMINAL MEAN ROTATIONAL ANGULAR VELOCITY OF EARTH
!*     IN RADIANS/SECOND, FROM IERS CONVENTIONS (2003)
      ELSE IF ( NAME == 'ANGVEL' ) THEN
          CONST = 7.2921150D-5

!*     RECIPROCAL MASSES OF SOLAR SYSTEM BODIES, FROM DE-405
!*     (SUN MASS / BODY MASS)
      ELSE IF ( NAME(1:4) == 'MASS' ) THEN

          CONST = 1.D0
          IF ( NAME(6:8) == 'SUN' ) CONST =         1.D0
          IF ( NAME(6:8) == 'MOO' ) CONST =  27068700.387534D0
          IF ( NAME(6:8) == 'MER' ) CONST =   6023600.D0
          IF ( NAME(6:8) == 'VEN' ) CONST =    408523.71D0
          IF ( NAME(6:8) == 'EAR' ) CONST =    332946.050895D0
          IF ( NAME(6:8) == 'MAR' ) CONST =   3098708.D0
          IF ( NAME(6:8) == 'JUP' ) CONST =      1047.3486D0
          IF ( NAME(6:8) == 'SAT' ) CONST =      3497.898D0
          IF ( NAME(6:8) == 'URA' ) CONST =     22902.98D0
          IF ( NAME(6:8) == 'NEP' ) CONST =     19412.24D0
          IF ( NAME(6:8) == 'PLU' ) CONST = 135200000.D0
          IF ( NAME(6:8) == 'EMB' ) CONST =    328900.561400D0

      END IF

      CONST = CONST * FACTOR

      RETURN

      END subroutine

!--------------------------------------------------------------------------

      SUBROUTINE REFRAC (HEIGHT,ZDOBS,POBS,TOBS,REFR)
!*     This code is from NOVAS package
!*     THIS SUBROUTINE COMPUTES ATMOSPHERIC REFRACTION IN ZENITH
!*     DISTANCE.  THIS VERSION COMPUTES APPROXIMATE REFRACTION FOR
!*     OPTICAL WAVELENGTHS.  IT CAN BE USED FOR PLANNING OBSERVATIONS
!*     OR TELESCOPE POINTING, BUT SHOULD NOT BE USED FOR THE REDUCTION
!*     OF PRECISE OBSERVATIONS.  BASIC ALGORITHM IS DESCRIBED IN THE
!*     EXPLANATORY SUPPLEMENT TO THE ASTRONOMICAL ALMANAC, P. 144,
!*     AND IS AN ADAPTATION OF A FORMULA IN BENNETT (1982), JOURNAL
!*     OF NAVIGATION (ROYAL INSTITUTE) 35, 255-259.
!*
!*          HEIGHT = HEIGHT OF OBSERVER IN METERS (IN)
!*          ZDOBS  = OBSERVED ZENITH DISTANCE IN DEGREES (IN)
!*          OBSP   = OBSERVED ATMOSPHERIC PRESSURE IN MILLIBARS (IN)
!*          OBST   = OBSERVED TEMPERATURE IN DEGREES CELSIUS (IN)
!*          OBSD   = OBSERVED DEW POINT IN DEGREES CELSIUS (IN)
!*          OBSWL  = OBSERVING WAVELENGTH IN MICRONS (IN)
!*
!*          REFR   = ATMOSPHERIC REFRACTION IN DEGREES (OUT)
!*
!*     NOTE:  HEIGHT IS NOT USED IF ENTRY REFDAT HAS BEEN CALLED
!*     TO SPECIFY ATMOSPHERIC PRESSURE.
!*

      real(kind=8) :: HEIGHT,ZDOBS,REFR,S
      real(kind=8) :: POBS,TOBS!,DOBS,WLOBS
      real(kind=8) :: P,T,D,WL,H,R,DEXP,DTAN

!*     S IS APPROXIMATE SCALE HEIGHT OF ATMOSPHERE IN METERS
      DATA S / 9.1D3 /
!      DATA POBS, TOBS, DOBS, WLOBS / 4 * -999.D0 /

!*     COMPUTE REFRACTION ONLY FOR ZENITH DISTANCES
!*     BETWEEN 0.1 AND 91 DEGREES

      IF ( ZDOBS < 0.1D0 .OR. ZDOBS .GT. 91.D0 ) THEN
          REFR = 0.D0
          GO TO 77
      END IF

!*     IF OBSERVED WEATHER DATA ARE AVAILABLE, USE THEM
!*     OTHERWISE, USE CRUDE ESTIMATES OF AVERAGE CONDITIONS
      IF ( POBS > 1.D0 .AND. TOBS > -100.D0 ) THEN
          P  = POBS
          T  = TOBS
          D  = 0.0D0 !DOBS
          WL = 0.0D0 !WLOBS
      ELSE
          P  = 1010.D0 * DEXP ( -HEIGHT / S )
          T  = 10.D0
          D  =  0.D0
          WL =  0.5D0
      END IF
!*     D AND WL NOT USED IN THIS VERSION

!      call Pi_DegRad(Pi, DegRad)

      H = 90.D0 - ZDOBS
      R = 0.016667D0 / DTAN ( ( H +  7.31D0 / ( H + 4.4D0 ) ) * DEGRAD )
      REFR = R * ( 0.28D0 * P / ( T + 273.D0 ) )

  77  RETURN
  end subroutine

 SUBROUTINE FUNARG (T,EL,ELP,F,D,OMEGA)
!*     This code is from NOVAS package
!*     THIS SUBROUTINE COMPUTES FUNDAMENTAL ARGUMENTS (MEAN ELEMENTS)
!*     OF THE SUN AND MOON.  SEE SIMON ET AL. (1994) ASTRONOMY AND
!*     ASTROPHYSICS 282, 663-683, ESPECIALLY SECTIONS 3.4-3.5.
!*
!*          T      = TDB TIME IN JULIAN CENTURIES SINCE J2000.0 (IN)
!*          EL     = MEAN ANOMALY OF THE MOON IN RADIANS
!*                   AT DATE TJD (OUT)
!*          ELP    = MEAN ANOMALY OF THE SUN IN RADIANS
!*                   AT DATE TJD (OUT)
!*          F      = MEAN LONGITUDE OF THE MOON MINUS MEAN LONGITUDE
!*                   OF THE MOON'S ASCENDING NODE IN RADIANS
!*                   AT DATE TJD (OUT)
!*          D      = MEAN ELONGATION OF THE MOON FROM THE SUN IN
!*                   RADIANS AT DATE TJD (OUT)
!*          OMEGA  = MEAN LONGITUDE OF THE MOON'S ASCENDING NODE
!*                   IN RADIANS AT DATE TJD (OUT)
!*
!*
      real(kind=8)  ::  T,EL,ELP,F,D,OMEGA,DMOD

      real(kind=8),PARAMETER ::  PI  = 3.14159265358979324D0
      real(kind=8),PARAMETER :: SECCON = 180.D0 * 3600.D0 / PI
      real(kind=8),PARAMETER :: REV    = 360.D0 * 3600.D0

!*     FUNDAMENTAL (DELAUNAY) ARGUMENTS FROM SIMON ET AL. (1994)

!*     MEAN ANOMALY OF THE MOON
      EL    = DMOD (         485868.249036D0 + &
                    T*( 1717915923.2178D0 +    &
                    T*(         31.8792D0 +    &
                    T*(          0.051635D0 +  &
                    T*(        - 0.00024470D0 )))), REV ) / SECCON

!*     MEAN ANOMALY OF THE SUN
      ELP   = DMOD (        1287104.79305D0 +  &
                    T*(  129596581.0481D0 +    &
                    T*(        - 0.5532D0 +    &
                    T*(          0.000136D0 +  &
                    T*(        - 0.00001149D0 )))), REV ) / SECCON

!*     MEAN ARGUMENT OF THE LATITUDE OF THE MOON
      F     = DMOD (         335779.526232D0 +  &
                    T*( 1739527262.8478D0 +     &
                    T*(       - 12.7512D0 +     &
                    T*(       -  0.001037D0 +   &
                    T*(          0.00000417D0 )))), REV ) / SECCON

!     MEAN ELONGATION OF THE MOON FROM THE SUN
      D     = DMOD (        1072260.70369D0 +  &
                    T*( 1602961601.2090D0 +    &
                    T*(        - 6.3706D0 +    &
                    T*(          0.006593D0 +  &
                    T*(        - 0.00003169D0 )))), REV ) / SECCON

!*     MEAN LONGITUDE OF THE ASCENDING NODE OF THE MOON (FROM SIMON
!*     SECTION 3.4(b.3), PRECESSION=5028.8200 ARCSEC/CY)
      OMEGA = DMOD (         450160.398036D0 + &
                    T*(  - 6962890.5431D0 +    &
                    T*(          7.4722D0 +    &
                    T*(          0.007702D0 +  &
                    T*(        - 0.00005939D0 )))), REV ) / SECCON

      RETURN

      END subroutine

    SUBROUTINE EROT (DATE1,DATE2,THETA)
!      This code is from NOVAS package
!*     THIS SUBROUTINE RETURNS THE VALUE OF THE EARTH ROTATION ANGLE
!*     (THETA) FOR A GIVEN UT1 JULIAN DATE.  THE EXPRESSION USED IS
!*     TAKEN FROM THE NOTE TO IAU RESOLUTION B1.8 OF 2000.
!*
!*         DATE1  = HIGH-ORDER PART OF UT1 JULIAN DATE (IN)
!*         DATE2  = LOW-ORDER PART OF UT1 JULIAN DATE (IN)
!*         THETA  = EARTH ROTATION ANGLE IN DEGREES (OUT)
!*
!*
      real(kind=8)  :: DATE1, DATE2, THETA, T0, THET1, THET2, THET3,DMOD

      DATA T0 / 2451545.0D0 /

!*     THE ALGORITHM USED BELOW IS EQUIVALENT TO THE CANNONICAL
!*     THETA = 0.7790572732640D0 + 1.00273781191135448D0 * T,
!*     WHERE T IS THE TIME IN UT1 DAYS FROM 2000.0 (T=DATE1+DATE2-T0),
!*     BUT IT AVOIDS MANY TWO-PI 'WRAPS' THAT DECREASE PRECISION
!*     (ADOPTED FROM SOFA ROUTINE IAU_ERA00 BY PAT WALLACE; SEE ALSO
!*     EXPRESSION AT TOP OF PAGE 35 OF IERS CONVENTIONS (1996))

      THET1 = 0.7790572732640D0 + 0.00273781191135448D0 * ( DATE1 - T0 )
      THET2 =                     0.00273781191135448D0 *   DATE2
      THET3 = DMOD ( DATE1, 1.D0 ) + DMOD ( DATE2, 1.D0 )
      THETA = DMOD ( THET1 + THET2 + THET3, 1.D0 ) * 360.D0
      IF ( THETA .LT. 0.D0 ) THETA = THETA + 360.D0

      RETURN

      END subroutine
!-----------------------------------------------------------------

      SUBROUTINE ETILT (TJD,OBLM,OBLT,EQEQ,DPSI,DEPS)
        implicit none
!*    This code is from NOVAS Package
!*     THIS SUBROUTINE COMPUTES QUANTITIES RELATED TO THE ORIENTATION
!*     OF THE EARTH'S ROTATION AXIS AT JULIAN DATE TJD.
!*
!*          TJD    = TDB JULIAN DATE FOR ORIENTATION PARAMETERS (IN)
!*          OBLM   = MEAN OBLIQUITY OF THE ECLIPTIC IN DEGREES AT
!*                   DATE TJD (OUT)
!*          OBLT   = TRUE OBLIQUITY OF THE ECLIPTIC IN DEGREES AT
!*                   DATE TJD (OUT)
!*          EQEQ   = EQUATION OF THE EQUINOXES IN TIME SECONDS AT
!*                   DATE TJD (OUT)
!*          DPSI   = NUTATION IN LONGITUDE IN ARCSECONDS AT
!*                   DATE TJD (OUT)
!*          DEPS   = NUTATION IN OBLIQUITY IN ARCSECONDS AT
!*                   DATE TJD (OUT)
!*
!*     NOTE:  THE EQUATION OF THE EQUINOXES INCLUDES THE COMPLEMENTARY
!*     TERMS.
!*
!*
      real(kind=8) :: TJD,OBLM,OBLT,EQEQ,DPSI,DEPS
      real(kind=8) :: T0,T,PSI,EPS,CTERMS,DELEPS
      real(kind=8) :: EL,ELP,F,D,OMEGA,OBM,OBT,EE
      real(kind=8) :: DELPSI !, EPSCOR,PSICOR

      real(kind=8),PARAMETER :: PI  = 3.14159265358979324D0
      real(kind=8),PARAMETER :: SECCON = 180.D0 * 3600.D0 / PI

!      integer   :: Mode
!*     T0 = TDB JULIAN DATE OF EPOCH J2000.0 (TT)
      DATA T0 / 2451545.00000000D0 /
!      DATA TLAST / 0.D0 /,   MLAST / 0 /
      DATA  CTERMS /  0.D0 /

      T = ( TJD - T0 ) / 36525.D0

!*     FUNCTION TO COMPUTE MEAN OBLIQUITY OF THE ECLIPTIC IN ARCSECONDS
!*     CAPITAINE ET AL. (2003), ASTRONOMY AND ASTROPHYSICS 412, 567-586,
!*     EXPRESSION FROM EQ. (39) WITH OBLIQUITY AT J2000.0 TAKEN FROM
!*     EQ. (37) OR TABLE 8
       OBM = & !OBLIQ(T) =
      ( ( ( ( -  0.0000000434D0  * T   &
                        -  0.000000576D0  ) * T   &
                        +  0.00200340D0   ) * T   &
                        -  0.0001831D0    ) * T   &
                        - 46.836769D0     ) * T + 84381.406D0

!*         OBTAIN NUTATION PARAMETERS IN ARCSECONDS

      CALL NOD ( T, PSI, EPS )

!*         OBTAIN COMPLEMENTARY TERMS FOR EQUATION OF THE EQUINOXES
!*         IN ARCSECONDS

      CALL FUNARG ( T,   EL, ELP, F, D, OMEGA )
!*          SERIES FROM IERS CONVENTIONS (2003), CHAPTER 5,
!*          TABLE 5.2C, WITH SOME ADJUSTMENTS TO COEFFICIENT VALUES
!*          COPIED FROM IERS FUNCTION EECT2000, WHICH HAS A MORE
!*          COMPLETE SERIES
      CTERMS = 2640.96D-6 * DSIN ( OMEGA ) &
        +   63.52D-6 * DSIN ( 2.D0 * OMEGA )  &
        +   11.75D-6 * DSIN ( 2.D0 * F - 2.D0 * D + 3.D0 * OMEGA )&
        +   11.21D-6 * DSIN ( 2.D0 * F - 2.D0 * D +        OMEGA )&
        -    4.55D-6 * DSIN ( 2.D0 * F - 2.D0 * D + 2.D0 * OMEGA )&
        +    2.02D-6 * DSIN ( 2.D0 * F            + 3.D0 * OMEGA )&
        +    1.98D-6 * DSIN ( 2.D0 * F            +        OMEGA )&
        -    1.72D-6 * DSIN ( 3.D0 * OMEGA )&
        -    0.87D-6 * T * DSIN ( OMEGA )
!*             (TERMS SMALLER THAN 2 MICROARCSECONDS OMITTED)

      DELPSI = PSI !+ PSICOR
      DELEPS = EPS !+ EPSCOR

!*     COMPUTE MEAN OBLIQUITY OF THE ECLIPTIC IN ARCSECONDS
!      OBM = OBLIQ(T)

!*     COMPUTE TRUE OBLIQUITY OF THE ECLIPTIC IN ARCSECONDS
      OBT = OBM + DELEPS

!*     COMPUTE EQUATION OF THE EQUINOXES IN ARCSECONDS
      EE = DELPSI * DCOS ( OBM / SECCON ) + CTERMS

!*     CONVERT TO OUTPUT UNITS
      OBLM = OBM / 3600.D0
      OBLT = OBT / 3600.D0
      EQEQ = EE  / 15.D0
      DPSI = DELPSI
      DEPS = DELEPS

      RETURN

    END subroutine

!------------------------------------------------------------------

      SUBROUTINE SIDTIM ( TJDH, TJDL , K, GST )
!*     This Code is from NOVAS
!*     THIS SUBROUTINE COMPUTES THE GREE NWICH SIDEREAL TIME
!*     (EITHER MEAN OR APPARENT) AT JULIAN DATE TJDH + TJDL.
!*
!*          TJDH   = UT1 JULIAN DATE, HIGH-ORDER PART (IN)
!*          TJDL   = UT1 JULIAN DATE, LOW-ORDER PART (IN)
!*                   THE JULIAN DATE MAY BE SPLIT AT ANY POINT, BUT
!*                   FOR HIGHEST PRECISION, SET TJDH TO BE THE INTEGRAL
!*                   PART OF THE JULIAN DATE, AND SET TJDL TO BE THE
!*                   FRACTIONAL PART
!*          K      = TIME SELECTION CODE (IN)
!*                   SET K=0 FOR GREENWICH MEAN SIDEREAL TIME
!*                   SET K=1 FOR GREENWICH APPARENT SIDEREAL TIME
!*          GST    = GREENWICH (MEAN OR APPARENT) SIDEREAL TIME
!*                   IN HOURS (OUT)
!*
!*     NOTE:  SEE ALSO SUBROUTINE SETDT TO SET THE VALUE OF DELTA-T
!*     (DELTA-T = TT - UT1) TO BE USED HERE.
!*
!*
      real(kind=8) :: TJDH,TJDL,GST
      real(kind=8) :: T0,UTJD,TTJD,TDBJD,SECDIF,A,THETA,RCIO
      real(kind=8) :: DMOD
      real(kind=8),DIMENSION(3):: UNITX
      integer  :: K
      real(kind=8),PARAMETER :: PI     = 3.14159265358979324D0
      real(kind=8),PARAMETER :: DEGCON = 180.D0 / PI

!*     T0 = TDB JULIAN DATE OF EPOCH J2000.0 (TT)
      DATA T0 / 2451545.00000000D0 /
      DATA UNITX / 1.D0, 0.D0, 0.D0 /

 !  3  FORMAT ( ' SIDTIM ERROR: CANNOT RETURN SIDEREAL TIME FOR ',
 !    .     'JD ', F10.1 )

 !     CALL DELTA_T(IYEAR,IMONTH,DELTAT)
 !     GETDT ( DELTAT )

!*     TIME ARGUMENT FOR PRECESSION AND NUTATION COMPONENTS OF SIDEREAL
!*     TIME IS TDB
      UTJD = TJDH + TJDL
      TTJD = UTJD! + DELTAT
      TDBJD = TTJD
      CALL TIMES ( TDBJD, A,   SECDIF )
      TDBJD = TTJD + SECDIF / 86400.D0

!*      EQUINOX-BASED MODE
!*      SEE USNO CIRCULAR 179, SECTION 2.6.2

!*      GET -1 TIMES THE MEAN OR TRUE RIGHT ASCENSION OF THE CIO
     CALL EQXRA ( TDBJD, K,   RCIO )
!*         GET EARTH ROTATION ANGLE
     CALL EROT ( TJDH, TJDL,   THETA )
!*         COMBINE TO OBTAIN SIDEREAL TIME
     GST = DMOD ( THETA / 15.D0 - RCIO, 24.D0 )
     IF ( GST < 0.D0 ) GST = GST + 24.D0

      RETURN

      END subroutine

!*****************************************************************

      SUBROUTINE EQXRA ( TJD, K,  RAEQ )
!*      This code is from NOVAS
!*     THIS SUBROUTINE COMPUTES THE INTERMEDIATE RIGHT ASCENSION
!*     OF THE EQUINOX AT JULIAN DATE TJD, USING AN ANALYTICAL EXPRESSION
!*     FOR THE ACCUMULATED PRECESSION IN RIGHT ASCENSION.  FOR THE
!*     TRUE EQUINOX THE RESULT IS THE EQUATION OF THE ORIGINS.
!*
!*          TJD    = TDB JULIAN DATE (IN)
!*          K      = EQUINOX SELECTION CODE (IN)
!*                   SET K=0 FOR MEAN EQUINOX
!*                   SET K=1 FOR TRUE EQUINOX (EQUATION OF THE ORIGINS)
!*          RADIF  = INTERMEDIATE RIGHT ASCENSION OF THE EQUINOX,
!*                   IN HOURS (+ OR -) (OUT)
!*
!*
      real(kind=8)::TJD,RAEQ,T0,TLAST,EE,EQEQ,T,A,PRECRA,DABS
      integer :: K

!*     T0 = TDB JULIAN DATE OF EPOCH J2000.0 (TT)
      DATA T0 / 2451545.00000000D0 /
      DATA TLAST / 0.D0 /,   EE / 0.D0 /

      T = ( TJD - T0 ) / 36525.D0

!*     FOR THE TRUE EQUINOX, OBTAIN THE EQUATION OF THE EQUINOXES IN
!*     TIME SECONDS, WHICH INCLUDES THE 'COMPLIMENTARY TERMS'
      IF ( K == 1 ) THEN
          IF ( DABS ( TJD - TLAST ) .GT. 1.D-8 ) THEN
              CALL ETILT ( TJD, A, A, EE, A, A )
              TLAST = TJD
          END IF
          EQEQ = EE
      ELSE
          EQEQ = 0.D0
      END IF

!*     PRECESSION IN RA IN ARCSECONDS TAKEN FROM CAPITAINE ET AL. (2003),
!*     ASTRONOMY AND ASTROPHYSICS 412, 567-586, EQ. (42)
      PRECRA = 0.014506D0 +&
              ( ( ( ( -    0.0000000368D0   * T&
                      -    0.000029956D0  ) * T&
                      -    0.00000044D0   ) * T&
                      +    1.3915817D0    ) * T&
                      + 4612.156534D0     ) * T

      RAEQ = - ( PRECRA / 15.D0 + EQEQ ) / 3600.D0

      RETURN

      END subroutine

!***********************************************************

    subroutine Atmos_Refrac(HEIGHT,Elevation,POBS,TOBS,REFR)
!      This code is modified code from NOVAS for true airless calculated
!      items . Atmospheric Refraction Astronomical algorithms, Jean, Meeus
!*     THIS SUBROUTINE COMPUTES ATMOSPHERIC REFRACTION IN Elevation
!*     DISTANCE.  THIS VERSION COMPUTES APPROXIMATE REFRACTION FOR
!*     OPTICAL WAVELENGTHS.
!*
!*          HEIGHT = HEIGHT OF OBSERVER IN METERS (IN)
!*          Elevation  = Elevation DISTANCE IN DEGREES (IN)
!*          OBSP   = OBSERVED ATMOSPHERIC PRESSURE IN MILLIBARS (IN)
!*          OBST   = OBSERVED TEMPERATURE IN DEGREES CELSIUS (IN)
!*          OBSD   = OBSERVED DEW POINT IN DEGREES CELSIUS (IN)
!*          OBSWL  = OBSERVING WAVELENGTH IN MICRONS (IN)
!*
!*          REFR   = ATMOSPHERIC REFRACTION IN DEGREES (OUT)
!*
!*     NOTE:  HEIGHT IS NOT USED IF ENTRY REFDAT HAS BEEN CALLED
!*     TO SPECIFY ATMOSPHERIC PRESSURE.
!*
      real(kind=8) :: HEIGHT,Elevation,REFR,S
      real(kind=8) :: POBS,TOBS!,DOBS,WLOBS
      real(kind=8) :: P,T,D,WL,H,R,DEXP,DTAN

!*     S IS APPROXIMATE SCALE HEIGHT OF ATMOSPHERE IN METERS
      DATA S / 9.1D3 /
!      DATA POBS, TOBS, DOBS, WLOBS / 4 * -999.D0 /

!*     COMPUTE REFRACTION ONLY FOR ZENITH DISTANCES
!*     BETWEEN 0.1 AND 91 DEGREES
      IF ( Elevation <= -1.D0 .OR. Elevation > 89.9D0 ) THEN
          REFR = 0.D0
          GO TO 77
      END IF

!*     IF OBSERVED WEATHER DATA ARE AVAILABLE, USE THEM
!*     OTHERWISE, USE CRUDE ESTIMATES OF AVERAGE CONDITIONS
      IF ( POBS > 1.D0 .AND. TOBS > -100.D0 ) THEN
          P  = POBS
          T  = TOBS
          D  = 0.0D0 !DOBS
          WL = 0.0D0 !WLOBS
      ELSE
          P  = 1010.D0 * DEXP ( -HEIGHT / S )
          T  = 10.D0
          D  =  0.D0
          WL =  0.5D0
      END IF
!*     D AND WL NOT USED IN THIS VERSION

!      call (Pi, DegRad)

      H = Elevation
      R = 0.017D0 / DTAN ( ( H +  10.3D0 / ( H + 5.11D0 ) ) * DEGRAD )
      REFR = R * ( 0.28D0 * P / ( T + 273.D0 ) )

  77  RETURN
  end subroutine

!------------------------------------------------------------

      SUBROUTINE TIMES (TDBJD,TTJD,SECDIF)
!*    This code is from NOVAS Package
!*     THIS SUBROUTINE COMPUTES THE TERRESTRIAL TIME (TT) JULIAN DATE
!*     CORRESPONDING TO A BARYCENTRIC DYNAMICAL TIME (TDB) JULIAN DATE.
!*     THE EXPRESSION USED IN THIS VERSION IS A TRUNCATED FORM OF A
!*     LONGER AND MORE PRECISE SERIES GIVEN BY FAIRHEAD & BRETAGNON
!*     (1990) A&A 229, 240.  THE RESULT IS GOOD TO ABOUT 10 MICROSECONDS.
!*
!*          TDBJD  = TDB JULIAN DATE (IN)
!*          TTJD   = TT JULIAN DATE (OUT)
!*          SECDIF = DIFFERENCE TDBJD-TTJD, IN SECONDS (OUT)
!*
!*
      real(kind=8):: TDBJD,TTJD,SECDIF,T,T0,DSIN

!*     T0 = TDB JULIAN DATE OF EPOCH J2000.0 (TT)
      DATA T0 / 2451545.00000000D0 /

      T = ( TDBJD - T0 ) / 36525.D0

!*     EXPRESSION GIVEN IN USNO CIRCULAR 179, EQ. 2.6
      SECDIF = 0.001657D0 * DSIN (  628.3076D0 * T + 6.2401D0) &
            + 0.000022D0 * DSIN (  575.3385D0 * T + 4.2970D0) &
            + 0.000014D0 * DSIN ( 1256.6152D0 * T + 6.1969D0) &
            + 0.000005D0 * DSIN (  606.9777D0 * T + 4.0212D0) &
            + 0.000005D0 * DSIN (   52.9691D0 * T + 0.4444D0) &
            + 0.000002D0 * DSIN (   21.3299D0 * T + 5.5431D0) &
            + 0.000010D0 * T * DSIN ( 628.3076D0 * T + 4.2490D0)

      TTJD = TDBJD - SECDIF / 86400.D0

      RETURN

      END subroutine

!-------------------------------------------------------------

      SUBROUTINE TERRA (GLON,GLAT,HT,ST,POS,VEL)
!*     This code is from NOVAS package
!*     THIS SUBROUTINE COMPUTES THE POSITION AND VELOCITY VECTORS OF
!*     A TERRESTRIAL OBSERVER WITH RESPECT TO THE GEOCENTER.
!*
!*          GLON   = LONGITUDE OF OBSERVER WITH RESPECT TO REFERENCE
!*                   MERIDIAN (EAST +) IN DEGREES (IN)
!*          GLAT   = GEODETIC LATITUDE (NORTH +) OF OBSERVER
!*                   IN DEGREES (IN)
!*          HT     = HEIGHT OF OBSERVER IN METERS (IN)
!*          ST     = LOCAL APPARENT SIDEREAL TIME AT REFERENCE MERIDIAN
!*                   IN HOURS (IN)
!*          POS    = POSITION VECTOR OF OBSERVER WITH RESPECT TO
!*                   GEOCENTER, EQUATORIAL RECTANGULAR COORDINATES,
!*                   REFERRED TO TRUE EQUATOR AND EQUINOX OF DATE,
!*                   COMPONENTS IN AU (OUT)
!*          VEL    = VELOCITY VECTOR OF OBSERVER WITH RESPECT TO
!*                   GEOCENTER, EQUATORIAL RECTANGULAR COORDINATES,
!*                   REFERRED TO TRUE EQUATOR AND EQUINOX OF DATE,
!*                   COMPONENTS IN AU/DAY (OUT)
!*
!*     NOTE 1:  IF REFERENCE MERIDIAN IS GREENWICH AND ST=0.D0, POS
!*     IS EFFECTIVELY REFERRED TO EQUATOR AND GREENWICH.
!*
!*     NOTE 2:  THIS SUBROUTINE IGNORES POLAR MOTION, UNLESS THE
!*     OBSERVER'S LONGITUDE AND LATITUDE HAVE BEEN CORRECTED FOR IT,
!*     AND VARIATION IN THE LENGTH OF DAY (ANGULAR VELOCITY OF EARTH).
!*     NEGLECT OF POLAR MOTION MAY YIELD 15 METERS ERROR IN POSITION
!*     AND OF ORDER 1 MILLIMETER/SEC ERROR IN VELOCITY.  NEGLECT OF
!*     VARIATIONS IN LENGTH OF DAY RESULTS IN EVEN SMALLER VELOCITY
!*     ERRORS.
!*
!*     NOTE 3:  THE TRUE EQUATOR AND EQUINOX OF DATE DO NOT FORM AN
!*     INERTIAL SYSTEM.  THEREFORE, WITH RESPECT TO AN INERTIAL SYSTEM,
!*     THE SMALL VELOCITY COMPONENT, OF ORDER 0.1 MILLIMETER/SEC,
!*     DUE TO THE PRECESSION AND NUTATION OF THE EARTH'S AXIS, IS NOT
!*     ACCOUNTED FOR HERE.
!*
!*
      real(8) :: GLON,GLAT,HT,ST,POS,VEL,PI,SECCON,ERAD,F,OMEGA,&
           AUKM,DF2,PHI,SINPHI,COSPHI,C,S,ACH,ASH,STLOCL,SINST,COSST, &
           DSQRT,DCOS,DSIN
      DIMENSION POS(3), VEL(3)
      SAVE

      PARAMETER ( PI     = 3.14159265358979324D0 )
      PARAMETER ( SECCON = 180.D0 * 3600.D0 / PI )

      integer :: J, NTIMES

      DATA NTIMES / 0 /

      NTIMES = NTIMES + 1
      IF (NTIMES.EQ.1) THEN
!*         GET ERAD, THE EQUATORIAL RADIUS OF EARTH IN KILOMETERS
          CALL ASTCON ('ERAD',1.D-3,ERAD)
!*         GET F, THE FLATTENING FACTOR OF EARTH ELLIPSOID
          CALL ASTCON ('F',1.D0,F)
!*         GET OMEGA, THE NOMINAL MEAN ROTATIONAL ANGULAR VELOCITY OF
!*         EARTH IN RADIANS/SECOND
          CALL ASTCON ('ANGVEL',1.D0,OMEGA)
!*         GET AUKM, THE LENGTH OF THE ASTRONOMICAL UNIT IN KILOMETERS
          CALL ASTCON ('AU',1.D-3,AUKM)
      END IF

!*     COMPUTE PARAMETERS RELATING TO GEODETIC TO GEOCENTRIC CONVERSION
      DF2 = (1.D0 - F)**2
      PHI = GLAT * 3600.D0 / SECCON
      SINPHI = DSIN(PHI)
      COSPHI = DCOS(PHI)
      C = 1.D0 / DSQRT ( COSPHI**2 + DF2 * SINPHI**2 )
      S = DF2 * C
      ACH = ERAD * C + HT/1000.D0
      ASH = ERAD * S + HT/1000.D0

!*     COMPUTE LOCAL SIDEREAL TIME FACTORS
      STLOCL = (ST * 54000.D0 + GLON * 3600.D0) / SECCON
      SINST = DSIN(STLOCL)
      COSST = DCOS(STLOCL)

!*     COMPUTE POSITION VECTOR COMPONENTS IN KM
      POS(1) = ACH * COSPHI * COSST
      POS(2) = ACH * COSPHI * SINST
      POS(3) = ASH * SINPHI

!*     COMPUTE VELOCITY VECTOR COMPONENTS IN KM/SEC
      VEL(1) = -OMEGA * ACH * COSPHI * SINST
      VEL(2) =  OMEGA * ACH * COSPHI * COSST
      VEL(3) =  0.D0

!*     CONVERT POSITION AND VELOCITY COMPONENTS TO AU AND AU/DAY
      DO 20 J=1,3
      POS(J) = POS(J) / AUKM
      VEL(J) = VEL(J) / AUKM * 86400.D0
   20 CONTINUE

      RETURN

      END subroutine

!*+----------------------------------------------------------------------

      SUBROUTINE NOD (T,DPSI,DEPS)
!*
!*     THIS SUBROUTINE RETURNS THE VALUES FOR NUTATION IN LONGITUDE AND
!*     NUTATION IN OBLIQUITY FOR A GIVEN TDB JULIAN DATE.
!*
!*          T     = TDB TIME IN JULIAN CENTURIES SINCE J2000.0 (IN)
!*          DPSI  = NUTATION IN LONGITUDE IN ARCSECONDS (OUT)
!*          DEPS  = NUTATION IN OBLIQUITY IN ARCSECONDS (OUT)
!*
!*
      real(kind=8) :: T,DPSI,DEPS,T0,T1,DP,DE

      real(kind=8) , PARAMETER :: PI = 3.14159265358979324D0
      real(kind=8) , PARAMETER :: SECCON = 180.D0 * 3600.D0 / PI

!*     T0 = TDB JULIAN DATE OF EPOCH J2000.0 (TT)
      DATA T0 / 2451545.00000000D0 /


      T1 = T * 36525.D0

!*     =================================================================
!*     EVALUATE NUTATION SERIES
!*     RESULTING NUTATION IN LONGITUDE AND OBLIQUITY IN ARCSECONDS

!*     CALL SUBROUTINE TO EVALUATE NUTATION SERIES
!      IF ( MODE /= 0 ) THEN
!*         HIGH ACCURACY MODE -- IERS SUBROUTINE
!          CALL NU2000A ( T0, T1, DP, DE )
!      ELSE
!*         LOW ACCURACY MODE -- MODIFICATION OF IERS SUBROUTINE
       CALL NU2000K ( T0, T1, DP, DE )
!      END IF
      DPSI = DP * SECCON
      DEPS = DE * SECCON

!*     =================================================================

      RETURN

      END subroutine

!**************************************************************************

      SUBROUTINE NU2000K ( DATE1, DATE2, DPSI, DEPS )

!*+
!*  - - - - - - - -
!*   N U 2 0 0 0 K
!*  - - - - - - - -
!*
!*  Nutation, IAU 2000A model (MHB_2000 without FCN) MODIFIED.  Series
!*     truncated for speed of execution, and using Simon et al. (1994)
!*     fundamental arguments throughout.  Accuracy, compared to
!*     IAU 2000 A series, is 0.1 mas in delta psi and 0.04 mas in
!*     delta epsilon and delta psi sin(epsilon) over 6 centuries
!*     centered at year 2000 (99% of errors less than these values).
!*
!*  Modified form of NU2000A, by Pat Wallace, given in subroutine annex
!*  to Chapter 5 of IERS Conventions (2003).
!*
!*  Given:
!*     DATE1,DATE2    d   TT date (JD = DATE1+DATE2)
!*
!*  Returned:
!*     DPSI,DEPS      d   nutation (luni-solar + planetary, radians)
!*
!*  This revision:  2002 November 25
!*                  2004 March 1     (by G. Kaplan)
!*
!*-----------------------------------------------------------------------

      IMPLICIT NONE

      Real(8) DATE1, DATE2, DPSI, DEPS

!*  Arcseconds to radians
      real(8) DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

!*  Milliarcseconds to radians
      real(8) DMAS2R
      PARAMETER ( DMAS2R = DAS2R / 1D3 )

!*  Arc seconds in a full circle
      real(8) TURNAS
      PARAMETER ( TURNAS = 1296000D0 )

!*  2Pi
      real(8) D2PI
      PARAMETER ( D2PI = 6.283185307179586476925287D0 )

!*  Units of 0.1 microarcsecond to radians
      real(8) U2R
      PARAMETER ( U2R = DAS2R/1D7 )

!*  Reference epoch (J2000), JD
      real(8) DJ0
      PARAMETER ( DJ0 = 2451545D0 )

!*  Days per Julian century
      real(8) DJC
      PARAMETER ( DJC = 36525D0 )

!*  Miscellaneous
      real(8) T, EL, ELP, F, D, OM, ARG, DP, DE, SARG, CARG,&
                      DPSILS, DEPSLS,&
                      ALME, ALVE, ALEA, ALMA, ALJU, ALSA, ALUR, ALNE,&
                      APA,&
                      DPSIPL, DEPSPL
      INTEGER I, J

!*  -------------------------
!*  Luni-Solar nutation model
!*  -------------------------

!*  Number of terms in the luni-solar nutation model
      INTEGER NLS
      PARAMETER ( NLS = 323 )

!*  Coefficients for fundamental arguments
      INTEGER NALS(5,NLS)

!*  Longitude and obliquity coefficients
      DOUBLE PRECISION CLS(6,NLS)

!*  ---------------
!*  Planetary terms
!*  ---------------

!*  Number of terms in the planetary nutation model
      INTEGER NPL
      PARAMETER ( NPL = 165 )

!*  Coefficients for fundamental arguments
      INTEGER NAPL(14,NPL)

!*  Longitude and obliquity coefficients
      real(8) CPL(4,NPL)

!*  ----------------------------------------
!*  Tables of argument and term coefficients
!*  ----------------------------------------

!*
!*  Luni-Solar argument multipliers:
!*               L     L'    F     D     Om
!*
      DATA ( ( NALS(I,J), I=1,5 ), J=  1, 20 ) /&
               0,    0,    0,    0,    1,&
               0,    0,    2,   -2,    2,&
               0,    0,    2,    0,    2,&
               0,    0,    0,    0,    2,&
               0,    1,    0,    0,    0,&
               0,    1,    2,   -2,    2,&
               1,    0,    0,    0,    0,&
               0,    0,    2,    0,    1,&
               1,    0,    2,    0,    2,&
               0,   -1,    2,   -2,    2,&
               0,    0,    2,   -2,    1,&
              -1,    0,    2,    0,    2,&
              -1,    0,    0,    2,    0,&
               1,    0,    0,    0,    1,&
              -1,    0,    0,    0,    1,&
              -1,    0,    2,    2,    2,&
               1,    0,    2,    0,    1,&
              -2,    0,    2,    0,    1,&
               0,    0,    0,    2,    0,&
               0,    0,    2,    2,    2/
      DATA ( ( NALS(I,J), I=1,5 ), J= 21, 40 ) /&
               0,   -2,    2,   -2,    2,&
              -2,    0,    0,    2,    0,&
               2,    0,    2,    0,    2,&
               1,    0,    2,   -2,    2,&
              -1,    0,    2,    0,    1,&
               2,    0,    0,    0,    0,&
               0,    0,    2,    0,    0,&
               0,    1,    0,    0,    1,&
              -1,    0,    0,    2,    1,&
               0,    2,    2,   -2,    2,&
               0,    0,   -2,    2,    0,&
               1,    0,    0,   -2,    1,&
               0,   -1,    0,    0,    1,&
              -1,    0,    2,    2,    1,&
               0,    2,    0,    0,    0,&
               1,    0,    2,    2,    2,&
              -2,    0,    2,    0,    0,&
               0,    1,    2,    0,    2,&
               0,    0,    2,    2,    1,&
               0,   -1,    2,    0,    2/
      DATA ( ( NALS(I,J), I=1,5 ), J= 41, 60 ) /&
               0,    0,    0,    2,    1,&
               1,    0,    2,   -2,    1,&
               2,    0,    2,   -2,    2,&
              -2,    0,    0,    2,    1,&
               2,    0,    2,    0,    1,&
               0,   -1,    2,   -2,    1,&
               0,    0,    0,   -2,    1,&
              -1,   -1,    0,    2,    0,&
               2,    0,    0,   -2,    1,&
               1,    0,    0,    2,    0,&
               0,    1,    2,   -2,    1,&
               1,   -1,    0,    0,    0,&
              -2,    0,    2,    0,    2,&
               3,    0,    2,    0,    2,&
               0,   -1,    0,    2,    0,&
               1,   -1,    2,    0,    2,&
               0,    0,    0,    1,    0,&
              -1,   -1,    2,    2,    2,&
              -1,    0,    2,    0,    0,&
               0,   -1,    2,    2,    2/
      DATA ( ( NALS(I,J), I=1,5 ), J= 61, 80 ) /&
              -2,    0,    0,    0,    1,&
               1,    1,    2,    0,    2,&
               2,    0,    0,    0,    1,&
              -1,    1,    0,    1,    0,&
               1,    1,    0,    0,    0,&
               1,    0,    2,    0,    0,&
              -1,    0,    2,   -2,    1,&
               1,    0,    0,    0,    2,&
              -1,    0,    0,    1,    0,&
               0,    0,    2,    1,    2,&
              -1,    0,    2,    4,    2,&
              -1,    1,    0,    1,    1,&
               0,   -2,    2,   -2,    1,&
               1,    0,    2,    2,    1,&
              -2,    0,    2,    2,    2,&
              -1,    0,    0,    0,    2,&
               1,    1,    2,   -2,    2,&
              -2,    0,    2,    4,    2,&
              -1,    0,    4,    0,    2,&
               2,    0,    2,   -2,    1/
      DATA ( ( NALS(I,J), I=1,5 ), J= 81,100 ) /&
               2,    0,    2,    2,    2,&
               1,    0,    0,    2,    1,&
               3,    0,    0,    0,    0,&
               3,    0,    2,   -2,    2,&
               0,    0,    4,   -2,    2,&
               0,    1,    2,    0,    1,&
               0,    0,   -2,    2,    1,&
               0,    0,    2,   -2,    3,&
              -1,    0,    0,    4,    0,&
               2,    0,   -2,    0,    1,&
              -2,    0,    0,    4,    0,&
              -1,   -1,    0,    2,    1,&
              -1,    0,    0,    1,    1,&
               0,    1,    0,    0,    2,&
               0,    0,   -2,    0,    1,&
               0,   -1,    2,    0,    1,&
               0,    0,    2,   -1,    2,&
               0,    0,    2,    4,    2,&
              -2,   -1,    0,    2,    0,&
               1,    1,    0,   -2,    1/
      DATA ( ( NALS(I,J), I=1,5 ), J=101,120 ) /&
              -1,    1,    0,    2,    0,&
              -1,    1,    0,    1,    2,&
               1,   -1,    0,    0,    1,&
               1,   -1,    2,    2,    2,&
              -1,    1,    2,    2,    2,&
               3,    0,    2,    0,    1,&
               0,    1,   -2,    2,    0,&
              -1,    0,    0,   -2,    1,&
               0,    1,    2,    2,    2,&
              -1,   -1,    2,    2,    1,&
               0,   -1,    0,    0,    2,&
               1,    0,    2,   -4,    1,&
              -1,    0,   -2,    2,    0,&
               0,   -1,    2,    2,    1,&
               2,   -1,    2,    0,    2,&
               0,    0,    0,    2,    2,&
               1,   -1,    2,    0,    1,&
              -1,    1,    2,    0,    2,&
               0,    1,    0,    2,    0,&
               0,   -1,   -2,    2,    0/
      DATA ( ( NALS(I,J), I=1,5 ), J=121,140 ) /&
               0,    3,    2,   -2,    2,&
               0,    0,    0,    1,    1,&
              -1,    0,    2,    2,    0,&
               2,    1,    2,    0,    2,&
               1,    1,    0,    0,    1,&
               1,    1,    2,    0,    1,&
               2,    0,    0,    2,    0,&
               1,    0,   -2,    2,    0,&
              -1,    0,    0,    2,    2,&
               0,    1,    0,    1,    0,&
               0,    1,    0,   -2,    1,&
              -1,    0,    2,   -2,    2,&
               0,    0,    0,   -1,    1,&
              -1,    1,    0,    0,    1,&
               1,    0,    2,   -1,    2,&
               1,   -1,    0,    2,    0,&
               0,    0,    0,    4,    0,&
               1,    0,    2,    1,    2,&
               0,    0,    2,    1,    1,&
               1,    0,    0,   -2,    2/
      DATA ( ( NALS(I,J), I=1,5 ), J=141,160 ) /&
              -1,    0,    2,    4,    1,&
               1,    0,   -2,    0,    1,&
               1,    1,    2,   -2,    1,&
               0,    0,    2,    2,    0,&
              -1,    0,    2,   -1,    1,&
              -2,    0,    2,    2,    1,&
               4,    0,    2,    0,    2,&
               2,   -1,    0,    0,    0,&
               2,    1,    2,   -2,    2,&
               0,    1,    2,    1,    2,&
               1,    0,    4,   -2,    2,&
              -1,   -1,    0,    0,    1,&
               0,    1,    0,    2,    1,&
              -2,    0,    2,    4,    1,&
               2,    0,    2,    0,    0,&
               1,    0,    0,    1,    0,&
              -1,    0,    0,    4,    1,&
              -1,    0,    4,    0,    1,&
               2,    0,    2,    2,    1,&
               0,    0,    2,   -3,    2/
      DATA ( ( NALS(I,J), I=1,5 ), J=161,180 ) /&
              -1,   -2,    0,    2,    0,&
               2,    1,    0,    0,    0,&
               0,    0,    4,    0,    2,&
               0,    0,    0,    0,    3,&
               0,    3,    0,    0,    0,&
               0,    0,    2,   -4,    1,&
               0,   -1,    0,    2,    1,&
               0,    0,    0,    4,    1,&
              -1,   -1,    2,    4,    2,&
               1,    0,    2,    4,    2,&
              -2,    2,    0,    2,    0,&
              -2,   -1,    2,    0,    1,&
              -2,    0,    0,    2,    2,&
              -1,   -1,    2,    0,    2,&
               0,    0,    4,   -2,    1,&
               3,    0,    2,   -2,    1,&
              -2,   -1,    0,    2,    1,&
               1,    0,    0,   -1,    1,&
               0,   -2,    0,    2,    0,&
              -2,    0,    0,    4,    1/
      DATA ( ( NALS(I,J), I=1,5 ), J=181,200 ) /&
              -3,    0,    0,    0,    1,&
               1,    1,    2,    2,    2,&
               0,    0,    2,    4,    1,&
               3,    0,    2,    2,    2,&
              -1,    1,    2,   -2,    1,&
               2,    0,    0,   -4,    1,&
               0,    0,    0,   -2,    2,&
               2,    0,    2,   -4,    1,&
              -1,    1,    0,    2,    1,&
               0,    0,    2,   -1,    1,&
               0,   -2,    2,    2,    2,&
               2,    0,    0,    2,    1,&
               4,    0,    2,   -2,    2,&
               2,    0,    0,   -2,    2,&
               0,    2,    0,    0,    1,&
               1,    0,    0,   -4,    1,&
               0,    2,    2,   -2,    1,&
              -3,    0,    0,    4,    0,&
              -1,    1,    2,    0,    1,&
              -1,   -1,    0,    4,    0/
      DATA ( ( NALS(I,J), I=1,5 ), J=201,220 ) /&
              -1,   -2,    2,    2,    2,&
              -2,   -1,    2,    4,    2,&
               1,   -1,    2,    2,    1,&
              -2,    1,    0,    2,    0,&
              -2,    1,    2,    0,    1,&
               2,    1,    0,   -2,    1,&
              -3,    0,    2,    0,    1,&
              -2,    0,    2,   -2,    1,&
              -1,    1,    0,    2,    2,&
               0,   -1,    2,   -1,    2,&
              -1,    0,    4,   -2,    2,&
               0,   -2,    2,    0,    2,&
              -1,    0,    2,    1,    2,&
               2,    0,    0,    0,    2,&
               0,    0,    2,    0,    3,&
              -2,    0,    4,    0,    2,&
              -1,    0,   -2,    0,    1,&
              -1,    1,    2,    2,    1,&
               3,    0,    0,    0,    1,&
              -1,    0,    2,    3,    2/
      DATA ( ( NALS(I,J), I=1,5 ), J=221,240 ) /&
               2,   -1,    2,    0,    1,&
               0,    1,    2,    2,    1,&
               0,   -1,    2,    4,    2,&
               2,   -1,    2,    2,    2,&
               0,    2,   -2,    2,    0,&
              -1,   -1,    2,   -1,    1,&
               0,   -2,    0,    0,    1,&
               1,    0,    2,   -4,    2,&
               1,   -1,    0,   -2,    1,&
              -1,   -1,    2,    0,    1,&
               1,   -1,    2,   -2,    2,&
              -2,   -1,    0,    4,    0,&
              -1,    0,    0,    3,    0,&
              -2,   -1,    2,    2,    2,&
               0,    2,    2,    0,    2,&
               1,    1,    0,    2,    0,&
               2,    0,    2,   -1,    2,&
               1,    0,    2,    1,    1,&
               4,    0,    0,    0,    0,&
               2,    1,    2,    0,    1/
      DATA ( ( NALS(I,J), I=1,5 ), J=241,260 ) /&
               3,   -1,    2,    0,    2,&
              -2,    2,    0,    2,    1,&
               1,    0,    2,   -3,    1,&
               1,    1,    2,   -4,    1,&
              -1,   -1,    2,   -2,    1,&
               0,   -1,    0,   -1,    1,&
               0,   -1,    0,   -2,    1,&
              -2,    0,    0,    0,    2,&
              -2,    0,   -2,    2,    0,&
              -1,    0,   -2,    4,    0,&
               1,   -2,    0,    0,    0,&
               0,    1,    0,    1,    1,&
              -1,    2,    0,    2,    0,&
               1,   -1,    2,   -2,    1,&
               1,    2,    2,   -2,    2,&
               2,   -1,    2,   -2,    2,&
               1,    0,    2,   -1,    1,&
               2,    1,    2,   -2,    1,&
              -2,    0,    0,   -2,    1,&
               1,   -2,    2,    0,    2/
      DATA ( ( NALS(I,J), I=1,5 ), J=261,280 ) /&
               0,    1,    2,    1,    1,&
               1,    0,    4,   -2,    1,&
              -2,    0,    4,    2,    2,&
               1,    1,    2,    1,    2,&
               1,    0,    0,    4,    0,&
               1,    0,    2,    2,    0,&
               2,    0,    2,    1,    2,&
               3,    1,    2,    0,    2,&
               4,    0,    2,    0,    1,&
              -2,   -1,    2,    0,    0,&
               0,    1,   -2,    2,    1,&
               1,    0,   -2,    1,    0,&
               2,   -1,    0,   -2,    1,&
              -1,    0,    2,   -1,    2,&
               1,    0,    2,   -3,    2,&
               0,    1,    2,   -2,    3,&
              -1,    0,   -2,    2,    1,&
               0,    0,    2,   -4,    2,&
               2,    0,    2,   -4,    2,&
               0,    0,    4,   -4,    4/
      DATA ( ( NALS(I,J), I=1,5 ), J=281,300 ) /&
               0,    0,    4,   -4,    2,&
              -2,    0,    0,    3,    0,&
               1,    0,   -2,    2,    1,&
              -3,    0,    2,    2,    2,&
              -2,    0,    2,    2,    0,&
               2,   -1,    0,    0,    1,&
               1,    1,    0,    1,    0,&
               0,    1,    4,   -2,    2,&
              -1,    1,    0,   -2,    1,&
               0,    0,    0,   -4,    1,&
               1,   -1,    0,    2,    1,&
               1,    1,    0,    2,    1,&
              -1,    2,    2,    2,    2,&
               3,    1,    2,   -2,    2,&
               0,   -1,    0,    4,    0,&
               2,   -1,    0,    2,    0,&
               0,    0,    4,    0,    1,&
               2,    0,    4,   -2,    2,&
              -1,   -1,    2,    4,    1,&
               1,    0,    0,    4,    1/
      DATA ( ( NALS(I,J), I=1,5 ), J=301,320 ) /&
               1,   -2,    2,    2,    2,&
               0,    0,    2,    3,    2,&
              -1,    1,    2,    4,    2,&
               3,    0,    0,    2,    0,&
              -1,    0,    4,    2,    2,&
              -2,    0,    2,    6,    2,&
              -1,    0,    2,    6,    2,&
               1,    1,   -2,    1,    0,&
              -1,    0,    0,    1,    2,&
              -1,   -1,    0,    1,    0,&
              -2,    0,    0,    1,    0,&
               0,    0,   -2,    1,    0,&
               1,   -1,   -2,    2,    0,&
               1,    2,    0,    0,    0,&
               3,    0,    2,    0,    0,&
               0,   -1,    1,   -1,    1,&
              -1,    0,    1,    0,    3,&
              -1,    0,    1,    0,    2,&
              -1,    0,    1,    0,    1,&
              -1,    0,    1,    0,    0/
      DATA ( ( NALS(I,J), I=1,5 ), J=321,323 ) /&
               0,    0,    1,    0,    2,&
               0,    0,    1,    0,    1,&
               0,    0,    1,    0,    0/

!*
!*  Luni-Solar nutation coefficients, unit 1e-7 arcsec:
!*  longitude (sin, t*sin, cos), obliquity (cos, t*cos, sin)
!*
      DATA ( ( CLS(I,J), I=1,6 ), J=  1, 20 ) /&
     -172064161.D0,-174666.D0, 33386.D0, 92052331.D0, 9086.D0,15377.D0,&
      -13170906.D0,  -1675.D0,-13696.D0,  5730336.D0,-3015.D0,-4587.D0,&
       -2276413.D0,   -234.D0,  2796.D0,   978459.D0, -485.D0, 1374.D0,&
        2074554.D0,    207.D0,  -698.D0,  -897492.D0,  470.D0, -291.D0,&
        1475877.D0,  -3633.D0, 11817.D0,    73871.D0, -184.D0,-1924.D0,&
        -516821.D0,   1226.D0,  -524.D0,   224386.D0, -677.D0, -174.D0,&
         711159.D0,     73.D0,  -872.D0,    -6750.D0,    0.D0,  358.D0,&
        -387298.D0,   -367.D0,   380.D0,   200728.D0,   18.D0,  318.D0,&
        -301461.D0,    -36.D0,   816.D0,   129025.D0,  -63.D0,  367.D0,&
         215829.D0,   -494.D0,   111.D0,   -95929.D0,  299.D0,  132.D0,&
         128227.D0,    137.D0,   181.D0,   -68982.D0,   -9.D0,   39.D0,&
         123457.D0,     11.D0,    19.D0,   -53311.D0,   32.D0,   -4.D0,&
         156994.D0,     10.D0,  -168.D0,    -1235.D0,    0.D0,   82.D0,&
          63110.D0,     63.D0,    27.D0,   -33228.D0,    0.D0,   -9.D0,&
         -57976.D0,    -63.D0,  -189.D0,    31429.D0,    0.D0,  -75.D0,&
         -59641.D0,    -11.D0,   149.D0,    25543.D0,  -11.D0,   66.D0,&
         -51613.D0,    -42.D0,   129.D0,    26366.D0,    0.D0,   78.D0,&
          45893.D0,     50.D0,    31.D0,   -24236.D0,  -10.D0,   20.D0,&
          63384.D0,     11.D0,  -150.D0,    -1220.D0,    0.D0,   29.D0,&
         -38571.D0,     -1.D0,   158.D0,    16452.D0,  -11.D0,   68.D0/
      DATA ( ( CLS(I,J), I=1,6 ), J= 21, 40 ) /&
          32481.D0,      0.D0,     0.D0,   -13870.D0,    0.D0,    0.D0,&
         -47722.D0,      0.D0,   -18.D0,      477.D0,    0.D0,  -25.D0,&
         -31046.D0,     -1.D0,   131.D0,    13238.D0,  -11.D0,   59.D0,&
          28593.D0,      0.D0,    -1.D0,   -12338.D0,   10.D0,   -3.D0,&
          20441.D0,     21.D0,    10.D0,   -10758.D0,    0.D0,   -3.D0,&
          29243.D0,      0.D0,   -74.D0,     -609.D0,    0.D0,   13.D0,&
          25887.D0,      0.D0,   -66.D0,     -550.D0,    0.D0,   11.D0,&
         -14053.D0,    -25.D0,    79.D0,     8551.D0,   -2.D0,  -45.D0,&
          15164.D0,     10.D0,    11.D0,    -8001.D0,    0.D0,   -1.D0,&
         -15794.D0,     72.D0,   -16.D0,     6850.D0,  -42.D0,   -5.D0,&
          21783.D0,      0.D0,    13.D0,     -167.D0,    0.D0,   13.D0,&
         -12873.D0,    -10.D0,   -37.D0,     6953.D0,    0.D0,  -14.D0,&
         -12654.D0,     11.D0,    63.D0,     6415.D0,    0.D0,   26.D0,&
         -10204.D0,      0.D0,    25.D0,     5222.D0,    0.D0,   15.D0,&
          16707.D0,    -85.D0,   -10.D0,      168.D0,   -1.D0,   10.D0,&
          -7691.D0,      0.D0,    44.D0,     3268.D0,    0.D0,   19.D0,&
         -11024.D0,      0.D0,   -14.D0,      104.D0,    0.D0,    2.D0,&
           7566.D0,    -21.D0,   -11.D0,    -3250.D0,    0.D0,   -5.D0,&
          -6637.D0,    -11.D0,    25.D0,     3353.D0,    0.D0,   14.D0,&
          -7141.D0,     21.D0,     8.D0,     3070.D0,    0.D0,    4.D0/
      DATA ( ( CLS(I,J), I=1,6 ), J= 41, 60 ) /&
          -6302.D0,    -11.D0,     2.D0,     3272.D0,    0.D0,    4.D0,&
           5800.D0,     10.D0,     2.D0,    -3045.D0,    0.D0,   -1.D0,&
           6443.D0,      0.D0,    -7.D0,    -2768.D0,    0.D0,   -4.D0,&
          -5774.D0,    -11.D0,   -15.D0,     3041.D0,    0.D0,   -5.D0,&
          -5350.D0,      0.D0,    21.D0,     2695.D0,    0.D0,   12.D0,&
          -4752.D0,    -11.D0,    -3.D0,     2719.D0,    0.D0,   -3.D0,&
          -4940.D0,    -11.D0,   -21.D0,     2720.D0,    0.D0,   -9.D0,&
           7350.D0,      0.D0,    -8.D0,      -51.D0,    0.D0,    4.D0,&
           4065.D0,      0.D0,     6.D0,    -2206.D0,    0.D0,    1.D0,&
           6579.D0,      0.D0,   -24.D0,     -199.D0,    0.D0,    2.D0,&
           3579.D0,      0.D0,     5.D0,    -1900.D0,    0.D0,    1.D0,&
           4725.D0,      0.D0,    -6.D0,      -41.D0,    0.D0,    3.D0,&
          -3075.D0,      0.D0,    -2.D0,     1313.D0,    0.D0,   -1.D0,&
          -2904.D0,      0.D0,    15.D0,     1233.D0,    0.D0,    7.D0,&
           4348.D0,      0.D0,   -10.D0,      -81.D0,    0.D0,    2.D0,&
          -2878.D0,      0.D0,     8.D0,     1232.D0,    0.D0,    4.D0,&
          -4230.D0,      0.D0,     5.D0,      -20.D0,    0.D0,   -2.D0,&
          -2819.D0,      0.D0,     7.D0,     1207.D0,    0.D0,    3.D0,&
          -4056.D0,      0.D0,     5.D0,       40.D0,    0.D0,   -2.D0,&
          -2647.D0,      0.D0,    11.D0,     1129.D0,    0.D0,    5.D0/
      DATA ( ( CLS(I,J), I=1,6 ), J= 61, 80 ) /&
          -2294.D0,      0.D0,   -10.D0,     1266.D0,    0.D0,   -4.D0,&
           2481.D0,      0.D0,    -7.D0,    -1062.D0,    0.D0,   -3.D0,&
           2179.D0,      0.D0,    -2.D0,    -1129.D0,    0.D0,   -2.D0,&
           3276.D0,      0.D0,     1.D0,       -9.D0,    0.D0,    0.D0,&
          -3389.D0,      0.D0,     5.D0,       35.D0,    0.D0,   -2.D0,&
           3339.D0,      0.D0,   -13.D0,     -107.D0,    0.D0,    1.D0,&
          -1987.D0,      0.D0,    -6.D0,     1073.D0,    0.D0,   -2.D0,&
          -1981.D0,      0.D0,     0.D0,      854.D0,    0.D0,    0.D0,&
           4026.D0,      0.D0,  -353.D0,     -553.D0,    0.D0, -139.D0,&
           1660.D0,      0.D0,    -5.D0,     -710.D0,    0.D0,   -2.D0,&
          -1521.D0,      0.D0,     9.D0,      647.D0,    0.D0,    4.D0,&
           1314.D0,      0.D0,     0.D0,     -700.D0,    0.D0,    0.D0,&
          -1283.D0,      0.D0,     0.D0,      672.D0,    0.D0,    0.D0,&
          -1331.D0,      0.D0,     8.D0,      663.D0,    0.D0,    4.D0,&
           1383.D0,      0.D0,    -2.D0,     -594.D0,    0.D0,   -2.D0,&
           1405.D0,      0.D0,     4.D0,     -610.D0,    0.D0,    2.D0,&
           1290.D0,      0.D0,     0.D0,     -556.D0,    0.D0,    0.D0,&
          -1214.D0,      0.D0,     5.D0,      518.D0,    0.D0,    2.D0,&
           1146.D0,      0.D0,    -3.D0,     -490.D0,    0.D0,   -1.D0,&
           1019.D0,      0.D0,    -1.D0,     -527.D0,    0.D0,   -1.D0/
      DATA ( ( CLS(I,J), I=1,6 ), J= 81,100 ) /&
          -1100.D0,      0.D0,     9.D0,      465.D0,    0.D0,    4.D0,&
           -970.D0,      0.D0,     2.D0,      496.D0,    0.D0,    1.D0,&
           1575.D0,      0.D0,    -6.D0,      -50.D0,    0.D0,    0.D0,&
            934.D0,      0.D0,    -3.D0,     -399.D0,    0.D0,   -1.D0,&
            922.D0,      0.D0,    -1.D0,     -395.D0,    0.D0,   -1.D0,&
            815.D0,      0.D0,    -1.D0,     -422.D0,    0.D0,   -1.D0,&
            834.D0,      0.D0,     2.D0,     -440.D0,    0.D0,    1.D0,&
           1248.D0,      0.D0,     0.D0,     -170.D0,    0.D0,    1.D0,&
           1338.D0,      0.D0,    -5.D0,      -39.D0,    0.D0,    0.D0,&
            716.D0,      0.D0,    -2.D0,     -389.D0,    0.D0,   -1.D0,&
           1282.D0,      0.D0,    -3.D0,      -23.D0,    0.D0,    1.D0,&
            742.D0,      0.D0,     1.D0,     -391.D0,    0.D0,    0.D0,&
           1020.D0,      0.D0,   -25.D0,     -495.D0,    0.D0,  -10.D0,&
            715.D0,      0.D0,    -4.D0,     -326.D0,    0.D0,    2.D0,&
           -666.D0,      0.D0,    -3.D0,      369.D0,    0.D0,   -1.D0,&
           -667.D0,      0.D0,     1.D0,      346.D0,    0.D0,    1.D0,&
           -704.D0,      0.D0,     0.D0,      304.D0,    0.D0,    0.D0,&
           -694.D0,      0.D0,     5.D0,      294.D0,    0.D0,    2.D0,&
          -1014.D0,      0.D0,    -1.D0,        4.D0,    0.D0,   -1.D0,&
           -585.D0,      0.D0,    -2.D0,      316.D0,    0.D0,   -1.D0/
      DATA ( ( CLS(I,J), I=1,6 ), J=101,120 ) /&
           -949.D0,      0.D0,     1.D0,        8.D0,    0.D0,   -1.D0,&
           -595.D0,      0.D0,     0.D0,      258.D0,    0.D0,    0.D0,&
            528.D0,      0.D0,     0.D0,     -279.D0,    0.D0,    0.D0,&
           -590.D0,      0.D0,     4.D0,      252.D0,    0.D0,    2.D0,&
            570.D0,      0.D0,    -2.D0,     -244.D0,    0.D0,   -1.D0,&
           -502.D0,      0.D0,     3.D0,      250.D0,    0.D0,    2.D0,&
           -875.D0,      0.D0,     1.D0,       29.D0,    0.D0,    0.D0,&
           -492.D0,      0.D0,    -3.D0,      275.D0,    0.D0,   -1.D0,&
            535.D0,      0.D0,    -2.D0,     -228.D0,    0.D0,   -1.D0,&
           -467.D0,      0.D0,     1.D0,      240.D0,    0.D0,    1.D0,&
            591.D0,      0.D0,     0.D0,     -253.D0,    0.D0,    0.D0,&
           -453.D0,      0.D0,    -1.D0,      244.D0,    0.D0,   -1.D0,&
            766.D0,      0.D0,     1.D0,        9.D0,    0.D0,    0.D0,&
           -446.D0,      0.D0,     2.D0,      225.D0,    0.D0,    1.D0,&
           -488.D0,      0.D0,     2.D0,      207.D0,    0.D0,    1.D0,&
           -468.D0,      0.D0,     0.D0,      201.D0,    0.D0,    0.D0,&
           -421.D0,      0.D0,     1.D0,      216.D0,    0.D0,    1.D0,&
            463.D0,      0.D0,     0.D0,     -200.D0,    0.D0,    0.D0,&
           -673.D0,      0.D0,     2.D0,       14.D0,    0.D0,    0.D0,&
            658.D0,      0.D0,     0.D0,       -2.D0,    0.D0,    0.D0/
      DATA ( ( CLS(I,J), I=1,6 ), J=121,140 ) /&
           -438.D0,      0.D0,     0.D0,      188.D0,    0.D0,    0.D0,&
           -390.D0,      0.D0,     0.D0,      205.D0,    0.D0,    0.D0,&
            639.D0,    -11.D0,    -2.D0,      -19.D0,    0.D0,    0.D0,&
            412.D0,      0.D0,    -2.D0,     -176.D0,    0.D0,   -1.D0,&
           -361.D0,      0.D0,     0.D0,      189.D0,    0.D0,    0.D0,&
            360.D0,      0.D0,    -1.D0,     -185.D0,    0.D0,   -1.D0,&
            588.D0,      0.D0,    -3.D0,      -24.D0,    0.D0,    0.D0,&
           -578.D0,      0.D0,     1.D0,        5.D0,    0.D0,    0.D0,&
           -396.D0,      0.D0,     0.D0,      171.D0,    0.D0,    0.D0,&
            565.D0,      0.D0,    -1.D0,       -6.D0,    0.D0,    0.D0,&
           -335.D0,      0.D0,    -1.D0,      184.D0,    0.D0,   -1.D0,&
            357.D0,      0.D0,     1.D0,     -154.D0,    0.D0,    0.D0,&
            321.D0,      0.D0,     1.D0,     -174.D0,    0.D0,    0.D0,&
           -301.D0,      0.D0,    -1.D0,      162.D0,    0.D0,    0.D0,&
           -334.D0,      0.D0,     0.D0,      144.D0,    0.D0,    0.D0,&
            493.D0,      0.D0,    -2.D0,      -15.D0,    0.D0,    0.D0,&
            494.D0,      0.D0,    -2.D0,      -19.D0,    0.D0,    0.D0,&
            337.D0,      0.D0,    -1.D0,     -143.D0,    0.D0,   -1.D0,&
            280.D0,      0.D0,    -1.D0,     -144.D0,    0.D0,    0.D0,&
            309.D0,      0.D0,     1.D0,     -134.D0,    0.D0,    0.D0/
      DATA ( ( CLS(I,J), I=1,6 ), J=141,160 ) /&
           -263.D0,      0.D0,     2.D0,      131.D0,    0.D0,    1.D0,&
            253.D0,      0.D0,     1.D0,     -138.D0,    0.D0,    0.D0,&
            245.D0,      0.D0,     0.D0,     -128.D0,    0.D0,    0.D0,&
            416.D0,      0.D0,    -2.D0,      -17.D0,    0.D0,    0.D0,&
           -229.D0,      0.D0,     0.D0,      128.D0,    0.D0,    0.D0,&
            231.D0,      0.D0,     0.D0,     -120.D0,    0.D0,    0.D0,&
           -259.D0,      0.D0,     2.D0,      109.D0,    0.D0,    1.D0,&
            375.D0,      0.D0,    -1.D0,       -8.D0,    0.D0,    0.D0,&
            252.D0,      0.D0,     0.D0,     -108.D0,    0.D0,    0.D0,&
           -245.D0,      0.D0,     1.D0,      104.D0,    0.D0,    0.D0,&
            243.D0,      0.D0,    -1.D0,     -104.D0,    0.D0,    0.D0,&
            208.D0,      0.D0,     1.D0,     -112.D0,    0.D0,    0.D0,&
            199.D0,      0.D0,     0.D0,     -102.D0,    0.D0,    0.D0,&
           -208.D0,      0.D0,     1.D0,      105.D0,    0.D0,    0.D0,&
            335.D0,      0.D0,    -2.D0,      -14.D0,    0.D0,    0.D0,&
           -325.D0,      0.D0,     1.D0,        7.D0,    0.D0,    0.D0,&
           -187.D0,      0.D0,     0.D0,       96.D0,    0.D0,    0.D0,&
            197.D0,      0.D0,    -1.D0,     -100.D0,    0.D0,    0.D0,&
           -192.D0,      0.D0,     2.D0,       94.D0,    0.D0,    1.D0,&
           -188.D0,      0.D0,     0.D0,       83.D0,    0.D0,    0.D0/
      DATA ( ( CLS(I,J), I=1,6 ), J=161,180 ) /&
            276.D0,      0.D0,     0.D0,       -2.D0,    0.D0,    0.D0,&
           -286.D0,      0.D0,     1.D0,        6.D0,    0.D0,    0.D0,&
            186.D0,      0.D0,    -1.D0,      -79.D0,    0.D0,    0.D0,&
           -219.D0,      0.D0,     0.D0,       43.D0,    0.D0,    0.D0,&
            276.D0,      0.D0,     0.D0,        2.D0,    0.D0,    0.D0,&
           -153.D0,      0.D0,    -1.D0,       84.D0,    0.D0,    0.D0,&
           -156.D0,      0.D0,     0.D0,       81.D0,    0.D0,    0.D0,&
           -154.D0,      0.D0,     1.D0,       78.D0,    0.D0,    0.D0,&
           -174.D0,      0.D0,     1.D0,       75.D0,    0.D0,    0.D0,&
           -163.D0,      0.D0,     2.D0,       69.D0,    0.D0,    1.D0,&
           -228.D0,      0.D0,     0.D0,        1.D0,    0.D0,    0.D0,&
             91.D0,      0.D0,    -4.D0,      -54.D0,    0.D0,   -2.D0,&
            175.D0,      0.D0,     0.D0,      -75.D0,    0.D0,    0.D0,&
           -159.D0,      0.D0,     0.D0,       69.D0,    0.D0,    0.D0,&
            141.D0,      0.D0,     0.D0,      -72.D0,    0.D0,    0.D0,&
            147.D0,      0.D0,     0.D0,      -75.D0,    0.D0,    0.D0,&
           -132.D0,      0.D0,     0.D0,       69.D0,    0.D0,    0.D0,&
            159.D0,      0.D0,   -28.D0,      -54.D0,    0.D0,   11.D0,&
            213.D0,      0.D0,     0.D0,       -4.D0,    0.D0,    0.D0,&
            123.D0,      0.D0,     0.D0,      -64.D0,    0.D0,    0.D0/
      DATA ( ( CLS(I,J), I=1,6 ), J=181,200 ) /&
           -118.D0,      0.D0,    -1.D0,       66.D0,    0.D0,    0.D0,&
            144.D0,      0.D0,    -1.D0,      -61.D0,    0.D0,    0.D0,&
           -121.D0,      0.D0,     1.D0,       60.D0,    0.D0,    0.D0,&
           -134.D0,      0.D0,     1.D0,       56.D0,    0.D0,    1.D0,&
           -105.D0,      0.D0,     0.D0,       57.D0,    0.D0,    0.D0,&
           -102.D0,      0.D0,     0.D0,       56.D0,    0.D0,    0.D0,&
            120.D0,      0.D0,     0.D0,      -52.D0,    0.D0,    0.D0,&
            101.D0,      0.D0,     0.D0,      -54.D0,    0.D0,    0.D0,&
           -113.D0,      0.D0,     0.D0,       59.D0,    0.D0,    0.D0,&
           -106.D0,      0.D0,     0.D0,       61.D0,    0.D0,    0.D0,&
           -129.D0,      0.D0,     1.D0,       55.D0,    0.D0,    0.D0,&
           -114.D0,      0.D0,     0.D0,       57.D0,    0.D0,    0.D0,&
            113.D0,      0.D0,    -1.D0,      -49.D0,    0.D0,    0.D0,&
           -102.D0,      0.D0,     0.D0,       44.D0,    0.D0,    0.D0,&
            -94.D0,      0.D0,     0.D0,       51.D0,    0.D0,    0.D0,&
           -100.D0,      0.D0,    -1.D0,       56.D0,    0.D0,    0.D0,&
             87.D0,      0.D0,     0.D0,      -47.D0,    0.D0,    0.D0,&
            161.D0,      0.D0,     0.D0,       -1.D0,    0.D0,    0.D0,&
             96.D0,      0.D0,     0.D0,      -50.D0,    0.D0,    0.D0,&
            151.D0,      0.D0,    -1.D0,       -5.D0,    0.D0,    0.D0/
      DATA ( ( CLS(I,J), I=1,6 ), J=201,220 ) /&
           -104.D0,      0.D0,     0.D0,       44.D0,    0.D0,    0.D0,&
           -110.D0,      0.D0,     0.D0,       48.D0,    0.D0,    0.D0,&
           -100.D0,      0.D0,     1.D0,       50.D0,    0.D0,    0.D0,&
             92.D0,      0.D0,    -5.D0,       12.D0,    0.D0,   -2.D0,&
             82.D0,      0.D0,     0.D0,      -45.D0,    0.D0,    0.D0,&
             82.D0,      0.D0,     0.D0,      -45.D0,    0.D0,    0.D0,&
            -78.D0,      0.D0,     0.D0,       41.D0,    0.D0,    0.D0,&
            -77.D0,      0.D0,     0.D0,       43.D0,    0.D0,    0.D0,&
              2.D0,      0.D0,     0.D0,       54.D0,    0.D0,    0.D0,&
             94.D0,      0.D0,     0.D0,      -40.D0,    0.D0,    0.D0,&
            -93.D0,      0.D0,     0.D0,       40.D0,    0.D0,    0.D0,&
            -83.D0,      0.D0,    10.D0,       40.D0,    0.D0,   -2.D0,&
             83.D0,      0.D0,     0.D0,      -36.D0,    0.D0,    0.D0,&
            -91.D0,      0.D0,     0.D0,       39.D0,    0.D0,    0.D0,&
            128.D0,      0.D0,     0.D0,       -1.D0,    0.D0,    0.D0,&
            -79.D0,      0.D0,     0.D0,       34.D0,    0.D0,    0.D0,&
            -83.D0,      0.D0,     0.D0,       47.D0,    0.D0,    0.D0,&
             84.D0,      0.D0,     0.D0,      -44.D0,    0.D0,    0.D0,&
             83.D0,      0.D0,     0.D0,      -43.D0,    0.D0,    0.D0,&
             91.D0,      0.D0,     0.D0,      -39.D0,    0.D0,    0.D0/
      DATA ( ( CLS(I,J), I=1,6 ), J=221,240 ) /&
            -77.D0,      0.D0,     0.D0,       39.D0,    0.D0,    0.D0,&
             84.D0,      0.D0,     0.D0,      -43.D0,    0.D0,    0.D0,&
            -92.D0,      0.D0,     1.D0,       39.D0,    0.D0,    0.D0,&
            -92.D0,      0.D0,     1.D0,       39.D0,    0.D0,    0.D0,&
            -94.D0,      0.D0,     0.D0,        0.D0,    0.D0,    0.D0,&
             68.D0,      0.D0,     0.D0,      -36.D0,    0.D0,    0.D0,&
            -61.D0,      0.D0,     0.D0,       32.D0,    0.D0,    0.D0,&
             71.D0,      0.D0,     0.D0,      -31.D0,    0.D0,    0.D0,&
             62.D0,      0.D0,     0.D0,      -34.D0,    0.D0,    0.D0,&
            -63.D0,      0.D0,     0.D0,       33.D0,    0.D0,    0.D0,&
            -73.D0,      0.D0,     0.D0,       32.D0,    0.D0,    0.D0,&
            115.D0,      0.D0,     0.D0,       -2.D0,    0.D0,    0.D0,&
           -103.D0,      0.D0,     0.D0,        2.D0,    0.D0,    0.D0,&
             63.D0,      0.D0,     0.D0,      -28.D0,    0.D0,    0.D0,&
             74.D0,      0.D0,     0.D0,      -32.D0,    0.D0,    0.D0,&
           -103.D0,      0.D0,    -3.D0,        3.D0,    0.D0,   -1.D0,&
            -69.D0,      0.D0,     0.D0,       30.D0,    0.D0,    0.D0,&
             57.D0,      0.D0,     0.D0,      -29.D0,    0.D0,    0.D0,&
             94.D0,      0.D0,     0.D0,       -4.D0,    0.D0,    0.D0,&
             64.D0,      0.D0,     0.D0,      -33.D0,    0.D0,    0.D0/
      DATA ( ( CLS(I,J), I=1,6 ), J=241,260 ) /&
            -63.D0,      0.D0,     0.D0,       26.D0,    0.D0,    0.D0,&
            -38.D0,      0.D0,     0.D0,       20.D0,    0.D0,    0.D0,&
            -43.D0,      0.D0,     0.D0,       24.D0,    0.D0,    0.D0,&
            -45.D0,      0.D0,     0.D0,       23.D0,    0.D0,    0.D0,&
             47.D0,      0.D0,     0.D0,      -24.D0,    0.D0,    0.D0,&
            -48.D0,      0.D0,     0.D0,       25.D0,    0.D0,    0.D0,&
             45.D0,      0.D0,     0.D0,      -26.D0,    0.D0,    0.D0,&
             56.D0,      0.D0,     0.D0,      -25.D0,    0.D0,    0.D0,&
             88.D0,      0.D0,     0.D0,        2.D0,    0.D0,    0.D0,&
            -75.D0,      0.D0,     0.D0,        0.D0,    0.D0,    0.D0,&
             85.D0,      0.D0,     0.D0,        0.D0,    0.D0,    0.D0,&
             49.D0,      0.D0,     0.D0,      -26.D0,    0.D0,    0.D0,&
            -74.D0,      0.D0,    -3.D0,       -1.D0,    0.D0,   -1.D0,&
            -39.D0,      0.D0,     0.D0,       21.D0,    0.D0,    0.D0,&
             45.D0,      0.D0,     0.D0,      -20.D0,    0.D0,    0.D0,&
             51.D0,      0.D0,     0.D0,      -22.D0,    0.D0,    0.D0,&
            -40.D0,      0.D0,     0.D0,       21.D0,    0.D0,    0.D0,&
             41.D0,      0.D0,     0.D0,      -21.D0,    0.D0,    0.D0,&
            -42.D0,      0.D0,     0.D0,       24.D0,    0.D0,    0.D0,&
            -51.D0,      0.D0,     0.D0,       22.D0,    0.D0,    0.D0/
      DATA ( ( CLS(I,J), I=1,6 ), J=261,280 ) /&
            -42.D0,      0.D0,     0.D0,       22.D0,    0.D0,    0.D0,&
             39.D0,      0.D0,     0.D0,      -21.D0,    0.D0,    0.D0,&
             46.D0,      0.D0,     0.D0,      -18.D0,    0.D0,    0.D0,&
            -53.D0,      0.D0,     0.D0,       22.D0,    0.D0,    0.D0,&
             82.D0,      0.D0,     0.D0,       -4.D0,    0.D0,    0.D0,&
             81.D0,      0.D0,    -1.D0,       -4.D0,    0.D0,    0.D0,&
             47.D0,      0.D0,     0.D0,      -19.D0,    0.D0,    0.D0,&
             53.D0,      0.D0,     0.D0,      -23.D0,    0.D0,    0.D0,&
            -45.D0,      0.D0,     0.D0,       22.D0,    0.D0,    0.D0,&
            -44.D0,      0.D0,     0.D0,       -2.D0,    0.D0,    0.D0,&
            -33.D0,      0.D0,     0.D0,       16.D0,    0.D0,    0.D0,&
            -61.D0,      0.D0,     0.D0,        1.D0,    0.D0,    0.D0,&
            -38.D0,      0.D0,     0.D0,       19.D0,    0.D0,    0.D0,&
            -33.D0,      0.D0,     0.D0,       21.D0,    0.D0,    0.D0,&
            -60.D0,      0.D0,     0.D0,        0.D0,    0.D0,    0.D0,&
             48.D0,      0.D0,     0.D0,      -10.D0,    0.D0,    0.D0,&
             38.D0,      0.D0,     0.D0,      -20.D0,    0.D0,    0.D0,&
             31.D0,      0.D0,     0.D0,      -13.D0,    0.D0,    0.D0,&
            -32.D0,      0.D0,     0.D0,       15.D0,    0.D0,    0.D0,&
             45.D0,      0.D0,     0.D0,       -8.D0,    0.D0,    0.D0/
      DATA ( ( CLS(I,J), I=1,6 ), J=281,300 ) /&
            -44.D0,      0.D0,     0.D0,       19.D0,    0.D0,    0.D0,&
            -51.D0,      0.D0,     0.D0,        0.D0,    0.D0,    0.D0,&
            -36.D0,      0.D0,     0.D0,       20.D0,    0.D0,    0.D0,&
             44.D0,      0.D0,     0.D0,      -19.D0,    0.D0,    0.D0,&
            -60.D0,      0.D0,     0.D0,        2.D0,    0.D0,    0.D0,&
             35.D0,      0.D0,     0.D0,      -18.D0,    0.D0,    0.D0,&
             47.D0,      0.D0,     0.D0,       -1.D0,    0.D0,    0.D0,&
             36.D0,      0.D0,     0.D0,      -15.D0,    0.D0,    0.D0,&
            -36.D0,      0.D0,     0.D0,       20.D0,    0.D0,    0.D0,&
            -35.D0,      0.D0,     0.D0,       19.D0,    0.D0,    0.D0,&
            -37.D0,      0.D0,     0.D0,       19.D0,    0.D0,    0.D0,&
             32.D0,      0.D0,     0.D0,      -16.D0,    0.D0,    0.D0,&
             35.D0,      0.D0,     0.D0,      -14.D0,    0.D0,    0.D0,&
             32.D0,      0.D0,     0.D0,      -13.D0,    0.D0,    0.D0,&
             65.D0,      0.D0,     0.D0,       -2.D0,    0.D0,    0.D0,&
             47.D0,      0.D0,     0.D0,       -1.D0,    0.D0,    0.D0,&
             32.D0,      0.D0,     0.D0,      -16.D0,    0.D0,    0.D0,&
             37.D0,      0.D0,     0.D0,      -16.D0,    0.D0,    0.D0,&
            -30.D0,      0.D0,     0.D0,       15.D0,    0.D0,    0.D0,&
            -32.D0,      0.D0,     0.D0,       16.D0,    0.D0,    0.D0/
      DATA ( ( CLS(I,J), I=1,6 ), J=301,320 ) /&
            -31.D0,      0.D0,     0.D0,       13.D0,    0.D0,    0.D0,&
             37.D0,      0.D0,     0.D0,      -16.D0,    0.D0,    0.D0,&
             31.D0,      0.D0,     0.D0,      -13.D0,    0.D0,    0.D0,&
             49.D0,      0.D0,     0.D0,       -2.D0,    0.D0,    0.D0,&
             32.D0,      0.D0,     0.D0,      -13.D0,    0.D0,    0.D0,&
            -43.D0,      0.D0,     0.D0,       18.D0,    0.D0,    0.D0,&
            -32.D0,      0.D0,     0.D0,       14.D0,    0.D0,    0.D0,&
             30.D0,      0.D0,     0.D0,        0.D0,    0.D0,    0.D0,&
            -34.D0,      0.D0,     0.D0,       15.D0,    0.D0,    0.D0,&
            -36.D0,      0.D0,     0.D0,        0.D0,    0.D0,    0.D0,&
            -38.D0,      0.D0,     0.D0,        0.D0,    0.D0,    0.D0,&
            -31.D0,      0.D0,     0.D0,        0.D0,    0.D0,    0.D0,&
            -34.D0,      0.D0,     0.D0,        0.D0,    0.D0,    0.D0,&
            -35.D0,      0.D0,     0.D0,        0.D0,    0.D0,    0.D0,&
             30.D0,      0.D0,     0.D0,       -2.D0,    0.D0,    0.D0,&
              0.D0,      0.D0, -1988.D0,        0.D0,    0.D0,-1679.D0,&
              0.D0,      0.D0,   -63.D0,        0.D0,    0.D0,  -27.D0,&
              0.D0,      0.D0,   364.D0,        0.D0,    0.D0,  176.D0,&
              0.D0,      0.D0, -1044.D0,        0.D0,    0.D0, -891.D0,&
              0.D0,      0.D0,   330.D0,        0.D0,    0.D0,    0.D0/
      DATA ( ( CLS(I,J), I=1,6 ), J=321,323 ) /&
              0.D0,      0.D0,    30.D0,        0.D0,    0.D0,   14.D0,&
              0.D0,      0.D0,  -162.D0,        0.D0,    0.D0, -138.D0,&
              0.D0,      0.D0,    75.D0,        0.D0,    0.D0,    0.D0/

!*
!*  Planetary argument multipliers:
!*              L   L'  F   D   Om  Me  Ve  E  Ma  Ju  Sa  Ur  Ne  pre
!*
      DATA ( ( NAPL(I,J), I=1,14), J=  1, 20 ) /&
              0,  0,  0,  0,  0,  0,  0,  8,-16,  4,  5,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0, -8, 16, -4, -5,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  0,  8,-16,  4,  5,  0,  0,  2,&
              0,  0,  1, -1,  1,  0,  0,  3, -8,  3,  0,  0,  0,  0,&
             -1,  0,  0,  0,  0,  0, 10, -3,  0,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  4, -8,  3,  0,  0,  0,  0,&
              0,  0,  1, -1,  1,  0,  0, -5,  8, -3,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  0,  0,  2, -5,  0,  0,  1,&
              0,  0,  1, -1,  1,  0,  0, -1,  0,  2, -5,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  0,  0,  2, -5,  0,  0,  0,&
              0,  0,  1, -1,  1,  0,  0, -1,  0, -2,  5,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  0,  0, -2,  5,  0,  0,  1,&
              1,  0,  0, -2,  0,  0, 19,-21,  3,  0,  0,  0,  0,  0,&
              1,  0,  0, -1,  1,  0,  0, -1,  0,  2,  0,  0,  0,  0,&
             -2,  0,  0,  2,  1,  0,  0,  2,  0, -2,  0,  0,  0,  0,&
             -1,  0,  0,  0,  0,  0, 18,-16,  0,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0, -8, 13,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0, -8, 13,  0,  0,  0,  0,  0,  1,&
              0,  0,  1, -1,  1,  0, -8, 12,  0,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  8,-13,  0,  0,  0,  0,  0,  0/
      DATA ( ( NAPL(I,J), I=1,14), J= 21, 40 ) /&
              0,  0, -1,  1,  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  1,  0,  0, -1,  2,  0,  0,  0,  0,  0,&
             -1,  0,  0,  1,  0,  0,  3, -4,  0,  0,  0,  0,  0,  0,&
              0,  0, -1,  1,  0,  0,  0,  1,  0,  0,  1,  0,  0,  0,&
              0,  0, -2,  2,  0,  0,  5, -6,  0,  0,  0,  0,  0,  0,&
             -2,  0,  0,  2,  0,  0,  6, -8,  0,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  8,-15,  0,  0,  0,  0,  0,&
              2,  0,  0, -2,  1,  0,  0, -2,  0,  3,  0,  0,  0,  0,&
             -2,  0,  0,  2,  0,  0,  0,  2,  0, -3,  0,  0,  0,  0,&
             -1,  0,  0,  1,  0,  0,  0,  1,  0, -1,  0,  0,  0,  0,&
              0,  0, -1,  1,  0,  0,  0,  1,  0,  1,  0,  0,  0,  0,&
              0,  0,  0,  0,  1,  0,  0,  0,  0,  1,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  0,  0,  0, -1,  0,  0,  1,&
              0,  0,  1, -1,  1,  0,  0, -1,  0,  0, -1,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,&
              0,  0,  1, -1,  1,  0,  0, -1,  0,  0,  1,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  1,&
              0,  0,  0,  0,  1,  0,  8,-13,  0,  0,  0,  0,  0,  0,&
             -1,  0,  0,  0,  1,  0, 18,-16,  0,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  1,  0,  0,  0,  0, -2,  5,  0,  0,  0/
      DATA ( ( NAPL(I,J), I=1,14), J= 41, 60 ) /&
              0,  0,  0,  0,  1,  0,  0, -4,  8, -3,  0,  0,  0,  0,&
              0,  0,  0,  0,  1,  0,  0,  4, -8,  3,  0,  0,  0,  0,&
              0,  0,  0,  0,  1,  0,  0,  0,  0,  2, -5,  0,  0,  0,&
             -2,  0,  0,  2,  0,  0,  0,  2,  0, -2,  0,  0,  0,  0,&
              1,  0,  0,  0,  1,  0,-18, 16,  0,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  1,  0, -8, 13,  0,  0,  0,  0,  0,  0,&
              0,  0,  1, -1,  1,  0,  0,  0, -2,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  1, -2,  0,  0,  0,  0,  0,&
              0,  0,  1, -1,  1,  0,  0, -2,  2,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0, -1,  2,  0,  0,  0,  0,  1,&
              0,  0,  1, -1,  1,  0,  0, -1,  0,  0,  2,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  2,  0,  0,  1,&
              0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  2,  0,  0,  2,&
              0,  0,  2, -2,  1,  0, -5,  6,  0,  0,  0,  0,  0,  0,&
              0,  0, -1,  1,  0,  0,  5, -7,  0,  0,  0,  0,  0,  0,&
             -2,  0,  0,  2,  1,  0,  0,  2,  0, -3,  0,  0,  0,  0,&
              0,  0,  1, -1,  1,  0,  0, -1,  0, -1,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  0,  0, -1,  0,  0,  0,  1,&
              0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  1/
      DATA ( ( NAPL(I,J), I=1,14), J= 61, 80 ) /&
              0,  0,  1, -1,  1,  0,  0, -1,  0,  1,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  0,  0,  2,&
             -2,  0,  0,  2,  0,  0,  3, -3,  0,  0,  0,  0,  0,  0,&
              2,  0,  0, -2,  1,  0,  0, -2,  0,  2,  0,  0,  0,  0,&
              0,  0,  0,  0,  1,  0,  0,  1, -2,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  3, -5,  0,  0,  0,  0,  0,  0,&
              0,  0,  1, -1,  1,  0, -3,  4,  0,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0, -3,  5,  0,  0,  0,  0,  0,  1,&
              0,  0,  0,  0,  0,  0, -3,  5,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0, -3,  5,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  0,  2, -4,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0, -2,  4,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0, -5,  8,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0, -5,  8,  0,  0,  0,  0,  0,  1,&
              0,  0,  1, -1,  1,  0, -5,  7,  0,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0, -5,  8,  0,  0,  0,  0,  0,  1,&
              0,  0,  0,  0,  0,  0,  5, -8,  0,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  0,  0,  2,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  0,  0,  2,  0,  0,  0,  1,&
              0,  0,  1, -1,  1,  0,  0, -1,  0,  2,  0,  0,  0,  0/
      DATA ( ( NAPL(I,J), I=1,14), J= 81,100 ) /&
              0,  0,  0,  0,  0,  0,  0,  0,  0,  2,  0,  0,  0,  1,&
              0,  0,  0,  0,  0,  0,  0,  0,  0,  2,  0,  0,  0,  2,&
              0,  0,  2, -2,  1,  0, -3,  3,  0,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  3, -6,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  1,  0,  2, -3,  0,  0,  0,  0,  0,  0,&
              0,  0,  2, -2,  1,  0,  0, -2,  0,  2,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0, -2,  3,  0,  0,  0,  0,  0,  1,&
              0,  0,  0,  0,  0,  0,  2, -3,  0,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  0,  0,  3,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  0,  3, -5,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  1, -2,  0,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  2, -3,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0, -4,  7,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0, -4,  6,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0, -4,  6,  0,  0,  0,  0,  0,  1,&
              0,  0,  0,  0,  0,  0,  4, -6,  0,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0, -1,  1,  0,  0,  0,  0,  0,  1,&
              0,  0,  0,  0,  0,  0,  1, -1,  0,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  1, -1,  0,  0,  0,  0,  0,  1,&
              0,  0,  0,  0,  0,  0,  0, -1,  0,  4,  0,  0,  0,  2/
      DATA ( ( NAPL(I,J), I=1,14), J=101,120 ) /&
              0,  0,  0,  0,  0,  0,  0, -1,  0,  3,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  0,  1,  0, -3,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0, -2,  4,  0,  0,  0,  0,  0,  1,&
              0,  0,  0,  0,  0,  0, -2,  4,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0, -6,  9,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  0,  1,  0, -2,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  3, -4,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  3, -4,  0,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  1,  0, -1,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  2, -2,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  1,  0,  0, -1,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  1,  0,  2, -5,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  0,  2,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  0,  1,  0,  1,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0, -5,  7,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0, -5,  7,  0,  0,  0,  0,  0,  1,&
              0,  0,  0,  0,  0,  0,  0,  1,  0,  2,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0, -2,  2,  0,  0,  0,  0,  0,  1,&
              0,  0,  0,  0,  0,  0,  2, -2,  0,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0, -1,  3,  0,  0,  0,  0,  0,  1/
      DATA ( ( NAPL(I,J), I=1,14), J=121,140 ) /&
              0,  0,  0,  0,  0,  0, -1,  3,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  0,  2,  0, -3,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0, -2,  5,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0, -6,  8,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  0,  2,  0, -2,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  4, -4,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0, -3,  3,  0,  0,  0,  0,  0,  1,&
              0,  0,  0,  0,  0,  0,  3, -3,  0,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  3, -3,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  0,  2,  0, -1,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  2,  0, -1,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  0,  3, -2,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0, -8, 15,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  0,  6, -8,  3,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  1,&
              0,  0,  0,  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  0, -6, 16, -4, -5,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  0, -2,  8, -3,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0, -8, 11,  0,  0,  0,  0,  0,  2/
      DATA ( ( NAPL(I,J), I=1,14), J=141,160 ) /&
              0,  0,  0,  0,  0,  0,  0,  1,  2,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  0,  2,  0,  1,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0, -3,  7,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  0,  0,  4,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  2, -1,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0, -7,  9,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  4, -4,  0,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  1,  1,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  0,  3,  0, -2,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  0,  5, -4,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  3, -2,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  0,  3,  0, -1,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  0,  4, -2,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0, -8, 10,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  5, -5,  0,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0, -9, 11,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  0,  4,  0, -3,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  6, -6,  0,  0,  0,  0,  0,  0,&
              0,  0,  0,  0,  0,  0,  0,  4,  0, -2,  0,  0,  0,  2/
      DATA ( ( NAPL(I,J), I=1,14), J=161,165 ) /&
              0,  0,  0,  0,  0,  0,  3, -1,  0,  0,  0,  0,  0,  2,&
              0,  0,  0,  0,  0,  0,  0,  4,  0, -1,  0,  0,  0,  2,&
             -1,  0,  0,  2,  0,  0,  0,  2,  0, -2,  0,  0,  0,  0,&
              1,  0,  2,  0,  2,  0,  0,  1,  0,  0,  0,  0,  0,  0,&
             -1,  0,  2,  0,  2,  0,  0, -4,  8, -3,  0,  0,  0,  0/

!*
!*  Planetary nutation coefficients, unit 1e-7 arcsec:
!*  longitude (sin, cos), obliquity (sin, cos)
!*
      DATA ( ( CPL(I,J), I=1,4 ), J=  1, 20 ) /&
         1440.D0,       0.D0,       0.D0,       0.D0,&
           56.D0,    -117.D0,     -42.D0,     -40.D0,&
          125.D0,     -43.D0,       0.D0,     -54.D0,&
         -114.D0,       0.D0,       0.D0,      61.D0,&
         -219.D0,      89.D0,       0.D0,       0.D0,&
         -462.D0,    1604.D0,       0.D0,       0.D0,&
           99.D0,       0.D0,       0.D0,     -53.D0,&
           14.D0,    -218.D0,     117.D0,       8.D0,&
           31.D0,    -481.D0,    -257.D0,     -17.D0,&
         -491.D0,     128.D0,       0.D0,       0.D0,&
        -3084.D0,    5123.D0,    2735.D0,    1647.D0,&
        -1444.D0,    2409.D0,   -1286.D0,    -771.D0,&
          103.D0,     -60.D0,       0.D0,       0.D0,&
          -26.D0,     -29.D0,     -16.D0,      14.D0,&
          284.D0,       0.D0,       0.D0,    -151.D0,&
          226.D0,     101.D0,       0.D0,       0.D0,&
          -41.D0,     175.D0,      76.D0,      17.D0,&
          425.D0,     212.D0,    -133.D0,     269.D0,&
         1200.D0,     598.D0,     319.D0,    -641.D0,&
          235.D0,     334.D0,       0.D0,       0.D0/
      DATA ( ( CPL(I,J), I=1,4 ), J= 21, 40 ) /&
          266.D0,     -78.D0,       0.D0,       0.D0,&
         -460.D0,    -435.D0,    -232.D0,     246.D0,&
            0.D0,     131.D0,       0.D0,       0.D0,&
          -42.D0,      20.D0,       0.D0,       0.D0,&
          -10.D0,     233.D0,       0.D0,       0.D0,&
           78.D0,     -18.D0,       0.D0,       0.D0,&
           45.D0,     -22.D0,       0.D0,       0.D0,&
           89.D0,     -16.D0,      -9.D0,     -48.D0,&
         -349.D0,     -62.D0,       0.D0,       0.D0,&
          -53.D0,       0.D0,       0.D0,       0.D0,&
          -21.D0,     -78.D0,       0.D0,       0.D0,&
           20.D0,     -70.D0,     -37.D0,     -11.D0,&
           32.D0,      15.D0,      -8.D0,      17.D0,&
          174.D0,      84.D0,      45.D0,     -93.D0,&
           11.D0,      56.D0,       0.D0,       0.D0,&
          -66.D0,     -12.D0,      -6.D0,      35.D0,&
           47.D0,       8.D0,       4.D0,     -25.D0,&
           46.D0,      66.D0,      35.D0,     -25.D0,&
          -68.D0,     -34.D0,     -18.D0,      36.D0,&
           76.D0,      17.D0,       9.D0,     -41.D0/
      DATA ( ( CPL(I,J), I=1,4 ), J= 41, 60 ) /&
           84.D0,     298.D0,     159.D0,     -45.D0,&
          -82.D0,     292.D0,     156.D0,      44.D0,&
          -73.D0,      17.D0,       9.D0,      39.D0,&
         -439.D0,       0.D0,       0.D0,       0.D0,&
           57.D0,     -28.D0,     -15.D0,     -30.D0,&
          -40.D0,      57.D0,      30.D0,      21.D0,&
          273.D0,      80.D0,      43.D0,    -146.D0,&
         -449.D0,     430.D0,       0.D0,       0.D0,&
           -8.D0,     -47.D0,     -25.D0,       4.D0,&
            6.D0,      47.D0,      25.D0,      -3.D0,&
          -48.D0,    -110.D0,     -59.D0,      26.D0,&
           51.D0,     114.D0,      61.D0,     -27.D0,&
         -133.D0,       0.D0,       0.D0,      57.D0,&
          -18.D0,    -436.D0,    -233.D0,       9.D0,&
           35.D0,      -7.D0,       0.D0,       0.D0,&
          -53.D0,      -9.D0,      -5.D0,      28.D0,&
          -50.D0,     194.D0,     103.D0,      27.D0,&
          -13.D0,      52.D0,      28.D0,       7.D0,&
          -91.D0,     248.D0,       0.D0,       0.D0,&
            6.D0,      49.D0,      26.D0,      -3.D0/
      DATA ( ( CPL(I,J), I=1,4 ), J= 61, 80 ) /&
           -6.D0,     -47.D0,     -25.D0,       3.D0,&
           52.D0,      23.D0,      10.D0,     -23.D0,&
         -138.D0,       0.D0,       0.D0,       0.D0,&
           54.D0,       0.D0,       0.D0,     -29.D0,&
          -37.D0,      35.D0,      19.D0,      20.D0,&
         -145.D0,      47.D0,       0.D0,       0.D0,&
          -10.D0,      40.D0,      21.D0,       5.D0,&
           11.D0,     -49.D0,     -26.D0,      -7.D0,&
        -2150.D0,       0.D0,       0.D0,     932.D0,&
           85.D0,       0.D0,       0.D0,     -37.D0,&
          -86.D0,     153.D0,       0.D0,       0.D0,&
          -51.D0,       0.D0,       0.D0,      22.D0,&
          -11.D0,    -268.D0,    -116.D0,       5.D0,&
           31.D0,       6.D0,       3.D0,     -17.D0,&
          140.D0,      27.D0,      14.D0,     -75.D0,&
           57.D0,      11.D0,       6.D0,     -30.D0,&
          -14.D0,     -39.D0,       0.D0,       0.D0,&
          -25.D0,      22.D0,       0.D0,       0.D0,&
           42.D0,     223.D0,     119.D0,     -22.D0,&
          -27.D0,    -143.D0,     -77.D0,      14.D0/
      DATA ( ( CPL(I,J), I=1,4 ), J= 81,100 ) /&
            9.D0,      49.D0,      26.D0,      -5.D0,&
        -1166.D0,       0.D0,       0.D0,     505.D0,&
          117.D0,       0.D0,       0.D0,     -63.D0,&
            0.D0,      31.D0,       0.D0,       0.D0,&
            0.D0,     -32.D0,     -17.D0,       0.D0,&
           50.D0,       0.D0,       0.D0,     -27.D0,&
           30.D0,      -3.D0,      -2.D0,     -16.D0,&
            8.D0,     614.D0,       0.D0,       0.D0,&
         -127.D0,      21.D0,       9.D0,      55.D0,&
          -20.D0,      34.D0,       0.D0,       0.D0,&
           22.D0,     -87.D0,       0.D0,       0.D0,&
          -68.D0,      39.D0,       0.D0,       0.D0,&
            3.D0,      66.D0,      29.D0,      -1.D0,&
          490.D0,       0.D0,       0.D0,    -213.D0,&
          -22.D0,      93.D0,      49.D0,      12.D0,&
          -46.D0,      14.D0,       0.D0,       0.D0,&
           25.D0,     106.D0,      57.D0,     -13.D0,&
         1485.D0,       0.D0,       0.D0,       0.D0,&
           -7.D0,     -32.D0,     -17.D0,       4.D0,&
           30.D0,      -6.D0,      -2.D0,     -13.D0/
      DATA ( ( CPL(I,J), I=1,4 ), J=101,120 ) /&
          118.D0,       0.D0,       0.D0,     -52.D0,&
          -28.D0,      36.D0,       0.D0,       0.D0,&
           14.D0,     -59.D0,     -31.D0,      -8.D0,&
         -458.D0,       0.D0,       0.D0,     198.D0,&
            0.D0,     -45.D0,     -20.D0,       0.D0,&
         -166.D0,     269.D0,       0.D0,       0.D0,&
          -78.D0,      45.D0,       0.D0,       0.D0,&
           -5.D0,     328.D0,       0.D0,       0.D0,&
        -1223.D0,     -26.D0,       0.D0,       0.D0,&
         -368.D0,       0.D0,       0.D0,       0.D0,&
          -75.D0,       0.D0,       0.D0,       0.D0,&
          -13.D0,     -30.D0,       0.D0,       0.D0,&
          -74.D0,       0.D0,       0.D0,      32.D0,&
         -262.D0,       0.D0,       0.D0,     114.D0,&
          202.D0,       0.D0,       0.D0,     -87.D0,&
           -8.D0,      35.D0,      19.D0,       5.D0,&
          -35.D0,     -48.D0,     -21.D0,      15.D0,&
           12.D0,      55.D0,      29.D0,      -6.D0,&
         -598.D0,       0.D0,       0.D0,       0.D0,&
            8.D0,     -31.D0,     -16.D0,      -4.D0/
      DATA ( ( CPL(I,J), I=1,4 ), J=121,140 ) /&
          113.D0,       0.D0,       0.D0,     -49.D0,&
           83.D0,      15.D0,       0.D0,       0.D0,&
            0.D0,    -114.D0,     -49.D0,       0.D0,&
          117.D0,       0.D0,       0.D0,     -51.D0,&
          393.D0,       3.D0,       0.D0,       0.D0,&
           18.D0,     -29.D0,     -13.D0,      -8.D0,&
            8.D0,      34.D0,      18.D0,      -4.D0,&
           89.D0,       0.D0,       0.D0,       0.D0,&
           54.D0,     -15.D0,      -7.D0,     -24.D0,&
            0.D0,      35.D0,       0.D0,       0.D0,&
         -154.D0,     -30.D0,     -13.D0,      67.D0,&
           80.D0,     -71.D0,     -31.D0,     -35.D0,&
           61.D0,     -96.D0,     -42.D0,     -27.D0,&
          123.D0,    -415.D0,    -180.D0,     -53.D0,&
            0.D0,       0.D0,       0.D0,     -35.D0,&
            7.D0,     -32.D0,     -17.D0,      -4.D0,&
          -89.D0,       0.D0,       0.D0,      38.D0,&
            0.D0,     -86.D0,     -19.D0,      -6.D0,&
         -123.D0,    -416.D0,    -180.D0,      53.D0,&
          -62.D0,     -97.D0,     -42.D0,      27.D0/
      DATA ( ( CPL(I,J), I=1,4 ), J=141,160 ) /&
          -85.D0,     -70.D0,     -31.D0,      37.D0,&
          163.D0,     -12.D0,      -5.D0,     -72.D0,&
          -63.D0,     -16.D0,      -7.D0,      28.D0,&
          -21.D0,     -32.D0,     -14.D0,       9.D0,&
            5.D0,    -173.D0,     -75.D0,      -2.D0,&
           74.D0,       0.D0,       0.D0,     -32.D0,&
           83.D0,       0.D0,       0.D0,       0.D0,&
         -339.D0,       0.D0,       0.D0,     147.D0,&
           67.D0,     -91.D0,     -39.D0,     -29.D0,&
           30.D0,     -18.D0,      -8.D0,     -13.D0,&
            0.D0,    -114.D0,     -50.D0,       0.D0,&
          517.D0,      16.D0,       7.D0,    -224.D0,&
          143.D0,      -3.D0,      -1.D0,     -62.D0,&
           50.D0,       0.D0,       0.D0,     -22.D0,&
           59.D0,       0.D0,       0.D0,       0.D0,&
          370.D0,      -8.D0,       0.D0,    -160.D0,&
           34.D0,       0.D0,       0.D0,     -15.D0,&
          -37.D0,      -7.D0,      -3.D0,      16.D0,&
           40.D0,       0.D0,       0.D0,       0.D0,&
         -184.D0,      -3.D0,      -1.D0,      80.D0/
      DATA ( ( CPL(I,J), I=1,4 ), J=161,165 ) /&
           31.D0,      -6.D0,       0.D0,     -13.D0,&
           -3.D0,     -32.D0,     -14.D0,       1.D0,&
          -34.D0,       0.D0,       0.D0,       0.D0,&
          126.D0,     -63.D0,     -27.D0,     -55.D0,&
         -126.D0,     -63.D0,     -27.D0,      55.D0/

!* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

!*  Interval between fundamental epoch J2000.0 and given date (JC).
      T = ( ( DATE1-DJ0 ) + DATE2 ) / DJC

!*  -------------------
!*  LUNI-SOLAR NUTATION
!*  -------------------

!*
!*  Fundamental (Delaunay) arguments from Simon et al. (1994)
!*
      CALL FUNARG ( T,   EL, ELP, F, D, OM )

!*  Initialize the nutation values.
      DP = 0.D0
      DE = 0.D0

!*  Summation of luni-solar nutation series (in reverse order).
      DO 100 I = NLS, 1, -1

!*     Argument and functions.
         ARG = MOD ( DBLE ( NALS(1,I) ) * EL  + &
                    DBLE ( NALS(2,I) ) * ELP + &
                    DBLE ( NALS(3,I) ) * F   + &
                    DBLE ( NALS(4,I) ) * D   + &
                    DBLE ( NALS(5,I) ) * OM, D2PI )
         SARG = SIN(ARG)
         CARG = COS(ARG)

!*     Term.
         DP = DP + ( CLS(1,I) + CLS(2,I) * T ) * SARG &
                +   CLS(3,I)                  * CARG
         DE = DE + ( CLS(4,I) + CLS(5,I) * T ) * CARG &
                +   CLS(6,I)                  * SARG

 100  CONTINUE

!*  Convert from 0.1 microarcsec units to radians.
      DPSILS = DP * U2R
      DEPSLS = DE * U2R

!*  ------------------
!*  PLANETARY NUTATION
!*  ------------------

!*  Planetary longitudes, Mercury through Neptune, wrt mean dynamical
!*  ecliptic and equinox of J2000, with high order terms omitted
!*  (Simon et al. 1994, 5.8.1-5.8.8).
      ALME = MOD ( 4.402608842461D0 + 2608.790314157421D0 * T, D2PI )
      ALVE = MOD ( 3.176146696956D0 + 1021.328554621099D0 * T, D2PI )
      ALEA = MOD ( 1.753470459496D0 +  628.307584999142D0 * T, D2PI )
      ALMA = MOD ( 6.203476112911D0 +  334.061242669982D0 * T, D2PI )
      ALJU = MOD ( 0.599547105074D0 +   52.969096264064D0 * T, D2PI )
      ALSA = MOD ( 0.874016284019D0 +   21.329910496032D0 * T, D2PI )
      ALUR = MOD ( 5.481293871537D0 +    7.478159856729D0 * T, D2PI )
      ALNE = MOD ( 5.311886286677D0 +    3.813303563778D0 * T, D2PI )

!*  General precession in longitude (Simon et al. 1994), equivalent
!*  to 5028.8200 arcsec/cy at J2000.
      APA = ( 0.024380407358D0 + 0.000005391235D0 * T ) * T

!*  Initialize the nutation values.
      DP = 0.D0
      DE = 0.D0

!*  Summation of planetary nutation series (in reverse order).
      DO 200 I = NPL, 1, -1

!*     Argument and functions.
         ARG = MOD ( DBLE ( NAPL( 1,I) ) * EL   + &
                    DBLE ( NAPL( 2,I) ) * ELP  + &
                    DBLE ( NAPL( 3,I) ) * F    + &
                    DBLE ( NAPL( 4,I) ) * D    + &
                    DBLE ( NAPL( 5,I) ) * OM   + &
                    DBLE ( NAPL( 6,I) ) * ALME + &
                    DBLE ( NAPL( 7,I) ) * ALVE + &
                    DBLE ( NAPL( 8,I) ) * ALEA + &
                    DBLE ( NAPL( 9,I) ) * ALMA + &
                    DBLE ( NAPL(10,I) ) * ALJU + &
                    DBLE ( NAPL(11,I) ) * ALSA + &
                    DBLE ( NAPL(12,I) ) * ALUR + &
                    DBLE ( NAPL(13,I) ) * ALNE + &
                    DBLE ( NAPL(14,I) ) * APA, D2PI )
         SARG = SIN(ARG)
         CARG = COS(ARG)

!*     Term.
         DP = DP + CPL(1,I) * SARG + CPL(2,I) * CARG
         DE = DE + CPL(3,I) * SARG + CPL(4,I) * CARG

 200  CONTINUE

!*  Convert from 0.1 microarcsec units to radians.
      DPSIPL = DP * U2R
      DEPSPL = DE * U2R

!*  -----
!*  TOTAL
!*  -----

!*  Add planetary and luni-solar components.
      DPSI = DPSIPL + DPSILS
      DEPS = DEPSPL + DEPSLS

      RETURN

      END subroutine
!_____________________________________________________________________________-

end module
