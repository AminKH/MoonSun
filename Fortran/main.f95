! A fortran95 program for G95
! By Amin Khiabani
program main

      USE MoonSun
      USE MSISE
      USE NOVAS
      USE SOFA

      implicit none

      integer :: UT_TT, IREF, airModel, options
      integer :: H,M,S,I, airType,Iry,Irm,Ird,Year
      integer :: GdayofYear , PdayofYear,adjust,DSTop
      integer :: kk, Hy,Hm,Hd, Method, aidAccept
      integer :: CalenType, Iyear,Gy,Gm,Gd,J,k,StatOver
      integer :: UH,UM,US, Imonth, Iday, Azans
      integer,dimension(3) :: Jdate

      real(kind=8) :: Hour,B_Ilum,UJDtoday, LJD, JDM, DS
      real(kind=8) :: JDtoday,TJDT,moonAlt,moonAz,moondel
      real(kind=8) :: SMUJD, MoonAngle, newMoonJD, FdayHijriJD
      real(kind=8), dimension(2) :: atmos,moonJDRS,mRShours,moonAzim,DST
      real(kind=8), dimension(3) :: RSTJD,RTSangles
      real(kind=8), dimension(4) :: Geo, Angles
      real(kind=8), dimension(9) :: sunTimes
      real(kind=8), dimension(0:3) :: TJD
      real(kind=8), dimension(0:14,0:3):: MoonPhaseJD
      real(kind=8), dimension(0:14) :: HijriJD
      real(kind=4) :: alt,lat,long,Stl,Den,Tem,Pre
      real(kind=8) :: sunAlt,sunZen,sunAz,topAlfa,topoDelta
      real(8) :: mu,Density,a,ts,pp,rm
      real(8) :: qm,RR, UnewMoonJD,DELTA_T

      character(16) ,dimension(9) :: STimes
      character(10),dimension(7) :: weekDays
      character(16),dimension(2) :: MTimes
      character(16),dimension(0:3):: EquiSolislael
      character(13),dimension(0:3) :: GregDate, PersDate, moonTime
      character(44) :: HijriDate
      character(40) :: filename1
      character(30) :: LocatName
      character(13) :: HGregDate, HPersDate
      character(9) :: weekday
      character(4) :: YearChar
      character(1) :: Responce

      integer,dimension(3) :: today, now, Irtoday

      data weekDays/'Sunday   ','Monday   ','Tuesday  ', &
                    'Wednesday','Thursday ','Friday   ','Saturday '/

      WRITE(*,5)
5     FORMAT(1X///4X,' IRANIAN, HEJRI, AND GREGORY CALENDAR, SUN AND MOON',&
     & /1X,63('-')/&
     & ' THIS PROGRAM ALLOWS YOU TO PRODUCE IRANIAN OR GREGORY CALENDAR FOR'/&
     & ' GEOGRAPHICAL LOATIONS WITH LONGITUDE, LATITUDE, ALTITUDE AND TIMEZONE'/&
     & , 1X,63('-'))
      WRITE(*,10)
10  FORMAT('  In each of the following steps you will be asked to enter'/&
     & ' one or more values, defining the properties of your location and'/,&
     & ' desired calendar'/   &
     & ' In each step you can enter appropriate value at prompt.'/&
     & ' Location Name: name of any place in characters'/&
     & ' Longitude in decimal degrees, 0.00 to 180.00 for eastern longitudes.'/&
     & ' 0.0 to -180.00 degrees for western longitudes'/ &
     & ' Latitude in decimal degrees 0.00 to 90.00 for northern and 0.00 to -90.00'/&
     & ' southern hemispheres'/&
     & ' Altitude is elevation from sea level in meters from -29 to 8000.'/&
     & ' Time Zone in decimal hours,positive 0 to 12.0 for eastern hemisphere '/&
     & ' negative 0.0 to -12.0 for western hemisphere.'/ &
     & 1X,63('*')/)

     UT_TT = 1
     IREF = 1

 70   call getLocation(LocatName,Geo)
      print*,
      print*,

 75   WRITE(*,80)
 80   FORMAT(1X,60('-')/' enter, to continue with current value(s);',&
        'n or N to repeat, Ctrl c, to exit')
      READ(*,'(A1)',ERR=75,END=90) Responce
      IF(Responce == 'n' .or. Responce== 'N') then
            GOTO 70
      END IF

      Write(*, 15)
 15   FORMAT('  When light of astronomical object pass through atmosphere'/&
     & ' refraction causes, bending of light, therefore objects appear at '/&
     & ' different positions. Pressure, temperature and humidity of atmosphere'/&
     & ' affect refraction. You can select atmospheric models and type of air.'// &
     & '  Select Atmospheric Model, by entering 1 or 2, default model is MSISE.'// &
     & '    1 - Standard Model'/&
     & '    2 - MSISE Model'/)

      write(*,'(A,$)') 'Select Air Model '
      read(*,'(I1)') airModel
      print*,
      write(*,20)
 20   format(  '  Select Air Type, by entering 1 or 2, default is Humid Air'/ &
     & '    1 - Dry Air'/&
     & '    2 - Humid Air'/)

      write(*,'(A,$)') 'Select Air Type '
      read(*,'(I1)') airType
      print*,
      if(airType == 1) then
            B_Ilum = 0.0035D0
      else
            B_Ilum = 0.005D0
      end if

 21   Write(*, 22)
 22   FORMAT('  Start of lunar based Hijri months depend on visibility new '/&
     & ' crescent moon. This software uses two criteria for visibility of new  '/&
     & ' moon.'//&
     & '   1 - Yallop Criteria'/ &
     & '   2 - Odeh Criteria' / &
     & '   3 - Both Methods'//)

      Write(*,'(A,$)') ' Select Method, 1, 2 or 3  '
     read(*,'(I1)')Method
     if(Method /= 1) Method = 3

     Write(*, 23)
 23  format('  Also religious scholars divide on using aid fro sighting new moon crescent'/&
     & ' You can select: '// &
     & '    1 - Using aid such as binocular or telescope is accepted'/&
     & '    2 - Only naked eye for sighting of new moon crescent'/&
     & ' Default of both options is 2'//)

     write(*,'(A,$)') ' Using aid is accepted 1 , or not accepted 2 '
     read(*,'(I1)') aidAccept
     if(aidAccept /= 1) aidAccept = 0

      call idate(today)   ! today(1)=day, (2)=month, (3)=year
      Jdate(1) = today(3)
      Jdate(2) = today(2)
      Jdate(3) = today(1)
      call itime(now)     ! now(1)=hour, (2)=minute, (3)=second

      call GregCal2IrCal(today(3),today(2),today(1),Irtoday(1),Irtoday(2),Irtoday(3))

      Gdayofyear = dayOfGyear(today(3),today(2),today(1))
      PdayofYear = dayOfPyear(Irtoday(2),Irtoday(3))

     if(airModel == 1) then
            call stdatm(Geo(3),atmos(2),atmos(1),Density,a,mu,ts,rr,pp,rm,qm,0,kk)
            atmos(1) = atmos(1)/1.D3
            atmos(2) = atmos(2)- 273.15D0
      elseif(airModel /= 1) then
            alt = real(Geo(3),kind=4)
            lat = real(Geo(2),kind=4)
            long = real(Geo(1),kind=4)
            Stl = real(now(1),kind=4)
            call GTD7Tropo(GdayofYear,alt,lat,long,stl,Den,Tem,Pre)
            Density = real(Den,kind=8)
            atmos(2) = real(Tem,kind=8)
            atmos(1) = real(Pre,kind=8)
     end if
      Hour = real(now(1),kind=8) + real(now(2),kind=8)/60.D0 &
            + real(now(3),kind=8)/3600.D0

     call JULDAT(today(3),today(2),today(1),Hour,JDtoday)
     UJDtoday = JDtoday - Geo(4)/24.D0
     call JULDAT(today(3),today(2),today(1),0.D0,SMUJD)

     weekday = JD2WeekDay(SMUJD,1)

     CALL Moon_Phases(today(3),today(2),today(1),0,UnewMoonJD)
     newMoonJD = UnewMoonJD + Geo(4)/24.D0

     if(newMoonJD > SMUJD) then
            CALL Moon_Phases(today(3),today(2)-1,today(1),0,UnewMoonJD)
            newMoonJD = UnewMoonJD + Geo(4)/24.D0
     end if

     call HijriAdjust(newMoonJD,Geo,UT_TT,Method,B_Ilum,aidAccept,&
            FdayHijriJD,adjust)
     call JD2Hijri(FdayHijriJD,Hy,Hm,Hd)

     if(DINT(SMUJD - FdayHijriJD) < 0)then
            CALL Moon_Phases(today(3),today(2)-2,today(1),0,UnewMoonJD)
            newMoonJD = UnewMoonJD + Geo(4)/24.D0
            call HijriAdjust(newMoonJD,Geo,UT_TT,Method,B_Ilum,aidAccept&
                  ,FdayHijriJD,adjust)
            call JD2Hijri(FdayHijriJD,Hy,Hm,Hd)
     end if
      Hd = INT(SMUJD - FdayHijriJD)+1
     print*,

     write(*,25)trim(LocatName),Geo(1),Geo(2),Geo(3),Geo(4),&
            weekday,Irtoday(3),PersianMonthName(Irtoday(2)),&
            Irtoday(1),now(1),now(2),now(3),&
            today(1),GregMonthName(today(2)),today(3),&
            Hd,HijriMonthName(Hm),Hy,PdayofYear, GdayofYear,&
            atmos(1), atmos(2), Density
 25   format(' Location: ',A,3X,'Longitude: ',F6.2,3X,'Latitude: ',F5.2,3X,&
            'Altitude: ',F7.2,3X,'Time Zone: ',F5.2 ,//&
            ' Today is: ',A,3X,I2,3X,A,3X,I4,3X,'Hour: ',I2,3X,'Min: ',I2,3X, &
            ' Sec: ',I2/23X,I2,3X,A,5X,I4/ 23X,I2,3X,A,5X,I4/  &
            ' Iranian Day of Year: ',I4,3X,'Gregory Day of Year:',I4//&
            ' Atmospheric Properties, Pressure: ', F7.2,3X,'Temperature: '&
            , F5.2,3X,' Density: ',F5.3//)

      call Solar_Position(UJDtoday,Geo,atmos,UT_TT,sunAlt,sunZen,sunAz,topAlfa,topoDelta,IREF)
      call lunar_position(UJDtoday,UT_TT,TJDT,Geo,Atmos,moonAlt,moonAz,moondel,IREF)

      write(*,30)sunAlt,sunAz,moonAlt, moonAz
30    format(' Sun Altitude Angle(Degrees): ',F6.2,3X,'Sun Azimuth Angle(Degrees): ',F7.2/&
             ' Moon Altitude Angle(Degrees): ',F6.2,3X,'Moon Azimuth Angle(Degrees): ',F7.2/)
      print*,
      print*,

      call AstroSolarTimes(SMUJD,Jdate,Geo,atmos,UT_TT,IREF,sunTimes,RSTJD,RTSangles)
      do I=1,9
            call Hour2HMS(sunTimes(I),H,M,S)
                  STimes(I) = adjustl(trim(I2str(H))//':'//trim(I2str(M))//&
                  ':'//trim(I2str(S)))
      end do

      write(*,*)' SunRise: ',STimes(4),'   Noon: ',STimes(5), '   SunSet: ', STimes(6)
      write(*,'(A,F7.3,3X,A,F6.3,3X,A,F7.3)')' SunRise Azimuth: ',RTSangles(1)&
            , 'Noon Altitude: ',RTSangles(2),'SunSet Azimuth: ', RTSangles(3)
      write(*,*)' Astronomical Twilight: ',STimes(1),'  Nautical Twilight: ',STimes(2),&
                  '  Civic Twilight: ', STimes(3)
      write(*,*)' Astronomical Twilight: ',STimes(9),'  Nautical Twilight: ',STimes(8),&
                  '  Civic Twilight: ', STimes(7)

      call Moon_Day_Rise_Set(Jdate,SMUJD,Geo,atmos,UT_TT,MoonAngle,moonJDRS,mRShours,moonAzim,IREF)

      print*

      do I=1,2
            call Hour2HMS(mRShours(I),H,M,S)
            MTimes(I) = adjustl(trim(I2str(H))//':'//trim(I2str(M))//&
            ':'//trim(I2str(S))//'-'//trim(D2str(moonAzim(I))))
      end do

      write(*,*)' MoonRise: ',MTimes(1),'   MoonSet: ', MTimes(2)

      print*,
      print*,

      Iyear = today(3)
      CalenType = 2
      YearChar = Trim(I2str(Iyear))
      DST = 0.D0

31    write(*,35) Iyear
35    format('Select Options'// '  1- Select Calendar type, 1- Iranian , 2- Gregory'/&
            '  2- Type a Year, current year is ',I4/&
            '  3- Select for Daylight saving Time in calendars? '/&
            '  4- Solar Equinoxes and Solstices for a Year'/&
            '  5- Moon Phases for a year'/&
            '  6- Hijri Months of Year'/ &
            '  7- Sun and Moon Calendar for a year'/&
            '  8- Moon Calendar for a year'/&
            '  9- Islam Prayer times calendar for a year '/&
            '  10- Start Over '/&
            '  11- Exit' )
      print*,
      write(*,'(A,$)') ' Your Option? '
      read(*,'(I3)') options

      if(options== 11) then
            return
      elseif(options==10) then
            write(*,49)
 49   format(' Start From: '/&
             ' 1- New Location'/&
             ' 2- Different Method')
            read(*,'(I1)') StatOver
            if(StatOver == 1) then
                  goto 70
            elseif(StatOver == 2) then
                  goto 21
            end if
      elseif(options==1) then
            write(*,'(A,$)') '  Select Calendar Type: '
            read(*,'(I1)') CalenType
      endif
      if(options==2) then
            write(*,'(A,$)') '  Enter year: '
            read(*,'(I4)') Iyear
            YearChar = Trim(I2str(Iyear))
      endif

      if(CalenType == 1) then
                  Year = Iyear +621
                  Imonth = 3
                  Iday = 1
            else
                  year = Iyear
                  Imonth =  1
                  Iday = 1
      end if

      if(options==3) then
            write(*,55)
 55   format(' Select of of options: '/&
             ' 1- Europe'/&
             ' 2- Iran'/&
             ' 3- North America')
            read(*,'(I1)') DSTop
      if(DSTop == 1) then
           call JULDAT(Year,3,1,0.D0,DS)
           do while(JD2WeekDayNum(DS)/= 2)
            DS = DS + 1
           end do
           DST(1) = DS + 21
           call JULDAT(Year,10,1,0.D0,DS)
           do while(JD2WeekDayNum(DS)/= 2)
            DS = DS + 1
           end do
           DST(2) = DS + 21
      elseif(DSTop == 2) then
            if(CalenType == 2) then
                  Year = Iyear-621
            else
                  Year = Iyear
            end if
            DST(1) = IrCal2JD(Year,1,2,0.D0)
            DST(2) = IrCal2JD(Year,6,31,0.D0)
      elseif(DSTop == 3) then
            call JULDAT(Year,3,1,0.D0,DS)
           do while(JD2WeekDayNum(DS)/= 2)
            DS = DS + 1
           end do
           DST(1) = DS + 7
           call JULDAT(Year,11,1,0.D0,DS)
           do while(JD2WeekDayNum(DS)/= 2)
            DS = DS + 1
           end do
           DST(2) = DS
      end if
      print*, 'Start of Daylight Saving Julian Day= ' , DST(1)
      print*, 'End of Daylight Saving Julian Day= ' ,  DST(2)
      endif

      if(options==4) then
            data EquiSolislael /'Spring Equinox','Summer Solstice',&
                              'Fall Equinox','Winter Solstice'/

            do k = 0,3
                  TJD(k) = TrueJDEquiSolitice(Year,k)
                  call RCALDAT(TJD(k),Gy,Gm,Gd,Hour)
                  call iau_DAT(Gy,Gm,Gd,Hour/24D0,DELTA_T,J)
                  Hour = Hour - (DELTA_T+32.184D0)/3600.D0
                  call Hour2HMS(Hour,H,M,S)
                  LJD = TJD(k) + (Geo(4)-(DELTA_T+32.184D0)/3600.D0)/24.D0
                  call JD2IrCal(LJD,Iry,Irm,Ird,Hour)
                  call Hour2HMS(Hour,UH,UM,US)
                  write(*,40),EquiSolislael(k),TJD(k),Gy,Gm,Gd,H,M,S,Iry,&
                        Irm,Ird,UH,UM,US
            end do
 40          format(A/' Julidan Day:',F15.4/ ' Gregory Date: ',I4,'/',I2,'/',I2&
                    ' , Universal Time: ', I2,':',I2,':',I2/' Iranian Date: ' &
                    ,I4,'/',I2,'/',I2,' , Local Time: ',I2,':',I2,':',I2//)

      elseif(options==5)then

            call YearMoonPhases(year,Imonth,Iday,MoonPhaseJD)

            filename1 = Trim(LocatName)//'MoonPhase'//YearChar//'.txt'
            write(*,'(3A,$)') 'If you wish to get moon phase of the year,in '&
            //trim(filename1)//' enter Y  '
            read(*,'(A)') Responce

            if(Responce=='Y' .or. Responce=='y') then

            open(unit = 20 ,file = trim(filename1), form = 'formatted',status = 'unknown')
                  write(20,'(20X,4A)') ' Moon Phases, year ',YearChar,', ', LocatName
                  write(*,'(X,A)') '------------------------------------------------------------'
                  write(20,'(20X,4(A,9X))')'New Moon','First Quarter','Full Moon','Last Quarter'
            endif

            print*,
            write(*,'(20X,4A)') ' Moon Phases, year ',YearChar,', ', LocatName
            write(*,'(X,A)') '------------------------------------------------------------'
            write(*,'(20X,4(A,9X))')'New Moon','First Quarter','Full Moon','Last Quarter'

      do I = 0 , 14

            do k = 0,3
                  JDM = (MoonPhaseJD(I,k)) + Geo(4)/24.D0
                  call RCALDAT (JDM,Gy,Gm,Gd,Hour)
                  GregDate(k) = adjustl(trim(I2str(Gy))//'/'//trim(I2str(Gm))//&
                  '/'//trim(I2str(Gd)))
                  call JD2IrCal(JDM,Iry,Irm,Ird,Hour)
                  PersDate(k) = adjustl(trim(I2str(Iry))//'/'//trim(I2str(Irm))&
                  //'/'//trim(I2str(Ird)))
                  call Hour2HMS(Hour,H,M,S)
                  moonTime(k) = adjustl(trim(I2str(H))//':'//trim(I2str(M))&
                  //':'//trim(I2str(S)))

            end do

            write(*,'(1X,A,4(F14.4,6X))')'Julian day(UTC)', MoonPhaseJD(I,0:3)
            write(*,'(1X,A,$)')'Gregory Date'
            write(*,'(5X,4(A,7X))')GregDate(0:3)
            write(*,'(1X,A,$)')'Iranian Date'
            write(*,'(5X,4(A,7X))')PersDate(0:3)
            write(*,'(1X,A,$)')'Local Time'
            write(*,'(7X,4(A,7X))')moonTime(0:3)
            write(*,*)

            if(Responce=='Y' .or. Responce=='y') then
                  write(20,*)
                  write(20,'(1X,A,4(F14.4,6X))')'Julian day(UTC)', MoonPhaseJD(I,0:3)
                  write(20,'(1X,A,$)')'Gregory Date'
                  write(20,'(5X,4(A,7X))')GregDate(0:3)
                  write(20,'(1X,A,$)')'Iranian Date'
                  write(20,'(5X,4(A,7X))')PersDate(0:3)
                  write(20,'(1X,A,$)')'Local Time'
                  write(20,'(7X,4(A,7X))')moonTime(0:3)
                  write(20,*)
            end if
      end do
            print*,
            if(Responce=='Y' .or. Responce=='y') then
                  Print*, 'Finished writing '//filename1
                  close(20)
            end if

      elseif(options==6) then

            call HijriMonths(year,Imonth,Iday,Geo,UT_TT,Method,B_Ilum, &
            AidAccept,HijriJD)
            print*,
            filename1 = Trim(LocatName)//'_HijriMonths'//YearChar//'.txt'
            write(*,'(3A,$)') 'If you wish to get Hijri Months of the year,in '&
            //trim(filename1)//' enter Y  '
            read(*,'(A)') Responce

            if(Responce=='Y' .or. Responce=='y') then

            open(unit = 20 ,file = trim(filename1), form = 'formatted',status = 'unknown')
                  write(20,'(20X,4A)') ' Hijri Months, year ',YearChar,', ', LocatName
                  write(20,'(X,A)') '------------------------------------------------------------'
                  write(20,*)
            endif

            print*,
            write(*,'(15X,4A)') ' Hijri Months, year ',YearChar,', ', LocatName
            write(*,'(X,A)') '------------------------------------------------------------'

      do I = 0 , 14

            JDM = (HijriJD(I))
            call JD2Hijri(JDM+14.D0,Hy,Hm,Hd)
            Hijridate = adjustl('First(1) of '//trim(HijriMonthName(Hm))//&
            ', year '//trim(I2str(Hy)))
            call RCALDAT (JDM,Gy,Gm,Gd,Hour)
            HGregDate = adjustl(trim(I2str(Gy))//'/'//trim(I2str(Gm))//&
            '/'//trim(I2str(Gd)))
            call JD2IrCal(JDM,Iry,Irm,Ird,Hour)
            HPersDate = adjustl(trim(I2str(Iry))//'/'//trim(I2str(Irm))&
            //'/'//trim(I2str(Ird)))

            write(*,*)
            write(*,'(1X,A)') HijriDate
            write(*,'(1X,A,2X,F10.2)')'Julian day(UTC)', HijriJD(I)
            write(*,'(1X,A,$)')'Gregory Date'
            write(*,'(5X,(A,2X))')HGregDate
            write(*,'(1X,A,$)')'Iranian Date'
            write(*,'(5X,(A,2X))')HPersDate
            write(*,*)

            if(Responce=='Y' .or. Responce=='y') then
                  write(20,'(1X,A)') HijriDate
                  write(20,'(1X,A,2X,F10.2)')'Julian day(UTC)', HijriJD(I)
                  write(20,'(1X,A,$)')'Gregory Date'
                  write(20,'(5X,(A,2X))')HGregDate
                  write(20,'(1X,A,$)')'Iranian Date'
                  write(20,'(5X,(A,2X))')HPersDate
                  write(20,*)
            end if
      end do
            print*,
            if(Responce=='Y' .or. Responce=='y') then
                  Print*, 'Finished writing '//filename1
                  close(20)
            end if
            print*,

      elseif(options==7) then

            filename1 = Trim(LocatName)//'_MoonSunCalendar'//YearChar//'.txt'

            write(*,42)filename1
42          format(' Because large size of data, calendar will save in file ',A/)
            write(*,'(A,$)') ' To proceed type Y or y '
            read(*,'(A)') Responce

            if(Responce == 'Y' .or. Responce == 'y') then
                  call SunMoonCalendar(LocatName,Iyear,CalenType,Geo,airModel,&
                  Method,B_Ilum,aidAccept,DST)
            end if

      elseif(options==8) then

            filename1 = Trim(LocatName)//'_MoonCalendar'//YearChar//'.txt'

            write(*,44)filename1
44          format(' Because large size of data, calendar will save in file ',A/)
            write(*,'(A,$)') ' To proceed type Y or y '
            read(*,'(A)') Responce

            if(Responce == 'Y' .or. Responce == 'y') then
                  call MoonCalendar(LocatName,Iyear,CalenType,Geo,airModel,&
                  Method,B_Ilum,aidAccept,DST)
            end if

      elseif(options==9)then

            filename1 = Trim(LocatName)//'_IslamCalendar'//YearChar//'.txt'

            write(*,45)filename1
45          format(' Because large size of data, calendar will save in file ',A/)
            write(*,'(A,$)') ' To proceed type Y or y '
            read(*,'(A)') Responce

            if(Responce == 'Y' .or. Responce == 'y') then

                  write(*,50)
50                format('  Select Prayer Time Conventions'/&
             52X,'Fajr',5X,'Asr',3X,'Maghreb',2X,'Isha '/ &
      '  1- Institute of Geophysics, University of Tehran -17.7  ,  1   ,  -4.5  ,  -14'/&
      '  2- http://www.azangoo.com ,Fajr based on latitude Var   ,  1   ,  -4    ,  -14'/&
      '  3- Leva Research Institute,Qum                   -16    ,  1   ,  -4.5  ,  -14'/&
      '  4- University of Islamic Sciences, Karachi       -18    ,  2   ,  -0.26 ,  -18'/&
      '  5- Islamic Society of North America (ISNA)       -15    ,  1   ,  -0.26 ,  -15'//)

            write(*,'(A,$)') 'Default is Azangoo, Your Response: '
            read(*,'(I2)') Azans

            if(Azans == 1) then
                 DATA Angles/ -17.7D0,-4.5D0,-14.D0,1.D0/
            elseif(Azans == 3) then
                  DATA Angles/ -16.D0,-4.5D0,-14.D0,1.D0/
            elseif(Azans==4) then
                  DATA Angles/ -18.D0,-0.2666D0,-18.D0,2.D0/
            elseif(Azans==5) then
                  DATA Angles/ -15.D0,-0.2666D0,-15.D0,1.D0/
            else
                  Angles(1) = AzanAngle(Geo(2))
                  Angles(2) = -4.D0
                  Angles(3) = -14.D0
                  Angles(4) = 1.D0
            end if
                 call IslamCalendar(LocatName,IYear,CalenType,Geo,airModel,Angles,&
            Method,B_Ilum,aidAccept,DST)
            end if

      end if

      goto 31
 90   stop
      return
end

subroutine IslamCalendar(Location,Iyear,CalenType,Geo,airModel,Angles,&
      Method,B_Ilum,aidAccept,DST)

      USE MoonSun
      USE MSISE
      USE NOVAS
      USE SOFA

      implicit none

       integer :: Iyear, CalenType,UT_TT,Method, aidAccept
       integer :: leap, weekDayNum, Oyear, Omonth, Oday
       integer :: OdaysinMonth, Days, k,I,J,Hy,Hm,Hd,n
       integer :: IREFR, H , S, M, airModel, kk
       integer :: dayofYear
       integer, dimension(3) :: Equinox, Jdate
       integer, dimension(0:4) :: MarDay,Gyear
       integer, dimension(12):: DaysInMonth, GdaysInMonth
       real(kind=8) :: Density,a,mu,ts,rr,pp,rm,qm,DELTA_T
       real(kind=8) :: B_Ilum, DJD, hour, newYearJD
       real(kind=4) :: alt,lat,long,stl,Den,Tem,Pre
       real(kind=8) ,dimension(4) :: Geo, Angles
       real(kind=8) ,dimension(2) :: atmos,DST
       real(kind=8),dimension(3) ::  RTSangles
       real(kind=8),dimension(0:4) :: UJD, Uhour
       character(9),dimension(7) :: weekDays
       real(kind=8) , dimension(0:14) :: newMoonJD
       real(kind=8),dimension(8) :: RTimes, NTJD
       character(3) :: OmonthAb
       character(4) :: YearChar
       character(5) :: HmonthAb
       character(7) :: Ocal
       character(12) :: monthName
       character(30) :: Location
       character(40) :: filename1
       character(55) :: aidAcceptText
       character(3),dimension(12) :: AbGmonthName, AbImonthName
       character(5),dimension(12) :: AbHmonthName
       character(8) ,dimension(8) :: STimes

      data weekDays/'Saturday ','Sunday   ','Monday   ', &
                    'Tuesday  ','Wednesday','Thursday ','Friday   '/

      data AbGmonthName / 'Jan','Feb','Mar','Apr','May','Jun','Jul',&
                          'Aug','Sep','Oct','Nov','Dec'/

      data AbImonthName / 'Far','Ord','Kho','Tir','Mor','Sha','Meh',&
                          'Abn','Azr','Dey','Bmn','Esf'/

      data AbHmonthName / 'Muhrm', 'Safar', 'Rabav','Rabth', 'Jumul', 'Jumth', &
                          'Rajab', 'Shabn', 'Ramzn','Shavl', 'DhulQ', 'Dhulh'/

      DATA DaysInMonth /31,31,31,31,31,31,30,30,30,30,30,29/
      DATA GdaysInMonth /31,28,31,30,31,30,31,31,30,31,30,31/

      IREFR = 1
      UT_TT = 1

      if(CalenType == 1)then
            call IranCalendar(Iyear,Gyear,UJD,leap,Equinox,MarDay,Uhour)
            if(Iyear == leap) DaysInMonth(12) = 30
            DJD = dint(UJD(2))+0.5D0
            Oyear = Gyear(2)
            Omonth = 3
            Oday = MarDay(2)
            if(Equinox(2) /= 1) Oday = Oday + 1
            call HijriMonths(Oyear,Omonth,20,Geo,UT_TT,Method,B_Ilum, &
            AidAccept,NewMoonJD)
            newYearJD = TrueJDEquiSolitice(Gyear(2),0)
            call iau_DAT(Gyear(2),3,MarDay(2),0.D0,DELTA_T,J)
      else
            call JULDAT(IYear,1,1,0.D0,DJD)
            if(IsLeapYear(Iyear)) GdaysInMonth(2) = 29
            call JD2IrCal(DJD,Oyear,Omonth,Oday,hour)
            call IranCalendar(Oyear,Gyear,UJD,leap,Equinox,MarDay,Uhour)
            if(Oyear == leap) DaysInMonth(12) = 30
            call HijriMonths(Iyear,1,1,Geo,UT_TT,Method,B_Ilum, &
            AidAccept,NewMoonJD)
            newYearJD = TrueJDEquiSolitice(Iyear,0)
            call iau_DAT(Iyear,1,1,0.D0,DELTA_T,J)
      end if

      Uhour(2) = dmod(newYearJD+0.5D0,1.D0)*24.D0+Geo(4)-(DELTA_T+32.184D0)/3600.D0

      weekDaynum = JD2WeekDayNum(DJD)

      do k =0,3
            if(DJD < newMoonJD(k)) then
                  call JD2Hijri(newMoonJD(k-1)+14.D0,Hy,Hm,Hd)
                  Hd = int(DJD-newMoonJD(k-1)) + 1
                  exit
            end if
      end do

      YearChar = Trim(I2str(Iyear))
      filename1 = Trim(Location)//'_Islam Calendar'//YearChar//'.txt'
      open(unit = 10 ,file = trim(filename1), form = 'formatted',status = 'unknown')

       if(aidAccept == 1) then
            aidAcceptText = 'Aid for new Moon sighting is accepted'
        else
            aidAcceptText = 'Only naked eye for sighting new moon crescent'
        end if

      call Hour2HMS(Uhour(2),H,M,S)
      WRITE(*,5)trim(Location),Geo(1),Geo(2),Geo(3),Geo(4),H,M,S,Equinox(3),&
      trim(PersianMonthName(Equinox(2))),Equinox(1),MarDay(2),Gyear(2),&
       Angles(1),Angles(4),Angles(2),Angles(3)
      WRITE(10,5)trim(Location),Geo(1),Geo(2),Geo(3),Geo(4),H,M,S,Equinox(3),&
      trim(PersianMonthName(Equinox(2))),Equinox(1),MarDay(2),Gyear(2),&
       Angles(1),Angles(4),Angles(2),Angles(3)
 5    FORMAT(2X,'                          IRANIAN, HEJRI, AND GREGORY CALENDAR ',&
       'with ISLAM PRAYER TIMES'/1X,110('-')/&
       ' Location: ',A,3X,'Longitude: ',F6.2,3X,'Latitude: ',F5.2,3X,&
            'Altitude: ',F7.2,3X,'Time Zone: ',F5.2 ,//&
       ' Iranian New Year Time, Hour: ',I2,2X,'Min :',I2,2X,'Sec: ',I2,' , Date: ',&
        I2,2X,A,2X,I4,2X,', ',I2,2X,'March ',2X,I4//1X,'Fajr Angle: ',F6.2,4X,&
        ' Asr Shadow length: ',F3.1,3X,'Maghreb Angle: ',F4.1,3X,'Isha Angle: ',F5.1,&
        2X,' Midnight: Astronomical'/)

      if(airModel == 1) then
            call stdatm(Geo(3),atmos(2),atmos(1),Density,a,mu,ts,rr,pp,rm,qm,0,kk)
            atmos(2) = atmos(2) -273.15D0
            atmos(1) = Atmos(1)/100.D0
            write(*,6)atmos(1),atmos(2),Density
            write(10,6)atmos(1),atmos(2),Density
 6    format(1X,'Standard Atmospheric Properties,  Pressure: ',F7.2,2X,&
      ' Temperature: ',F6.2,2X,' Density: ',F5.2 /)
      end if

      write(*,7)aidAcceptText,Iyear
      write(10,7)aidAcceptText,Iyear
 7    format(1X,A,/110('-')/55X,I4)

      do I = 1, 12

            WRITE(*,10)
            WRITE(10,10)
 10         FORMAT(1X,110('-')/)

            if(CalenType == 1) then
                  Ocal = 'Gregory'
                  monthName = PersianMonthName(I)
                  Days = DaysInMonth(I)
                  OdaysinMonth = GdaysInMonth(Omonth)
                  OmonthAb = AbGmonthName(Omonth)
                  dayofYear = dayOfGyear(Oyear,Omonth,Oday)
            else
                  Ocal = 'Iranian'
                  monthName = GregMonthName(I)
                  Days = GdaysInMonth(I)
                  OdaysinMonth = DaysInMonth(Omonth)
                  OmonthAb = AbImonthName(Omonth)
                  dayofYear = dayOfGyear(Iyear,I,1)
            end if

            HmonthAb = AbHmonthName(Hm)

            if(airModel /= 1) then
                  alt = real(Geo(3),kind=4)
                  lat = real(Geo(2),kind=4)
                  long = real(Geo(1),kind=4)
                  Stl = real(5.0,kind=4)
                  call GTD7Tropo(dayofYear,alt,lat,long,stl,Den,Tem,Pre)
                  Density = real(Den,kind=8)
                  atmos(2) = real(Tem,kind=8)
                  atmos(1) = real(Pre,kind=8)
           end if

            WRITE(*,12) monthName
            WRITE(10,12) monthName
 12         FORMAT(1X,110('-')/,48X,A12)

            if(airModel /=1) then
                  WRITE(*,14)atmos(1), atmos(2), Density
                  WRITE(10,14) atmos(1), atmos(2), Density
 14         FORMAT(' Atmospheric Properties, Pressure: ',&
                  F8.3,', Temperature: ',F8.3,', Density: ', F8.3/)
            end if

            WRITE(*,15)Iyear,Oyear,Hy,monthName,Ocal
            WRITE(10,15)Iyear,Oyear,Hy,monthName,Ocal
 15         FORMAT(5X,I4,6X,I4,5X,I4,5X,'Morning',2X,' Sun ',5X,' Noon ',4X,' Asr ',6X,' Sun ',5X,&
                  'Maghreb',3X,' Isha ',4x,' Mid '/2X,A,1X,A,2X,'Hijri    Prayer',2X,'  Rise ',4X,&
                  ' Time ',4X,' Prayer ',3X,' Set ',5X,'Prayer',3X,' Prayer ',3X,' Night ')

            do J=1,Days

                  call IslamSolarTimes(DJD,Jdate,Geo,atmos,Angles,UT_TT,IREFR,RTimes,NTJD,RTSangles)
                  if(DJD>= DST(1).and. DJD <= DST(2)) RTimes = RTimes + 1.D0
                  do n=1,9
                        call Hour2HMS(RTimes(n),H,M,S)
                        STimes(n) = adjustl(trim(I2str(H))//':'//trim(I2str(M))//&
                        ':'//trim(I2str(S)))
                  end do

                  write(*,20)WeekDays(weekDaynum),J,OmonthAb,Oday,HmonthAb,Hd,Stimes
                  write(10,20)WeekDays(weekDaynum),J,OmonthAb,Oday,HmonthAb,Hd,Stimes
 20               format(A,2X,I2,2X,A,','I2,2X,A,',',I2,8(2X,A))

                  DJD = DJD + 1.D0

                  weekDayNum = weekDayNum + 1
                  if(weekDayNum > 7) weekDayNum = 1
                  Oday = Oday + 1
                  if(Oday > OdaysInMonth) then
                        Oday = 1
                        Omonth = Omonth + 1
                        if(Omonth >12) then
                              Oyear = Oyear + 1
                              Omonth = 1
                        end if
                        if(CalenType == 1) then
                              OmonthAb = AbGmonthName(Omonth)
                              if(IsLeapYear(Oyear)) then
                                    GdaysInMonth(2) = 29
                              else
                                    GdaysInMonth(2) = 28
                              end if
                        else
                              OmonthAb = AbImonthName(Omonth)
                        end if
                  end if
                  Hd = Hd +1
                  if(DJD == newMoonJD(k)) then
                        k = k + 1
                        Hd = 1
                        Hm = Hm + 1
                        if(Hm > 12) then
                              Hy = Hy + 1
                              Hm = 1
                        end if
                        HmonthAb = AbHmonthName(Hm)
                  end if
            end do
      end do
      close(10)
end subroutine

subroutine SunMoonCalendar(Location,Iyear,CalenType,Geo,airModel,&
      Method,B_Ilum,aidAccept,DST)

      USE MoonSun
      USE MSISE
      USE NOVAS
      USE SOFA

      implicit none

       integer :: Iyear, CalenType,UT_TT,Method, aidAccept
       integer :: leap, weekDayNum, Oyear, Omonth, Oday
       integer :: OdaysinMonth, Days, k,I,J,Hy,Hm,Hd,n
       integer :: IREFR, H , S, M, airModel, kk, L, q
       integer :: dayofYear,Adjust
       integer, dimension(3) :: Equinox, Jdate
       integer, dimension(0:4) :: MarDay,Gyear
       integer, dimension(12):: DaysInMonth, GdaysInMonth
       real(kind=8) :: Density,a,mu,ts,rr,pp,rm,qm,DELTA_T
       real(kind=8) :: B_Ilum, DJD, hour, newYearJD
       real(kind=4) :: alt,lat,long,stl,Den,Tem,Pre
       real(kind=8) :: MoonAngle
       real(kind=8) ,dimension(4) :: Geo
       real(kind=8) ,dimension(2) :: atmos, DST
       real(kind=8) ,dimension(2):: RSTJDout ,RS_Hours, RS_Azim
       real(kind=8) ,dimension(3):: RTSangles
       real(kind=8) ,dimension(0:4) :: UJD, Uhour
       character(9) ,dimension(7) :: weekDays
       real(kind=8) ,dimension(0:14,0:3):: MoonPhaseJD
       real(kind=8) ,dimension(0:14) :: newMoonJD
       real(kind=8) ,dimension(9) :: RTimes,RSTJD
       character(3) :: OmonthAb
       character(4) :: YearChar
       character(5) :: HmonthAb
       character(7) :: Ocal
       character(12) :: monthName,MoonStat
       character(30) :: Location
       character(40) :: filename1
       character(55) :: aidAcceptText
       character(3),dimension(12) :: AbGmonthName, AbImonthName
       character(5),dimension(12) :: AbHmonthName
       character(18) ,dimension(9) :: STimes
       character(18) ,dimension(2) :: MTimes


      data weekDays / 'Sunday   ','Monday   ','Tuesday  ', &
                      'Wednesday','Thursday ','Friday   ',&
                      'Saturday '/

      data AbGmonthName / 'Jan','Feb','Mar','Apr','May','Jun','Jul',&
                          'Aug','Sep','Oct','Nov','Dec'/

      data AbImonthName / 'Far','Ord','Kho','Tir','Mor','Sha','Meh',&
                          'Abn','Azr','Dey','Bmn','Esf'/

      data AbHmonthName / 'Muhrm', 'Safar', 'Rabav','Rabth', 'Jumul', 'Jumth', &
                          'Rajab', 'Shabn', 'Ramzn','Shavl', 'DhulQ', 'Dhulh'/

      DATA DaysInMonth /31,31,31,31,31,31,30,30,30,30,30,29/
      DATA GdaysInMonth /31,28,31,30,31,30,31,31,30,31,30,31/

      IREFR = 1
      UT_TT = 1

      if(CalenType == 1)then
            call IranCalendar(Iyear,Gyear,UJD,leap,Equinox,MarDay,Uhour)
            if(Iyear == leap) DaysInMonth(12) = 30
            DJD = dint(UJD(2))+0.5D0
            Oyear = Gyear(2)
            Omonth = 3
            Oday = MarDay(2)
            if(Equinox(2) /= 1) Oday = Oday + 1
            call YearMoonPhases(Oyear,Omonth,20,moonPhaseJD)
            newYearJD = TrueJDEquiSolitice(Gyear(2),0)
            call iau_DAT(Oyear,3,MarDay(2),0.D0,DELTA_T,J)
      else
            call JULDAT(IYear,1,1,0.D0,DJD)
            if(IsLeapYear(Iyear)) GdaysInMonth(2) = 29
            call JD2IrCal(DJD,Oyear,Omonth,Oday,hour)
            call IranCalendar(Oyear,Gyear,UJD,leap,Equinox,MarDay,Uhour)
            if(Oyear == leap) DaysInMonth(12) = 30
            call YearMoonPhases(Iyear,1,1,moonPhaseJD)
            newYearJD = TrueJDEquiSolitice(Iyear,0)
            call iau_DAT(Iyear,1,1,0.D0,DELTA_T,J)
      end if

      Uhour(2) = dmod(newYearJD+0.5D0,1.D0)*24.D0+Geo(4)-(DELTA_T+32.184D0)/3600.D0

      weekDaynum = JD2WeekDayNum(DJD)

       do I = 0 , 14
            call HijriAdjust(MoonPhaseJD(I,0),Geo,UT_TT,Method,B_Ilum, &
            AidAccept,NewMoonJD(I),Adjust)
      end do

      do k =0,3
            if(DJD < newMoonJD(k)) then
                  call JD2Hijri(newMoonJD(k-1)+14.D0,Hy,Hm,Hd)
                  Hd = int(DJD-newMoonJD(k-1)) + 1
                  exit
            end if
      end do

      YearChar = Trim(I2str(Iyear))
      filename1 = Trim(Location)//'_MoonSunCalendar'//YearChar//'.txt'
      open(unit = 10 ,file = trim(filename1), form = 'formatted',status = 'unknown')

       if(aidAccept == 1) then
            aidAcceptText = 'Aid for new Moon sighting is accepted'
        else
            aidAcceptText = 'Only naked eye for sighting new moon crescent'
        end if

      call Hour2HMS(Uhour(2),H,M,S)
      WRITE(*,5)trim(Location),Geo(1),Geo(2),Geo(3),Geo(4),H,M,S,Equinox(3),&
      trim(PersianMonthName(Equinox(2))),Equinox(1),MarDay(2),Gyear(2)
      WRITE(10,5)trim(Location),Geo(1),Geo(2),Geo(3),Geo(4),H,M,S,Equinox(3),&
      trim(PersianMonthName(Equinox(2))),Equinox(1),MarDay(2),Gyear(2)

 5    FORMAT(48X,' IRANIAN, HEJRI, AND GREGORY CALENDAR ',&
       'with SUN AND MOON TIMES'/1X,200('-')/&
       ' Location: ',A,3X,'Longitude: ',F6.2,3X,'Latitude: ',F5.2,3X,&
            'Altitude: ',F7.2,3X,'Time Zone: ',F5.2 ,//&
       ' Iranian New Year Time, Hour: ',I2,2X,'Min :',I2,2X,'Sec: ',I2,' , Date: ',&
        I2,2X,A,2X,I4,2X,', ',I2,2X,'March ',2X,I4/)

      if(airModel == 1) then
            call stdatm(Geo(3),atmos(2),atmos(1),Density,a,mu,ts,rr,pp,rm,qm,0,kk)
            atmos(2) = atmos(2) -273.15D0
            atmos(1) = Atmos(1)/100.D0
            write(*,6)atmos(1),atmos(2),Density
            write(10,6)atmos(1),atmos(2),Density
 6    format(1X,'Standard Atmospheric Properties,  Pressure: ',F7.2,2X,&
      ' Temperature: ',F6.2,2X,' Density: ',F5.2 /)
      end if

      write(*,7)aidAcceptText,Iyear
      write(10,7)aidAcceptText,Iyear
 7    format(1X,A,/200('-')/100X,I4)

      do I = 1, 12

            if(CalenType == 1) then
                  Ocal = 'Gregory'
                  monthName = PersianMonthName(I)
                  Days = DaysInMonth(I)
                  OdaysinMonth = GdaysInMonth(Omonth)
                  OmonthAb = AbGmonthName(Omonth)
                  dayofYear = dayOfGyear(Oyear,Omonth,Oday)
            else
                  Ocal = 'Iranian'
                  monthName = GregMonthName(I)
                  Days = GdaysInMonth(I)
                  OdaysinMonth = DaysInMonth(Omonth)
                  OmonthAb = AbImonthName(Omonth)
                  dayofYear = dayOfGyear(Iyear,I,1)
            end if

            HmonthAb = AbHmonthName(Hm)

            if(airModel /= 1) then
                  alt = real(Geo(3),kind=4)
                  lat = real(Geo(2),kind=4)
                  long = real(Geo(1),kind=4)
                  Stl = real(5.0,kind=4)
                  call GTD7Tropo(dayofYear,alt,lat,long,stl,Den,Tem,Pre)
                  Density = real(Den,kind=8)
                  atmos(2) = real(Tem,kind=8)
                  atmos(1) = real(Pre,kind=8)
           end if

            WRITE(*,12) monthName
            WRITE(10,12) monthName
 12         FORMAT(1X,200('-')/,96X,A12)

            if(airModel /=1) then
                  WRITE(*,14)atmos(1), atmos(2), Density
                  WRITE(10,14) atmos(1), atmos(2), Density
 14         FORMAT(1X,' Atmospheric Properties, Pressure: ',&
                  F8.3,', Temperature: ',F8.3,', Density: ', F8.3/)
            end if

            WRITE(*,15)Iyear,Oyear,Hy,monthName,Ocal
            WRITE(10,15)Iyear,Oyear,Hy,monthName,Ocal
 15         FORMAT(5X,I4,6X,I4,5X,I4,5X,'---------Twilight---------',8X,'Sun',18X,'Noon',16X,'Sun ',&
                  12X,'---------Twilight---------',10X,'Moon',16X,'Moon',12X,'Moon'/2X,A,1X,A,2X,'Hijri',&
                  2X,'Astronomical Nautical  Civic',5X,'Rise-Azimuth ',7X,'Time-Altitude',9X,&
                  'Set-Azimuth',6X,'Civic  Nautical Astronomical',5X,'Rise-Azimuth',9X,'Set_Azimuth'&
                   ,7X,'Phases')

             MoonStat = ''

            do J=1,Days

                  call AstroSolarTimes(DJD,Jdate,Geo,atmos,UT_TT,IREFR,RTimes,RSTJD,RTSangles)

                  call Moon_Day_Rise_Set(Jdate ,DJD, Geo, ATmos ,UT_TT , &
                        MoonAngle,RSTJDout ,RS_Hours, RS_Azim, IREFR)


                  if(DJD>= DST(1).and. DJD <= DST(2)) then
                        RTimes = RTimes + 1.D0
                        RS_Hours =RS_Hours +1.D0
                  end if

                  do n=1,9
                        call Hour2HMS(RTimes(n),H,M,S)
                        if( n>= 4 .and. n<=6) then
                                    STimes(n) = adjustl(trim(I2str(H))//':'//trim(I2str(M))//&
                        ':'//trim(I2str(S))//  '-'//trim(D2str(RTSAngles(n-3))))
                              else
                                    STimes(n) = adjustl(trim(I2str(H))//':'//trim(I2str(M))//&
                        ':'//trim(I2str(S)))
                             end if

                  end do

                  do L =1 , 2
                        call Hour2HMS(RS_Hours(L), H , M , S)
                        MTimes(L) = adjustl(trim(I2str(H))//':'//trim(I2str(M))//&
                        ':'//trim(I2str(S))//  '-'//trim(D2str(RS_Azim(L))))
                  end do


                  do L= I-1 , I+1
                        do q = 0,3
                              if(DJD == dint(MoonPhaseJD(L,q))+0.5D0) then
                                    if(q==0) then
                                          MoonStat = ' New Moon'
                                          exit
                                    elseif(q==1) then
                                          MoonStat = ' First Half'
                                          exit
                                    elseif(q==2)then
                                          MoonStat = ' Full Moon'
                                          exit
                                    else
                                          MoonStat = ' Last Half'
                                          exit
                                    end if
                              end if
                        end do
                  end do


                  write(*,20)WeekDays(weekDaynum),J,OmonthAb,Oday,HmonthAb,Hd,(Stimes)&
                  ,(MTimes),MoonStat
                  write(10,20)WeekDays(weekDaynum),J,OmonthAb,Oday,HmonthAb,Hd,(Stimes)&
                  ,(MTimes),MoonStat
 20               format(A,2X,I2,2X,A,','I2,2X,A,',',I2,3(2X,A8),3(2X,A),3(2X,A8),X,2(2X,A),2X,A)

                  RTimes = 0.D0
                  RS_Hours = 0.D0
                  RS_Azim = 0.D0
                  MoonStat = ''
                  DJD = DJD + 1.D0
                  weekDayNum = weekDayNum + 1
                  if(weekDayNum > 7) weekDayNum = 1
                  Oday = Oday + 1
                  if(Oday > OdaysInMonth) then
                        Oday = 1
                        Omonth = Omonth + 1
                        if(Omonth >12) then
                              Oyear = Oyear + 1
                              Omonth = 1
                        end if
                        if(CalenType == 1) then
                              OmonthAb = AbGmonthName(Omonth)
                              if(IsLeapYear(Oyear)) then
                                    GdaysInMonth(2) = 29
                              else
                                    GdaysInMonth(2) = 28
                              end if
                        else
                              OmonthAb = AbImonthName(Omonth)
                        end if
                  end if
                  Hd = Hd +1
                  if(DJD == newMoonJD(k)) then
                        k = k + 1
                        Hd = 1
                        Hm = Hm + 1
                        if(Hm > 12) then
                              Hy = Hy + 1
                              Hm = 1
                        end if
                        HmonthAb = AbHmonthName(Hm)
                  end if
            end do
      end do
      close(10)
end subroutine

subroutine MoonCalendar(Location,Iyear,CalenType,Geo,airModel,&
      Method,B_Ilum,aidAccept,DST)

      USE MoonSun
      USE MSISE
      USE NOVAS
      USE SOFA

      implicit none

       integer :: Iyear, CalenType,UT_TT,Method, aidAccept
       integer :: leap, weekDayNum, Oyear, Omonth, Oday
       integer :: OdaysinMonth, Days, k,I,J,Hy,Hm,Hd,n
       integer :: IREFR, H , S, M, airModel, kk, L, q
       integer :: dayofYear,Adjust
       integer, dimension(3) :: Equinox, Jdate
       integer, dimension(0:4) :: MarDay,Gyear
       integer, dimension(12):: DaysInMonth, GdaysInMonth
       real(kind=8) :: Density,a,mu,ts,rr,pp,rm,qm,DELTA_T
       real(kind=8) :: B_Ilum, DJD, hour, newYearJD
       real(kind=4) :: alt,lat,long,stl,Den,Tem,Pre
       real(kind=8) :: MoonAngle,Elev
       real(kind=8) ,dimension(4) :: Geo
       real(kind=8) ,dimension(2) :: atmos, DST
       real(kind=8), dimension(2):: RSTJDout ,RS_Hours, RS_Azim
       real(kind=8),dimension(3) :: RTSJD,ilumRatio,RSHours
       real(kind=8),dimension(0:4) :: UJD, Uhour
       character(9),dimension(7) :: weekDays
       real(kind=8), dimension(0:14,0:3):: MoonPhaseJD
       real(kind=8) , dimension(0:14) :: newMoonJD
       character(3) :: OmonthAb
       character(4) :: YearChar
       character(5) :: HmonthAb
       character(7) :: Ocal
       character(12) :: monthName,MoonStat
       character(30) :: Location
       character(40) :: filename1
       character(55) :: aidAcceptText
       character(3),dimension(12) :: AbGmonthName, AbImonthName
       character(5),dimension(12) :: AbHmonthName
       character(24) ,dimension(3) :: MTimes

      data weekDays / 'Sunday   ','Monday   ','Tuesday  ', &
                      'Wednesday','Thursday ','Friday   ',&
                      'Saturday '/

      data AbGmonthName / 'Jan','Feb','Mar','Apr','May','Jun','Jul',&
                          'Aug','Sep','Oct','Nov','Dec'/

      data AbImonthName / 'Far','Ord','Kho','Tir','Mor','Sha','Meh',&
                          'Abn','Azr','Dey','Bmn','Esf'/

      data AbHmonthName / 'Muhrm', 'Safar', 'Rabav','Rabth', 'Jumul', 'Jumth', &
                          'Rajab', 'Shabn', 'Ramzn','Shavl', 'DhulQ', 'Dhulh'/

      DATA DaysInMonth /31,31,31,31,31,31,30,30,30,30,30,29/
      DATA GdaysInMonth /31,28,31,30,31,30,31,31,30,31,30,31/

      IREFR = 1
      UT_TT = 1

      if(CalenType == 1)then
            call IranCalendar(Iyear,Gyear,UJD,leap,Equinox,MarDay,Uhour)
            if(Iyear == leap) DaysInMonth(12) = 30
            DJD = dint(UJD(2))+0.5D0
            Oyear = Gyear(2)
            Omonth = 3
            Oday = MarDay(2)
            if(Equinox(2) /= 1) Oday = Oday + 1
            call YearMoonPhases(Oyear,Omonth,20,moonPhaseJD)
            newYearJD = TrueJDEquiSolitice(Gyear(2),0)
            call iau_DAT(Oyear,3,MarDay(2),0.D0,DELTA_T,J)
      else
            call JULDAT(IYear,1,1,0.D0,DJD)
            if(IsLeapYear(Iyear)) GdaysInMonth(2) = 29
            call JD2IrCal(DJD,Oyear,Omonth,Oday,hour)
            call IranCalendar(Oyear,Gyear,UJD,leap,Equinox,MarDay,Uhour)
            if(Oyear == leap) DaysInMonth(12) = 30
            call YearMoonPhases(Iyear,1,1,moonPhaseJD)
            newYearJD = TrueJDEquiSolitice(Iyear,0)
            call iau_DAT(Iyear,1,1,0.D0,DELTA_T,J)
      end if

      Uhour(2) = dmod(newYearJD+0.5D0,1.D0)*24.D0+Geo(4)-(DELTA_T+32.184D0)/3600.D0

      weekDaynum = JD2WeekDayNum(DJD)

       do I = 0 , 14
            call HijriAdjust(MoonPhaseJD(I,0),Geo,UT_TT,Method,B_Ilum, &
            AidAccept,NewMoonJD(I),Adjust)
      end do

      do k =0,3
            if(DJD < newMoonJD(k)) then
                  call JD2Hijri(newMoonJD(k-1)+14.D0,Hy,Hm,Hd)
                  Hd = int(DJD-newMoonJD(k-1)) + 1
                  exit
            end if
      end do

      YearChar = Trim(I2str(Iyear))
      filename1 = Trim(Location)//'_MoonCalendar'//YearChar//'.txt'
      open(unit = 10 ,file = trim(filename1), form = 'formatted',status = 'unknown')

       if(aidAccept == 1) then
            aidAcceptText = 'Aid for new Moon sighting is accepted'
        else
            aidAcceptText = 'Only naked eye for sighting new moon crescent'
        end if

      call Hour2HMS(Uhour(2),H,M,S)
      WRITE(*,5)trim(Location),Geo(1),Geo(2),Geo(3),Geo(4),H,M,S,Equinox(3),&
      trim(PersianMonthName(Equinox(2))),Equinox(1),MarDay(2),Gyear(2)
      WRITE(10,5)trim(Location),Geo(1),Geo(2),Geo(3),Geo(4),H,M,S,Equinox(3),&
      trim(PersianMonthName(Equinox(2))),Equinox(1),MarDay(2),Gyear(2)

 5    FORMAT(48X,' IRANIAN, HEJRI, AND GREGORY CALENDAR ',&
       'with MOON TIMES'/1X,120('-')/&
       ' Location: ',A,3X,'Longitude: ',F6.2,3X,'Latitude: ',F5.2,3X,&
            'Altitude: ',F7.2,3X,'Time Zone: ',F5.2 ,//&
       ' Iranian New Year Time, Hour: ',I2,2X,'Min :',I2,2X,'Sec: ',I2,' , Date: ',&
        I2,2X,A,2X,I4,2X,', ',I2,2X,'March ',2X,I4/)

      if(airModel == 1) then
            call stdatm(Geo(3),atmos(2),atmos(1),Density,a,mu,ts,rr,pp,rm,qm,0,kk)
            atmos(2) = atmos(2) -273.15D0
            atmos(1) = Atmos(1)/100.D0
            write(*,6)atmos(1),atmos(2),Density
            write(10,6)atmos(1),atmos(2),Density
 6    format(1X,'Standard Atmospheric Properties,  Pressure: ',F7.2,2X,&
      ' Temperature: ',F6.2,2X,' Density: ',F5.2 /)
      end if

      write(*,7)aidAcceptText,Iyear
      write(10,7)aidAcceptText,Iyear
 7    format(1X,A,/120('-')/52X,I4)

      do I = 1, 12

            if(CalenType == 1) then
                  Ocal = 'Gregory'
                  monthName = PersianMonthName(I)
                  Days = DaysInMonth(I)
                  OdaysinMonth = GdaysInMonth(Omonth)
                  OmonthAb = AbGmonthName(Omonth)
                  dayofYear = dayOfGyear(Oyear,Omonth,Oday)
            else
                  Ocal = 'Iranian'
                  monthName = GregMonthName(I)
                  Days = GdaysInMonth(I)
                  OdaysinMonth = DaysInMonth(Omonth)
                  OmonthAb = AbImonthName(Omonth)
                  dayofYear = dayOfGyear(Iyear,I,1)
            end if

            HmonthAb = AbHmonthName(Hm)

            if(airModel /= 1) then
                  alt = real(Geo(3),kind=4)
                  lat = real(Geo(2),kind=4)
                  long = real(Geo(1),kind=4)
                  Stl = real(5.0,kind=4)
                  call GTD7Tropo(dayofYear,alt,lat,long,stl,Den,Tem,Pre)
                  Density = real(Den,kind=8)
                  atmos(2) = real(Tem,kind=8)
                  atmos(1) = real(Pre,kind=8)
           end if

            WRITE(*,12) monthName
            WRITE(10,12) monthName
 12         FORMAT(1X,120('-')/,50X,A12)

            if(airModel /=1) then
                  WRITE(*,14)atmos(1), atmos(2), Density
                  WRITE(10,14) atmos(1), atmos(2), Density
 14         FORMAT(1X,' Atmospheric Properties, Pressure: ',&
                  F8.3,', Temperature: ',F8.3,', Density: ', F8.3/)
            end if

            WRITE(*,15)Iyear,Oyear,Hy,monthName,Ocal
            WRITE(10,15)Iyear,Oyear,Hy,monthName,Ocal
 15         FORMAT(5X,I4,6X,I4,5X,I4,14X,'Moon',2(22X,'Moon')/&
                  2X,A,1X,A,2X,'Hijri ',2X,'Rise-Azimuth-Illumination ',2X,&
                  'Transit-Altitude-Illum',2X,'Set-Azimuth-Illumination')

             MoonStat = ''

            do J=1,Days

                  call Moon_Day_Rise_Set(Jdate ,DJD, Geo, ATmos ,UT_TT , &
                        MoonAngle,RSTJDout ,RS_Hours, RS_Azim, IREFR)

                  RTSJD(1) = RSTJDout(1)
                  RTSJD(3) = RSTJDout(2)
                  RSHours(1) = RS_Hours(1)
                  RSHours(3) = RS_Hours(2)

                  call MoonTransit(Jdate ,DJD, Geo, Atmos ,UT_TT , &
                  MoonAngle,RTSJD(2) ,RSHours(2), Elev)


                  do n = 1 ,3
                       if(RTSJD(n)/= 0.D0)then
                        RTSJD(n) = RTSJD(n) - Geo(4)/24.D0
                        call Moon_Illum_Fractio(jDate,Hour,RTSJD(n),UT_TT,ilumRatio(n))
                       end if

                  end do

                  if(DJD>= DST(1).and. DJD <= DST(2)) then
                        do n = 1, 3
                           if(RSHours(n)/=0.D0) then
                              RSHours(n) =RSHours(n) +1.D0
                           end if
                        end do
                  end if


                  call Hour2HMS(RSHours(1), H , M , S)
                  MTimes(1) = adjustl(trim(I2str(H))//':'//trim(I2str(M))//&
                  ':'//trim(I2str(S))// '-'//trim(D2str(RS_Azim(1)))//'-'//trim(D2str(ilumRatio(1))))

                  call Hour2HMS(RSHours(2), H , M , S)
                        MTimes(2) = adjustl(trim(I2str(H))//':'//trim(I2str(M))//&
                        ':'//trim(I2str(S))// '-'//trim(D2str(Elev))//'-'//trim(D2str(ilumRatio(2))))

                  call Hour2HMS(RSHours(3), H , M , S)
                  MTimes(3) = adjustl(trim(I2str(H))//':'//trim(I2str(M))//&
                  ':'//trim(I2str(S))// '-'//trim(D2str(RS_Azim(2)))//'-'//trim(D2str(ilumRatio(3))))

                  do L= I-1 , I+1
                        do q = 0,3
                              if(DJD <= MoonPhaseJD(L,q)+Geo(4)/24.D0 .and. &
                                 DJD+1.D0 > MoonPhaseJD(L,q)+Geo(4)/24.D0) then
                                    if(q==0) then
                                          MoonStat = ' New Moon'
                                          exit
                                    elseif(q==1) then
                                          MoonStat = ' First Half'
                                          exit
                                    elseif(q==2)then
                                          MoonStat = ' Full Moon'
                                          exit
                                    else
                                          MoonStat = ' Last Half'
                                          exit
                                    end if
                              end if
                        end do
                  end do


                  write(*,20)WeekDays(weekDaynum),J,OmonthAb,Oday,HmonthAb,Hd&
                  ,(MTimes),MoonStat
                  write(10,20)WeekDays(weekDaynum),J,OmonthAb,Oday,HmonthAb,Hd&
                  ,(MTimes),MoonStat
 20               format(A,2X,I2,2X,A,','I2,2X,A,',',I2,3(2X,A24),2X,A)

                  RS_Hours = 0.D0
                  RS_Azim = 0.D0
                  RSHours = 0.D0
                  RSTJDout = 0.D0
                  RTSJD = 0.D0
                  ilumRatio = 0.D0
                  Elev = 0.D0
                  MTimes = ''
                  MoonStat = ''
                  DJD = DJD + 1.D0
                  weekDayNum = weekDayNum + 1
                  if(weekDayNum > 7) weekDayNum = 1
                  Oday = Oday + 1
                  if(Oday > OdaysInMonth) then
                        Oday = 1
                        Omonth = Omonth + 1
                        if(Omonth >12) then
                              Oyear = Oyear + 1
                              Omonth = 1
                        end if
                        if(CalenType == 1) then
                              OmonthAb = AbGmonthName(Omonth)
                              if(IsLeapYear(Oyear)) then
                                    GdaysInMonth(2) = 29
                              else
                                    GdaysInMonth(2) = 28
                              end if
                        else
                              OmonthAb = AbImonthName(Omonth)
                        end if
                  end if
                  Hd = Hd +1
                  if(DJD == newMoonJD(k)) then
                        k = k + 1
                        Hd = 1
                        Hm = Hm + 1
                        if(Hm > 12) then
                              Hy = Hy + 1
                              Hm = 1
                        end if
                        HmonthAb = AbHmonthName(Hm)
                  end if
            end do
      end do
      close(10)
end subroutine


subroutine getLocation(LocatName,Geo)

      real(kind=8),dimension(4) :: Geo
      character(30) :: LocatName

      Write(*,'(A,$)') 'Enter Name of Location: '
      read(*,'(A)') LocatName

      InErr = 1
      do while(InErr == 1)
            Write(*,'(A,$)') 'Enter Longitude in decimal degrees: '
            read(*,*) Geo(1)
            if(Geo(1) > 180.D0 .or. Geo(1) < -180.D0) then
                  write(*,*) 'Longitude is out of range.'
            else
                  InErr = 0
            end if
      end do

      InErr = 1
      do while(InErr == 1)
            Write(*,'(A,$)') 'Enter Latitude in decimal degrees: '
            read(*,*) Geo(2)
            if(Geo(2) > 90.D0 .or. Geo(2) < -90.D0) then
                  write(*,*) 'Latitude is out of range.'
            else
                  InErr = 0
             end if
      end do

      InErr = 1
      do while(InErr == 1)
            Write(*,'(A,$)') 'Enter Altitude in decimal meters: '
            read(*,*) Geo(3)
            if(Geo(3) > 8000.D0 .or. Geo(3) < -29.D0) then
                  write(*,*) 'Altitude is out of range.'
            else
                  InErr = 0
            end if
      end do

      InErr = 1
      do while(InErr == 1)
            Write(*,'(A,$) ') 'Enter Time Zone in decimal hours: '
            read(*,*) Geo(4)
            if(Geo(4) > 12.D0 .or. Geo(4) < -12.D0) then
                  write(*,*) 'Time Zone is out of range.'
            else
                  InErr = 0
            end if
      end do

end subroutine

!---------------------------------------------------------------------------

