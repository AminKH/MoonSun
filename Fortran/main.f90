! A fortran95 program for G95
! By Amin Khiabani
program main

      USE MoonSun
      USE MSISE
      USE SOFA

      implicit none

      integer :: UT_TT, IREF, airModel, options
      integer :: H,M,S,I, airType,Iry,Irm,Ird,Year
      integer :: GdayofYear , PdayofYear,DSTop
      integer :: kk, Hy,Hm,Hd, Method, aidAccept
      integer :: CalenType, Iyear,Gy,Gm,Gd,J,k,StatOver
      integer :: UH,UM,US, Imonth, Iday
      integer,dimension(3) :: Jdate
      integer,dimension(14,2) :: newMoonDate

      real(kind=8) :: Hour,B_Ilum,UJDtoday, LJD, JDM, DS
      real(kind=8) :: JDtoday,TJDT,moonAlt,moonAz,moondel
      real(kind=8) :: SMUJD
      real(kind=8) :: D1, D2,ilumRatio, NMJD,hour0
      real(kind=8), dimension(2) :: atmos,DST
      real(kind=8), dimension(3) :: RSTJD,RTSangles
      real(kind=8), dimension(4) :: Geo
      real(kind=8), dimension(9) :: sunTimes
      real(kind=8), dimension(0:3) :: TJD
      real(kind=8), dimension(14,0:3):: MoonPhaseJD
      real(kind=8), dimension(14) :: HijriJD
      real(kind=8),dimension(4) :: mRTSJD,mRTShour,mRTSangles
      integer,dimension(4)  :: mEvents
      real(kind=4) :: alt,lat,long,Stl,Den,Tem,Pre
      real(kind=8) :: sunAlt,sunZen,sunAz,topAlfa,topoDelta
      real(kind=8) :: mu,Density,a,ts,pp,rm
      real(kind=8) :: qm,RR, DELTA_T, moonAl
      character(16) ,dimension(9) :: STimes
      character(10),dimension(7) :: weekDays
      character(19) :: MTime
      character(16),dimension(0:3):: EquiSolislael
      character(13),dimension(0:3) :: GregDate, PersDate, moonTime
      character(44) :: HijriDate
      character(40) :: filename1
      character(30) :: LocatName
      character(13) :: HGregDate, HPersDate
      character(9) :: weekday
      character(4) :: YearChar
      character(1) :: Responce
      character(LEN=8) :: Milum
      integer,dimension(3) :: today, now, Irtoday
      real(kind=8),dimension(3) :: Times,RSTJD3,RTS_Angles

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

     UT_TT = 0
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
            atmos(1) = atmos(1)/1.D2
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

     call iau_DTF2D ( '*', today(3),today(2),today(1), now(1), now(2),real(now(3),kind=8), D1, D2, J )
     JDtoday =  D1 + D2
     UJDtoday = JDtoday - Geo(4)/24.D0

     SMUJD = CAL2JD ( today(3),today(2),today(1) )

     weekday = JD2WeekDay(SMUJD,1)

     call JD2TradHijri(SMUJD,Geo,Atmos,UT_TT,Method,B_Ilum,aidAccept,IREF,NMJD,HY,HM,HD,Hour0)


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
      call lunar_position(UJDtoday,UT_TT,TJDT,Geo,Atmos,moonAlt,moonAz,moonAl,moondel,IREF)

      write(*,30)sunAlt,sunAz,moonAlt, moonAz
30    format(' Sun Altitude Angle(Degrees): ',F6.2,3X,'Sun Azimuth Angle(Degrees): ',F7.2/&
             ' Moon Altitude Angle(Degrees): ',F6.2,3X,'Moon Azimuth Angle(Degrees): ',F7.2/)
      print*,
      print*,

      call AstroSolarTimes(SMUJD,Jdate,Geo,atmos,UT_TT,IREF,sunTimes,RSTJD,RTSangles)
   !   call iau_DAT ( today(3),today(2),today(1),sunTimes(5)/24.D0, DELTA_T, J )


      do I=1,9
          !  sunTimes(I) = sunTimes(I) + (DELTA_T+32.184D0)/3600.D0
            call Hour2HMS(sunTimes(I),H,M,S)
                  STimes(I) = adjustl(trim(I2str(H))//':'//trim(I2str(M))//&
                  ':'//trim(I2str(S)))
      end do

      write(*,*)'  SunRise: ',STimes(4),'   Noon: ',STimes(5), '   SunSet: ', STimes(6)
      write(*,'(A,F7.3,3X,A,F6.3,3X,A,F7.3)')' SunRise Azimuth: ',RTSangles(1)&
            , 'Noon Altitude: ',RTSangles(2),'SunSet Azimuth: ', RTSangles(3)
      write(*,*)' Astronomical Twilight: ',STimes(1),'  Nautical Twilight: ',STimes(2),&
                  '  Civic Twilight: ', STimes(3)
      write(*,*)' Astronomical Twilight: ',STimes(9),'  Nautical Twilight: ',STimes(8),&
                  '  Civic Twilight: ', STimes(7)

      call SolarTimes(SMUJD,Jdate,Geo,Atmos,UT_TT,IREF,Times,RSTJD3,RTS_Angles)

      write(*,*) 'MOON'

      call Moon_RiseTranSet(Jdate,SMUJD,Geo,Atmos,UT_TT,IREF,mEvents,mRTSJD,mRTShour,mRTSangles)

      do J = 1 , 4
            if(mRTSJD(J) /=0.D0 ) then
                  Mtime = Hour2text(mRTShour(J))// &
                        ','//adjustl(trim(D2str(mRTSangles(J))))
                  call Moon_Illum_Fractio(Jdate,Hour,mRTSJD(J),UT_TT,ilumRatio)
                  Milum = ','//adjustl(trim(D2str(ilumRatio)))
                  write(*,'(2X,A,2X,A,2X,$)') Mtime//Milum,' '
            end if
     end do
      print*,
      print*,

      Iyear = today(3)
      CalenType = 2
      YearChar = Trim(I2str(Iyear))
      DST = 0.D0

31    write(*,35) Iyear
35    format('Select Options'// '  1- Select Calendar type, 1- Iranian , 2- Gregory, default is Gregory'/&
            '  and  Type a Year, current year is ',I4/&
            '  2- Select for Daylight saving Time in calendars? '/&
            '  3- Solar Equinoxes and Solstices for a Year'/&
            '  4- Moon Phases for a year'/&
            '  5- Hijri Months of Year'/ &
            '  6- Sun and Moon Calendar for a year'/&
            '  7- Moon Calendar for a year with SunRise And Set times'/&
            '  8- Start Over '/&
            '  9- Exit' )
      print*,
      write(*,'(A,$)') ' Your Option? '
      read(*,'(I3)') options

      if(options== 9) then
            return
      elseif(options==8) then
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
            write(*,'(A,$)') '  Select Calendar Type, and Year: '
            read(*,*) CalenType,Iyear
            YearChar = Trim(I2str(Iyear))
      endif

      if(CalenType == 1) then
                  Year = Iyear +621
                  Imonth = 3
                  Iday = 21
            else
                  year = Iyear
                  Imonth =  1
                  Iday = 15
      end if

      if(options==2) then
            write(*,55)
       55   format(' Select of of options: '/&
                   ' 1- Europe'/&
                   ' 2- Iran'/&
                   ' 3- North America')
                  read(*,'(I1)') DSTop
            if(DSTop == 1) then
                  DS = CAL2JD ( Year,3,1)
                 do while(JD2WeekDayNum(DS)/= 2)
                  DS = DS + 1
                 end do
                 DST(1) = DS + 21
                 DS =CAL2JD ( Year,10,1)

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
                  DS = CAL2JD ( Year,3,1)
                 do while(JD2WeekDayNum(DS)/= 2)
                  DS = DS + 1
                 end do
                 DST(1) = DS + 7
                  DS = CAL2JD ( Year,11,1)
                 do while(JD2WeekDayNum(DS)/= 2)
                  DS = DS + 1
                 end do
                 DST(2) = DS
      end if
      print*, 'Start of Daylight Saving Julian Day= ' , DST(1)
      print*, 'End of Daylight Saving Julian Day= ' ,  DST(2)
      endif

      if(options==3) then
            data EquiSolislael /'Spring Equinox','Summer Solstice',&
                              'Fall Equinox','Winter Solstice'/

            do k = 0,3
                  TJD(k) = TrueJDEquiSolitice(Year,k)
                  call JD2CAL(TJD(k),Gy,Gm,Gd,Hour)
                  call iau_DAT(Gy,Gm,Gd,Hour/24D0,DELTA_T,J)
                  Hour = Hour - (DELTA_T+32.184D0)/3600.D0
                  call Hour2HMS(Hour,H,M,S)
                  LJD = TJD(k) + (Geo(4)-(DELTA_T+32.184D0)/3600.D0)/24.D0
                  call JD2IrCal(LJD,Iry,Irm,Ird,Hour)
                  call Hour2HMS(Hour,UH,UM,US)
                  write(*,40) EquiSolislael(k),TJD(k),Gy,Gm,Gd,H,M,S,Iry,&
                        Irm,Ird,UH,UM,US
            end do
 40          format(A/' Julidan Day:',F15.4/ ' Gregory Date: ',I4,'/',I2,'/',I2&
                    ' , Universal Time: ', I2,':',I2,':',I2/' Iranian Date: ' &
                    ,I4,'/',I2,'/',I2,' , Local Time: ',I2,':',I2,':',I2//)

      elseif(options==4)then

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

            do I = 1 , 14

                  do k = 0,3
                        JDM = (MoonPhaseJD(I,k)) + Geo(4)/24.D0
                        call JD2CAL (JDM,Gy,Gm,Gd,Hour )

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

      elseif(options==5) then

            call HijriMonthCS(year,Imonth,Iday,Geo,Atmos,UT_TT,Method,B_Ilum,AidAccept,IREF,HijriJD,newMoonDate)

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

            do I = 1 , 14
                  JDM = (HijriJD(I))
                  Hm = newMoonDate(I,2)
                  Hy = newMoonDate(I,1)
                  print*,I, HY,HM
                  Hijridate = adjustl('First(1) of '//trim(HijriMonthName(Hm))//&
                  ', year '//trim(I2str(Hy)))

                  call JD2CAL(JDM,Gy,Gm,Gd,Hour)
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

      elseif(options==6) then

            filename1 = Trim(LocatName)//'_MoonSunCalendar'//YearChar//'.txt'

            write(*,42)filename1
42          format(' Because large size of data, calendar will save in file ',A/)
            write(*,'(A,$)') ' To proceed type Y or y '
            read(*,'(A)') Responce

            if(Responce == 'Y' .or. Responce == 'y') then
                  call SunMoonCalendar(LocatName,Iyear,CalenType,Geo,airModel,&
                  Method,B_Ilum,aidAccept,DST,UT_TT,IREF)
            end if


      elseif(options==7) then

            filename1 = Trim(LocatName)//'_MoonCalendar'//YearChar//'.txt'

            write(*,44)filename1
44          format(' Because large size of data, calendar will save in file ',A/)
            write(*,'(A,$)') ' To proceed type Y or y '
            read(*,'(A)') Responce

            if(Responce == 'Y' .or. Responce == 'y') then
                  call MoonCalendar(LocatName,Iyear,CalenType,Geo,airModel,&
                  Method,B_Ilum,aidAccept,DST,UT_TT,IREF)
            end if

      end if

      goto 31
 90   stop
      return
end

subroutine SunMoonCalendar(Location,Iyear,CalenType,Geo,airModel,&
      Method,B_Ilum,aidAccept,DST,UT_TT,IREFR)

      USE MoonSun
      USE MSISE
      USE SOFA

      implicit none

       integer :: Iyear, CalenType,UT_TT,Method, aidAccept
       integer ::  weekDayNum, Oyear, Omonth, Oday,Nm
       integer :: OdaysinMonth, Days, k,I,J,Hy,Hm,Hd,n,km
       integer :: IREFR, H , S, M, airModel, kk, L, q
       integer :: dayofYear
       integer, dimension(3) :: Equinox, Jdate
       integer :: MarDay,Gyear
       integer, dimension(12):: DaysInMonth, GdaysInMonth
       integer,dimension(14,2) :: newMoonDate
       real(kind=8) :: Density,a,mu,ts,rr,pp,rm,qm ,DELTA_T
       real(kind=8) :: B_Ilum, DJD, hour, newYearJD,NMJD
       real(kind=4) :: alt,lat,long,stl,Den,Tem,Pre
       real(kind=8) ,dimension(4) :: Geo, Geo1
       real(kind=8) ,dimension(2) :: atmos, DST
       real(kind=8) ,dimension(3):: RTSangles
       real(kind=8) :: UJD, Uhour
       character(9) ,dimension(7) :: weekDays
       real(kind=8) ,dimension(14,0:3):: MoonPhaseJD
       real(kind=8) ,dimension(14) :: newMoonJD
       real(kind=8) ,dimension(9) :: RTimes,RSTJD
       real(kind=8) ,dimension(0:2,0:2) :: MRTShour,MRTSJD,MRTSang
       real(kind=8) ,dimension(4) :: moonRTSJD,moonRTShour,moonRTSangles
       character(Len=28), dimension(4) :: moonEvents
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
       character(112) :: MTimes
       logical :: isLeap

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

      Geo1 = Geo

      if(CalenType == 1)then
            call IranCalendar(IYear,Gyear,UJD,isleap,Equinox,MarDay,UHour)
            if(isLeap) DaysInMonth(12) = 30
            DJD = dint(UJD)+0.5D0
            Oyear = Gyear
            Omonth = 3
            Oday = MarDay
            if(Equinox(2) /= 1) Oday = Oday + 1
            call HijriMonths(Gyear,Omonth,Oday,Geo,Atmos,UT_TT,Method,B_Ilum,AidAccept,IREFR,NewMoonJD,newMoonDate,MoonPhaseJD)
            newYearJD = TrueJDEquiSolitice(Gyear,0)
            call iau_DAT(Gyear,3,MarDay,0.D0,DELTA_T,J)
      else
            DJD = CAL2JD (IYear,1,1)
            if(IsLeapYear(Iyear)) GdaysInMonth(2) = 29
            call JD2IrCal(DJD,Oyear,Omonth,Oday,hour)
            call IranCalendar(Oyear,Gyear,UJD,isLeap,Equinox,MarDay,Uhour)
            if(isLeap) DaysInMonth(12) = 30
            call HijriMonths(Iyear,1,1,Geo,Atmos,UT_TT,Method,B_Ilum,AidAccept,IREFR,NewMoonJD,newMoonDate,MoonPhaseJD)
            newYearJD = TrueJDEquiSolitice(Iyear,0)
            call iau_DAT(Iyear,1,1,0.D0,DELTA_T,J)
      end if

      Uhour= dmod(newYearJD+0.5D0,1.D0)*24.D0+Geo(4) -(DELTA_T+32.184D0)/3600.D0

      weekDaynum = JD2WeekDayNum(DJD)

      YearChar = Trim(I2str(Iyear))
      filename1 = Trim(Location)//'_MoonSunCalendar'//YearChar//'.txt'

      open(unit = 10 ,file = filename1, form = 'formatted',status = 'unknown')

       if(aidAccept == 1) then
            aidAcceptText = 'Aid for new Moon sighting is accepted'
        else
            aidAcceptText = 'Only naked eye for sighting new moon crescent'
        end if

      call Hour2HMS(Uhour,H,M,S)
      WRITE(*,5)trim(Location),Geo(1),Geo(2),Geo(3),Geo(4),H,M,S,Equinox(3),&
      trim(PersianMonthName(Equinox(2))),Equinox(1),MarDay,Gyear
      WRITE(10,5)trim(Location),Geo(1),Geo(2),Geo(3),Geo(4),H,M,S,Equinox(3),&
      trim(PersianMonthName(Equinox(2))),Equinox(1),MarDay,Gyear

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

      call JD2TradHijri(DJD,Geo,Atmos,UT_TT,Method,B_Ilum,aidAccept,IREFR,NMJD,HY,HM,HD,Hour)

      do km =1,3
            if(DJD < newMoonJD(km)) exit
      end do

      write(*,7)aidAcceptText,Iyear
      write(10,7)aidAcceptText,Iyear
 7    format(1X,A,/200('-')/100X,I4)

      Call Moon_RTS(Jdate ,DJD-1.D0, Geo, Atmos ,UT_TT,IREFR,MRTShour(0,:),MRTSJD(0,:),MRTSang(0,:))
      call Moon_RTS(Jdate ,DJD, Geo, Atmos ,UT_TT,IREFR,MRTShour(1,:),MRTSJD(1,:),MRTSang(1,:))

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

            WRITE(*,12) monthName, Iyear
            WRITE(10,12) monthName, Iyear
 12         FORMAT(1X,200('-')/,96X,A12,2X,I4)

            if(airModel /=1) then
                  WRITE(*,14)atmos(1), atmos(2), Density
                  WRITE(10,14) atmos(1), atmos(2), Density
 14         FORMAT(1X,' Atmospheric Properties, Pressure: ',&
                  F8.3,', Temperature: ',F8.3,', Density: ', F8.3/)
            end if

            WRITE(*,15)Iyear,Oyear,Hy,monthName,Ocal
            WRITE(10,15)Iyear,Oyear,Hy,monthName,Ocal
 15         FORMAT(5X,I4,6X,I4,5X,I4,5X,'---------Twilight---------',8X,'Sun',18X,'Noon',16X,'Sun ',&
                  12X,'---------Twilight---------',6X,'Moon',14X,'Moon',22X,'Moon',22X,'Moon'/2X,A,1X,A,2X,'Hijri',&
                  2X,'Astronomical Nautical  Civic',5X,'Rise-Azimuth ',7X,'Time-Altitude',9X,&
                  'Set-Azimuth',6X,'Civic  Nautical Astronomical',5X,'Phases')

             MoonStat = ''

            do J=1,Days

                  if(DJD>= DST(1).and. DJD <= DST(2)) then
                        Geo1(4) = Geo(4) + 1.D0
                  else
                        Geo1(4) = Geo(4)
                  end if

                  call AstroSolarTimes(DJD,Jdate,Geo1,atmos,UT_TT,IREFR,RTimes,RSTJD,RTSangles)

                    do n=1,9
                        if(RTimes(n) /= 99.D0 .and. RTimes(n) /= 0.D0) then
                              call Hour2HMS(RTimes(n),H,M,S)
                              if( n>= 4 .and. n<=6) then
                                    STimes(n) = adjustl(trim(I2str(H))//':'//trim(I2str(M))//&
                                    ':'//trim(I2str(S))//  ','//trim(D2str(RTSAngles(n-3))))
                              else
                                    STimes(n) = adjustl(trim(I2str(H))//':'//trim(I2str(M))//&
                                    ':'//trim(I2str(S)))
                             end if
                        else
                             if(RTimes(n) == 99.D0) then
                                  STimes(n) = "Sun up"
                              else if (RTimes(n) == 0.D0) then
                                  STimes(n) = "Sun down"
                             end if
                        end if

                  end do

                  call Moon_RTS(Jdate ,DJD+1.D0, Geo1, Atmos ,UT_TT,IREFR,MRTShour(2,:),MRTSJD(2,:),MRTSang(2,:))
                  call MoonRiseTranSet(DJD,MRTSJD,MRTShour,MRTSang,Geo1(4), &
                              moonEvents,moonRTSJD,moonRTShour,moonRTSangles)

                  Nm = 1

                  MTimes = '--------:--------,---------- --------:--------,---------- --------:--------,----------'
                  do K = 1 , 4
                      if(moonRTSJD(K) /= 0.D0 ) then
                          Nm = Nm + 1
                      end if
                  end do

                  if(Nm-1 <= 3) then
                      MTimes = &
                          moonEvents(1)//moonEvents(2)//moonEvents(3)
                  elseif(Nm-1 == 4) then
                     MTimes = &
                          moonEvents(1)//moonEvents(2)//moonEvents(3)//moonEvents(4)
                  end if


                  do L= I , I+2
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

                  write(*,20)WeekDays(weekDaynum),J,OmonthAb,Oday,HmonthAb,Hd,Stimes,MoonStat,MTimes
                  write(10,20)WeekDays(weekDaynum),J,OmonthAb,Oday,HmonthAb,Hd,Stimes,MoonStat,MTimes
 20               format(A,2X,I2,2X,A,','I2,2X,A,',',I2,3(2X,A8),3(2X,A18),3(2X,A8),X,A,(X,A98))

                  MRTSJD(0,:) = MRTSJD(1,:)
                  MRTShour(0,:) = MRTShour(1,:)
                  MRTSang(0,:) = MRTSang(1,:)
                  MRTSJD(1,:) = MRTSJD(2,:)
                  MRTShour(1,:) = MRTShour(2,:)
                  MRTSang(1,:) = MRTSang(2,:)

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
                  if(DJD == newMoonJD(km)) then
                        Hd = 1
                        Hy = newMoonDate(km,1)
                        Hm = newMoonDate(km,2)
                        HmonthAb = AbHmonthName(Hm)
                        km = km + 1
                  end if
            end do
      end do
      close(10)

end subroutine

subroutine MoonCalendar(Location,Iyear,CalenType,Geo,airModel,&
      Method,B_Ilum,aidAccept,DST,UT_TT,IREFR)

      USE MoonSun
      USE MSISE
      USE SOFA

      implicit none

       integer :: Iyear, CalenType,UT_TT,Method, aidAccept
       integer :: weekDayNum, Oyear, Omonth, Oday, Nm, Km
       integer :: OdaysinMonth, Days, k,I,J,Hy,Hm,Hd
       integer :: IREFR, H , S, M, airModel, kk, L, q
       integer :: dayofYear
       integer, dimension(3) :: Equinox, Jdate
       integer :: MarDay,Gyear
       integer, dimension(12):: DaysInMonth, GdaysInMonth
       integer,dimension(14,2) :: newMoonDate

       real(kind=8) :: Density,a,mu,ts,rr,pp,rm,qm ,DELTA_T
       real(kind=8) :: B_Ilum, DJD, hour, newYearJD
       real(kind=4) :: alt,lat,long,stl,Den,Tem,Pre
       real(kind=8) :: ilumRatio, NMJD
       real(kind=8) ,dimension(4) :: Geo, Geo1
       real(kind=8) ,dimension(2) :: atmos, DST
       real(kind=8) ,dimension(0:2,0:2) :: MRTShour,MRTSJD,MRTSang
       real(kind=8) ,dimension(4) :: moonRTSJD,moonRTShour,moonRTSangles
       real(kind=8) ,dimension(3) :: Times,RSTJD,RTS_Angles
       real(kind=8) :: UJD, Uhour
       character(Len=28), dimension(4) :: moonEvents
       character(9),dimension(7) :: weekDays
       real(kind=8), dimension(14,0:3):: MoonPhaseJD
       real(kind=8) , dimension(14) :: newMoonJD
       character(3) :: OmonthAb
       character(4) :: YearChar
       character(5) :: HmonthAb
       character(7) :: Ocal
       character(12) :: monthName,MoonStat
       character(30) :: Location
       character(40) :: filename1
       character(55) :: aidAcceptText
       character(8), dimension(2) :: Stime
       character(10), dimension(2) :: Sangle
       character(6), dimension(4) :: MSilum
       character(3),dimension(12) :: AbGmonthName, AbImonthName
       character(5),dimension(12) :: AbHmonthName
       character(39) :: Stimes
       character(143) :: MTimes
       logical :: isLeap

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

      Geo1 = Geo

      if(CalenType == 1)then
            call IranCalendar(IYear,Gyear,UJD,isleap,Equinox,MarDay,UHour)
            if(isLeap) DaysInMonth(12) = 30
            DJD = dint(UJD)+0.5D0
            Oyear = Gyear
            Omonth = 3
            Oday = MarDay
            if(Equinox(2) /= 1) Oday = Oday + 1
            call HijriMonths(Gyear,Omonth,Oday,Geo,Atmos,UT_TT,Method,B_Ilum,AidAccept,IREFR,NewMoonJD,newMoonDate,MoonPhaseJD)
            newYearJD = TrueJDEquiSolitice(Gyear,0)
            call iau_DAT(Gyear,3,MarDay,0.D0,DELTA_T,J)
      else
            DJD = CAL2JD (IYear,1,1)
            if(IsLeapYear(Iyear)) GdaysInMonth(2) = 29
            call JD2IrCal(DJD,Oyear,Omonth,Oday,hour)
            call IranCalendar(Oyear,Gyear,UJD,isLeap,Equinox,MarDay,Uhour)
            if(isLeap) DaysInMonth(12) = 30
            call HijriMonths(Iyear,1,1,Geo,Atmos,UT_TT,Method,B_Ilum,AidAccept,IREFR,NewMoonJD,newMoonDate,MoonPhaseJD)
            newYearJD = TrueJDEquiSolitice(Iyear,0)
            call iau_DAT(Iyear,1,1,0.D0,DELTA_T,J)
      end if

      Uhour= dmod(newYearJD+0.5D0,1.D0)*24.D0+Geo(4) -(DELTA_T+32.184D0)/3600.D0

      weekDaynum = JD2WeekDayNum(DJD)

      YearChar = Trim(I2str(Iyear))
      filename1 = Trim(Location)//'_MoonCalendar'//YearChar//'.txt'
      open(unit = 10 ,file = trim(filename1), form = 'formatted',status = 'unknown')

       if(aidAccept == 1) then
            aidAcceptText = 'Aid for new Moon sighting is accepted'
        else
            aidAcceptText = 'Only naked eye for sighting new moon crescent'
        end if

      call Hour2HMS(Uhour,H,M,S)
      WRITE(*,5)trim(Location),Geo(1),Geo(2),Geo(3),Geo(4),H,M,S,Equinox(3),&
      trim(PersianMonthName(Equinox(2))),Equinox(1),MarDay,Gyear
      WRITE(10,5)trim(Location),Geo(1),Geo(2),Geo(3),Geo(4),H,M,S,Equinox(3),&
      trim(PersianMonthName(Equinox(2))),Equinox(1),MarDay,Gyear

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

      call JD2TradHijri(DJD,Geo,Atmos,UT_TT,Method,B_Ilum,aidAccept,IREFR,NMJD,HY,HM,HD,Hour)

      do km =1,3
            if(DJD < newMoonJD(km)) exit
      end do

      Call Moon_RTS(Jdate ,DJD-1.D0, Geo1, Atmos ,UT_TT,IREFR,MRTShour(0,:),MRTSJD(0,:),MRTSang(0,:))
      call Moon_RTS(Jdate ,DJD, Geo1, Atmos ,UT_TT,IREFR,MRTShour(1,:),MRTSJD(1,:),MRTSang(1,:))

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

            WRITE(*,12) monthName,Iyear
            WRITE(10,12) monthName,Iyear
 12         FORMAT(1X,120('-')/,50X,A12,2X,I4)

            if(airModel /=1) then
                  WRITE(*,14)atmos(1), atmos(2), Density
                  WRITE(10,14) atmos(1), atmos(2), Density
 14         FORMAT(1X,' Atmospheric Properties, Pressure: ',&
                  F8.3,', Temperature: ',F8.3,', Density: ', F8.3/)
            end if

            WRITE(*,15)Iyear,Oyear,Hy,monthName,Ocal
            WRITE(10,15)Iyear,Oyear,Hy,monthName,Ocal
 15         FORMAT(5X,I4,6X,I4,5X,I4,2(14X,'SUN'),2(14X,'Moon'),2(32X,'Moon')/&
                  2X,A,1X,A,2X,'Hijri',12X,'Rise',14X,'Set',14X,'Phase')


            do J=1,Days

                  if(DJD>= DST(1).and. DJD <= DST(2)) then
                        Geo1(4) = Geo(4) + 1.D0
                  else
                        Geo1(4) = Geo(4)
                  end if

                  call SolarTimes(DJD,Jdate,Geo1,Atmos,UT_TT,IREFR,Times,RSTJD,RTS_Angles)

                  if (Times(1) /= 99.D0 .and. Times(1) /= 0.D0 ) then
                        STime(1) = Hour2text(Times(1))
                        Sangle(1) = Angles2text(RTS_Angles(1))
                  endif
                  IF (Times(3) /= 99.D0 .and. Times(3) /= 0.D0 ) then
                        STime(2) = Hour2text(Times(3))
                        Sangle(2) = Angles2text(RTS_Angles(3))
                  endif
                 if(Times(1) == 99.D0) then
                      STime(1) = "Sun up"
                  else if (Times(1) == 0.D0) then
                      STime(1) = "Sun down"
                 end if
                 if(Times(3) == 99.D0) then
                      STime(2) = "Sun up"
                  else if (Times(3) == 0.D0) then
                      STime(2) = "Sun down"
                 end if

                  Stimes = Stime(1)//','//Sangle(1)//' '//Stime(2)//','//Sangle(2)

                  call Moon_RTS(Jdate ,DJD+1.D0, Geo1, Atmos ,UT_TT,IREFR,MRTShour(2,:),MRTSJD(2,:),MRTSang(2,:))
                  call MoonRiseTranSet(DJD,MRTSJD,MRTShour,MRTSang,Geo1(4), &
                              moonEvents,moonRTSJD,moonRTShour,moonRTSangles)

                  Nm = 1
                  MSilum = '------'
                  MTimes = '--------:--------,----------,------' // &
                  &        '--------:--------,----------,------'//  &
                  &        '--------:--------,----------,------'
                  do K = 1 , 4
                      if(moonRTSJD(K) /= 0.D0 ) then
                          call Moon_Illum_Fractio(Jdate,Hour,moonRTSJD(K),UT_TT,ilumRatio)
                          MSilum(Nm) = adjustl(trim(D2str(ilumRatio)))
                          Nm = Nm + 1
                      end if
                  end do

                  if(Nm-1 <= 3) then
                      MTimes = &
                              moonEvents(1)//','//MSilum(1)// &
                         ' '//moonEvents(2)//','//MSilum(2)// &
                         ' '//moonEvents(3)//','//MSilum(3)
                  elseif(Nm-1 == 4) then
                     MTimes = &
                               moonEvents(1)//','//MSilum(1)// &
                          ' '//moonEvents(2)//','//MSilum(2)// &
                          ' '//moonEvents(3)//','//MSilum(3)// &
                          ' '//moonEvents(4)//','//MSilum(4)
                  end if


                  do L= I , I+2
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

                  write(*,20)WeekDays(weekDaynum),J,OmonthAb,Oday,HmonthAb,Hd,Stimes,MoonStat,MTimes
                  write(10,20)WeekDays(weekDaynum),J,OmonthAb,Oday,HmonthAb,Hd,Stimes,MoonStat,MTimes
 20               format(A,2X,I2,2X,A,','I2,2X,A,',',I2,2X,A40,2X,A,X,A140)


                  MRTSJD(0,:) = MRTSJD(1,:)
                  MRTShour(0,:) = MRTShour(1,:)
                  MRTSang(0,:) = MRTSang(1,:)
                  MRTSJD(1,:) = MRTSJD(2,:)
                  MRTShour(1,:) = MRTShour(2,:)
                  MRTSang(1,:) = MRTSang(2,:)

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
                  if(DJD == newMoonJD(km)) then
                        Hd = 1
                        Hy = newMoonDate(km,1)
                        Hm = newMoonDate(km,2)
                        HmonthAb = AbHmonthName(Hm)
                        km = km + 1
                  end if
            end do
      end do
      close(10)
end subroutine


subroutine getLocation(LocatName,Geo)

      real(kind=8),dimension(4) :: Geo
      logical :: InErr
      character(30) :: LocatName

      Write(*,'(A,$)') 'Enter Name of Location: '
      read(*,'(A)') LocatName

      InErr = .True.
      do while(InErr )
            Write(*,'(A,$)') 'Enter Longitude in decimal degrees: '
            read(*,*) Geo(1)
            if(Geo(1) > 180.D0 .or. Geo(1) < -180.D0) then
                  write(*,*) 'Longitude is out of range.'
            else
                  InErr = .False.
            end if
      end do

      InErr = .True.
      do while(InErr)
            Write(*,'(A,$)') 'Enter Latitude in decimal degrees: '
            read(*,*) Geo(2)
            if(Geo(2) > 90.D0 .or. Geo(2) < -90.D0) then
                  write(*,*) 'Latitude is out of range.'
            else
                  InErr = .False.
             end if
      end do

      InErr = .True.
      do while(InErr)
            Write(*,'(A,$)') 'Enter Altitude in decimal meters: '
            read(*,*) Geo(3)
            if(Geo(3) > 8000.D0 .or. Geo(3) < -29.D0) then
                  write(*,*) 'Altitude is out of range.'
            else
                  InErr = .False.
            end if
      end do

      InErr = .True.
      do while(InErr)
            Write(*,'(A,$) ') 'Enter Time Zone in decimal hours: '
            read(*,*) Geo(4)
            if(Geo(4) > 12.D0 .or. Geo(4) < -12.D0) then
                  write(*,*) 'Time Zone is out of range.'
            else
                  InErr = .False.
            end if
      end do

end subroutine

!---------------------------------------------------------------------------

