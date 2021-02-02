using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Runtime.InteropServices;
using System.Globalization;

namespace ClassMoonSun
{
    internal static class NativeMethods
    {

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_dayofgyear")]
        public static extern int dayofGyear(ref int Gy, ref int Gm, ref int Gd);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_dayofpyear")]
        public static extern int dayofIyear(ref int Im, ref int Id);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__novas_MOD_juldat")]
        public static extern void JULDAT(ref int Iy, ref int Im, ref int Id, ref double H, ref double TJD);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__novas_MOD_rcaldat")]
        public static extern void RCALDAT(ref double TJD, ref int Iy, ref int Im, ref int Id, ref double H); 

         [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__sofa_MOD_iau_jd2cal")]
        public static extern void JD2Cal(ref double DJ1,ref double DJ2,ref int Iy, ref int Im, ref int Id,ref double  FD, ref int J);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__sofa_MOD_iau_utctai")]
        public static extern void UTC2TAI(ref double UTC1, ref double UTC2, ref double TAI1, ref double TAI2, ref int J);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__sofa_MOD_iau_taitt")]
        public static extern void TAI2TT(ref double TAI1, ref double TAI2, ref double TT1, ref double TT2, ref int J);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__sofa_MOD_iau_dat")]
        public static extern void iau_DAT(ref int IY, ref int IM, ref int ID, ref double FD, ref double DELTAT, ref int J);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_jdequisolitice")]
        public static extern double JDEquiSolitice(ref int Iy, ref int k );

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_hour2hms")]
        public static extern void Hour2HMS(ref double Hour, ref int H, ref int M, ref int S);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_jd2hijri")]
        public static extern void JD2Hijri(ref double UTJD, ref int HY, ref int HM, ref int HD);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_ircal2jd")]
        public static extern double IrCal2JD(ref int Iry, ref int Irm, ref int Ird, ref double Hour);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_ircal2gregcal")]
        public static extern void IrCal2GregCal(ref int Iry, ref int Irm, ref int Ird, ref int Gy, ref int Gm, ref int Gd);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_gregcal2ircal")]
        public static extern void GregCal2IrCal(ref int Gy, ref int Gm, ref int Gd, ref int Iry, ref int Irm, ref int Ird);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_jd2ircal")]
        public static extern void JD2IrCal(ref double TJD, ref int Iy, ref int Im, ref int Id, ref double H);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_irancalendar")]
        public static extern void IranCalendar(ref int Iyear,ref  int Gyear, ref double UJD,ref bool leap, [Out] int[] Equinox, 
           ref  int MarDay,ref double UHours);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_isleapyear")]
        public static extern bool GregIsLeapYear(ref int Iyear);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_gdays_in_month")]
        public static extern int GregDaysInmonth(ref int Iyear, ref int Imonth);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_idays_in_month")]
        public static extern int IranianDaysInmonth(ref int Iyear, ref int Imonth);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_deltat")]
        public static extern double DELTAT(ref int Iy, ref int Im);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_azanangle")]
        public static extern double azanAngle(ref double latitude);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_truejdequisolitice")]
        public static extern double TrueJDEquiSolitice(ref int Year, ref int k);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_stdatm")]
        public static extern void StdAtm(ref double z,ref double t,ref double p,ref double r, ref double a,
            ref double mu,ref double ts,ref double rr,ref double pp,ref double rm,ref double qm,ref int kd,ref int kk) ;
        
        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_moon_phases")]
        public static extern void Moon_Phases(ref int Year, ref int Month, ref int Day, ref int New_Full, ref double JDE);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_moon_illum_fractio")]
        public static extern void Moon_IlumRatio([MarshalAs(UnmanagedType.LPArray, SizeConst = 3)] int[] Jdate,
            ref double Hour,ref double  TJD,ref int UT_TT,ref double Ilum_Ratio);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_hijriadjust")]
        public static extern void HijriAdjust(ref double JDE, [MarshalAs(UnmanagedType.LPArray, SizeConst = 4)] double[] Geo,
            ref int UT_TT, ref int Method,ref double B_Ilum, ref int AidAccept,ref double NTJD, ref int Adjust);        

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_moonvisibility")]
           public static extern void MoonVisibility(ref double NJDE, ref double TJD, ref int UT_TT,
            [MarshalAs(UnmanagedType.LPArray, SizeConst = 4)] double[] Geo, ref int Method,
           ref double B_Ilum,ref int AidAccept, ref bool LStatus, ref int visiStat, [Out] double[] Criteria);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_lunar_position")]
        public static extern void lunar_position(ref double TJD, ref int UT_TT, ref double TJDT,
            [MarshalAs(UnmanagedType.LPArray, SizeConst = 4)] double[] Geo,
            [MarshalAs(UnmanagedType.LPArray, SizeConst = 2)] double[] Atmos,
            ref double Elev,ref double Azim, ref double delta,ref int Iref);
                
        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_moon_day_rise_set")]
        public static extern void Moon_Day_Rise_Set([MarshalAs(UnmanagedType.LPArray, SizeConst = 3)] int[] Jdate , 
            ref double TJD1,[MarshalAs(UnmanagedType.LPArray, SizeConst = 4)] double[] Geo,
             [MarshalAs(UnmanagedType.LPArray, SizeConst = 2)] double[] Atmos,
             ref int UT_TT ,ref double MoonAngle,[Out] double[] RSTJDout ,[Out] double[] RS_Hours ,[Out] double[] RS_Azim, ref int IREFR);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_moonsemidia")]
        public static extern void MoonSemiDia(ref double TJD, ref double MoonElevation, ref double GeoDia,ref double TopoDia);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_solar_position")]
        public static extern void Solar_Position(ref double TJD,
           [MarshalAs(UnmanagedType.LPArray, SizeConst = 4)] double[] Geo,
           [MarshalAs(UnmanagedType.LPArray, SizeConst = 2)] double[] Atmos,
           ref int UT_TT ,ref double SunElev, ref double Zenit,ref double Azim,ref double TopoAlfa, ref double TopoDelta,ref int Iref);
        
        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_sunrise_set_noon")]
        public static extern void SunRise_Set_Noon(ref double TJD,
           [MarshalAs(UnmanagedType.LPArray, SizeConst = 3)] int[] Jdate,
           [MarshalAs(UnmanagedType.LPArray, SizeConst = 4)] double[] Geo,
           [MarshalAs(UnmanagedType.LPArray, SizeConst = 2)] double[] Atmos,
           ref double Altitude, ref int UT_TT, ref int Iref, ref double Transit, ref double Rise, ref double Set);

        
        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_astrosolartimes")]
        public static extern void AstroSolarTimes(ref double TJD,
           [MarshalAs(UnmanagedType.LPArray, SizeConst = 3)] int[] Jdate,
           [MarshalAs(UnmanagedType.LPArray, SizeConst = 4)] double[] Geo,
           [MarshalAs(UnmanagedType.LPArray, SizeConst = 2)] double[] Atmos,           
            ref int UT_TT, ref int Iref, [Out] double[] Times, [Out] double[] RSTJD,[Out] double[] RTS_Angles);
        
            
        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_sun_earth_vector")]
        public static extern void Sun_Earth_Vector(ref double Tc, ref double R);

        
        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_moon_mean_long_lat_dist")]
        public static extern void Moon_Mean_Long_Lat_Dist(ref double T ,ref double Landa,ref double  Beta ,
            ref double Delta,ref double Lunar_Pi);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_hijrimonths")]
        public static extern void hijriMonths(ref int year, ref int month, ref int day,
            [MarshalAs(UnmanagedType.LPArray, SizeConst = 4)] double[] Geo, ref int UT_TT, ref int Method,
            ref double B_Ilum, ref int AidAccept, [Out] double[] NewMoonJD);

        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_moontransit")]
        public static extern void moonTransit(
            [MarshalAs(UnmanagedType.LPArray, SizeConst = 3)] int[] Jdate,ref double TJD,
           [MarshalAs(UnmanagedType.LPArray, SizeConst = 4)] double[] Geo,
           [MarshalAs(UnmanagedType.LPArray, SizeConst = 2)] double[] Atmos, ref int UT_TT,
            ref double MoonAngle,ref double TranJDout ,ref double Thour,ref double Televation);
       
        [DllImport("StaticMoonSunC.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__moonsun_MOD_yearmoonphases")]
        public static extern void FyearMoonPhases(ref int year, ref int month, ref int day, [Out] double[] moonPhaseJD);

        [DllImport("StaticMSISE.dll", CallingConvention = CallingConvention.Cdecl, EntryPoint = "__msise_MOD_gtd7tropo")]
        public static extern void MSIStropo(ref int DY, ref float ALT, ref float Glat, ref float Glong,
            ref float STL, ref float Density, ref float Temp, ref float Press);

        public static double[] stdAtmosTP(double latitude, int kd = 0 )
        {
            double[] Atmos = { 1010.0, 10.0 };
            double r = 0.0;
            double a = 0.0;
            double mu = 0.0;
            double ts = 0.0;
            double rr = 0.0;
            double pp = 0.0;
            double rm = 0.0;
            double qm = 0.0;
            double t = 0.0;
            double p = 0.0;
            int kk = 0;

           StdAtm(ref latitude, ref t, ref p, ref r, ref a, ref mu, ref ts, ref rr,
                ref pp, ref rm, ref qm, ref kd, ref kk);
            Atmos[1] = t - 273.15;
            Atmos[0] = p / 100.0;
            return Atmos;
        }

        public static double[] MSISEatm(int Id, double[] Geo)
        {
            float h = 5.0F;
            float alt = (float)Geo[2];
            float glat = (float)Geo[1];
            float glong = (float)Geo[0];
            float Density = 0.0F;
            float Temp = 0.0F;
            float Press = 0.0F;

            MSIStropo(ref Id, ref alt, ref glat, ref glong, ref h, ref Density, ref Temp, ref Press);

            double[] results = { Convert.ToDouble(Press), Convert.ToDouble(Temp) };

            return results;
        }

        public static int JD2WeekDayNum(double TJD)
        {
            int weekDay =  (Convert.ToInt32(TJD) + 2) % 7;
            return weekDay;
        }

        public static string WeekDays(int weekDay, int Language = 1)
        {
            string[] abreWeek = {"Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri" };
            string[] enWeek = {"Saturday","Sunday","Monday","Tuesday","Wednsday","Thursday","Friday"};
            string[] faWeek = {"شنبه", "یکشنبه","دوشنبه","سه شنبه","چهارشنبه","پنج شنبه","جمعه"};            

            switch (Language)
            {
                case 1:
                    return abreWeek[weekDay];
                case 2:
                    return string.Format(CultureInfo.CurrentCulture, "{0,9}", enWeek[weekDay]);
                case 3:
                    return string.Format(CultureInfo.CurrentCulture, "{0,9}", faWeek[weekDay]);
                default:
                    return abreWeek[weekDay];
            }
        }

        public static string JUL2WeekDay(double TJD, int Language=1)
        {
            string[] abreWeek = { "Sat", "Sun","Mon", "Tue", "Wed", "Thu", "Fri"};
            string[] enWeek = { "Saturday", "Sunday", "Monday", "Tuesday", "Wednsday", "Thursday", "Friday" };
            string[] faWeek = { "شنبه", "یکشنبه", "دوشنبه", "سه شنبه", "چهارشنبه", "پنج شنبه", "جمعه" };
                 
            int weekDay = JD2WeekDayNum(TJD);

            switch (Language)
            {
                case 1:
                    return abreWeek[weekDay];
                case 2:
                    return string.Format(CultureInfo.CurrentCulture,"{0,9}", enWeek[weekDay]);
                case 3:
                    return string.Format(CultureInfo.CurrentCulture,"{0,9}", faWeek[weekDay]);
                default:
                    return abreWeek[weekDay]; 
            }
        }

        public static string pesianMonthName(int monthNumber, int Language = 1)
        {

            string[] persianMonths = {
                "Farvardin", "Ordibehesht", "Khordad", "Tir",
                "Mordad", "Shahrivar  ", "Mehr", "Aban",
                "Azar", "Day", "Bahman", "Esphand"};

            string[] abrevMonths = {"FAR","ORD","KRD","TIR","MRD","SHA",
                                     "MER","ABN","AZR","DAY","BMN","SFN"};

            string[] farsiMonths = {"فروردین", "اردیبهشت", "خرداد", "تیر", "مرداد", "شهریور",
                "مهر", "آبان", "آذر", "دی", "بهمن", "اسفند"};

            switch (Language)
            {
                case 1:
                    return string.Format(CultureInfo.CurrentCulture, "{0,12}", persianMonths[monthNumber]);
                case 2:
                    return abrevMonths[monthNumber];
                case 3:
                    return string.Format(CultureInfo.CurrentCulture, "{0,12}", farsiMonths[monthNumber]);
                default:
                    return string.Format(CultureInfo.CurrentCulture, "{0,12}", persianMonths[monthNumber]);
            }
        }

        public static string gregoryMonthName(int monthNumber, int Language = 1)
        {

            string[] gregoryMonths = {
                "January", "February", "March", "April", "May",
                "June", "July", "August", "September", "October",
                "Novemer", "December"};

            string[] abrevMonths = {"JAN","FEB","MAR","APR","MAY","JUN",
                                   "JUL","AUG","SEP","OCT","NOV","DEC"};

            string[] farsiMonths = {"ژانویه", "فوریه", "مارس" , "آوریل", "مه",
                "ژون", "ژویه", "اوت", "سپتامبر", "اکتبر", "نوامبر", "دسامبر"};

            switch (Language)
            {
                case 1:
                    return string.Format(CultureInfo.CurrentCulture, "{0,9}", gregoryMonths[monthNumber]);
                case 2:
                    return abrevMonths[monthNumber];
                case 3:
                    return string.Format(CultureInfo.CurrentCulture, "{0,8}", farsiMonths[monthNumber]);
                default:
                    return string.Format(CultureInfo.CurrentCulture, "{0,9}", gregoryMonths[monthNumber]);
            }
        }


        public static string hijriMonthName(int monthNumber, int Language = 1)
        {

            string[] hijriMonths = {
                     "Muharram" , "Safar" , "Rabi-al-awwal" , 
                     "Rabi-ath-thani" , "Jumada-al-ula" , "Jumada-al-akhirah" ,
                     "Rajab" , "Shaban" , "Ramadan" ,
                     "Shawwal" , "Dhual-Qadah" , "Dhual-Hijjah"};

            string[] arabicMonths = {"محرم", "صفر", "ربیع الاول", "ربیع الثانی", "جمادی الاول","جمادی الاخر",
                "رجب", "شعبان", "رمضان", "شوال", "ذی القعده", "ذی الحجه"};

            switch (Language)
            {
                case 1:
                    return string.Format(CultureInfo.CurrentCulture, "{0,18}", hijriMonths[monthNumber]);
                case 2:
                    return string.Format(CultureInfo.CurrentCulture, "{0,12}", arabicMonths[monthNumber]);
                default:
                    return string.Format(CultureInfo.CurrentCulture, "{0,18}", hijriMonths[monthNumber]);
            }
        }


        public static string Visibility(int visiStat)
        {
            switch (visiStat)
            {
                case 1:
                    return "Age or Lag, Moon is not Visible!";
                case 2:
                    return "Moon is not bright enough, Not Visible!";
                case 10:
                    return "Moon is Visible!";
                case 11:
                    return "Moon is Visible under perfect conditions";
                case 12:
                    return "May need optical aid to find crescent";
                case 13:
                    return "Will need Optical aid to find crescent";
                case 14:
                    return "Not visible with telescope AECL <=8.5";
                case 15:
                    return "Moon is not Visible!";
                case 20:
                    return "Crescent is visible by naked eye";
                case 21:
                    return "Crescent is visible by optical aid";
                case 22:
                    return "Crescent is visible only by optical aid";
                case 23:
                    return "Crescent is not visible";
                default:
                    return "";
            }
        }

        public static void YearMoonPhases(int IYear,int Imonth,int Iday,double [][] MoonPhaseJD)
        {
            // returns array of Julian day of moon phases for one year

            double[] moonPhaseJD1 = new double[60];

            FyearMoonPhases(ref IYear, ref Imonth, ref Iday,  moonPhaseJD1);

            for(int i=0; i<=14; i++)
            {
                for(int j=0; j<=3; j++)
                {
                    MoonPhaseJD[i][j] = moonPhaseJD1[i + j * 15];
                }
            }

        }
            
          
    }
   
}
