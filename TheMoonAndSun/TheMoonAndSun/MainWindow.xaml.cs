using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.Globalization;
using System.Diagnostics;
using System.IO;
using ClassMoonSun;
using SunMoonUtility;
using System.Xml;

namespace TheMoonAndSun
{

    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {

        public MainWindow()
        {
           InitializeComponent();
            DateTime thisTime = DateTime.Now;
            year.Text = Convert.ToString(thisTime.Year, CultureInfo.CurrentCulture);
            month.Text = Convert.ToString(thisTime.Month, CultureInfo.CurrentCulture);
            day.Text = Convert.ToString(thisTime.Day, CultureInfo.CurrentCulture);
            LocalHour.Text = Convert.ToString(thisTime.Hour, CultureInfo.CurrentCulture);
            localMin.Text = Convert.ToString(thisTime.Minute, CultureInfo.CurrentCulture);
            localSec.Text = Convert.ToString(thisTime.Second, CultureInfo.CurrentCulture);

            int[] iranDate = Greg2Jalali();
            irYear.Text =  Convert.ToString(iranDate[0], CultureInfo.CurrentCulture);
            irMonth.Text = Convert.ToString(iranDate[1], CultureInfo.CurrentCulture);
            irDay.Text =   Convert.ToString(iranDate[2], CultureInfo.CurrentCulture);
            // UTC.Text = "UTC \r\n" +
            //     thisTime.ToUniversalTime().ToString("dd/MM/yyyy HH:mm:ss", CultureInfo.CurrentCulture);
    
            try
            {
                string[] geoLocation = System.IO.File.ReadAllLines("location.dat");
                Location.Text = geoLocation[0];
                Longitude.Text = geoLocation[1];
                Latitude.Text = geoLocation[2];
                Elevation.Text = geoLocation[3];
                timeZone.Text = geoLocation[4];
   
                if (geoLocation[5] == "0")
                {
                    gregCalendar.IsChecked = true;
                }
                else if (geoLocation[5] == "1")
                {
                    persCalendar.IsChecked = true;
                }

                if (geoLocation[6] == "0")
                {
                    dryAirSelect.IsChecked = true;
                }
                else if (geoLocation[6] == "1")
                {
                    wetAirSelect.IsChecked = true;
                }
                else if (geoLocation[6] == "2")
                {
                    noAirSelect.IsChecked = true;
                }

                tempBox.Text = geoLocation[8];
                pressBox.Text = geoLocation[9];

                double[] Geo = new double[3];
                Geo[0] = getLongitude();
                Geo[1] = getLatitude();
                Geo[2] = getAltitude();
                double[] atm = { 10.0, 1010.0 };

                if (geoLocation[7] == "0")
                {
                    msiseAtm.IsChecked = true;
                    atm = NativeMethods.MSISEatm(thisTime.DayOfYear, Geo);
                }
                else if (geoLocation[7] == "1")
                {
                    standardAtm.IsChecked = true;
                    int kd = 0;
                    atm = NativeMethods.stdAtmosTP(Geo[1], kd);
                }

                tempBox.Text = atm[1].ToString("F2", CultureInfo.CurrentCulture);
                pressBox.Text = atm[0].ToString("F2", CultureInfo.CurrentCulture);

                if (geoLocation[10] == "1") { aidAccept.IsChecked = true; }

                if (geoLocation[11] == "0")
                {
                    yallop.IsChecked = true;
                }
                else if (geoLocation[11] == "1")
                {
                    odeh.IsChecked = true;
                }
                else if (geoLocation[11] == "2")
                {
                    yallopOdeh.IsChecked = true;
                }

                if (geoLocation[12] == "12")
                {
                    DST.IsChecked = true;
                    selectRegion.Items.Add("Europe");
                    selectRegion.Items.Add("Iran");
                    selectRegion.Items.Add("North America");
                    selectRegion.Items.Add("Custom");
                    selectRegion.SelectedIndex = Utility.getInteger(geoLocation[12]);
                    startDST.Text = geoLocation[19];
                    endDST.Text = geoLocation[20];
                }

                System.IO.File.WriteAllLines(@"location.dat", geoLocation);

            }
            catch (IOException ex)
            {
                MessageBox.Show(ex.Message);
            }
     
        
            //   calculateItmes();        
        }


        private void calculateItmes()
        {

            const double C = 299792458.0;
            const double AUSEC = 499.0047838061;
            int UT_TT = 0;
            double[] Geo = getGeoLocation();
            string geoName = Location.Text;
            if (geoName.Length == 0) geoName = "---------";

            double GeoNDST = Geo[3];
            double GeoDST = Geo[3] + 1.0;

            calendarChange();

            DateTime thisDate = getDate();

            double NJD = julianDay();

            if ((bool)DST.IsChecked)
            {
                double[] dstJD = DSTJD();
                if (NJD <= dstJD[0] | NJD >= dstJD[1])
                {
                    Geo[3] = GeoNDST;
                }
                else
                {
                    Geo[3] = GeoDST;
                }
            }

            int Jy = 1;
            int Jm = 1;
            int Jd = 1;
            double T = 0.0;
            NativeMethods.JD2IrCal(ref NJD, ref Jy, ref Jm, ref Jd, ref T);

            string IrDayofYear = "Day of Iranian Year :  " + NativeMethods.dayofIyear(ref Jm, ref Jd).ToString(CultureInfo.CurrentCulture);

            double[] Atmos = { 1010.0, 10.0 };

            string atmModel = null;
            if ((bool)msiseAtm.IsChecked)
            {
                atmModel = " MSISE";
                Atmos = NativeMethods.MSISEatm(thisDate.DayOfYear, Geo);
            }
            else if ((bool)standardAtm.IsChecked)
            {
                atmModel = "Standard";
                int kd = 0;
                Atmos = NativeMethods.stdAtmosTP(Geo[1], kd);
            }

            tempBox.Text = Atmos[1].ToString("F2", CultureInfo.CurrentCulture);
            pressBox.Text = Atmos[0].ToString("F2", CultureInfo.CurrentCulture);

            int Iref = 1;
            if ((bool)noAirSelect.IsChecked)
            {
                Iref = 0;
            }

            int[] jDate = { thisDate.Year, thisDate.Month, thisDate.Day };
            double TJD = 0.0;
            double[] RTS_Angles = new double[3];
            double[] Times = new double[9];
            double[] RSTJD = new double[3];

            NativeMethods.AstroSolarTimes(ref TJD, jDate, Geo, Atmos, ref UT_TT, ref Iref, Times, RSTJD, RTS_Angles);

            double SunElev = 0.0;
            double Zenit = 0.0;
            double Azim = 0.0;
            double TopoAlfa = 0.0;
            double TopoDelta = 0.0;
            double UJD = NJD - Geo[3] / 24.0;

            NativeMethods.Solar_Position(ref UJD, Geo, Atmos, ref UT_TT, ref SunElev, ref Zenit, ref Azim, ref TopoAlfa, ref TopoDelta, ref Iref);
                                        
            string[] sTimes = new string[9];
            for (int i = 0; i <= 8; i++)
            {
                sTimes[i] = Utility.hour2Time(Times[i]);
            }

            string sunLocation = "";
            if (SunElev < -0.2665694)
            {
                sunLocation = String.Format(CultureInfo.CurrentCulture, "Sun is Below Horizon ({0:00.0000})", SunElev);
            }
            else
            {
                sunLocation = String.Format(CultureInfo.CurrentCulture, " Sun Altitude: {0:00.0000} \t Sun Azimuth: {1:000.000}", SunElev, Azim);
            }

            double Tcen = ((NJD - 2451545.0)) / 36525.0;
            double R = 0.0;

            NativeMethods.Sun_Earth_Vector(ref Tcen, ref R);
            double AU = AUSEC * C / 1000000000.0;

            double Earth_Sun_Vector = AU * R;

            double[] moonRTSJD = new double[4];
            double[] moonHours = new double[4];
            double[] moonAngles = new double[4];
            string[] moonEvents = new string[4];
            double DJD = NJD - NJD % 1.0 - 0.5;

            NativeMethods.Moon_RiseTranSet(jDate, DJD, Geo, Atmos, UT_TT, Iref, moonEvents, moonRTSJD, moonHours, moonAngles);
            double delta = 0.0;
            double alfa = 0.0;
            double TTJDT = 0.0;
            double MoonElev = 0.0;
            double MoonAzim = 0.0;
            NativeMethods.lunar_position(ref UJD, ref UT_TT, ref TTJDT, Geo, Atmos, ref MoonElev, ref MoonAzim, ref alfa, ref delta, ref Iref);
            double GeoDia = 0.0;
            double TopoDia = 0.0;
            NativeMethods.MoonSemiDia(ref UJD, ref MoonElev, ref GeoDia, ref TopoDia);
            double Ilum_Ratio = 0.0;
            double hour = 0.0;
            NativeMethods.Moon_IlumRatio(jDate, ref hour, ref UJD, ref UT_TT, ref Ilum_Ratio);

            string moonLocation = "";

            if (MoonElev < TopoDia)
            {
                moonLocation = String.Format(CultureInfo.CurrentCulture, "Moon is Below Horizon ({0:00.000})", MoonElev);
            }
            else
            {
                moonLocation = String.Format(CultureInfo.CurrentCulture, "Moon Altitude: {0:00.000} \t Moon Azimuth: {1:000.000}", MoonElev, MoonAzim);
            }

            Position.Text = "Position of " + sunLocation + "\r\n" + "Position of " + moonLocation;

            double Landa = 0.0;
            double beta = 0.0;
            double MoonDistance = 0.0;
            double LunPi = 0.0;

            NativeMethods.Moon_Mean_Long_Lat_Dist(ref Tcen, ref Landa, ref beta, ref MoonDistance, ref LunPi);

            double B_Ilum = 0.0035;

            int Method = 3;

            if ((bool)yallop.IsChecked)
            {
                Method = 1;
            }
            else if ((bool)odeh.IsChecked)
            {
                Method = 2;
            }
            else if ((bool)yallopOdeh.IsChecked)
            {
                Method = 3;
            }


            if ((bool)dryAirSelect.IsChecked)
            {
                B_Ilum = 0.0035;
            }
            else if ((bool)wetAirSelect.IsChecked)
            {
                B_Ilum = 0.005;
            }

            int aidAccepted = 0;            

            if ((bool)aidAccept.IsChecked)
            {
                aidAccepted = 1;
            }

            double[][] MoonPhaseJD = new double[14][];
            for(int i = 0;i <= 13; i++)
            {
                MoonPhaseJD[i] = new double[4];               
            }

            NativeMethods.YearMoonPhases(jDate[0], 1, 15, MoonPhaseJD);

           
            double AnewMoonJD = 0.0;
            double BnewMoonJD = 0.0;
            double AfullMoonJD = 0.0;
            double BfullMoonJD = 0.0;

            for (int i = 0; i <= 13; i++)
            {
                if (NJD <= MoonPhaseJD[i][0])
                {
                    AnewMoonJD = MoonPhaseJD[i][0];
                    break;
                }
                else
                {
                    BnewMoonJD = MoonPhaseJD[i][0];                    
                }
            }


            for (int i = 0; i <= 13; i++)
            {
                if (NJD <= MoonPhaseJD[i][2])
                {
                    AfullMoonJD = MoonPhaseJD[i][2];
                    break;
                }
                else
                {
                    BfullMoonJD = MoonPhaseJD[i][2];                  
                }
            }

            int daysinceNewMoon = Convert.ToInt32(NJD - BnewMoonJD);
            int daystoNextnewMoon = Convert.ToInt32(AnewMoonJD - NJD);

            int daysSinceFullMoon = Convert.ToInt32(NJD - BfullMoonJD);
            int daystoNextFullMoon = Convert.ToInt32(AfullMoonJD - NJD);

            double JD = NativeMethods.Cal2JD(ref jDate[0], ref jDate[1], ref jDate[2]);

            
            int Hy = 1;
            int Hm = 1;
            int Hd = 1;
            double NMJD = 0.0;
            double Hour = 0.0;

            NativeMethods.JD2TradHijri(ref JD, Geo, Atmos, ref UT_TT, ref Method, ref B_Ilum,
                ref aidAccepted, ref Iref, ref NMJD, ref Hy, ref Hm, ref Hd, ref Hour); 

            string hijridate = Hd.ToString("D2", CultureInfo.CurrentCulture) + " " + NativeMethods.hijriMonthName(Hm , 1) + " " + Hy.ToString("D4", CultureInfo.CurrentCulture);

            result.Text = " Location : " + geoName + "\t Longitude: " + Geo[0].ToString("F2", CultureInfo.CurrentCulture)
                 + "\t Latitude: " + Geo[1].ToString("F2", CultureInfo.CurrentCulture) + "\t Elevation: " + Geo[2].ToString("F2", CultureInfo.CurrentCulture) + "\r\n";
            result.AppendText(" Date is: " + NativeMethods.JUL2WeekDay(NJD, 2) + " " + thisDate.ToString("dd / MM / yyyy HH:mm:ss", CultureInfo.CurrentCulture)
             + "\t Iranian Calendar: " + Jd.ToString("D2", CultureInfo.CurrentCulture) + " " + NativeMethods.pesianMonthName(Jm - 1, 1) + " " + Jy.ToString("D4", CultureInfo.CurrentCulture) + "\r\n");
            result.AppendText(" Hijri Lunar Calendar: " + hijridate + "\t UT Julian Day: " + NJD.ToString("F4", CultureInfo.CurrentCulture) + "\r\n");
            result.AppendText(" Day of Gregory Year: " + (thisDate.DayOfYear).ToString("D3", CultureInfo.CurrentCulture) + " \t\t " + IrDayofYear + "\r\n");
            
            result.AppendText(atmModel + " Atmospheric model properies for refraction at this Location: \r\n");
            result.AppendText(" Atmospheric Pressure (millibar): " + Atmos[0].ToString("F2", CultureInfo.CurrentCulture) + ", \t "
             + "Temperature(C) :" + Atmos[1].ToString("F2", CultureInfo.CurrentCulture) + "\r\n");
           
            result.AppendText(" The Sun \r\n");
            result.AppendText("\t Rise Time: " + sTimes[3] + "\t Noon Time " + sTimes[4] + "\t  Set Time: " + sTimes[5] + "\r\n");
            result.AppendText("\t Rise Azimuth " + RTS_Angles[0].ToString("F3", CultureInfo.CurrentCulture) + "\t\t Altitude: " + RTS_Angles[1].ToString("F3", CultureInfo.CurrentCulture)
             + "\t Set Azimuth: " + RTS_Angles[2].ToString("F3", CultureInfo.CurrentCulture) + "\r\n");
            result.AppendText("\t" + sunLocation + "\t Sun Earth Distance: " + Earth_Sun_Vector.ToString("F3", CultureInfo.CurrentCulture) + " million kilometers \r\n");
            result.AppendText(" Twilight Times \r\n");
            result.AppendText(" Morning  Astronomical: " + sTimes[0] + "\t Nautical: " + sTimes[1] + "\t Civic: " + sTimes[2] + "\r\n");
            result.AppendText(" Evening  Astronomical: " + sTimes[8] + "\t Nautical: " + sTimes[7] + "\t Civic: " + sTimes[6] + "\r\n");

            result.AppendText(" The Moon \r\n");
            for(int k = 0; k<= 3; k++)
            {
                if (moonRTSJD[k] != 0.0)
                {                   
                    result.AppendText("\t" + moonEvents[k]);
                }
                if (k == 3)
                {
                    result.AppendText("\r\n");
                }
            }
            
            result.AppendText(" Current Moon Position:" + moonLocation + "\t  Moon illumination: " 
                + Ilum_Ratio.ToString("F4", CultureInfo.CurrentCulture) + "\r\n");
            result.AppendText(" Moon Earth Distance: " + MoonDistance.ToString("F3", CultureInfo.CurrentCulture) 
                + " kilometers \r\n");
            result.AppendText(" Days since last new Moon: " + daysinceNewMoon.ToString("D2", CultureInfo.CurrentCulture) 
                + "\t Days to next new Moon: " + daystoNextnewMoon.ToString("D2", CultureInfo.CurrentCulture) + "\r\n");
            result.AppendText(" Days since last Full Moon: " + daysSinceFullMoon.ToString("D2", CultureInfo.CurrentCulture) 
                + "\t Days to next Full Moon: " + daystoNextFullMoon.ToString("D2", CultureInfo.CurrentCulture) + "\r\n");

            if ((bool)DST.IsChecked)
            {
                double[] dstJD = DSTJD();
                result.AppendText(dstJD[0].ToString(CultureInfo.CurrentCulture)+ " "+ dstJD[1].ToString(CultureInfo.CurrentCulture));               
            }
        }



        private double getHour()
        {

            double hour = Convert.ToDouble(LocalHour.Text, CultureInfo.CurrentCulture);
            try
            {
                hour = hour + Convert.ToDouble(localMin.Text, CultureInfo.CurrentCulture) / 60.0
                    + Convert.ToDouble(localSec.Text, CultureInfo.CurrentCulture) / 3600.0;
            }
            catch (FormatException fEx)
            {
                MessageBox.Show(fEx.Message);
            }
            catch (OverflowException ex)
            {
                MessageBox.Show(ex.Message);
            }

            return hour;
        }

        private double getLongitude()
        {
            double longi = Utility.getDouble(Longitude.Text);
            Utility.getDoubleArg(longi, -180.0, 180.0, "Longitude");
            return longi;
        }

        private double getLatitude()
        {
            double lati = Utility.getDouble(Latitude.Text);
            Utility.getDoubleArg(lati, -90.0, 90.0, "Latitude");
            return lati;
        }

        private double getAltitude()
        {
            double elev = Utility.getDouble(Elevation.Text);
            Utility.getDoubleArg(elev, -29.0, 9000.0, "Elevation");
            return elev;
        }
        private double getTimeZone()
        {
            double tz = Utility.getDouble(timeZone.Text);
            Utility.getDoubleArg(tz, -12.0, 12.0, "Time Zone");
            return tz;
        }

        private double[] getGeoLocation()
        {
            double[] Geo = new double[4];
            Geo[0] = getLongitude();
            Geo[1] = getLatitude();
            Geo[2] = getAltitude();
            Geo[3] = getTimeZone();
            return Geo;
        }

        private int getYear()
        {
            int y = Utility.getInteger(year.Text);
            Utility.getIntArg(y, 1, 3000, "Year");
            return y;
        }

        private int getMonth()
        {
            int m = Utility.getInteger(month.Text);
            Utility.getIntArg(m, 1, 12, "Month");
            return m;
        }

        private int getIrYear()
        {
            int y = Utility.getInteger(irYear.Text);
            Utility.getIntArg(y, 1, 2000, "Year");
            return y;
        }

        private int getIrMonth()
        {
            int m = Utility.getInteger(irMonth.Text);
            Utility.getIntArg(m, 1, 12, "Month");
            return m;
        }

        private int getDay()
        {
            int d = Utility.getInteger(day.Text);
            int Iyear = getYear();
            int Imonth = getMonth();
            int days = DateTime.DaysInMonth(Iyear, Imonth);
            Utility.getIntArg(d, 1, days, "Day in month");
            return d;
        }

        private int getIrDay()
        {
            int d = Utility.getInteger(irDay.Text);
            int Iyear = getIrYear();
            int Imonth = getIrMonth();
            int[] days = { 31, 31, 31, 31, 31, 31, 30, 30, 30, 30, 30, 29 };
            int Gy = 0;
            int[] Equinox = new int[3];
            bool Leap = false;
            int Marday = 0;
            double Uhour = 0.0;
            double NJD = 0.0;

            NativeMethods.IranCalendar(ref Iyear, ref Gy, ref NJD, ref Leap, Equinox, ref Marday, ref Uhour);
            if (Leap) days[11] = 30;
            Utility.getIntArg(d, 1, days[Imonth - 1], "Day in month");
            return d;
        }

        private double julianDay()
        {
            DateTime dateIn = getDate();
            int Y = dateIn.Year;
            int M = dateIn.Month;
            int D = dateIn.Day;
            double H = getHour();
            double NTJD = NativeMethods.Cal2JDH(Y, M, D, H);
            return NTJD;
        }


        private int[] getTime()
        {

            int H = Utility.getInteger(LocalHour.Text);
            Utility.getIntArg(H, 0, 24, "Hours");

            int M = Utility.getInteger(localMin.Text);
            Utility.getIntArg(M, 0, 60, "Minutes");


            int S = Utility.getInteger(localSec.Text);
            Utility.getIntArg(S, 0, 60, "Seconds");

            int[] thisTime = { H, M, S };
            return thisTime;
        }

        private DateTime getDate()
        {
            int Y = getYear();
            int M = getMonth();
            int D = getDay();
            int[] time = getTime();
            DateTime thisDate = new DateTime();
            thisDate = new DateTime(Y, M, D, time[0], time[1], time[2]);
            return thisDate;
        }

        private double[] getAtm(double[] Geo, int Dy = 1)
        {
            double[] atm = { 10.0, 1010.0 };

            if ((bool)msiseAtm.IsChecked)
            {
                atm = NativeMethods.MSISEatm(Dy, Geo);
            }
            else
            {
                int kd = 0;
                atm = NativeMethods.stdAtmosTP(Geo[2], kd);
                atm[0] = Utility.getDouble(pressBox.Text);
                atm[1] = Utility.getDouble(tempBox.Text);

            }

            return atm;
        }

        private int[] Greg2Jalali()
        {
            int Y = getYear();
            int M = getMonth();
            int D = getDay();
            int Jy = 1;
            int Jm = 1;
            int Jd = 1;

            NativeMethods.GregCal2IrCal(ref Y, ref M, ref D, ref Jy, ref Jm, ref Jd);
            int[] time = getTime();
            int[] jalaliDate = { Jy, Jm, Jd, time[0], time[1], time[2] };

            return jalaliDate;
        }

        private DateTime Jalali2Greg()
        {
            PersianCalendar persian = new PersianCalendar();
            DateTime thisDate = new DateTime();

            int Y = getIrYear();
            int M = getIrMonth();
            int D = getIrDay();
            int[] time = getTime();

            thisDate = new DateTime(Y, M, D, time[0], time[1], time[2], persian);

            return thisDate;
        }

        private double[] DSTJD()
        {
            int y = getYear();
            int Jy = getIrYear();
            int sMonth = 3;
            int day = 1;
            double hour = 0.0;
            int sDay = 1;
            int eDay = 1;
            int eMonth = 1;
            int sJm = 1;
            int sJd = 1;
            int eJm = 6;
            int eJd = 31;
            double SJD = 0.0;
            double EJD = 0.0;
            double H = 0.0;

            if (selectRegion.SelectedIndex == 0)
            {
                SJD = NativeMethods.Cal2JDH(y, sMonth, day, hour);
                sDay = -Convert.ToInt32(SJD + 1.5) % 7 + 28;
                eMonth = 10;
                EJD = NativeMethods.Cal2JDH(y, eMonth, day, hour);
                eDay = -Convert.ToInt32(EJD + 1.5) % 7 + 28;
                SJD = SJD + sDay;
                EJD = EJD + eDay;
            }
            else if (selectRegion.SelectedIndex == 1)
            {
                if ((bool)gregCalendar.IsChecked)
                {
                    sDay = 22;
                    SJD = NativeMethods.Cal2JD(ref y, ref sMonth, ref sDay);
                    eMonth = 9;
                    eDay = 22;
                    EJD = NativeMethods.Cal2JD(ref y, ref eMonth, ref eDay);
                }
                else if ((bool)persCalendar.IsChecked)
                {
                    SJD = NativeMethods.IrCal2JD(ref Jy, ref sJm, ref sJd, ref H);
                    EJD = NativeMethods.IrCal2JD(ref Jy, ref eJm, ref eJd, ref H);
                }
            }
            else if (selectRegion.SelectedIndex == 2)
            {
                SJD = NativeMethods.Cal2JDH(y, sMonth, day, hour);
                sDay = -Convert.ToInt32(SJD + 1.5) % 7 + 14;
                eMonth = 11;
                EJD = NativeMethods.Cal2JDH(y, eMonth, day, hour);
                eDay = -Convert.ToInt32(EJD + 1.5) % 7 + 7;
                SJD = SJD + sDay;
                EJD = EJD + eDay;
            }

            double[] DST = { SJD, EJD };
            return DST;
        }

        private void DSTChecked(object sender, RoutedEventArgs e)
        {
            if ((bool)DST.IsChecked == true)
            {
                selectRegion.Items.Add("Europe");
                selectRegion.Items.Add("Iran");
                selectRegion.Items.Add("North America");
            }
            else if ((bool)DST.IsChecked == false)
            {
                selectRegion.Items.Clear();
            }
        }


        private void showClick(object sender, RoutedEventArgs e)
        {
            calculateItmes();
        }

        private void selectRegion_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            if ((bool)DST.IsChecked == true)
            {

                double[] dstJD = DSTJD();
                double SJD = dstJD[0];
                double EJD = dstJD[1];

                int sYear = 1;
                int eYear = 1;
                int sMonth = 1;
                int eMonth = 1;
                int sDay = 1;
                int eDay = 1;
                double FD = 0.0;

                if ((bool)gregCalendar.IsChecked)
                {
                    NativeMethods.JD2Cal(ref SJD, ref sYear, ref sMonth, ref sDay, ref FD);
                    NativeMethods.JD2Cal(ref EJD, ref eYear, ref eMonth, ref eDay, ref FD);
                }
                else if ((bool)persCalendar.IsChecked)
                {
                    NativeMethods.JD2IrCal(ref SJD, ref sYear, ref sMonth, ref sDay, ref FD);
                    NativeMethods.JD2IrCal(ref EJD, ref eYear, ref eMonth, ref eDay, ref FD);
                }

                startDST.Text = sYear.ToString(CultureInfo.CurrentCulture) + "/"
                    + sMonth.ToString(CultureInfo.CurrentCulture) + "/" + sDay.ToString(CultureInfo.CurrentCulture);
                endDST.Text = eYear.ToString(CultureInfo.CurrentCulture) + "/" + eMonth.ToString(CultureInfo.CurrentCulture) + "/" + eDay.ToString(CultureInfo.CurrentCulture);

            }
            else if ((bool)DST.IsChecked == false)
            {
                startDST.Text = null;
                endDST.Text = null;
            }

        }

        private void savetoFile(object sender, RoutedEventArgs e)
        {

            // WriteAllText creates a file, writes the specified string to the file,
            // and then closes the file.    You do NOT need to call Flush() or Close().
            // System.IO.File.WriteAllText(@"C:\Works\MoonSun\MoonSunText.txt", result.Text);

            Microsoft.Win32.SaveFileDialog saveFileDialog1 = new Microsoft.Win32.SaveFileDialog();
            saveFileDialog1.FileName = "MoonSun"; // Default file name
            saveFileDialog1.DefaultExt = ".text"; // Default file extension
            saveFileDialog1.Filter = "Text documents (.txt)|*.txt"; // Filter files by extension

            // Show save file dialog box
            Nullable<bool> resultBox = saveFileDialog1.ShowDialog();

            // Process save file dialog box results
            if (resultBox == true)
            {
                // Save document
                string filename = saveFileDialog1.FileName;
                System.IO.File.WriteAllText(saveFileDialog1.FileName, result.Text);

            }

        }

        private void closeWindow(object sender, RoutedEventArgs e)
        {
            this.Close();
        }

        private void saveLocation(object sender, RoutedEventArgs e)
        {
            string[] geoLocation = new string[16];
            geoLocation[0] = Location.Text;
            geoLocation[1] = Longitude.Text;
            geoLocation[2] = Latitude.Text;
            geoLocation[3] = Elevation.Text;
            geoLocation[4] = timeZone.Text;

            if ((bool)gregCalendar.IsChecked)
            {
                geoLocation[5] = (0).ToString(CultureInfo.CurrentCulture);
            }
            else if ((bool)persCalendar.IsChecked)
            {
                geoLocation[5] = (1).ToString(CultureInfo.CurrentCulture);
            }

            if ((bool)dryAirSelect.IsChecked)
            {
                geoLocation[6] = (0).ToString(CultureInfo.CurrentCulture);
            }
            else if ((bool)wetAirSelect.IsChecked)
            {
                geoLocation[6] = (1).ToString(CultureInfo.CurrentCulture);
            }
            else if ((bool)noAirSelect.IsChecked)
            {
                geoLocation[6] = (2).ToString(CultureInfo.CurrentCulture);
            }

            if ((bool)msiseAtm.IsChecked)
            {
                geoLocation[7] = (0).ToString(CultureInfo.CurrentCulture);
            }
            else if ((bool)standardAtm.IsChecked)
            {
                geoLocation[7] = (1).ToString(CultureInfo.CurrentCulture);
            }

            geoLocation[8] = tempBox.Text;
            geoLocation[9] = pressBox.Text;

            if ((bool)aidAccept.IsChecked)
            {
                geoLocation[10] = (1).ToString(CultureInfo.CurrentCulture);
            }
            else
            {
                geoLocation[10] = (0).ToString(CultureInfo.CurrentCulture);
            }

            if ((bool)yallop.IsChecked)
            {
                geoLocation[11] = (0).ToString(CultureInfo.CurrentCulture);
            }
            else if ((bool)odeh.IsChecked)
            {
                geoLocation[11] = (1).ToString(CultureInfo.CurrentCulture);
            }
            else if ((bool)yallopOdeh.IsChecked)
            {
                geoLocation[11] = (2).ToString(CultureInfo.CurrentCulture);
            }


            if (DST.IsChecked == true)
            {
                geoLocation[12] = (1).ToString(CultureInfo.CurrentCulture);
                geoLocation[13] = selectRegion.SelectedIndex.ToString(CultureInfo.CurrentCulture);
                geoLocation[14] = startDST.Text;
                geoLocation[15] = endDST.Text;
            }
            else
            {
                geoLocation[12] = (0).ToString(CultureInfo.CurrentCulture);
            }

            System.IO.File.WriteAllLines(@"location.dat", geoLocation);

        }

        private void clearFile(object sender, RoutedEventArgs e)
        {
            string[] geoLocation = new string[17];
            string startUP = (0.0).ToString(CultureInfo.CurrentCulture);
            for (int i = 0; i <= 4; i++)
            {
                geoLocation[i] = startUP;
            }
            System.IO.File.WriteAllLines(@"location.dat", geoLocation);
            Location.Text = geoLocation[0];
            Longitude.Text = geoLocation[1];
            Latitude.Text = geoLocation[2];
            Elevation.Text = geoLocation[3];
            timeZone.Text = geoLocation[4];

        }

        private void HelpClick(object sender, RoutedEventArgs e)
        {
            Window w1 = new SunMoon.Window1();
            w1.Show();
        }

        private void moonPhase(object sender, RoutedEventArgs e)
        {
            SunMoon.Window2 w2 = new SunMoon.Window2();
            w2.Show();
            double[] Geo = new double[4];
            string geoName = Location.Text;
            if (geoName.Length == 0) geoName = "---------";
            Geo[0] = getLongitude();
            Geo[1] = getLatitude();
            Geo[2] = getAltitude();
            Geo[3] = getTimeZone();

            calendarChange();
            int ye = getYear();

            string yearText = year.Text;
            if ((bool)persCalendar.IsChecked)
            {
                yearText = irYear.Text;
                ye = ye - 1;
            }

            double GeoNDST = Geo[3];
            double GeoDST = Geo[3] + 1.0;

            int newMon = 1;
            int day = 15;
            if ((bool)persCalendar.IsChecked)
            { 
                newMon = 3;
                day = 21;
            }

            double[][] moonPhaseJD = new double[14][];
            for (int i = 0; i <= 13; i++)
            {
                moonPhaseJD[i] = new double[4];
            }            

            NativeMethods.YearMoonPhases(ye, newMon, day, moonPhaseJD);

            double JDM = 0.0;
            int Iyear = 1;
            int Imonth = 1;
            int Iday = 1;

            string[] gregDate = new string[4];
            string[] persDate = new string[4];
            string[] time = new string[4];
            double hour = 0.0;

            w2.moonPhaseResult.AppendText("\t\t Moon Phases For Year " + yearText + "\r\n");
            w2.moonPhaseResult.AppendText("=============================================================\r\n");
            w2.moonPhaseResult.AppendText("Location: " + geoName + ", Longitude: " + Geo[0].ToString("F2", CultureInfo.CurrentCulture)
                 + " \t Latitude: " + Geo[1].ToString("F2", CultureInfo.CurrentCulture) + "\t Elevation: " +
                 Geo[2].ToString("F2", CultureInfo.CurrentCulture) + "\r\n");
            w2.moonPhaseResult.AppendText("=============================================================\r\n");
            w2.moonPhaseResult.AppendText("\t\t New Moon \t First Quarter \t Full Moon \t  Last Quarter \r\n");

            string moonPhase = null;
            for (int i = 0; i <= 13; i++)
            {
                for (int j = 0; j <= 3; j++)
                {

                    if ((bool)DST.IsChecked)
                    {
                        double[] dstJD = DSTJD();
                        if (moonPhaseJD[i][j] <= dstJD[0] | moonPhaseJD[i][j] >= dstJD[1])
                        {
                            Geo[3] = GeoDST;
                        }
                        else
                        {
                            Geo[3] = GeoNDST;
                        }
                    }

                    JDM = moonPhaseJD[i][j] + Geo[3] / 24.0;
                    Iyear = 1;
                    Imonth = 1;
                    Iday = 1;
                    NativeMethods.JD2Cal(ref JDM, ref Iyear, ref Imonth, ref Iday, ref hour);
                    gregDate[j] = String.Format(CultureInfo.CurrentCulture, "\t " + Iyear.ToString(CultureInfo.CurrentCulture) + "/" +
                        Imonth.ToString(CultureInfo.CurrentCulture) + "/" + Iday.ToString(CultureInfo.CurrentCulture));

                    NativeMethods.JD2IrCal(ref JDM, ref Iyear, ref Imonth, ref Iday, ref hour);
                    persDate[j] = String.Format(CultureInfo.CurrentCulture, "\t " + Iyear.ToString(CultureInfo.CurrentCulture)
                        + "/" + Imonth.ToString(CultureInfo.CurrentCulture) + "/" + Iday.ToString(CultureInfo.CurrentCulture));

                    time[j] = Utility.hour2Time(hour);
                }
                moonPhase = String.Format(CultureInfo.CurrentCulture, "Julian day(UTC)\t {0:F3} \t {1:F3} \t {2:F3} \t {3:F3} \r\n",
                    moonPhaseJD[i][0], moonPhaseJD[i][1], moonPhaseJD[i][2], moonPhaseJD[i][3]);

                w2.moonPhaseResult.AppendText(moonPhase);
                w2.moonPhaseResult.AppendText("Gregory Date " + gregDate[0] + gregDate[1] + gregDate[2] + gregDate[3] + "\r\n");
                w2.moonPhaseResult.AppendText("Iranian Date " + persDate[0] + persDate[1] + persDate[2] + persDate[3] + "\r\n");
                w2.moonPhaseResult.AppendText("Local Time \t " + time[0] + " \t " + time[1] + " \t " + time[2] + " \t " + time[3] + "\r\n");
                w2.moonPhaseResult.AppendText("\r\n");

                newMon = newMon + 1;
            }

        }

        private void hijriClick(object sender, RoutedEventArgs e)
        {
            SunMoon.Window3 w3 = new SunMoon.Window3();
            w3.Show();
            double[] Geo = getGeoLocation();
            string geoName = Location.Text;
            if (geoName.Length == 0) geoName = "---------";

            calendarChange();

            int ye = getYear();
            string yearText = year.Text;
            if ((bool)persCalendar.IsChecked)
            {
                yearText = irYear.Text;
                ye = ye - 1;
            }

            double[] Atmos = getAtm(Geo);

            int Method = 3;            

            if ((bool)yallop.IsChecked)
            {
                Method = 1;             
            }
            else if ((bool)odeh.IsChecked)
            {
                Method = 2;               
            }
            else if ((bool)yallopOdeh.IsChecked)
            {
                Method = 3;              
            }

            int Iref = 1;
            if ((bool)noAirSelect.IsChecked)
            {
                Iref = 0;
            }

            double B_Ilum = 0.0035;

            string airConditin = null;
            if ((bool)dryAirSelect.IsChecked)
            {
                B_Ilum = 0.0035;
                airConditin = "Air : Dry, Minimum Moon Ilumination = " + B_Ilum.ToString("P2", CultureInfo.CurrentCulture);
            }
            else if ((bool)wetAirSelect.IsChecked)
            {
                B_Ilum = 0.005;
                airConditin = "Air : Humid, Minimum Moon Ilumination = " + B_Ilum.ToString("P2", CultureInfo.CurrentCulture);
            }

            int aidAccepted = 0;
            string aidString = null;

            if ((bool)aidAccept.IsChecked)
            {
                aidAccepted = 1;
                aidString = "New Moon sighting with aid is accepted.";
            }
            else
            {
                aidString = "New Moon sighting only with naked eye.";
            }

            int UT_TT = 0;
            double GeoNDST = Geo[3];
            double GeoDST = Geo[3] + 1.0;
                                                                      
            
            w3.hijriMonths.AppendText("\t\t\t Hijri Months For Year " + yearText + "\r\n");
            w3.hijriMonths.AppendText("=================================================================\r\n");
            w3.hijriMonths.AppendText("Location: " + geoName + ", Longitude: " + Geo[0].ToString("F2", CultureInfo.CurrentCulture)
                 + " \t Latitude: " + Geo[1].ToString("F2", CultureInfo.CurrentCulture) + "\t Elevation: " + Geo[2].ToString("F2", CultureInfo.CurrentCulture)
                 + ", Time Zone: " + Geo[3].ToString("F2", CultureInfo.CurrentCulture) + "\r\n");
            w3.hijriMonths.AppendText("\r\n");
            w3.hijriMonths.AppendText(airConditin + ", " + aidString + "\r\n");         
            w3.hijriMonths.AppendText("=================================================================\r\n");
            w3.hijriMonths.AppendText("\r\n");

            double[] MoonJD = new double[14];
            long[] newMoonDate = new long[28];
         
            int day = 15;
            int newMoonMon = 1;
            if ((bool)persCalendar.IsChecked)
            {
                newMoonMon = 3;
                day = 21;
            }

            double hour = 0.0;

            NativeMethods.FhijriMonths(ref ye,ref newMoonMon,ref day, Geo, Atmos,ref UT_TT,ref Method,ref B_Ilum,ref  aidAccepted, 
                ref Iref, MoonJD, newMoonDate);

            int Iday = 1;
            int Imonth = 1;
            int Iyear = 1; 

            for (int i = 0; i <= 13; i++)
            {         
                double HJD = MoonJD[i];
                int Hm = (int)newMoonDate[i + 14];
                string hijriDate = String.Format(CultureInfo.CurrentCulture, "First of " +
                    NativeMethods.hijriMonthName(Hm-1,1) + " " + newMoonDate[i].ToString(CultureInfo.CurrentCulture));
                NativeMethods.JD2Cal(ref HJD, ref Iyear, ref Imonth, ref Iday, ref hour);
                string gregHijri = String.Format(CultureInfo.CurrentCulture, Iyear.ToString(CultureInfo.CurrentCulture) + "/" +
                    Imonth.ToString(CultureInfo.CurrentCulture) + "/" + Iday.ToString(CultureInfo.CurrentCulture));
                NativeMethods.JD2IrCal(ref HJD, ref Iyear, ref Imonth, ref Iday, ref hour);
                string persHijri = String.Format(CultureInfo.CurrentCulture, Iyear.ToString(CultureInfo.CurrentCulture) + "/" +
                    Imonth.ToString(CultureInfo.CurrentCulture) + "/" + Iday.ToString(CultureInfo.CurrentCulture));

                w3.hijriMonths.AppendText(hijriDate + " \t Gregory: " + gregHijri + "\t Iranian: " + persHijri + "\r\n");                
                w3.hijriMonths.AppendText("\r\n");                
            }

        }

        private void MoonCalendar(object sender, RoutedEventArgs e)
        {
            SunMoon.Window4 w4 = new SunMoon.Window4();
            if ((bool)persCalendar.IsChecked == false && (bool)gregCalendar.IsChecked == false)
            {
                w4.waitWarn.Content = "Please first select a calendar";
            }
            else
            {
                w4.waitWarn.Content = "Please Wait....";
            }
            w4.Show();
            int[] daysIranmonth = { 31, 31, 31, 31, 31, 31, 30, 30, 30, 30, 30, 29 };
            int[] daysGregmonth = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

            double[] Geo = getGeoLocation();
            string geoName = Location.Text;
            if (geoName.Length == 0) geoName = "---------";

            calendarChange();

            double[] Atmos = getAtm(Geo);

            double B_Ilum = 0.0035;

            int Method = 3;
            string method = null;

            if ((bool)yallop.IsChecked)
            {
                Method = 1;
                method = "New Moon Sighting Critera: Yallop";
            }
            else if ((bool)odeh.IsChecked)
            {
                Method = 2;
                method = "New Moon Sighting Critera: Odeh";
            }
            else if ((bool)yallopOdeh.IsChecked)
            {
                Method = 3;
                method = "New Moon Sighting Critera: Yallop or Odeh";
            }

            string airConditin = null;
            if ((bool)dryAirSelect.IsChecked)
            {
                B_Ilum = 0.0035;
                airConditin = "Air : Dry, Minimum Moon Ilumination = " + B_Ilum.ToString("P2", CultureInfo.CurrentCulture);
            }
            else if ((bool)wetAirSelect.IsChecked)
            {
                B_Ilum = 0.005;
                airConditin = "Air : Humid, Minimum Moon Ilumination = " + B_Ilum.ToString("P2", CultureInfo.CurrentCulture);
            }

            int aidAccepted = 0;
            string aidString = null;

            if ((bool)aidAccept.IsChecked)
            {
                aidAccepted = 1;
                aidString = "New Moon sighting with aid is accepted.";
            }
            else
            {
                aidString = "New Moon sighting only with naked eye.";
            }

            int Iref = 1;
            if ((bool)noAirSelect.IsChecked)
            {
                Iref = 0;
            }

            double[] dstJD = new double[2];
            if ((bool)DST.IsChecked)
            {
                dstJD = DSTJD();
            }

            int year1 = getYear();
            string yearText = year.Text;
            if ((bool)persCalendar.IsChecked)
            {
                yearText = irYear.Text;
                year1 = year1 - 1;
            }

            int k = 0;
            double JD = NativeMethods.TrueJDEquiSolitice(ref year1, ref k);

            int Iyear = 1;
            int Imonth = 1;
            int Iday = 1;
            double FD = 0.0;
            int J = 0;
            double DELTA_T = 0.0;
            NativeMethods.JD2Cal(ref JD, ref Iyear, ref Imonth, ref Iday, ref FD);
            NativeMethods.iau_DAT(ref Iyear, ref Imonth, ref Iday, ref FD, ref DELTA_T, ref J);           
            JD = JD + Geo[3] / 24.0 - (DELTA_T+32.184)/(24.0*3600);
            int Jyear = 1;
            int Jmonth = 1;
            int Jday = 1;
            double Hours = 0.0;
            NativeMethods.JD2IrCal(ref JD, ref Jyear, ref Jmonth, ref Jday, ref Hours);
            
            
            int UT_TT = 0;

            double UJD = 0.0;
            int Dy = 15;

            int Gyear = 0;
            double IUJD = 0.0;
            bool leap = false;
            int[] Equinox = new int[3];
            int MarDay = 20;
            double Uhour = 0.0;

            double[][] MoonJD = new double[14][];
            for (int i = 0; i <= 13; i++)
            {
                MoonJD[i] = new double[4];
            }

            double[] newHijriJD = new double[14];
            int[][] newMoonDate = new int[14][];
            for (int i = 0; i <= 13; i++)
            {
                newMoonDate[i] = new int[2];
            }

            int oYear = 1;
            int oMonth = 1;
            int oDay = 1;
            int dayInOtherMonth = 1;
            int dayInMonth = 1;            
            
            if ((bool)gregCalendar.IsChecked)
            {
                int Im = 1;
                int Id = 1;
                UJD = NativeMethods.Cal2JD(ref Iyear, ref Im, ref Id);
                oYear = 1;
                oMonth = 1;
                oDay = 1;
                double H = 0.0;
                NativeMethods.JD2IrCal(ref UJD, ref oYear, ref oMonth, ref oDay, ref H);
                if (NativeMethods.GregIsLeapYear(ref Iyear)) { daysGregmonth[1] = 29; }
                NativeMethods.IranCalendar(ref oYear, ref Gyear, ref IUJD, ref leap, Equinox, ref MarDay, ref Uhour);
                if (leap is true) { daysIranmonth[11] = 30; }
                NativeMethods.YearMoonPhases(Iyear, Im, 15, MoonJD);
                NativeMethods.HijriMonths(Iyear, Im, 15, Geo, Atmos, UT_TT, Method, B_Ilum, aidAccepted, Iref,
                    newHijriJD, newMoonDate);
            }
            else if ((bool)persCalendar.IsChecked)
            {
                int Jy = getIrYear();
                NativeMethods.IranCalendar(ref Jy, ref Gyear, ref IUJD, ref leap, Equinox, ref MarDay, ref Uhour);
                int Im = 1;
                int Id = 1;
                double H = 0.0;
                UJD = NativeMethods.IrCal2JD(ref Jy, ref Im, ref Id, ref H);
                NativeMethods.JD2Cal(ref UJD, ref oYear, ref oMonth, ref oDay, ref H);
                if (leap) daysIranmonth[11] = 30;
                if (NativeMethods.GregIsLeapYear(ref Gyear)) { daysGregmonth[1] = 29; }
                NativeMethods.YearMoonPhases(Gyear, oMonth, MarDay, MoonJD);
                NativeMethods.HijriMonths(Gyear, oMonth, MarDay, Geo, Atmos, UT_TT, Method, B_Ilum, aidAccepted, Iref,
                    newHijriJD, newMoonDate);
            }

            w4.MoonCaledar.AppendText(string.Format(CultureInfo.CurrentCulture, "{0,80}", " Moon Caledar For Year " + yearText + "\r\n"));
            w4.MoonCaledar.AppendText("======================================================================================"
                    + "===========================\r\n");
            w4.MoonCaledar.AppendText(" Location: " + geoName + ", Longitude: " + Geo[0].ToString("F2", CultureInfo.CurrentCulture) +
                " \t Latitude: " + Geo[1].ToString("F2", CultureInfo.CurrentCulture) + "\t Elevation: " +
                Geo[2].ToString("F2", CultureInfo.CurrentCulture) + ", Time Zone: " + Geo[3].ToString("F2", CultureInfo.CurrentCulture) + "\r\n");
            w4.MoonCaledar.AppendText(airConditin + ", " + aidString + "\t" + method + "\r\n");
            w4.MoonCaledar.AppendText(" New year at: " + Utility.hour2Time(Hours) + ", " + Jday.ToString(CultureInfo.CurrentCulture) + " " +
                NativeMethods.pesianMonthName(Jmonth - 1, 1) + " " + Jyear.ToString(CultureInfo.CurrentCulture)
                + " \t " + Iday.ToString(CultureInfo.CurrentCulture) + " " + NativeMethods.gregoryMonthName(Imonth - 1, 1) + " " +
                    Iyear.ToString(CultureInfo.CurrentCulture) + "\r\n");

            if ((bool)msiseAtm.IsChecked == false)
            {
                w4.MoonCaledar.AppendText(" Atmospheric properties for refraction: \r\n"
                        + "Pressure(millibar): " + Atmos[0].ToString("F2", CultureInfo.CurrentCulture) + ", " + "Temperature(C) :"
                        + Atmos[1].ToString("F2", CultureInfo.CurrentCulture));
            }

            double NMJD = 0.0;
            int Hy = 1;
            int Hm = 1;
            int Hd = 1;
            double h = 0.0;            

            NativeMethods.JD2TradHijri(ref UJD, Geo, Atmos,ref UT_TT, ref Method, ref B_Ilum,
                ref aidAccepted, ref Iref, ref NMJD, ref Hy, ref Hm, ref Hd, ref h);

            for(int km = 0; km<=3; km++)
            {
                if (newHijriJD[km] > NMJD) 
                {
                    k = km ;                    
                    break; 
                }
            }
            
            int[] jDate = { Iyear, 1, 1 };            

            double[][] mRTSJD = new double[3][];
            double[][] mRTS_Hours = new double[3][];
            double[][] mRTS_Angles = new double[3][];
            for(int i =0; i<=2; i++)
            {
                mRTSJD[i] = new double[3];
                mRTS_Hours[i] = new double[3];
                mRTS_Angles[i] = new double[3];
            }

            double DJD = UJD;
            double BJD = DJD - 1.0;
            double[] Geo1 = new double[4];           
            for (int l = 0; l <=3; l++ )
            {
                Geo1[l] = Geo[l]; 
            }
            
            NativeMethods.Moon_RTS(jDate ,ref BJD, Geo1, Atmos, ref UT_TT, ref Iref, mRTS_Hours[0], mRTSJD[0], mRTS_Angles[0]);
            
            NativeMethods.Moon_RTS(jDate, ref DJD, Geo1, Atmos, ref UT_TT, ref Iref, mRTS_Hours[1], mRTSJD[1], mRTS_Angles[1]);

            int weekDaynum = NativeMethods.JD2WeekDayNum(UJD + (Uhour + Geo1[3]) / 24.0);
            
            for (int i = 0; i < 12; i++)
            {
               string strOyear = null;
               string mainMonth = null;
               string strOmonth = null;
               string strHmonth = null;

                if ((bool)gregCalendar.IsChecked)
                {
                    mainMonth = NativeMethods.gregoryMonthName(i);
                    strOyear = oYear.ToString(CultureInfo.CurrentCulture);
                    dayInMonth = daysGregmonth[i];
                    dayInOtherMonth = daysIranmonth[oMonth - 1];
                }
                else if ((bool)persCalendar.IsChecked)
                {
                    mainMonth = NativeMethods.pesianMonthName(i);
                    strOyear = oYear.ToString(CultureInfo.CurrentCulture);
                    dayInMonth = daysIranmonth[i];
                    dayInOtherMonth = daysGregmonth[oMonth - 1];
                }
   
                string strHYear = Hy.ToString(CultureInfo.CurrentCulture);
   
                w4.MoonCaledar.AppendText(
                    " ================================================================================================"
                 + "==========================\r\n");
                w4.MoonCaledar.AppendText(string.Format(CultureInfo.CurrentCulture, "{0,60}", mainMonth + " " + yearText + "\r\n"));
   

                if ((bool)msiseAtm.IsChecked)
                {
                    if (Dy > 365) { Dy = Dy - 365; }
                    Atmos = getAtm(Geo1, Dy);

                    string strPres = Atmos[0].ToString("F2", CultureInfo.CurrentCulture);
                    string strTemp = Atmos[1].ToString("F2", CultureInfo.CurrentCulture);
                    string strAtm = string.Format(CultureInfo.CurrentCulture, "Pressure: {0}   Temperature: {1}",strPres,strTemp );
                    w4.MoonCaledar.AppendText(strAtm + "\r\n");

                    Dy = Dy + 30;
                }

                w4.MoonCaledar.AppendText("                                        " +
                       "                               Moon                         Moon                            Moon \r\n");
                w4.MoonCaledar.AppendText(" " + mainMonth + "        " + strOyear + "              " + strHYear
                        + "                       Time-Angle-Illumination           Time-Angle-Ilumination           Time-Angle-Illumination\r\n");
                w4.MoonCaledar.AppendText(
                         " --------------------------------------------------------------------------------------------"
                        + "-------------------------\r\n");

                for (int j = 1; j <= dayInMonth; j++)
                {
                    if (weekDaynum > 6) { weekDaynum = 0; }

                    if (oDay == 1 | j == 1)
                    {
                        if ((bool)gregCalendar.IsChecked)
                        {
                            strOmonth = NativeMethods.pesianMonthName(oMonth - 1, 1);
                        }
                        else if ((bool)persCalendar.IsChecked)
                        {
                            strOmonth = NativeMethods.gregoryMonthName(oMonth - 1, 1);
                        }
                    }
                    else
                    {
                        if ((bool)gregCalendar.IsChecked)
                        {
                            strOmonth = string.Format(CultureInfo.CurrentCulture, "{0,12}", "#  ");
                        }
                        else if ((bool)persCalendar.IsChecked)
                        {
                            strOmonth = string.Format(CultureInfo.CurrentCulture, "{0,9}", "#  ");
                        }
                    }
       
                    if (Hd == 1 | j == 1)
                    {
                        int km = k - 1;
                        if(km < 0) { km = 11; }
                        strHmonth = NativeMethods.hijriMonthName(newMoonDate[km][1]-1, 1);                     
                    }
                    else
                    {
                        strHmonth = string.Format(CultureInfo.CurrentCulture, "{0,18}", "#  ");
                    }
        
                    double AJD = DJD + 1.0;
                    if ((bool)DST.IsChecked)
                    {
                        if (AJD >= dstJD[0] ) 
                       {   
                            Geo1[3] = Geo[3] +1.0;
                        }
                        else if(AJD >= dstJD[1])
                       {                                                    
                          Geo1[3] = Geo[3];
                       }                        
                    }
                   
                    NativeMethods.Moon_RTS(jDate, ref AJD, Geo1, Atmos, ref UT_TT, ref Iref, mRTS_Hours[2], mRTSJD[2], mRTS_Angles[2]);
            
                    double[] moonRTSJD = new double[4];               
                    string[] moonEvents = new string[4];
               
                    NativeMethods.MoonRiseTranSet(DJD, mRTSJD, mRTS_Hours, mRTS_Angles, Geo1[3], moonEvents, moonRTSJD);

                    double hour = 0.0;
                    double IlumRatio = 0.0;
                    string [] mEvents = new string[4];                   

                    int ms = 0;

                    for (int il=0; il<=3; il++)
                    {
                        string strIlum = null;
                        mEvents[ms] = null;
                        if (moonRTSJD[il] != 0.0)
                        {
                            NativeMethods.Moon_IlumRatio(jDate, ref hour, ref moonRTSJD[il], ref UT_TT, ref IlumRatio);
                            strIlum = IlumRatio.ToString("0.0000", CultureInfo.CurrentCulture);
                            mEvents[ms] = String.Format(CultureInfo.CurrentCulture,"{0},{1}",moonEvents[il],strIlum);
                            ms = ms + 1;
                        }                                             
                    }

                    string MoonStat = "           ";

                    for (int m = i; m <= i + 2; m++)
                    {   
                        for (int n = 0; n <= 3; n++)
                        {                            
                            if (DJD == MoonJD[m][n]- MoonJD[m][n]%1.0-0.5 )
                            {
                                if (n == 0)
                                {
                                    MoonStat = "New Moon  ";
                                    break;
                                }
                                else if (n == 1)
                                {
                                    MoonStat = "First Half";
                                    break;
                                }
                                else if (n == 2)
                                {
                                    MoonStat = "Full Moon ";
                                    break;
                                }
                                else
                                {
                                    MoonStat = "Last Half ";
                                    break;
                                }
                            }
                        }
                    }

                    string strWekDy = NativeMethods.WeekDays(weekDaynum, 1);
                    string strNum = j.ToString("D2", CultureInfo.CurrentCulture);
                    string strOday = oDay.ToString("D2", CultureInfo.CurrentCulture);
                    string strHd = Hd.ToString("D2", CultureInfo.CurrentCulture);
                    string strCalendar = string.Format(CultureInfo.CurrentCulture, 
                        "{0} {1} {2},{3} {4},{5} {6}",strWekDy,strNum,strOmonth,strOday,strHmonth,strHd,MoonStat);
                   
                    w4.MoonCaledar.AppendText(strCalendar);

                    for (int lm=0; lm <ms; lm++)
                     {                        
                         w4.MoonCaledar.AppendText("\t" + mEvents[lm]);                      
                     }
                    
                    w4.MoonCaledar.AppendText("\r\n");

                    for(int n=0; n<=1; n++)
                    {
                        for(int l=0; l<=2; l++)
                        {
                            mRTSJD[n][l] = mRTSJD[n+1][l];
                            mRTS_Hours[n][l] = mRTS_Hours[n + 1][l];
                            mRTS_Angles[n][l] = mRTS_Angles[n + 1][l];
                        }
                    }
         
                    DJD = DJD + 1.0;
                    weekDaynum += 1;
                    oDay += 1;
                    if (oDay > dayInOtherMonth)
                    {
                        oDay = 1;
                        oMonth += 1;
                        if (oMonth > 12)
                        {
                            oMonth = 1;
                            oYear = (oYear + 1);
                        }
                    }
                    Hd += 1;
                    if (DJD == newHijriJD[k])
                    {
                        Hd = 1;
                        Hm = newMoonDate[k][1];
                        Hy = newMoonDate[k][0];
                        k = k + 1; 
                    }
                }
                w4.waitWarn.Content = null;
            }
        }

        private void SunCalendar(object sender, RoutedEventArgs e)
        {
            SunMoon.Window5 w5 = new SunMoon.Window5();

            if ((bool)persCalendar.IsChecked == false && (bool)gregCalendar.IsChecked == false)
            {
                w5.waitWarn.Content = "Please first select a calendar";

            }
            else
            {
                w5.waitWarn.Content = "Please Wait....";
            }

            w5.Show();
            int[] daysIranmonth = { 31, 31, 31, 31, 31, 31, 30, 30, 30, 30, 30, 29 };
            int[] daysGregmonth = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

            double[] Geo = getGeoLocation();
            string geoName = Location.Text;
            if (geoName.Length == 0) geoName = "---------";

            calendarChange();

            double[] Atmos = getAtm(Geo);

            double B_Ilum = 0.0035;

            int Method = 3;
            string method = null;

            if ((bool)yallop.IsChecked)
            {
                Method = 1;
                method = "New Moon Sighting Critera: Yallop";
            }
            else if ((bool)odeh.IsChecked)
            {
                Method = 2;
                method = "New Moon Sighting Critera: Odeh";
            }
            else if ((bool)yallopOdeh.IsChecked)
            {
                Method = 3;
                method = "New Moon Sighting Critera: Yallop or Odeh";
            }

            string airConditin = null;
            if ((bool)dryAirSelect.IsChecked)
            {
                B_Ilum = 0.0035;
                airConditin = "Air : Dry, Minimum Moon Ilumination = " + B_Ilum.ToString("P2", CultureInfo.CurrentCulture);
            }
            else if ((bool)wetAirSelect.IsChecked)
            {
                B_Ilum = 0.005;
                airConditin = "Air : Humid, Minimum Moon Ilumination = " + B_Ilum.ToString("P2", CultureInfo.CurrentCulture);
            }

            int aidAccepted = 0;
            string aidString = null;

            if ((bool)aidAccept.IsChecked)
            {
                aidAccepted = 1;
                aidString = "New Moon sighting with aid is accepted.";
            }
            else
            {
                aidString = "New Moon sighting only with naked eye.";
            }

            int Iref = 1;
            if ((bool)noAirSelect.IsChecked)
            {
                Iref = 0;
            }

            double[] dstJD = new double[2];
            if ((bool)DST.IsChecked)
            {
                dstJD = DSTJD();
            }

            int year1 = getYear();
            int month1 = getMonth();
            int day1 = getDay();
            string yearText = year.Text;

            if ((bool)persCalendar.IsChecked)
            {
                yearText = irYear.Text;
                if (month1 < 3)
                {
                    year1 = year1 - 1;
                }  
                else if(month1 == 3 & day1 < 21)
                {
                    year1 = year1 - 1;
                }                
            }

            int k = 0;
            double JD = NativeMethods.TrueJDEquiSolitice(ref year1, ref k);
            
            double DJ1 = 0.0;
            int Iyear = 1;
            int Imonth = 1;
            int Iday = 1;
            double FD = 0.0;
            double DELTA_T = 0.0;
            int J = 0;
            NativeMethods.iauJD2Cal(ref JD, ref DJ1, ref Iyear, ref Imonth, ref Iday, ref FD, ref k);
            NativeMethods.iau_DAT(ref Iyear, ref Imonth, ref Iday, ref FD, ref DELTA_T, ref J);

            JD = JD + Geo[3] / 24.0 - (DELTA_T + 32.184) / (24.0 * 3600);

            int Jyear = 1;
            int Jmonth = 1;
            int Jday = 1;
            double Hours = 0.0;
            NativeMethods.JD2IrCal(ref JD, ref Jyear, ref Jmonth, ref Jday, ref Hours);
            
            int UT_TT = 0;

            double UJD = 0.0;
            int Dy = 15;

            int Gyear = 0;
            double IUJD = 0.0;
            bool leap = false;
            int[] Equinox = new int[3];
            int MarDay = 20;
            double Uhour = 0.0;

            double[] newHijriJD = new double[14];
            int[][] newMoonDate = new int[14][];
            for (int i = 0; i <= 13; i++)
            {
                newMoonDate[i] = new int[2];
            }

            double[][] MoonJD = new double[14][];
            for (int i = 0; i <= 13; i++)
            {
                MoonJD[i] = new double[4];
            }
            
            int oYear = 1;
            int oMonth = 1;
            int oDay = 1;
            int dayInOtherMonth = 1;
            int dayInMonth = 1;

            if ((bool)gregCalendar.IsChecked)
            {
                int Im = 1;
                int Id = 1;

                UJD = NativeMethods.Cal2JD(ref Iyear, ref Im, ref Id);
                oYear = 1;
                oMonth = 1;
                oDay = 1;
                double H = 0.0;
                NativeMethods.JD2IrCal(ref UJD, ref oYear, ref oMonth, ref oDay, ref H);
                if (NativeMethods.GregIsLeapYear(ref Iyear)) { daysGregmonth[1] = 29; }
                NativeMethods.IranCalendar(ref oYear, ref Gyear, ref IUJD, ref leap, Equinox, ref MarDay, ref Uhour);
                if (leap is true) { daysIranmonth[11] = 30; }
                NativeMethods.YearMoonPhases(Iyear, Im, Id, MoonJD);                
                NativeMethods.HijriMonths(Iyear, Im, 15, Geo, Atmos, UT_TT, Method, B_Ilum, aidAccepted, Iref,
                    newHijriJD, newMoonDate);
            }
            else if ((bool)persCalendar.IsChecked)
            {
                int Jy = getIrYear();
                NativeMethods.IranCalendar(ref Jy, ref Gyear, ref IUJD, ref leap, Equinox, ref MarDay, ref Uhour);
                int Im = 1;
                int Id = 1;
                double H = 0.0;
                UJD = NativeMethods.IrCal2JD(ref Jy, ref Im, ref Id, ref H);
                NativeMethods.JD2Cal(ref UJD, ref oYear, ref oMonth, ref oDay, ref H);
                if (leap) daysIranmonth[11] = 30;
                if (NativeMethods.GregIsLeapYear(ref Gyear)) { daysGregmonth[1] = 29; }
                NativeMethods.YearMoonPhases(Gyear, oMonth, MarDay, MoonJD);               
                NativeMethods.HijriMonths(Gyear, oMonth, MarDay, Geo, Atmos, UT_TT, Method, B_Ilum, aidAccepted, Iref,
                    newHijriJD, newMoonDate);
                Dy = Dy + daysGregmonth[0] + daysGregmonth[1] + MarDay;           
            }

            w5.sunMoonCaledar.AppendText(string.Format(CultureInfo.CurrentCulture, "{0,90}", " Sun & Moon Caledar For Year " + yearText + "\r\n"));
            w5.sunMoonCaledar.AppendText("======================================================================================"
                    + "=============================================================================================="
                    + "==================================\r\n");
            w5.sunMoonCaledar.AppendText(" Location: " + geoName + ", Longitude: " + Geo[0].ToString("F2", CultureInfo.CurrentCulture) +
                " \t Latitude: " + Geo[1].ToString("F2", CultureInfo.CurrentCulture) + "\t Elevation: " + Geo[2].ToString("F2", CultureInfo.CurrentCulture)
                + ", Time Zone: " + Geo[3].ToString("F2", CultureInfo.CurrentCulture) + "\r\n");
            w5.sunMoonCaledar.AppendText(airConditin + ", " + aidString + "\t" + method + "\r\n");
            w5.sunMoonCaledar.AppendText(" New year at: " + Utility.hour2Time(Hours) + ", " + Jday.ToString(CultureInfo.CurrentCulture) + " " + NativeMethods.pesianMonthName(Jmonth - 1, 1)
                + " " + Jyear.ToString(CultureInfo.CurrentCulture) + " \t " + Iday.ToString(CultureInfo.CurrentCulture) +
                " " + NativeMethods.gregoryMonthName(Imonth - 1, 1) + " " + Iyear.ToString(CultureInfo.CurrentCulture) + "\r\n");

            if ((bool)msiseAtm.IsChecked == false)
            {
                w5.sunMoonCaledar.AppendText(" Atmospheric properties for refraction: "
                + "Pressure(millibar): " + Atmos[0].ToString("F2", CultureInfo.CurrentCulture) + ", " + "Temperature(C) :"
                + Atmos[1].ToString("F2", CultureInfo.CurrentCulture) + "\r\n");
            }

            double NMJD = 0.0;
            int Hy = 1;
            int Hm = 1;
            int Hd = 1;
            double h = 0.0;

            NativeMethods.JD2TradHijri(ref UJD, Geo, Atmos, ref UT_TT, ref Method, ref B_Ilum,
                ref aidAccepted, ref Iref, ref NMJD, ref Hy, ref Hm, ref Hd, ref h);

            for (int km = 0; km <= 3; km++)
            {
                if (newHijriJD[km] > NMJD)
                {
                    k = km;
                    break;
                }
            }

            int[] jDate = { Iyear, 1, 1 };

            double[][] mRTSJD = new double[3][];
            double[][] mRTS_Hours = new double[3][];
            double[][] mRTS_Angles = new double[3][];

            for (int i = 0; i <= 2; i++)
            {
                mRTSJD[i] = new double[3];
                mRTS_Hours[i] = new double[3];
                mRTS_Angles[i] = new double[3];
            }

            double DJD = UJD;
            double BJD = DJD - 1.0;
            double[] Geo1 = new double[4];
            for (int l = 0; l <= 3; l++)
            {
                Geo1[l] = Geo[l];
            }

            NativeMethods.Moon_RTS(jDate, ref BJD, Geo1, Atmos, ref UT_TT, ref Iref, mRTS_Hours[0], mRTSJD[0], mRTS_Angles[0]);

            NativeMethods.Moon_RTS(jDate, ref DJD, Geo1, Atmos, ref UT_TT, ref Iref, mRTS_Hours[1], mRTSJD[1], mRTS_Angles[1]);

            int weekDaynum = NativeMethods.JD2WeekDayNum(UJD + (Uhour + Geo1[3]) / 24.0);

            
            for (int i = 0; i < 12; i++)
            {
                string strOyear = null;
                string mainMonth = null;
                string strOmonth = null;
                string strHmonth = null;

                if ((bool)gregCalendar.IsChecked)
                {
                    mainMonth = NativeMethods.gregoryMonthName(i);
                    strOyear = oYear.ToString(CultureInfo.CurrentCulture);
                    dayInMonth = daysGregmonth[i];
                    dayInOtherMonth = daysIranmonth[oMonth - 1];
                }
                else if ((bool)persCalendar.IsChecked)
                {
                    mainMonth = NativeMethods.pesianMonthName(i);
                    strOyear = oYear.ToString(CultureInfo.CurrentCulture);
                    dayInMonth = daysIranmonth[i];
                    dayInOtherMonth = daysGregmonth[oMonth - 1];
                }

                string strHYear = Hy.ToString(CultureInfo.CurrentCulture);

                w5.sunMoonCaledar.AppendText(
             " ================================================================================================"
            + "================================================================================================"
            + "===================\r\n");
                w5.sunMoonCaledar.AppendText(string.Format(CultureInfo.CurrentCulture, "{0,75}", mainMonth + " " + yearText + "\r\n"));

                if ((bool)msiseAtm.IsChecked)
                {
                    if (Dy > 365) { Dy = Dy - 365; }
                    Atmos = getAtm(Geo, Dy);

                    w5.sunMoonCaledar.AppendText(" pressure: " + Atmos[0].ToString("F2", CultureInfo.CurrentCulture)
                    + " \t Temperature: " + Atmos[1].ToString("F2", CultureInfo.CurrentCulture) + "\r\n");

                    Dy = Dy + 30;
                }

                w5.sunMoonCaledar.AppendText( 
                    "                                                      SunRise            Noon           SunSet      "
                    + "           Moon               Moon                    Moon                     Moon\r\n");
                w5.sunMoonCaledar.AppendText("  " + mainMonth + "         " + strOyear + "               " + strHYear
                 + "      Time-Azimuth    Time-Elevation    Time-Azimuth     "
                 + "                         Time-Angle               Time-Angle                Time-Angle\r\n");
                w5.sunMoonCaledar.AppendText("------------------------------------------------------------------------------------------------------------"
                 + "--------------------------------------------------------------------------------------------------------\r\n");

                for (int j = 1; j <= dayInMonth; j++)
                {

                    if (weekDaynum > 6) { weekDaynum = 0; }

                    if (oDay == 1 | j == 1)
                    {
                        if ((bool)gregCalendar.IsChecked)
                        {
                            strOmonth = NativeMethods.pesianMonthName(oMonth - 1, 1);
                        }
                        else if ((bool)persCalendar.IsChecked)
                        {
                            strOmonth = NativeMethods.gregoryMonthName(oMonth - 1, 1);
                        }
                    }
                    else
                    {
                        if ((bool)gregCalendar.IsChecked)
                        {
                            strOmonth = string.Format(CultureInfo.CurrentCulture, "{0,12}", "#  ");
                        }
                        else if ((bool)persCalendar.IsChecked)
                        {
                            strOmonth = string.Format(CultureInfo.CurrentCulture, "{0,9}", "#  ");
                        }
                    }

                    if (Hd == 1 | j == 1)
                    {
                        strHmonth = NativeMethods.hijriMonthName(Hm - 1, 1);
                    }
                    else
                    {
                        strHmonth = string.Format(CultureInfo.CurrentCulture, "{0,18}", "#  ");
                    }
            
                    double AJD = DJD + 1.0;
                    if ((bool)DST.IsChecked)
                    {
                        if (DJD >= dstJD[0] & DJD < dstJD[1])
                        {
                            Geo1[3] = Geo[3] + 1.0;
                        }
                        else
                        {
                            Geo1[3] = Geo[3];
                        }
                    }

                    double[] Times = new double[3];
                    double[] RSTJD = new double[3];
                    double[] RTS_Angles = new double[3];

                    NativeMethods.SolarTimes(ref DJD, jDate, Geo1, Atmos, ref UT_TT, ref Iref, Times, RSTJD, RTS_Angles);

                    NativeMethods.Moon_RTS(jDate, ref AJD, Geo1, Atmos, ref UT_TT, ref Iref, mRTS_Hours[2], mRTSJD[2], mRTS_Angles[2]);                   

                    double[] moonRTSJD = new double[4];
                    string[] moonEvents = new string[4];

                    NativeMethods.MoonRiseTranSet(DJD, mRTSJD, mRTS_Hours, mRTS_Angles, Geo1[3], moonEvents, moonRTSJD);
                    
                    string MoonStat = "            ";

                    for (int m = i; m <= i + 2; m++)
                    {                        
                        for (int n = 0; n <= 3; n++)
                        {
                            if (DJD == MoonJD[m][n] - MoonJD[m][n] % 1.0 - 0.5)
                            {
                                if (n == 0)
                                {
                                    MoonStat = " New Moon  ";
                                    break;
                                }
                                else if (n == 1)
                                {
                                    MoonStat = " First Half";
                                    break;
                                }
                                else if (n == 2)
                                {
                                    MoonStat = " Full Moon ";
                                    break;
                                }
                                else
                                {
                                    MoonStat = " Last Half ";
                                    break;
                                }
                            }
                        }
                    }
                
                    w5.sunMoonCaledar.AppendText(NativeMethods.WeekDays(weekDaynum, 2) + "  " + j.ToString("D2", CultureInfo.CurrentCulture)
                    + "  " + strOmonth + "," + oDay.ToString("D2", CultureInfo.CurrentCulture) + "  " + strHmonth + "," +
                    Hd.ToString("D2", CultureInfo.CurrentCulture)                     
                    + "  " + Utility.hour2Time(Times[0]) + "-" + RTS_Angles[0].ToString("000.00", CultureInfo.CurrentCulture)
                    + "  " + Utility.hour2Time(Times[1]) + "-" + RTS_Angles[1].ToString("000.00", CultureInfo.CurrentCulture)
                    + "  " + Utility.hour2Time(Times[2]) + "-" + RTS_Angles[2].ToString("000.00", CultureInfo.CurrentCulture)
                    +"  " + MoonStat);

                    for (int l = 0; l < moonEvents.Length; l++)
                    {
                        w5.sunMoonCaledar.AppendText("\t" + moonEvents[l]);
                    }

                    w5.sunMoonCaledar.AppendText("\r\n");

                    for (int n = 0; n <= 1; n++)
                    {
                        for (int l = 0; l <= 2; l++)
                        {
                            mRTSJD[n][l] = mRTSJD[n + 1][l];
                            mRTS_Hours[n][l] = mRTS_Hours[n + 1][l];
                            mRTS_Angles[n][l] = mRTS_Angles[n + 1][l];
                        }
                    }

                    DJD += 1.0;                    
                    weekDaynum += 1;
                    oDay += 1;
                    if (oDay > dayInOtherMonth)
                    {
                        oDay = 1;
                        oMonth += 1;
                        if (oMonth > 12)
                        {
                            oMonth = 1;
                            oYear = (oYear + 1);
                        }
                    }
                    
                    Hd += 1;
                    if (DJD == newHijriJD[k])
                    {
                        Hd = 1;
                        Hm = newMoonDate[k][1];
                        Hy = newMoonDate[k][0];
                        k = k + 1;
                    }
                }

                w5.waitWarn.Content = null;
            }

        }

        private void ShowHelp(object sender, RoutedEventArgs e)
        {
            System.Windows.Forms.Help.ShowHelp(null, @"TheMoonSun.chm");
        }

        private void newTime_Click(object sender, RoutedEventArgs e)
        {
            DateTime thisTime = DateTime.Now;
            LocalHour.Text = thisTime.Hour.ToString(CultureInfo.CurrentCulture);
            localMin.Text = thisTime.Minute.ToString(CultureInfo.CurrentCulture);
            localSec.Text = thisTime.Second.ToString(CultureInfo.CurrentCulture);
            //   UTC.Text = "UTC \r\n" +
            //       thisTime.ToUniversalTime().ToString("dd/MM/yyyy HH:mm:ss", CultureInfo.CurrentCulture);

            calculateItmes();
        }

        private void gregCalendar_Checked(object sender, RoutedEventArgs e)
        {
            year.Foreground = new SolidColorBrush(Colors.Blue);
            month.Foreground = new SolidColorBrush(Colors.Red);
            day.Foreground = new SolidColorBrush(Colors.Red);
            irYear.Foreground = new SolidColorBrush(Colors.Khaki);
            irMonth.Foreground = new SolidColorBrush(Colors.Khaki);
            irDay.Foreground = new SolidColorBrush(Colors.Khaki);

            DateTime gregDate = Jalali2Greg();
            year.Text = Convert.ToString(gregDate.Year, CultureInfo.CurrentCulture);
            month.Text = Convert.ToString(gregDate.Month, CultureInfo.CurrentCulture);
            day.Text = Convert.ToString(gregDate.Day, CultureInfo.CurrentCulture);
        }

        private void persCalendar_Checked(object sender, RoutedEventArgs e)
        {
            irYear.Foreground = new SolidColorBrush(Colors.Blue);
            irMonth.Foreground = new SolidColorBrush(Colors.Red);
            irDay.Foreground = new SolidColorBrush(Colors.Red);
            year.Foreground = new SolidColorBrush(Colors.Khaki);
            month.Foreground = new SolidColorBrush(Colors.Khaki);
            day.Foreground = new SolidColorBrush(Colors.Khaki);

            int[] iranDate = Greg2Jalali();
            irYear.Text = Convert.ToString(iranDate[0], CultureInfo.CurrentCulture);
            irMonth.Text = Convert.ToString(iranDate[1], CultureInfo.CurrentCulture);
            irDay.Text = Convert.ToString(iranDate[2], CultureInfo.CurrentCulture);
        }

        private void calendarChange()
        {
            if ((bool)gregCalendar.IsChecked)
            {
                year.Foreground = new SolidColorBrush(Colors.Blue);
                month.Foreground = new SolidColorBrush(Colors.Red);
                day.Foreground = new SolidColorBrush(Colors.Red);
                irYear.Foreground = new SolidColorBrush(Colors.Khaki);
                irMonth.Foreground = new SolidColorBrush(Colors.Khaki);
                irDay.Foreground = new SolidColorBrush(Colors.Khaki);

                int[] iranDate = Greg2Jalali();
                irYear.Text = Convert.ToString(iranDate[0], CultureInfo.CurrentCulture);
                irMonth.Text = Convert.ToString(iranDate[1], CultureInfo.CurrentCulture);
                irDay.Text = Convert.ToString(iranDate[2], CultureInfo.CurrentCulture);
            }
            else if ((bool)persCalendar.IsChecked)
            {
                irYear.Foreground = new SolidColorBrush(Colors.Blue);
                irMonth.Foreground = new SolidColorBrush(Colors.Red);
                irDay.Foreground = new SolidColorBrush(Colors.Red);
                year.Foreground = new SolidColorBrush(Colors.Khaki);
                month.Foreground = new SolidColorBrush(Colors.Khaki);
                day.Foreground = new SolidColorBrush(Colors.Khaki);

                DateTime gregDate = Jalali2Greg();
                year.Text = Convert.ToString(gregDate.Year, CultureInfo.CurrentCulture);
                month.Text = Convert.ToString(gregDate.Month, CultureInfo.CurrentCulture);
                day.Text = Convert.ToString(gregDate.Day, CultureInfo.CurrentCulture);
            }
        }
    }
}

