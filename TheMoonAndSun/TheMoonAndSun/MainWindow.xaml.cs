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
            irYear.Text = Convert.ToString(iranDate[0], CultureInfo.CurrentCulture);
            irMonth.Text = Convert.ToString(iranDate[1], CultureInfo.CurrentCulture);
            irDay.Text = Convert.ToString(iranDate[2], CultureInfo.CurrentCulture);
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
            int UT_TT = 1;
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

            string sunLocation = "";
            if (SunElev < -0.2666)
            {
                sunLocation = String.Format(CultureInfo.CurrentCulture, "Sun is Below Horizon ({0:00.000})", SunElev);
            }
            else
            {
                sunLocation = String.Format(CultureInfo.CurrentCulture, " Sun Altitude: {0:00.000} \t Sun Azimuth: {1:000.000}", SunElev, Azim);
            }

            double Tcen = ((UJD - 2451545.0)) / 36525.0;
            double R = 0.0;

            NativeMethods.Sun_Earth_Vector(ref Tcen, ref R);
            double AU = AUSEC * C / 1000000000.0;

            double Earth_Sun_Vector = AU * R;

            double MoonAngle = 0.0;
            double[] RSJD = new double[2];
            double[] RShours = new double[2];
            double[] RSangles = new double[2];
            double[] Ilum_Ratio = new double[3];
            double[] moonJD = new double[3];
            double[] moonRShours = new double[3];
            double[] moonRSangles = new double[3];

            NativeMethods.Moon_Day_Rise_Set(jDate, ref TJD, Geo, Atmos, ref UT_TT, ref MoonAngle, RSJD, RShours,
                RSangles, ref Iref);

            moonJD[0] = RSJD[0] - Geo[3] / 24.0;
            moonJD[2] = RSJD[1] - Geo[3] / 24.0;
            moonRSangles[0] = RSangles[0];
            moonRSangles[2] = RSangles[1];
            moonRShours[0] = RShours[0];
            moonRShours[2] = RShours[1];

            NativeMethods.moonTransit(jDate, ref TJD, Geo, Atmos, ref UT_TT, ref MoonAngle, ref moonJD[1], ref moonRShours[1],
                ref moonRSangles[1]);

            moonJD[1] = moonJD[1] - Geo[3] / 24.0;
            double Hour = 0.0;

            for (int n = 0; n <= 2; n++)
            {
                NativeMethods.Moon_IlumRatio(jDate, ref Hour, ref moonJD[n], ref UT_TT, ref Ilum_Ratio[n]);
            }
            double delta = 0.0;
            double TTJDT = 0.0;
            double CMoonElev = 0.0;
            double CMoonAzim = 0.0;
            NativeMethods.lunar_position(ref UJD, ref UT_TT, ref TTJDT, Geo, Atmos, ref CMoonElev, ref CMoonAzim, ref delta, ref Iref);
            double GeoDia = 0.0;
            double TopoDia = 0.0;
            NativeMethods.MoonSemiDia(ref UJD, ref CMoonElev, ref GeoDia, ref TopoDia);
            string moonLocation = "";
            if (CMoonElev < -TopoDia)
            {
                moonLocation = String.Format(CultureInfo.CurrentCulture, "Moon is Below Horizon ({0:00.000})", CMoonElev);
            }
            else
            {
                moonLocation = String.Format(CultureInfo.CurrentCulture, "Moon Altitude: {0:00.000} \t Moon Azimuth: {1:000.000}", CMoonElev, CMoonAzim);
            }

            Position.Text = "Position of " + sunLocation + "\r\n" +
                "Position of " + moonLocation;

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
            int adjust = 0;

            if ((bool)aidAccept.IsChecked)
            {
                aidAccepted = 1;
            }

            int ye = jDate[0];
            int newMoonMon = jDate[1] - 2;
            double[] NPhMoonJD = new double[5];
            double[] FPhMoonJD = new double[5];
            double[] newMoonJD = new double[5];

            for (int i = 0; i <= 4; i++)
            {
                if (newMoonMon > 12)
                {
                    newMoonMon = 1;
                    ye = ye + 1;
                }
                else if (newMoonMon <= 0)
                {
                    newMoonMon = 12 + newMoonMon;
                    ye = ye - 1;
                }
                int j = 0;
                NativeMethods.Moon_Phases(ref ye, ref newMoonMon, ref jDate[2], ref j, ref NPhMoonJD[i]);
                j = 2;
                NativeMethods.Moon_Phases(ref ye, ref newMoonMon, ref jDate[2], ref j, ref FPhMoonJD[i]);

                NativeMethods.HijriAdjust(ref NPhMoonJD[i], Geo, ref UT_TT, ref Method, ref B_Ilum, ref aidAccepted,
                 ref newMoonJD[i], ref adjust);

                newMoonMon = newMoonMon + 1;
            }

            double AnewMoonJD = 0.0;
            double BnewMoonJD = 0.0;
            double AfullMoonJD = 0.0;
            double BfullMoonJD = 0.0;

            for (int i = 0; i <= 4; i++)
            {
                if (NJD >= newMoonJD[i])
                {
                    AnewMoonJD = newMoonJD[i];
                }
                else
                {
                    BnewMoonJD = newMoonJD[i];
                    break;
                }
            }


            for (int i = 0; i <= 4; i++)
            {
                if (NJD >= FPhMoonJD[i])
                {
                    AfullMoonJD = FPhMoonJD[i];
                }
                else
                {
                    BfullMoonJD = FPhMoonJD[i];
                    break;
                }
            }

            int daysinceNewMoon = Convert.ToInt32(NJD - AnewMoonJD);
            int daystoNextnewMoon = Convert.ToInt32(BnewMoonJD - NJD);

            int daysSinceFullMoon = Convert.ToInt32(NJD - AfullMoonJD);
            int daystoNextFullMoon = Convert.ToInt32(BfullMoonJD - NJD);

            double H = 0.0;
            double JD = 0.0;
            NativeMethods.JULDAT(ref jDate[0], ref jDate[1], ref jDate[2], ref H, ref JD);

            double newHijriJD = 0.0;

            for (int l = 0; l <= 4; l++)
            {
                if (JD < newMoonJD[l])
                {
                    newHijriJD = newMoonJD[l - 1];
                    break;
                }
            }

            double JDH = newHijriJD + 10.0;
            int Hy = 1;
            int Hm = 1;
            int Hd = 1;
            NativeMethods.JD2Hijri(ref JDH, ref Hy, ref Hm, ref Hd);

            Hd = Convert.ToInt32(JD - newHijriJD) + 1;

            string hijridate = Hd.ToString("D2", CultureInfo.CurrentCulture) + " " + NativeMethods.hijriMonthName(Hm - 1, 1) + " " + Hy.ToString("D4", CultureInfo.CurrentCulture);

            result.Text = " Location : " + geoName + "\t Longitude: " + Geo[0].ToString("F2", CultureInfo.CurrentCulture)
                 + "\t Latitude: " + Geo[1].ToString("F2", CultureInfo.CurrentCulture) + "\t Elevation: " + Geo[2].ToString("F2", CultureInfo.CurrentCulture) + "\r\n";
            result.AppendText(" Date is: " + NativeMethods.JUL2WeekDay(NJD, 2) + " " + thisDate.ToString("dd / MM / yyyy HH:mm:ss", CultureInfo.CurrentCulture)
             + "\t Iranian Calendar: " + Jd.ToString("D2", CultureInfo.CurrentCulture) + " " + NativeMethods.pesianMonthName(Jm - 1, 1) + " " + Jy.ToString("D4", CultureInfo.CurrentCulture) + "\r\n");
            result.AppendText(" Hijri Lunar Calendar: " + hijridate + "\t UT Julian Day: " + UJD.ToString("F4", CultureInfo.CurrentCulture) + "\r\n");
            result.AppendText(" Day of Gregory Year: " + (thisDate.DayOfYear).ToString("D3", CultureInfo.CurrentCulture) + " \t\t " + IrDayofYear + "\r\n");
            result.AppendText(atmModel + " Atmospheric model properies for refraction at this Location: \r\n");
            result.AppendText(" Atmospheric Pressure (millibar): " + Atmos[0].ToString("F2", CultureInfo.CurrentCulture) + ", \t "
             + "Temperature(C) :" + Atmos[1].ToString("F2", CultureInfo.CurrentCulture) + "\r\n");
            result.AppendText(" The Sun \r\n");
            result.AppendText("\t Rise Time: " + Utility.hour2Time(Times[3]) + "\t Noon Time " + Utility.hour2Time(Times[4])
             + "\t  Set Time: " + Utility.hour2Time(Times[5]) + "\r\n");
            result.AppendText("\t Rise Azimuth " + RTS_Angles[0].ToString("F3", CultureInfo.CurrentCulture) + "\t\t Altitude: " + RTS_Angles[1].ToString("F3", CultureInfo.CurrentCulture)
             + "\t Set Azimuth: " + RTS_Angles[2].ToString("F3", CultureInfo.CurrentCulture) + "\r\n");
            result.AppendText("\t" + sunLocation + "\t Sun Earth Distance: " + Earth_Sun_Vector.ToString("F3", CultureInfo.CurrentCulture) + " million kilometers \r\n");
            result.AppendText(" The Moon \r\n");
            if (moonJD[2] > moonJD[0])
            {
                result.AppendText("\t Rise Time: " + Utility.hour2Time(moonRShours[0]) + "\t Transit Time " +
                    Utility.hour2Time(moonRShours[1]) + "\t  Set Time: " + Utility.hour2Time(moonRShours[2]) + "\r\n");
                result.AppendText("\t Rise Azimuth " + moonRSangles[0].ToString("F3", CultureInfo.CurrentCulture) + "\t Transit Altitude: " +
                    moonRSangles[1].ToString("F3", CultureInfo.CurrentCulture) + "\t  Set Azimuth: " + moonRSangles[2].ToString("F3", CultureInfo.CurrentCulture) + "\r\n");
                result.AppendText("\t Rise Phase " + (Ilum_Ratio[0]).ToString("00.00%", CultureInfo.CurrentCulture) + "\t\t Transit Phase: " +
                    (Ilum_Ratio[1]).ToString("00.00%", CultureInfo.CurrentCulture) + "\t  Set Phase: " + (Ilum_Ratio[2]).ToString("00.00%", CultureInfo.CurrentCulture) + "\r\n");
            }
            else
            {
                result.AppendText("\t Set Time: " + Utility.hour2Time(moonRShours[2]) + "\t Transit Time " +
                    Utility.hour2Time(moonRShours[1]) + "\t  Rise Time: " + Utility.hour2Time(moonRShours[0]) + "\r\n");
                result.AppendText("\t Set Azimuth " + moonRSangles[2].ToString("F3", CultureInfo.CurrentCulture) + "\t Transit Altitude: " +
                    moonRSangles[1].ToString("F3", CultureInfo.CurrentCulture) + "\t  Rise Azimuth: " + moonRSangles[0].ToString("F3", CultureInfo.CurrentCulture) + "\r\n");
                result.AppendText("\t Set Phase " + (Ilum_Ratio[2]).ToString("00.00%", CultureInfo.CurrentCulture) + "\t\t Transit Phase: " +
                    (Ilum_Ratio[1]).ToString("00.00%", CultureInfo.CurrentCulture) + "\t  Rise Phase: " + (Ilum_Ratio[0]).ToString("00.00%", CultureInfo.CurrentCulture) + "\r\n");
            }
            result.AppendText("\t Current Moon Position:" + moonLocation + "\t  Moon Diameter: " + TopoDia.ToString("F4", CultureInfo.CurrentCulture) + "\r\n");
            result.AppendText(" Moon Earth Distance: " + MoonDistance.ToString("F3", CultureInfo.CurrentCulture) + " kilometers \r\n");
            result.AppendText(" Days since last new Moon: " + daysinceNewMoon.ToString("D2", CultureInfo.CurrentCulture) + "\t Days to next new Moon: " + daystoNextnewMoon.ToString("D2", CultureInfo.CurrentCulture) + "\r\n");
            result.AppendText(" Days since last Full Moon: " + daysSinceFullMoon.ToString("D2", CultureInfo.CurrentCulture) + "\t Days to next Full Moon: " + daystoNextFullMoon.ToString("D2", CultureInfo.CurrentCulture) + "\r\n");
            result.AppendText(" Twilight Times \r\n");
            result.AppendText(" Morning  Astronomical: " + Utility.hour2Time(Times[0]) + "\t Nautical: " + Utility.hour2Time(Times[1]) + "\t Civic: " + Utility.hour2Time(Times[2]) + "\r\n");
            result.AppendText(" Evening  Astronomical: " + Utility.hour2Time(Times[8]) + "\t Nautical: " + Utility.hour2Time(Times[7]) + "\t Civic: " + Utility.hour2Time(Times[6]));
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

        private double [] getGeoLocation()
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
            double NTJD = 0.0;
            int Y = dateIn.Year;
            int M = dateIn.Month;
            int D = dateIn.Day;
            double H = getHour();        
            NativeMethods.JULDAT(ref Y, ref M, ref D, ref H, ref NTJD);
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
            double H = getHour();
            double JD = 0.0;

            NativeMethods.JULDAT(ref Y, ref M, ref D, ref H, ref JD);
            int Jy = 1;
            int Jm = 1;
            int Jd = 1;
            NativeMethods.JD2IrCal(ref JD, ref Jy, ref Jm, ref Jd, ref H);
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
                NativeMethods.JULDAT(ref y, ref sMonth, ref day, ref hour, ref SJD);
                sDay = -Convert.ToInt32(SJD + 1.5) % 7 + 28;
                eMonth = 10;
                NativeMethods.JULDAT(ref y, ref eMonth, ref day, ref hour, ref EJD);
                eDay = -Convert.ToInt32(EJD + 1.5) % 7 + 28;
                SJD = SJD + sDay;
                EJD = EJD + eDay;
            }
            else if (selectRegion.SelectedIndex == 1)
            {
                if ((bool)gregCalendar.IsChecked)
                {
                    sDay = 22;
                    NativeMethods.JULDAT(ref y, ref sMonth, ref sDay, ref H, ref SJD);
                    eMonth = 9;
                    eDay = 22;
                    NativeMethods.JULDAT(ref y, ref eMonth, ref eDay, ref H, ref EJD);
                }
                else if ((bool)persCalendar.IsChecked)
                {
                    SJD = NativeMethods.IrCal2JD(ref Jy, ref sJm, ref sJd, ref H);
                    EJD = NativeMethods.IrCal2JD(ref Jy, ref eJm, ref eJd, ref H);
                }
            }
            else if (selectRegion.SelectedIndex == 2)
            {
                NativeMethods.JULDAT(ref y, ref sMonth, ref day, ref hour, ref SJD);
                sDay = -Convert.ToInt32(SJD + 1.5) % 7 + 14;
                eMonth = 11;
                NativeMethods.JULDAT(ref y, ref eMonth, ref day, ref hour, ref EJD);
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
                    NativeMethods.RCALDAT(ref SJD, ref sYear, ref sMonth, ref sDay, ref FD);
                    NativeMethods.RCALDAT(ref EJD, ref eYear, ref eMonth, ref eDay, ref FD);
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
            for(int i= 0; i<=4; i++)
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
            
            int newMoonMon = 1;
            if ((bool)persCalendar.IsChecked) { newMoonMon = 3; }            

            double[][] moonPhaseJD = new double[15][];
            for (int i = 0; i <= 14; i++)
            {
                moonPhaseJD[i] = new double[4];
            }
            int day = 1;           

            NativeMethods.YearMoonPhases(ye, newMoonMon, day, moonPhaseJD);

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
            for (int i = 0; i <= 14; i++)
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
                    NativeMethods.RCALDAT(ref JDM, ref Iyear, ref Imonth, ref Iday, ref hour);
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

                newMoonMon = newMoonMon + 1;
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

            int Method = 3;
            string method = null;

            if ((bool)yallop.IsChecked)
            {
                Method = 1;
                method = "Yallop Criteria = ";
            }
            else if ((bool)odeh.IsChecked)
            {
                Method = 2;
                method = "Odeh Criteria = ";
            }
            else if ((bool)yallopOdeh.IsChecked)
            {
                Method = 3;
                method = "Yallop or Odeh Criteria = ";
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

            int UT_TT = 1;
            double GeoNDST = Geo[3];
            double GeoDST = Geo[3] + 1.0;

            int day = 1;
            int newMoonMon = 1;
            if ((bool)persCalendar.IsChecked) { newMoonMon = 3; }

            bool Status = false;
            int visiStat = 0;
            double[] criteria = new double[12];
            double HJD = 0.0;
            int Iyear = 1;
            int Imonth = 1;
            int Iday = 1;

            string hijriDate = null; 

            w3.hijriMonths.AppendText("\t\t\t Hijri Months For Year " + yearText + "\r\n");
            w3.hijriMonths.AppendText("=================================================================\r\n");
            w3.hijriMonths.AppendText("Location: " + geoName + ", Longitude: " + Geo[0].ToString("F2", CultureInfo.CurrentCulture)
                 + " \t Latitude: " + Geo[1].ToString("F2", CultureInfo.CurrentCulture) + "\t Elevation: " + Geo[2].ToString("F2", CultureInfo.CurrentCulture)
                 + ", Time Zone: " + Geo[3].ToString("F2", CultureInfo.CurrentCulture) + "\r\n");
            w3.hijriMonths.AppendText("\r\n");
            w3.hijriMonths.AppendText(airConditin + ", " + aidString + "\r\n");
            w3.hijriMonths.AppendText("\r\n");
            w3.hijriMonths.AppendText("DAZ = Sun Azimuth - Moon Azimth in Degrees. \r\n");
            w3.hijriMonths.AppendText("ARCV = Moon Elevation - Sun Elevation in Degrees. \r\n");
            w3.hijriMonths.AppendText("Cos(ARCL) = Cos(ARCV)*Cos(DAZ) \r\n");
            w3.hijriMonths.AppendText("TopoDia = Topocenteric semi diameter of moon \r\n");
            w3.hijriMonths.AppendText("W_p = width of moon crescent in minutes of arc \r\n");
            w3.hijriMonths.AppendText("No atmospheric refraction for sun set. Moon location at 4/9 of lag, no refraction effect.\r\n");
            w3.hijriMonths.AppendText("=================================================================\r\n");
            w3.hijriMonths.AppendText("\r\n");

            double MoonJD = 0.0;
            int m = 0;

            for (int i = 0; i <= 13; i++)
            {
                if (newMoonMon > 12)
                {
                    newMoonMon = 1;
                    ye = ye + 1;
                }

                NativeMethods.Moon_Phases(ref ye, ref newMoonMon, ref day, ref m, ref MoonJD);

                HJD = MoonJD - MoonJD % 1.0 - 0.5;
                if ((bool)DST.IsChecked)
                {
                    double[] dstJD = DSTJD();
                    if (HJD >= dstJD[0] | HJD <= dstJD[1])
                    {
                        Geo[3] = GeoDST;
                    }
                    else
                    {
                        Geo[3] = GeoNDST;
                    }
                }
                for (int j = 0; j <= 3; j++)
                {
                    NativeMethods.MoonVisibility(ref MoonJD, ref HJD, ref UT_TT, Geo, ref Method, ref B_Ilum,
                    ref aidAccepted, ref Status, ref visiStat, criteria);
                    HJD += 1.0;
                    if (Status) break;
                }

                double HJD7 = HJD + 7.0;
                double hour = 0.0;
                Iyear = 1;
                Imonth = 1;
                Iday = 1;

                NativeMethods.JD2Hijri(ref HJD7, ref Iyear, ref Imonth, ref Iday);
                hijriDate = String.Format(CultureInfo.CurrentCulture, "First of " +
                    NativeMethods.hijriMonthName(Imonth - 1) + " " + Iyear.ToString(CultureInfo.CurrentCulture));
                NativeMethods.RCALDAT(ref HJD, ref Iyear, ref Imonth, ref Iday, ref hour);
                string gregHijri = String.Format(CultureInfo.CurrentCulture, Iyear.ToString(CultureInfo.CurrentCulture) + "/" +
                    Imonth.ToString(CultureInfo.CurrentCulture) + "/" + Iday.ToString(CultureInfo.CurrentCulture));
                NativeMethods.JD2IrCal(ref HJD, ref Iyear, ref Imonth, ref Iday, ref hour);
                string persHijri = String.Format(CultureInfo.CurrentCulture, Iyear.ToString(CultureInfo.CurrentCulture) + "/" +
                    Imonth.ToString(CultureInfo.CurrentCulture) + "/" + Iday.ToString(CultureInfo.CurrentCulture));

                w3.hijriMonths.AppendText(hijriDate + " \t Gregory: " + gregHijri + "\t Iranian: " +
                    persHijri + "\r\n");
                w3.hijriMonths.AppendText(" Sun set time: " + Utility.hour2Time(criteria[11]) + "\t");
                w3.hijriMonths.AppendText(" Moon Azimuth = " + criteria[9].ToString("F2", CultureInfo.CurrentCulture) + "\t"
                + "Moon Elevation = " + criteria[10].ToString("F2", CultureInfo.CurrentCulture) + "\r\n");
                w3.hijriMonths.AppendText("Moon Visibility Criteria \r\n" +
                    "Age = " + Utility.hour2Time(criteria[0]) + " , Lag = " + Utility.hour2Time(criteria[1])
                    + " , Percent of Moon phase = " + (criteria[2]).ToString("P2", CultureInfo.CurrentCulture)
                    + " , DAZ = " + criteria[3].ToString("F3", CultureInfo.CurrentCulture));
                w3.hijriMonths.AppendText(" , ARCV = " + criteria[4].ToString("F3", CultureInfo.CurrentCulture)
                   + " , Cos(ARCL) = " + criteria[5].ToString("F4", CultureInfo.CurrentCulture) + ", TopoDia = "
                   + criteria[6].ToString("F3", CultureInfo.CurrentCulture) + ", W_p = " + criteria[7].ToString("F4", CultureInfo.CurrentCulture));
                w3.hijriMonths.AppendText(" , " + method + criteria[8].ToString("F2", CultureInfo.CurrentCulture) + "\r\n");
                w3.hijriMonths.AppendText("\r\n");

                newMoonMon = newMoonMon + 1;
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
            JD = JD + Geo[3] / 24.0;
            int Jyear = 1;
            int Jmonth = 1;
            int Jday = 1;
            double Hours = 0.0;
            NativeMethods.JD2IrCal(ref JD, ref Jyear, ref Jmonth, ref Jday, ref Hours);
            double DJ1 = 0.0;
            int Iyear = 1;
            int Imonth = 1;
            int Iday = 1;
            double FD = 0.0;
            double deltaT = 0.0;
            NativeMethods.JD2Cal(ref JD, ref DJ1, ref Iyear, ref Imonth, ref Iday, ref FD, ref k);
            NativeMethods.iau_DAT(ref Iyear, ref Imonth, ref Iday, ref FD, ref deltaT, ref k);
            deltaT = deltaT + 32.184;
            Hours = FD * 24.0 - deltaT / 3600.0;

            int UT_TT = 1;  
           
            double UJD = 0.0;
            int Dy = 15;

            int Gyear = 0;
            double IUJD = 0.0;
            bool leap = false;
            int[] Equinox = new int[3];
            int MarDay = 20;
            double Uhour = 0.0;

            double[][] MoonJD = new double[15][];
            for (int i = 0; i <= 14; i++)
            {
                MoonJD[i] = new double[4];
            }

            double[] newHijriJD = new double[15];
            int oYear = 1;
            int oMonth = 1;
            int oDay = 1;
            int dayInOtherMonth = 1;
            int dayInMonth = 1;


            if ((bool)gregCalendar.IsChecked)
            {
                int Im = 1;
                int Id = 1;
                double H = 0.0;
                NativeMethods.JULDAT(ref Iyear, ref Im, ref Id, ref H, ref UJD);
                oYear = 1;
                oMonth = 1;
                oDay = 1;
                NativeMethods.JD2IrCal(ref UJD, ref oYear, ref oMonth, ref oDay, ref H);
                if (NativeMethods.GregIsLeapYear(ref Iyear)) { daysGregmonth[1] = 29; }
                NativeMethods.IranCalendar(ref oYear, ref Gyear, ref IUJD, ref leap, Equinox, ref MarDay, ref Uhour);
                if (leap is true) { daysIranmonth[11] = 30; }
                NativeMethods.YearMoonPhases(Iyear, Im, Id, MoonJD);
                //       w4.MoonCaledar.AppendText(string.Format(CultureInfo.CurrentCulture, "{0,80}", " Moon Caledar For Year " + year.Text + "\r\n"));
            }
            else if ((bool)persCalendar.IsChecked)
            {
                int Jy = getIrYear();
                NativeMethods.IranCalendar(ref Jy, ref Gyear, ref IUJD, ref leap, Equinox, ref MarDay, ref Uhour);
                int Im = 1;
                int Id = 1;
                double H = 0.0;
                UJD = NativeMethods.IrCal2JD(ref Jy, ref Im, ref Id, ref H);
                NativeMethods.RCALDAT(ref UJD, ref oYear, ref oMonth, ref oDay, ref H);
                if (leap) daysIranmonth[11] = 30;
                if (NativeMethods.GregIsLeapYear(ref Gyear)) { daysGregmonth[1] = 29; }
                NativeMethods.YearMoonPhases(Gyear, oMonth, MarDay, MoonJD);
                Dy = Dy + daysGregmonth[0] + daysGregmonth[1] + MarDay;
                //     w4.MoonCaledar.AppendText(string.Format(CultureInfo.CurrentCulture, "{0,80}", " Moon Caledar For Year " + iranYear.Text + "\r\n"));
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

            int Adjust = 0;

            for (int i = 0; i <= 14; i++)
            {
                NativeMethods.HijriAdjust(ref MoonJD[i][0], Geo, ref UT_TT, ref Method, ref B_Ilum,
                    ref aidAccepted, ref newHijriJD[i], ref Adjust);
            }

            int Hy = 1;
            int Hm = 1;
            int Hd = 1;

            for (int n = 0; n <= 3; n++)
            {
                if (UJD < newHijriJD[n])
                {
                    double HJD = newHijriJD[n - 1] + 14.0;
                    NativeMethods.JD2Hijri(ref HJD, ref Hy, ref Hm, ref Hd);
                    Hd = Convert.ToInt32(UJD - newHijriJD[n - 1]) + 1;
                    k = n;
                    break;
                }
            }

            int[] jDate = { Iyear, 1, 1 };            

            double TJD = UJD;
            int weekDaynum = NativeMethods.JD2WeekDayNum(TJD + (Uhour+Geo[3]) / 24.0);

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
                w4.MoonCaledar.AppendText(string.Format(CultureInfo.CurrentCulture, "{0,60}", mainMonth + " " +yearText+ "\r\n"));


                if ((bool)msiseAtm.IsChecked)
                {
                    if (Dy > 365) { Dy = Dy - 365; }
                    Atmos = getAtm(Geo, Dy);

                    w4.MoonCaledar.AppendText(" pressure: " + Atmos[0].ToString("F2", CultureInfo.CurrentCulture)
                        + " \t Temperature: " + Atmos[1].ToString("F2", CultureInfo.CurrentCulture) + "\r\n");

                    Dy = Dy + 30;
                }

                w4.MoonCaledar.AppendText("                                        " +
                       "            Moon Rise                     Moon Transit                  Moon Set\r\n");
                w4.MoonCaledar.AppendText(" " + mainMonth + "        " + strOyear + "              " + strHYear
                        + "      Time-Azimuth-Illumination     Time-Altitude-Ilum     Time-Azimuth-Illumination\r\n");
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
                        strHmonth = NativeMethods.hijriMonthName(Hm - 1, 1);
                    }
                    else
                    {
                        strHmonth = string.Format(CultureInfo.CurrentCulture, "{0,18}", "#  ");
                    }


                    double MoonAngle = 0.0;
                    double[] RSJD = new double[2];
                    double[] RShours = new double[2];
                    double[] RSangles = new double[2];
                    double[] Ilum_Ratio = new double[3];
                    double[] moonRTSjd = new double[3];
                    double[] moonRShours = new double[3];
                    double[] moonRSangles = new double[3];

                    NativeMethods.Moon_Day_Rise_Set(jDate, ref TJD, Geo, Atmos, ref UT_TT, ref MoonAngle, RSJD, RShours,
                        RSangles, ref Iref);

                    moonRTSjd[0] = RSJD[0];
                    moonRTSjd[2] = RSJD[1];
                    moonRSangles[0] = RSangles[0];
                    moonRSangles[2] = RSangles[1];
                    moonRShours[0] = RShours[0];
                    moonRShours[2] = RShours[1];

                    NativeMethods.moonTransit(jDate, ref TJD, Geo, Atmos, ref UT_TT, ref MoonAngle, ref moonRTSjd[1], ref moonRShours[1],
                        ref moonRSangles[1]);

                    double Hour = 0.0;

                    for (int n = 0; n <= 2; n++)
                    {
                        NativeMethods.Moon_IlumRatio(jDate, ref Hour, ref moonRTSjd[n], ref UT_TT, ref Ilum_Ratio[n]);
                    }


                    if ((bool)DST.IsChecked)
                    {
                        if (TJD > dstJD[0] & TJD < dstJD[1])
                        {
                            for (int n = 0; n <= 2; n++)
                            {
                                if (moonRTSjd[n] != 0.0)
                                {
                                    moonRShours[n] += 1;
                                }
                            }
                        }
                    }

                    string MoonStat = null;

                    for (int m = i - 1; m <= i + 1; m++)
                    {
                        if (m < 0) { m = 0; }
                        for (int n = 0; n <= 3; n++)
                        {
                            if (TJD <= MoonJD[m][n] + Geo[3] / 24.0 & (TJD + 1.0) > MoonJD[m][n] + Geo[3] / 24.0)
                            {
                                if (n == 0)
                                {
                                    MoonStat = " New Moon";
                                    break;
                                }
                                else if (n == 1)
                                {
                                    MoonStat = " First Half";
                                    break;
                                }
                                else if (n == 2)
                                {
                                    MoonStat = " Full Moon";
                                    break;
                                }
                                else
                                {
                                    MoonStat = " Last Half";
                                    break;
                                }
                            }
                        }
                    }

                    w4.MoonCaledar.AppendText(NativeMethods.WeekDays(weekDaynum, 1) + "  " + j.ToString("D2", CultureInfo.CurrentCulture)
                        + "  " + strOmonth + "," + oDay.ToString("D2", CultureInfo.CurrentCulture) + "  " + strHmonth + "," +
                        Hd.ToString("D2", CultureInfo.CurrentCulture) + "   " + Utility.hour2Time(moonRShours[0]) + "-" +
                          moonRSangles[0].ToString("000.00", CultureInfo.CurrentCulture) + "-" + Ilum_Ratio[0].ToString("00.00%", CultureInfo.CurrentCulture)
                          + "   " + Utility.hour2Time(moonRShours[1]) + "-" + moonRSangles[1].ToString("000.00", CultureInfo.CurrentCulture) + "-" +
                           Ilum_Ratio[1].ToString("00.00%", CultureInfo.CurrentCulture) + "   " + Utility.hour2Time(moonRShours[2]) + "-" +
                           moonRSangles[2].ToString("000.00", CultureInfo.CurrentCulture) + "-" + Ilum_Ratio[2].ToString("00.00%", CultureInfo.CurrentCulture) + MoonStat + "\r\n");

                    TJD += 1;
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
                    if (TJD == newHijriJD[k])
                    {
                        k = k + 1;
                        Hd = 1;
                        Hm = Hm + 1;
                        if (Hm > 12)
                        {
                            Hm = 1;
                            Hy = (Hy + 1);
                        }
                    }
                }
                w4.waitWarn.Content = null ;
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
            string yearText = year.Text;
            if ((bool)persCalendar.IsChecked)
            {
                yearText = irYear.Text;
                year1 = year1 - 1;
            }

            int k = 0;
            double JD = NativeMethods.TrueJDEquiSolitice(ref year1, ref k);
            JD = JD + Geo[3] / 24.0;
            int Jyear = 1;
            int Jmonth = 1;
            int Jday = 1;
            double Hours = 0.0;
            NativeMethods.JD2IrCal(ref JD, ref Jyear, ref Jmonth, ref Jday, ref Hours);
            double DJ1 = 0.0;
            int Iyear = 1;
            int Imonth = 1;
            int Iday = 1;
            double FD = 0.0;
            double deltaT = 0.0;
            NativeMethods.JD2Cal(ref JD, ref DJ1, ref Iyear, ref Imonth, ref Iday, ref FD, ref k);
            NativeMethods.iau_DAT(ref Iyear, ref Imonth, ref Iday, ref FD, ref deltaT, ref k);
            deltaT = deltaT + 32.184;
            Hours = FD * 24.0 - deltaT / 3600.0;

            int UT_TT = 1;

            double UJD = 0.0;
            int Dy = 15;

            int Gyear = 0;
            double IUJD = 0.0;
            bool leap = false;
            int[] Equinox = new int[3];
            int MarDay = 20;
            double Uhour = 0.0;

            double[][] MoonJD = new double[15][];
            for (int i = 0; i <= 14; i++)
            {
                MoonJD[i] = new double[4];
            }

            double[] newHijriJD = new double[15];
            int oYear = 1;
            int oMonth = 1;
            int oDay = 1;
            int dayInOtherMonth = 1;
            int dayInMonth = 1;


            if ((bool)gregCalendar.IsChecked)
            {
                int Im = 1;
                int Id = 1;
                double H = 0.0;
                NativeMethods.JULDAT(ref Iyear, ref Im, ref Id, ref H, ref UJD);
                oYear = 1;
                oMonth = 1;
                oDay = 1;
                NativeMethods.JD2IrCal(ref UJD, ref oYear, ref oMonth, ref oDay, ref H);
                if (NativeMethods.GregIsLeapYear(ref Iyear)) { daysGregmonth[1] = 29; }
                NativeMethods.IranCalendar(ref oYear, ref Gyear, ref IUJD, ref leap, Equinox, ref MarDay, ref Uhour);
                if (leap is true) { daysIranmonth[11] = 30; }
                NativeMethods.YearMoonPhases(Iyear, Im, Id, MoonJD);
               
            }
            else if ((bool)persCalendar.IsChecked)
            {
                int Jy = getIrYear();
                NativeMethods.IranCalendar(ref Jy, ref Gyear, ref IUJD, ref leap, Equinox, ref MarDay, ref Uhour);
                int Im = 1;
                int Id = 1;
                double H = 0.0;
                UJD = NativeMethods.IrCal2JD(ref Jy, ref Im, ref Id, ref H);
                NativeMethods.RCALDAT(ref UJD, ref oYear, ref oMonth, ref oDay, ref H);
                if (leap) daysIranmonth[11] = 30;
                if (NativeMethods.GregIsLeapYear(ref Gyear)) { daysGregmonth[1] = 29; }
                NativeMethods.YearMoonPhases(Gyear, oMonth, MarDay, MoonJD);
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

            int Adjust = 0;

            for (int i = 0; i <= 14; i++)
            {
                NativeMethods.HijriAdjust(ref MoonJD[i][0], Geo, ref UT_TT, ref Method, ref B_Ilum,
                    ref aidAccepted, ref newHijriJD[i], ref Adjust);
            }

            int Hy = 1;
            int Hm = 1;
            int Hd = 1;

            for (int n = 0; n <= 3; n++)
            {
                if (UJD < newHijriJD[n])
                {
                    double HJD = newHijriJD[n - 1] + 14.0;
                    NativeMethods.JD2Hijri(ref HJD, ref Hy, ref Hm, ref Hd);
                    Hd = Convert.ToInt32(UJD - newHijriJD[n - 1]) + 1;
                    k = n;
                    break;
                }
            }

            int[] jDate = { Iyear, 1, 1 };

            double TJD = UJD;
            int weekDaynum = NativeMethods.JD2WeekDayNum(TJD + (Uhour+Geo[3]) / 24.0);

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
            + "========================\r\n");                
                w5.sunMoonCaledar.AppendText(string.Format(CultureInfo.CurrentCulture, "{0,75}", mainMonth + " " + yearText + "\r\n"));

                if ((bool)msiseAtm.IsChecked)
                {
                    if (Dy > 365) { Dy = Dy - 365; }
                    Atmos = getAtm(Geo, Dy);

                    w5.sunMoonCaledar.AppendText(" pressure: " + Atmos[0].ToString("F2", CultureInfo.CurrentCulture)
                    + " \t Temperature: " + Atmos[1].ToString("F2", CultureInfo.CurrentCulture) + "\r\n");

                  Dy = Dy + 30;
                }
    
                w5.sunMoonCaledar.AppendText("                                                           Tweilights"
                    + "                SunRise            Noon           SunSet                Tweilights                MoonRise      "
                    + "                MoonSet      Moon Phase\r\n");
                w5.sunMoonCaledar.AppendText("  " + mainMonth + "         " + strOyear + "               " + strHYear
                 + "   Astronomical  Nautical  Civic    Time-Azimuth    Time-Elevation    Time-Azimuth      Civic    Nautical  Astronomical"
                 + "  Time-Azimuth-Ilum    Time-Azimuth-Ilum\r\n");
                w5.sunMoonCaledar.AppendText("------------------------------------------------------------------------------------------------------------"
                 + "-------------------------------------------------------------------------------------------------------------\r\n");

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

                    double[] Times = new double[9];
                    double[] RSTJD = new double[3];
                    double[] RTS_Angles = new double[3];

                    NativeMethods.AstroSolarTimes(ref TJD, jDate, Geo, Atmos, ref UT_TT, ref Iref, Times, RSTJD, RTS_Angles);
        
                    double moonAngle = 0.0;
                    double Hour = 0.0;
                    double[] RSJD = new double[2];
                    double[] RShours = new double[2];
                    double[] RSangles = new double[2];
                    double[] ilumRatio = new double[2];

                    NativeMethods.Moon_Day_Rise_Set(jDate, ref TJD, Geo, Atmos, ref UT_TT, ref moonAngle,
                        RSJD, RShours, RSangles, ref Iref);
        
                    for (int n = 0; n < 2; n++)
                    {
                        RSJD[n] = RSJD[n] - Geo[3] / 24.0;
                        NativeMethods.Moon_IlumRatio(jDate, ref Hour, ref RSJD[n], ref UT_TT, ref ilumRatio[n]);
                    }

                    if ((bool)DST.IsChecked)
                    {
                        if (TJD > dstJD[0] & TJD < dstJD[1])
                        {
                            for (int m = 0; m <= 8; m++)
                            {
                                Times[m] += 1.0;
                            }

                            for (int n = 0; n < 2; n++)
                            {
                                RShours[n] += 1;
                            }
                        }
                    }
        
                    string MoonStat = null;

                    for (int m = i - 1; m <= i + 1; m++)
                    {
                        if (m < 0) { m = 0; }
                        for (int n = 0; n <= 3; n++)
                        {
                            if (TJD <= MoonJD[m][n] + Geo[3] / 24.0 & (TJD + 1.0) > MoonJD[m][n] + Geo[3] / 24.0)
                            {
                                if (n == 0)
                                {
                                    MoonStat = " New Moon";
                                    break;
                                }
                                else if (n == 1)
                                {
                                    MoonStat = " First Half";
                                    break;
                                }
                                else if (n == 2)
                                {
                                    MoonStat = " Full Moon";
                                    break;
                                }
                                else
                                {
                                    MoonStat = " Last Half";
                                    break;
                                }
                            }
                        }
                    }
        
                    w5.sunMoonCaledar.AppendText(NativeMethods.WeekDays(weekDaynum, 2) + "  " + j.ToString("D2", CultureInfo.CurrentCulture)
                    + "  " + strOmonth + "," + oDay.ToString("D2", CultureInfo.CurrentCulture) + "  " + strHmonth + "," +
                    Hd.ToString("D2", CultureInfo.CurrentCulture) + "  " + Utility.hour2Time(Times[0]) + "  " + Utility.hour2Time(Times[1]) + "  " + Utility.hour2Time(Times[2])
                    + "  " + Utility.hour2Time(Times[3]) + "-" + RTS_Angles[0].ToString("000.00", CultureInfo.CurrentCulture)
                    + "  " + Utility.hour2Time(Times[4]) + "-" + RTS_Angles[1].ToString("000.00", CultureInfo.CurrentCulture)
                    + "  " + Utility.hour2Time(Times[5]) + "-" + RTS_Angles[2].ToString("000.00", CultureInfo.CurrentCulture)
                    + "  " + Utility.hour2Time(Times[6]) + "  " + Utility.hour2Time(Times[7]) + "  " + Utility.hour2Time(Times[8])
                    + "  " + Utility.hour2Time(RShours[0]) + "-" + RSangles[0].ToString("000.00", CultureInfo.CurrentCulture)
                    + "-" + ilumRatio[0].ToString("00.00%", CultureInfo.CurrentCulture) + "  " + Utility.hour2Time(RShours[1])
                    + "-" + RSangles[1].ToString("000.00", CultureInfo.CurrentCulture) + "-" + ilumRatio[1].ToString("00.00%", CultureInfo.CurrentCulture) + MoonStat+"\r\n");


                    TJD += 1;
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
                    if (TJD == newHijriJD[k])
                    {
                        k = k + 1;
                        Hd = 1;
                        Hm = Hm + 1;
                        if (Hm > 12)
                        {
                            Hm = 1;
                            Hy = (Hy + 1);
                        }
                    }
                 }

                w5.waitWarn.Content = null ;
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

