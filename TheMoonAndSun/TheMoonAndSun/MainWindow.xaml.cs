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
using ClassMoonSun;


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
            LocalTime.Text = thisTime.ToString("HH:mm:ss");
            UTC.Text = thisTime.ToUniversalTime().ToString("dd/MM/yyyy HH:mm:ss");
            year.Text = thisTime.Year.ToString();
            month.Text = thisTime.Month.ToString();
            day.Text = thisTime.Day.ToString();

            int[] iranDate = Greg2Jalali();

            iranDay.Text = iranDate[2].ToString();
            iranMonth.Text = iranDate[1].ToString();
            iranYear.Text = iranDate[0].ToString();

            try
            {
                string[] geoLocation = System.IO.File.ReadAllLines("location.dat");
                Location.Text = geoLocation[0];
                Longitude.Text = geoLocation[1];
                Latitude.Text = geoLocation[2];
                Elevation.Text = geoLocation[3];
                timeZone.Text = geoLocation[4];

                if(geoLocation[5] == "0")
                {
                    gregCalendar.IsChecked = true;                
                }
                else if(geoLocation[5] == "1")
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

                if (geoLocation[7] == "0")
                {
                    msiseAtm.IsChecked = true;
                    double[] Geo = new double[3];
                    Geo[0] = getLongitude();
                    Geo[1] = getLatitude();
                    Geo[2] = getAltitude();
                    double[] atm = { 10.0, 1010.0 };
                    atm = MoonSun.MSISEatm(thisTime.DayOfYear, Geo);
                    tempBox.Text = atm[1].ToString("F2");
                    pressBox.Text = atm[0].ToString("F2");
                }
                else if (geoLocation[7] == "1")
                {
                    standardAtm.IsChecked = true;
                }
                else if (geoLocation[7] == "2")
                {
                    cusomAtm.IsChecked = true;
                }                

                if(geoLocation[10] == "1") { aidAccept.IsChecked = true; }

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

                if (geoLocation[12] == "0")
                {
                    azanGoo.IsChecked = true;
                }
                else if (geoLocation[12] == "1")
                {
                    tehranGeo.IsChecked = true;
                }
                else if (geoLocation[12] == "2")
                {
                    levaQum.IsChecked = true;
                }
                else if (geoLocation[12] == "3")
                {
                    islamKarachi.IsChecked = true;
                }
                else if (geoLocation[12] == "4")
                {
                    ISNA.IsChecked = true;
                }
                else if (geoLocation[12] == "5")
                {
                    egyptian.IsChecked = true;
                }
                else if (geoLocation[12] == "6")
                {
                    custom.IsChecked = true;
                    Fajr.Text = geoLocation[13];
                    magrib.Text = geoLocation[14];
                    isha.Text = geoLocation[15];
                }

                if (geoLocation[16] == "0")
                {
                    asr1length.IsChecked = true;
                }
                else if (geoLocation[16] == "1")
                {
                    asr2length.IsChecked = true;
                }                 

                if(geoLocation[17] == "1")
                {
                    DST.IsChecked = true;
                    selectRegion.Items.Add("Europe");
                    selectRegion.Items.Add("Iran");
                    selectRegion.Items.Add("North America");
                    selectRegion.Items.Add("Custom");
                    selectRegion.SelectedIndex = getInteger(geoLocation[18]);
                    startDST.Text = geoLocation[19];
                    endDST.Text = geoLocation[20];
                }

            }
            catch (Exception ex)
            {
                errorMsg.Text = ex.Message;
            }

            azanSeletion();
            calculateItmes();
        }


        private void showClick(object sender, RoutedEventArgs e)
        {
            calculateItmes();
        }


        private void calculateItmes()
        {
            if (errorMsg.Text != "")
            {
                errorMsg.Text = "First remove the error";
            }
            else
            {
                const double C = 299792458.0;
                const double AUSEC = 499.0047838061;
                int UT_TT = 1;
                double[] Geo = new double[4];
                string geoName = Location.Text;
                if (geoName == "" || geoName == null) geoName = "---------";
                Geo[0] = getLongitude();
                Geo[1] = getLatitude();
                Geo[2] = getAltitude();
                Geo[3] = getTimeZone();

                double GeoNDST = Geo[3];
                double GeoDST = Geo[3] + 1.0;

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
                MoonSun.JD2IrCal(ref NJD, ref Jy, ref Jm, ref Jd,ref T);
                               
                string IrDayofYear = "Day of Iranian Year :  "+ MoonSun.dayofIyear(ref Jm, ref Jd).ToString();

                double[] Atmos = { 1010.0, 10.0 };                           

                Atmos[0] = getDouble(pressBox.Text);
                Atmos[1] = getDouble(tempBox.Text);

                string atmModel = null;

                if ((bool)msiseAtm.IsChecked)
                {
                    atmModel = " MSISE";
                }
                else if ((bool)standardAtm.IsChecked)
                {
                    atmModel = "Standard";
                }
                else if ((bool)cusomAtm.IsChecked)
                {
                    atmModel = " User defined";
                }

                int[] jDate = { thisDate.Year, thisDate.Month, thisDate.Day };
                double TJD = 0.0;

                int Iref = 1;
                if ((bool)noAirSelect.IsChecked)
                {
                    Iref = 0;
                }

                double[] RTS_Angles = new double[3];
                double[] azanAngles = getAzanAngles();
                double[] isTimes = new double[8];
                double[] isNTJD = new double[8];

                MoonSun.IslamSolarTimes(ref TJD, jDate, Geo, Atmos, azanAngles,ref UT_TT, ref Iref, isTimes, isNTJD, RTS_Angles);

                double[] Times = new double[9];
                double[] RSTJD = new double[3];              

                MoonSun.AstroSolarTimes(ref TJD, jDate, Geo, Atmos,ref UT_TT, ref Iref, Times, RSTJD, RTS_Angles);
                double SunElev = 0.0;
                double Zenit = 0.0;
                double Azim = 0.0;
                double TopoAlfa = 0.0;
                double TopoDelta = 0.0;

                double UJD = NJD - Geo[3] / 24.0;

                MoonSun.Solar_Position(ref UJD, Geo, Atmos,ref UT_TT, ref SunElev, ref Zenit, ref Azim, ref TopoAlfa, ref TopoDelta, ref Iref);

                string sunLocation = "";
                if (SunElev < -0.2666)
                {
                    sunLocation = String.Format(" Below Horizon ({0:00.000})", SunElev);
                }
                else
                {
                    sunLocation = String.Format(" Sun Altitude: {0:00.000} \t Sun Azimuth: {1:000.000}", SunElev, Azim);
                }

                double Tcen = ((UJD - 2451545.0)) / 36525.0;
                double R = 0.0;

                MoonSun.Sun_Earth_Vector(ref Tcen, ref R);
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

                MoonSun.Moon_Day_Rise_Set(jDate, ref TJD, Geo, Atmos, ref UT_TT, ref MoonAngle, RSJD, RShours,
                    RSangles, ref Iref);

                moonJD[0] = RSJD[0] - Geo[3] / 24.0;
                moonJD[2] = RSJD[1] - Geo[3] / 24.0;
                moonRSangles[0] = RSangles[0];
                moonRSangles[2] = RSangles[1];
                moonRShours[0] = RShours[0];
                moonRShours[2] = RShours[1];

                MoonSun.moonTransit(jDate, ref UJD,Geo,  Atmos, ref UT_TT, ref MoonAngle, ref moonJD[1], ref moonRShours[1],
                    ref moonRSangles[1]);

                moonJD[1] = moonJD[1] - Geo[3] / 24.0;
                double Hour = 0.0;

                for (int n = 0; n <= 2; n++)
                {
                    MoonSun.Moon_IlumRatio(jDate, ref Hour, ref moonJD[n], ref UT_TT, ref Ilum_Ratio[n]);
                }
                double delta = 0.0;
                double TTJDT = 0.0;
                double CMoonElev = 0.0;
                double CMoonAzim = 0.0;
                MoonSun.lunar_position(ref UJD, ref UT_TT, ref TTJDT, Geo, Atmos, ref CMoonElev, ref CMoonAzim, ref delta, ref Iref);
                double GeoDia = 0.0;
                double TopoDia = 0.0;
                MoonSun.MoonSemiDia(ref UJD, ref CMoonElev, ref GeoDia, ref TopoDia);

                string moonLocation = "";
                if (CMoonElev < -TopoDia)
                {
                    moonLocation = String.Format("Below Horizon ({0:00.000})", CMoonElev);
                }
                else
                {
                    moonLocation = String.Format("Moon Altitude: {0:00.000} \t Moon Azimuth: {1:000.000}", CMoonElev, CMoonAzim);
                }

                double Landa = 0.0;
                double beta = 0.0;
                double MoonDistance = 0.0;
                double LunPi = 0.0;

                MoonSun.Moon_Mean_LonG_Lat_Dist(ref Tcen, ref Landa, ref beta, ref MoonDistance, ref LunPi);
                
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

               // int day = 1;
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
                        newMoonMon = 12 + newMoonMon ;
                        ye = ye - 1;
                    }
                    int j = 0;
                     MoonSun.Moon_Phases(ref ye, ref newMoonMon, ref jDate[2], ref j, ref NPhMoonJD[i]);
                     j = 2;
                     MoonSun.Moon_Phases(ref ye, ref newMoonMon, ref jDate[2], ref j, ref FPhMoonJD[i]);

                    MoonSun.HijriAdjust(ref NPhMoonJD[i], Geo, ref UT_TT, ref Method, ref B_Ilum, ref aidAccepted,
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
                MoonSun.JULDAT(ref jDate[0], ref jDate[1], ref jDate[2], ref H, ref JD);

                double newHijriJD = 0.0;
                
                for (int l = 0; l <= 4; l++)
                {
                    if (JD < newMoonJD[l])
                    {
                        newHijriJD = newMoonJD[l-1];
                        break;
                    }                     
                }

                double JDH = newHijriJD + 10.0; 
                int Hy = 1;
                int Hm = 1;
                int Hd = 1;                
                MoonSun.JD2Hijri(ref JDH, ref Hy, ref Hm, ref Hd);

                Hd = Convert.ToInt32(JD - newHijriJD) + 1;

                string hijridate = Hd.ToString() + " " + MoonSun.hijriMonthName(Hm - 1, 1) + " " + Hy.ToString();
                

                result.Text = " Location : " + geoName + "\t Longitude: " + Geo[0].ToString("F2")
                 + "\t Latitude: " + Geo[1].ToString("F2") + "\t Elevation: " + Geo[2].ToString("F2")+"\r\n";
                result.AppendText(" Date is: " + MoonSun.JUL2WeekDay(NJD,2)+ " " + thisDate.ToString("dd / MM / yyyy HH:mm:ss")
                 + "\t Iranian Calendar: " + Jd.ToString() + " " + MoonSun.pesianMonthName(Jm - 1, 1) + " " + Jy.ToString()+"\r\n");
                result.AppendText(" Hijri Lunar Calendar: " + hijridate + "\t UT Julian Day: " + UJD.ToString("F4")+"\r\n");
                result.AppendText(" Day of Gregory Year: " + (thisDate.DayOfYear).ToString() + " \t\t " + IrDayofYear+"\r\n");
            
                result.AppendText(atmModel + " Atmospheric model properies for refraction at this Location: \r\n");
                result.AppendText(" Atmospheric Pressure (millibar): " + Atmos[0].ToString("F2") + ", \t "
                 + "Temperature(C) :" + Atmos[1].ToString("F2")+"\r\n");
                result.AppendText(" Islam Prayer Times \r\n");
                result.AppendText(" Fajr: " + hour2Time(isTimes[0]) + "    Asr: " + hour2Time(isTimes[3]) + "    Maghreb: " + hour2Time(isTimes[5])
                 + "    Isha: " + hour2Time(isTimes[6]) + "    Midnight: " + hour2Time(isTimes[7])+"\r\n");
                result.AppendText(" The Sun \r\n");
                result.AppendText("\t Rise Time: " + hour2Time(Times[3]) + "\t Noon Time " + hour2Time(Times[4])
                 + "\t  Set Time: " + hour2Time(Times[5])+"\r\n");
                result.AppendText("\t Rise Azimuth " + RTS_Angles[0].ToString("F3") + "\t Altitude: " + RTS_Angles[1].ToString("F3")
                 + "\t Set Azimuth: " + RTS_Angles[2].ToString("F3") + "\r\n");
                result.AppendText(sunLocation + "\t Sun Earth Distance: " + Earth_Sun_Vector.ToString("F3") + " million kilometers \r\n");
                result.AppendText(" The Moon \r\n");
                result.AppendText( "\t Rise Time: " + hour2Time(moonRShours[0]) + "\t Transit Time " + hour2Time(moonRShours[1])
                 + "\t  Set Time: " + hour2Time(moonRShours[2])+"\r\n");

                result.AppendText("\t Rise Azimuth " + moonRSangles[0].ToString("F3") + "\t Transit Altitude: " + moonRSangles[1].ToString("F3")
                + "\t  Set Azimuth: " + moonRSangles[2].ToString("F3") + "\r\n");
                result.AppendText("\t Rise Phase " + (Ilum_Ratio[0]).ToString("P2") + "\t Transit Phase: " + (Ilum_Ratio[1]).ToString("P2")
                + "\t  Set Phase: " + (Ilum_Ratio[2]).ToString("P2") + "\r\n");
                result.AppendText(" Current Moon Position: " + moonLocation+"\r\n");
                result.AppendText(" Moon Earth Distance: " + MoonDistance.ToString("F3") + " kilometers \r\n");
                result.AppendText(" Days since last new Moon: " + daysinceNewMoon.ToString() + "\t Days to next new Moon: " + daystoNextnewMoon.ToString()+"\r\n");
                result.AppendText(" Days since last Full Moon: " + daysSinceFullMoon.ToString() + "\t Days to next Full Moon: " + daystoNextFullMoon.ToString() + "\r\n");
                result.AppendText(" Twilight Times \r\n");
                result.AppendText(" Morning  Astronomical: " + hour2Time(Times[0]) + "\t Nautical: " + hour2Time(Times[1]) + "\t Civic: " + hour2Time(Times[2])+"\r\n");
                result.AppendText(" Evening  Astronomical: " + hour2Time(Times[8]) + "\t Nautical: " + hour2Time(Times[7]) + "\t Civic: " + hour2Time(Times[6]));
           }

        }    

        private void Longitude_TextChanged(object sender, TextChangedEventArgs e)
        {
            getLongitude();
        }
        

        private void Latitude_TextChanged(object sender, TextChangedEventArgs e)
        {
            getLatitude();
        }



        private void Elevation_TextChanged(object sender, TextChangedEventArgs e)
        {
            getAltitude();
        }


        private void timeZone_TextChanged(object sender, TextChangedEventArgs e)
        {
            getTimeZone();
            DateTime thisDate = getUTDate();
            UTC.Text = thisDate.ToString("dd/MM/yyyy HH:mm:ss");
        }


        private void year_TextChanged(object sender, TextChangedEventArgs e)
        {
            DateTime thisDate = getUTDate();
            UTC.Text = thisDate.ToString("dd/MM/yyyy HH:mm:ss");
            changeCalendar();

        }

        private void month_TextChanged(object sender, TextChangedEventArgs e)
        {
            DateTime thisDate = getUTDate();
            UTC.Text = thisDate.ToString("dd/MM/yyyy HH:mm:ss");
            changeCalendar();
        }

        private void day_TextChanged(object sender, TextChangedEventArgs e)
        {
            DateTime thisDate = getUTDate();
            UTC.Text = thisDate.ToString("dd/MM/yyyy HH:mm:ss");
            changeCalendar();
        }

        private void LocalTime_TextChanged(object sender, TextChangedEventArgs e)
        {
            DateTime thisDate = getUTDate();
            UTC.Text = thisDate.ToString("dd/MM/yyyy HH:mm:ss");
        }



        private void showHelp(object sender, RoutedEventArgs e)
        {
            Window1 w1 = new Window1();
            w1.Show();
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


        private void dateChanged(object sender, SelectionChangedEventArgs e)
        {
            // ... Get DatePicker reference.
            var picker = sender as DatePicker;

            // ... Get nullable DateTime from SelectedDate.
            DateTime? date = picker.SelectedDate;

            year.Text = date.Value.Year.ToString();
            month.Text = date.Value.Month.ToString();
            day.Text = date.Value.Day.ToString();

        }

        private void iranYear_TextChanged(object sender, TextChangedEventArgs e)
        {
            changeCalendar();
        }

        private void iranMonth_TextChanged(object sender, TextChangedEventArgs e)
        {
            changeCalendar();
        }

        private void iranDay_TextChanged(object sender, TextChangedEventArgs e)
        {
            changeCalendar();
        }

        private void changeCalendar()
        {
            try
            {
                if ((bool)gregCalendar.IsChecked)
                {
                    int[] iranDate = Greg2Jalali();
                    iranYear.Text = iranDate[0].ToString();
                    iranMonth.Text = iranDate[1].ToString();
                    iranDay.Text = iranDate[2].ToString();
                }
                else if ((bool)persCalendar.IsChecked)
                {
                    DateTime iranDate = Jalali2Greg();
                    year.Text = iranDate.Year.ToString();
                    month.Text = iranDate.Month.ToString();
                    day.Text = iranDate.Day.ToString();
                }
                else
                {
                    throw new InvalidOperationException("No operator selected");
                }
            }
            catch (InvalidOperationException ioEx)
            {
                errorMsg.Text = ioEx.Message;
            }
            catch (Exception ex)
            {
                errorMsg.Text = ex.Message;
            }

        }

        private void gregCalendar_Checked(object sender, RoutedEventArgs e)
        {
            errorMsg.Text = "";
            changeCalendar();
        }

        private void persCalendar_Checked(object sender, RoutedEventArgs e)
        {
            errorMsg.Text = "";
            changeCalendar();
        }

        private void moonPhase(object sender, RoutedEventArgs e)
        
        {
            Window2 w2 = new Window2();

            double[] Geo = new double[4];
            string geoName = Location.Text;
            if (geoName == "" || geoName == null) geoName = "---------";
            Geo[0] = getLongitude();
            Geo[1] = getLatitude();
            Geo[2] = getAltitude();
            Geo[3] = getTimeZone();

            double GeoNDST = Geo[3];
            double GeoDST = Geo[3] + 1.0;
            double[,] moonPhaseJD = new double[4, 15];
            int day = 1;
            int ye = getYear(year.Text);
            int newMoonMon = 1;
            if ((bool)persCalendar.IsChecked)
            {
                newMoonMon = 3;
                ye = getYear(iranYear.Text) + 621;
            }
            MoonSun.yearMoonPhases(ref ye, ref newMoonMon, ref day, moonPhaseJD);
          
            double JDM = 0.0;
            int Iyear = 1;
            int Imonth = 1;
            int Iday = 1;

            string[] gregDate = new string[4];
            string[] persDate = new string[4];
            string[] time = new string[4];
            double hour = 0.0;

            if ((bool)gregCalendar.IsChecked)
            {
                w2.moonPhaseResult.AppendText("\t\t Moon Phases For Year " + year.Text + "\r\n");
            }
            else if ((bool)persCalendar.IsChecked)
            {
                w2.moonPhaseResult.AppendText("\t\t Moon Phases For Year " + iranYear.Text + "\r\n");
            }

            w2.moonPhaseResult.AppendText("=============================================================================\r\n");
            w2.moonPhaseResult.AppendText("Location: " + geoName + ", Longitude: " + Geo[0].ToString("F2")
                 + " \t Latitude: " + Geo[1].ToString("F2") + "\t Elevation: " + Geo[2].ToString("F2") + "\r\n");
            w2.moonPhaseResult.AppendText("=============================================================================\r\n");
            w2.moonPhaseResult.AppendText("\t\t New Moon \t First Quarter \t Full Moon \t  Last Quarter \r\n");

            string moonPhase = null;
            for (int i = 0; i <= 14; i++)
            {                
                for (int j = 0; j <= 3; j++)
                {                    
                    if ((bool)DST.IsChecked)
                    {
                        double[] dstJD = DSTJD();                        
                        if (moonPhaseJD[j,i] <= dstJD[0] | moonPhaseJD[j,i] >= dstJD[1])
                        {
                            Geo[3] = GeoDST;
                        }
                        else
                        {
                            Geo[3] = GeoNDST;
                        }
                    }

                    JDM = moonPhaseJD[j, i] + Geo[3] / 24.0;
                    Iyear = 1;
                    Imonth = 1;
                    Iday = 1;
                    MoonSun.RCALDAT(ref JDM, ref Iyear, ref Imonth, ref Iday, ref hour);
                    gregDate[j] = String.Format("\t " + Iyear.ToString() + "/" + Imonth.ToString() + "/" +
                        Iday.ToString());

                    MoonSun.JD2IrCal(ref JDM, ref Iyear, ref Imonth, ref Iday, ref hour);
                    persDate[j] = String.Format("\t " + Iyear.ToString() + "/" + Imonth.ToString() + "/" +
                       Iday.ToString());

                    time[j] = hour2Time(hour);                    
                }                
                moonPhase = String.Format("Julian day(UTC)\t {0:F3} \t {1:F3} \t {2:F3} \t {3:F3} \r\n",
                    moonPhaseJD[0,i], moonPhaseJD[1,i], moonPhaseJD[2,i], moonPhaseJD[3,i]);

                w2.moonPhaseResult.AppendText(moonPhase);
                w2.moonPhaseResult.AppendText("Gregory Date " + gregDate[0] + gregDate[1] + gregDate[2] + gregDate[3] + "\r\n");
                w2.moonPhaseResult.AppendText("Iranian Date " + persDate[0] + persDate[1] + persDate[2] + persDate[3] + "\r\n");
                w2.moonPhaseResult.AppendText("Local Time \t " + time[0] + " \t " + time[1] + " \t " + time[2] + " \t " + time[3] + "\r\n");
                w2.moonPhaseResult.AppendText("\r\n");

                newMoonMon = newMoonMon + 1;
            }
            w2.Show();
        }


        private void hijriClick(object sender, RoutedEventArgs e)
        {
            Window3 w3 = new Window3();

            double[] Geo = new double[4];
            string geoName = Location.Text;
            if (geoName == "" || geoName == null) geoName = "---------";
            Geo[0] = getLongitude();
            Geo[1] = getLatitude();
            Geo[2] = getAltitude();
            Geo[3] = getTimeZone();

            double GeoNDST = Geo[3];
            double GeoDST = Geo[3] + 1.0;

            int UT_TT = 1;

            int day = 1;
            int ye = getYear(year.Text);
            int newMoonMon = 0;
            if ((bool)persCalendar.IsChecked)
            {
                newMoonMon = 3;
                ye = getYear(iranYear.Text) + 621;
            }

            double B_Ilum = 0.0035;

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

            string airConditin = null;
            if ((bool)dryAirSelect.IsChecked)
            {
                B_Ilum = 0.0035;
                airConditin = "Air : Dry, Minimum Moon Ilumination = " + B_Ilum.ToString("P2");
            }
            else if ((bool)wetAirSelect.IsChecked)
            {
                B_Ilum = 0.005;
                airConditin = "Air : Humid, Minimum Moon Ilumination = " + B_Ilum.ToString("P2");
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

            bool Status = false;
            int visiStat = 0;
            double[] criteria = new double[12];
            double HJD = 0.0;
            int Iyear = 1;
            int Imonth = 1;
            int Iday = 1;            

            string hijriDate = null;
            
            if ((bool)gregCalendar.IsChecked)
            {
                w3.hijriMonths.AppendText("\t\t\t Hijri Months For Year " + year.Text + "\r\n");
            }
            else if ((bool)persCalendar.IsChecked)
            {
                w3.hijriMonths.AppendText("\t\t\t Hijri Months For Year " + iranYear.Text + "\r\n");
            }

            w3.hijriMonths.AppendText("=================================================================\r\n");
            w3.hijriMonths.AppendText("Location: " + geoName + ", Longitude: " + Geo[0].ToString("F2")
                 + " \t Latitude: " + Geo[1].ToString("F2") + "\t Elevation: " + Geo[2].ToString("F2")
                 + ", Time Zone: " + Geo[3].ToString("F2") + "\r\n");
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

                MoonSun.Moon_Phases(ref ye, ref newMoonMon, ref day, ref m, ref MoonJD);

                HJD = MoonJD - MoonJD % 1.0 - 0.5;
                if ((bool)DST.IsChecked)
                {
                    double[] dstJD = DSTJD();
                    if (HJD <= dstJD[0] | HJD >= dstJD[1])
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
                    MoonSun.MoonVisibility(ref MoonJD, ref HJD, ref UT_TT, Geo, ref Method, ref B_Ilum,
                    ref aidAccepted, ref Status, ref visiStat, criteria);
                    HJD += 1.0;
                    if (Status) break;
                }
                
                double HJD7 = HJD + 7.0;                
                double hour = 0.0;
                Iyear = 1;
                Imonth = 1;
                Iday = 1;
                
                MoonSun.JD2Hijri(ref HJD7, ref Iyear, ref Imonth, ref Iday);
                hijriDate = String.Format("First of " + MoonSun.hijriMonthName(Imonth-1) + " " + Iyear.ToString());
               MoonSun.RCALDAT(ref HJD, ref Iyear, ref Imonth, ref Iday, ref hour);
                string gregHijri = String.Format(Iyear.ToString() + "/" + Imonth.ToString() + "/" +
                       Iday.ToString());
                MoonSun.JD2IrCal(ref HJD, ref Iyear, ref Imonth, ref Iday,ref hour);
                string persHijri = String.Format(Iyear.ToString() + "/" + Imonth.ToString() + "/" +
                   Iday.ToString());

                w3.hijriMonths.AppendText(hijriDate + " \t Gregory: " + gregHijri + "\t Iranian: " +
                    persHijri + "\r\n");
                w3.hijriMonths.AppendText(" Sun set time: " + hour2Time(criteria[11]) + "\t");
                w3.hijriMonths.AppendText(" Moon Azimuth = " + criteria[9].ToString("F2") + "\t"
                + "Moon Elevation = " + criteria[10].ToString("F2") + "\r\n");
                w3.hijriMonths.AppendText("Moon Visibility Criteria \r\n" +
                    "Age = " + hour2Time(criteria[0]) + " , Lag = " + hour2Time(criteria[1])
                    + " , Percent of Moon phase = " + (criteria[2]).ToString("P2")
                    + " , DAZ = " + criteria[3].ToString("F3"));
                w3.hijriMonths.AppendText(" , ARCV = " + criteria[4].ToString("F3")
                   + " , Cos(ARCL) = " + criteria[5].ToString("F4") + ", TopoDia = "
                   + criteria[6].ToString("F3") + ", W_p = " + criteria[7].ToString("F4"));
                w3.hijriMonths.AppendText(" , " + method + criteria[8].ToString("F2") + "\r\n");
                w3.hijriMonths.AppendText("\r\n");

                newMoonMon = newMoonMon + 1;
            }
            w3.Show();
        }

        private void moonSunCalendar(object sender, RoutedEventArgs e)
        {

            Window4 w4 = new Window4();
            
            int[] daysIranmonth = { 31, 31, 31, 31, 31, 31, 30, 30, 30, 30, 30, 29 };
            int[] daysGregmonth = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

            double[] Geo = new double[4];
            string geoName = Location.Text;
            if (geoName == "" || geoName == null) geoName = "---------";
            Geo[0] = getLongitude();
            Geo[1] = getLatitude();
            Geo[2] = getAltitude();
            Geo[3] = getTimeZone();

            double[] Atmos = getAtm(Geo);
            int year1 = 1;
            string fileName =  Location.Text + " MoonSun Calendar";            
                
            if ((bool)gregCalendar.IsChecked)
            {
                year1 = getYear(year.Text);
                fileName = fileName + year.Text;
            }
            else if ((bool)persCalendar.IsChecked)
            {
                year1 = getYear(iranYear.Text) + 621;
                fileName = fileName + iranYear.Text;
            }
            int k = 0;
            double JD = MoonSun.TrueJDEquiSolitice(ref year1, ref k);
            JD = JD + Geo[3] / 24.0;
            int Jyear = 1;
            int Jmonth = 1;
            int Jday = 1;
            double Hours = 0.0;
            MoonSun.JD2IrCal(ref JD, ref Jyear, ref Jmonth, ref Jday, ref Hours);
            double DJ1 = 0.0;
            int Iyear = 1;
            int Imonth = 1;
            int Iday = 1;
            double FD = 0.0;
            double deltaT = 0.0;
            MoonSun.JD2Cal(ref JD, ref DJ1, ref Iyear, ref Imonth, ref Iday, ref FD, ref k);
            MoonSun.iau_DAT(ref Iyear, ref Imonth, ref Iday, ref FD, ref deltaT, ref k);
            deltaT = deltaT + 32.184;
            Hours = FD * 24.0 - deltaT / 3600.0;

            int UT_TT = 1;

            int ye = getYear(year.Text);

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
                airConditin = "Air : Dry, Minimum Moon Ilumination = " + B_Ilum.ToString("P2");
            }
            else if ((bool)wetAirSelect.IsChecked)
            {
                B_Ilum = 0.005;
                airConditin = "Air : Humid, Minimum Moon Ilumination = " + B_Ilum.ToString("P2");
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

            double[] dstJD = new double[2];
            if ((bool)DST.IsChecked)
            {
                dstJD = DSTJD();
            }

            double UJD = 0.0;
            int Dy = 15;

            int[] Gyear = new int[5];
            double[] IUJD = new double[5];
            int leap = 1;
            int[] Equinox = new int[3];
            int[] MarDay = new int[3];
            double[] Uhour = new double[5];
            double[,] MoonJD = new double[4,15];
            double[] newHijriJD = new double[15];
            int oYear = 1;
            int oMonth = 1;
            int oDay = 1;
            int dayInOtherMonth = 1;
            int dayInMonth = 1;

            string path = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments);
            fileName = path + "\\" + fileName;
            string dummyName = fileName + ".txt";
            for (int i = 1; i < 100; i++)
            {
                if (System.IO.File.Exists(dummyName))
                {
                    dummyName = fileName + "(" + i.ToString() + ")" + ".txt";
                }
                else
                {
                    break;
                }
            }
            fileName = dummyName;
            w4.fileNameBox.Text = fileName;

            w4.Show();
            using (System.IO.StreamWriter file = new System.IO.StreamWriter(fileName, true))
            {

                if ((bool)gregCalendar.IsChecked)
                {
                    int Im = 1;
                    int Id = 1;
                    double H = 0.0;
                    MoonSun.JULDAT(ref Iyear, ref Im, ref Id, ref H, ref UJD);
                    oYear = 1;
                    oMonth = 1;
                    oDay = 1;
                    MoonSun.JD2IrCal(ref UJD, ref oYear, ref oMonth, ref oDay, ref H);
                    if (MoonSun.GregIsLeapYear(ref Iyear)) { daysGregmonth[1] = 29; }
                    MoonSun.IranCalendar(ref oYear, Gyear, IUJD, ref leap, Equinox, MarDay, Uhour);
                    if (leap == oYear) daysIranmonth[11] = 30;
                    MoonSun.yearMoonPhases(ref Iyear, ref Im, ref Id, MoonJD);     
                    file.WriteLine(string.Format("{0,90}", " Sun & Moon Caledar For Year " + year.Text));
                }
                else if ((bool)persCalendar.IsChecked)
                {
                    int Jy = getYear(iranYear.Text);
                    MoonSun.IranCalendar(ref Jy, Gyear, IUJD, ref leap, Equinox, MarDay, Uhour);
                    int Im = 1;
                    int Id = 1;
                    double H = 0.0;
                    UJD = MoonSun.IrCal2JD(ref Jy, ref Im, ref Id, ref H);
                    MoonSun.RCALDAT(ref UJD, ref oYear, ref oMonth, ref oDay, ref H);
                    if (leap == Jy) daysIranmonth[11] = 30;
                    if (MoonSun.GregIsLeapYear(ref Gyear[2])) { daysGregmonth[1] = 29; }
                    MoonSun.yearMoonPhases(ref Gyear[2], ref oMonth, ref MarDay[2], MoonJD);
                    Dy = Dy + daysGregmonth[0] + daysGregmonth[1] + MarDay[2];    
                    file.WriteLine(string.Format("{0,90}", " Sun & Moon Caledar For Year " + iranYear.Text));
                }
    
                file.WriteLine( "======================================================================================"
                    + "=============================================================================================="
                    + "==================================");
                file.WriteLine(" Location: " + geoName + ", Longitude: " + Geo[0].ToString("F2") + " \t Latitude: " + Geo[1].ToString("F2")
                    + "\t Elevation: " + Geo[2].ToString("F2") + ", Time Zone: " + Geo[3].ToString("F2"));
                file.WriteLine(airConditin + ", " + aidString + "\t" + method);
                file.WriteLine(" New year at: " + hour2Time(Hours) + ", " + Jday.ToString() + " " + MoonSun.pesianMonthName(Jmonth - 1, 1)
                    + " " + Jyear.ToString() + " \t " + Iday.ToString() + " " + MoonSun.gregoryMonthName(Imonth - 1, 1) + " " +
                    Iyear.ToString());  
                
                if ((bool)msiseAtm.IsChecked == false)
                {
       
                    file.WriteLine(" Atmospheric properties for refraction: "
                    + "Pressure(millibar): " + Atmos[0].ToString("F2") + ", " + "Temperature(C) :"
                    + Atmos[1].ToString("F2"));
                    
                }

                int Adjust = 0;

                for (int i = 0; i <= 14; i++)
                {
                    MoonSun.HijriAdjust(ref MoonJD[0, i], Geo, ref UT_TT, ref Method, ref B_Ilum,
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
                        MoonSun.JD2Hijri(ref HJD, ref Hy, ref Hm, ref Hd);
                        Hd = Convert.ToInt32(UJD - newHijriJD[n - 1]) + 1;
                        k = n;
                        break;
                    }
                }

                int[] jDate = { Iyear, 1, 1 };
                int Iref = 1;
                if ((bool)noAirSelect.IsChecked)
                {
                    Iref = 0;
                }

                double TJD = UJD;
                int weekDaynum = MoonSun.JD2WeekDayNum(TJD+Geo[3]/24.0);

                for (int i = 0; i < 12; i++)
                {
                    string strOyear = null;
                    string mainMonth = null;
                    string strOmonth = null;
                    string strHmonth = null;

                    if ((bool)gregCalendar.IsChecked)
                    {
                        mainMonth = MoonSun.gregoryMonthName(i);
                        strOyear = oYear.ToString();
                        dayInMonth = daysGregmonth[i];
                        dayInOtherMonth = daysIranmonth[oMonth - 1];
                    }
                    else if ((bool)persCalendar.IsChecked)
                    {
                        mainMonth = MoonSun.pesianMonthName(i);
                        strOyear = oYear.ToString();
                        dayInMonth = daysIranmonth[i];
                        dayInOtherMonth = daysGregmonth[oMonth - 1];
                    }

                    
                 //   int Hdays = Convert.ToInt32(newHijriJD[k] - newHijriJD[k - 1]);
                    string strHYear = Hy.ToString();  

                    file.WriteLine(
                 " ================================================================================================"
                + "================================================================================================"
                + "========================");
                 file.WriteLine(string.Format("{0,75}", mainMonth));


                    if ((bool)msiseAtm.IsChecked)
                    {
                        if (Dy > 365) { Dy = Dy - 365; }
                        Atmos = getAtm(Geo, Dy);

                        file.WriteLine(" pressure: " + Atmos[0].ToString("F2")
                        + " \t Temperature: " + Atmos[1].ToString("F2"));
                        
                        Dy = Dy + 30;
                    }
    
                    file.WriteLine("                                                           Tweilights"
                        + "                SunRise            Noon           SunSet                Tweilights                MoonRise      "
                        + "                MoonSet      Moon Phase");
                       file.WriteLine("  " + mainMonth + "         " + strOyear + "               " + strHYear
                        + "   Astronomical  Nautical  Civic    Time-Azimuth    Time-Elevation    Time-Azimuth      Civic    Nautical  Astronomical"
                        + "  Time-Azimuth-Ilum    Time-Azimuth-Ilum");
                       file.WriteLine("------------------------------------------------------------------------------------------------------------"
                        + "-------------------------------------------------------------------------------------------------------------");
                    
                    for (int j = 1; j <= dayInMonth; j++)
                    {

                        if (weekDaynum > 6) { weekDaynum = 0; }

                        if (oDay == 1 | j == 1)
                        {
                            if ((bool)gregCalendar.IsChecked)
                            {
                                strOmonth = MoonSun.pesianMonthName(oMonth - 1, 1);
                            }
                            else if ((bool)persCalendar.IsChecked)
                            {
                                strOmonth = MoonSun.gregoryMonthName(oMonth - 1, 1);
                            }
                        }
                        else
                        {
                            if ((bool)gregCalendar.IsChecked)
                            {
                                strOmonth = string.Format("{0,12}", "#  ");
                            }
                            else if ((bool)persCalendar.IsChecked)
                            {
                                strOmonth = string.Format("{0,9}", "#  ");
                            }
                        }

                        if (Hd == 1 | j == 1)
                        {
                            strHmonth = MoonSun.hijriMonthName(Hm - 1, 1);
                        }
                        else
                        {
                            strHmonth = string.Format("{0,18}", "#  ");
                        }

                        double[] Times = new double[9];
                        double[] RSTJD = new double[3];
                        double[] RTS_Angles = new double[3];

                        MoonSun.AstroSolarTimes(ref TJD, jDate, Geo, Atmos, ref UT_TT, ref Iref, Times, RSTJD, RTS_Angles);

                        double moonAngle = 0.0;
                        double Hour = 0.0;
                        double[] RSJD = new double[2];
                        double[] RShours = new double[2];
                        double[] RSangles = new double[2];
                        double[] ilumRatio = new double[2];

                        MoonSun.Moon_Day_Rise_Set(jDate, ref TJD, Geo, Atmos, ref UT_TT, ref moonAngle,
                            RSJD, RShours, RSangles, ref Iref);

                        for (int n = 0; n < 2; n++)
                        {
                            RSJD[n] = RSJD[n] - Geo[3] / 24.0;
                            MoonSun.Moon_IlumRatio(jDate, ref Hour, ref RSJD[n], ref UT_TT, ref ilumRatio[n]);                            
                        }

                        if ((bool)DST.IsChecked)
                        {
                            if (TJD > dstJD[0] & TJD < dstJD[1])
                            {
                                for (int m = 0; m < 8; m++)
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
                                if (TJD <= MoonJD[n, m] + Geo[3] / 24.0 & (TJD + 1.0) > MoonJD[n, m] + Geo[3] / 24.0)
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
    
                        file.WriteLine(MoonSun.WeekDays(weekDaynum, 2) + "  " + j.ToString("D2")
                        + "  " + strOmonth + "," + oDay.ToString("D2") + "  " + strHmonth + "," + Hd.ToString("D2")
                        + "  " + hour2Time(Times[0]) + "  " + hour2Time(Times[1]) + "  " + hour2Time(Times[2])
                        + "  " + hour2Time(Times[3]) + "-" + RTS_Angles[0].ToString("000.00")
                        + "  " + hour2Time(Times[4]) + "-" + RTS_Angles[1].ToString("000.00")
                        + "  " + hour2Time(Times[5]) + "-" + RTS_Angles[2].ToString("000.00")
                        + "  " + hour2Time(Times[6]) + "  " + hour2Time(Times[7]) + "  " + hour2Time(Times[8])
                        + "  " + hour2Time(RShours[0]) + "-" + RSangles[0].ToString("000.00")
                        + "-" + ilumRatio[0].ToString("00.00%") + "  " + hour2Time(RShours[1])
                        + "-" + RSangles[1].ToString("000.00") + "-" + ilumRatio[1].ToString("00.00%") + MoonStat);

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
                }
            }

            w4.statusBox.AppendText(" Finished writing");
        }

        private void islamcalnedar(object sender, RoutedEventArgs e)
        {
            Window5 w5 = new Window5();
            int[] daysIranmonth = { 31, 31, 31, 31, 31, 31, 30, 30, 30, 30, 30, 29 };
            int[] daysGregmonth = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };            

            double[] Geo = new double[4];
            string geoName = Location.Text;
            if (geoName == "" || geoName == null) geoName = "---------";
            Geo[0] = getLongitude();
            Geo[1] = getLatitude();
            Geo[2] = getAltitude();
            Geo[3] = getTimeZone();        

            double[] Atmos = getAtm(Geo);
            int year1 = 1;
            if ((bool)gregCalendar.IsChecked)
            {
                year1 = getYear(year.Text);
            }
            else if ((bool)persCalendar.IsChecked)
            {
                year1 = getYear(iranYear.Text) + 621;
            }
            int k = 0;
            double JD = MoonSun.TrueJDEquiSolitice(ref year1, ref k);
            JD = JD + Geo[3] / 24.0;
            int Jyear = 1;
            int Jmonth = 1;
            int Jday = 1;
            double Hours = 0.0;
            MoonSun.JD2IrCal(ref JD, ref Jyear, ref Jmonth, ref Jday, ref Hours);
            double DJ1 = 0.0;
            int Iyear = 1;
            int Imonth = 1;
            int Iday = 1;
            double FD = 0.0;
            double deltaT = 0.0;
            MoonSun.JD2Cal(ref JD, ref DJ1, ref Iyear, ref Imonth, ref Iday, ref FD, ref k);
            MoonSun.iau_DAT(ref Iyear, ref Imonth, ref Iday, ref FD, ref deltaT, ref k);
            deltaT = deltaT + 32.184;
            Hours = FD * 24.0 - deltaT / 3600.0;

            int UT_TT = 1;        

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
                airConditin = "Air : Dry, Minimum Moon Ilumination = " + B_Ilum.ToString("P2");
            }
            else if ((bool)wetAirSelect.IsChecked)
            {
                B_Ilum = 0.005;
                airConditin = "Air : Humid, Minimum Moon Ilumination = " + B_Ilum.ToString("P2");
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

            double[] dstJD = new double[2];
            if ((bool)DST.IsChecked)
            {
                dstJD = DSTJD();                
            }

            double[] azanAngles = getAzanAngles(); // { -17.7, -4.5, -14.0, 1.0 };
            double UJD = 0.0;
            int Dy = 15;

            int[] Gyear = new int[5];
            double[] IUJD = new double[5];
            int leap = 1;
            int[] Equinox = new int[3];
            int[] MarDay = new int[3];
            double[] Uhour = new double[5];
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
                MoonSun.JULDAT(ref Iyear, ref Im, ref Id, ref H, ref UJD);
                oYear = 1;
                oMonth = 1;
                oDay = 1;
                MoonSun.JD2IrCal(ref UJD, ref oYear, ref oMonth, ref oDay, ref H);
                if (MoonSun.GregIsLeapYear(ref Iyear)) { daysGregmonth[1] = 29; }
                MoonSun.IranCalendar(ref oYear, Gyear, IUJD, ref leap, Equinox, MarDay, Uhour);
                if (leap == oYear) daysIranmonth[11] = 30;
                MoonSun.hijriMonths(ref Iyear, ref Im, ref Id, Geo, ref UT_TT, ref Method, ref B_Ilum,
                    ref aidAccepted, newHijriJD);
                w5.islamCalendar.AppendText(string.Format("{0,75}","Islamic Caledar For Year " + year.Text + "\r\n"));
                
            }
            else if ((bool)persCalendar.IsChecked)
            {
                int Jy = getYear(iranYear.Text);
                MoonSun.IranCalendar(ref Jy, Gyear, IUJD, ref leap, Equinox, MarDay, Uhour);                
                int Im = 1;
                int Id = 1;
                double H = 0.0;
                UJD = MoonSun.IrCal2JD(ref Jy, ref Im, ref Id, ref H);
                MoonSun.RCALDAT(ref UJD, ref oYear, ref oMonth, ref oDay, ref H);
                if (leap == Jy) daysIranmonth[11] = 30;
                if (MoonSun.GregIsLeapYear(ref Gyear[2])) { daysGregmonth[1] = 29; }
                MoonSun.hijriMonths(ref Gyear[2], ref oMonth, ref oDay, Geo, ref UT_TT, ref Method, ref B_Ilum,
                    ref aidAccepted, newHijriJD);
                Dy = Dy + daysGregmonth[0] + daysGregmonth[1] + MarDay[2];
                w5.islamCalendar.AppendText(string.Format("{0,75}", "Islamic Caledar For Year " + iranYear.Text + "\r\n"));            
            }           


            int Hy = 1;
            int Hm = 1;
            int Hd = 1;
            for (int n = 0; n <= 3; n++)
            {
                if (UJD < newHijriJD[n])
                {
                    double HJD = newHijriJD[n - 1] + 14.0;
                    MoonSun.JD2Hijri(ref HJD, ref Hy, ref Hm, ref Hd);
                    Hd = Convert.ToInt32(UJD - newHijriJD[n - 1]) + 1;
                    k = n;
                    break;
                }
            }
            int[] jDate = { Iyear, 1, 1 };
            int Iref = 1;
            if ((bool)noAirSelect.IsChecked)
            {
                Iref = 1;
            }          

            double TJD = UJD;
            int weekDaynum = MoonSun.JD2WeekDayNum(TJD+Geo[3]/24.0);
            w5.islamCalendar.AppendText("======================================================================");
            w5.islamCalendar.AppendText("=========================================================\r\n");
            w5.islamCalendar.AppendText(" Location: " + geoName + ", Longitude: " + Geo[0].ToString("F2")
                 + " \t Latitude: " + Geo[1].ToString("F2") + "\t Elevation: " + Geo[2].ToString("F2")
                 + ", Time Zone: " + Geo[3].ToString("F2") + "\r\n");
            if ((bool)msiseAtm.IsChecked == false)
            {
                w5.islamCalendar.AppendText("\r\n  Atmospheric properties for refraction: "
                 + "Pressure(millibar): " + Atmos[0].ToString("F2") + ", " + "Temperature(C) :"
                 + Atmos[1].ToString("F2") + "\r\n");
            }
            w5.islamCalendar.AppendText(airConditin + ", " + aidString+"\t"+ method + "\r\n"
            + "Sun elevation at prayer times: \t" + "Fajr: " + azanAngles[0].ToString("F2") + "\t Marghreb: "
            + azanAngles[1].ToString("F2") + " \t Isha: " + azanAngles[2].ToString("F2"));

            if (azanAngles[3] == 1.0)
            {
                w5.islamCalendar.AppendText("\t Asr: Standard");
            }
            else
            {
                w5.islamCalendar.AppendText("\t Asr: Hanafi ");
            }
            w5.islamCalendar.AppendText("\t Midnight: Astronomical   "+"\r\n"+"\r\n");       
           
            w5.islamCalendar.AppendText(" New year at: " + hour2Time(Hours) + ", "
                + Jday.ToString() + " " + MoonSun.pesianMonthName(Jmonth - 1, 1) + " " + Jyear.ToString()
                + " \t " + Iday.ToString() + " " + MoonSun.gregoryMonthName(Imonth - 1, 1) + " " 
                + Iyear.ToString()+ "\r\n");           

            for (int i = 0; i < 12; i++)
            {
  
                string strOyear = null;            
                string mainMonth = null;
                string strOmonth = null;
                string strHmonth = null;

                if ((bool)gregCalendar.IsChecked)
                {
                    mainMonth = MoonSun.gregoryMonthName(i);                      
                    strOyear = oYear.ToString();               
                    dayInMonth = daysGregmonth[i];
                    dayInOtherMonth = daysIranmonth[oMonth-1];
                }
                else if ((bool)persCalendar.IsChecked)
                {
                    mainMonth = MoonSun.pesianMonthName(i);
                    strOyear = oYear.ToString();               
                    dayInMonth = daysIranmonth[i];
                    dayInOtherMonth = daysGregmonth[oMonth-1];
                }
             
                string strHYear = Hy.ToString();

                w5.islamCalendar.AppendText("======================================================================"
                + "=========================================================\r\n");
                w5.islamCalendar.AppendText(string.Format("{0,75}", mainMonth) + "\r\n");

                if ((bool)msiseAtm.IsChecked)
                {
                    if (Dy > 365) { Dy = Dy - 365; }
                    Atmos = getAtm(Geo, Dy);
                    w5.islamCalendar.AppendText( " pressure: " + Atmos[0].ToString("F2")
                        + " \t Temperature: " + Atmos[1].ToString("F2") + "\r\n");                    
                    Dy = Dy + 30;
                }
                w5.islamCalendar.AppendText("  " + mainMonth + "       " + strOyear + "             " + strHYear
                + "             Fajr       Rise      Noon       Asr      Set      Maghrb   Isha    Midnight \r\n");
                w5.islamCalendar.AppendText("----------------------------------------------------------------------"
                + "--------------------------------------------------------------\r\n");

                for (int j = 1; j <= dayInMonth; j++)
                {
                    
                    if (weekDaynum > 6) { weekDaynum = 0; }                    

                    if (oDay == 1 | j == 1)
                    {
                        if ((bool)gregCalendar.IsChecked)
                        {
                            strOmonth = MoonSun.pesianMonthName(oMonth - 1, 1);
                        }
                        else if ((bool)persCalendar.IsChecked)
                        {
                            strOmonth = MoonSun.gregoryMonthName(oMonth - 1, 1);
                        }
                    }
                    else
                    {
                        if ((bool)gregCalendar.IsChecked)
                        {
                            strOmonth = string.Format("{0,12}", "#  ");
                        }
                        else if ((bool)persCalendar.IsChecked)
                        {
                            strOmonth = string.Format("{0,9}", "#  ");
                        }
                    } 
                    
                    if(Hd == 1 | j == 1)
                    {
                        strHmonth = MoonSun.hijriMonthName(Hm - 1, 1);
                    }
                    else
                    {
                        strHmonth = string.Format("{0,18}", "#  ");
                    }                     
                    

                    double[] Times = new double[8];
                    double[] RSTJD = new double[8];
                    double[] RTS_Angles = new double[3];
                    MoonSun.IslamSolarTimes(ref TJD, jDate, Geo, Atmos, azanAngles, ref UT_TT, ref Iref, Times, RSTJD, RTS_Angles);

                    if ((bool)DST.IsChecked)
                    {                        
                        if (TJD > dstJD[0] & TJD < dstJD[1])
                        {
                            for (int m = 0; m < 8; m++)
                            {
                                Times[m] += 1.0;
                            }
                        }                        
                    }

                    w5.islamCalendar.AppendText(MoonSun.WeekDays(weekDaynum, 2) + "  " + j.ToString("D2")
                    + "  " + strOmonth + "," + oDay.ToString("D2") + "  " + strHmonth + "," + Hd.ToString("D2")
                    + "  " + hour2Time(Times[0])+"  "+ hour2Time(Times[1])+"  "+ hour2Time(Times[2])
                    + "  " + hour2Time(Times[3])+"  "+ hour2Time(Times[4])+"  "+ hour2Time(Times[5])
                    + "  " + hour2Time(Times[6])+"  "+ hour2Time(Times[7]) + "\r\n");

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
            }        
            w5.Show();            
        }

        private static string hour2Time(double Hour)
        {
            int H = 0;//= Convert.ToInt32(Hour - Hour % 1);
            int M = 0;//= Convert.ToInt32((Hour % 1) * 60.0 - ((Hour % 1) * 60.0) % 1);
            int S = 0;//= Convert.ToInt32((((Hour % 1) * 60.0) % 1) * 60.0);
            MoonSun.Hour2HMS(ref Hour, ref H, ref M, ref S);
            string timeFormat = String.Format("{0:00}:{1:00}:{2:00}", H, M, S);
            return timeFormat;

        }

        private  double getDouble(String input)
        {
            double output = 0.0;
            try
            {
                if (input != null)
                {
                    output = double.Parse(input);
                }
                else
                {
                    output = 0.0;
                }

            }
            catch (FormatException fEx)
            {
                errorMsg.Text = fEx.Message;
            }

            catch (InvalidOperationException ioEx)
            {
                errorMsg.Text = ioEx.Message;
            }
            catch (Exception ex)
            {
                errorMsg.Text = ex.Message;
            }

            return output;
        }

        private int getInteger(String input)
        {
            int output = 0;
            try
            {
                if (input != null)
                {
                    output = int.Parse(input);
                }

                else
                {
                    output = 0;
                }

            }
            catch (FormatException fEx)
            {
                errorMsg.Text = fEx.Message;
            }

            catch (InvalidOperationException ioEx)
            {
                errorMsg.Text = ioEx.Message;
            }
            catch (Exception ex)
            {
                errorMsg.Text = ex.Message;
            }

            return output;
        }

        private double getHour()
        {
            DateTime time = new DateTime();
            double Hour = 0.0;
            try
            {
                time = DateTime.Parse(LocalTime.Text);
                if (time != null)
                {
                    Hour = time.Hour + Convert.ToInt32(time.Minute) / 60.0 + Convert.ToInt32(time.Second) / 3600.0;
                }

            }
            catch (FormatException fEx)
            {
                errorMsg.Text = fEx.Message;
            }

            catch (Exception ex)
            {
                errorMsg.Text = ex.Message;
            }

            return Hour;
        }

        private double getLongitude()
        {
            double longi = getDouble(Longitude.Text);
            try
            {
                if (longi > 180.0 || longi < -180.0)
                {
                    Longitude.Text = "";
                    longi = 0.0;
                    throw new Exception("Longitude Out of Limits");

                }
                else
                {
                    errorMsg.Text = "";
                    hourAngle.Text = " Hour Angle of Longitude = " +
                    (longi / 15.0).ToString("F2");
                }
            }
            catch (Exception ex)
            {
                errorMsg.Text = ex.Message;
            }

            return longi;
        }

        private double getLatitude()
        {
            double lati = getDouble(Latitude.Text);
            try
            {

                if (lati > 90.0 || lati < -90.0)
                {
                    Latitude.Text = "";
                    lati = 0.0;
                    throw new Exception("Latitude Out of Limits");
                }

                else
                {
                    errorMsg.Text = "";
                }
            }
            catch (Exception ex)
            {
                errorMsg.Text = ex.Message;
            }

            return lati;
        }

        private double getAltitude()
        {
            double elev = getDouble(Elevation.Text);
            try
            {
                if (elev > 9000.0 || elev < -29.0)
                {
                    Elevation.Text = "";
                    elev = 0.0;
                    throw new Exception("Elevation out of Limits");
                }
                else
                {
                    errorMsg.Text = "";
                }
            }
            catch (Exception ex)
            {
                errorMsg.Text = ex.Message;
            }

            return elev;
        }

        private double getTimeZone()
        {
            double tz = getDouble(timeZone.Text);
            try
            {
                if (tz > 12.0 || tz < -12.0)
                {
                    timeZone.Text = "";
                    tz = 0.0;
                    throw new Exception("TimeZone out of Limits");
                }
                else
                {
                    errorMsg.Text = "";
                }
            }
            catch (Exception ex)
            {
                errorMsg.Text = ex.Message;
            }

            return tz;
        }

        private int getYear(string input)
        {
            int y = getInteger(input);
            try
            {
                if (y > 3000 || y < 1)
                {
                    // year.Text = "";
                    y = 1;
                    throw new Exception("Year out of Limits");
                }
                else
                {
                    errorMsg.Text = "";
                }
            }
            catch (Exception ex)
            {
                errorMsg.Text = ex.Message;
            }

            return y;
        }

        private int getMonth(string input)
        {
            int m = getInteger(input);
            try
            {
                if (m > 12 || m < 1)
                {
                    // month.Text = "";
                    m = 1;
                    throw new Exception("Wrong month number");
                }
                else
                {
                    errorMsg.Text = "";
                }
            }
            catch (Exception ex)
            {
                errorMsg.Text = ex.Message;
            }

            return m;
        }

        private int getDay()
        {
            int d = getInteger(day.Text);
            int Iyear = getYear(year.Text);
            int Imonth = getMonth(month.Text);
            int days = DateTime.DaysInMonth(Iyear, Imonth);

            try
            {
                if (d > days || d < 1)
                {
                    day.Text = "";
                    d = 1;
                    throw new Exception("Wrong day in month");
                }
                else
                {
                    errorMsg.Text = "";
                }
            }
            catch (Exception ex)
            {
                errorMsg.Text = ex.Message;
            }

            return d;
        }

        private int getIranianDay()
        {
            int d = getInteger(iranDay.Text);
            int Iyear = getYear(iranYear.Text);
            int Imonth = getMonth(iranMonth.Text);
            int[] days = { 31, 31, 31, 31, 31, 31, 30, 30, 30, 30, 30, 29 };
            int[] Gy = new int[5];
            int[] Equinox = new int[3];
            int Leap = 0;
            int[] Marday = new int[5];
            double[] Uhour = new double[5];
            double[] NJD = new double[5];

            MoonSun.IranCalendar(ref Iyear, Gy, NJD ,ref Leap, Equinox, Marday, Uhour);
            if (Leap == 0) days[11] = 30;

            try
            {
                if (d > days[Imonth - 1] || d < 1)
                {
                    iranDay.Text = "";
                    d = 1;
                    throw new Exception("Wrong day in month");
                }
                else
                {
                    errorMsg.Text = "";
                }
            }
            catch (Exception ex)
            {
                errorMsg.Text = ex.Message;
            }

            return d;
        }

        private double julianDay()
        {
            DateTime dateIn = getDate();
            double NTJD = 0.0;
            int Y = dateIn.Year;
            int M = dateIn.Month;
            int D = dateIn.Day;
            double H = Convert.ToDouble(dateIn.Hour) + Convert.ToDouble(dateIn.Minute / 60.0)
                + Convert.ToDouble(dateIn.Second / 3600.0);
            MoonSun.JULDAT(ref Y, ref M, ref D, ref H, ref NTJD);

            return NTJD;
        }

        private int[] getTime()
        {
            int[] thisTime = { 0, 0, 0 };

            try
            {
                string timeStr = LocalTime.Text;
                int timeStrlen = timeStr.Length;
                if (timeStrlen > 8)
                {

                    throw new Exception("Wrong Time Format");
                }
                else
                {
                    errorMsg.Text = "";
                    timeStr = timeStr + "..";
                    string delimStr = " ,.:";
                    char[] delimiter = delimStr.ToCharArray();
                    string[] split = null;

                    split = timeStr.Split(delimiter, 4);

                    int H = getInteger(split[0]);
                    if (H >= 24 || H < 0)
                    {
                        throw new Exception("Wrong Hours");
                    }
                    int M = getInteger(split[1]);
                    if (M >= 60 || M < 0)
                    {
                        throw new Exception("Wrong Minutes");
                    }
                    int S = getInteger(split[2]);

                    if (S >= 60 || S < 0)
                    {
                        throw new Exception("Wrong Seconds");
                    }

                    int[] thistime = { H, M, S };
                    return thistime;
                }
            }
            catch (Exception ex)
            {
                errorMsg.Text = ex.Message;
            }

            return thisTime;
        }

        private DateTime getDate()
        {

            DateTime thisDate = new DateTime();
            int Y = getYear(year.Text);
            int M = getMonth(month.Text);
            int D = getDay();

            int[] time = getTime();
            thisDate = new DateTime(Y, M, D, time[0], time[1], time[2]);

            return thisDate;
        }

        private int[] getIranianDate()
        {
            int Y = getYear(iranYear.Text);
            int M = getMonth(iranMonth.Text);
            int D = getIranianDay();

            int[] time = getTime();

            int[] thisDate = { Y, M, D, time[0], time[1], time[2] };

            return thisDate;
        }

        private DateTime Jalali2Greg()
        {
            PersianCalendar persian = new PersianCalendar();
            DateTime thisDate = new DateTime();

            int Y = getYear(iranYear.Text);
            int M = getMonth(iranMonth.Text);
            int D = getIranianDay();

            int[] time = getTime();

            thisDate = new DateTime(Y, M, D, time[0], time[1], time[2], persian);

            return thisDate;
        }

        private int[] Greg2Jalali()
        {
            DateTime date1 = getDate();

            int Y = date1.Year;
            int M = date1.Month;
            int D = date1.Day;
            double H = 12.0;
            double JD = 0.0;

            MoonSun.JULDAT(ref Y, ref M, ref D, ref H, ref JD);
            int Jy = 1;
            int Jm = 1;
            int Jd = 1;
            MoonSun.JD2IrCal(ref JD, ref Jy, ref Jm, ref Jd, ref H);

            int[] jalaliDate = { Jy, Jm, Jd, date1.Hour, date1.Minute, date1.Second };

            return jalaliDate;
        }


        private DateTime getUTDate()
        {
            DateTime thisDate = getDate();
            double tz = getTimeZone();
            int H = 0;
            int M = 0;
            int S = 0;
            MoonSun.Hour2HMS(ref tz, ref H, ref M, ref S);
            TimeSpan tSpan = new System.TimeSpan(0, H, M, S);

            DateTime output = thisDate - tSpan;
            return output;
        }       

       
        private void saveLocation(object sender, RoutedEventArgs e)
        {
            string[] geoLocation = new string[21];
            geoLocation[0] = Location.Text;    
            geoLocation[1] = Longitude.Text;
            geoLocation[2] = Latitude.Text;
            geoLocation[3] = Elevation.Text;
            geoLocation[4] = timeZone.Text;

            if ((bool)gregCalendar.IsChecked)
            {
                geoLocation[5] = (0).ToString();
            }
            else if ((bool)persCalendar.IsChecked)
            {
                geoLocation[5] = (1).ToString();
            }

            if ((bool)dryAirSelect.IsChecked)
            {
                geoLocation[6] = (0).ToString();
            }
            else if ((bool)wetAirSelect.IsChecked)
            {
                geoLocation[6] = (1).ToString();
            }
            else if ((bool)noAirSelect.IsChecked)
            {
                geoLocation[6] = (2).ToString();
            }

            if ((bool)msiseAtm.IsChecked)
            {
                geoLocation[7] = (0).ToString();
            }
            else if ((bool)standardAtm.IsChecked)
            {
                geoLocation[7] = (1).ToString();
            }
            else if ((bool)cusomAtm.IsChecked)
            {
                geoLocation[7] = (2).ToString();
            }

            geoLocation[8] = tempBox.Text;
            geoLocation[9] = pressBox.Text;

            if ((bool)aidAccept.IsChecked)
            {
                geoLocation[10] = (1).ToString();
            }
            else
            {
                geoLocation[10] = (0).ToString();
            }

            if ((bool)yallop.IsChecked)
            {
                geoLocation[11] = (0).ToString();
            }
            else if ((bool)odeh.IsChecked)
            {
                geoLocation[11] = (1).ToString();
            }
            else if ((bool)yallopOdeh.IsChecked)
            {
                geoLocation[11] = (2).ToString();
            }

            if ((bool)azanGoo.IsChecked)
            {
                geoLocation[12] = (0).ToString();
            }
            else if ((bool)tehranGeo.IsChecked)
            {
                geoLocation[12] =  (1).ToString();
            }
            else if ((bool)levaQum.IsChecked)
            {
                geoLocation[12] =  (2).ToString();
            }
            else if ((bool)islamKarachi.IsChecked)
            {
                geoLocation[12] =  (3).ToString();
            }
            else if ((bool)ISNA.IsChecked)
            {
                geoLocation[12] =  (4).ToString();
            }
            else if ((bool)egyptian.IsChecked)
            {
                geoLocation[12] =  (5).ToString();
            }
            else if ((bool)custom.IsChecked)
            {
                geoLocation[12] =  (6).ToString();
                geoLocation[13] = Fajr.Text;
                geoLocation[14] = magrib.Text;
                geoLocation[15] = isha.Text;
            }

            if ((bool)asr1length.IsChecked)
            {
                geoLocation[16] =  (0).ToString();
            }
            else if ((bool)asr2length.IsChecked)
            {
                geoLocation[16] =  (1).ToString();
            }                      
                       

            if (DST.IsChecked == true)
            {
                geoLocation[17] = (1).ToString();
                geoLocation[18] = selectRegion.SelectedIndex.ToString();
                geoLocation[19] = startDST.Text;
                geoLocation[20] =   endDST.Text;                
            }
            else
            {
                geoLocation[17] = (0).ToString();
            }

            System.IO.File.WriteAllLines(@"location.dat", geoLocation);

        }

        public void azanSeletion()
        {
            if ((bool)azanGoo.IsChecked)
            {
                // { -var, -4.0, -14.0, 1.0 };
                double latitude = getLatitude();
                Fajr.Text = MoonSun.azanAngle(ref latitude).ToString("F2");
                double magangle = -4.0;
                magrib.Text = magangle.ToString();
                double ishaAngle = -14.0;
                isha.Text = ishaAngle.ToString();
                asr1length.IsChecked = true;

            }
            else if ((bool)tehranGeo.IsChecked)
            {
                // { -17.7, -4.5, -14.0, 1.0 };
                double fajrAngle = -17.70;
                Fajr.Text = fajrAngle.ToString();
                double magangle = -4.50;
                magrib.Text = magangle.ToString();
                double ishaAngle = -14.0;
                isha.Text = ishaAngle.ToString();
                asr1length.IsChecked = true;
            }
            else if ((bool)levaQum.IsChecked)
            {
                // { -16.0, -4.5, -14.0, 1.0 };
                double fajrAngle = -16.0;
                Fajr.Text = fajrAngle.ToString();
                double magangle = -4.50;
                magrib.Text = magangle.ToString();
                double ishaAngle = -14.0;
                isha.Text = ishaAngle.ToString();
                asr1length.IsChecked = true;
            }
            else if ((bool)islamKarachi.IsChecked)
            {
                // { -18.0, -0.2666, -18.0, 1.0 };
                double fajrAngle = -16.0;
                Fajr.Text = fajrAngle.ToString();
                double magangle = -0.266;
                magrib.Text = magangle.ToString();
                double ishaAngle = -18.0;
                isha.Text = ishaAngle.ToString();
                asr1length.IsChecked = true;
            }
            else if ((bool)ISNA.IsChecked)
            {
                // { -15.0, -0.2666, -15.0, 1.0 };
                double fajrAngle = -15.0;
                Fajr.Text = fajrAngle.ToString();
                double magangle = -0.266;
                magrib.Text = magangle.ToString();
                double ishaAngle = -15.0;
                isha.Text = ishaAngle.ToString();
                asr1length.IsChecked = true;
            }
            else if ((bool)egyptian.IsChecked)
            {
                // { -19.5, -0.2666, -17.5, 1.0 };
                double fajrAngle = -19.5;
                Fajr.Text = fajrAngle.ToString();
                double magangle = -0.26660;
                magrib.Text =  magangle.ToString();
                double ishaAngle = -17.5;
                isha.Text =  ishaAngle.ToString();
                asr1length.IsChecked = true;
            }
            else if ((bool)custom.IsChecked)
            {
                
            }
        }

        public double[] getAzanAngles()
        {
            double[] azanAngles = new double[4];
            azanAngles[0] = getDouble(Fajr.Text);
            azanAngles[1] = getDouble(magrib.Text);
            azanAngles[2] = getDouble(isha.Text);
            if ((bool)asr1length.IsChecked)
            {
                azanAngles[3] = 1.0;                
            }
            else if ((bool)asr2length.IsChecked)
            {
                azanAngles[3] = 2.0;
            }
            try
            {
                errorMsg.Text = "";
                if (azanAngles[0] < -20.0 | azanAngles[0] > -12.0)
                {
                    throw new Exception("Wrong Fajr Angle");
                }
                else if (azanAngles[2] < -18.5 | azanAngles[2] > -12.0)
                {
                    throw new Exception("Wrong Isha Angle");
                }
                else if (azanAngles[1] < -5.0 | azanAngles[1] > -0.266)
                {
                    throw new Exception("Wrong Maghreb Angle");
                }
            }
            catch (Exception ex)
            {
                errorMsg.Text = ex.Message;
            }

            return azanAngles;
        }

        private void conventions_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            azanSeletion();
        }
        
        private double[] DSTJD()
        {
            int y = getYear(year.Text);
            int sMonth = 3;
            int day = 1;
            double hour = 0.0;           
            int sDay = 1;
            int eDay = 1;
            int eMonth = 1;
            int Jy = getYear(iranYear.Text);
            int sJm = 1;
            int sJd = 1;
            int eJm = 6;
            int eJd = 31;
            double SJD = 0.0;
            double EJD = 0.0;
            double H = 0.0;

            if (selectRegion.SelectedIndex == 0)
            {
                MoonSun.JULDAT(ref y, ref sMonth, ref day, ref hour, ref SJD);
                sDay = -Convert.ToInt32(SJD + 1.5) % 7 + 28;
                eMonth = 10;
                MoonSun.JULDAT(ref y, ref eMonth, ref day, ref hour, ref EJD);
                eDay = -Convert.ToInt32(EJD + 1.5) % 7 + 28;
                SJD = SJD + sDay;
                EJD = EJD + eDay;
            }
            else if (selectRegion.SelectedIndex == 1)
            {
                if ((bool)gregCalendar.IsChecked)
                {
                    sDay = 22;
                    MoonSun.JULDAT(ref y, ref sMonth,ref  sDay,ref H, ref SJD);
                    eMonth = 9;
                    eDay = 22;
                    MoonSun.JULDAT(ref y, ref eMonth, ref eDay, ref H, ref EJD);
                }
                else if ((bool)persCalendar.IsChecked)
                {
                    SJD = MoonSun.IrCal2JD(ref Jy, ref sJm, ref sJd, ref H);
                    EJD = MoonSun.IrCal2JD(ref Jy, ref eJm, ref eJd, ref H);
                }   
            }
            else if (selectRegion.SelectedIndex == 2)
            {
                MoonSun.JULDAT(ref y, ref sMonth, ref day, ref hour, ref SJD);
                sDay = -Convert.ToInt32(SJD + 1.5) % 7 + 14;
                eMonth = 11;
                MoonSun.JULDAT(ref y, ref eMonth, ref day, ref hour, ref EJD);
                eDay = -Convert.ToInt32(EJD + 1.5) % 7 + 7;
                SJD = SJD + sDay;
                EJD = EJD + eDay;
            }
            else if (selectRegion.SelectedIndex == 3)
            {

            }

            double[] DST = { SJD, EJD };
            return DST;       
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
                    MoonSun.RCALDAT(ref SJD, ref sYear, ref sMonth, ref sDay, ref FD);
                    MoonSun.RCALDAT(ref EJD, ref eYear, ref eMonth, ref eDay, ref FD);
                }
                else if ((bool)persCalendar.IsChecked)
                {
                     MoonSun.JD2IrCal(ref SJD,ref sYear, ref sMonth, ref sDay,ref FD);
                     MoonSun.JD2IrCal(ref EJD, ref eYear, ref eMonth, ref eDay,ref FD);
                }

                startDST.Text = sYear.ToString() + "/" + sMonth.ToString() + "/" + sDay.ToString();
                endDST.Text =  eYear.ToString() + "/" + eMonth.ToString() + "/" + eDay.ToString();

            }
            else if ((bool)DST.IsChecked == false)
            {
                startDST.Text = null;
                endDST.Text = null;
            }
            
        }

        private void DSTChecked(object sender, RoutedEventArgs e)
        {
             if ((bool)DST.IsChecked == true)
            {
                selectRegion.Items.Add("Europe");
                selectRegion.Items.Add("Iran");
                selectRegion.Items.Add("North America");
                selectRegion.Items.Add("Custom");
            }
            else if ((bool)DST.IsChecked == false)
            {
                selectRegion.Items.Clear();
            }
        }

        private double[] getAtm(double[] Geo , int Dy = 1)
        {
            double[] atm = { 10.0, 1010.0 };

            if ((bool)msiseAtm.IsChecked)
            {               
                atm = MoonSun.MSISEatm(Dy, Geo);
            }
            else 
            {
                if(pressBox.Text != null & tempBox.Text != null)
                {
                    atm[0] = getDouble(pressBox.Text);
                    atm[1] = getDouble(tempBox.Text);
                }                
            }

            return atm;
        }
      
        private void AtmSelection_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            Window6 w6 = new Window6();
            double[] atm = new double[2];
            double[] Geo = new double[3]; 
            Geo[0] = getLongitude();
            Geo[1] = getLatitude();
            Geo[2] = getAltitude();

            DateTime thisDate = getDate();

            if ((bool)msiseAtm.IsChecked)
            {
                atm = MoonSun.MSISEatm(thisDate.DayOfYear, Geo);
                pressBox.Text = atm[0].ToString("F2");
                tempBox.Text = atm[1].ToString("F2");
            }
            else if ((bool)standardAtm.IsChecked)
            {
                atm = MoonSun.stdAtmosTP(Geo[2]);
                pressBox.Text = atm[0].ToString("F2");
                tempBox.Text = atm[1].ToString("F2");
            }
            else if ((bool)cusomAtm.IsChecked)
            {
                pressBox.Text = null;
                tempBox.Text = null;
            }
        }

        private void MenuItem_Click_1(object sender, RoutedEventArgs e)
        {

            System.Windows.Forms.Help.ShowHelp(null, @"TheMoonSun.chm");
        }

        private void clearFile(object sender, RoutedEventArgs e)
        {
            using (System.IO.StreamWriter file = new System.IO.StreamWriter(@"location.dat"))
            {
                file.WriteLine("");
            }
        }

        private void mooncalendar(object sender, RoutedEventArgs e)
        {
            Window6 w6 = new Window6();
            int[] daysIranmonth = { 31, 31, 31, 31, 31, 31, 30, 30, 30, 30, 30, 29 };
            int[] daysGregmonth = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

            double[] Geo = new double[4];
            string geoName = Location.Text;
            if (geoName == "" || geoName == null) geoName = "---------";
            Geo[0] = getLongitude();
            Geo[1] = getLatitude();
            Geo[2] = getAltitude();
            Geo[3] = getTimeZone();

            double[] Atmos = getAtm(Geo);
            int year1 = 1;
            string fileName = Location.Text + " Moon Calendar";

            if ((bool)gregCalendar.IsChecked)
            {
                year1 = getYear(year.Text);
                fileName = fileName + year.Text;
            }
            else if ((bool)persCalendar.IsChecked)
            {
                year1 = getYear(iranYear.Text) + 621;
                fileName = fileName + iranYear.Text;
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

            int ye = getYear(year.Text);

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
                airConditin = "Air : Dry, Minimum Moon Ilumination = " + B_Ilum.ToString("P2");
            }
            else if ((bool)wetAirSelect.IsChecked)
            {
                B_Ilum = 0.005;
                airConditin = "Air : Humid, Minimum Moon Ilumination = " + B_Ilum.ToString("P2");
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

            double[] dstJD = new double[2];
            if ((bool)DST.IsChecked)
            {
                dstJD = DSTJD();
            }

            double UJD = 0.0;
            int Dy = 15;

            int[] Gyear = new int[5];
            double[] IUJD = new double[5];
            int leap = 1;
            int[] Equinox = new int[3];
            int[] MarDay = new int[3];
            double[] Uhour = new double[5];
            double[,] MoonJD = new double[4, 15];
            double[] newHijriJD = new double[15];
            int oYear = 1;
            int oMonth = 1;
            int oDay = 1;
            int dayInOtherMonth = 1;
            int dayInMonth = 1;

            string path = System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments);
            fileName = path + "\\" + fileName;
            string dummyName = fileName + ".txt";
            for (int i = 1; i < 100; i++)
            {
                if (System.IO.File.Exists(dummyName))
                {
                    dummyName = fileName + "(" + i.ToString() + ")" + ".txt";
                }
                else
                {
                    break;
                }
            }
            fileName = dummyName;
            w6.fileNameBox.Text = fileName;

            w6.Show();
            using (System.IO.StreamWriter file = new System.IO.StreamWriter(fileName, true))
            {

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
                    NativeMethods.IranCalendar(ref oYear, Gyear, IUJD, ref leap, Equinox, MarDay, Uhour);
                    if (leap == oYear) daysIranmonth[11] = 30;
                    NativeMethods.yearMoonPhases(ref Iyear, ref Im, ref Id, MoonJD);
                    file.WriteLine(string.Format("{0,80}", " Moon Caledar For Year " + year.Text));
                }
                else if ((bool)persCalendar.IsChecked)
                {
                    int Jy = getYear(iranYear.Text);
                    NativeMethods.IranCalendar(ref Jy, Gyear, IUJD, ref leap, Equinox, MarDay, Uhour);
                    int Im = 1;
                    int Id = 1;
                    double H = 0.0;
                    UJD = NativeMethods.IrCal2JD(ref Jy, ref Im, ref Id, ref H);
                    NativeMethods.RCALDAT(ref UJD, ref oYear, ref oMonth, ref oDay, ref H);
                    if (leap == Jy) daysIranmonth[11] = 30;
                    if (NativeMethods.GregIsLeapYear(ref Gyear[2])) { daysGregmonth[1] = 29; }
                    NativeMethods.yearMoonPhases(ref Gyear[2], ref oMonth, ref MarDay[2], MoonJD);
                    Dy = Dy + daysGregmonth[0] + daysGregmonth[1] + MarDay[2];
                    file.WriteLine(string.Format("{0,80}", " Moon Caledar For Year " + iranYear.Text));
                }

                file.WriteLine("======================================================================================"
                       + "===========================");
                file.WriteLine(" Location: " + geoName + ", Longitude: " + Geo[0].ToString("F2") + " \t Latitude: " + Geo[1].ToString("F2")
                        + "\t Elevation: " + Geo[2].ToString("F2") + ", Time Zone: " + Geo[3].ToString("F2"));
                file.WriteLine(airConditin + ", " + aidString + "\t" + method);
                file.WriteLine(" New year at: " + hour2Time(Hours) + ", " + Jday.ToString() + " " + NativeMethods.pesianMonthName(Jmonth - 1, 1)
                       + " " + Jyear.ToString() + " \t " + Iday.ToString() + " " + NativeMethods.gregoryMonthName(Imonth - 1, 1) + " " +
                        Iyear.ToString());

                if ((bool)msiseAtm.IsChecked == false)
                {
                    file.WriteLine(" Atmospheric properties for refraction: "
                        + "Pressure(millibar): " + Atmos[0].ToString("F2") + ", " + "Temperature(C) :"
                        + Atmos[1].ToString("F2"));
                }

                int Adjust = 0;

                for (int i = 0; i <= 14; i++)
                {
                    NativeMethods.HijriAdjust(ref MoonJD[0, i], Geo, ref UT_TT, ref Method, ref B_Ilum,
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
                int Iref = 1;
                if ((bool)noAirSelect.IsChecked)
                {
                    Iref = 0;
                }

                double TJD = UJD;
                int weekDaynum = NativeMethods.JD2WeekDayNum(TJD + Geo[3] / 24.0);

                for (int i = 0; i < 12; i++)
                {
                    string strOyear = null;
                    string mainMonth = null;
                    string strOmonth = null;
                    string strHmonth = null;

                    if ((bool)gregCalendar.IsChecked)
                    {
                        mainMonth = NativeMethods.gregoryMonthName(i);
                        strOyear = oYear.ToString();
                        dayInMonth = daysGregmonth[i];
                        dayInOtherMonth = daysIranmonth[oMonth - 1];
                    }
                    else if ((bool)persCalendar.IsChecked)
                    {
                        mainMonth = NativeMethods.pesianMonthName(i);
                        strOyear = oYear.ToString();
                        dayInMonth = daysIranmonth[i];
                        dayInOtherMonth = daysGregmonth[oMonth - 1];
                    }

                    string strHYear = Hy.ToString();

                    file.WriteLine(
                    " ================================================================================================"
                + "==========================");
                    file.WriteLine(string.Format("{0,60}", mainMonth));


                    if ((bool)msiseAtm.IsChecked)
                    {
                        if (Dy > 365) { Dy = Dy - 365; }
                        Atmos = getAtm(Geo, Dy);

                        file.WriteLine(" pressure: " + Atmos[0].ToString("F2")
                        + " \t Temperature: " + Atmos[1].ToString("F2"));

                        Dy = Dy + 30;
                    }

                    file.WriteLine("                                        " +
                        "            Moon Rise                     Moon Transit                  Moon Set");
                    file.WriteLine(" " + mainMonth + "        " + strOyear + "              " + strHYear
                        + "      Time-Azimuth-Illumination     Time-Altitude-Ilum     Time-Azimuth-Illumination");
                    file.WriteLine(
                        " --------------------------------------------------------------------------------------------"
                       + "-------------------------");

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
                                strOmonth = string.Format("{0,12}", "#  ");
                            }
                            else if ((bool)persCalendar.IsChecked)
                            {
                                strOmonth = string.Format("{0,9}", "#  ");
                            }
                        }

                        if (Hd == 1 | j == 1)
                        {
                            strHmonth = NativeMethods.hijriMonthName(Hm - 1, 1);
                        }
                        else
                        {
                            strHmonth = string.Format("{0,18}", "#  ");
                        }


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

                       
                        if ((bool)DST.IsChecked)
                        {
                            if (TJD > dstJD[0] & TJD < dstJD[1])
                            {
                                for (int n = 0; n <=2; n++)
                                {
                                    if (moonJD[n] != 0.0)
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
                                if (TJD <= MoonJD[n, m] + Geo[3] / 24.0 & (TJD + 1.0) > MoonJD[n, m] + Geo[3] / 24.0)
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

                        file.WriteLine(NativeMethods.WeekDays(weekDaynum, 1) + "  " + j.ToString("D2")
                        + "  " + strOmonth + "," + oDay.ToString("D2") + "  " + strHmonth + "," + Hd.ToString("D2")
                        + "   " + hour2Time(moonRShours[0]) + "-" + moonRSangles[0].ToString("000.00")
                        + "-" + Ilum_Ratio[0].ToString("00.00%") + "   " + hour2Time(moonRShours[1])
                        + "-" + moonRSangles[1].ToString("000.00") + "-" + Ilum_Ratio[1].ToString("00.00%")
                        + "   " + hour2Time(moonRShours[2]) + "-" + moonRSangles[2].ToString("000.00")
                        + "-" + Ilum_Ratio[2].ToString("00.00%") + MoonStat);

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
                }
            }
            w6.statusBox.AppendText(" Finished writing");
        }

        private void aboutClick(object sender, RoutedEventArgs e)
        {
            Window7 w7 = new Window7();
            w7.Show();
        }
    }     
 }
