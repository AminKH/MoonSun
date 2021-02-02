using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ClassMoonSun;
using System.Globalization;
using System.Windows;

namespace SunMoonUtility
{
     static class Utility
    {
        public static string hour2Time(double Hour)
        {
            int H = 0;//= Convert.ToInt32(Hour - Hour % 1);
            int M = 0;//= Convert.ToInt32((Hour % 1) * 60.0 - ((Hour % 1) * 60.0) % 1);
            int S = 0;//= Convert.ToInt32((((Hour % 1) * 60.0) % 1) * 60.0);
            NativeMethods.Hour2HMS(ref Hour, ref H, ref M, ref S);
            string timeFormat = String.Format(CultureInfo.CurrentCulture, "{0:00}:{1:00}:{2:00}", H, M, S);
            return timeFormat;
        }

        public static int getInteger(String strint)
        {

            int outint = 0;
            try

            {
                outint = Convert.ToInt32(strint, null);
            }
            catch (OverflowException ioEx)
            {
                MessageBox.Show(ioEx.Message);
            }
            catch (ArgumentNullException Nex)
            {
                MessageBox.Show(Nex.Message);
            }
            catch (FormatException Fex)
            {
                MessageBox.Show(Fex.Message);
            }

            return outint;

        }

        public static double getDouble(String strint)
        {

            double outDbl = 0.0;
            try

            {
                outDbl = Convert.ToDouble(strint, null);
                return outDbl;
            }
            catch (OverflowException ioEx)
            {
                MessageBox.Show(ioEx.Message);
            }
            catch (ArgumentNullException Nex)
            {
                MessageBox.Show(Nex.Message);
            }
            catch (FormatException Fex)
            {
                MessageBox.Show(Fex.Message);
            }
            return outDbl;    
        }

        public static int getIntArg(int arg, int Llimit , int Ulimit, string strArg)
        {
            if (arg >= Llimit && arg <= Ulimit)
            {
                return arg;
            }
            else
            {
                arg = 0;
                MessageBox.Show(strArg+" out of limit");
            }
            return arg;
        }

        public static double getDoubleArg(double arg, double Llimit, double Ulimit, string strArg)
        {
            if (arg >= Llimit && arg <= Ulimit)
            {
                return arg;
            }
            else
            {
                arg = 0.0;
                MessageBox.Show(strArg + " out of limit");
            }
            return arg;
        }
    }
}
