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
using System.Windows.Shapes;

namespace TheMoonAndSun
{
    /// <summary>
    /// Interaction logic for Window1.xaml
    /// </summary>
    public partial class Window1 : Window
    {
        public Window1()
        {
            InitializeComponent();
        }

        private void ListBoxItem_Selected(object sender, RoutedEventArgs e)
        {
            checkWeb("https://www.gps-coordinates.net/");
            //Use no more than one assignment when you test this code.   
        }

        private void ListBoxItem_Selected_1(object sender, RoutedEventArgs e)
        {
            checkWeb("http://elevationmap.net/");
            //Use no more than one assignment when you test this code. 
        }

        private void ListBoxItem_Selected_2(object sender, RoutedEventArgs e)
        {
            checkWeb("https://www.bahesab.ir/map/geographic/");
            //Use no more than one assignment when you test this code.
        }

        private void ListBoxItem_Selected_4(object sender, RoutedEventArgs e)
        {
            checkWeb("https://www.timeanddate.com/time/map/");
        }       

        private void checkWeb(string target)
        {
            try
            {
                System.Diagnostics.Process.Start(target);
            }
            catch
                (
                 System.ComponentModel.Win32Exception noBrowser)
            {
                if (noBrowser.ErrorCode == -2147467259)
                    MessageBox.Show(noBrowser.Message);
            }
            catch (System.Exception other)
            {
                MessageBox.Show(other.Message);
            }
        }

        private void closeWin1(object sender, RoutedEventArgs e)
        {
            this.Close();
        }

        private void helpStart(object sender, RoutedEventArgs e)
        {
            System.Windows.Forms.Help.ShowHelp(null,"TheMoonSun.chm");
        }
    }
}
