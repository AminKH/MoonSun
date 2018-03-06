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
using ClassMoonSun;



namespace TheMoonAndSun
{
    /// <summary>
    /// Interaction logic for Window5.xaml
    /// </summary>
    public partial class Window5 : Window
    {
        
        public Window5()
        {
            InitializeComponent();
        }        

       
   
        private void closeWin5(object sender, RoutedEventArgs e)
        {
            this.Close();
        }

        private void saveislamClendar(object sender, RoutedEventArgs e)
        {
            MainWindow w = new MainWindow();
            Microsoft.Win32.SaveFileDialog saveFileDialog1 = new Microsoft.Win32.SaveFileDialog();
            saveFileDialog1.FileName = w.Location.Text + " Islam Calendar"; // Default file name
            saveFileDialog1.DefaultExt = ".text"; // Default file extension
            saveFileDialog1.Filter = "Text documents (.txt)|*.txt"; // Filter files by extension

            // Show save file dialog box
            Nullable<bool> resultBox = saveFileDialog1.ShowDialog();

            // Process save file dialog box results
            if (resultBox == true)
            {
                // Save document
                string filename = saveFileDialog1.FileName;
                System.IO.File.WriteAllText(saveFileDialog1.FileName, islamCalendar.Text);
            }
        }

        private void islamCalendar_TextChanged(object sender, TextChangedEventArgs e)
        {

        }
    }
}
