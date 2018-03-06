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
    /// Interaction logic for Window2.xaml
    /// </summary>
    public partial class Window2 : Window
    {
        public Window2()
        {
            InitializeComponent();
           
        }

        private void MoonPhases(object sender, RoutedEventArgs e)
        {
            // WriteAllText creates a file, writes the specified string to the file,
            // and then closes the file.    You do NOT need to call Flush() or Close().
            // System.IO.File.WriteAllText(@"C:\Works\MoonSun\MoonSunText.txt", result.Text);

            MainWindow w = new MainWindow();
            Microsoft.Win32.SaveFileDialog saveFileDialog1 = new Microsoft.Win32.SaveFileDialog();
            saveFileDialog1.FileName = w.Location.Text +  "Moon Phases"; // Default file name
            saveFileDialog1.DefaultExt = ".text"; // Default file extension
            saveFileDialog1.Filter = "Text documents (.txt)|*.txt"; // Filter files by extension

            // Show save file dialog box
            Nullable<bool> resultBox = saveFileDialog1.ShowDialog();

            // Process save file dialog box results
            if (resultBox == true)
            {
                // Save document
                string filename = saveFileDialog1.FileName;
                System.IO.File.WriteAllText(saveFileDialog1.FileName, moonPhaseResult.Text);

            }
        }

        private void closeWindow2_Click(object sender, RoutedEventArgs e)
        {
            this.Close();
        }
    }
}
