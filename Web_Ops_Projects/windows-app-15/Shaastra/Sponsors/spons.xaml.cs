using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Navigation;
using Microsoft.Phone.Controls;
using Microsoft.Phone.Shell;

namespace Shaastra.Sponsors
{
    public partial class spons : PhoneApplicationPage
    {
        public spons()
        {
            InitializeComponent();
            this.Loaded += spons_Loaded;
        }

        void spons_Loaded(object sender, RoutedEventArgs e)
        {
            try
            {
                _browser.IsScriptEnabled = true;
                _browser.Navigate(new Uri("http://shaastra.org/2015/main/php/pages/spons.php#tab0"));
            }
            catch(Exception _ex)
            {
                MessageBox.Show(_ex.Message, "Error loading page", MessageBoxButton.OK);
            }
        }
    }
}