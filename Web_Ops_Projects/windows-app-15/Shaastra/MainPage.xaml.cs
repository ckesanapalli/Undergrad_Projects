using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Navigation;
using Microsoft.Phone.Controls;
using Microsoft.Phone.Shell;
using Shaastra.Resources;
using System.Windows.Threading;

namespace Shaastra
{
    public partial class MainPage : PhoneApplicationPage
    {
        System.Windows.Threading.DispatcherTimer unfoldTick;
        // Constructor
        public MainPage()
        {
            InitializeComponent();
            // Sample code to localize the ApplicationBar
            //BuildLocalizedApplicationBar();
        }

        protected override void OnNavigatedTo(NavigationEventArgs e)
        {
            TiltEffect.TiltableItems.Add(typeof(Grid));
            //Height Setup
            part1x1.Height = App.Current.Host.Content.ActualHeight / 4.0;
            part1x2.Height = App.Current.Host.Content.ActualHeight / 4.0;
            part2x1.Height = App.Current.Host.Content.ActualHeight / 4.0;
            part2x2.Height = App.Current.Host.Content.ActualHeight / 4.0;
            part3x1.Height = App.Current.Host.Content.ActualHeight / 4.0;
            part3x2.Height = App.Current.Host.Content.ActualHeight / 4.0;
            part4x1.Height = App.Current.Host.Content.ActualHeight / 4.0;
            part4x2.Height = App.Current.Host.Content.ActualHeight / 4.0;
            //Width Setup
            part1x1.Width = App.Current.Host.Content.ActualWidth / 2.0;
            part1x2.Width = App.Current.Host.Content.ActualWidth / 2.0;
            part2x1.Width = App.Current.Host.Content.ActualWidth / 2.0;
            part2x2.Width = App.Current.Host.Content.ActualWidth / 2.0;
            part3x1.Width = App.Current.Host.Content.ActualWidth / 2.0;
            part3x2.Width = App.Current.Host.Content.ActualWidth / 2.0;
            part4x1.Width = App.Current.Host.Content.ActualWidth / 2.0;
            part4x2.Width = App.Current.Host.Content.ActualWidth / 2.0;
            //Margin Setup
            part1x1.Margin = new Thickness(0, 0, App.Current.Host.Content.ActualWidth / 2.0, 3 * App.Current.Host.Content.ActualHeight / 4.0);
            part1x2.Margin = new Thickness(0, 0, App.Current.Host.Content.ActualWidth / 2.0, 3 * App.Current.Host.Content.ActualHeight / 4.0);
            part2x1.Margin = new Thickness(0, 0, App.Current.Host.Content.ActualWidth / 2.0, 3 * App.Current.Host.Content.ActualHeight / 4.0);
            part2x2.Margin = new Thickness(0, 0, App.Current.Host.Content.ActualWidth / 2.0, 3 * App.Current.Host.Content.ActualHeight / 4.0);
            part3x1.Margin = new Thickness(0, 0, App.Current.Host.Content.ActualWidth / 2.0, 3 * App.Current.Host.Content.ActualHeight / 4.0);
            part3x2.Margin = new Thickness(0, 0, App.Current.Host.Content.ActualWidth / 2.0, 3 * App.Current.Host.Content.ActualHeight / 4.0);
            part4x1.Margin = new Thickness(0, 0, App.Current.Host.Content.ActualWidth / 2.0, 3 * App.Current.Host.Content.ActualHeight / 4.0);
            part4x2.Margin = new Thickness(0, 0, App.Current.Host.Content.ActualWidth / 2.0, 3 * App.Current.Host.Content.ActualHeight / 4.0);
            //Plane Projection setup
            turn1x1.RotationX = 0;
            turn1x1.RotationY = 0;
            turn1x2.RotationX = 0;
            turn1x2.RotationY = 0;
            turn2x1.RotationX = 0;
            turn2x1.RotationY = 0;
            turn2x2.RotationX = 0;
            turn2x2.RotationY = 0;
            turn3x1.RotationX = 0;
            turn3x1.RotationY = 0;
            turn3x2.RotationX = 0;
            turn3x2.RotationY = 0;
            turn4x1.RotationX = 0;
            turn4x1.RotationY = 0;
            turn4x2.RotationX = 0;
            turn4x2.RotationY = 0;

            //Reset global Y offset on navigating back from reset
            turn4x1.GlobalOffsetY = 0;
            turn3x1.GlobalOffsetY = 0;
            //Reset Opacity
            partN1x1.Opacity = 0;
            partN2x1.Opacity = 0;
            partN2x2.Opacity = 0;
            partN3x1.Opacity = 0;
            partN3x2.Opacity = 0;
            partN4x1.Opacity = 0;
            partN4x2.Opacity = 0;
            //Start main storyboard timer
            unfoldTick = new DispatcherTimer();
            unfoldTick.Interval = new TimeSpan(0, 0, 0, 0, 1000);
            unfoldTick.Tick += unfoldTick_Tick;
            unfoldTick.Start();
        }

        void unfoldTick_Tick(object sender, EventArgs e)
        {
            turn1x2.CenterOfRotationY = 1;
            turn3x1.CenterOfRotationY = 1;
            turn2x2.CenterOfRotationY = 1;
            turn4x1.CenterOfRotationY = 1;
            stageOne4x1.Completed += stageOne4x1_Completed;         //Fire up Animation stage two on completing stage one
            stageOne1x2.Begin();
            stageOne3x1.Begin();
            stageOne2x2.Begin();
            stageOne4x1.Begin();
            unfoldTick.Stop();
        }

        void stageOne4x1_Completed(object sender, EventArgs e)
        {
            turn2x1.CenterOfRotationX = 1;
            turn4x2.CenterOfRotationX = 1;
            turn2x2.CenterOfRotationX = 1;
            turn4x1.CenterOfRotationX = 1;
            stageTwo4x1.Completed += stageTwo4x1_Completed;         //Fire up Animation stage three on completing stage two
            stageTwo2x1.Begin();
            stageTwo4x2.Begin();
            stageTwo2x2.Begin();
            stageTwo4x1.Begin();
        }

        void stageTwo4x1_Completed(object sender, EventArgs e)
        {
            turn3x1.GlobalOffsetY = App.Current.Host.Content.ActualHeight / 4.0;
            turn4x1.GlobalOffsetY = App.Current.Host.Content.ActualHeight / 4.0;
            turn3x1.CenterOfRotationY = 1;
            turn4x1.CenterOfRotationY = 1;
            turn3x2.CenterOfRotationY = 2;
            turn4x2.CenterOfRotationY = 2;
            stageThree3x1.Completed += stageThree3x1_Completed;
            stageThree3x1.Begin();
            stageThree3x2.Begin();
            stageThree4x1.Begin();
            stageThree4x2.Begin();
        }

        void stageThree3x1_Completed(object sender, EventArgs e)
        {
            N1x1.Margin = new Thickness(0, 0, 0, 3 * App.Current.Host.Content.ActualHeight / 4.0);
            N2x1.Margin = new Thickness(0, App.Current.Host.Content.ActualHeight / 4.0, App.Current.Host.Content.ActualWidth / 2.0, App.Current.Host.Content.ActualHeight / 2.0);
            N2x2.Margin = new Thickness(App.Current.Host.Content.ActualWidth / 2.0, App.Current.Host.Content.ActualHeight / 4.0, 0, App.Current.Host.Content.ActualHeight / 2.0);
            N3x1.Margin = new Thickness(0, App.Current.Host.Content.ActualHeight / 2.0, App.Current.Host.Content.ActualWidth / 2.0, App.Current.Host.Content.ActualHeight / 4.0);
            N3x2.Margin = new Thickness(App.Current.Host.Content.ActualWidth / 2.0, App.Current.Host.Content.ActualHeight / 2.0, 0, App.Current.Host.Content.ActualHeight / 4.0);
            N4x1.Margin = new Thickness(0, 3 * App.Current.Host.Content.ActualHeight / 4.0, App.Current.Host.Content.ActualWidth / 2.0, 0);
            N4x2.Margin = new Thickness(App.Current.Host.Content.ActualWidth / 2.0, 3 * App.Current.Host.Content.ActualHeight / 4.0, 0, 0);
            stageFour.Begin();
        }

        protected override void OnRemovedFromJournal(JournalEntryRemovedEventArgs e)
        {
            unfoldTick.Tick -= unfoldTick_Tick;
            unfoldTick = null;
        }

        private void clickLectures_Tap(object sender, System.Windows.Input.GestureEventArgs e)
        {
            NavigationService.Navigate(new Uri("/Lectures/lectures.xaml", UriKind.Relative));
        }

        private void clickEvents_Tap(object sender, System.Windows.Input.GestureEventArgs e)
        {
            NavigationService.Navigate(new Uri("/Events/events.xaml", UriKind.Relative));
        }

        private void clickShows_Tap(object sender, System.Windows.Input.GestureEventArgs e)
        {
            NavigationService.Navigate(new Uri("/Shows/shows.xaml", UriKind.Relative));
        }

        private void clickTemp_Tap(object sender, System.Windows.Input.GestureEventArgs e)
        {
            NavigationService.Navigate(new Uri("/Lectures/lecturedetails.xaml?key=pic1f", UriKind.Relative));
        }

<<<<<<< HEAD
        private void part1x1_Tap(object sender, System.Windows.Input.GestureEventArgs e)
        {
            NavigationService.Navigate(new Uri("/Events/events.xaml", UriKind.Relative));
=======
        private void partN4x2_Tap(object sender, System.Windows.Input.GestureEventArgs e)
        {
            NavigationService.Navigate(new Uri("/Sponsors/spons.xaml", UriKind.Relative));
>>>>>>> origin/master
        }

    }
}