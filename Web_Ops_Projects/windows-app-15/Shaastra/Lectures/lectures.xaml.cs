using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Navigation;
using Microsoft.Phone.Controls;
using Microsoft.Phone.Shell;
using System.Windows.Threading;
using System.Windows.Media.Imaging;
using System.Threading;
using System.Windows.Media;

namespace Shaastra.Lectures
{
    public partial class lectures : PhoneApplicationPage
    {
        bool switchTaps1 = false;
        bool switchTaps2 = false;
        bool switchTaps3 = false;    //This is for storing the odd/even property of the no. of taps for each pair of images

        bool rswitchTaps1 = false;
        bool rswitchTaps2 = false;
        bool rswitchTaps3 = false;    //This is for storing the Rotation Flip property of the no. of taps for each pair of images

        Uri store11 = (new Uri(@"Assets/nameBoard/pic1x.png", UriKind.Relative));
        Uri store12 = (new Uri(@"Assets/nameBoard/pic2x.png", UriKind.Relative));
        Uri store21 = (new Uri(@"Assets/nameBoard/pic3x.png", UriKind.Relative));
        Uri store22 = (new Uri(@"Assets/nameBoard/pic4x.png", UriKind.Relative));
        Uri store31 = (new Uri(@"Assets/nameBoard/pic5x.png", UriKind.Relative));
        Uri store32 = (new Uri(@"Assets/nameBoard/pic6x.png", UriKind.Relative));

        BitmapImage mem11 = new BitmapImage();
        BitmapImage mem12 = new BitmapImage();
        BitmapImage mem21 = new BitmapImage();
        BitmapImage mem22 = new BitmapImage();
        BitmapImage mem31 = new BitmapImage();
        BitmapImage mem32 = new BitmapImage();

        DispatcherTimer set1;

        // Constructor
        public lectures()
        {
            InitializeComponent();
            //popup

            //TiltEffect.TiltableItems.Add(typeof(Image));        //Do something about tilting images
            //ApplicationBar appBar = new ApplicationBar();             //Leave it for now
            //appBar.IsMenuEnabled = false;
            //appBar.Mode = ApplicationBarMode.Minimized;
            //appBar.Opacity = 0.8;
            //ApplicationBarIconButton button1 = new ApplicationBarIconButton();

            breathe.Begin();
            // Sample code to localize the ApplicationBar
            //BuildLocalizedApplicationBar();



            //Timer For flipping
            set1 = new DispatcherTimer();            
            set1.Interval = new TimeSpan(0, 0, 0, 5, 0);
            set1.Start();
            set1.Tick += set1_Tick;

            //Set Image Sources
            TopRightImage.Source = mem31;
            BottomLeftImage.Source = mem32;
            TopLeftImage.Source = mem21;
            BottomRightImage.Source = mem22;
            TopImage.Source = mem11;
            BottomImage.Source = mem12;

            //Initialize with default images
            mem11.UriSource = store11;
            mem12.UriSource = store12;
            mem21.UriSource = store21;
            mem22.UriSource = store22;
            mem31.UriSource = store31;
            mem32.UriSource = store32;

        }

        protected override void OnRemovedFromJournal(System.Windows.Navigation.JournalEntryRemovedEventArgs e)
        {
            mem11.UriSource = null;
            mem12.UriSource = null;
            mem21.UriSource = null;
            mem22.UriSource = null;
            mem31.UriSource = null;
            mem32.UriSource = null;
            set1.Tick -= set1_Tick;
            set1.Stop();
            breathe.Stop();
        }

        void set1_Tick(object sender, EventArgs e)
        {
            DispatcherTimer sub1 = new DispatcherTimer();
            sub1.Interval = new TimeSpan(0, 0, 0, 0, 250);
            sub1.Tick += sub1_Tick;
            sub1.Start();

            DispatcherTimer sub2 = new DispatcherTimer();
            sub2.Interval = new TimeSpan(0, 0, 0, 0, 375);
            sub2.Tick += sub2_Tick;
            sub2.Start();

            DispatcherTimer sub3 = new DispatcherTimer();
            sub3.Interval = new TimeSpan(0, 0, 0, 0, 500);
            sub3.Tick += sub3_Tick;
            sub3.Start();

            Rot_Up_Top.Begin();
            Rot_Down_Bottom.Begin();

            Rot_Left_Top_Left.BeginTime = new TimeSpan(0, 0, 0, 0, 125);
            Rot_Right_Bottom_Right.BeginTime = new TimeSpan(0, 0, 0, 0, 125);
            Rot_Left_Top_Left.Begin();
            Rot_Right_Bottom_Right.Begin();

            Rot_Up_Top_Right.BeginTime = new TimeSpan(0, 0, 0, 0, 250);
            Rot_Down_Bottom_Left.BeginTime = new TimeSpan(0, 0, 0, 0, 250);
            Rot_Up_Top_Right.Begin();
            Rot_Down_Bottom_Left.Begin();
        }

        void sub3_Tick(object sender, EventArgs e)
        {
            if (rswitchTaps3)
            {
                rswitchTaps3 = !rswitchTaps3;
                mem31.UriSource = store31;
                mem32.UriSource = store32;
            }
            else
            {
                rswitchTaps3 = !rswitchTaps3;
                string s31, s32;
                s31 = (TopRightImage.Source as BitmapImage).UriSource.ToString();
                s32 = (BottomLeftImage.Source as BitmapImage).UriSource.ToString();
                s31 = s31.Replace("nameBoard", "tileFaces");
                s32 = s32.Replace("nameBoard", "tileFaces");
                s31 = s31.Replace("x", "f");
                s32 = s32.Replace("x", "f");
                mem31.UriSource = (new Uri(@s31, UriKind.Relative));
                mem32.UriSource = (new Uri(@s32, UriKind.Relative));
            }
            (sender as DispatcherTimer).Stop();
        }

        void sub2_Tick(object sender, EventArgs e)
        {
            if (rswitchTaps2)
            {
                rswitchTaps2 = !rswitchTaps2;
                mem21.UriSource = store21;
                mem22.UriSource = store22;
            }
            else
            {
                rswitchTaps2 = !rswitchTaps2;
                string s21, s22;
                s21 = (TopLeftImage.Source as BitmapImage).UriSource.ToString();
                s22 = (BottomRightImage.Source as BitmapImage).UriSource.ToString();
                s21 = s21.Replace("nameBoard", "tileFaces");
                s22 = s22.Replace("nameBoard", "tileFaces");
                s21 = s21.Replace("x", "f");
                s22 = s22.Replace("x", "f");
                mem21.UriSource = (new Uri(@s21, UriKind.Relative));
                mem22.UriSource = (new Uri(@s22, UriKind.Relative));
            }
            (sender as DispatcherTimer).Stop();
        }

        void sub1_Tick(object sender, EventArgs e)
        {
            if (rswitchTaps1)
            {
                rswitchTaps1 = !rswitchTaps1;
                mem11.UriSource = store11;
                mem12.UriSource = store12;
            }
            else
            {
                rswitchTaps1 = !rswitchTaps1;
                string s11, s12;
                s11 = (TopImage.Source as BitmapImage).UriSource.ToString();
                s12 = (BottomImage.Source as BitmapImage).UriSource.ToString();
                s11 = s11.Replace("nameBoard", "tileFaces");
                s12 = s12.Replace("nameBoard", "tileFaces");
                s11 = s11.Replace("x", "f");
                s12 = s12.Replace("x", "f");
                mem11.UriSource = (new Uri(@s11, UriKind.Relative));
                mem12.UriSource = (new Uri(@s12, UriKind.Relative));
            }
            (sender as DispatcherTimer).Stop();
        }



        private void MiddleImage_Tap(object sender, System.Windows.Input.GestureEventArgs e)
        {
            System.Windows.Threading.DispatcherTimer delayCount1 = new System.Windows.Threading.DispatcherTimer();
            delayCount1.Interval = new TimeSpan(0, 0, 0, 0, 500);
            delayCount1.Tick += delayCount1_Tick;
            System.Windows.Threading.DispatcherTimer delayCount2 = new System.Windows.Threading.DispatcherTimer();
            delayCount2.Interval = new TimeSpan(0, 0, 0, 0, 750);
            delayCount2.Tick += delayCount2_Tick;
            System.Windows.Threading.DispatcherTimer delayCount3 = new System.Windows.Threading.DispatcherTimer();
            delayCount3.Interval = new TimeSpan(0, 0, 0, 0, 1000);
            delayCount3.Tick += delayCount3_Tick;

            delayCount1.Start();
            delayCount2.Start();
            delayCount3.Start();

            {
                Move_Up_Top.Begin();
                Move_Down_Bottom.Begin();

                Move_Left_Top_Left.BeginTime = new TimeSpan(0, 0, 0, 0, 250);
                Move_Right_Bottom_Right.BeginTime = new TimeSpan(0, 0, 0, 0, 250);
                Move_Left_Top_Left.Begin();
                Move_Right_Bottom_Right.Begin();

                Move_Up_Top_Right.BeginTime = new TimeSpan(0, 0, 0, 0, 500);
                Move_Down_Bottom_Left.BeginTime = new TimeSpan(0, 0, 0, 0, 500);
                Move_Up_Top_Right.Begin();
                Move_Down_Bottom_Left.Begin();
            }

            {
                Move_Up_Top1.BeginTime = new TimeSpan(0, 0, 0, 0, 500);
                Move_Down_Bottom1.BeginTime = new TimeSpan(0, 0, 0, 0, 500);
                Move_Up_Top1.Begin();
                Move_Down_Bottom1.Begin();

                Move_Left_Top_Left1.BeginTime = new TimeSpan(0, 0, 0, 0, 750);
                Move_Right_Bottom_Right1.BeginTime = new TimeSpan(0, 0, 0, 0, 750);
                Move_Left_Top_Left1.Begin();
                Move_Right_Bottom_Right1.Begin();

                Move_Up_Top_Right1.BeginTime = new TimeSpan(0, 0, 0, 0, 1000);
                Move_Down_Bottom_Left1.BeginTime = new TimeSpan(0, 0, 0, 0, 1000);
                Move_Up_Top_Right1.Begin();
                Move_Down_Bottom_Left1.Begin();
            }
        }

        void delayCount3_Tick(object sender, EventArgs e)
        {
            if (switchTaps3)
            {
                switchTaps3 = !switchTaps3;
                mem31.UriSource = (new Uri(@"Assets/nameBoard/pic5x.png", UriKind.Relative));
                mem32.UriSource = (new Uri(@"Assets/nameBoard/pic6x.png", UriKind.Relative));
                store31 = mem31.UriSource;
                store32 = mem32.UriSource;
            }
            else
            {
                switchTaps3 = !switchTaps3;
                mem31.UriSource = (new Uri(@"Assets/nameBoard/pic11x.png", UriKind.Relative));
                mem32.UriSource = (new Uri(@"Assets/hexagon_unfocused.png", UriKind.Relative));
                store31 = mem31.UriSource;
                store32 = mem32.UriSource;
            }
            (sender as DispatcherTimer).Stop();
        }

        void delayCount2_Tick(object sender, EventArgs e)
        {
            if (switchTaps2)
            {
                switchTaps2 = !switchTaps2;
                mem21.UriSource = (new Uri(@"Assets/nameBoard/pic3x.png", UriKind.Relative));
                mem22.UriSource = (new Uri(@"Assets/nameBoard/pic4x.png", UriKind.Relative));
                store21 = mem21.UriSource;
                store22 = mem22.UriSource;
            }
            else
            {
                switchTaps2 = !switchTaps2;
                mem21.UriSource = (new Uri(@"Assets/nameBoard/pic9x.png", UriKind.Relative));
                mem22.UriSource = (new Uri(@"Assets/nameBoard/pic10x.png", UriKind.Relative));
                store21 = mem21.UriSource;
                store22 = mem22.UriSource;
            }
            (sender as DispatcherTimer).Stop();
        }

        void delayCount1_Tick(object sender, EventArgs e)
        {
            if (switchTaps1)
            {
                switchTaps1 = !switchTaps1;
                mem11.UriSource = (new Uri(@"Assets/nameBoard/pic1x.png", UriKind.Relative));
                mem12.UriSource = (new Uri(@"Assets/nameBoard/pic2x.png", UriKind.Relative));
                store11 = mem11.UriSource;
                store12 = mem12.UriSource;
            }
            else
            {
                switchTaps1 = !switchTaps1;
                mem11.UriSource = (new Uri(@"Assets/nameBoard/pic7x.png", UriKind.Relative));
                mem12.UriSource = (new Uri(@"Assets/nameBoard/pic8x.png", UriKind.Relative));
                store11 = mem11.UriSource;
                store12 = mem12.UriSource;
            }
            (sender as DispatcherTimer).Stop();
        }

        private void Image_Tap(object sender, System.Windows.Input.GestureEventArgs e)
        {
            string argVal = ((sender as Image).Source as BitmapImage).UriSource.ToString();
            argVal = argVal.Replace("Assets/nameBoard/", "");
            argVal = argVal.Replace("Assets/tileFaces/", "");
            argVal = argVal.Replace(".png", "");
            NavigationService.Navigate(new Uri("/Lectures/lecturedetails.xaml?key="+argVal, UriKind.Relative));
        }

        

        
    }
}