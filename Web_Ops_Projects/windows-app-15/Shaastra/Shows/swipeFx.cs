using Microsoft.Phone.Controls;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Animation;

namespace Shaastra.Shows
{
    class swipeFx
    {

        public static void slidePanorama(Panorama pan)
        {
            FrameworkElement panWrapper = VisualTreeHelper.GetChild(pan, 0) as FrameworkElement;
            FrameworkElement panTitle = VisualTreeHelper.GetChild(panWrapper, 1) as FrameworkElement;
            //Get the panorama layer to calculate all panorama items size
            FrameworkElement panLayer = VisualTreeHelper.GetChild(panWrapper, 2) as FrameworkElement;
            //Get the title presenter to calculate the title size
            FrameworkElement panTitlePresenter = VisualTreeHelper.GetChild(VisualTreeHelper.GetChild(panTitle, 0) as FrameworkElement, 1) as FrameworkElement;

            //Current panorama item index
            int curIndex = pan.SelectedIndex;

            //Get the next of next panorama item
            FrameworkElement third = VisualTreeHelper.GetChild(pan.Items[(curIndex + 2) % pan.Items.Count] as PanoramaItem, 0) as FrameworkElement;

            //Be sure the RenderTransform is TranslateTransform
            if (!(pan.RenderTransform is TranslateTransform)
                || !(panTitle.RenderTransform is TranslateTransform))
            {
                pan.RenderTransform = new TranslateTransform();
                panTitle.RenderTransform = new TranslateTransform();
            }

            //Increase width of panorama to let it render the next slide (if not, default panorama is 480px and the null area appear if we transform it)
            pan.Width = 960;

            //Animate panorama control to the right
            Storyboard sb = new Storyboard();
            DoubleAnimation a = new DoubleAnimation();
            a.From = 0;
            a.To = -(pan.Items[curIndex] as PanoramaItem).ActualWidth; //Animate the x transform to a width of one item
            a.Duration = new Duration(TimeSpan.FromMilliseconds(700));
            a.EasingFunction = new CircleEase(); //This is default panorama easing effect
            sb.Children.Add(a);
            Storyboard.SetTarget(a, pan.RenderTransform);
            Storyboard.SetTargetProperty(a, new PropertyPath(TranslateTransform.XProperty));

            //Animate panorama title separately
            DoubleAnimation aTitle = new DoubleAnimation();
            aTitle.From = 0;
            aTitle.To = (panLayer.ActualWidth - panTitlePresenter.ActualWidth) / (pan.Items.Count - 1) * 1.5; //Calculate where should the title animate to
            aTitle.Duration = a.Duration;
            aTitle.EasingFunction = a.EasingFunction; //This is default panorama easing effect
            sb.Children.Add(aTitle);
            Storyboard.SetTarget(aTitle, panTitle.RenderTransform);
            Storyboard.SetTargetProperty(aTitle, new PropertyPath(TranslateTransform.XProperty));

            //Start the effect
            sb.Begin();

            //After effect completed, we change the selected item
            a.Completed += (obj, args) =>
            {
                //Reset panorama width
                pan.Width = 480;
                //Change the selected item
                (pan.Items[curIndex] as PanoramaItem).Visibility = Visibility.Collapsed;
                pan.SetValue(Panorama.SelectedItemProperty, pan.Items[(curIndex + 1) % pan.Items.Count]);
                pan.Measure(new Size(double.PositiveInfinity, double.PositiveInfinity));
                (pan.Items[curIndex] as PanoramaItem).Visibility = Visibility.Visible;
                //Reset panorama render transform
                (pan.RenderTransform as TranslateTransform).X = 0;
                //Reset title render transform
                (panTitle.RenderTransform as TranslateTransform).X = 0;

                //Because of the next of next item will be load after we change the selected index to next item
                //I do not want it appear immediately without any effect, so I create a custom effect for it
                if (!(third.RenderTransform is TranslateTransform))
                {
                    third.RenderTransform = new TranslateTransform();
                }
                Storyboard sb2 = new Storyboard();
                DoubleAnimation aThird = new DoubleAnimation() { From = 100, To = 0, Duration = new Duration(TimeSpan.FromMilliseconds(300)) };

                sb2.Children.Add(aThird);
                Storyboard.SetTarget(aThird, third.RenderTransform);
                Storyboard.SetTargetProperty(aThird, new PropertyPath(TranslateTransform.XProperty));
                sb2.Begin();
            };
        }
    }
}