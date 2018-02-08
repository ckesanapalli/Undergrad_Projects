using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Navigation;
using Microsoft.Phone.Controls;
using Microsoft.Phone.Shell;
using Windows.Storage;
using System.IO;
using System.Windows.Documents;
using System.Windows.Media.Imaging;
using Microsoft.Phone.Tasks;
using Windows.Devices.Geolocation;
using System.Device.Location;
using System.Threading.Tasks;


namespace Shaastra.Lectures
{

    public partial class lecturedetails : PhoneApplicationPage
    {
        string argVal;
        string jsData;
        System.Device.Location.GeoCoordinate pos;
        DateTime dt;
        BitmapImage bearer;
        public lecturedetails()
        {
            InitializeComponent();
            progressOverlay.Show();
        }

        private async void loadData()
        {
            List<lectureDetailRootObject> scholars = new List<lectureDetailRootObject>();
            Stream jsStream = Application.GetResourceStream(new Uri("Lectures\\lecture_data.json", UriKind.Relative)).Stream;
            TextReader dumper = new StreamReader(jsStream);
            jsData = await dumper.ReadToEndAsync();
            await Task.Factory.StartNew(() => { scholars = Newtonsoft.Json.JsonConvert.DeserializeObject<List<lectureDetailRootObject>>(jsData); });
            foreach (lectureDetailRootObject element in scholars)
            {
                if (element.key == argVal || element.pic == argVal)
                {
                    personName.Text = element.name;
                    string tempPic = element.pic;
                    tempPic = tempPic.Replace("f", "");
                    tempPic = tempPic.Replace("x", "");
                    bearer = new BitmapImage(new Uri("Assets\\pageFaces\\" + tempPic + ".jpg", UriKind.Relative));
                    profilePic.Source = bearer;
                    //Description Box
                    Paragraph dPara = new Paragraph();
                    Run dRun = new Run();
                    dRun.Text = element.desc;
                    dPara.Inlines.Add(dRun);
                    descbox.Blocks.Clear();
                    descbox.Blocks.Add(dPara);
                    pos = new System.Device.Location.GeoCoordinate(element.latitude, element.longitude);
                    eventLocation.Text = element.venue;
                    dt = new DateTime(element.year, element.month, element.date, element.hrs, element.mins, 0);
                    eventDate.Text = dt.DayOfWeek.ToString() + ", " + dt.Day + "/" + dt.Month + "/" + dt.Year;
                    eventTime.Text = dt.Hour.ToString() + dt.Minute.ToString() + " hrs";
                    break;
                }
            }
            progressOverlay.Hide();
        }

        protected override void OnRemovedFromJournal(System.Windows.Navigation.JournalEntryRemovedEventArgs e)
        {
            pos = null;
            bearer.UriSource = null;
            profilePic.Source = bearer;
            GC.Collect();
        }

        protected override void OnNavigatedTo(NavigationEventArgs e)
        {
            argVal = NavigationContext.QueryString["key"];
            loadData();
        }

       

        private void glonass_Click(object sender, RoutedEventArgs e)
        {
            //string launchNokiaMaps = "directions://v2.0/route/destination/?latlon=" + pos.Latitude + "," + pos.Longitude + "&mode=walk&appid=7795db32-220f-425b-99c7-bc42b526e509";
            //await Windows.System.Launcher.LaunchUriAsync(new Uri(launchNokiaMaps));
            MapsDirectionsTask routeGen = new MapsDirectionsTask();
            routeGen.End = new LabeledMapLocation();
            routeGen.End.Label = eventLocation.Text;
            routeGen.End.Location = pos;
            routeGen.Show();
            
        }

        private void notifier_Click(object sender, RoutedEventArgs e)
        {
            Microsoft.Phone.Tasks.SaveAppointmentTask myTask = new Microsoft.Phone.Tasks.SaveAppointmentTask();
            myTask.StartTime = dt;
            myTask.EndTime = dt.AddHours(3);
            myTask.AppointmentStatus = Microsoft.Phone.UserData.AppointmentStatus.Busy;
            myTask.Location = eventLocation.Text;
            myTask.Subject = "Lecture by " + personName.Text;
            myTask.Show();
        }
    }
}