using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Shaastra.Lectures
{
    public class lectureDetailRootObject
    {
        public string pic { get; set; }
        public string key { get; set; }
        public string name { get; set; }
        public string field { get; set; }
        public string desc { get; set; }
        public string venue { get; set; }
        public int date { get; set; }
        public int month { get; set; }
        public int year { get; set; }
        public int hrs { get; set; }
        public int mins { get; set; }
        public double latitude { get; set; }
        public double longitude { get; set; }
    }
}
