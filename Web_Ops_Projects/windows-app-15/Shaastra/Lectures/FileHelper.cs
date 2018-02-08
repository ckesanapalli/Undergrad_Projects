using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;

namespace Shaastra.Lectures
{
    class FileHelper
    {
        public static string ReadFile(string filePath)
        {
            var ResrouceStream = Application.GetResourceStream(new Uri(filePath, UriKind.Relative));
            if (ResrouceStream != null)
            {
                Stream myFileStream = ResrouceStream.Stream;
                if (myFileStream.CanRead)
                {
                    StreamReader myStreamReader = new StreamReader(myFileStream);

                    return myStreamReader.ReadToEnd();
                }
            }
            return "";
        }
        public async static void ReadFileAlt()
        {
            string filename = "Assets/lecture_data.json";
            Windows.Storage.StorageFile sFile = await Windows.Storage.StorageFile.GetFileFromPathAsync(filename);
            Stream fileStream = await sFile.OpenStreamForReadAsync();
        }
    }
}
