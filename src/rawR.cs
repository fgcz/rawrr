/// adapded from the ThermoFischer `Hello, world!` example provided by Jim Shofstahl 
/// see URL http://planetorbitrap.com/rawfilereader#.WjkqIUtJmL4
/// the ThermoFisher library has to be manual downloaded and installed
/// Please read the License document
/// Witold Wolski <wew@fgcz.ethz.ch> and Christian Panse <cp@fgcz.ethz.ch> and Christian Trachsel
/// 2017-09-25 Zurich, Switzerland
/// 2018-04-24 Zurich, Switzerland
/// 2018-06-04 San Diego, CA, USA added xic option
/// 2018-06-28 added xic and scan option
/// 2018-07-24 bugfix
/// 2018-11-23 added scanFilter option
/// 2019-01-28 extract monoisotopicmZ attribute; include segments in MGF iff no centroid data are availbale
/// 2019-05-28 save info as Yaml
/// 2020-08-12 added infoR option
/// 2020-08-26 readSpectrum backend
 
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Runtime.ExceptionServices;
using System.Collections;
//using System.Configuration;
//using System.Diagnostics.Eventing;
//using System.Data.Common;
using System.Linq;
//using System.Runtime.DesignerServices;
//using System.Runtime.InteropServices.WindowsRuntime;
//using System.Xml.Schema;
//using System.Runtime.InteropServices;
using ThermoFisher.CommonCore.Data;
using ThermoFisher.CommonCore.Data.Business;
using ThermoFisher.CommonCore.Data.FilterEnums;
using ThermoFisher.CommonCore.Data.Interfaces;
using ThermoFisher.CommonCore.MassPrecisionEstimator;
using ThermoFisher.CommonCore.RawFileReader;



namespace FGCZExtensions
{
    //Extension methods must be defined in a static class
    public static class StringExtension
    {
        /// <summary>
        /// make all the existing header names distinct
        /// </summary>
        /// <author>
        /// Christian Panse <cp@fgcz.ethz.ch> 2017-11-02
        /// </author>
        /// <param name="s"> a string</param>
        /// <returns>a string</returns>
        public static string CleanRawfileTrailerHeader(this string s)
        {
            return(s.Replace(" ", "")
                .Replace("#", "")
                .Replace("m/z", "mZ")
                .Replace("M/Z", "mZ")
                .Replace("(", "")
                .Replace(".", "")
                .Replace(")", "")
                .Replace(":", "")
                .Replace("-", "")
                .Replace("=", ""));
        }
    }

    public static class IRawDataPlusExtension
    {

        

        public static void ExtractQualityControlTable(this IRawDataPlus rawFile, string outputFilename)
        {
            int firstScanNumber = rawFile.RunHeaderEx.FirstSpectrum;
            int lastScanNumber = rawFile.RunHeaderEx.LastSpectrum;
            var filename = Path.GetFileName(rawFile.FileName);

            using (System.IO.StreamWriter file =
                new System.IO.StreamWriter(outputFilename))

            {
                // TODO(cp): rename header; start with capital character
                file.Write(
                    "filename"
                    + "\tscanNumber"
                    + "\tScanEventNumber"
                    + "\tStartTime"
                    + "\tBasePeakMass"
                    + "\tBasePeakIntensity"
                    + "\tTIC"
                    + "\tScanType"
                    + "\tCycleNumber"
                    + "\tFrequency"
                    + "\tHighMass"
                    + "\tIonizationMode"
                    + "\tMSOrder"
                    + "\tMassAnalyzer"
                    + "\tDetector"
                    + "\tLock"
                    + "\tPrecursorMass"
                    + "\tLastPrecursorMass"
                    + "\tCollisionEnergy"
                    + "\tIsolationWidth"
                );
                var trailerFields = rawFile.GetTrailerExtraHeaderInformation();
                var trailerHeaderFieldCounter = 0;

                foreach (var field in trailerFields)
                {
                    trailerHeaderFieldCounter++;
                    var trailerHeaderFieldValue =
                        field.Label.ToString().CleanRawfileTrailerHeader();
                    
                    if (trailerHeaderFieldValue.Length > 1){
                        file.Write("\t{0}", trailerHeaderFieldValue, trailerHeaderFieldValue.Length );
                    } else {
                        file.Write("\tunnamedTrailerHeaderField_{0}", trailerHeaderFieldCounter);
                    }

                }


                // "filename"          "scanNumber"       "StartTime"          "BasePeakMass"      "BasePeakIntensity"  
                // "TIC"                "ScanType"           "CycleNumber"   "MSOrder"            "MassAnalyzer"  
                // "PrecursorMass"     

                // "ChargeState"      
                //  [13] "IonInjectionTimems" "OrbitrapResolution" "FTResolution"       "MasterScanNumber" 
                //  [17] "LMCorrectionppm"    "LMmZCorrectionppm" -> LMCorrection


                file.WriteLine();

                foreach (var scanNumber in Enumerable
                    .Range(1, lastScanNumber - firstScanNumber))
                {
                    var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
                    var scanFilter = rawFile.GetFilterForScanNumber(scanNumber);
                    var scanEvent = rawFile.GetScanEventForScanNumber(scanNumber);
                    var scanTrailer = rawFile.GetTrailerExtraInformation(scanNumber);

//                       var xx = rawFile.
                    // Console.WriteLine("Polarity: {0}", filter.Polarity);

                    file.Write(
                        Path.GetFileName(filename)
                        + "\t" + scanNumber
                        + "\t" + scanStatistics.ScanEventNumber
                        + "\t" + scanStatistics.StartTime
                        + "\t" + scanStatistics.BasePeakMass
                        + "\t" + scanStatistics.BasePeakIntensity.ToString()
                        + "\t" + scanStatistics.TIC.ToString()
                        + "\t" + scanStatistics.ScanType.ToString()
                        + "\t" + scanStatistics.CycleNumber.ToString()
                        + "\t" + scanStatistics.Frequency.ToString()
                        + "\t" + scanStatistics.HighMass.ToString()
                        + "\t" + scanFilter.IonizationMode.ToString()
                        + "\t" + scanFilter.MSOrder.ToString()
                        + "\t" + scanFilter.MassAnalyzer.ToString()
                        + "\t" + scanFilter.Detector.ToString()
                        + "\t" + scanFilter.Lock.ToString() + "\t");

                    try
                    {
                        var reaction0 = scanEvent.GetReaction(0);
                        file.Write(reaction0.PrecursorMass
                                      + "\t" + reaction0.LastPrecursorMass
                                      + "\t" + reaction0.CollisionEnergy
                                      + "\t" + reaction0.IsolationWidth
                        );
                    }
                    catch
                    {
                        file.Write("NA\tNA\tNA\tNA");
                    }

                    foreach (var scanTrailerField in scanTrailer.Values)
                    {
                        file.Write("\t{0}", scanTrailerField.Replace("\t", "").Replace(" ", ""));
                    }

                    file.WriteLine();
                }
                //GetIntensitySum(rawFile, i, firstFilter.ToString(), true));
            }
        }

        public static void PrintInfoAsRcode(this IRawDataPlus rawFile)
	{
                Console.WriteLine("#R\n\ne$info <- list()");
                Console.WriteLine("e$info$`RAW file` <- '" + Path.GetFileName(rawFile.FileName) + "'");
                Console.WriteLine("e$info$`RAW file version` <- '" + rawFile.FileHeader.Revision + "'");
                Console.WriteLine("e$info$`Creation date` <- '" + rawFile.FileHeader.CreationDate + "'");
                Console.WriteLine("e$info$Operator <- '" + rawFile.FileHeader.WhoCreatedId + "'");
                Console.WriteLine("e$info$`Number of instruments` <- {0}", rawFile.InstrumentCount);
                Console.WriteLine("e$info$Description <- '" + rawFile.FileHeader.FileDescription + "'");
                Console.WriteLine("e$info$`Instrument model` <- '{0}'", rawFile.GetInstrumentData().Model);
                Console.WriteLine("e$info$`Instrument name` <- '{0}'", rawFile.GetInstrumentData().Name);
               // Console.WriteLine("e$info$`Instrument method` <- '{0}'", rawFile.GetAllInstrumentFriendlyNamesFromInstrumentMethod().Length);
                Console.WriteLine("e$info$`Serial number` <- '{0}'", rawFile.GetInstrumentData().SerialNumber);
                Console.WriteLine("e$info$`Software version` <- '{0}'", rawFile.GetInstrumentData().SoftwareVersion);
                Console.WriteLine("e$info$`Firmware version` <- '{0}'", rawFile.GetInstrumentData().HardwareVersion);
                Console.WriteLine("e$info$Units <- '{0}'", rawFile.GetInstrumentData().Units);
                Console.WriteLine("e$info$`Mass resolution` <- '{0:F3}'", rawFile.RunHeaderEx.MassResolution);
                Console.WriteLine("e$info$`Number of scans` <- {0}", rawFile.RunHeaderEx.SpectraCount);
             	int firstScanNumber = rawFile.RunHeaderEx.FirstSpectrum;
            	int lastScanNumber = rawFile.RunHeaderEx.LastSpectrum;
                Console.WriteLine("e$info$`Number of ms2 scans` <- {0}", Enumerable.Range(1, lastScanNumber - firstScanNumber).Count(x => rawFile.GetFilterForScanNumber(x).ToString().Contains("Full ms2")));
                Console.WriteLine("e$info$`Scan range` <- c({0}, {1})", firstScanNumber, lastScanNumber);
                double startTime = rawFile.RunHeaderEx.StartTime;
                double endTime = rawFile.RunHeaderEx.EndTime;
                Console.WriteLine("e$info$`Time range` <- c({0:F2}, {1:F2})", startTime, endTime);
                Console.WriteLine("e$info$`Mass range` <- c({0:F4}, {1:F4})", rawFile.RunHeaderEx.LowMass, rawFile.RunHeaderEx.HighMass);

                var firstFilter = rawFile.GetFilterForScanNumber(firstScanNumber);
                var lastFilter = rawFile.GetFilterForScanNumber(lastScanNumber);
                int numberFilters = rawFile.GetFilters().Count;
                Console.WriteLine("e$info$`Scan filter (first scan)` <- '{0}'", firstFilter.ToString());
                Console.WriteLine("e$info$`Scan filter (last scan)` <- '{0}'", lastFilter.ToString());
                Console.WriteLine("e$info$`Total number of filters` <- '{0}'", numberFilters);

                Console.WriteLine("e$info$`Sample name` <- '{0}' ", rawFile.SampleInformation.SampleName);
                Console.WriteLine("e$info$`Sample id` <- '{0}' ", rawFile.SampleInformation.SampleId);
                Console.WriteLine("e$info$`Sample type` <- '{0}' ", rawFile.SampleInformation.SampleType);
                Console.WriteLine("e$info$`Sample comment` <- '{0}' ", rawFile.SampleInformation.Comment);
                Console.WriteLine("e$info$`Sample vial` <- '{0}' ", rawFile.SampleInformation.Vial);
                Console.WriteLine("e$info$`Sample volume` <- '{0}' ", rawFile.SampleInformation.SampleVolume);
                Console.WriteLine("e$info$`Sample injection volume` <- '{0}' ", rawFile.SampleInformation.InjectionVolume);
                Console.WriteLine("e$info$`Sample row number` <- '{0}' ", rawFile.SampleInformation.RowNumber);
                Console.WriteLine("e$info$`Sample dilution factor` <- '{0}' ", rawFile.SampleInformation.DilutionFactor);
                Console.WriteLine("e$info$`Sample barcode` <- '{0}' ", rawFile.SampleInformation.Barcode);
	}

        /// <summary>
        /// </summary>
        /// <param name="rawFile"></param>
        /// <param name="filename"></param>
        /// <param name="L"></param>
        public static void WriteSpectrumAsRcode(this IRawDataPlus rawFile, string filename, List<int> L)
        {
            int count = 1;
            using (System.IO.StreamWriter file =
                new System.IO.StreamWriter(filename))
            {
                // file.WriteLine("e <- new.env(); e$PeakList <- list()");
                foreach (int scanNumber in L)
                {

                    var trailerFields = rawFile.GetTrailerExtraHeaderInformation();
                    var pepmass = -1.0;
                    var charge = 0;
                    var monoisotopicMz = "NA";
                    var basepeakIntensity = -1.0;

                    var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
                    var centroidStream = rawFile.GetCentroidStream(scanNumber, false);
                    var scanTrailer = rawFile.GetTrailerExtraInformation(scanNumber);
                    var scanEvent = rawFile.GetScanEventForScanNumber(scanNumber);
                    
                    try
                    {
                        var reaction0 = scanEvent.GetReaction(0);
                        var idx_PEPMASS = trailerFields
                            .Select((item, index) => new
                            {
                                name = item.Label.ToString().CleanRawfileTrailerHeader(),
                                Position = index
                            })
                            .First(x => x.name.Contains("MonoisotopicmZ")).Position;

                        var idx_CHARGE = trailerFields
                            .Select((item, index) => new
                            {
                                name = item.Label.ToString(),
                                Position = index
                            })
                            .First(x => x.name.Contains("Charge State")).Position;

                            pepmass = reaction0.PrecursorMass;
                            basepeakIntensity =  Math.Round(scanStatistics.BasePeakIntensity);
                            charge = int.Parse(scanTrailer.Values.ToArray()[idx_CHARGE]);
                            monoisotopicMz = scanTrailer.Values.ToArray()[idx_PEPMASS];
                    }
                    catch
                    {
                        // Console.WriteLine("catch");
                        pepmass = -1.0;
                        basepeakIntensity = -1.0;
                        charge = 0;
                        monoisotopicMz = "NA";
                    }

                        var scan = Scan.FromFile(rawFile, scanNumber);

                            file.WriteLine("e$PeakList[[{0}]] <- list(", count++);
                            file.WriteLine("\tscan = {0},", scanNumber);
                            file.WriteLine("\tscanType = \"{0}\",", scanStatistics.ScanType.ToString());
                            file.WriteLine("\trtinseconds = {0},", Math.Round(scanStatistics.StartTime * 60 * 1000) / 1000);
                            file.WriteLine("\tpepmass = c({0}, {1}),", pepmass, basepeakIntensity);
                        if (scanStatistics.IsCentroidScan && centroidStream.Length > 0)
                        {
                            // Get the centroid (label) data from the RAW file for this scan
                            file.WriteLine("\tcentroidStream = TRUE,");

                            file.WriteLine("\tHasCentroidStream = '{0}, Length={1}',", scan.HasCentroidStream, scan.CentroidScan.Length);
			    if(scan.HasCentroidStream){
                                file.WriteLine("\tcentroid.mZ = c(" + string.Join(", ", scan.CentroidScan.Masses.ToArray()) + "),");
                                file.WriteLine("\tcentroid.intensity = c(" + string.Join(", ", scan.CentroidScan.Intensities.ToArray()) + "),");
			    }

                            file.WriteLine("\ttitle = \"File: {0}; SpectrumID: {1}; scans: {2}\",",
                                Path.GetFileName(rawFile.FileName),
                                null,
                                scanNumber);

                            file.WriteLine("\tcharge = {0},", charge);
                            file.WriteLine("\tmonoisotopicMz = {0},", monoisotopicMz);
                            file.WriteLine("\tmZ = c(" + string.Join(", ", centroidStream.Masses) + "),");
                            file.WriteLine("\tintensity = c(" + string.Join(", ", centroidStream.Intensities) + "),");

                        }
                        else
                        {
                            file.WriteLine("\tcentroidStream = FALSE,");

                            file.WriteLine("\tHasCentroidStream = '{0}, Length={1}',", scan.HasCentroidStream, scan.CentroidScan.Length);
			    if(scan.HasCentroidStream){
                                file.WriteLine("\tcentroid.mZ = c(" + string.Join(",", scan.CentroidScan.Masses.ToArray()) + "),");
                                file.WriteLine("\tcentroid.intensity = c(" + string.Join(",", scan.CentroidScan.Intensities.ToArray()) + "),");
			    }

                            file.WriteLine("\ttitle = \"File: {0}; SpectrumID: {1}; scans: {2}\",",
                                Path.GetFileName(rawFile.FileName),
                                null,
                                scanNumber);

                            file.WriteLine("\tcharge = {0},", charge);
                            file.WriteLine("\tmonoisotopicMz = {0},", monoisotopicMz);
                            file.WriteLine("\tmZ = c(" + string.Join(",", scan.SegmentedScan.Positions) + "),");
                            file.WriteLine("\tintensity = c(" + string.Join(",", scan.SegmentedScan.Intensities) + ")");
                        }
                        // write scan Trailer
			    var trailerValues = scanTrailer.Values;
			    var trailerLabels = scanTrailer.Labels;
                var zipTrailer = trailerLabels.ToArray().Zip(trailerValues, (a, b) => string.Format("\t\"{0}\" = \"{1}\"", a, b));
                file.WriteLine(string.Join(", \n", zipTrailer));
                            file.WriteLine(")");
                }
            }

            return;
        }

    }
}

namespace FGCZ_Raw
{

    using FGCZExtensions;


    internal static class Program
    {
        private static void Main(string[] args)
        {
            // This local variable controls if the AnalyzeAllScans method is called
            // bool analyzeScans = false;
            string rawR_version = "0.0.1";

            // Get the memory used at the beginning of processing
            Process processBefore = Process.GetCurrentProcess();
            
            long memoryBefore = processBefore.PrivateMemorySize64 / 1024;

            try
            {
                // Check to see if the RAW file name was supplied as an argument to the program
                string filename = string.Empty;
                string mode = string.Empty;
                Hashtable hashtable = new Hashtable()
                {
                    {"version", "print version information."},
                    {"scanFilter", "print scan filters."},
                    {"infoR", "print the raw file's meta data as R code."},
                    {"chromatogram", "base peak chromatogram."},
                    {"qc", "prints a qc table having one entry per scan."},
                    {"xic", "prints xic unfiltered."},
                    {"scans", "print spectrum of scanid as Rcode."}
                };

                if (args.Length > 0)
                {
                    filename = args[0];

                    if (args.Length == 1)
                    {
                        Console.WriteLine("rawR version = {}", rawR_version);
                        Console.WriteLine("missing mode argument. setting to mode = 'info'.");
                        mode = "info";
                    }
                    else
                    {
                        mode = args[1];
                    }


                    if (!hashtable.Contains(mode))
                    {
                        Console.WriteLine("rawR version = {}", rawR_version);
                        Console.WriteLine("mode '{0}' not allowed. Please use one of the following modes:", mode);
                        foreach (var k in hashtable.Keys)
                        {
                            Console.WriteLine("{0} - {1}", k.ToString(), hashtable[k].ToString());
                        }

                        Environment.Exit(1);
                    }
                }

                if (string.IsNullOrEmpty(filename))
                {
                    
                    Console.WriteLine("No RAW file specified!");

                    return;
                }

                // Check to see if the specified RAW file exists
                if (!File.Exists(filename))
                {
                    Console.WriteLine("rawR version = {}", rawR_version);
                    Console.WriteLine(@"The file doesn't exist in the specified location - {0}", filename);
                    
                    return;
                }

                // Create the IRawDataPlus object for accessing the RAW file
                var rawFile = RawFileReaderAdapter.FileFactory(filename);

                if (!rawFile.IsOpen || rawFile.IsError)
                {
                    Console.WriteLine("Unable to access the RAW file using the RawFileReader class!");

                    return;
                }

                // Check for any errors in the RAW file
                if (rawFile.IsError)
                {
                    Console.WriteLine("Error opening ({0}) - {1}", rawFile.FileError, filename);
                    
                    return;
                }

                // Check if the RAW file is being acquired
                if (rawFile.InAcquisition)
                {
                    Console.WriteLine("RAW file still being acquired - " + filename);

                    return;
                }

                rawFile.SelectInstrument(Device.MS, 1);
                //Console.WriteLine("DEBUG {0}", rawFile.GetInstrumentMethod(3).ToString());

                // Get the first and last scan from the RAW file
                int firstScanNumber = rawFile.RunHeaderEx.FirstSpectrum;
                int lastScanNumber = rawFile.RunHeaderEx.LastSpectrum;

                // Get the start and end time from the RAW file
                double startTime = rawFile.RunHeaderEx.StartTime;
                double endTime = rawFile.RunHeaderEx.EndTime;

                if (mode == "systeminfo")
                {

                    Console.WriteLine("raw file name: {0}", Path.GetFileName(filename));
                    // Print some OS and other information
                    Console.WriteLine("System Information:");
                    Console.WriteLine("    OS Version: " + Environment.OSVersion);
                    Console.WriteLine("    64 bit OS: " + Environment.Is64BitOperatingSystem);
                    Console.WriteLine("    Computer: " + Environment.MachineName);
                    Console.WriteLine("    number Cores: " + Environment.ProcessorCount);
                    Console.WriteLine("    Date: " + DateTime.Now);
                }

                if (mode == "infoR"){
		 rawFile.PrintInfoAsRcode();
		 return;
		}
                // Display all of the trailer extra data fields present in the RAW file

                // Get the number of filters present in the RAW file
                int numberFilters = rawFile.GetFilters().Count;

                // Get the scan filter for the first and last spectrum in the RAW file
                //var firstFilter = rawFile.GetFilterForScanNumber(firstScanNumber);
                //var lastFilter = rawFile.GetFilterForScanNumber(lastScanNumber);

                if (mode == "scanFilter")
                {
                    foreach (var filter in rawFile.GetFilters())
                    {
                        Console.WriteLine(filter.ToString());
                    }
                    Environment.Exit(0);
                }
                
                if (mode == "version")
                {
                     Console.WriteLine("version={}", rawR_version);   
                     Environment.Exit(0);
                }
                
                if (mode == "chromatogram")
                {
                    // Get the BasePeak chromatogram for the MS data
                    GetChromatogram(rawFile, firstScanNumber, lastScanNumber, true);
                    Environment.Exit(0);
                }

                if (mode == "qc")
                {
                    

                    if (args.Length == 3)
                    {
                        var outputFilename = args[2].ToString();
                        rawFile.ExtractQualityControlTable(outputFilename);
                        
                    }else if (args.Length == 2)
                    {
                        rawFile.ExtractQualityControlTable($"/dev/stdout");
                        
                    }
                    else
                    {
                        Console.WriteLine("ERROR");
                        Environment.Exit(1);
                    }
            
                    Environment.Exit(0);
                }

                if (mode == "scans")
                {
                    List<int> scans = new List<int>();

                    var scanfile = args[2];
                    int scanNumber;

                    foreach (var line in File.ReadAllLines(scanfile))
                    {

                        Int32.TryParse(line, out scanNumber);
                        scans.Add(scanNumber);
                    }

                    rawFile.WriteSpectrumAsRcode(args[3], scans);

                    return;

                }

                if (mode == "xic")
                {
                    try   
                    {
                        var inputFilename = args[2];
                        double ppmError = Convert.ToDouble(args[3]);
                        var outputFilename = args[4];
                        List<double> massList = new List<double>();
                        if (File.Exists(args[2]))
                        {


                            foreach (var line in File.ReadAllLines(inputFilename))
                            {
                                massList.Add(Convert.ToDouble(line));
                            }

                            GetXIC(rawFile, -1, -1, massList, ppmError, outputFilename);
                        }

                        return;
                    }
                    catch (Exception ex)
                    {
                        Console.Error.WriteLine("failed to catch configfile and itol");
                        Console.Error.WriteLine("{}", ex.Message);
                        return;
                    }
                }
            }

            catch (Exception ex)
            {
                Console.WriteLine("Error accessing RAWFileReader library! - " + ex.Message);
            }

            // Get the memory used at the end of processing
            Process processAfter = Process.GetCurrentProcess();
            long memoryAfter = processAfter.PrivateMemorySize64 / 1024;

            Console.WriteLine();
            Console.WriteLine("Memory Usage:");
            Console.WriteLine("   Before {0} kb, After {1} kb, Extra {2} kb", memoryBefore, memoryAfter,
                memoryAfter - memoryBefore);
        }


        /// <summary>
        /// Reads the base peak chromatogram for the RAW file
        /// </summary>
        /// <param name="rawFile">
        /// The RAW file being read
        /// </param>
        /// <param name="startScan">
        /// Start scan for the chromatogram
        /// </param>
        /// <param name="endScan">
        /// End scan for the chromatogram
        /// </param>
        /// <param name="outputData">
        /// The output data flag.
        /// </param>
        private static void GetChromatogram(IRawDataPlus rawFile, int startScan, int endScan, bool outputData)
        {
            // Define the settings for getting the Base Peak chromatogram
            ChromatogramTraceSettings settings = new ChromatogramTraceSettings(TraceType.BasePeak);

            // Get the chromatogram from the RAW file. 
            var data = rawFile.GetChromatogramData(new IChromatogramSettings[] {settings}, startScan, endScan);

            // Split the data into the chromatograms
            var trace = ChromatogramSignal.FromChromatogramData(data);

            if (trace[0].Length > 0)
            {
                // Print the chromatogram data (time, intensity values)
                Console.WriteLine("# Base Peak chromatogram ({0} points)", trace[0].Length);

                Console.WriteLine("scan,rt,intensity");
                if (outputData)
                {
                    for (int i = 0; i < trace[0].Length; i++)
                    {
                        Console.WriteLine("{0},{1:F3},{2:F0}", i, trace[0].Times[i], trace[0].Intensities[i]);
                    }
                }
            }

            Console.WriteLine();
        }


        private static void GetXIC(IRawDataPlus rawFile, int startScan, int endScan, List<double> massList,
            double ppmError, string filename)
        {

            List<ChromatogramTraceSettings> settingList = new List<ChromatogramTraceSettings>();

            foreach (var mass in massList)
            {

                double massError = (0.5 * ppmError * mass) / 1000000;
                ChromatogramTraceSettings settings = new ChromatogramTraceSettings(TraceType.MassRange)
                {
                    Filter = "ms",
                    MassRanges = new[] {ThermoFisher.CommonCore.Data.Business.Range.Create(mass - massError, mass + massError)}
                };

                settingList.Add(settings);
            }

            IChromatogramSettings[] allSettings = settingList.ToArray();

            var data = rawFile.GetChromatogramData(allSettings, startScan, endScan);

            
            // Split the data into the chromatograms
            var trace = ChromatogramSignal.FromChromatogramData(data);

            using (System.IO.StreamWriter file =
                new System.IO.StreamWriter(filename))
            {
                //file.WriteLine("#R\ne <- new.env(); e$XIC <- list()");
                file.WriteLine("#R\ne$XIC <- list()");


                for (int i = 0; i < trace.Length; i++)
                {
                    
                    
                    List<double> tTime = new List<double>();
                    List<double> tIntensities = new List<double>();
                    
                    for (int j = 0; j < trace[i].Times.Count; j++)
                    {
                        if (trace[i].Intensities[j] > 0)
                        {
                            tTime.Add(trace[i].Times[j]);
                            tIntensities.Add(trace[i].Intensities[j]);
                        }
                        
                    }
                   
                    file.WriteLine("e$XIC[[{0}]] <- list(", i + 1);
                    file.WriteLine("\tmass = {0},", massList[i]);

                    file.WriteLine("\ttimes = c(" + string.Join(",", tTime) + "),");
                    file.WriteLine("\tintensities = c(" + string.Join(",", tIntensities) + ")");
                    
                    file.WriteLine(");");
                }
            }
        }
    


    /// <summary>
        /// Gets the spectrum from the RAW file.
        /// </summary>
        /// <param name="rawFile">
        
        private static void ComputeXIC(IRawDataPlus rawFile, int firstScanNumber, int lastScanNumber, bool outputData)
        {
            for (int scanNumber = firstScanNumber; scanNumber <= lastScanNumber; scanNumber++)
            {
                try
                {
                    // Get the scan filter for the spectrum
                    var scanFilter = rawFile.GetFilterForScanNumber(firstScanNumber);  
                    
                    if (string.IsNullOrEmpty(scanFilter.ToString()))
                    {
                        continue;
                    }

                    // Get the scan from the RAW file.  This method uses the Scan.FromFile method which returns a
                    // Scan object that contains both the segmented and centroid (label) data from an FTMS scan
                    // or just the segmented data in non-FTMS scans.  The GetSpectrum method demonstrates an
                    // alternative method for reading scans.
                    var scan = Scan.FromFile(rawFile, scanNumber);
                    
                    // If that scan contains FTMS data then Centroid stream will be populated so check to see if it is present.
                    int labelSize = 0;

                    if (scan.HasCentroidStream)
                    {
                        labelSize = scan.CentroidScan.Length;
                    }

                    // For non-FTMS data, the preferred data will be populated
                    int dataSize = scan.PreferredMasses.Length;

                    if (outputData)
                    {
                        Console.WriteLine("Spectrum {0} - {1}: normal {2}, label {3} points", scanNumber, scanFilter.ToString(), dataSize, labelSize);
                    }
                }
                catch (Exception ex)
                {
                    Console.WriteLine("Error reading spectrum {0} - {1}", scanNumber, ex.Message);
                }
            }
        }

        /// <summary>
        /// Read all spectra in the RAW file.
        /// </summary>
        /// <param name="rawFile">
        /// The raw file.
        /// </param>
        /// <param name="firstScanNumber">
        /// The first scan number.
        /// </param>
        /// <param name="lastScanNumber">
        /// The last scan number.
        /// </param>
        /// <param name="outputData">
        /// The output data flag.
        /// </param>
        [HandleProcessCorruptedStateExceptions]
        private static void ReadAllSpectra(IRawDataPlus rawFile, int firstScanNumber, int lastScanNumber, bool outputData)
        {
            for (int scanNumber = firstScanNumber; scanNumber <= lastScanNumber; scanNumber++)
            {
                try
                {
                    // Get the scan filter for the spectrum
                    var scanFilter = rawFile.GetFilterForScanNumber(firstScanNumber);  
                    
                    if (string.IsNullOrEmpty(scanFilter.ToString()))
                    {
                        continue;
                    }

                    // Get the scan from the RAW file.  This method uses the Scan.FromFile method which returns a
                    // Scan object that contains both the segmented and centroid (label) data from an FTMS scan
                    // or just the segmented data in non-FTMS scans.  The GetSpectrum method demonstrates an
                    // alternative method for reading scans.
                    var scan = Scan.FromFile(rawFile, scanNumber);
                    
                    // If that scan contains FTMS data then Centroid stream will be populated so check to see if it is present.
                    int labelSize = 0;

                    if (scan.HasCentroidStream)
                    {
                        labelSize = scan.CentroidScan.Length;
                    }

                    // For non-FTMS data, the preferred data will be populated
                    int dataSize = scan.PreferredMasses.Length;

                    if (outputData)
                    {
                        Console.WriteLine("Spectrum {0} - {1}: normal {2}, label {3} points", scanNumber, scanFilter.ToString(), dataSize, labelSize);
                    }
                }
                catch (Exception ex)
                {
                    Console.WriteLine("Error reading spectrum {0} - {1}", scanNumber, ex.Message);
                }
            }
        }
    }
}
