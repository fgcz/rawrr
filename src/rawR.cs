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

        

    private static void PrintMGFSpectrum(this IRawDataPlus rawFile, int scanNumber)
        {
            var trailerFields = rawFile.GetTrailerExtraHeaderInformation();
            
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

            var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
            var centroidStream = rawFile.GetCentroidStream(scanNumber, false);
            var scanEvent = rawFile.GetScanEventForScanNumber(scanNumber);
            
            // Get the centroid (label) data from the RAW file for this scan
            var scanTrailer = rawFile.GetTrailerExtraInformation(scanNumber);
            var reaction0 = scanEvent.GetReaction(0);
            
            double pepmass = Convert.ToDouble(scanTrailer.Values.ToArray()[idx_PEPMASS]);
            
            if (pepmass == 0.0)
            {
                Console.WriteLine("# PrecursorMass");
                pepmass = Convert.ToDouble(reaction0.PrecursorMass);
            }

            Console.WriteLine("BEGIN IONS");
            Console.WriteLine("TITLE=File: \"{0}\"; SpectrumID: \"{1}\"; scans: \"{2}\"", 
                Path.GetFileName(rawFile.FileName),
                null,
                scanNumber);
            
            
            Console.WriteLine("PEPMASS={0:F5}", pepmass);
                //Math.Round(scanStatistics.BasePeakIntensity));
            Console.WriteLine("CHARGE={0}+", scanTrailer.Values.ToArray()[idx_CHARGE]);
            Console.WriteLine("RTINSECONDS={0:F0}", Math.Round(scanStatistics.StartTime * 60 * 100)/100);
            Console.WriteLine("SCANS={0}", scanNumber);

            if (scanStatistics.IsCentroidScan && centroidStream.Length > 0)
            {
                for (int i = 0; i < centroidStream.Length; i++)
                {
                    Console.WriteLine("{0:F5} {1:F2}", centroidStream.Masses[i], centroidStream.Intensities[i]);
                }
            }
            else
            {
                var scan = Scan.FromFile(rawFile, scanNumber);
                Console.WriteLine("# No centroid stream available");
                var mZ = scan.SegmentedScan.Positions;
                var intensity = scan.SegmentedScan.Intensities;
                
                for (int i = 0; i < scan.SegmentedScan.Positions.Length; i++)
                {
                    Console.WriteLine("{0:F5} {1:F2}", mZ[i], intensity[i]);
                }
            }
            Console.WriteLine("END IONS\n");
        }

        
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
                Console.WriteLine("#R\ne <- new.env();\ne$info <- list()");
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
        public static void PrintScanAsRcode(this IRawDataPlus rawFile, string filename, List<int> L)
        {
            int count = 1;
            using (System.IO.StreamWriter file =
                new System.IO.StreamWriter(filename))
            {
                file.WriteLine("e <- new.env(); e$PeakList <- list()");
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

                        if (scanStatistics.IsCentroidScan && centroidStream.Length > 0)
                        {
                            // Get the centroid (label) data from the RAW file for this scan

                            file.WriteLine("e$PeakList[[{0}]] <- list(", count++);
                            file.WriteLine("\tscan = {0},", scanNumber);
                            file.WriteLine("\tscanType = \"{0}\",", scanStatistics.ScanType.ToString());
                            file.WriteLine("\trtinseconds = {0},",
                                Math.Round(scanStatistics.StartTime * 60 * 1000) / 1000);
                            file.WriteLine("\tpepmass = c({0}, {1}),", pepmass, basepeakIntensity);
                            file.WriteLine("\tcentroidStream = TRUE,");

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
                            file.WriteLine("\tmZ = c(" + string.Join(",", centroidStream.Masses) + "),");
                            file.WriteLine("\tintensity = c(" + string.Join(",", centroidStream.Intensities) + ")");
                            file.WriteLine(")");

                        }
                        else
                        {
                            
                            file.WriteLine("e$PeakList[[{0}]] <- list(", count++);
                            file.WriteLine("\tscan = {0},", scanNumber);
                            file.WriteLine("\tscanType = \"{0}\",", scanStatistics.ScanType.ToString());
                            file.WriteLine("\trtinseconds = {0},",
                                Math.Round(scanStatistics.StartTime * 60 * 1000) / 1000);
                            file.WriteLine("\tpepmass = c({0}, {1}),", pepmass, basepeakIntensity);
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
                            file.WriteLine(")");
                            
                        }
                }
            }

            return;
        }

        /// <summary>
        /// TODO(cp): replace the console output by a text stream as return value
        /// </summary>
        /// <param name="rawFile"></param>
        public static void ExtractMascotGenericFile(this IRawDataPlus rawFile)
        {
            int firstScanNumber = rawFile.RunHeaderEx.FirstSpectrum;
            int lastScanNumber = rawFile.RunHeaderEx.LastSpectrum;
     
            Console.WriteLine("MASS=Monoisotopic");
            foreach (var scanNumber in Enumerable.Range(1, lastScanNumber - firstScanNumber))
            {
                if ( rawFile.GetFilterForScanNumber(scanNumber).ToString().Contains("Full ms2"))
                {
                    rawFile.PrintMGFSpectrum(scanNumber);
                }
            }
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
            bool analyzeScans = false;
            string rawDiagVersion = "0.0.35";

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
                    {"info", "print the raw file's meta data."},
                    {"infoR", "print the raw file's meta data as R code."},
                    {"chromatogram", "base peak chromatogram."},
                    {"mgf", "writes MS2 CentroidStream as mascot generic file (mgf)."},
                    {"qc", "prints a qc table having one entry per scan."},
                    {"xic", "prints xic unfiltered."},
                    {"tic", "total ion count."},
                    {"scans", "print spectrum of scanid as Rcode."}
                };

                if (args.Length > 0)
                {
                    filename = args[0];

                    if (args.Length == 1)
                    {
                        Console.WriteLine("rawDiag version = {}", rawDiagVersion);
                        Console.WriteLine("missing mode argument. setting to mode = 'info'.");
                        mode = "info";
                    }
                    else
                    {
                        mode = args[1];
                    }


                    if (!hashtable.Contains(mode))
                    {
                        Console.WriteLine("rawDiag version = {}", rawDiagVersion);
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
                    Console.WriteLine("rawDiag version = {}", rawDiagVersion);
                    Console.WriteLine(@"The file doesn't exist in the specified location - " + filename);
                    
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

                if (mode == "info")
                {
                    // Get the number of instruments (controllers) present in the RAW file and set the 
                    // selected instrument to the MS instrument, first instance of it
                    Console.WriteLine("Number of instruments: {0}", rawFile.InstrumentCount);
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
                if (mode == "info")
                {

                    // Get some information from the header portions of the RAW file and display that information.
                    // The information is general information pertaining to the RAW file.
                    Console.WriteLine("General File Information:");
                    Console.WriteLine("    RAW file: " + Path.GetFileName(rawFile.FileName));
                    Console.WriteLine("    RAW file version: " + rawFile.FileHeader.Revision);
                    Console.WriteLine("    Creation date: " + rawFile.FileHeader.CreationDate);
                    Console.WriteLine("    Operator: " + rawFile.FileHeader.WhoCreatedId);
                    Console.WriteLine("    Number of instruments: " + rawFile.InstrumentCount);
                    Console.WriteLine("    Description: " + rawFile.FileHeader.FileDescription);
                    Console.WriteLine("    Instrument model: " + rawFile.GetInstrumentData().Model);
                    Console.WriteLine("    Instrument name: " + rawFile.GetInstrumentData().Name);
//                    Console.WriteLine("   Instrument method: {0}", rawFile.GetAllInstrumentFriendlyNamesFromInstrumentMethod().Length);
                    Console.WriteLine("    Serial number: " + rawFile.GetInstrumentData().SerialNumber);
                    Console.WriteLine("    Software version: " + rawFile.GetInstrumentData().SoftwareVersion);
                    Console.WriteLine("    Firmware version: " + rawFile.GetInstrumentData().HardwareVersion);
                    Console.WriteLine("    Units: " + rawFile.GetInstrumentData().Units);
                    Console.WriteLine("    Mass resolution: {0:F3} ", rawFile.RunHeaderEx.MassResolution);
                    Console.WriteLine("    Number of scans: {0}", rawFile.RunHeaderEx.SpectraCount);
                    Console.WriteLine("    Number of ms2 scans: {0}",
                        Enumerable
                            .Range(1, lastScanNumber - firstScanNumber)
                            .Count(x => rawFile.GetFilterForScanNumber(x)
                                .ToString()
                                .Contains("Full ms2")));
                    Console.WriteLine("    Scan range: [{0}, {1}]", firstScanNumber, lastScanNumber);
                    Console.WriteLine("    Time range: [{0:F2}, {1:F2}]", startTime, endTime);
                    Console.WriteLine("    Mass range: [{0:F4}, {1:F4}]", rawFile.RunHeaderEx.LowMass,
                        rawFile.RunHeaderEx.HighMass);
                    Console.WriteLine();

                    // Get information related to the sample that was processed
                    Console.WriteLine("Sample Information:");
                    Console.WriteLine("    Sample name: " + rawFile.SampleInformation.SampleName);
                    Console.WriteLine("    Sample id: " + rawFile.SampleInformation.SampleId);
                    Console.WriteLine("    Sample type: " + rawFile.SampleInformation.SampleType);
                    Console.WriteLine("    Sample comment: " + rawFile.SampleInformation.Comment);
                    Console.WriteLine("    Sample vial: " + rawFile.SampleInformation.Vial);
                    Console.WriteLine("    Sample volume: " + rawFile.SampleInformation.SampleVolume);
                    Console.WriteLine("    Sample injection volume: " + rawFile.SampleInformation.InjectionVolume);
                    Console.WriteLine("    Sample row number: " + rawFile.SampleInformation.RowNumber);
                    Console.WriteLine("    Sample dilution factor: " + rawFile.SampleInformation.DilutionFactor);
                    Console.WriteLine();

                    // Read the first instrument method (most likely for the MS portion of the instrument).
                    // NOTE: This method reads the instrument methods from the RAW file but the underlying code
                    // uses some Microsoft code that hasn't been ported to Linux or MacOS.  Therefore this
                    // method won't work on those platforms therefore the check for Windows.
                    if (Environment.OSVersion.ToString().Contains("Windows"))
                    {
                        var deviceNames = rawFile.GetAllInstrumentNamesFromInstrumentMethod();

                        foreach (var device in deviceNames)
                        {
                            Console.WriteLine("Instrument method: " + device);
                        }

                        Console.WriteLine();
                    }
                }

                // Display all of the trailer extra data fields present in the RAW file

                // Get the number of filters present in the RAW file
                int numberFilters = rawFile.GetFilters().Count;

                // Get the scan filter for the first and last spectrum in the RAW file
                var firstFilter = rawFile.GetFilterForScanNumber(firstScanNumber);
                var lastFilter = rawFile.GetFilterForScanNumber(lastScanNumber);

                if (mode == "scanFilter")
                {
                    foreach (var filter in rawFile.GetFilters())
                    {
                        Console.WriteLine(filter.ToString());
                    }
                    Environment.Exit(0);
                }
                
                if (mode == "info")
                {
                    Console.WriteLine("Filter Information:");
                    Console.WriteLine("    Scan filter (first scan): " + firstFilter.ToString());
                    Console.WriteLine("    Scan filter (last scan): " + lastFilter.ToString());
                    Console.WriteLine("    Total number of filters: " + numberFilters);
                    Console.WriteLine();
                    //  ListTrailerExtraFields(rawFile);
                    Environment.Exit(0);
                }

                if (mode == "version")
                {
                     Console.WriteLine("version={}", rawDiagVersion);   
                     Environment.Exit(0);
                }
                
                if (mode == "chromatogram")
                {
                    // Get the BasePeak chromatogram for the MS data
                    GetChromatogram(rawFile, firstScanNumber, lastScanNumber, true);
                    Environment.Exit(0);
                }

                if (mode == "tic")
                {
                    Console.WriteLine("Scan\tIntensitySum\tTIC");

                    foreach (var i in Enumerable
                        .Range(1, lastScanNumber - firstScanNumber)
                        .Where(x => rawFile.GetFilterForScanNumber(x)
                            .ToString()
                            .Contains(" ms ")))
                    {

                        var scanStatistics = rawFile.GetScanStatsForScanNumber(i);
                        // var scanFilter = rawFile.GetFilterForScanNumber(i);

                        Console.WriteLine("{0}\t{1:F2}\t{2}",
                            i, GetIntensitySum(rawFile, i, firstFilter.ToString(), true), scanStatistics.TIC);
                    }

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

                if (mode == "mgf")
                {
                    rawFile.ExtractMascotGenericFile();
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

                    rawFile.PrintScanAsRcode(args[3], scans);

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

                if (mode == "allScans")
                {
                    Console.WriteLine("BEGIN DEBUG");
                    GetSpectrum(rawFile, firstScanNumber, firstFilter.ToString(), false);
                    GetSpectrum(rawFile, 100, firstFilter.ToString(), false);
                    GetSpectrum(rawFile, 101, firstFilter.ToString(), false);
                    GetSpectrum(rawFile, 102, firstFilter.ToString(), false);
                    //GetSpectrum(rawFile, 103, firstFilter.ToString(), true);
                    Console.WriteLine("END DEBUG");


                    // Read the scan information for each scan in the RAW file
                    ReadScanInformation(rawFile, firstScanNumber, lastScanNumber, true);

                    // Get a spectrum from the RAW file.  
                    //Environment.Exit(0);

                    // Get a average spectrum from the RAW file for the first 15 scans in the file.  
                    GetAverageSpectrum(rawFile, 1, 15, false);
                    Environment.Exit(0);

                    // Read each spectrum
                    //ReadAllSpectra(rawFile, firstScanNumber, lastScanNumber, true);

                    // Calculate the mass precision for a spectrum
                    CalculateMassPrecision(rawFile, 1);

                    // Check all of the scans for out of order data.  This method isn't enabled by
                    // default because it is very, very time consuming.  If you would like to 
                    // call this method change the value of _analyzeScans to true.
                    if (analyzeScans)
                    {
                        AnalyzeAllScans(rawFile, firstScanNumber, lastScanNumber);
                    }

                    // Close (dispose) the RAW file
                    Console.WriteLine();
                    Console.WriteLine("Closing " + filename);

                    rawFile.Dispose();
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
        /// Reads all of the scans in the RAW and looks for out of order data
        /// </summary>
        /// <param name="rawFile">
        /// The RAW file being read
        /// </param>
        /// <param name="firstScanNumber">
        /// The first scan in the RAW file
        /// </param>
        /// <param name="lastScanNumber">
        /// the last scan in the RAW file
        /// </param>
        private static void AnalyzeAllScans(IRawDataPlus rawFile, int firstScanNumber, int lastScanNumber)
        {
            // Test the preferred (normal) data and centroid (high resolution/label) data
            int failedCentroid = 0;
            int failedPreferred = 0;

            for (int scanNumber = firstScanNumber; scanNumber <= lastScanNumber; scanNumber++)
            {
                // Get each scan from the RAW file
                var scan = Scan.FromFile(rawFile, scanNumber);

                // Check to see if the RAW file contains label (high-res) data and if it is present
                // then look for any data that is out of order
                if (scan.HasCentroidStream)
                {
                    if (scan.CentroidScan.Length > 0)
                    {
                        double currentMass = scan.CentroidScan.Masses[0];

                        for (int index = 1; index < scan.CentroidScan.Length; index++)
                        {
                            if (scan.CentroidScan.Masses[index] > currentMass)
                            {
                                currentMass = scan.CentroidScan.Masses[index];
                            }
                            else
                            {
                                if (failedCentroid == 0)
                                {
                                    Console.WriteLine("First failure: Failed in scan data at: Scan: " + scanNumber +
                                                      " Mass: "
                                                      + currentMass.ToString("F4"));
                                }

                                failedCentroid++;
                            }
                        }
                    }
                }

                // Check the normal (non-label) data in the RAW file for any out-of-order data
                if (scan.PreferredMasses.Length > 0)
                {
                    double currentMass = scan.PreferredMasses[0];

                    for (int index = 1; index < scan.PreferredMasses.Length; index++)
                    {
                        if (scan.PreferredMasses[index] > currentMass)
                        {
                            currentMass = scan.PreferredMasses[index];
                        }
                        else
                        {
                            if (failedPreferred == 0)
                            {
                                Console.WriteLine("First failure: Failed in scan data at: Scan: " + scanNumber +
                                                  " Mass: "
                                                  + currentMass.ToString("F2"));
                            }

                            failedPreferred++;
                        }
                    }
                }
            }

            // Display a message indicating if any of the scans had data that was "out of order"
            if (failedPreferred == 0 && failedCentroid == 0)
            {
                Console.WriteLine("Analysis completed: No out of order data found");
            }
            else
            {
                Console.WriteLine("Analysis completed: Preferred data failed: " + failedPreferred +
                                  " Centroid data failed: " + failedCentroid);
            }
        }

        /// <summary>
        /// Calculates the mass precision for a spectrum.  
        /// </summary>
        /// <param name="rawFile">
        /// The RAW file being read
        /// </param>
        /// <param name="scanNumber">
        /// The scan to process
        /// </param>
        private static void CalculateMassPrecision(IRawDataPlus rawFile, int scanNumber)
        {
            // Get the scan from the RAW file
            var scan = Scan.FromFile(rawFile, scanNumber);

            // Get the scan event and from the scan event get the analyzer type for this scan
            var scanEvent = rawFile.GetScanEventForScanNumber(scanNumber);

            // Get the trailer extra data to get the ion time for this file
            LogEntry logEntry = rawFile.GetTrailerExtraInformation(scanNumber);

            var trailerHeadings = new List<string>();
            var trailerValues = new List<string>();
            for (var i = 0; i < logEntry.Length; i++)
            {
                trailerHeadings.Add(logEntry.Labels[i]);
                trailerValues.Add(logEntry.Values[i]);
            }

            // Create the mass precision estimate object
            IPrecisionEstimate precisionEstimate = new PrecisionEstimate();

            // Get the ion time from the trailer extra data values
            var ionTime = precisionEstimate.GetIonTime(scanEvent.MassAnalyzer, scan, trailerHeadings, trailerValues);

            // Calculate the mass precision for the scan
            var listResults = precisionEstimate.GetMassPrecisionEstimate(scan, scanEvent.MassAnalyzer, ionTime,
                rawFile.RunHeader.MassResolution);

            // Output the mass precision results
            if (listResults.Count > 0)
            {
                Console.WriteLine("Mass Precision Results:");

                foreach (var result in listResults)
                {
                    Console.WriteLine("Mass {0:F5}, mmu = {1:F3}, ppm = {2:F2}", result.Mass, result.MassAccuracyInMmu,
                        result.MassAccuracyInPpm);
                }
            }
        }

        /// <summary>
        /// Gets the average spectrum from the RAW file.  
        /// </summary>
        /// <param name="rawFile">
        /// The RAW file being read
        /// </param>
        /// <param name="firstScanNumber">
        /// The first scan to consider for the averaged spectrum
        /// </param>
        /// <param name="lastScanNumber">
        /// The last scan to consider for the averaged spectrum
        /// </param>
        /// <param name="outputData">
        /// The output data flag.
        /// </param>
        private static void GetAverageSpectrum(IRawDataPlus rawFile, int firstScanNumber, int lastScanNumber,
            bool outputData)
        {
            // Create the mass options object that will be used when averaging the scans
            var options = rawFile.DefaultMassOptions();

            options.ToleranceUnits = ToleranceUnits.ppm;
            options.Tolerance = 5.0;

            // Get the scan filter for the first scan.  This scan filter will be used to located
            // scans within the given scan range of the same type
            var scanFilter = rawFile.GetFilterForScanNumber(firstScanNumber);

            // Get the average mass spectrum for the provided scan range. In addition to getting the
            // average scan using a scan range, the library also provides a similar method that takes
            // a time range.
            var averageScan = rawFile.AverageScansInScanRange(firstScanNumber, lastScanNumber, scanFilter, options);

            if (averageScan.HasCentroidStream)
            {
                Console.WriteLine("Average spectrum ({0} points)", averageScan.CentroidScan.Length);

                // Print the spectral data (mass, intensity values)
                if (outputData)
                {
                    for (int i = 0; i < averageScan.CentroidScan.Length; i++)
                    {
                        Console.WriteLine("  {0:F4} {1:F0}", averageScan.CentroidScan.Masses[0],
                            averageScan.CentroidScan.Intensities[i]);
                    }
                }
            }

            // This example uses a different method to get the same average spectrum that was calculated in the
            // previous portion of this method.  Instead of passing the start and end scan, a list of scans will
            // be passed to the GetAveragedMassSpectrum function.
            List<int> scans = new List<int>(new[] {1, 6, 7, 9, 11, 12, 14});

            averageScan = rawFile.AverageScans(scans, options);

            if (averageScan.HasCentroidStream)
            {
                Console.WriteLine("Average spectrum ({0} points)", averageScan.CentroidScan.Length);

                // Print the spectral data (mass, intensity values)
                if (outputData)
                {
                    for (int i = 0; i < averageScan.CentroidScan.Length; i++)
                    {
                        Console.WriteLine("  {0:F4} {1:F0}", averageScan.CentroidScan.Masses[0],
                            averageScan.CentroidScan.Intensities[i]);
                    }
                }
            }

            Console.WriteLine();
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
                file.WriteLine("#R\ne <- new.env(); e$XIC <- list()");


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
        /// The RAW file being read
        /// </param>
        /// <param name="scanNumber">
        /// The scan number being read
        /// </param>
        /// <param name="scanFilter">
        /// The scan filter for that scan
        /// </param>
        /// <param name="outputData">
        /// The output data flag.
        /// </param>
        private static void GetSpectrum(IRawDataPlus rawFile, int scanNumber, string scanFilter, bool outputData)
        {
            // Check for a valid scan filter
            if (string.IsNullOrEmpty(scanFilter))
            {
                return;
            }

            // Get the scan statistics from the RAW file for this scan number
            var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);

            // Check to see if the scan has centroid data or profile data.  Depending upon the
            // type of data, different methods will be used to read the data.  While the ReadAllSpectra
            // method demonstrates reading the data using the Scan.FromFile method, generating the
            // Scan object takes more time and memory to do, so that method isn't optimum.
            if (scanStatistics.IsCentroidScan)
            {
                // Get the centroid (label) data from the RAW file for this scan
                var centroidStream = rawFile.GetCentroidStream(scanNumber, false);

                Console.WriteLine("Spectrum (centroid/label) {0} - {1} points", scanNumber, centroidStream.Length);

                // Print the spectral data (mass, intensity, charge values).  Not all of the information in the high resolution centroid 
                // (label data) object is reported in this example.  Please check the documentation for more information about what is
                // available in high resolution centroid (label) data.
                if (outputData)
                {
                    for (int i = 0; i < centroidStream.Length; i++)
                    {
                        Console.WriteLine("  {0} - {1:F4}, {2:F0}, {3:F0}", i, centroidStream.Masses[i], centroidStream.Intensities[i], centroidStream.Charges[i]);
                    }
                }

                Console.WriteLine();
            }
            else
            {
                // Get the segmented (low res and profile) scan data
                var segmentedScan = rawFile.GetSegmentedScanFromScanNumber(scanNumber, scanStatistics);
                

                Console.WriteLine("Spectrum (normal data) {0} - {1} points", scanNumber, segmentedScan.Positions.Length);

                // Print the spectral data (mass, intensity values)
                if (outputData)
                {
                    for (int i = 0; i < segmentedScan.Positions.Length; i++)
                    {
                        Console.WriteLine("  {0} - {1:F4}, {2:F0}", i, segmentedScan.Positions[i], segmentedScan.Intensities[i]);
                    }
                }

                Console.WriteLine();
            }
        }

        private static double GetIntensitySum(IRawDataPlus rawFile, int scanNumber, string scanFilter, bool outputData)
        {
            // Check for a valid scan filter
            if (string.IsNullOrEmpty(scanFilter))
            {
                return 0.0;
            }

            double SumIntensity = 0.0;

            // Get the scan statistics from the RAW file for this scan number
            var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);

            // Check to see if the scan has centroid data or profile data.  Depending upon the
            // type of data, different methods will be used to read the data.  While the ReadAllSpectra
            // method demonstrates reading the data using the Scan.FromFile method, generating the
            // Scan object takes more time and memory to do, so that method isn't optimum.
            if (scanStatistics.IsCentroidScan)
            {
                var centroidStream = rawFile.GetCentroidStream(scanNumber, false);
                if (outputData)
                {
                    for (int i = 0; i < centroidStream.Length; i++)
                    {
                        // Console.WriteLine("  {0} - {1:F4}, {2:F0}, {3:F0}", i, centroidStream.Masses[i], centroidStream.Intensities[i], centroidStream.Charges[i]);
                        SumIntensity += centroidStream.Intensities[i];
                    }
                }

            }
            else
            {
                var segmentedScan = rawFile.GetSegmentedScanFromScanNumber(scanNumber, scanStatistics);
                if (outputData)
                {
                    for (int i = 0; i < segmentedScan.Positions.Length; i++)
                    {
                        SumIntensity += segmentedScan.Intensities[i];
                    }
                }
            }
            return (SumIntensity);
        }

        
        
        /// <summary>
        /// Reads and reports the trailer extra data fields present in the RAW file.
        /// </summary>
        /// <param name="rawFile">
        /// The RAW file
        /// </param>
        private static void ListTrailerExtraFields(IRawDataPlus rawFile)
        {
            // Get the Trailer Extra data fields present in the RAW file
            var trailerFields = rawFile.GetTrailerExtraHeaderInformation();

            // Display each value
            int i = 0;

            Console.WriteLine("Trailer Extra Data Information:");

            foreach (var field in trailerFields)
            {
                Console.WriteLine("   Field {0} = {1} storing data of type {2}", i, field.Label, field.DataType);

                i++;
            }

            Console.WriteLine();
        }
        
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

            
        /// <summary>
        /// Reads the general scan information for each scan in the RAW file using the scan filter object and also the
        /// trailer extra data section for that same scan.
        /// </summary>
        /// <param name="rawFile">
        /// The RAW file being read
        /// </param>
        /// <param name="firstScanNumber">
        /// The first scan in the RAW file
        /// </param>
        /// <param name="lastScanNumber">
        /// the last scan in the RAW file
        /// </param>
        /// <param name="outputData">
        /// The output data flag.
        /// </param>
        private static void ReadScanInformation(IRawDataPlus rawFile, int firstScanNumber, int lastScanNumber, bool outputData)
        {
            // Read each scan in the RAW File
            for (int scan = firstScanNumber; scan <= lastScanNumber; scan++)
            {
                // Get the retention time for this scan number.  This is one of two comparable functions that will
                // convert between retention time and scan number.
                double time = rawFile.RetentionTimeFromScanNumber(scan);

                // Get the scan filter for this scan number
                var scanFilter = rawFile.GetFilterForScanNumber(scan);

                // Get the scan event for this scan number
                var scanEvent = rawFile.GetScanEventForScanNumber(scan);
               
                // Get the ionizationMode, MS2 precursor mass, collision energy, and isolation width for each scan
                if (scanFilter.MSOrder == MSOrderType.Ms2)
                {
                    // Get the reaction information for the first precursor
                    var reaction = scanEvent.GetReaction(0);

                    double precursorMass = reaction.PrecursorMass;
                    double collisionEnergy = reaction.CollisionEnergy;
                    double isolationWidth = reaction.IsolationWidth;
                    double monoisotopicMass = 0.0;
                    int masterScan = 0;
                    var ionizationMode = scanFilter.IonizationMode;
                    var order = scanFilter.MSOrder;

                    // Get the trailer extra data for this scan and then look for the monoisotopic m/z value in the 
                    // trailer extra data list
                    var trailerData = rawFile.GetTrailerExtraInformation(scan);

                    for (int i = 0; i < trailerData.Length; i++)
                    {
                        if (trailerData.Labels[i] == "Monoisotopic M/Z:")
                        {
                            monoisotopicMass = Convert.ToDouble(trailerData.Values[i]);
                        }

                        if ((trailerData.Labels[i] == "Master Scan Number:") || (trailerData.Labels[i] == "Master Scan Number") || (trailerData.Labels[i] == "Master Index:"))
                        {
                            masterScan = Convert.ToInt32(trailerData.Values[i]);
                        }
                    }

                    if (outputData)
                    {
                        Console.WriteLine(
                            "Scan number: {0} @ time {1:F2} - Master scan = {2}, Ionization mode={3}, MS Order={4}, Precursor mass={5:F4}, Monoisotopic Mass = {6:F4}, Collision energy={7:F2}, Isolation width={8:F2}",
                            scan, time, masterScan, ionizationMode, order, precursorMass, monoisotopicMass, collisionEnergy, isolationWidth);
                    }
                }
                else if (scanFilter.MSOrder == MSOrderType.Ms)
                {
                    var scanDependents = rawFile.GetScanDependents(scan, 5);

                    Console.WriteLine(
                        "Scan number {0} @ time {1:F2} - Instrument type={2}, Number dependent scans={3}",
                        scan, time, scanDependents.RawFileInstrumentType, scanDependents.ScanDependentDetailArray.Length);
                }
            }
        }
    }
}

