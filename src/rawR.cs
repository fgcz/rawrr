    /*
      aGetTrailerExtraInformationdapded from the ThermoFischer `Hello, world!` example provided by Jim Shofstahl 
      see URL http://planetorbitrap.com/rawfilereader#.WjkqIUtJmL4
      the ThermoFisher library has to be manual downloaded and installed
      Please read the License document
      Witold Wolski <wew@fgcz.ethz.ch> and Christian Panse <cp@fgcz.ethz.ch> and Christian Trachsel
      2017-09-25 Zurich, Switzerland
      2018-04-24 Zurich, Switzerland
      2018-06-04 San Diego, CA, USA added xic option
      2018-06-28 added xic and scan option
      2018-07-24 bugfix
      2018-11-23 added scanFilter option
      2019-01-28 extract monoisotopicmZ attribute; include segments in MGF iff no centroid data are availbale
      2019-05-28 save info as Yaml
      2020-08-12 added infoR option
      2020-08-26 readSpectrum backend
    */
     
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
        /// <summary>
        /// The StringExtension  class
        /// </summary>
        public static class StringExtension
        {

            /// <summary>
            /// make all the existing header names distinct
            /// </summary>
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

        /// <summary>
        /// utilize the new ThermoFisher RawFileReader
        /// </summary>
        public static class IRawDataPlusExtension
        {
    /*
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


    */

            /// <summary>
            /// write file header (metainfo) into R code
            /// </summary>
            public static void PrintInfoAsRcode(this IRawDataPlus rawFile)
	    {
                    Console.WriteLine("#R\n\n");
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

            public static void WriteSpectrumAsRcode0(this IRawDataPlus rawFile, string filename)
            {

                    int scanNumber = 1;
                    var trailerFields = rawFile.GetTrailerExtraHeaderInformation();

		    int idx_CHARGE = -1;
                        try
                        {
                            idx_CHARGE = trailerFields
                                .Select((item, index) => new
                                {
                                    name = item.Label.ToString(),
                                    Position = index
                                })
                                .First(x => x.name.Contains("Charge State")).Position;
                        }
                        catch
                        {
		        }

             	    int firstScanNumber = rawFile.RunHeaderEx.FirstSpectrum;
            	    int lastScanNumber = rawFile.RunHeaderEx.LastSpectrum;
		        int charge = -1;
		        double pc=-1;

                using (System.IO.StreamWriter file =
                    new System.IO.StreamWriter(filename))
                {
             	    for  (scanNumber = firstScanNumber; scanNumber < lastScanNumber; scanNumber++){
                        var scanTrailer = rawFile.GetTrailerExtraInformation(scanNumber);
                        var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
                        var scanEvent = rawFile.GetScanEventForScanNumber(scanNumber);

		       
		        try{
                        var reaction0 = scanEvent.GetReaction(0);
		        pc =  reaction0.PrecursorMass;
		        }
		        catch{
			        pc = -1;
		        }

		        try{
                    	    charge = int.Parse(scanTrailer.Values.ToArray()[idx_CHARGE]);
                        }
		        catch {
			        charge=-1;
		        }

                        file.WriteLine("e$Spectrum[[{0}]] <- list(", scanNumber);
                        file.WriteLine("\tscan = {0},", scanNumber);
                        file.WriteLine("\tscanType = \"{0}\",", scanStatistics.ScanType.ToString());
                        file.WriteLine("\trtinseconds = {0},", Math.Round(scanStatistics.StartTime * 60 * 1000) / 1000);
                        file.WriteLine("\tprecursorMass = {0},", pc);
                        file.WriteLine("\tcharge = {0}", charge);
                                file.WriteLine(")");
		    }
	        }
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
                    foreach (int scanNumber in L)
                    {

                        var trailerFields = rawFile.GetTrailerExtraHeaderInformation();
                        var pepmass = -1.0;
                        var charge = 0;
                        var monoisotopicMz = "NA";
                        var basepeakMass = -1.0;
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
                                basepeakMass =  (scanStatistics.BasePeakMass);
                                basepeakIntensity =  Math.Round(scanStatistics.BasePeakIntensity);
                                charge = int.Parse(scanTrailer.Values.ToArray()[idx_CHARGE]);
                                monoisotopicMz = scanTrailer.Values.ToArray()[idx_PEPMASS];
                        }
                        catch
                        {
                            // Console.WriteLine("catch");
                            pepmass = -1.0;
                            basepeakMass = -1.0;
                            basepeakIntensity = -1.0;
                            charge = 0;
                            monoisotopicMz = "NA";
                        }

                            var scan = Scan.FromFile(rawFile, scanNumber);

                                file.WriteLine("e$Spectrum[[{0}]] <- list(", count++);
                                file.WriteLine("\tscan = {0},", scanNumber);
                                file.WriteLine("\tbasePeak = c({0}, {1}),", basepeakMass, basepeakIntensity);
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
                                file.WriteLine("\tnoises = c(" + string.Join(", ", centroidStream.Noises) + "),");
                                file.WriteLine("\tcharges = c(" + string.Join(", ", centroidStream.Charges) + "),");
                                file.WriteLine("\tbaselines = c(" + string.Join(", ", centroidStream.Baselines) + "),");

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
                                file.WriteLine("\tintensity = c(" + string.Join(",", scan.SegmentedScan.Intensities) + "),");
                            }
			    // ============= Instrument Data =============
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


                    if (mode == "scans")
                    {
                        List<int> scans = new List<int>();

                        var scanfile = args[2];
                        int scanNumber;

                        foreach (var line in File.ReadAllLines(scanfile))
                        {

			    try{
                            Int32.TryParse(line, out scanNumber);
			    if (scanNumber > 0)
                        	    scans.Add(scanNumber);
			    }
			    catch{}
                        }

		        if (scans.Count == 0)
                    	    rawFile.WriteSpectrumAsRcode0(args[3]);
		        else
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
			    string filter = "ms";
			    try {
				    filter = args[5];
			    }
			    catch{
			    }
                            List<double> massList = new List<double>();
                            if (File.Exists(args[2]))
                            {
                                foreach (var line in File.ReadAllLines(inputFilename))
                                {
                                    massList.Add(Convert.ToDouble(line));
                                }

                                ExtractChromatogram(rawFile, -1, -1, massList, ppmError, outputFilename, filter);
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

	    private static bool IsValidFilter(IRawDataPlus rawFile, string filter)
	    {
		    if (rawFile.GetFilterFromString(filter) == null) {
			    return false;
		    } 
		    return true; 
	    }

            private static void ExtractChromatogram(IRawDataPlus rawFile, int startScan, int endScan, List<double> massList,
                double ppmError, string filename, string filter = "ms")
            {

	        if (IsValidFilter(rawFile, filter) == false){
                    using (System.IO.StreamWriter file =
                        new System.IO.StreamWriter(filename))
                    {
                        file.WriteLine("e$error <- \"'{0}' is not a valid filter string.\";", filter);
		    }
		    return;
	        }

                List<ChromatogramTraceSettings> settingList = new List<ChromatogramTraceSettings>();

                foreach (var mass in massList)
                {
                    double massError = (0.5 * ppmError * mass) / 1000000;
                    ChromatogramTraceSettings settings = new ChromatogramTraceSettings(TraceType.MassRange)
                    {
                        Filter = filter,
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
                    file.WriteLine("#R\n");

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
                       
                        file.WriteLine("e$chromatogram[[{0}]] <- list(", i + 1);
                        file.WriteLine("\tfilter = '{0}',", filter);
                        file.WriteLine("\tppm = {0},", ppmError);
                        file.WriteLine("\tmass = {0},", massList[i]);
                        file.WriteLine("\ttimes = c(" + string.Join(",", tTime) + "),");
                        file.WriteLine("\tintensities = c(" + string.Join(",", tIntensities) + ")");
                        file.WriteLine(");");
                    }
                }
            }
        }
    }
