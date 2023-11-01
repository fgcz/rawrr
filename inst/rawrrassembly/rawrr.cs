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
      2020-11-27 fix basePeak issue #21
      2020-08-12 added headerR option
      2020-08-26 readSpectrum backend
      2021-05-03 reorder xic arguments
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
            public static void PrintHeaderAsRcode(this IRawDataPlus rawFile, string filename)
	    {
		     using (System.IO.StreamWriter file =
		                         new System.IO.StreamWriter(filename))
		                     {
                    file.WriteLine("#R\n\n");
                    file.WriteLine("e$info <- list()\n");
                    file.WriteLine("e$info$`RAW file` <- '" + Path.GetFileName(rawFile.FileName) + "'");
                    file.WriteLine("e$info$`RAW file version` <- '" + rawFile.FileHeader.Revision + "'");
                    file.WriteLine("e$info$`Creation date` <- '" + rawFile.FileHeader.CreationDate + "'");
                    file.WriteLine("e$info$Operator <- '" + rawFile.FileHeader.WhoCreatedId + "'");
                    file.WriteLine("e$info$`Number of instruments` <- {0}", rawFile.InstrumentCount);
                    file.WriteLine("e$info$Description <- '" + rawFile.FileHeader.FileDescription + "'");
                    file.WriteLine("e$info$`Instrument model` <- '{0}'", rawFile.GetInstrumentData().Model);
                    file.WriteLine("e$info$`Instrument name` <- '{0}'", rawFile.GetInstrumentData().Name);
                    file.WriteLine("e$info$`Instrument method` <- '" + rawFile.SampleInformation.InstrumentMethodFile.Replace("\\", "/") + "'");
                    //file.WriteLine("e$info$`Instrument method` <- '{0}'", rawFile.GetAllInstrumentFriendlyNamesFromInstrumentMethod().Length);
                    file.WriteLine("e$info$`Serial number` <- '{0}'", rawFile.GetInstrumentData().SerialNumber);
                    file.WriteLine("e$info$`Software version` <- '{0}'", rawFile.GetInstrumentData().SoftwareVersion);
                    file.WriteLine("e$info$`Firmware version` <- '{0}'", rawFile.GetInstrumentData().HardwareVersion);
                    file.WriteLine("e$info$Units <- '{0}'", rawFile.GetInstrumentData().Units);
                    file.WriteLine("e$info$`Mass resolution` <- '{0:F3}'", rawFile.RunHeaderEx.MassResolution);
                    file.WriteLine("e$info$`Number of scans` <- {0}", rawFile.RunHeaderEx.SpectraCount);
             	    int firstScanNumber = rawFile.RunHeaderEx.FirstSpectrum;
            	    int lastScanNumber = rawFile.RunHeaderEx.LastSpectrum;
                    file.WriteLine("e$info$`Number of ms2 scans` <- {0}", Enumerable.Range(1, lastScanNumber - firstScanNumber).Count(x => rawFile.GetFilterForScanNumber(x).ToString().Contains("Full ms2")));
                    file.WriteLine("e$info$`Scan range` <- c({0}, {1})", firstScanNumber, lastScanNumber);
                    double startTime = rawFile.RunHeaderEx.StartTime;
                    double endTime = rawFile.RunHeaderEx.EndTime;
                    file.WriteLine("e$info$`Time range` <- c({0:F2}, {1:F2})", startTime, endTime);
                    file.WriteLine("e$info$`Mass range` <- c({0:F4}, {1:F4})", rawFile.RunHeaderEx.LowMass, rawFile.RunHeaderEx.HighMass);

                    var firstFilter = rawFile.GetFilterForScanNumber(firstScanNumber);
                    var lastFilter = rawFile.GetFilterForScanNumber(lastScanNumber);
                    int numberFilters = rawFile.GetFilters().Count;
                    file.WriteLine("e$info$`Scan filter (first scan)` <- '{0}'", firstFilter.ToString());
                    file.WriteLine("e$info$`Scan filter (last scan)` <- '{0}'", lastFilter.ToString());
                    file.WriteLine("e$info$`Total number of filters` <- '{0}'", numberFilters);

                    file.WriteLine("e$info$`Sample name` <- '{0}' ", rawFile.SampleInformation.SampleName);
                    file.WriteLine("e$info$`Sample id` <- '{0}' ", rawFile.SampleInformation.SampleId);
                    file.WriteLine("e$info$`Sample type` <- '{0}' ", rawFile.SampleInformation.SampleType);
                    file.WriteLine("e$info$`Sample comment` <- '{0}' ", rawFile.SampleInformation.Comment);
                    file.WriteLine("e$info$`Sample vial` <- '{0}' ", rawFile.SampleInformation.Vial);
                    file.WriteLine("e$info$`Sample volume` <- '{0}' ", rawFile.SampleInformation.SampleVolume);
                    file.WriteLine("e$info$`Sample injection volume` <- '{0}' ", rawFile.SampleInformation.InjectionVolume);
                    file.WriteLine("e$info$`Sample row number` <- '{0}' ", rawFile.SampleInformation.RowNumber);
                    file.WriteLine("e$info$`Sample dilution factor` <- '{0}' ", rawFile.SampleInformation.DilutionFactor);
                    file.WriteLine("e$info$`Sample barcode` <- '{0}' ", rawFile.SampleInformation.Barcode);

                    file.WriteLine("e$info$`User text 0` <- '{0}' ", rawFile.SampleInformation.UserText[0]);
                    file.WriteLine("e$info$`User text 1` <- '{0}' ", rawFile.SampleInformation.UserText[1]);
                    file.WriteLine("e$info$`User text 2` <- '{0}' ", rawFile.SampleInformation.UserText[2]);
                    file.WriteLine("e$info$`User text 3` <- '{0}' ", rawFile.SampleInformation.UserText[3]);
                    file.WriteLine("e$info$`User text 4` <- '{0}' ", rawFile.SampleInformation.UserText[4]);
	    }
	    }

            public static void GetIndex(this IRawDataPlus rawFile){
	            int firstScanNumber = rawFile.RunHeaderEx.FirstSpectrum;
	            int lastScanNumber = rawFile.RunHeaderEx.LastSpectrum;

	            int idxCharge = rawFile.GetIndexOfPattern("Charge State");
	            int idxMasterScan = rawFile.GetIndexOfPattern("Master Scan Number:");
	            int idxDependencyType = rawFile.GetIndexOfPattern("Dependency Type:");
		    int idxMonoisotopicmZ = rawFile.GetIndexOfPattern("Monoisotopic M/Z:");

	            double charge, precursorMass;
		    double monoIsotopicMz;
	            int masterScan, dependencyType;

	            Console.WriteLine("scan;scanType;StartTime;precursorMass;MSOrder;charge;masterScan;dependencyType;monoisotopicMz");

		    foreach (int scanNumber in Enumerable.Range(firstScanNumber, lastScanNumber)){
		            var scanTrailer = rawFile.GetTrailerExtraInformation(scanNumber);
		            var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
		            var scanEvent = rawFile.GetScanEventForScanNumber(scanNumber);
		            var scanFilter = rawFile.GetFilterForScanNumber(scanNumber);

		            try{
			            var reaction0 = scanEvent.GetReaction(0);
			            precursorMass =  reaction0.PrecursorMass;
		            } catch{
			            precursorMass = -1;
		            }

		            try{
			            charge = int.Parse(scanTrailer.Values.ToArray()[idxCharge]);
		            } catch {
			            charge = -1;
		            }

		            try{
			            masterScan = int.Parse(scanTrailer.Values.ToArray()[idxMasterScan]);
		            } catch {
			            masterScan= -1;
		            }

		            try{
			            dependencyType = int.Parse(scanTrailer.Values.ToArray()[idxDependencyType]);
		            } catch {
			            dependencyType = -1;
		            }

		            try{
                                monoIsotopicMz = Convert.ToDouble(scanTrailer.Values.ToArray()[idxMonoisotopicmZ]);
				//monoisotopicMz = 0.0;
			    } catch {
				monoIsotopicMz = -1.0;
			        //monoisotopicMz = -1;
			    }

		            Console.WriteLine("{0};{1};{2};{3};{4};{5};{6};{7};{8}", scanNumber,
			            scanStatistics.ScanType.ToString(),
			            scanStatistics.StartTime,
			            precursorMass,
			            scanFilter.MSOrder.ToString(),
			            charge,
				    masterScan,
				    dependencyType,
				    monoIsotopicMz);
	            }
            }

	    private static int GetIndexOfPattern(this IRawDataPlus rawFile, string pattern="Charge State"){
                    var trailerFields = rawFile.GetTrailerExtraHeaderInformation();

		    int idx = -1;
                        try
                        {
                            idx = trailerFields
                                .Select((item, index) => new
                                {
                                    name = item.Label.ToString(),
                                    Position = index
                                })
                                .First(x => x.name.Contains(pattern)).Position;
                        }
                        catch
                        {
		        }
			return (idx);
	    }

            public static void WriteSpectrumAsRcode0(this IRawDataPlus rawFile, string filename)
            {


		    int idxCharge = rawFile.GetIndexOfPattern();
             	    int firstScanNumber = rawFile.RunHeaderEx.FirstSpectrum;
            	    int lastScanNumber = rawFile.RunHeaderEx.LastSpectrum;
		        int charge = -1;
		        double pc=-1;

                using (System.IO.StreamWriter file =
                    new System.IO.StreamWriter(filename))
                {
		    foreach (int scanNumber in Enumerable.Range(firstScanNumber, lastScanNumber)){
                        var scanTrailer = rawFile.GetTrailerExtraInformation(scanNumber);
                        var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
                        var scanEvent = rawFile.GetScanEventForScanNumber(scanNumber);
			var scanFilter = rawFile.GetFilterForScanNumber(scanNumber);


		        try{
                        var reaction0 = scanEvent.GetReaction(0);
		        pc =  reaction0.PrecursorMass;
		        }
		        catch{
			        pc = -1;
		        }

		        try{
                    	    charge = int.Parse(scanTrailer.Values.ToArray()[idxCharge]);
                        }
		        catch {
			        charge=-1;
		        }

                        file.WriteLine("e$Spectrum[[{0}]] <- list(", scanNumber);
                        file.WriteLine("\tscan = {0};", scanNumber);
                        file.WriteLine("\tscanType = \"{0}\";", scanStatistics.ScanType.ToString());
                        file.WriteLine("\tStartTime = {0},", scanStatistics.StartTime);
                        file.WriteLine("\trtinseconds = {0};", Math.Round(scanStatistics.StartTime * 60 * 1000) / 1000);
                        file.WriteLine("\tprecursorMass = {0};", pc);
			file.WriteLine("\tMSOrder = '{0}';", scanFilter.MSOrder.ToString());
                        file.WriteLine("\tcharge = {0}", charge);
                                file.WriteLine(")");
		    }
	        }
	    }



            /// <summary>
	    ///    implements
 	    ///    https://github.com/fgcz/rawrr/issues/43
            /// </summary>
            /// <param name="rawFile"></param>
            /// <param name="filename"></param>
            /// <param name="L"></param>
            public static void WriteCentroidSpectrumAsRcode(this IRawDataPlus rawFile, string filename, List<int> L)
            {
                int count = 1;
                var trailerFields = rawFile.GetTrailerExtraHeaderInformation();
		            int indexCharge = rawFile.GetIndexOfPattern("Charge State");

                using (System.IO.StreamWriter file =
                    new System.IO.StreamWriter(filename))
                {
                    foreach (int scanNumber in L)
                    {
                        var scan = Scan.FromFile(rawFile, scanNumber);
                        var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
                        var centroidStream = rawFile.GetCentroidStream(scanNumber, false);
                        var scanEvent = rawFile.GetScanEventForScanNumber(scanNumber);
                        var scanTrailer = rawFile.GetTrailerExtraInformation(scanNumber);

                        file.WriteLine("e$Spectrum[[{0}]] <- list(", count++);
                        file.WriteLine("\tscan = {0},", scanNumber);
                        file.WriteLine("\tStartTime = {0},", scanStatistics.StartTime);
                        file.WriteLine("\trtinseconds = {0},", Math.Round(scanStatistics.StartTime * 60 * 1000) / 1000);
                        if (indexCharge > 0)
                                file.WriteLine("\tcharge = {0},", int.Parse(scanTrailer.Values.ToArray()[indexCharge]));
			    else
                                file.WriteLine("\tcharge = NA,");

			try{
                        	var reaction0 = scanEvent.GetReaction(0);
                        	file.WriteLine("\tpepmass = {0},", reaction0.PrecursorMass);
			}catch{
                        	file.WriteLine("\tpepmass = NA,");
			}

                        if (scanStatistics.IsCentroidScan && centroidStream.Length > 0)
                        {
                        	file.WriteLine("\tmZ = c(" + string.Join(", ", centroidStream.Masses) + "),");
                                file.WriteLine("\tintensity = c(" + string.Join(", ", centroidStream.Intensities) + ")");
			} else{
				file.WriteLine("\tmZ = NULL,\n\tintensity = NULL");
			}
                        file.WriteLine("\t)");
		    }
		}
	    }


            public static void WriteTrailerLabel(this IRawDataPlus rawFile)
	    {
		    foreach (int scanNumber in Enumerable.Range(rawFile.RunHeaderEx.FirstSpectrum, rawFile.RunHeaderEx.LastSpectrum))
                    {
                        var scanTrailer = rawFile.GetTrailerExtraInformation(scanNumber);
                    	Console.WriteLine(string.Join("\n", scanTrailer.Labels.ToArray()));
			return;
		    }
	    }

            public static void WriteTrailerValues(this IRawDataPlus rawFile, string label)
	    {
		    int idx = -1;
		    try{
		    	idx = rawFile.GetIndexOfPattern(label);
                    	//Console.WriteLine("idx = {}.", idx.ToString());
		    }catch (Exception ex){
                    	//Console.WriteLine("GetIndexOfPattern {} caused an exception {}.", label, ex.Message);
                    	Console.WriteLine("GetIndexOfPattern  caused an exception .");
                        return;
		    }

		    for  (int scanNumber = rawFile.RunHeaderEx.FirstSpectrum; scanNumber < rawFile.RunHeaderEx.LastSpectrum; scanNumber++)
                    {
                        var scanTrailer = rawFile.GetTrailerExtraInformation(scanNumber);
                    	Console.WriteLine(scanTrailer.Values.ToArray()[idx]);
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
                var trailerFields = rawFile.GetTrailerExtraHeaderInformation();
                int indexCharge = rawFile.GetIndexOfPattern("Charge State");
		int indexMonoisotopicmZ = rawFile.GetIndexOfPattern("MonoisotopicmZ");

                using (System.IO.StreamWriter file =
                    new System.IO.StreamWriter(filename))
                {

                    foreach (int scanNumber in L)
                    {
                        var basepeakMass = -1.0;
                        var basepeakIntensity = -1.0;

                        var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
                        var centroidStream = rawFile.GetCentroidStream(scanNumber, false);
                        var scanTrailer = rawFile.GetTrailerExtraInformation(scanNumber);
                        var scanEvent = rawFile.GetScanEventForScanNumber(scanNumber);

                        var scan = Scan.FromFile(rawFile, scanNumber);

                        file.WriteLine("e$Spectrum[[{0}]] <- list(", count++);
                        file.WriteLine("\tscan = {0},", scanNumber);

			try
			{
                                basepeakMass =  (scanStatistics.BasePeakMass);
                                basepeakIntensity =  Math.Round(scanStatistics.BasePeakIntensity);
                                file.WriteLine("\tbasePeak = c({0}, {1}),", basepeakMass, basepeakIntensity);
			}
			catch
			{
                                file.WriteLine("\tbasePeak = c(NA, NA),");
			}
                                file.WriteLine("\tTIC = {0},", scanStatistics.TIC.ToString());
                                file.WriteLine("\tmassRange = c({0}, {1}),", scanStatistics.LowMass.ToString(), scanStatistics.HighMass.ToString());
                                file.WriteLine("\tscanType = \"{0}\",", scanStatistics.ScanType.ToString());
                                file.WriteLine("\tStartTime = {0},", scanStatistics.StartTime);
                                file.WriteLine("\trtinseconds = {0},", Math.Round(scanStatistics.StartTime * 60 * 1000) / 1000);
			try
			{
                            var reaction0 = scanEvent.GetReaction(0);
                                file.WriteLine("\tpepmass = {0},", reaction0.PrecursorMass);
			}
			catch
			{
                                file.WriteLine("\tpepmass = NA,");
			}

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

			    if (indexMonoisotopicmZ > 0)
                                file.WriteLine("\tmonoisotopicMz = {0},", Convert.ToDouble(scanTrailer.Values.ToArray()[indexMonoisotopicmZ]));
			    else
                                file.WriteLine("\tmonoisotopicMz = NA,");


			    if (indexCharge > 0)
                                file.WriteLine("\tcharge = {0},", int.Parse(scanTrailer.Values.ToArray()[indexCharge]));
			    else
                                file.WriteLine("\tcharge = NA,");

                                file.WriteLine("\tmZ = c(" + string.Join(", ", centroidStream.Masses) + "),");
                                file.WriteLine("\tintensity = c(" + string.Join(", ", centroidStream.Intensities) + "),");
                                file.WriteLine("\tnoises = c(" + string.Join(", ", centroidStream.Noises) + "),");
				file.WriteLine("\tresolutions = c(" + string.Join(", ", centroidStream.Resolutions.ToArray()) + "),");
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

				    // https://github.com/compomics/ThermoRawFileParser/blob/c293d4aa1b04bfd62124ff42c512572427a4316a/Writer/MzMlSpectrumWriter.cs#L1664
				    file.WriteLine("\tcentroid.PreferredNoises = c({0}),", string.Join(", ", scan.PreferredNoises.ToArray()));
				    file.WriteLine("\tcentroid.PreferredMasses = c({0}),", string.Join(", ", scan.PreferredMasses.ToArray()));
				    //Console.WriteLine("\tcentroid.PreferredBaselines = c({0}),", string.Join(", ", scan.PreferredBaselines.ToArray()));
			        }

                                file.WriteLine("\ttitle = \"File: {0}; SpectrumID: {1}; scans: {2}\",",
                                    Path.GetFileName(rawFile.FileName),
                                    null,
                                    scanNumber);

			    if (indexCharge > 0)
                                file.WriteLine("\tcharge = {0},", int.Parse(scanTrailer.Values.ToArray()[indexCharge]));
			    else
                                file.WriteLine("\tcharge = NA,");

			    if (indexMonoisotopicmZ > 0)
                                file.WriteLine("\tmonoisotopicMz = {0},", Convert.ToDouble(scanTrailer.Values.ToArray()[indexMonoisotopicmZ]));
			    else
                                file.WriteLine("\tmonoisotopicMz = NA,");

                                file.WriteLine("\tmZ = c(" + string.Join(",", scan.SegmentedScan.Positions) + "),");
                                file.WriteLine("\tintensity = c(" + string.Join(",", scan.SegmentedScan.Intensities) + "),");
                               // file.WriteLine("\tnoises = c(" + string.Join(",", scan.SegmentedScan.Noises) + "),");
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
		        const string rawR_version = "1.3.1";
		        string filename = string.Empty;
		        string mode = string.Empty;
		        string filterString = string.Empty;
		        Hashtable hashtable = new Hashtable()
		        {
			        {"filter", "List all scan ids pass the filter string (option 2)."},
			        {"getFilters", "List all scan filters of a given raw file."},
			        {"isValidFilter", "Checks whether the provided argument string (option 2) is a valid filter."},
			        {"headerR", "Writes the raw file's meta data as R code to a file."},
			        {"chromatogram", "Extracts base peak and total ion count chromatograms into a file."},
			        {
				        "xic",
				        "Extracts filtered (option 2) ion chromatograms within a given mass and mass tolerance [in ppm] (option 3) xic of a given raw file as R code into a file."
			        },
			        {"scans", "Extracts scans (spectra) of a given ID as Rcode."},
			        {"cscans", "Extracts 'barbone' scans (spectra), including only mZ, intensity , precursorMass, rtinsecodonds and charge state, of a given ID as Rcode."},
			        {"index", "Prints index as csv of all scans."},
			        {"trailer", "Prints all trailer labels."}
		        };
		        var helpOptions = new List<string>() {"help", "--help", "-h", "h", "/h"};

		        if (args.Length >= 2){
			        filename = args[0];
			        mode = args[1];
			        if (!hashtable.Contains(mode))
			        {
				        Console.WriteLine("\nOption '{0}' is not defined. Please use one of the following options as argument:", mode);
				        foreach (var k in hashtable.Keys)
					        Console.WriteLine("  {0,-15}   {1}", k.ToString(), hashtable[k].ToString());
				        Console.WriteLine();

				        Environment.Exit(1);
			        }}
		        else
		        {
			        if (args.Length == 0)
			        {
						Console.WriteLine("No RAW file specified!");
			        return;
				        Console.WriteLine("run 'rawrr.exe help'.");
				        Environment.Exit(1);
			        }
			        else if (args[0] == "version")
			        {
				        Console.WriteLine(rawR_version);
				        Environment.Exit(0);
			        }
			        else if (helpOptions.Contains(args[0]))
			        {
				        Console.WriteLine("\nUsage:\n");
				        Console.WriteLine("  rawrr.exe <raw file> <option>\n");
				        Console.WriteLine("  rawrr.exe <raw file> <option> <input file> <output file>\n");
				        Console.WriteLine(
					        "  rawrr.exe <raw file> <option 1> <option 2> <option 3> <input file> <output file>\n");
				        Console.WriteLine("\nOptions:\n");
				        foreach (var k in hashtable.Keys)
				        {
					        Console.WriteLine("  {0,-15}   {1}", k.ToString(), hashtable[k].ToString());
				        }
				        Console.WriteLine("\nReport bugs at <https://github.com/fgcz/rawrr/issues>.\n");
				        Environment.Exit(0);
			        }
			        else
			        {
				        Console.WriteLine("run 'rawrr.exe help'.");
				        Environment.Exit(1);
			        }
		        }

	        if (string.IsNullOrEmpty(filename))
		        {
			        Console.WriteLine("No RAW file specified!");
			        return;
		        }
				//Environment.Exit(0);

		        // Get the memory used at the beginning of processing
		        Process processBefore = Process.GetCurrentProcess();
		        long memoryBefore = processBefore.PrivateMemorySize64 / 1024;
                try
                {
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

                    if (mode == "headerR"){
	                    var outputFilename = args[3];
	                    rawFile.PrintHeaderAsRcode(outputFilename);
	                    return;
                    }

                    // Get the number of filters present in the RAW file
                    int numberFilters = rawFile.GetFilters().Count;

		    if (mode == "trailer" && args.Length == 2){
		    	rawFile.WriteTrailerLabel();
			return;
		    } else if (mode == "trailer" && args.Length == 3) {
			//Console.WriteLine(args[2]);
		    	rawFile.WriteTrailerValues(args[2]);
			return;
		    }

                    if (mode == "filter")
                    {
	                    filterString = args[2].ToString();
                            int precision = int.Parse(args[3]);
	                    var outputFilename = args[4];

	                    if(!IsValidFilter(rawFile, filterString))
							Environment.Exit(1);

	                    using (System.IO.StreamWriter file =
		                    new System.IO.StreamWriter(outputFilename))
	                    {
		                    foreach (var ss in rawFile
			                    .GetFilteredScanEnumerator(rawFile.GetFilterFromString(filterString, precision)).ToArray())
		                    {
			                    file.WriteLine(ss);
		                    }
	                    }

	                    Environment.Exit(0);
                    }

                    if (mode == "isValidFilter")
                    {
	                    Console.WriteLine(IsValidFilter(rawFile, args[2].ToString()).ToString());
                        Environment.Exit(0);
                    }

                    if (mode == "getFilters")
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
			string filter = "ms";
			string outputcsv = "chromatogram.csv";
			try {
			    filter = args[2];
			}
			catch{
			}
			try {
			    outputcsv = args[3];
			}
			catch{
			}
                        GetChromatogram(rawFile, firstScanNumber, lastScanNumber, outputcsv, filter);
                        Environment.Exit(0);
                    }


                    if (mode == "index"){
                    	    rawFile.GetIndex();
			    return;
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
                    if (mode == "cscans")
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
                    	    rawFile.WriteCentroidSpectrumAsRcode(args[3], scans);

                        return;

                    }

                    if (mode == "xic")
                    {
                          //  Console.WriteLine("xic");
                        try
                        {
                            double ppmError = Convert.ToDouble(args[2]);
                            // Console.WriteLine(ppmError);
			    string filter = "ms";
			    try {
				    filter = args[3];
			    }
			    catch{
			    }
                            //Console.WriteLine(filter);
                            var inputFilename = args[4];
                            var outputFilename = args[5];

                            List<double> massList = new List<double>();
                            if (File.Exists(inputFilename))
                            {
                                foreach (var line in File.ReadAllLines(inputFilename))
                                {
                               	    //Console.WriteLine(Convert.ToDouble(line));
                                    massList.Add(Convert.ToDouble(line));
                                }

                                ExtractIonChromatogramAsRcode(rawFile, -1, -1, massList, ppmError, outputFilename, filter);
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
            /// <param name="filter">
            /// The chromatic filter flag.
            /// </param>
            private static void GetChromatogram(IRawDataPlus rawFile, int startScan, int endScan,  string filename, string filter = "ms")
            {
	            if (IsValidFilter(rawFile, filter) == false){
                    Console.WriteLine("# '{0}' is not a valid filter string.", filter);
		          return;
	            }

using (System.IO.StreamWriter file =
		                         new System.IO.StreamWriter(filename))
		                     {
		// TODO(tk@fgcz.ethz.ch): check mass interval for chromatograms and its dep for diff MS detector types
		// TODO(cp@fgcz.ethz.ch): return mass intervals to the R enviroment
                // Define the settings for getting the Base Peak chromatogram
                ChromatogramTraceSettings settingsTIC = new ChromatogramTraceSettings(TraceType.TIC){Filter=filter};
                ChromatogramTraceSettings settingsBasePeak = new ChromatogramTraceSettings(TraceType.BasePeak){
			Filter=filter,
                        MassRanges = new[] {ThermoFisher.CommonCore.Data.Business.Range.Create(100, 1805)}
			};
                ChromatogramTraceSettings settingsMassRange = new ChromatogramTraceSettings(TraceType.MassRange){
        		Filter=filter,
                        MassRanges = new[] {ThermoFisher.CommonCore.Data.Business.Range.Create(50, 2000000)}
			};

                // Get the chromatogram from the RAW file.
                var dataTIC = rawFile.GetChromatogramData(new IChromatogramSettings[] {settingsTIC}, startScan, endScan);
                var dataMassRange = rawFile.GetChromatogramData(new IChromatogramSettings[] {settingsMassRange}, startScan, endScan);
                var dataBasePeak = rawFile.GetChromatogramData(new IChromatogramSettings[] {settingsBasePeak}, startScan, endScan);

                // Split the data into the chromatograms
                var traceTIC = ChromatogramSignal.FromChromatogramData(dataTIC);
                var traceMassRange = ChromatogramSignal.FromChromatogramData(dataMassRange);
                var traceBasePeak = ChromatogramSignal.FromChromatogramData(dataBasePeak);

                
                if (traceBasePeak[0].Length > 0)
                {
                   
                    // Print the chromatogram data (time, intensity values)
                    file.WriteLine("# TIC chromatogram ({0} points)", traceTIC[0].Length);
                    file.WriteLine("# Base Peak chromatogram ({0} points)", traceBasePeak[0].Length);
                    file.WriteLine("# MassRange chromatogram ({0} points)", traceMassRange[0].Length);

                    file.WriteLine("rt;intensity.BasePeak;intensity.TIC;intensity.MassRange");
                   
                        for (int i = 0; i < traceBasePeak[0].Length; i++)
                        {
                            file.WriteLine("{1:F3};{2:F0};{3:F0};{4:F0}", i, traceBasePeak[0].Times[i], traceBasePeak[0].Intensities[i], traceTIC[0].Intensities[i], traceMassRange[0].Intensities[i]);
                        }
                    
                }
                file.WriteLine();
            }
              
            }

	    private static bool IsValidFilter(IRawDataPlus rawFile, string filter)
	    {
		    if (rawFile.GetFilterFromString(filter) == null) {
			    return false;
		    }
		    return true;
	    }

            private static void ExtractIonChromatogramAsRcode(IRawDataPlus rawFile, int startScan, int endScan, List<double> massList,
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
                         //   if (trace[i].Intensities[j] > 0)
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
