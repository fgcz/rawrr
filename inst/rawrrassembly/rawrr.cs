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
      2025-05-27 https://github.com/fgcz/rawrr/issues/84
      2026-11-28 added devices and statusLog options (e.g., LC pump pressure profiles)
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

	            double charge, precursorMass;
		    double monoIsotopicMz;
	            int masterScan, dependencyType;

	            Console.WriteLine("scan;scanType;StartTime;precursorMass;MSOrder;charge;masterScan;dependencyType;monoisotopicMz");

                    Dictionary<string, string> ScanTrailerDict;

		    foreach (int scanNumber in Enumerable.Range(firstScanNumber, lastScanNumber)){
		            var scanTrailer = rawFile.GetTrailerExtraInformation(scanNumber);
		            var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
		            var scanEvent = rawFile.GetScanEventForScanNumber(scanNumber);
		            var scanFilter = rawFile.GetFilterForScanNumber(scanNumber);

                	// TODO(cpanse): implement a public class ScanTrailer
                	ScanTrailerDict = new Dictionary<string, string>();
			foreach (var (key, value) in Enumerable.Range(0, scanTrailer.Length).Select(i => (scanTrailer.Labels[i], scanTrailer.Values[i])))
			{ ScanTrailerDict[key] = value.Trim(); }

		            try{
			            var reaction0 = scanEvent.GetReaction(0);
			            precursorMass =  reaction0.PrecursorMass;
		            } catch{
			            precursorMass = -1;
		            }

		            try{
			            charge = int.Parse(ScanTrailerDict["Charge State:"]);
		            } catch {
			            charge = -1;
		            }

		            try{
			            masterScan = int.Parse(ScanTrailerDict["Master Scan Number:"]);
		            } catch {
			            masterScan= -1;
		            }

		            try{
			            dependencyType = int.Parse(ScanTrailerDict["Dependency Type:"]);
		            } catch {
			            dependencyType = -1;
		            }

		            try{
                                monoIsotopicMz = Convert.ToDouble(ScanTrailerDict["Monoisotopic M/Z:"]);
			    } catch {
				monoIsotopicMz = -1.0;
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

            public static void WriteSpectrumAsRcode0(this IRawDataPlus rawFile, string filename)
            {
             	    int firstScanNumber = rawFile.RunHeaderEx.FirstSpectrum;
            	    int lastScanNumber = rawFile.RunHeaderEx.LastSpectrum;
		    int charge = -1;
		    double pc=-1;
                    Dictionary<string, string> ScanTrailerDict;

                using (System.IO.StreamWriter file =
                    new System.IO.StreamWriter(filename))
                {
		    foreach (int scanNumber in Enumerable.Range(firstScanNumber, lastScanNumber)){
                        var scanTrailer = rawFile.GetTrailerExtraInformation(scanNumber);
                        var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
                        var scanEvent = rawFile.GetScanEventForScanNumber(scanNumber);
			var scanFilter = rawFile.GetFilterForScanNumber(scanNumber);

		        ScanTrailerDict = new Dictionary<string, string>();
			foreach (var (key, value) in Enumerable.Range(0, scanTrailer.Length).Select(i => (scanTrailer.Labels[i], scanTrailer.Values[i])))
			{ ScanTrailerDict[key] = value.Trim(); }


		        try{
                          var reaction0 = scanEvent.GetReaction(0);
		          pc =  reaction0.PrecursorMass;
		        }
		        catch{
			  pc = -1;
		        }

		        try{
                    	    charge = int.Parse(ScanTrailerDict["Charge State:"]);
                        }
		        catch {
			        charge = -1;
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
		int charge = -1;
                Dictionary<string, string> ScanTrailerDict;
                var trailerFields = rawFile.GetTrailerExtraHeaderInformation();

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

		        ScanTrailerDict = new Dictionary<string, string>();
			foreach (var (key, value) in Enumerable.Range(0, scanTrailer.Length).Select(i => (scanTrailer.Labels[i], scanTrailer.Values[i])))
			{ ScanTrailerDict[key] = value.Trim(); }

		        try{
                    	    charge = int.Parse(ScanTrailerDict["Charge State:"]);
                        }
		        catch {
			        charge = -1;
		        }


                        file.WriteLine("e$Spectrum[[{0}]] <- list(", count++);
                        file.WriteLine("\tscan = {0},", scanNumber);
                        file.WriteLine("\tStartTime = {0},", scanStatistics.StartTime);
                        file.WriteLine("\trtinseconds = {0},", Math.Round(scanStatistics.StartTime * 60 * 1000) / 1000);
                        if (charge > 0)
                                file.WriteLine("\tcharge = {0},", charge);
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
                                file.WriteLine("\tcentroidScan.mZ = c(" + string.Join(", ", scan.CentroidScan.Masses.ToArray()) + "),");
                                file.WriteLine("\tcentroidScan.intensity = c(" + string.Join(", ", scan.CentroidScan.Intensities.ToArray()) + "),");
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

            /// <summary>
            /// Lists all instruments (devices / data streams) of the raw file as csv.
            /// The device index is 1-based per device type, as required by SelectInstrument.
            /// </summary>
            public static void WriteDevices(this IRawDataPlus rawFile)
            {
                var counter = new Dictionary<Device, int>();
                Console.WriteLine("deviceType;deviceIndex;name;model;serialNumber");
                for (int i = 0; i < rawFile.InstrumentCount; i++)
                {
                    Device type = rawFile.GetInstrumentType(i);
                    counter[type] = counter.ContainsKey(type) ? counter[type] + 1 : 1;
                    string name = "", model = "", serial = "";
                    try
                    {
                        rawFile.SelectInstrument(type, counter[type]);
                        var data = rawFile.GetInstrumentData();
                        name = data.Name ?? "";
                        model = data.Model ?? "";
                        serial = data.SerialNumber ?? "";
                    }
                    catch { }
                    Console.WriteLine("{0};{1};{2};{3};{4}", type, counter[type],
                        name.Replace(";", ","), model.Replace(";", ","), serial.Replace(";", ","));
                }
            }

            /// <summary>
            /// Prints position and label of all plottable (numeric) status log items of a device.
            /// </summary>
            public static void WriteStatusLogLabels(this IRawDataPlus rawFile, Device type, int index)
            {
                rawFile.SelectInstrument(type, index);
                Console.WriteLine("position;label");
                foreach (var kv in rawFile.StatusLogPlottableData)
                {
                    Console.WriteLine("{0};{1}", kv.Value, kv.Key.Replace(";", ","));
                }
            }

            /// <summary>
            /// Prints the trend (retention time [min]; value) of a status log item at a given position.
            /// Uses the unformatted values: the formatted ones (GetStatusLogAtPosition) are rounded to
            /// the display precision of the device, e.g., an ion gauge pressure of 1.36e-05 Torr is
            /// formatted as '0.0'.
            /// </summary>
            public static void WriteStatusLogValues(this IRawDataPlus rawFile, Device type, int index, int position)
            {
                var invariant = System.Globalization.CultureInfo.InvariantCulture;
                rawFile.SelectInstrument(type, index);
                // all time points come from the (formatted) single value log;
                // GetStatusLogEntriesCount() can be smaller than its length
                // (e.g., Orbitrap Fusion Lumos MS: 1052 vs. 1168). Entries align
                // 1:1 by index; values are replaced by the unformatted ones where
                // available, otherwise the formatted value is kept.
                var log = rawFile.GetStatusLogAtPosition(position);
                Console.WriteLine("times;values");
                for (int i = 0; i < log.Times.Length; i++)
                {
                    string value = log.Values[i].Trim();
                    try
                    {
                        var entry = rawFile.GetStatusLogValues(i, false);
                        if (entry != null && position < entry.Values.Length
                            && Math.Abs(entry.RetentionTime - log.Times[i]) < 1e-6)
                            value = entry.Values[position].Trim();
                    }
                    catch { }
                    // print numbers culture-invariant with full precision;
                    // values stored as 32-bit float are printed as float (298.3, not 298.29998779296875)
                    if (double.TryParse(value, System.Globalization.NumberStyles.Float,
                            System.Globalization.CultureInfo.CurrentCulture, out double d))
                        value = ((double)(float)d == d) ? ((float)d).ToString("R", invariant) : d.ToString("R", invariant);
                    Console.WriteLine("{0};{1}", log.Times[i].ToString(invariant), value);
                }
            }

            public static void WriteTrailerValues(this IRawDataPlus rawFile, string label)
	    {
                Dictionary<string, string> ScanTrailerDict;
		    foreach (int scanNumber in Enumerable.Range(rawFile.RunHeaderEx.FirstSpectrum, rawFile.RunHeaderEx.LastSpectrum))
                    {
                        var scanTrailer = rawFile.GetTrailerExtraInformation(scanNumber);

                	// TODO(cpanse): implement a public class ScanTrailer 
                	ScanTrailerDict = new Dictionary<string, string>();
			foreach (var (key, value) in Enumerable.Range(0, scanTrailer.Length).Select(i => (scanTrailer.Labels[i], scanTrailer.Values[i])))
			{ ScanTrailerDict[key] = value.Trim(); }

			if (ScanTrailerDict.ContainsKey(label)){
                    		Console.WriteLine(ScanTrailerDict[label]);
			}else{
                    		Console.WriteLine("NA");
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
                int charge = -1;
                double monoIsotopicMz = -1;
                var trailerFields = rawFile.GetTrailerExtraHeaderInformation();
                Dictionary<string, string> ScanTrailerDict;

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
 

		        ScanTrailerDict = new Dictionary<string, string>();
			foreach (var (key, value) in Enumerable.Range(0, scanTrailer.Length).Select(i => (scanTrailer.Labels[i], scanTrailer.Values[i])))
			{ ScanTrailerDict[key] = value.Trim(); }

		        try{
			        charge = int.Parse(ScanTrailerDict["Charge State:"]);
		        } catch {
			        charge = -1;
		        }

		        try{
                            monoIsotopicMz = Convert.ToDouble(ScanTrailerDict["Monoisotopic M/Z:"]);
			} catch {
			    monoIsotopicMz = -1.0;
			}

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

			    if (monoIsotopicMz > 0)
                                file.WriteLine("\tmonoisotopicMz = {0},", monoIsotopicMz);
			    else
                                file.WriteLine("\tmonoisotopicMz = NA,");


			    if (charge > 0){
                                file.WriteLine("\tcharge = {0},", charge);
			    }
                            else{
                                file.WriteLine("\tcharge = NA,");
                            }
	

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


			    if (charge > 0)
                                file.WriteLine("\tcharge = {0},", charge);
			    else
                                file.WriteLine("\tcharge = NA,");

			    if (monoIsotopicMz > 0)
                                file.WriteLine("\tmonoisotopicMz = {0},", monoIsotopicMz);
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
	        private const string rawrr_version = "1.23.2";

	        private static readonly List<string> HelpOptions = new List<string>() {"help", "--help", "-h", "h", "/h"};
	        private static readonly List<string> VersionOptions = new List<string>() {"version", "--version", "-v", "-V", "/v"};

	        /// <summary>
	        /// A rawrr mode (command line option).
	        /// args[0] is the raw file, args[1] the mode, args[2..] the mode specific arguments.
	        /// The table below is the single source of the CLI documentation:
	        /// 'rawrr help', 'rawrr help &lt;mode&gt;', and 'rawrr help --markdown'
	        /// (used by inst/rawrrassembly/README.md) are generated from it.
	        /// </summary>
	        /// <param name="Name">mode name, the second command line argument</param>
	        /// <param name="Arguments">synopsis of the arguments after the mode; [] marks optional ones</param>
	        /// <param name="Help">one line description</param>
	        /// <param name="Output">what is written where</param>
	        /// <param name="MinArgs">minimal number of arguments after the mode</param>
	        /// <param name="Run">handler; returns the process exit code</param>
	        private sealed record Command(string Name, string Arguments, string Help, string Output,
		        int MinArgs, Func<IRawDataPlus, string[], int> Run);

	        /// <summary>
	        /// All modes; the order defines the order of the help output.
	        /// To add a mode, add an entry here and a '## `mode`' section to inst/rawrrassembly/README.md
	        /// (enforced by tests/testthat/test-assembly-doc.R).
	        /// </summary>
	        private static readonly List<Command> Commands = new List<Command>()
	        {
		        new Command("index", "",
			        "Prints the index of all scans.",
			        "csv (';' separated) on stdout", 0,
			        (raw, a) => { raw.GetIndex(); return 0; }),
		        new Command("headerR", "<input file> <output file>",
			        "Writes the raw file's meta data as R code to a file; the input file is ignored.",
			        "R code in the output file", 2,
			        (raw, a) => { raw.PrintHeaderAsRcode(a[3]); return 0; }),
		        new Command("filter", "<filter> <precision> <output file>",
			        "Lists the scan numbers passing the filter string with the given mass precision.",
			        "scan numbers in the output file, one per line", 3, Filter),
		        new Command("getFilters", "",
			        "Lists all scan filters of the raw file.",
			        "filter strings on stdout, one per line", 0, GetFilters),
		        new Command("isValidFilter", "<filter>",
			        "Checks whether the filter string is valid.",
			        "True or False on stdout", 1,
			        (raw, a) => { Console.WriteLine(IsValidFilter(raw, a[2]).ToString()); return 0; }),
		        new Command("scans", "<scan file> <output file>",
			        "Extracts the scans (spectra) listed in the scan file (one scan number per line; empty file = all scans).",
			        "R code in the output file", 2,
			        (raw, a) => WriteScans(raw, a, barebone: false)),
		        new Command("barebone", "<scan file> <output file>",
			        "Extracts 'barebone' scans (spectra), including only mZ, intensity, precursorMass, rtinseconds, and charge state.",
			        "R code in the output file", 2,
			        (raw, a) => WriteScans(raw, a, barebone: true)),
		        new Command("chromatogram", "[<filter> [<output csv>]]",
			        "Extracts base peak, total ion count, and mass range chromatograms (default filter: ms).",
			        "csv (';' separated) in the output csv (default: chromatogram.csv)", 0, Chromatogram),
		        new Command("xic", "<ppm> <filter> <mass file> <output file>",
			        "Extracts filtered ion chromatograms of the masses in the mass file (one per line) within a mass tolerance in ppm.",
			        "R code in the output file", 4, Xic),
		        new Command("trailer", "[<label>]",
			        "Prints all trailer labels, or the values of the given trailer label for all scans.",
			        "labels or values on stdout, one per line", 0,
			        (raw, a) => { if (a.Length > 2) raw.WriteTrailerValues(a[2]); else raw.WriteTrailerLabel(); return 0; }),
		        new Command("devices", "",
			        "Lists all instruments (devices) stored in the raw file, e.g., MS, LC pump, autosampler.",
			        "csv (';' separated) on stdout", 0,
			        (raw, a) => { raw.WriteDevices(); return 0; }),
		        new Command("statusLog", "<device type> [<device index> [<position>]]",
			        "Prints the plottable status log labels of a device, or the trend of the label at the given position (e.g., LC pump pressure).",
			        "csv (';' separated) on stdout", 1, StatusLog),
	        };

	        private static int Main(string[] args)
	        {
		        if (args.Length == 0)
		        {
			        // health check used by rawrr:::.isAssemblyWorking(); keep text and exit code 0
			        Console.WriteLine("No RAW file specified!");
			        return 0;
		        }

		        // help [<mode> | --markdown]
		        if (HelpOptions.Contains(args[0]))
		        {
			        if (args.Length == 1)
			        {
				        PrintHelp();
				        return 0;
			        }
			        if (args[1] == "--markdown")
			        {
				        PrintMarkdown();
				        return 0;
			        }
			        var c = Commands.FirstOrDefault(x => x.Name == args[1]);
			        if (c == null)
			        {
				        Console.Error.WriteLine("Option '{0}' is not defined. Run 'rawrr.exe help'.", args[1]);
				        return 1;
			        }
			        PrintCommandHelp(c);
			        return 0;
		        }

		        if (args.Length == 1)
		        {
			        if (VersionOptions.Contains(args[0]))
			        {
				        Console.WriteLine(rawrr_version);
				        return 0;
			        }
			        Console.Error.WriteLine("run 'rawrr.exe help'.");
			        return 1;
		        }

		        var command = Commands.FirstOrDefault(c => c.Name == args[1]);
		        if (command == null)
		        {
			        // stdout: the R package checks for 'is not defined' to detect outdated assemblies
			        Console.WriteLine("\nOption '{0}' is not defined. Please use one of the following options as argument:", args[1]);
			        PrintOptions();
			        Console.WriteLine();
			        return 1;
		        }

		        if (args.Length - 2 < command.MinArgs)
		        {
			        Console.Error.WriteLine("Option '{0}' requires at least {1} argument(s) after the option name.",
				        command.Name, command.MinArgs);
			        Console.Error.WriteLine("Usage: {0}", Synopsis(command));
			        return 1;
		        }

		        string filename = args[0];
		        try
		        {
			        // Create the IRawDataPlus object for accessing the RAW file
			        using (var rawFile = RawFileReaderAdapter.FileFactory(filename))
			        {
				        if (!rawFile.IsOpen || rawFile.IsError)
				        {
					        Console.Error.WriteLine("Unable to access the RAW file using the RawFileReader class! Error opening ({0}) - {1}",
						        rawFile.FileError?.ErrorMessage, filename);
					        return 1;
				        }

				        if (rawFile.InAcquisition)
				        {
					        Console.Error.WriteLine("RAW file still being acquired - " + filename);
					        return 1;
				        }

				        rawFile.SelectInstrument(Device.MS, 1);
				        return command.Run(rawFile, args);
			        }
		        }
		        catch (Exception ex)
		        {
			        Console.Error.WriteLine("Error accessing RAWFileReader library! - {0} failed: {1}", command.Name, ex.Message);
			        return 1;
		        }
	        }

	        private static void PrintOptions()
	        {
		        foreach (var c in Commands)
			        Console.WriteLine("  {0,-15}   {1}", c.Name, c.Help);
	        }

	        private static string Synopsis(Command c)
	        {
		        return ("rawrr.exe <raw file> " + c.Name + " " + c.Arguments).TrimEnd();
	        }

	        private static void PrintHelp()
	        {
		        Console.WriteLine("\nUsage:\n");
		        Console.WriteLine("  rawrr.exe <raw file> <option> [<arguments>]");
		        Console.WriteLine("  rawrr.exe help [<option> | --markdown]");
		        Console.WriteLine("  rawrr.exe version");
		        Console.WriteLine("\nOptions:\n");
		        PrintOptions();
		        Console.WriteLine("\nRun 'rawrr.exe help <option>' for the arguments and output of an option.");
		        Console.WriteLine("\nReport bugs at <https://github.com/fgcz/rawrr/issues>.\n");
	        }

	        private static void PrintCommandHelp(Command c)
	        {
		        Console.WriteLine("Usage: {0}\n", Synopsis(c));
		        Console.WriteLine("  {0}\n", c.Help);
		        Console.WriteLine("Output: {0}", c.Output);
	        }

	        /// <summary>
	        /// Prints the option table as markdown; embedded in inst/rawrrassembly/README.md.
	        /// </summary>
	        private static void PrintMarkdown()
	        {
		        Console.WriteLine("| Option | Arguments | Description | Output |");
		        Console.WriteLine("|--------|-----------|-------------|--------|");
		        foreach (var c in Commands)
		        {
			        Console.WriteLine("| `{0}` | {1} | {2} | {3} |",
				        c.Name,
				        string.IsNullOrEmpty(c.Arguments) ? "" : "`" + c.Arguments + "`",
				        c.Help.Replace("|", "\\|"),
				        c.Output.Replace("|", "\\|"));
		        }
	        }

	        // filter <filter string> <precision> <output file>
	        private static int Filter(IRawDataPlus rawFile, string[] args)
	        {
		        string filterString = args[2];
		        int precision = int.Parse(args[3]);
		        var outputFilename = args[4];

		        if (!IsValidFilter(rawFile, filterString))
		        {
			        Console.Error.WriteLine("'{0}' is not a valid filter string.", filterString);
			        return 1;
		        }

		        using (var file = new StreamWriter(outputFilename))
		        {
			        foreach (var scan in rawFile.GetFilteredScanEnumerator(rawFile.GetFilterFromString(filterString, precision)))
				        file.WriteLine(scan);
		        }
		        return 0;
	        }

	        private static int GetFilters(IRawDataPlus rawFile, string[] args)
	        {
		        foreach (var filter in rawFile.GetFilters())
			        Console.WriteLine(filter.ToString());
		        return 0;
	        }

	        // scans|barebone <scan file> <output file>; an empty scan file selects all scans
	        private static int WriteScans(IRawDataPlus rawFile, string[] args, bool barebone)
	        {
		        // accepts only integers greater than 0
		        var scans = File.ReadAllLines(args[2])
			        .Select(line => Int32.TryParse(line, out int scanNumber) ? scanNumber : 0)
			        .Where(scanNumber => scanNumber > 0)
			        .ToList();
		        var outputFilename = args[3];

		        if (scans.Count == 0)
			        rawFile.WriteSpectrumAsRcode0(outputFilename);
		        else if (barebone)
			        rawFile.WriteCentroidSpectrumAsRcode(outputFilename, scans);
		        else
			        rawFile.WriteSpectrumAsRcode(outputFilename, scans);
		        return 0;
	        }

	        // chromatogram [<filter> [<output csv>]]
	        private static int Chromatogram(IRawDataPlus rawFile, string[] args)
	        {
		        string filter = args.Length > 2 ? args[2] : "ms";
		        string outputcsv = args.Length > 3 ? args[3] : "chromatogram.csv";
		        return GetChromatogram(rawFile, rawFile.RunHeaderEx.FirstSpectrum, rawFile.RunHeaderEx.LastSpectrum,
			        outputcsv, filter) ? 0 : 1;
	        }

	        // xic <ppm> <filter> <mass file> <output file>
	        private static int Xic(IRawDataPlus rawFile, string[] args)
	        {
		        double ppmError = Convert.ToDouble(args[2]);
		        string filter = args[3];
		        var inputFilename = args[4];
		        var outputFilename = args[5];

		        if (!File.Exists(inputFilename))
		        {
			        Console.Error.WriteLine("xic: mass file '{0}' does not exist.", inputFilename);
			        return 1;
		        }

		        List<double> massList = File.ReadAllLines(inputFilename).Select(line => Convert.ToDouble(line)).ToList();
		        ExtractIonChromatogramAsRcode(rawFile, -1, -1, massList, ppmError, outputFilename, filter);
		        return 0;
	        }

	        // statusLog <device type> [<device index> [<position>]]
	        private static int StatusLog(IRawDataPlus rawFile, string[] args)
	        {
		        Device device;
		        int deviceIndex = 1;
		        if (!Enum.TryParse<Device>(args[2], true, out device))
		        {
			        Console.Error.WriteLine("statusLog: invalid or missing device type. Use one of: {0}",
				        string.Join(", ", Enum.GetNames(typeof(Device))));
			        return 1;
		        }
		        if (args.Length >= 4 && !Int32.TryParse(args[3], out deviceIndex))
		        {
			        Console.Error.WriteLine("statusLog: invalid device index '{0}'.", args[3]);
			        return 1;
		        }
		        try
		        {
			        if (args.Length >= 5)
				        rawFile.WriteStatusLogValues(device, deviceIndex, Int32.Parse(args[4]));
			        else
				        rawFile.WriteStatusLogLabels(device, deviceIndex);
		        }
		        catch (Exception ex)
		        {
			        Console.Error.WriteLine("statusLog: failed to read status log of {0} {1}: {2}",
				        device, deviceIndex, ex.Message);
			        return 1;
		        }
		        return 0;
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
            /// <returns>false if the filter is not valid</returns>
            private static bool GetChromatogram(IRawDataPlus rawFile, int startScan, int endScan,  string filename, string filter = "ms")
            {
	            if (IsValidFilter(rawFile, filter) == false){
                    Console.Error.WriteLine("# '{0}' is not a valid filter string.", filter);
		          return false;
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
                return true;
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
