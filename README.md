# __dsssdCal__

#### Author
- D. Connolly

## __Description__

Simple program to calibrate DRAGON's DSSSD using triple alpha source data. Takes as input name of a rootfile containing triple alpha source data. To calibrate the DSSSD, and write the calibration variables to the MIDAS ODB, do the following:

1. Run the following command from a ssh session on smaug:
   `dsssdCal --reset`
2. Take triple alpha source data. When the run is stopped, `end_run.sh` runs automatically, producing a rootfile `$DH/rootfiles/runxxxx.root`).
3. Run the following command from a ssh session on smaug:
   `dsssdCal runxxxx.root --odb` (`--grid` if Tengblad DSSSD in use)
4. It may be necessary to adjust the `threshold` and `sigma` parameters used by `TSpectrum::Search`:
   `dsssdCal runxxxx.root -s <sigma> -t <threshold> --odb` (`--grid` if Tengblad DSSSD in use)


## __Usage__

```
usage: dsssdCal <input file> [-o <output file>] [-s <sigma>] [-t <threshold> ] [--grid] [--draw] [--help]
Run 'dsssdCal --help' for more information.

	<input file>     	 name of rootfile containing triple alpha source data;
	                 	 if full path not given, default search path is $DH
	                 	 environment variable (if it exists), otherwise search
	                 	 path is $PWD.
Options:
	-o <output file> 	 Specify output file
	-s <sigma>       	 Specify sigma for TSpectrum::Search
	-t <threshold>   	 Specify threshold for TSpectrum::Search
	--draw           	 Draw calibrated spectra (not yet implemented)
	--full           	 Save full calibrated odb tree to .xml file as:
	                 	 $DH/../calibration/dsssdCal_full.xml
	                 	 (automatically switches --odb to true)
	--grid           	 Tengblad design DSSSD in use
	--help           	 Show this help message
	--json           	 Save .json file of DSSSD ODB variables to 
	                 	 $DH/../calibration/<input filename>_dsssdCal.json
	--odb            	 Write DSSSD calibration variables to ODB
	--reset          	 Reset DSSSD ODB variables; if given with an input file
	                 	 specified, requisite midas file is reanalyzed with reset
	                 	 ODB variables (overwrites previous rootfile)
	--xml            	 Save .xml file of DSSSD ODB variables to: 
	                 	 $DH/../calibration/<input filename>_dsssdCal.xml

``` 


## _Todo__

- Implement a TApplication in order to draw calibrated spectra
- write method (similar to `DsssdCalibrator::Run`) to handle pulser walk data
- Implement GUI to handle manual selection of triple alpha peaks in each channel (similar to hvcalib)
- Add option hybrid detector (with mylar window of given thickness) and add necessary code to calculate energy loss in window.

[//]: # Local Variables:
[//]: # mode: gfm
[//]: # End: