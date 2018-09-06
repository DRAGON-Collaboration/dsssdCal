////////////////////////////////////////////////////////////////////////////////
/// \file dsssdCal.cxx
/// \author D. Connolly
/// \brief Implements the main() function for a simple program
///  to calibrate the DRAGON DSSSD
////////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <string>
#include <memory>
#include <cassert>
#include <algorithm>
#include <iostream>
#include <TROOT.h>
#include <TString.h>
#include <TSystem.h>
#include <TTree.h>
#include <TFile.h>
#include <TCanvas.h>
#include <TError.h>
#include "midas.h"
#include "Dragon.hxx"
#include "midas/Database.hxx"
#include "utils/definitions.h"
#include "utils/RootAnalysis.hxx"
#include "utils/Calibration.hxx"

#ifndef DOXYGEN_SKIP
namespace {
  bool arg_return = false;
  const char* const msg_use =
	"usage: dsssdCal <input file> [-o <output file>] [-s <sigma>] [-t <threshold> ] "
	"[--grid] [--draw] [--help]\n";
}
#endif

//
/// Encloses dsssdCal helper functions
namespace dCal{

  // Skip documenting a bunch of internal stuff
#ifndef DOXYGEN_SKIP
  typedef std::vector<std::string> strvector_t;

  // Error handling stuff
  std::ostream cnul (0);

  class strm_ref {
  public:
	strm_ref(std::ostream& strm):
      pstrm(&strm) {}
	strm_ref():
      pstrm(&cnul) {}
	strm_ref& operator= (std::ostream& strm)
    { pstrm = &strm; return *this; }
	template <class T>
	std::ostream& operator<< (const T& t)
    { return *pstrm << t; }
	std::ostream& flush()
    { return std::flush(*pstrm); }
  private:
	std::ostream* pstrm;
  };

  inline std::ostream& flush(strm_ref& strm)
  { return strm.flush(); }

  strm_ref cout = std::cout;
  strm_ref cwar = std::cerr;
  strm_ref cerr = std::cerr;

#endif

  ////////////////////////////////////////////////////////////////////////////////
  /// Program options
  struct Options_t {
    std::string fIn;
    std::string fOut;
    std::string fSigma;
    std::string fThresh;
    bool fDraw;
    bool fFull;
    bool fGrid;
    bool fJson;
    bool fOdb;
    bool fReset;
    bool fXml;
    Options_t(): fDraw(false), fFull(false), fGrid(false), fOdb(false), fReset(false), fJson(false), fXml(false) {}
  };

  ////////////////////////////////////////////////////////////////////////////////
  /// Print a usage message
  int show_usage(const char* what = 0)
  {
	const char* msg =
      "Run 'dsssdCal --help' for more information.\n";
	dCal::cerr << msg_use << msg << std::endl;
	if (what) dCal::cerr << "Error: " << what << ".\n";
	arg_return = true;
	return 1;
  }

  ////////////////////////////////////////////////////////////////////////////////
  /// Print a help message
  int help()
  {
    show_usage(0);
    dCal::cerr << "\t<input file>     \t name of rootfile containing triple alpha source data;\n"
               << "\t                 \t if full path not given, default search path is $DH\n"
               << "\t                 \t environment variable (if it exists), otherwise search\n"
               << "\t                 \t path is $PWD.\n"
               << "Options:\n"
               << "\t-o <output file> \t Specify output xml file\n"
               << "\t-s <sigma>       \t Specify sigma for TSpectrum::Search\n"
               << "\t-t <threshold>   \t Specify threshold for TSpectrum::Search\n"
               << "\t--draw           \t Draw calibrated spectra (not yet implemented)\n"
               << "\t--full           \t Save full calibrated odb tree to .xml file as: \n"
               << "\t                 \t $DH/../calibration/dsssdCal_full.xml"
               << "\t                 \t (automatically switches --odb to true)"
               << "\t--grid           \t Tengblad design DSSSD in use\n"
               << "\t--help           \t Show this help message\n"
               << "\t--json           \t Save .json file of DSSSD ODB variables to \n"
               << "\t                 \t $DH/../calibration/<input filename>_dsssdCal.json\n"
               << "\t--odb            \t Write DSSSD calibration variables to ODB\n"
               << "\t--reset          \t Reset DSSSD ODB variables; if given with an input file\n"
               << "\t                 \t specified, requisite midas file is reanalyzed with reset\n"
               << "\t                 \t ODB variables (overwrites previous rootfile)\n"
               << "\t--xml            \t Save .xml file of DSSSD ODB variables to: \n"
               << "\t                 \t $DH/../calibration/<input filename>_dsssdCal.xml"
               << std::endl;
	arg_return = true;
    return 0;
  }

  ////////////////////////////////////////////////////////////////////////////////
  /// Reset DSSSD ODB variables and reanalyze file (if given)
  int reset(bool infile = false, Options_t* options = 0)
  {
    int result = system("odbedit -c 'set /dragon/dsssd/variables/adc/offset[0..31] 0'");
    result = system("odbedit -c 'set /dragon/dsssd/variables/adc/slope[0..31] 1'");
    result = system("odbedit -c 'set /sonik/variables/adc/offset[0..31] 0'");
    result = system("odbedit -c 'set /sonik/variables/adc/slope[0..31] 1'");
    if(result != 0) cout << "\nError: Failed to reset DSSSD variables.\n";
    else cout << "Attention: DSSSD ODB variables reset.\n";
    if(infile){
      cout << "Attention: Reanalyzing specified input file with ODB variables reset.\n";
      result = system("odbedit -c \"save -x $DH/../calibration/dsssd_reset.xml\"");
      TString vars = "dsssd_reset.xml";
      vars = gSystem->BaseName(vars.Data());
      TString caldir = "$DH/../calibration";
      gSystem->PrependPathName(caldir.Data(), vars);

      // Get input file name
      TString in = options->fIn.c_str();
      in.Remove(in.Index(".root"));
      in.Append(".mid");
      if(options->fIn.substr(0, 3) == "run"){
        TString indir = "$DH";
        Int_t gel = gErrorIgnoreLevel;
        gErrorIgnoreLevel = 3001; // Suppress error if $DH isn't available
        Bool_t have_DH = !(gSystem->ExpandPathName(indir));
        gErrorIgnoreLevel = gel; // reset error level
        if (!have_DH) indir = ".";
        gSystem->PrependPathName(indir.Data(), in);
      }

      TString command = in;
      command.Prepend("mid2root ");
      command.Append(" -v ");
      command.Append(vars);
      command.Append(" --singles --overwrite");
      result = system(command);
    }
	arg_return = true;
    return result;
  }

  ////////////////////////////////////////////////////////////////////////////////
  /// Parse command line arguments
  int process_args(int argc, char** argv, Options_t* options)
  {
	strvector_t args(argv+1, argv+argc);
	strvector_t::iterator iarg = args.begin();

	//
	// Look for input file
	for(iarg = args.begin(); iarg != args.end(); ++iarg) {
      if(iarg->substr(0, 2) == "--")
        continue;
      options->fIn = *iarg;
      break;
	}

	//
	// Check other arguments
	for(iarg = args.begin(); iarg != args.end(); ++iarg) {
      if (*iarg == options->fIn) { // Input file, already set
        continue;
      }
      else if (*iarg == "--draw") { // Draw summary
        options->fDraw = true;
      }
      else if (*iarg == "--full") { // write full odb tree to .xml
        options->fFull = true;
        options->fOdb  = true;
      }
      else if (*iarg == "--grid") { // Gridded DSSSD
        options->fGrid = true;
      }
      else if (*iarg == "--help") { // Help message
        return help();
      }
      else if (*iarg == "--json") { // write vars to .json
        options->fJson = true;
      }
      else if (*iarg == "--odb") { // write vars to ODB
        options->fOdb = true;
      }
      else if (*iarg == "--reset") { // reset ODB vars
        if( !(options->fIn.empty()) ){
          return reset(true, options);
        }
        return reset();
      }
      else if (*iarg == "--xml") { // write vars to .xml
        options->fXml = true;
      }
      else if (*iarg == "-o") { // Output file
        if (++iarg == args.end()) return show_usage("output file not specified");
        options->fOut = *iarg;
      }
      else if (*iarg == "-s") { // sigma
        if (++iarg == args.end()) return show_usage("sigma not specified");
        options->fSigma = *iarg;
      }
      else if (*iarg == "-t") { // threshold
        if (++iarg == args.end()) return show_usage("threshold not specified");
        options->fThresh = *iarg;
      }
      else { // Unknown flag
        TString what = "unknown flag \'";
        what += *iarg; what += "\'";
        show_usage(what.Data());
        return 1;
      }
	}

	if (options->fIn.empty()) // Didn't find input file
      return show_usage("no input file specified");

	return 0;
  }

} // namespace dCal

////////////////////////////////////////////////////////////////////////////////
/// The main function implementation
int main(int argc, char** argv)
{
  double sigma, thresh;
  dCal::Options_t options;
  int arg_result = dCal::process_args(argc, argv, &options);
  if(arg_return) return arg_result;

  // Get input file name
  TString in = options.fIn.c_str();

  if(options.fIn.substr(0, 3) == "run"){
    TString indir = "$DH";
    Int_t gel = gErrorIgnoreLevel;
    gErrorIgnoreLevel = 3001; // Suppress error if $DH isn't available
    Bool_t have_DH = !(gSystem->ExpandPathName(indir));
    gErrorIgnoreLevel = gel; // reset error level

  incheck:
    if (have_DH) { // Check for $DH/../calibration
      indir += "/rootfiles";
      void *d = gSystem->OpenDirectory(indir.Data());
      if (d) // Directory exists, need to free it
        gSystem->FreeDirectory(d);
      else { // Doesn't exist, set path back to "."
        have_DH = false;
        goto incheck;
      }
    }
    else indir = ".";

    gSystem->PrependPathName(indir.Data(), in);
  }

  TString db_str = in.Data();
  if(db_str.Contains("/rootfiles")){
    db_str.Remove(db_str.Index("/rootfiles"), 10);
  }
  db_str.Remove(db_str.Index(".root"));
  db_str.Append(".xml");


  TString sigma_str = options.fSigma.c_str();
  TString thresh_str = options.fThresh.c_str();

  if(options.fSigma.empty()){
    sigma = 5;
  }else{
    sigma = std::atof(sigma_str);
  }

  if(options.fThresh.empty()){
    thresh = 0.2;
  }else{
    thresh = std::atof(thresh_str);
  }

  // Get output file name
  TString out = options.fOut.empty() ? options.fIn.c_str() : options.fOut.c_str();
  // If no output specified, create file name from input
  if(options.fOut.empty()){
    // Strip '.root' extension if it's there
    if(out.Contains(".root")){
      out.Remove(out.Index(".root"));
    }
    // Add '.xml' extension
    out.Append("_dsssdCal.xml");
    // Strip input directory
    out = gSystem->BaseName(out.Data());
    // Check for $DH/rootfiles
    TString outdir = "$DH";
    int gel = gErrorIgnoreLevel;
    gErrorIgnoreLevel = 3001; // Suppress error if $DH isn't available
    bool have_DH = !(gSystem->ExpandPathName(outdir));
    gErrorIgnoreLevel = gel; // reset error level

  outcheck:
    if (have_DH) { // Check for $DH/../calibration
      outdir += "/../calibration";
      void *d = gSystem->OpenDirectory(outdir.Data());
      if (d) // Directory exists, need to free it
        gSystem->FreeDirectory(d);
      else { // Doesn't exist, set path back to "."
        have_DH = false;
        goto outcheck;
      }
    }
    else outdir = ".";

    gSystem->PrependPathName(outdir.Data(), out);
  } // if (options.fOut.empty()) {

  midas::Database *db = new midas::Database(db_str);

  TFile fa(in);
  TTree *ta = (TTree *)fa.Get("t3");

  dragon::utils::DsssdCalibrator dcal(ta, db);
  std::cout << "Running calibration!\n";

  // dcal.RunPulser();
  // dcal.RunAlpha();
  // dcal.Run(835,500,3840,5,0.2,kTRUE);
  dcal.Run(835, 500, 3840, sigma, thresh, options.fGrid);

  dcal.PrintResults();

  // must implement TApplication for this to work
  // if(options.fDraw){
  //   // dcal.DrawSummary();
  //   // new TCanvas();
  //   dcal.DrawSummaryCal();
  //   new TCanvas();
  //   dcal.DrawFrontCal();
  // }

  if(options.fXml) dcal.WriteXml(out.Data());
  if(options.fJson){
    out.Remove(out.Index(".xml"));
    out.Append(".json");
    dcal.WriteJson(out.Data());
  }
  if(options.fOdb){
    dcal.WriteOdb(kFALSE, kFALSE);
    if(options.fFull){
      system("odbedit -c \"save -x $DH/../calibration/dsssdCal_full.xml\"");
      std::cout << "ATTENTION: Full ODB tree saved to $DH/../dsssdCal_full.xml\n";
    }
  }

  fa.Close();
  return 0;

}
