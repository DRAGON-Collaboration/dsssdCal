////////////////////////////////////////////////////
/// Class to gainmatch Sonik detectors           ///
///                                              ///
/// 02/2017 - Devin Connolly                     ///
////////////////////////////////////////////////////
#ifndef HAVE_GAINMATCH_HXX
#define HAVE_GAINMATCH_HXX
#ifndef __MAKECINT__
#include <iostream>
#endif

#include <vector>

#include "Riostream.h"
#include "TString.h"
#include "TSystem.h"
#include "TFile.h"
#include "TFitResult.h"
#include "TH1.h"
#include "TH2.h"
#include "TTree.h"
#include "TGraph.h"
#include "TGraphErrors.h"
#include "TMath.h"
#include "TF1.h"
#include "TObject.h"
#include "TVectorD.h"

//namespace dragon {

static const Int_t N_STRIPS     = 32;
static const Int_t Nalpha       = 3;
static const Int_t cut_channel  = 352; // set to relatively high channel in case ADC thresholds aren't set appropriately
static const Int_t Max_pulser   = 12;
static const Double_t sigma     = 10;
static const Double_t thresh    = 0.25;

const Double_t alpha_E[3]    = {5.15659,5.48556,5.80477}; // alpha energies of triple alpha source in MeV
const Double_t dLayer        = 0.00374; // dead layer thickness (Al equivalent) in mm (from C. Wrede's thesis).
const Double_t dLayer_grid   = 5e-8; // dead layer thickness (Si) of Tengblad design (girdded) DSSSD
// const Double_t E_peak1[3] = {alpha_E[0]-0.023685,alpha_E[1]-0.022975,alpha_E[2]-0.021994}; //LISE 200 nm Si
// const Double_t E_peak2[3] = {alpha_E[0]-0.0059122,alpha_E[1]-0.0057372,alpha_E[2]-0.0054921}; LISE 50 nm Si
const Double_t dEdx_Al[3]    = {139.006,133.701,129.296}; // stopping powers (MeV/mm) of alphas in Si corresponding to above energies according to SRIM 2008
const Double_t dEdx_Si[3]    = {160.1167,154.1106,148.9053}; // stopping powers (MeV/mm) of alphas in Al corresponding to above energies according to SRIM 2008
const Double_t E_dep[3]      = {alpha_E[0]-dEdx_Al[0]*dLayer,alpha_E[1]-dEdx_Al[1]*dLayer,alpha_E[2]-dEdx_Al[2]*dLayer};
const Double_t E_dep_grid[3] = {alpha_E[0]-dEdx_Si[0]*dLayer_grid,alpha_E[1]-dEdx_Si[1]*dLayer_grid,alpha_E[2]-dEdx_Si[2]*dLayer_grid};

class Gainmatch{// class for gainmatch data
public:
	Gainmatch() { } ;
	inline virtual ~Gainmatch() { } ;
	void WriteTree();

public:
	Int_t pulser[Max_pulser]; //#
	Double_t u_p[Max_pulser]; //#
	Double_t alpha[Nalpha];   //#
	Double_t u_a[Nalpha];     //#
	Double_t offset;          //#
	Double_t gain;            //#
	Double_t INL;             //#

	TVectorD threshold(N_STRIPS);
	TVectorD offset(N_STRIPS);
	TVectorD gain(N_STRIPS);
	TVectorD INL(N_STRIPS);
	TVectorD c(1);

private:
	ClassDef(dragon::Gainmatch,1)
};

//} // namespace sonik


#endif
