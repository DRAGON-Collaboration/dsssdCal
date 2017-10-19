// macro to calibrate the DRAGON DSSSD from data in the rootfiles of pulser walkthrough for the front and back strips, as well as a triple-alpha energy calibration run.
// Author: D. Connolly

#include <cstdio>
#include <cstring>
#include <sstream>
#include <Riostream.h>
#include <TString.h>
#include <TFile.h>
#include <TFitResult.h>
#include <TSystem.h>
#include <TDirectory.h>
#include <TH1.h>
#include <TH2.h>
#include <TTree.h>
#include <TGraph.h>
#include <TMath.h>
#include "Dragon.hxx"


static const Int_t Nalpha       = 3;
static const Int_t cut_channel  = 352; // set to relatively high channel in case ADC thresholds aren't set appropriately
static const Int_t Max_pulser   = 12;
static const Double_t sigma_f   = 3;
static const Double_t thresh_f  = 0.25;
static const Double_t sigma_b   = 5;
static const Double_t thresh_b  = 0.2;

const Double_t alpha_E[3]    = {5.15659,5.48556,5.80477}; // alpha energies of triple alpha source in MeV
const Double_t dLayer        = 0.00374; // dead layer thickness (Al equivalent) in mm (from C. Wrede's thesis).
const Double_t dLayer_grid   = 5e-8; // dead layer thickness (Si) of Tengblad design (girdded) DSSSD
// const Double_t E_peak2[3] = {alpha_E[0]-0.0059122,alpha_E[1]-0.0057372,alpha_E[2]-0.0054921}; // LISE 50 nm Si
const Double_t dEdx_Al[3]    = {139.006,133.701,129.296}; // stopping powers (MeV/mm) of alphas in Si corresponding to above energies according to SRIM 2008
const Double_t dEdx_Si[3]    = {160.1167,154.1106,148.9053}; // stopping powers (MeV/mm) of alphas in Al corresponding to above energies according to SRIM 2008
const Double_t E_dep[3]      = {alpha_E[0]-dEdx_Al[0]*dLayer,alpha_E[1]-dEdx_Al[1]*dLayer,alpha_E[2]-dEdx_Al[2]*dLayer};
const Double_t E_dep_grid[3] = {alpha_E[0]-dEdx_Si[0]*dLayer_grid,alpha_E[1]-dEdx_Si[1]*dLayer_grid,alpha_E[2]-dEdx_Si[2]*dLayer_grid};


Double_t pulser(const char *fname, Int_t strip, Double_t sigma, Double_t thresh)
{

    TFile *f = TFile::Open(fname);

    TH1F *h1 = new TH1F("h1", "h1", 4096, 0, 4095);
    Double_t offset;

    t3->Draw(Form("dsssd.ecal[%i]>>h1",strip),Form("dsssd.ecal[%i]>%f",strip,cut_channel),"goff");
    TSpectrum *s = new TSpectrum();
    Int_t nfound = s->Search(h1,sigma,"",thresh);
    const int npoints = nfound;
    cout << "Found " << nfound << " pulser peaks in strip " << strip << "\n";

    if (nfound < 3){
        cout << "WARNING: Insufficient number of pulser peaks to find offset for strip " << strip << "; skipping...\n";
        offset = 0;
        return offset;
        h1->Delete();
    }

    else{
        Float_t *xpeaks = s->GetPositionX();
        Int_t index[npoints];
        TMath::Sort(npoints,xpeaks,index,0);

        Float_t xsort;
        Double_t pulser;
        TGraph *g1=new TGraph(npoints);

        for (Int_t p = 0; p < npoints; p++){
            xsort   = xpeaks[index[p]];
            pulser  = (p+1)/2.;
            g1->SetPoint(p, pulser, xsort);
            // cout << p << "\t" << pulser << "\t" << xsort << "\n";
        }

        // new TCanvas();
        // g1->SetMarkerStyle(21);
        // g1->Draw("AP");
        TFitResultPtr fit=g1->Fit("pol1","qns");
        offset = fit->Value(0);
        cout << "Offset for strip " << strip << ":\n";
        cout << offset << "\n";
        // Double_t slope     = fit->Value(1);

        return offset;

        h1->Delete();
        f->Close();
    }

}

Double_t *alpha(const char *fname, Int_t strip, Double_t offset, Double_t sigma, Double_t thresh)
{
    TFile *f=TFile::Open(fname);

    TH1F *h1 = new TH1F("h1","h1",4096,0,4095);
    Double_t *par=new Double_t[2];

    dragon::Tail* ptail = new dragon::Tail();
    t3->SetBranchAddress("tail", &ptail);
    for(Long_t evt = 0; evt < t3->GetEntries(); evt++) {
        t3->GetEntry(evt);
        Double_t val = ptail->dsssd.ecal[strip]-offset;
        if(val<cut_channel) continue;
        h1->Fill(val);
    }

    TSpectrum *s = new TSpectrum();
    const Int_t nfound = s->Search(h1, sigma, "", thresh);
    cout << "Found " << nfound << " alpha peaks in strip " << strip << " spectrum.\n";

    if (nfound < 3){
        cout << "WARNING: Failed to find 3 alpha peaks in strip " << strip << " spectrum; skipping calibration.\n";
        par[0] = 0, par[1] = -1.99999;
        return par;
        h1->Delete();
    }

    else if (nfound > 3){
        cout << "WARNING: Found too many alpha peaks in strip " << strip << " spectrum; skipping calibration.\n";
        cout << "You may need to adjust threshold and/or sigma argument(s) for TSpectrum::Search() (in dsssdcal.C).\n";
        par[0] = 0, par[1] = -1.99999;
        return par;
        h1->Delete();
    }

    else{

        const Int_t npoints = nfound;
        Float_t *xpeaks=s->GetPositionX();

        Float_t xsort;
        Int_t index[npoints];
        TMath::Sort(npoints,xpeaks,index,kFALSE);
        TGraph *g1=new TGraph(npoints);

        for (Int_t p=0; p<nfound; p++){
            xsort = xpeaks[index[p]];
            g1->SetPoint(p,xsort,E_dep_grid[p]);
            // cout << p << "\t" << xsort << "\t" << E_peak[p] << "\n";
        }

        // new TCanvas();
        // g1->SetMarkerStyle(21);
        // g1->Draw("AP");
        // g1->Fit("pol1","Q");
        TFitResultPtr fit = g1->Fit("pol1","qns");
        Double_t *par=new Double_t[2];
        par[0] = fit->Value(0), par[1] = fit->Value(1);
        cout << "Energy calibration parameters for strip " << strip << ":\n";
        cout << "par[0]" << "\t" << "par[1]\n";
        cout << par[0] << "\t" << par[1] << "\n";
        return par;
        h1->Delete();
    }

    f->Close();

}

void dsssdcal(const char *f1, const char *f2, Bool_t odb = kTRUE, Bool_t xml = kTRUE){

    Double_t offset[dragon:Dsssd:MAX_CHANNELS];
    Double_t gain[dragon:Dsssd:MAX_CHANNELS];
    Double_t intercept[dragon:Dsssd:MAX_CHANNELS];

    // Get offsets and energy calibration parameters
    for (Int_t i = 0; i < dragon:Dsssd:MAX_CHANNELS; i++){
        if(i < 16) {
            offset[i]     = pulser(f1, i, sigma_f, thresh_f);
            Double_t *par = alpha(f2, i, offset[i], sigma_f, thresh_f);
        }
        else {
            offset[i]     = pulser(f1,i, sigma_b, thresh_b);
            Double_t *par = alpha(f2, i, offset[i], sigma_b, thresh_b);
        }
        intercept[i]  = par[0];
        gain[i] = par[1];
        delete[] par;
    }

    if(odb){
        // Write slopes and offsets to odb
        for(Int_t i=0; i< dragon:Dsssd:MAX_CHANNELS; ++i) {
            gSystem->Exec(Form("odbedit -c \"set /dragon/dsssd/variables/adc/slope[%d] %.6g\"\n", i, gain[i]));
            gSystem->Exec(Form("odbedit -c \"set /dragon/dsssd/variables/adc/offset[%d] %.6g\"\n", i,intercept[i]-offset[i]*gain[i]));
        }
        cout << "ATTENTION: Gains and offsets written to odb!\n";
    }

    if(xml){
    // Save current odb state to xml file
    gSystem->Exec("odbedit -d /dragon/dsssd/variables/adc -c 'save -x dsssdcal.xml'"); // save calibration as xml file in pwd
    gSystem->Exec("if [ ! -d $DH/../calibration ]; then mkdir -p $DH/../calibration; fi");
    gSystem->Exec("mv -f ./dsssdcal.xml ${DH}/../calibration"); // move xml file to $DH/../calibration/
    cout << "ATTENTION: Current odb state saved to dsssd.xml in ${DH}/../calibration.\n";
    }

    // Fill calibrated summary spectrum
    // TCanvas *c2=new TCanvas();
    TH2F *dsssd_cal = new TH2F("dsssd_cal","Calibrated DSSSD Spectrum",dragon:Dsssd:MAX_CHANNELS,0,dragon:Dsssd:MAX_CHANNELS,4096,0,20);
    dragon::Tail* ptail = new dragon::Tail();
    t3->SetBranchAddress("tail", &ptail);
    for(Long_t evt = 0; evt < t3->GetEntries(); evt++) {
        t3->GetEntry(evt);
        for(Int_t i=0; i<dragon:Dsssd:MAX_CHANNELS; i++){
            Double_t val = (ptail->dsssd.ecal[i]-offset[i])*gain[i]+intercept[i];
            if(val<1.5) continue;
            dsssd_cal->Fill(i,val);
        }
    }

    t3->ResetBranchAddresses();
    delete ptail;

    TH2F *h0=new TH2F("h0","Uncalibrated DSSSD Spectrum",dragon:Dsssd:MAX_CHANNELS,0,dragon:Dsssd:MAX_CHANNELS,4096,0,4095);
    TH1F *h2=new TH1F("h2","Uncalibrated DSSSD Energy (Front)",4096,0,4095);
    TH1F *h3=new TH1F("h3","Uncalibrated DSSSD Energy (Back)",4096,0,4095);

    t3->Draw("dsssd.ecal[]:Iteration$>>h0","","goff");
    t3->Draw("dsssd.efront>>h2","","goff");
    t3->Draw("dsssd.eback>>h3","","goff");

    // Create rootfile for writing
    TFile *file=new TFile("dsssdcal.root","RECREATE");

    dsssd_cal->Write();
    h0->Write();
    h2->Write();
    h3->Write();

    gSystem->Exec("mv -f ./dsssdcal.root ${DH}/../calibration/"); // move root file to exp dir
    cout << "Wrote calibrated spectra to $DH/../calibration/dsssdcal.root.\n";
    cout << "Run dsssd_draw to view calibrated dsssd spectra.\n";


}
