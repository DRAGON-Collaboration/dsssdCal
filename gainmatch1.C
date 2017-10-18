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

// macro to gainmatch the DRAGON DSSSD using rootfile of an attenuated beam run.
// Author: D. Connolly

// const Double_t alpha_E[3] = {5.15659,5.48556,5.80477}; // alpha energies of triple alpha source in MeV
// const Double_t dEdx[3]    = {160.1167,154.1106,148.9053}; // stopping powers (MeV/mm) of alphas in Al corresponding to above energies according to SRIM 2008

const Int_t nstrips       = 32;
const Int_t cut_channel   = 128; // set to relatively high channel in case ADC thresholds aren't set appropriately
const Double_t beam_E     = 3.035; // outgoing beam energy
// const Double_t dLayer     = 0.000374; // dead layer thickness (Al equivalent) in mm (from C. Wrede's thesis).
const Double_t dLayer     = 5e-5; // dead layer thickness (Al equivalent) in mm (from C. Wrede's thesis).
const Double_t dEdx       = 487.31;// stopping powers (MeV/mm) of beam in Si
const Double_t E_beam     = beam_E - dEdx*dLayer; //{alpha_E[0]-dEdx[0]*dLayer,alpha_E[1]-dEdx[1]*dLayer,alpha_E[2]-dEdx[2]*dLayer};
const Double_t sigma  = 2;
const Double_t thresh = 0.5;

void dsssdcal(const char * fname){//, bool gain=0){

    // const char* fname[128];
    // sprintf(fname,"${DH}/rootfiles/run%i.root",runnum);

    Double_t gain[nstrips];
    Double_t chan[nstrips];
    Int_t min_peak = pow(2.0,12);
    Int_t min_chan = 99;


    TFile *f=TFile::Open(fname);

    TH1D style("dsssd_ch","",1024,0,4095);
    TH1D *hArray[nstrips];

    for(Int_t i=0;i<nstrips;i++) {
        hArray[i]= (TH1D*)style.Clone(Form("dsssd_%d",i));
    }


    dragon::Tail* ptail = new dragon::Tail();
    t3->SetBranchAddress("tail", &ptail);
    for(Long_t evt = 0; evt < t3->GetEntries(); evt++) {
        t3->GetEntry(evt);
        for (Int_t i = 0; i < nstrips; ++i){
            Double_t val = ptail->dsssd.ecal[i];
            if(val<cut_channel) continue;
            hArray[i]->Fill(val);
        }
    }

    // Get peak centroid
    for (Int_t i = 0; i < nstrips; i++){

        TSpectrum *s = new TSpectrum();
        const Int_t nfound = s->Search(hArray[i],sigma,"",thresh);

        cout << "Found " << nfound << " heavy ion peaks in strip " << i << " attenuated beam spectrum.\n";

        if (nfound == 0 ) {
            cout << "Warning! No heavy ion peaks found in strip " << i << " ; skipping! \n";
            chan[i] = 4095;
        }
        else{
            Float_t *xpeaks  = s->GetPositionX();
            chan[i] = xpeaks[0];
        }

        cout << "Centroid for for strip " << i << " in channel " << chan[i] << "\n";

        if ( chan[i] < min_peak) {
            min_peak = chan[i];
            min_chan = i;
        }
    }

    cout << "Minimum gain channel = " << min_chan << "\t" << "; centroid = " << min_peak << "\n";

    cout << "Channel" << "\t" << "\t" << "Gain" << "\n";
    cout << "=======" << "\t" << "\t" << "====" << "\n";
    // Calculate and write gains to odb
    for (Int_t i = 0; i < nstrips; i++){
        gain[i] = min_peak / chan[i];
        if ( chan[i] == 4095) gain[i] = -1.999;

        cout << i << "\t" << "\t" << gain[i] << "\n";

        gSystem->Exec(Form("odbedit -c \"set /dragon/dsssd/variables/adc/slope[%d] %.6g\"\n", i, gain[i] ) );

    }

    // for (Int_t i = 0; i < nstrips; i++){
    // }

    // cout << "ATTENTION: Gains and offsets written to odb!\n";

    // Save current odb state to xml file
    // gSystem->Exec("odbedit -c 'save -x dsssdcal.xml'"); // save calibration as xml file in cwd
    // gSystem->Exec("cp -f ./dsssdcal.xml ${DH}/../dsssdCal/"); // copy xml file to $DH/../dsssdCal
    // gSystem->Exec("mv -f ./dsssdcal.xml ${DH}"); // move xml file to $DH
    // cout << "ATTENTION: Current odb state saved as ${DH}/dsssd.xml!\n";

    // Fill calibrated summary spectrum
    // TCanvas *c2=new TCanvas();

    TH2F *dsssd_cal = new TH2F("dsssd_cal","Calibrated DSSSD Spectrum",nstrips,0,nstrips,1024,0,4095);

    dragon::Tail* ptail = new dragon::Tail();
    t3->SetBranchAddress("tail", &ptail);
    for(Long_t evt = 0; evt < t3->GetEntries(); evt++) {
        t3->GetEntry(evt);
        for(Int_t i=0; i<nstrips; i++){
            Double_t val = (ptail->dsssd.ecal[i])*gain[i];
            // if(val<1.0) continue;
            dsssd_cal->Fill(i,val);
        }
    }

    t3->ResetBranchAddresses();
    delete ptail;

    TH2F *h0=new TH2F("h0","Uncalibrated DSSSD Spectrum",
                      nstrips,0,nstrips,1024,0,4095);
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

    file->Close();

    gSystem->Exec("mv -f ./dsssdcal.root ${DH}/../dsssdCal/"); // move root file to experiment directory

    cout << "Wrote calibrated spectra to $DH/../dsssdCal/ .\n";
    cout << "Run dsssd_draw to view dsssd spectra.\n";

}
