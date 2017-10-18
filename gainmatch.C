// macro to gain match the DRAGON DSSSD using rootfiles of pulser walkthroughs for the front and back strips
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


const Int_t nstrips       = 32;
const Int_t cut_channel   = 200; // set to relatively high channel in case ADC thresholds aren't set appropriately
const Double_t alpha_E[3] = {5.15659,5.48556,5.80477}; // alpha energies of triple alpha source in MeV
const Double_t dLayer     = 0.000374; // dead layer thickness (Al equivalent) in mm (from C. Wrede's thesis).
const Double_t dEdx[3]    = {160.1167,154.1106,148.9053}; // stopping powers (MeV/mm) of alphas in Al corresponding to above energies according to SRIM 2008
const Double_t E_peak[3]  = {alpha_E[0]-dEdx[0]*dLayer,alpha_E[1]-dEdx[1]*dLayer,alpha_E[2]-dEdx[2]*dLayer};
const Double_t sigma      = 2;
const Double_t thresh     = 0.2;


Double_t *pulser(const char *fname, Int_t strip,Double_t sigma,Double_t thresh){

    TFile *f=TFile::Open(fname);

    TH1F *h1      = new TH1F("h1","h1",4096,0,4095);
    Double_t *par = new Double_t[2];

    t3->Draw(Form("dsssd.ecal[%i]>>h1",strip),Form("dsssd.ecal[%i]>200",strip),"goff");
    TSpectrum *s = new TSpectrum();
    Int_t nfound = s->Search(h1,sigma,"",thresh);
    const int npoints = nfound;
    cout << "Found " << nfound << " pulser peaks in strip " << strip << " pulser walk spectrum.\n";

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

        for (Int_t p=0; p<npoints; p++){
            xsort   = xpeaks[index[p]];
            pulser  = p;
            g1->SetPoint(p,pulser,xsort);
            // cout << p << "\t" << pulser << "\t" << xsort << "\n";
        }

        // new TCanvas();
        // g1->SetMarkerStyle(21);
        // g1->Draw("AP");
        TFitResultPtr fit=g1->Fit("pol1","qns");
        par[0] = fit->Value(0);
        cout << "Offset for strip " << strip << ":\n";
        cout << par[0] << "\n";
        par[1] = fit->Value(1);

        return par;

        h1->Delete();
        f->Close();
    }

}

void gainmatch(const Int_t runnum){//, bool gain=0){

    const char* fname = Form("${DH}/rootfiles/run%i.root",runnum);

    Double_t offset[nstrips];
    Double_t gain[nstrips];
    Double_t intercept[nstrips];
    Double_t gain0 = 4096.;
    Double_t *cal = new Double_t[2];

    // Get offsets and energy calibration parameters
    for (Int_t i=0; i<nstrips; i++){
        cal = pulser(fname,i,sigma,thresh); // front strips
        offset[i]     = cal[0];
        gain[i]       = cal[1];
        if(gain[i] < gain0) gain0 = gain[i];
    }

    for (Int_t i = 0; i < nstrips; ++i){
        Double_t gn = gain[i] / gain0;
        gSystem->Exec(Form("odbedit -c \"set /dragon/dsssd/variables/adc/slope[%d] %.6g\"\n", i, gn));
        gSystem->Exec(Form("odbedit -c \"set /dragon/dsssd/variables/adc/offset[%d] %.6g\"\n", i,-offset));

    }

    cout << "ATTENTION: Gains and offsets written to odb!\n";

    // Save current odb state to xml file
    gSystem->Exec("odbedit -c 'save -x dsssdcal.xml'"); // save calibration as xml file in cwd
    gSystem->Exec("cp -f ./dsssdcal.xml ${DH}/../dsssdCal/"); // copy xml file to $DH/../dsssdCal
    gSystem->Exec("mv -f ./dsssdcal.xml ${DH}"); // move xml file to $DH
    cout << "ATTENTION: Current odb state saved to dsssd.xml in $DH.\n";

    // Fill calibrated summary spectrum
    // TCanvas *c2=new TCanvas();
    TH2F *dsssd_cal = new TH2F("dsssd_cal","Calibrated DSSSD Spectrum",nstrips,0,nstrips,4096,0,20);
    dragon::Tail* ptail = new dragon::Tail();
    t3->SetBranchAddress("tail", &ptail);
    for(Long_t evt = 0; evt < t3->GetEntries(); evt++) {
        t3->GetEntry(evt);
        for(Int_t i=0; i<nstrips; i++){
            Double_t val = (ptail->dsssd.ecal[i]-offset[i])*gain[i]+intercept[i];
            if(val<1.5) continue;
            dsssd_cal->Fill(i,val);
        }
    }

    t3->ResetBranchAddresses();
    delete ptail;

    TH2F *h0=new TH2F("h0","Uncalibrated DSSSD Spectrum",
                      nstrips,0,nstrips,4096,0,4095);
    TH1F *h2=new TH1F("h2","Uncalibrated DSSSD Energy (Front)",4096,0,4095);
    TH1F *h3=new TH1F("h3","Uncalibrated DSSSD Energy (Back)",4096,0,4095);

    t3->Draw("dsssd.ecal[]:Iteration$>>h0","","goff");
    t3->Draw("dsssd.efront>>h2","","goff");
    t3->Draw("dsssd.eback>>h3","","goff");

    // Create rootfile for writing
    TFile *file=new TFile("dsssdcal_all.root","RECREATE");

    dsssd_cal->Write();
    h0->Write();
    h2->Write();
    h3->Write();

    file->Close();

    gSystem->Exec("mv -f ./dsssdcal.root ${DH}/../dsssdCal/"); // move root file to experiment directory

    cout << "Wrote calibrated spectra to $DH/../dsssdCal/ .\n";
    cout << "Run dsssd_draw to view dsssd spectra.\n";


}
