////////////////////////////////////////////////////
/// Implemantaion of Gainmatch.hxx               ///
///                                              ///
/// 02/2017 - Devin Connolly                     ///
////////////////////////////////////////////////////

#include "Gainmatch.hxx"
ClassImp(dragon::Gainmatch);

void dsssdcal(const char *f_front, const char *f_back, const char *f_alpha){//, bool gain=0){

    Double_t offset[N_STRIPS];
    Double_t gain[N_STRIPS];
    Double_t intercept[N_STRIPS];

    // Gain Match
    for (Int_t i=0; i<N_STRIPS; i++){
        offset[i]  = pulser(f_front,i,sigma,thresh); // front strips
        Double_t *par = alpha(f_alpha,i,offset[i],sigma,thresh);
        intercept[i] = par[0];
        gain[i] = par[1];
        delete[] par;
    }

    for (Int_t i=0; i<N_STRIPS; i++){
        // Write gains and offsets to odb
        gSystem->Exec(Form("odbedit -c \"set /dragon/dsssd/variables/adc/slope[%d] %.6g\"\n", i, gain[i]));
        gSystem->Exec(Form("odbedit -c \"set /dragon/dsssd/variables/adc/offset[%d] %.6g\"\n", i,intercept[i]-offset[i]*gain[i]));
    }


    cout << "ATTENTION: Gains and offsets written to odb!\n";

    // Save current odb state to xml file
    gSystem->Exec("odbedit -c 'save -x dsssdcal.xml'"); // save calibration as xml file in cwd
    gSystem->Exec("cp -f ./dsssdcal.xml ${DH}/../dsssdCal/"); // copy xml file to $DH/../dsssdCal
    gSystem->Exec("mv -f ./dsssdcal.xml ${DH}"); // move xml file to $DH
    cout << "ATTENTION: Current odb state saved to dsssd.xml in $DH.\n";

    // Fill calibrated summary spectrum
    // TCanvas *c2=new TCanvas();
    TH2F *dsssd_cal = new TH2F("dsssd_cal","Calibrated DSSSD Spectrum",N_STRIPS,0,N_STRIPS,4096,0,20);
    dragon::Tail* ptail = new dragon::Tail();
    t3->SetBranchAddress("tail", &ptail);
    for(Long_t evt = 0; evt < t3->GetEntries(); evt++) {
        t3->GetEntry(evt);
        for(Int_t i=0; i<N_STRIPS; i++){
            Double_t val = (ptail->dsssd.ecal[i]-offset[i])*gain[i]+intercept[i];
            if(val<1.5) continue;
            dsssd_cal->Fill(i,val);
        }
    }

    t3->ResetBranchAddresses();
    delete ptail;

    TH2F *h0=new TH2F("h0","Uncalibrated DSSSD Spectrum",
                      N_STRIPS,0,N_STRIPS,1024,0,4095);
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

void dragon::Gainmatch::WriteTree(Int_t pulse_run, Int_t alpha_run){

    const char* fname0 = Form("${DH}/../calibration/gainmatch_%i_%i.root",pulse_run,alpha_run);
    const char* fname_p = Form("${DH}/rootfiles/run%i.root",pulse_run);
    const char* fname_a = Form("${DH}/rootfiles/run%i.root",alpha_run);

    TFile *f0 = new TFile(fname0,"RECREATE");
    f0->cd();

    // vars for writing rootfile
    Gainmatch *gm = new Gainmatch;

    TTree *tg = new TTree("tg","Measured and calculated gainmatch parameters");
    tg->Branch("gm","Gainmatch",&gm);

    Double_t gain_min = pow(2.0,16);
    Int_t min_chan;
    Double_t V0, b1, m1, b2, m2, V_line;

	Int_t ch[MAX_CHANNELS], pulse[MAX_CHANNELS][Npulser], alpha[MAX_CHANNELS][Nalpha];
	Double_t gain[MAX_CHANNELS], os[MAX_CHANNELS], nl[MAX_CHANNELS], up[MAX_CHANNELS][Npulser], ua[MAX_CHANNELS][Nalpha];

    TFile *f1 = TFile::Open(fnamep);
    f1->cd();

    for(Int_t i = 0; i < MAX_CHANNELS; ++i){

        TH1F *h1 = new TH1F("h1","h1",1024,0,4095);

        t3->Draw(Form("dsssd.ecal[%i]>>h1",i),Form("dsssd.ecal[%i]>%d",i,cut_channel),"goff");
        TSpectrum *s = new TSpectrum();
        Int_t nfound = s->Search(h1,sigma,"",thresh);
        const int Npulser = nfound;
        cout << "Found " << nfound << " pulser peaks in strip " << i << " pulser walk spectrum.\n";

        if (nfound < 3){
            cout << "WARNING: Insufficient number of pulser peaks to find offset for strip " << i << "; skipping...\n";
            os[i] = 0.0;
            gain[i] = 1.0;
            h1->Delete();
        }

        else{
            Float_t *xpeaks = s->GetPositionX();
            Int_t index[npoints];
            TMath::Sort(npoints,xpeaks,index,0);

            TGraph *grp = new TGraphErrors(npoints);

            for (Int_t p=0; p<npoints; p++){
                grp->SetPoint(p,p,xpeaks[index[p]]);
                // grp->SetPointError(p,0,xsigma[index[p]]);


            }

            // new TCanvas();
            // grp->SetMarkerStyle(21);
            // grp->Draw("AP");
            TFitResultPtr fit1 = grp->Fit("pol1","QS");
            os[i]              = fit1->Parameter(0);
            Double_t m1        = fit1->Parameter(1);

            // find maximum INL
            nl[i] = 0;
            for(Int_t j = 0; j < 3; ++j){
                V_line = (grp->GetPoint(j,1) - os[i]) / m1;
                Double_t temp = 100.0*TMath::Abs(V[j] - V_line) / V_line;
                if(temp > nl[i]) nl[i] = temp;
            }

            h1->Delete();
        }

    }

    f1->Close();


    // scale gains to min gain channel
    cout << "Channel" << "\t\t" << "Offset" << "\t\t" << "Gain" << "\t\t" << "INL" << "\n";
    cout << "=======" << "\t\t" << "======" << "\t\t" << "====" << "\t\t" << "===" << "\n";

    for(Int_t i=0; i < MAX_CHANNELS; ++i){
        if( i == min_chan){
            gain[i] = 1.0;
        }
        else{
            gain[i] = gain_min/gain[i];
        }
        cout << i << "\t\t" << os[i] << "\t\t" << gain[i] << "\t\t" << nl[i] << "\n";
    }

    // ===== pulser data ===== //
    // find offset via linear fit of pulser data and set tree values
    for(Int_t j=0;j<3;j++){
        grp->SetPoint(j,V[j],pulse[i][j]);
        grp->SetPointError(j,0,up[i][j]);
    }

    TFitResultPtr fit1 = grp->Fit("pol1","QS");
    os[i] = fit1->Parameter(0);
    m1 = fit1->Parameter(1);


    cout << ch[i] << "\t\t" << os[i] << "\t\t" << nl[i] << "\n";

    // ===== source data ===== //
    // find gain via linear fit of source data and set tree values
    if(ch[i] >= 16 && ch[i] <= 25){// PIPS detectors
        for(Int_t j=0; j < 3; j++){
            gra->SetPoint(j,E_peak2[j],alpha[i][j]-os[i]);
            gra->SetPointError(j,0,ua[i][j]);
        }
    }
    else{
        for(Int_t j=0; j < 3; j++){// all other detectors
            gra->SetPoint(j,E_peak1[j],alpha[i][j]-os[i]);
            gra->SetPointError(j,0,ua[i][j]);
        }
    }

    TFitResultPtr fit2 = gra->Fit("pol1","QS");
    b2       = fit2->Parameter(0);
    m2       = fit2->Parameter(1);
    g[i]     = m2;

    // find min gain channel
    if(g[i] < gain_min){
        gain_min = g[i];
        min_chan = ch[i];
        c[0]     = 1/gain_min;
    }

    // scale all gains to min gain channel
    if(i == N_STRIPS-1){
        cout << "Min gain channel = " << min_chan << "\n";

        // scale gains to min gain channel
        cout << "Channel" << "\t\t" << "Offset" << "\t\t" << "Gain" << "\n";
        cout << "=======" << "\t\t" << "======" << "\t\t" << "====" << "\n";

        for(Int_t j=0; j < N_STRIPS; ++j){
            if( ch[j] == min_chan){
                g[j]     = 1.0;
            }
            else{
                g[j]     = gain_min/g[j];
            }
            gain[j] = g[j];
            cout << ch[j] << "\t\t" << os[j] << "\t\t" << g[j] << "\n";
        }
    }

    // write graphs
    sprintf(pname,"ch_%i_pulser",ch[i]);
    sprintf(aname,"ch_%i_alpha",ch[i]);
    grp->Write(pname);
    gra->Write(aname);

    // fill TVectors
    channel[i] = ch[i];
    offset[i]  = os[i];
    INL[i]     = nl[i];

    // fill tree
    for(Int_t i = 0; i < N_STRIPS; ++i){

        for(Int_t j=0;j<3;j++){
            gm->pulser[j] = pulse[i][j];
            gm->u_p[j]    = up[i][j];
            gm->alpha[j]  = alpha[i][j];
            gm->u_a[j]    = ua[i][j];
        }

        gm->offset    = os[i];
        gm->gain      = g[i];
        gm->INL       = nl[i];
        // fill tree
        tg->Fill();
    }

    grp->Delete();
    gra->Delete();
    in.close();

    // // Write slopes and offsets to odb
    //	for(Int_t i=0; i< adc.size(); ++i) {
    //		gSystem->Exec(Form("odbedit -c \"set /sonik/variables/adc/slope[%d] %.6g\"\n", ch[i], g[i] ) );
    //		gSystem->Exec(Form("odbedit -c \"set /sonik/variables/adc/offset[%d] %.6g\"\n", ch[i], -os[i]) );
    //		gSystem->Exec(Form("odbedit -c \"set /dragon/dsssd/variables/adc/slope[%d] %.6g\"\n", ch[i], g[i] ) );
    //		gSystem->Exec(Form("odbedit -c \"set /dragon/dsssd/variables/adc/offset[%d] %.6g\"\n", ch[i], -os[i] ) );
    //	}
    //	cout << "ATTENTION: Gains and offsets written to odb!\n";

    // // Save current odb state to xml file
    //	gSystem->Exec("odbedit -c 'save -x SONIK_Ecal.xml'"); // save calibration as xml file in pwd

    // write TVectors
    c.Write("c");
    offset.Write("offset");
    gain.Write("gain");
    INL.Write("INL");

    // Write root file

    f0->Write();

}
