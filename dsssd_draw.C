#include <TFile.h>
#include <TH1.h>
#include <TH2.h>


void dsssd_draw(const char *fname="dsssdcal.root"){

	TFile *f=TFile::Open(fname);

	TCanvas *c0=new TCanvas();
	h0->Draw();
	TCanvas *c1=new TCanvas();
	h2->Draw();

	// Draw calibrated summary spectrum & total energy spectrum
	TCanvas *c2=new TCanvas();
	dsssd_cal->GetYaxis()->SetRangeUser(0,8);
 	dsssd_cal->Draw("colz");

	TH1D *py=dsssd_cal->ProjectionY("DSSSD Energy",0,8);
	TCanvas *c3=new TCanvas();
	py->Draw();


}
