#include <TFile.h>
#include <TH1.h>
#include <TH2.h>


void dsssd_draw(const char *fname="dsssdcal.root"){

	TFile *f=TFile::Open(fname);

	TCanvas *c0=new TCanvas();
	h0->Draw("colz");
	h0->GetXaxis()->SetRangeUser(0,32);
	c0->SaveAs("${DH}/../dsssdcal/DSSSD_Summary_uncal.pdf");

	TCanvas *c1=new TCanvas();
	h2->Draw();
	h2->SetFillColor(2011);
	h2->GetXaxis()->SetTitle("Channel");
	h2->GetYaxis()->SetTitle("Counts / Channel");
	h2->GetXaxis()->CenterTitle();
	h2->GetYaxis()->CenterTitle();
	c1->Modified();
	c1->Update();
	c1->SaveAs("${DH}/../dsssdcal/DSSSD_front_uncal.pdf");

	// Draw calibrated summary spectrum & total energy spectrum
	TCanvas *c2=new TCanvas();
	// dsssd_cal->GetYaxis()->SetRangeUser(0,8);
	dsssd_cal->Draw("colz");
	c2->SaveAs("${DH}/../dsssdcal/DSSSD_Summary_cal.pdf");

	TH1D *py=dsssd_cal->ProjectionY("DSSSD Energy",0,8);
	TCanvas *c3=new TCanvas();
	py->GetXaxis()->SetTitle("Energy [MeV]");
	py->GetYaxis()->SetTitle("Counts / 5 keV");
	py->GetXaxis()->CenterTitle();
	py->GetYaxis()->CenterTitle();
	py->SetFillColor(2011);
	py->Draw();
	py->GetXaxis()->SetRangeUser(4.5,6);
	c3->Modified();
	c3->Update();
	c3->SaveAs("${DH}/../dsssdcal/DSSSD_front_cal.pdf");

}
