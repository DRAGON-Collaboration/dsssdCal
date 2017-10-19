#include <sstream>
void dsssd(){

	
  const char *abfile="/data2/dragon/S1318/data/rootfiles/run4617.root";
  TFile *file_ab=TFile::Open(abfile);

	stringstream ss[32];

  gStyle->SetOptStat(0);
	TCanvas* c1 = new TCanvas("c1", "Canvas", 1600, 900);
	c1->Divide(2,2);
  c1->ToggleEditor();
  c1->ToggleToolBar();
  c1->ToggleEventStatus();

	c1->cd(1);
	for (Int_t i=0; i<16; i++) {
		cout << i << endl;
		ss[i] << "dsssd.ecal[" << i << "]:tof.mcp>>dsssd_att(201,30,50,512,0,4096)";
		t3->Draw(ss[i].str().data(),"","same");
		dsssd_att->SetMarkerSize(0.5);
		dsssd_att->SetMarkerColor(14);
		dsssd_att->SetMarkerStyle(8);
		dsssd_att->Draw("P");
		c1->Update();
	}

/*
	c1->cd(2);
	t3->Draw("dsssd.efront:xtofh>>ab_xtof(512,0,4096,-10001,-10000,10000)","","goff");
	ab_xtof->SetLineColor(14);
	ab_xtof->SetFillColor(17);
	ab_xtof->Draw();
*/
/*
	int firstRun=4619, lastRun=4620;
	vector<int> runs;

  for (int i = firstRun; i<=lastRun; i++){
		runs.push_back(i);
  }

  dragon::MakeChains(runs);

	t5->SetAlias("t","tail");
	t5->SetAlias("h","head");

/*
	//===== Cuts =====\\
//	TCut haveICs  = "ic.anode[0]>350 && ic.anode[1]>350";
  TCut haveDsssD = "dsssd.efront>500";
	TCut haveMCPs = "tof.mcp>34 && tof.mcp<44";
	TCut haveMCPc = "t.tof.mcp>34 && t.tof.mcp<50";
	TCut MCPccut  = "t.tof.mcp>41 && t.tof.mcp<48";
	TCut haveBGO  = "h.bgo.esort[0]>1 && h.bgo.esort[0]<14";
	TCut BGOcut   = "h.bgo.esort[0]>4 && h.bgo.esort[0]<13";

	c1->cd(1);
	t3->SetMarkerStyle(3);
	t3->SetMarkerColor(1);
	t3->Draw("dsssd.ecal:tof.mcp>>h1(201,30,50,512,0,4096)","","colz,same");
	t3->SetMarkerSize(0.5);
	t3->SetMarkerColor(2);
/*
	t5->Draw("t.dsssd.efront:t.tof.mcp>>h2(201,30,50,512,0,4096)",haveMCPc && haveBGO,"same");
	t5->SetMarkerStyle(3);
	t5->SetMarkerColor(3);
	t5->Draw("t.dsssd.efront:t.tof.mcp>>h3(201,30,50,512,0,4096)",haveMCPc && BGOcut,"same");
/*
	c1->cd(2);
	c1.cd(2).SetLogy(1);
	t3->SetLineColor(1);
	t3->Draw("tof.mcp>>mcps(141,34,48)","","same");
	t5->SetLineColor(4);
	t5->Draw("t.tof.mcp>>mcptof(141,34,48)","","same");
	t5->SetLineColor(2);
	t5->Draw("t.tof.mcp>>mcptofc(141,34,48)",haveMCPc && haveBGO,"same");
	t5->SetLineColor(3);
	t5->Draw("t.tof.mcp>>mcpc(141,34,48)",haveMCPc && BGOcut,"same");

	c1->cd(3);
	t5->SetMarkerStyle(8);
	t5->SetMarkerColor(14);
	t5->Draw("t.tof.mcp:xtofh>>tof_tof(10001,-10000,10000,1001,0,100)","","");
	t5->SetMarkerSize(0.5);
	t5->SetMarkerColor(2);
	t5->Draw("t.tof.mcp:xtofh>>tof_tof(10001,-10000,10000,1001,0,100)",haveMCPc && haveBGO,"same");
	t5->SetMarkerStyle(23);
	t5->SetMarkerSize(0.75);
	t5->SetMarkerColor(3);
	t5->Draw("t.tof.mcp:xtofh>>tof_tof(10001,-10000,10000,1001,0,100)",haveMCPc && BGOcut,"same");

	c1->cd(4);
	t5->SetLineColor(4);
	t5->Draw("h.bgo.esort[0]>>E0(201,0,16)",haveBGO && haveMCPc,"");
	t5->SetLineColor(3);
	t5->Draw("h.bgo.esort[0]>>E0cut(201,0,16)",haveMCPc && BGOcut,"same");

	TCanvas* c3 = new TCanvas();
	c3->Divide(1,2);
  c3->ToggleEditor();
  c3->ToggleToolBar();
  c3->ToggleEventStatus();

	t5->SetAlias("ic_sum","tail.ic.anode[0]+tail.ic.anode[1]+tail.ic.anode[2]");

	c3->cd(1);
	t5->SetMarkerStyle(3);
	t5->SetMarkerColor(1);
	t5->Draw("t.ic.anode[0]:ic_sum>>ic2_0(512,0,4096)","","colz");
	t5->SetMarkerSize(0.5);
	t5->SetMarkerColor(2);
	t5->Draw("t.ic.anode[0]:ic_sum>>ic2_0_1(512,0,4096)",haveMCPc && haveBGO,"same");
	t5->SetMarkerStyle(3);
	t5->SetMarkerColor(3);
	t5->Draw("t.ic.anode[0]:ic_sum>>ic2_0_2(512,0,4096)",haveMCPc && "h.bgo.sum>5 && xtofh>0 && xtofh<6000","same");

	c3->cd(2);
	t5->SetMarkerStyle(3);
	t5->SetMarkerColor(1);
	t5->Draw("t.ic.anode[0]:t.tof.mcp>>ic_mcp(1001,0,100,512,0,4096)","","colz");
	t5->SetMarkerSize(0.5);
	t5->SetMarkerColor(2);
	t5->Draw("t.ic.anode[0]:t.tof.mcp>>ic1_mcp(1001,0,100,512,0,4096)",haveMCPc && haveBGO,"same");
	t5->SetMarkerStyle(3);
	t5->SetMarkerColor(3);
	t5->Draw("t.ic.anode[0]:t.tof.mcp>>ic2_mcp(1001,0,100,512,0,4096)",haveMCPc && "h.bgo.sum>5 && xtofh>0 && xtofh<6000","same");
*/

}
