
void anal(){

   for (Int_t k=0; k<nentries; ++k){
     fChain->GetEntry(k);

     double mass(0.105658367);

     TLorentzVector muon1,muon2,zbos;

     // muon- -> muon1, muon+ -> muon2
     muon1.SetPxPyPzE(px1,py1,pz1,e1);
     muon2.SetPxPyPzE(px2,py2,pz2,e2);

     zbos.SetPxPyPzE(pxz,pyz,pzz,ez);
     double zmass = zbos.M();

     TLorentzVector dimuon = muon1+muon2;
     double  bpt  = dimuon.Pt();
     double  zy   = dimuon.Rapidity();

     double eta1 = muon1.Eta();
     double eta2 = muon2.Eta();

     double result[4];
     calCSVariables(muon1,muon2,result,0);

     double theta  = TMath::ACos(result[0]);
     double lcos   = result[0];
     double genphi = result[3];

     //==========================================================
     // <m0> = <(1/2)*(1-3cos^2(tjeta))> = (3/20)*(A0 - (2/3))
     // <m1> = <sin(2*theta)*cos(phi)> = (1/5)*A1
     // <m2> = <sin^2(theta)*cos(2*phi)> = (1/10)*A2
     // <m3> = <sin(theta)*cos(phi)> = (1/4)*A3
     // <m4> = <cos(theta)> - (1/4)*A4
     // <m5> = <sin^2(theta)*sin(2*phi)> = (1/5)*A5
     // <m6> = <sin(2*theta)*sin(phi)> = (1/5)*A6
     // <m7> = <sin(theta)*sin(phi)> = (1/4)*A7
     //===========================================================

     double mom0 = 0.5*(1.0-3.0*lcos*lcos);
     double mom1 = sin(2.0*theta)*cos(genphi);
     double mom2 = sin(theta)*sin(theta)*cos(2.0*genphi);
     double mom3 = sin(theta)*cos(genphi);
     double mom4 = lcos;
     double mom5 = sin(theta)*sin(theta)*sin(2.0*genphi);
     double mom6 = sin(2.0*theta)*sin(genphi);
     double mom7 = sin(theta)*sin(genphi);

     double a0   = (20.0/3.0)*mom0 + (2.0/3.0);
     double a1   = 5.0*mom1;
     double a2   = 10.0*mom2;
     double a3   = 4.0*mom3;
     double a4   = 4.0*mom4;
     double a5   = 5.0*mom5;
     double a6   = 5.0*mom6;
     double a7   = 4.0*mom7;

   }
}

/**************************************************************************
  *
  * for Z analysis
  *
  **************************************************************************/
// calculate the Colins-Soper variables;
// everything is in the lab frame


#include "TVector3.h"
#include "TLorentzVector.h"

#include "math.h"

#define NUM_FLAVORS 13
#define PDG_Z_MASS  91.19
#define PDG_W_MASS  80.40
#define CM_ENERGY   1960.0

void calCSVariables(const TLorentzVector& mu,
		    const TLorentzVector& mubar,
		    double* res, bool swap) {

   // convention. beam direction is on the positive Z direction.
   // beam contains quark flux.
   TLorentzVector Pbeam  (0, 0,  CM_ENERGY/2.0, CM_ENERGY/2.0);
   TLorentzVector Ptarget(0, 0, -CM_ENERGY/2.0, CM_ENERGY/2.0);

   TLorentzVector Q(mu+mubar);
   /************************************************************************
    *
    * 1) cos(theta) = 2 Q^-1 (Q^2+Qt^2)^-1 (mu^+ mubar^- - mu^- mubar^+)
    *
    *
    ************************************************************************/
   double muplus  = 1.0/sqrt(2.0) * (mu.E() + mu.Z());
   double muminus = 1.0/sqrt(2.0) * (mu.E() - mu.Z());

   double mubarplus  = 1.0/sqrt(2.0) * (mubar.E() + mubar.Z());
   double mubarminus = 1.0/sqrt(2.0) * (mubar.E() - mubar.Z());

   double costheta = 2.0 / Q.Mag() / sqrt(pow(Q.Mag(), 2) + 
pow(Q.Pt(), 2)) * (muplus * mubarminus - muminus * mubarplus);
   if (swap) costheta = -costheta;

   /************************************************************************
    *
    * 2) sin2(theta) = Q^-2 Dt^2 - Q^-2 (Q^2 + Qt^2)^-1 * (Dt dot Qt)^2
    *
    ************************************************************************/
   TLorentzVector D(mu-mubar);
   double dt_qt = D.X()*Q.X() + D.Y()*Q.Y();
   double sin2theta = pow(D.Pt()/Q.Mag(), 2)
     - 1.0/pow(Q.Mag(), 2)/(pow(Q.Mag(), 2) + pow(Q.Pt(), 2))*pow(dt_qt, 2);


   /************************************************************************
    *
    * 3) tanphi = (Q^2 + Qt^2)^1/2 / Q (Dt dot R unit) /(Dt dot Qt unit)
    *
    ************************************************************************/
   // unit vector on R direction
   TVector3 R = Pbeam.Vect().Cross(Q.Vect());
   TVector3 Runit = R.Unit();

   // unit vector on Qt
   TVector3 Qt = Q.Vect(); Qt.SetZ(0);
   TVector3 Qtunit = Qt.Unit();

   TVector3 Dt = D.Vect(); Dt.SetZ(0);
   double tanphi = sqrt(pow(Q.Mag(), 2) + pow(Q.Pt(), 2)) / Q.Mag() * 
Dt.Dot(Runit) / Dt.Dot(Qtunit);
   if (swap) tanphi = -tanphi;

   double phi = 
TMath::ATan2(sqrt(pow(Q.Mag(),2)+pow(Q.Pt(),2))*Dt.Dot(Runit),Q.Mag()*Dt.Dot(Qtunit));
   //if (swap) phi = 
TMath::ATan2(sqrt(pow(Q.Mag(),2)+pow(Q.Pt(),2))*Dt.Dot(Runit),-1.*Q.Mag()*Dt.Dot(Qtunit));
   if (swap) phi = 
TMath::ATan2(-1.*sqrt(pow(Q.Mag(),2)+pow(Q.Pt(),2))*Dt.Dot(Runit),Q.Mag()*Dt.Dot(Qtunit));

   res[0] = costheta;
   res[1] = sin2theta;
   res[2] = tanphi;
   res[3] = phi;

}
