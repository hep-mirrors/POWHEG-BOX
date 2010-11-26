#include <iostream>
#include "fastjet/ClusterSequence.hh"
#include "fastjet/CDFJetCluPlugin.hh"

namespace fj = fastjet;
using namespace std;

extern "C" {   

// f77 interface to CDF JetClu (cone) algorithm (via fastjet)
//
// Corresponds to the following Fortran subroutine
// interface structure:
//
//   SUBROUTINE FASTJETCDFJETCLU(P,NPART,R,ETMIN,F,F77JETS,NJETS)
//   DOUBLE PRECISION P(4,*), R, ETMIN, F, F77JETS(4,*)
//   INTEGER          NPART, NJETS
// 
// where on input
//
//   P        the input particle 4-momenta
//   NPART    the number of input momenta
//   R        the radius parameter
//   ETMIN    the minimum E_T
//   F        the overlap fraction, usually 0.75 in D0 
//
// and on output 
//
//   F77JETS  the output jet momenta (whose second dim should be >= NPART)
//            sorted in order of decreasing p_t.
//   NJETS    the number of output jets 
//
void fastjetcdfjetclu_(const double * p, const int & npart,                   
                     const double & R, const double & Etmin, const double & f,                  
                     double * f77jets, int & njets) {

    // transfer p[4*ipart+0..3] -> input_particles[i]
    vector<fj::PseudoJet> input_particles;   
    for (int i=0; i<npart; i++) {
      valarray<double> mom(4); // mom[0..3]
      for (int j=0;j<=3; j++) {
         mom[j] = *(p++);
      }
      fj::PseudoJet psjet(mom);
      input_particles.push_back(psjet);    
    }
    
    // prepare jet def and run fastjet
    fj::CDFJetCluPlugin * plugin = new fj::CDFJetCluPlugin(R,f,Etmin);
    fj::JetDefinition jet_def(plugin);
    
    // perform clustering
    fj::ClusterSequence cs(input_particles,jet_def);
    // extract jets (pt-ordered)
    vector<fj::PseudoJet> jets = sorted_by_pt(cs.inclusive_jets());
    njets = jets.size();

    // transfer jets -> f77jets[4*ijet+0..3]
    for (int i=0; i<njets; i++) {
      for (int j=0;j<=3; j++) {
        *f77jets = jets[i][j];
        f77jets++;
      } 
    }

    // clean up
    delete plugin;
    
   }
}

