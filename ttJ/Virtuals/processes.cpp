// $Modified: Tue Feb  6 18:15:36 2007 by puwer $
#include "processes.h"
#include "EvalSMExx.h"
#include "SquaredMatrixElements.h"
#include "StandardModelParameters.h"
#include "string.h"

#define USE_MEMSET

using namespace std;


static StandardModelParameters& parms = StandardModelParameters::instance();

ggttg::ggttg() {

#ifdef USE_MEMSET
  memset(ampLO, 0, sizeof(ampLO));
#else
  for (unsigned int color = 1; color < 7; color++) {
    for (unsigned int ispin = 0; ispin < 32; ispin++) {
      ampLO[color][ispin] = 0.;
    } 
  }
#endif


  particles.push_back(GLUON);
  particles.push_back(GLUON);
  particles.push_back(TOP);
  particles.push_back(ATOP);
  particles.push_back(GLUON);
  
  ggcorrelations(cij[0][0].c, // we put lo-matrix in cij[0][0], 
		 cij[0][1].c, //cg1g2 
		 cij[2][3].c, //cqqb,
		 cij[0][4].c, //cg1g3, 
		 cij[1][4].c, //cg2g3, 
		 cij[0][2].c, //cg1q,
		 cij[1][2].c, //cg2q, 
		 cij[4][2].c, //cg3q, 
		 cij[0][3].c, //cg1qb,
		 cij[1][3].c, //cg2qb, 
		 cij[4][3].c  //cg3qb
		 );
  
  SYMMETRIZE(0,1);
  SYMMETRIZE(2,3);
  SYMMETRIZE(0,4);
  SYMMETRIZE(1,4);
  SYMMETRIZE(0,2);
  SYMMETRIZE(1,2);
  SYMMETRIZE(4,2);
  SYMMETRIZE(0,3);
  SYMMETRIZE(1,3);
  SYMMETRIZE(4,3);
  
} // Constructor

void ggttg::EvalfAmplitude(const vector<FourMomentum> & pset){

    
    const FourMomentum & p1  =  pset[0];
    const FourMomentum & p2  =  pset[1];
    const FourMomentum   p3  = -pset[4];
    const FourMomentum & kq  =  pset[2];
    const FourMomentum & kqb =  pset[3];
    
    FourMomentum q1, q2, r1, r2;  
    evalqr(kq,kqb,q1,q2,r1,r2);

    double f77p1[4],f77p2[4],f77p3[4],f77q1[4],f77q2[4],f77r1[4],f77r2[4];  

    p1.getF77Array(f77p1);
    p2.getF77Array(f77p2);
    p3.getF77Array(f77p3);
    q1.getF77Array(f77q1);
    q2.getF77Array(f77q2);
    r1.getF77Array(f77r1);
    r2.getF77Array(f77r2);


    complex<double> ammm,ammp,ampm,ampp,apmm,apmp,appm,appp;

    // 1
    gggtthel_(f77p1,f77p2,f77p3,f77q1,f77q2,f77r1,f77r2,
	     ammm,ammp,ampm,ampp,apmm,apmp,appm,appp);

    /* 
     * For the outgoing gluon the labelling is wrong:
     */
#define FLIP(__XX__) ( ( __XX__ == MINUS) ? PLUS : MINUS )
    ampLO[1][PolKey(MINUS, MINUS, PLUS, PLUS, FLIP(MINUS))] = ammm;
    ampLO[1][PolKey(MINUS, MINUS, PLUS, PLUS,  FLIP(PLUS))] = ammp;
    ampLO[1][PolKey(MINUS,  PLUS, PLUS, PLUS, FLIP(MINUS))] = ampm;
    ampLO[1][PolKey(MINUS,  PLUS, PLUS, PLUS,  FLIP(PLUS))] = ampp;
    ampLO[1][PolKey( PLUS, MINUS, PLUS, PLUS, FLIP(MINUS))] = apmm;
    ampLO[1][PolKey( PLUS, MINUS, PLUS, PLUS,  FLIP(PLUS))] = apmp;
    ampLO[1][PolKey( PLUS,  PLUS, PLUS, PLUS, FLIP(MINUS))] = appm;
    ampLO[1][PolKey( PLUS,  PLUS, PLUS, PLUS,  FLIP(PLUS))] = appp;

    // 2
    gggtthel_(f77p1,f77p3,f77p2,f77q1,f77q2,f77r1,f77r2,
	      ammm,ammp,ampm,ampp,apmm,apmp,appm,appp);

    ampLO[2][PolKey(MINUS, MINUS, PLUS, PLUS, FLIP(MINUS))] = ammm;
    ampLO[2][PolKey(MINUS, MINUS, PLUS, PLUS,  FLIP(PLUS))] = ampm;
    ampLO[2][PolKey(MINUS,  PLUS, PLUS, PLUS, FLIP(MINUS))] = ammp;
    ampLO[2][PolKey(MINUS,  PLUS, PLUS, PLUS,  FLIP(PLUS))] = ampp;
    ampLO[2][PolKey( PLUS, MINUS, PLUS, PLUS, FLIP(MINUS))] = apmm;
    ampLO[2][PolKey( PLUS, MINUS, PLUS, PLUS,  FLIP(PLUS))] = appm;
    ampLO[2][PolKey( PLUS,  PLUS, PLUS, PLUS, FLIP(MINUS))] = apmp;
    ampLO[2][PolKey( PLUS,  PLUS, PLUS, PLUS,  FLIP(PLUS))] = appp;

    // 3
    gggtthel_(f77p2,f77p1,f77p3,f77q1,f77q2,f77r1,f77r2,
	     ammm,ammp,ampm,ampp,apmm,apmp,appm,appp);
    
    ampLO[3][PolKey(MINUS, MINUS, PLUS, PLUS, FLIP(MINUS))] = ammm;
    ampLO[3][PolKey(MINUS, MINUS, PLUS, PLUS,  FLIP(PLUS))] = ammp;
    ampLO[3][PolKey(MINUS,  PLUS, PLUS, PLUS, FLIP(MINUS))] = apmm;
    ampLO[3][PolKey(MINUS,  PLUS, PLUS, PLUS,  FLIP(PLUS))] = apmp;
    ampLO[3][PolKey( PLUS, MINUS, PLUS, PLUS, FLIP(MINUS))] = ampm;
    ampLO[3][PolKey( PLUS, MINUS, PLUS, PLUS,  FLIP(PLUS))] = ampp;
    ampLO[3][PolKey( PLUS,  PLUS, PLUS, PLUS, FLIP(MINUS))] = appm;
    ampLO[3][PolKey( PLUS,  PLUS, PLUS, PLUS,  FLIP(PLUS))] = appp;

    // 4
    gggtthel_(f77p2,f77p3,f77p1,f77q1,f77q2,f77r1,f77r2,
	      ammm,ammp,ampm,ampp,apmm,apmp,appm,appp);
    
    ampLO[4][PolKey(MINUS, MINUS, PLUS, PLUS, FLIP(MINUS))] = ammm;
    ampLO[4][PolKey(MINUS, MINUS, PLUS, PLUS,  FLIP(PLUS))] = ampm;
    ampLO[4][PolKey(MINUS,  PLUS, PLUS, PLUS, FLIP(MINUS))] = apmm;
    ampLO[4][PolKey(MINUS,  PLUS, PLUS, PLUS,  FLIP(PLUS))] = appm;
    ampLO[4][PolKey( PLUS, MINUS, PLUS, PLUS, FLIP(MINUS))] = ammp;
    ampLO[4][PolKey( PLUS, MINUS, PLUS, PLUS,  FLIP(PLUS))] = ampp;
    ampLO[4][PolKey( PLUS,  PLUS, PLUS, PLUS, FLIP(MINUS))] = apmp;
    ampLO[4][PolKey( PLUS,  PLUS, PLUS, PLUS,  FLIP(PLUS))] = appp;

    // 5
    gggtthel_(f77p3,f77p1,f77p2,f77q1,f77q2,f77r1,f77r2,
	     ammm,ammp,ampm,ampp,apmm,apmp,appm,appp);

    ampLO[5][PolKey(MINUS, MINUS, PLUS, PLUS, FLIP(MINUS))] = ammm;
    ampLO[5][PolKey(MINUS, MINUS, PLUS, PLUS,  FLIP(PLUS))] = apmm;
    ampLO[5][PolKey(MINUS,  PLUS, PLUS, PLUS, FLIP(MINUS))] = ammp;
    ampLO[5][PolKey(MINUS,  PLUS, PLUS, PLUS,  FLIP(PLUS))] = apmp;
    ampLO[5][PolKey( PLUS, MINUS, PLUS, PLUS, FLIP(MINUS))] = ampm;
    ampLO[5][PolKey( PLUS, MINUS, PLUS, PLUS,  FLIP(PLUS))] = appm;
    ampLO[5][PolKey( PLUS,  PLUS, PLUS, PLUS, FLIP(MINUS))] = ampp;
    ampLO[5][PolKey( PLUS,  PLUS, PLUS, PLUS,  FLIP(PLUS))] = appp;

    // 6
    gggtthel_(f77p3,f77p2,f77p1,f77q1,f77q2,f77r1,f77r2,
	      ammm,ammp,ampm,ampp,apmm,apmp,appm,appp);

    ampLO[6][PolKey(MINUS, MINUS, PLUS, PLUS, FLIP(MINUS))] = ammm;
    ampLO[6][PolKey(MINUS, MINUS, PLUS, PLUS,  FLIP(PLUS))] = apmm;
    ampLO[6][PolKey(MINUS,  PLUS, PLUS, PLUS, FLIP(MINUS))] = ampm;
    ampLO[6][PolKey(MINUS,  PLUS, PLUS, PLUS,  FLIP(PLUS))] = appm;
    ampLO[6][PolKey( PLUS, MINUS, PLUS, PLUS, FLIP(MINUS))] = ammp;
    ampLO[6][PolKey( PLUS, MINUS, PLUS, PLUS,  FLIP(PLUS))] = apmp;
    ampLO[6][PolKey( PLUS,  PLUS, PLUS, PLUS, FLIP(MINUS))] = ampp;
    ampLO[6][PolKey( PLUS,  PLUS, PLUS, PLUS,  FLIP(PLUS))] = appp;



    // 1
    gggtthel_(f77p1,f77p2,f77p3,f77q1,f77q2,f77r2,f77r1,
	     ammm,ammp,ampm,ampp,apmm,apmp,appm,appp);

    ampLO[1][PolKey(MINUS, MINUS, PLUS, MINUS, FLIP(MINUS))] = ammm;
    ampLO[1][PolKey(MINUS, MINUS, PLUS, MINUS,  FLIP(PLUS))] = ammp;
    ampLO[1][PolKey(MINUS,  PLUS, PLUS, MINUS, FLIP(MINUS))] = ampm;
    ampLO[1][PolKey(MINUS,  PLUS, PLUS, MINUS,  FLIP(PLUS))] = ampp;
    ampLO[1][PolKey( PLUS, MINUS, PLUS, MINUS, FLIP(MINUS))] = apmm;
    ampLO[1][PolKey( PLUS, MINUS, PLUS, MINUS,  FLIP(PLUS))] = apmp;
    ampLO[1][PolKey( PLUS,  PLUS, PLUS, MINUS, FLIP(MINUS))] = appm;
    ampLO[1][PolKey( PLUS,  PLUS, PLUS, MINUS,  FLIP(PLUS))] = appp;

    // 2
    gggtthel_(f77p1,f77p3,f77p2,f77q1,f77q2,f77r2,f77r1,
	      ammm,ammp,ampm,ampp,apmm,apmp,appm,appp);

    ampLO[2][PolKey(MINUS, MINUS, PLUS, MINUS, FLIP(MINUS))] = ammm;
    ampLO[2][PolKey(MINUS, MINUS, PLUS, MINUS,  FLIP(PLUS))] = ampm;
    ampLO[2][PolKey(MINUS,  PLUS, PLUS, MINUS, FLIP(MINUS))] = ammp;
    ampLO[2][PolKey(MINUS,  PLUS, PLUS, MINUS,  FLIP(PLUS))] = ampp;
    ampLO[2][PolKey( PLUS, MINUS, PLUS, MINUS, FLIP(MINUS))] = apmm;
    ampLO[2][PolKey( PLUS, MINUS, PLUS, MINUS,  FLIP(PLUS))] = appm;
    ampLO[2][PolKey( PLUS,  PLUS, PLUS, MINUS, FLIP(MINUS))] = apmp;
    ampLO[2][PolKey( PLUS,  PLUS, PLUS, MINUS,  FLIP(PLUS))] = appp;

    // 3
    gggtthel_(f77p2,f77p1,f77p3,f77q1,f77q2,f77r2,f77r1,
	     ammm,ammp,ampm,ampp,apmm,apmp,appm,appp);
    
    ampLO[3][PolKey(MINUS, MINUS, PLUS, MINUS, FLIP(MINUS))] = ammm;
    ampLO[3][PolKey(MINUS, MINUS, PLUS, MINUS,  FLIP(PLUS))] = ammp;
    ampLO[3][PolKey(MINUS,  PLUS, PLUS, MINUS, FLIP(MINUS))] = apmm;
    ampLO[3][PolKey(MINUS,  PLUS, PLUS, MINUS,  FLIP(PLUS))] = apmp;
    ampLO[3][PolKey( PLUS, MINUS, PLUS, MINUS, FLIP(MINUS))] = ampm;
    ampLO[3][PolKey( PLUS, MINUS, PLUS, MINUS,  FLIP(PLUS))] = ampp;
    ampLO[3][PolKey( PLUS,  PLUS, PLUS, MINUS, FLIP(MINUS))] = appm;
    ampLO[3][PolKey( PLUS,  PLUS, PLUS, MINUS,  FLIP(PLUS))] = appp;

    // 4
    gggtthel_(f77p2,f77p3,f77p1,f77q1,f77q2,f77r2,f77r1,
	      ammm,ammp,ampm,ampp,apmm,apmp,appm,appp);
    
    ampLO[4][PolKey(MINUS, MINUS, PLUS, MINUS, FLIP(MINUS))] = ammm;
    ampLO[4][PolKey(MINUS, MINUS, PLUS, MINUS,  FLIP(PLUS))] = ampm;
    ampLO[4][PolKey(MINUS,  PLUS, PLUS, MINUS, FLIP(MINUS))] = apmm;
    ampLO[4][PolKey(MINUS,  PLUS, PLUS, MINUS,  FLIP(PLUS))] = appm;
    ampLO[4][PolKey( PLUS, MINUS, PLUS, MINUS, FLIP(MINUS))] = ammp;
    ampLO[4][PolKey( PLUS, MINUS, PLUS, MINUS,  FLIP(PLUS))] = ampp;
    ampLO[4][PolKey( PLUS,  PLUS, PLUS, MINUS, FLIP(MINUS))] = apmp;
    ampLO[4][PolKey( PLUS,  PLUS, PLUS, MINUS,  FLIP(PLUS))] = appp;

    // 5
    gggtthel_(f77p3,f77p1,f77p2,f77q1,f77q2,f77r2,f77r1,
	     ammm,ammp,ampm,ampp,apmm,apmp,appm,appp);

    ampLO[5][PolKey(MINUS, MINUS, PLUS, MINUS, FLIP(MINUS))] = ammm;
    ampLO[5][PolKey(MINUS, MINUS, PLUS, MINUS,  FLIP(PLUS))] = apmm;
    ampLO[5][PolKey(MINUS,  PLUS, PLUS, MINUS, FLIP(MINUS))] = ammp;
    ampLO[5][PolKey(MINUS,  PLUS, PLUS, MINUS,  FLIP(PLUS))] = apmp;
    ampLO[5][PolKey( PLUS, MINUS, PLUS, MINUS, FLIP(MINUS))] = ampm;
    ampLO[5][PolKey( PLUS, MINUS, PLUS, MINUS,  FLIP(PLUS))] = appm;
    ampLO[5][PolKey( PLUS,  PLUS, PLUS, MINUS, FLIP(MINUS))] = ampp;
    ampLO[5][PolKey( PLUS,  PLUS, PLUS, MINUS,  FLIP(PLUS))] = appp;

    // 6
    gggtthel_(f77p3,f77p2,f77p1,f77q1,f77q2,f77r2,f77r1,
	      ammm,ammp,ampm,ampp,apmm,apmp,appm,appp);

    ampLO[6][PolKey(MINUS, MINUS, PLUS, MINUS, FLIP(MINUS))] = ammm;
    ampLO[6][PolKey(MINUS, MINUS, PLUS, MINUS,  FLIP(PLUS))] = apmm;
    ampLO[6][PolKey(MINUS,  PLUS, PLUS, MINUS, FLIP(MINUS))] = ampm;
    ampLO[6][PolKey(MINUS,  PLUS, PLUS, MINUS, FLIP( PLUS))] = appm;
    ampLO[6][PolKey( PLUS, MINUS, PLUS, MINUS, FLIP(MINUS))] = ammp;
    ampLO[6][PolKey( PLUS, MINUS, PLUS, MINUS,  FLIP(PLUS))] = apmp;
    ampLO[6][PolKey( PLUS,  PLUS, PLUS, MINUS, FLIP(MINUS))] = ampp;
    ampLO[6][PolKey( PLUS,  PLUS, PLUS, MINUS,  FLIP(PLUS))] = appp;

  }

void ggttg::EvalfAmplitude2(const vector<FourMomentum> & pset){
    
    const FourMomentum & p1 = pset[0];
    const FourMomentum & p2 = pset[1];
    const FourMomentum & p3 = pset[4];
    const FourMomentum & kq = pset[2];
    const FourMomentum & kqb = pset[3];
    
    FourMomentum q1, q2, r1, r2; 

    complex<double> S[2][2][2][133];
    double f[7][133];  
    
    
    unsigned int ispin;

    evalqr(kq,kqb,q1,q2,r1,r2);
    EvalLOfgg(f, p1,p2,p3,kq,kqb);

    for (unsigned int tspin=MINUS; tspin<=MINUS;tspin++){
      for (unsigned int tbspin=MINUS; tbspin<=PLUS;tbspin++){
	/*
	 *  It is sufficient to sum only over half of the ttbar
	 * spin configurations, the second half is accounted by a factor
	 * of 2. This works due to the parity invaraince of QCD, which ensures
	 * that |T| = a + b s_t * s_tb .
	 */
	if ( ( tspin == MINUS ) &&  ( tbspin == MINUS ) ){
	  EvalSMEgg(S,p1, p2, p3,q1,q2,r1,r2);
	}
	
	if ( ( tspin == MINUS ) &&  ( tbspin == PLUS ) ){
	  EvalSMEgg(S,p1, p2, p3,q1,q2,r2,r1);
	}
	
	if ( ( tspin == PLUS ) &&  ( tbspin == MINUS ) ){
	  EvalSMEgg(S,p1, p2, p3,q2,q1,r1,r2);
	}
	
	if ( ( tspin == PLUS ) &&  ( tbspin == PLUS ) ){
	  EvalSMEgg(S,p1, p2, p3,q2,q1,r2,r1);
	}
	
	for (unsigned int pol0=MINUS; pol0 <= PLUS; pol0++){
	  for (unsigned int pol1=MINUS; pol1 <= PLUS; pol1++){
	    for (unsigned int pol4=MINUS; pol4 <= PLUS; pol4++){

	      ispin = PolKey(pol0, pol1, tspin, tbspin, FLIP(pol4));

	      for(int color=1;color < 7;color++){ 
		
		ampLO[color][ispin] = 0.;
		
		for(int i=1; i < 133; i++){
		  ampLO[color][ispin] += S[pol0][pol1][pol4][i] * f[color][i];
		}
		
		ampLO[color][PolKeyFlip(PolKeyFlip(ispin,2),3)] = 0;
	      
	      }

	    }
	  }
	}
      }
    }
  }


void ggttg::EvalfDipole(Dipole & d){
    
  EvalfAmplitude(d.momenta);
  
  complex<double> res(0.);
  //  d.value = 1.0; 
  //return;

  /*
   * Spin diagonal part:
   */
#define ORI
#ifdef ORI
  for ( unsigned int ispin = 0; ispin < 32; ispin++ ){
    for( unsigned int c1=1; c1 < 7; c1++){
      for( unsigned int c2=1; c2 < 7; c2++){
	res += d.Vdiag * conj(ampLO[c1][ispin]) 
	  * cij[d.spectator][d.emitter].c[c1][c2] 
	  * ampLO[c2][ispin];
      }
    }
  }
#else
  for ( unsigned int ispin = 0; ispin < 32; ispin++ ){
    for( unsigned int c1=1; c1 < 7; c1++){
	res += conj(ampLO[c1][ispin]) 
	  * cij[d.spectator][d.emitter].c[c1][c1] 
	  * ampLO[c1][ispin];
      for( unsigned int c2=c1+1; c2 < 7; c2++){
	res += 2.0 * conj(ampLO[c1][ispin]) 
	  * cij[d.spectator][d.emitter].c[c1][c2] 
	  * ampLO[c2][ispin];
      }
    }
  }
  res *= d.Vdiag;
#endif
#ifdef ORI
  /*
   * Spin correlations:
   */
  //	if ( d.Vpm != complex<double>(0.,0.) ) {   
  if ( d.emittertype == GLUON ) {   
    for ( unsigned int ispin = 0; ispin < 32; ispin++ ){
      for( unsigned int c1=1; c1 < 7; c1++){
	for( unsigned int c2=1; c2 < 7; c2++){
	  res +=  conj(ampLO[c1][PolKeyFlip(ispin,d.emitter)]) 
	    * cij[d.spectator][d.emitter].c[c1][c2] 
	    * ampLO[c2][ispin]
	    * ( Key2Pol(ispin,d.emitter) == MINUS ? (d.Vpm) : conj(d.Vpm) );
	}
      }
    }
  }
#else
  if ( d.emittertype == GLUON ) {   
    for ( unsigned int ispin = 0; ispin < 32; ispin++ ){
      complex<double> tmp(0.);
      for( unsigned int c1=1; c1 < 7; c1++){
	tmp +=  conj(ampLO[c1][PolKeyFlip(ispin,d.emitter)]) 
	  * cij[d.spectator][d.emitter].c[c1][c1] 
	  * ampLO[c1][ispin];
	for( unsigned int c2=c1+1; c2 < 7; c2++){
	  tmp +=  2.0 * conj(ampLO[c1][PolKeyFlip(ispin,d.emitter)]) 
	    * cij[d.spectator][d.emitter].c[c1][c2] 
	    * ampLO[c2][ispin];
	}
      }
      res += 
	tmp * ( Key2Pol(ispin,d.emitter) == MINUS ? (d.Vpm) : conj(d.Vpm) );
    }
  }
#endif
  // factor 2 too account for the ++,+- configuration of ttb.
  d.value = 2.0 * res.real() *  pow(parms.getAlphasNLO()*4.0*M_PI,3);
}

double ggttg::EvalfAmplitudeSquared(const vector<FourMomentum> & momenta){
    
  EvalfAmplitude(momenta);
  
  complex<double> res(0.);
  
  for ( unsigned int ispin = 0; ispin < 32; ispin++ ){
    for( unsigned int c1=1; c1 < 7; c1++){
      for( unsigned int c2=1; c2 < 7; c2++){
	/*
	 * Spin diagonal part:
	 */
	res += 
	  conj(ampLO[c1][ispin]) * ampLO[c2][ispin] * cij[0][0].c[c1][c2];
      }
    }
  }
  // factor 2 too account for the ++,+- configuration of ttb.
  return( 2.0 * res.real() *  pow(parms.getAlphasNLO()*4.0*M_PI,3) );
}

complex<double> ggttg::EvalfCorrelation(double KP[7][7]){

  complex<double> res(0.);

  /*
   * We make use of the symmetries of color matrices.
   */
  for ( unsigned int ispin = 0; ispin < 32; ispin++ ){
    for( unsigned int c1=1; c1 < 7; c1++){
	res += conj(ampLO[c1][ispin]) * KP[c1][c1] * ampLO[c1][ispin];
      for( unsigned int c2=c1+1; c2 < 7; c2++){
	res += 2.0 * conj(ampLO[c1][ispin]) * KP[c1][c2] * ampLO[c2][ispin];
      }
    }
  }

  return( 2.0 * res *  pow(parms.getAlphasNLO()*4.0*M_PI,3) );
}

qqttg::qqttg() {

#ifdef USE_MEMSET
    memset(ampLO, 0, sizeof(ampLO));
#else
    for ( unsigned int color=1; color < 5; color++) {
      for ( unsigned int ispin=0; ispin < 32; ispin++) {
	ampLO[color][ispin] = 0.;
      }
    }
#endif

    particles.push_back(UP);
    particles.push_back(AUP);
    particles.push_back(TOP);
    particles.push_back(ATOP);
    particles.push_back(GLUON);

    qqcorrelations(cij[0][0].c, // we put the lo matrix in [0][0]
		   cij[0][1].c, // cqqb, 
		   cij[2][3].c, // cttb,
		   cij[0][4].c, // cqg3, 
		   cij[1][4].c, // cqbg3, 
		   cij[0][2].c, // cqt,
		   cij[1][2].c, // cqbt, 
		   cij[4][2].c, // cg3t, 
		   cij[0][3].c, // cqtb,
		   cij[1][3].c, // cqbtb, 
		   cij[4][3].c // cg3tb
		   );


    SYMMETRIZE(0,1);
    SYMMETRIZE(2,3);
    SYMMETRIZE(0,4);
    SYMMETRIZE(1,4);
    SYMMETRIZE(0,2);
    SYMMETRIZE(1,2);
    SYMMETRIZE(4,2);
    SYMMETRIZE(0,3);
    SYMMETRIZE(1,3);
    SYMMETRIZE(4,3);
  } // constructor

void qqttg::EvalfAmplitude(const vector<FourMomentum> & pset){
    
    const FourMomentum & p1  = pset[0];
    const FourMomentum & p2  = pset[1];
    const FourMomentum & p3  = pset[4];
    const FourMomentum & kq  = pset[2];
    const FourMomentum & kqb = pset[3];
    
    FourMomentum q1, q2, r1, r2;  
    evalqr(kq,kqb,q1,q2,r1,r2);

    double f77p1[4],f77p2[4],f77p3[4],f77q1[4],f77q2[4],f77r1[4],f77r2[4];  
  
    p1.getF77Array(f77p1);
    p2.getF77Array(f77p2);
    p3.getF77Array(f77p3);
    q1.getF77Array(f77q1);
    q2.getF77Array(f77q2);
    r1.getF77Array(f77r1);
    r2.getF77Array(f77r2);

    
    complex<double> a1mm,a2mm,a3mm,a4mm,a1mp,a2mp,a3mp,a4mp,
      a1pm,a2pm,a3pm,a4pm,a1pp,a2pp,a3pp,a4pp;

    qqgtthel_(f77p1,f77p2,f77p3,f77q1,f77q2,f77r1,f77r2,
	      a1mm,a1mp,a1pm,a1pp,a2mm,a2mp,a2pm,a2pp,
	      a3mm,a3mp,a3pm,a3pp,a4mm,a4mp,a4pm,a4pp);

    ampLO[1][PolKey(MINUS, PLUS, PLUS, PLUS, MINUS)] = a2mm;
    ampLO[1][PolKey(PLUS, MINUS, PLUS, PLUS, MINUS)] = a2pm;
    ampLO[1][PolKey(MINUS, PLUS, PLUS, PLUS, PLUS)]  = a2mp;
    ampLO[1][PolKey(PLUS, MINUS, PLUS, PLUS, PLUS)]  = a2pp;

    ampLO[2][PolKey(MINUS, PLUS, PLUS, PLUS, MINUS)] = a4mm;
    ampLO[2][PolKey(PLUS, MINUS, PLUS, PLUS, MINUS)] = a4pm;
    ampLO[2][PolKey(MINUS, PLUS, PLUS, PLUS, PLUS)]  = a4mp;
    ampLO[2][PolKey(PLUS, MINUS, PLUS, PLUS, PLUS)]  = a4pp;

    ampLO[3][PolKey(MINUS, PLUS, PLUS, PLUS, MINUS)] = a1mm;
    ampLO[3][PolKey(PLUS, MINUS, PLUS, PLUS, MINUS)] = a1pm;
    ampLO[3][PolKey(MINUS, PLUS, PLUS, PLUS, PLUS)]  = a1mp;
    ampLO[3][PolKey(PLUS, MINUS, PLUS, PLUS, PLUS)]  = a1pp;

    ampLO[4][PolKey(MINUS, PLUS, PLUS, PLUS, MINUS)] = a3mm;
    ampLO[4][PolKey(PLUS, MINUS, PLUS, PLUS, MINUS)] = a3pm;
    ampLO[4][PolKey(MINUS, PLUS, PLUS, PLUS, PLUS)]  = a3mp;
    ampLO[4][PolKey(PLUS, MINUS, PLUS, PLUS, PLUS)]  = a3pp;

    qqgtthel_(f77p1,f77p2,f77p3,f77q1,f77q2,f77r2,f77r1,
	      a1mm,a1mp,a1pm,a1pp,a2mm,a2mp,a2pm,a2pp,
	      a3mm,a3mp,a3pm,a3pp,a4mm,a4mp,a4pm,a4pp);

    ampLO[1][PolKey(MINUS, PLUS, PLUS, MINUS, MINUS)] = a2mm;
    ampLO[1][PolKey(PLUS, MINUS, PLUS, MINUS, MINUS)] = a2pm;
    ampLO[1][PolKey(MINUS, PLUS, PLUS, MINUS, PLUS)]  = a2mp;
    ampLO[1][PolKey(PLUS, MINUS, PLUS, MINUS, PLUS)]  = a2pp;

    ampLO[2][PolKey(MINUS, PLUS, PLUS, MINUS, MINUS)] = a4mm;
    ampLO[2][PolKey(PLUS, MINUS, PLUS, MINUS, MINUS)] = a4pm;
    ampLO[2][PolKey(MINUS, PLUS, PLUS, MINUS, PLUS)]  = a4mp;
    ampLO[2][PolKey(PLUS, MINUS, PLUS, MINUS, PLUS)]  = a4pp;

    ampLO[3][PolKey(MINUS, PLUS, PLUS, MINUS, MINUS)] = a1mm;
    ampLO[3][PolKey(PLUS, MINUS, PLUS, MINUS, MINUS)] = a1pm;
    ampLO[3][PolKey(MINUS, PLUS, PLUS, MINUS, PLUS)]  = a1mp;
    ampLO[3][PolKey(PLUS, MINUS, PLUS, MINUS, PLUS)]  = a1pp;

    ampLO[4][PolKey(MINUS, PLUS, PLUS, MINUS, MINUS)] = a3mm;
    ampLO[4][PolKey(PLUS, MINUS, PLUS, MINUS, MINUS)] = a3pm;
    ampLO[4][PolKey(MINUS, PLUS, PLUS, MINUS, PLUS)]  = a3mp;
    ampLO[4][PolKey(PLUS, MINUS, PLUS, MINUS, PLUS)]  = a3pp;


  }

void qqttg::EvalfDipole(Dipole & d){
    
  EvalfAmplitude(d.momenta);
  
  complex<double> res(0.);

  /*
   * Spin diagonal part:
   */  
  for ( unsigned int ispin = 0; ispin < 32; ispin++ ) {
    for ( unsigned int c1 = 1; c1 < 5; c1++ ) {
      for ( unsigned int c2 = 1; c2 < 5; c2++ ) {
	res += conj(ampLO[c1][ispin]) * ampLO[c2][ispin] * d.Vdiag
	  * cij[d.spectator][d.emitter].c[c1][c2];
      }
    }
  }

  /*
   * Spin correlations:
   */
  //  if ( d.Vpm != complex<double>(0.,0.) ) {  
  if ( d.emittertype == GLUON ) {  
    for ( unsigned int ispin = 0; ispin < 32; ispin++ ) {
      for ( unsigned int c1 = 1; c1 < 5; c1++ ) {
	for ( unsigned int c2 = 1; c2 < 5; c2++ ) {
	  /*
	   * For final state emitter the helicity labelling is wrong
	   * the correct labelling is obtained from interchanging 
	   * + <--> -.
	   */
	  res +=  conj(ampLO[c1][PolKeyFlip(ispin,d.emitter)]) 
	    * ampLO[c2][ispin] 
	    * cij[d.spectator][d.emitter].c[c1][c2] 
	    * ( Key2Pol(ispin,d.emitter) == MINUS ? conj(d.Vpm) : d.Vpm );
	}
      }
    }
  }

  // factor 2 too account for the ++,+- configuration of ttb.
  d.value = 2.0 * res.real() *  pow(parms.getAlphasNLO()*4.0*M_PI,3);
}

double qqttg::EvalfAmplitudeSquared( const vector<FourMomentum> & momenta ){
    
  EvalfAmplitude(momenta);
  
  complex<double> res(0.);

  /*
   * We use the symmetries of the color matrices:
   */   
  for (unsigned int ispin = 0; ispin < 32; ispin++ ){
    for ( unsigned int c1 = 1; c1 < 5; c1++ ){
      res += 
	  conj(ampLO[c1][ispin]) * ampLO[c1][ispin] * cij[0][0].c[c1][c1];
      for ( unsigned int c2 = c1+1; c2 < 5; c2++ ){
	res += 
	  2.0 * conj(ampLO[c1][ispin]) * ampLO[c2][ispin] * cij[0][0].c[c1][c2];
      }
    }
  }
  // factor 2 too account for the ++,+- configuration of ttb.
  return( 2.0 * res.real() *  pow(parms.getAlphasNLO()*4.0*M_PI,3));
}

complex<double> qqttg::EvalfCorrelation(double KP[5][5]){

  complex<double> res(0.);

  /*
   * We use the symmetries of the color matrices:
   */   
  for (unsigned int ispin = 0; ispin < 32; ispin++ ){
    for ( unsigned int c1 = 1; c1 < 5; c1++ ){
	res += 
	  conj(ampLO[c1][ispin]) * ampLO[c1][ispin] * KP[c1][c1];
      for ( unsigned int c2 = c1+1; c2 < 5; c2++ ){
	res += 
	  2.0 * conj(ampLO[c1][ispin]) * ampLO[c2][ispin] * KP[c1][c2];
      }
    }
  }
  // factor 2 too account for the ++,+- configuration of ttb.
  return( 2.0 * res *  pow(parms.getAlphasNLO()*4.0*M_PI,3));

}

qgttq::qgttq() {

#ifdef USE_MEMSET
    memset(ampLO, 0, sizeof(ampLO));
#else  
  for ( unsigned int color=1; color < 5; color++) {
    for ( unsigned int ispin=0; ispin < 32; ispin++) {
      ampLO[color][ispin] = 0.;
    }
  }
#endif
  
  particles.push_back(UP);
  particles.push_back(GLUON);
  particles.push_back(TOP);
  particles.push_back(ATOP);
  particles.push_back(UP);
  
  qqcorrelations(cij[0][0].c, //we put the lo matrix in [0][0]
		 cij[0][4].c, // cqqb, 
		 cij[2][3].c, // cttb,
		 cij[0][1].c, // cqg3, 
		 cij[4][1].c, // cqbg3, 
		 cij[0][2].c, // cqt,
		 cij[4][2].c, // cqbt, 
		 cij[1][2].c, // cg3t, 
		 cij[0][3].c, // cqtb,
		 cij[4][3].c, // cqbtb, 
		 cij[1][3].c // cg3tb
		 );
  
  
  SYMMETRIZE(0,1);
  SYMMETRIZE(2,3);
  SYMMETRIZE(0,4);
  SYMMETRIZE(4,1);
  SYMMETRIZE(0,2);
  SYMMETRIZE(1,2);
  SYMMETRIZE(4,2);
  SYMMETRIZE(0,3);
  SYMMETRIZE(1,3);
  SYMMETRIZE(4,3);
} // constructor

void qgttq::EvalfAmplitude(const vector<FourMomentum> & pset){
    

  const FourMomentum & p1  =  pset[0];
  const FourMomentum   p2  = -pset[4];
  const FourMomentum   p3  = -pset[1];
  const FourMomentum & kq  =  pset[2];
  const FourMomentum & kqb =  pset[3];
  
  FourMomentum q1, q2, r1, r2;  
  evalqr(kq,kqb,q1,q2,r1,r2);
  
  double f77p1[4],f77p2[4],f77p3[4],f77q1[4],f77q2[4],f77r1[4],f77r2[4];  
  
  p1.getF77Array(f77p1);
  p2.getF77Array(f77p2);
  p3.getF77Array(f77p3);
  q1.getF77Array(f77q1);
  q2.getF77Array(f77q2);
  r1.getF77Array(f77r1);
  r2.getF77Array(f77r2);
  
  
  complex<double> a1mm,a2mm,a3mm,a4mm,a1mp,a2mp,a3mp,a4mp,
    a1pm,a2pm,a3pm,a4pm,a1pp,a2pp,a3pp,a4pp;
  

  qqgtthel_(f77p1,f77p2,f77p3,f77q1,f77q2,f77r1,f77r2,
	    a1mm,a1mp,a1pm,a1pp,a2mm,a2mp,a2pm,a2pp,
	    a3mm,a3mp,a3pm,a3pp,a4mm,a4mp,a4pm,a4pp);
  
  ampLO[1][PolKey(MINUS, MINUS, PLUS, PLUS, PLUS)] = a2mm;
  ampLO[1][PolKey(PLUS, MINUS, PLUS, PLUS, MINUS)] = a2pm;
  ampLO[1][PolKey(MINUS, PLUS, PLUS, PLUS, PLUS)]  = a2mp;
  ampLO[1][PolKey(PLUS, PLUS, PLUS, PLUS, MINUS)]  = a2pp;
  
  ampLO[2][PolKey(MINUS, MINUS, PLUS, PLUS, PLUS)] = a4mm;
  ampLO[2][PolKey(PLUS, MINUS, PLUS, PLUS, MINUS)] = a4pm;
  ampLO[2][PolKey(MINUS, PLUS, PLUS, PLUS, PLUS)]  = a4mp;
  ampLO[2][PolKey(PLUS, PLUS, PLUS, PLUS, MINUS)]  = a4pp;
  
  ampLO[3][PolKey(MINUS, MINUS, PLUS, PLUS, PLUS)] = a1mm;
  ampLO[3][PolKey(PLUS, MINUS, PLUS, PLUS, MINUS)] = a1pm;
  ampLO[3][PolKey(MINUS, PLUS, PLUS, PLUS, PLUS)]  = a1mp;
  ampLO[3][PolKey(PLUS, PLUS, PLUS, PLUS, MINUS)]  = a1pp;
  
  ampLO[4][PolKey(MINUS, MINUS, PLUS, PLUS, PLUS)] = a3mm;
  ampLO[4][PolKey(PLUS, MINUS, PLUS, PLUS, MINUS)] = a3pm;
  ampLO[4][PolKey(MINUS, PLUS, PLUS, PLUS, PLUS)]  = a3mp;
  ampLO[4][PolKey(PLUS, PLUS, PLUS, PLUS, MINUS)]  = a3pp;
  
  qqgtthel_(f77p1,f77p2,f77p3,f77q1,f77q2,f77r2,f77r1,
	    a1mm,a1mp,a1pm,a1pp,a2mm,a2mp,a2pm,a2pp,
	    a3mm,a3mp,a3pm,a3pp,a4mm,a4mp,a4pm,a4pp);

  ampLO[1][PolKey(MINUS, MINUS, PLUS, MINUS, PLUS)] = a2mm;
  ampLO[1][PolKey(PLUS, MINUS, PLUS, MINUS, MINUS)] = a2pm;
  ampLO[1][PolKey(MINUS, PLUS, PLUS, MINUS, PLUS)]  = a2mp;
  ampLO[1][PolKey(PLUS, PLUS, PLUS, MINUS, MINUS)]  = a2pp;
  
  ampLO[2][PolKey(MINUS, MINUS, PLUS, MINUS, PLUS)] = a4mm;
  ampLO[2][PolKey(PLUS, MINUS, PLUS, MINUS, MINUS)] = a4pm;
  ampLO[2][PolKey(MINUS, PLUS, PLUS, MINUS, PLUS)]  = a4mp;
  ampLO[2][PolKey(PLUS, PLUS, PLUS, MINUS, MINUS)]  = a4pp;
  
  ampLO[3][PolKey(MINUS, MINUS, PLUS, MINUS, PLUS)] = a1mm;
  ampLO[3][PolKey(PLUS, MINUS, PLUS, MINUS, MINUS)] = a1pm;
  ampLO[3][PolKey(MINUS, PLUS, PLUS, MINUS, PLUS)]  = a1mp;
  ampLO[3][PolKey(PLUS, PLUS, PLUS, MINUS, MINUS)]  = a1pp;
  
  ampLO[4][PolKey(MINUS, MINUS, PLUS, MINUS, PLUS)] = a3mm;
  ampLO[4][PolKey(PLUS, MINUS, PLUS, MINUS, MINUS)] = a3pm;
  ampLO[4][PolKey(MINUS, PLUS, PLUS, MINUS, PLUS)]  = a3mp;
  ampLO[4][PolKey(PLUS, PLUS, PLUS, MINUS, MINUS)]  = a3pp;
  
  
}


void qgttq::EvalfDipole(Dipole & d){
    
  EvalfAmplitude(d.momenta);

  complex<double> res(0.);
  /*
   * Spin diagonal part:
   */  
  for ( unsigned int ispin = 0; ispin < 32; ispin++ ){
    for( unsigned int c1 = 1; c1 < 5; c1++){
      for( unsigned int c2 = 1; c2 < 5; c2++){
	res += conj(ampLO[c1][ispin]) * ampLO[c2][ispin] * d.Vdiag
	  * cij[d.spectator][d.emitter].c[c1][c2];
      }
    }
  }

  /*
   * Spin correlations:
   */
  //  if ( d.Vpm != complex<double>(0.,0.) ) {   
  if ( d.emittertype == GLUON ) {   
    for ( unsigned int ispin = 0; ispin < 32; ispin++ ){
      for( unsigned int c1 = 1; c1 < 5; c1++){
	for( unsigned int c2 = 1; c2 < 5; c2++){
	  res +=  conj(ampLO[c1][PolKeyFlip(ispin,d.emitter)]) 
	    * ampLO[c2][ispin] 
	    * cij[d.spectator][d.emitter].c[c1][c2] 
	    * ( Key2Pol(ispin,d.emitter) == MINUS ? (d.Vpm) : conj(d.Vpm) );
	}
      }
    }
  }
  
  // factor 2 too account for the ++,+- configuration of ttb.
  d.value = 2.0 * res.real() *  pow(parms.getAlphasNLO()*4.0*M_PI,3);    
}

double qgttq::EvalfAmplitudeSquared( const vector<FourMomentum> & momenta){
    
  EvalfAmplitude(momenta);

  complex<double> res(0.);
    
  for ( unsigned int ispin = 0; ispin < 32; ispin++ ){
    for(int c1=1; c1 < 5; c1++){
      for(int c2=1; c2 < 5; c2++){
	res += 
	  conj(ampLO[c1][ispin]) * ampLO[c2][ispin] * cij[0][0].c[c1][c2];
      }
    }
  }
  // factor 2 too account for the ++,+- configuration of ttb.
  return( 2.0 * res.real() *  pow(parms.getAlphasNLO()*4.0*M_PI,3) );    
}

complex<double> qgttq::EvalfCorrelation(double KP[5][5]){

  complex<double> res(0.);
  /*
   * We use the symmetry of the color matrices..
   */    
  for ( unsigned int ispin = 0; ispin < 32; ispin++ ){
    for(int c1=1; c1 < 5; c1++){
      res += 
	conj(ampLO[c1][ispin]) * ampLO[c1][ispin] * KP[c1][c1];
      for(int c2=c1+1; c2 < 5; c2++){
	res += 
	  2.0 * conj(ampLO[c1][ispin]) * ampLO[c2][ispin] * KP[c1][c2];
      }
    }
  }
  // factor 2 too account for the ++,+- configuration of ttb.
  return( 2.0 * res.real() *  pow(parms.getAlphasNLO()*4.0*M_PI,3) );    
}


gqbttqb::gqbttqb() {

#ifdef USE_MEMSET
    memset(ampLO, 0, sizeof(ampLO));
#else
  for (unsigned int color=1; color < 5; color++) {
    for ( unsigned int ispin=0; ispin < 32; ispin++) {
      ampLO[color][ispin] = 0.;
    }
  }
#endif

  particles.push_back(GLUON);
  particles.push_back(AUP);
  particles.push_back(TOP);
  particles.push_back(ATOP);
  particles.push_back(AUP);
  
  qqcorrelations(cij[0][0].c,  //we put the lo matrix in [0][0]
		 cij[4][1].c, // cqqb, 
		 cij[2][3].c, // cttb,
		 cij[4][0].c, // cqg3, 
		 cij[1][0].c, // cqbg3, 
		 cij[4][2].c, // cqt,
		 cij[1][2].c, // cqbt, 
		 cij[0][2].c, // cg3t, 
		 cij[4][3].c, // cqtb,
		 cij[1][3].c, // cqbtb, 
		 cij[0][3].c // cg3tb
		 );
  
  SYMMETRIZE(4,1);
  SYMMETRIZE(2,3);
  SYMMETRIZE(4,0);
  SYMMETRIZE(1,0);
  SYMMETRIZE(4,2);
  SYMMETRIZE(1,2);
  SYMMETRIZE(0,2);
  SYMMETRIZE(4,3);
  SYMMETRIZE(1,3);
  SYMMETRIZE(0,3);


} // constructor

void gqbttqb::EvalfAmplitude(const vector<FourMomentum> & pset){
  
  
  const FourMomentum   p1  = -pset[4];
  const FourMomentum & p2  =  pset[1];
  const FourMomentum   p3  = -pset[0];
  const FourMomentum & kq  =  pset[2];
  const FourMomentum & kqb =  pset[3];
  
  FourMomentum q1, q2, r1, r2;  
  evalqr(kq,kqb,q1,q2,r1,r2);
  
  double f77p1[4],f77p2[4],f77p3[4],f77q1[4],f77q2[4],f77r1[4],f77r2[4];  
  
  p1.getF77Array(f77p1);
  p2.getF77Array(f77p2);
  p3.getF77Array(f77p3);
  q1.getF77Array(f77q1);
  q2.getF77Array(f77q2);
  r1.getF77Array(f77r1);
  r2.getF77Array(f77r2);

  
  complex<double> a1mm,a2mm,a3mm,a4mm,a1mp,a2mp,a3mp,a4mp,
    a1pm,a2pm,a3pm,a4pm,a1pp,a2pp,a3pp,a4pp;

  
  qqgtthel_(f77p1,f77p2,f77p3,f77q1,f77q2,f77r1,f77r2,
	    a1mm,a1mp,a1pm,a1pp,a2mm,a2mp,a2pm,a2pp,
	    a3mm,a3mp,a3pm,a3pp,a4mm,a4mp,a4pm,a4pp);
  
  ampLO[1][PolKey(MINUS, MINUS, PLUS, PLUS, PLUS)] = a2mm;
  ampLO[1][PolKey(MINUS, PLUS, PLUS, PLUS, MINUS)] = a2pm;
  ampLO[1][PolKey(PLUS, MINUS, PLUS, PLUS, PLUS)]  = a2mp;
  ampLO[1][PolKey(PLUS, PLUS, PLUS, PLUS, MINUS)]  = a2pp;
  
  ampLO[2][PolKey(MINUS, MINUS, PLUS, PLUS, PLUS)] = a4mm;
  ampLO[2][PolKey(MINUS, PLUS, PLUS, PLUS, MINUS)] = a4pm;
  ampLO[2][PolKey(PLUS, MINUS, PLUS, PLUS, PLUS)]  = a4mp;
  ampLO[2][PolKey(PLUS, PLUS, PLUS, PLUS, MINUS)]  = a4pp;
  
  ampLO[3][PolKey(MINUS, MINUS, PLUS, PLUS, PLUS)] = a1mm;
  ampLO[3][PolKey(MINUS, PLUS, PLUS, PLUS, MINUS)] = a1pm;
  ampLO[3][PolKey(PLUS, MINUS, PLUS, PLUS, PLUS)]  = a1mp;
  ampLO[3][PolKey(PLUS, PLUS, PLUS, PLUS, MINUS)]  = a1pp;
  
  ampLO[4][PolKey(MINUS, MINUS, PLUS, PLUS, PLUS)] = a3mm;
  ampLO[4][PolKey(MINUS, PLUS, PLUS, PLUS, MINUS)] = a3pm;
  ampLO[4][PolKey(PLUS, MINUS, PLUS, PLUS, PLUS)]  = a3mp;
  ampLO[4][PolKey(PLUS, PLUS, PLUS, PLUS, MINUS)]  = a3pp;
  
  qqgtthel_(f77p1,f77p2,f77p3,f77q1,f77q2,f77r2,f77r1,
	    a1mm,a1mp,a1pm,a1pp,a2mm,a2mp,a2pm,a2pp,
	    a3mm,a3mp,a3pm,a3pp,a4mm,a4mp,a4pm,a4pp);
  
  ampLO[1][PolKey(MINUS, MINUS, PLUS, MINUS, PLUS)] = a2mm;
  ampLO[1][PolKey(MINUS, PLUS, PLUS, MINUS, MINUS)] = a2pm;
  ampLO[1][PolKey(PLUS, MINUS, PLUS, MINUS, PLUS)]  = a2mp;
  ampLO[1][PolKey(PLUS, PLUS, PLUS, MINUS, MINUS)]  = a2pp;
  
  ampLO[2][PolKey(MINUS, MINUS, PLUS, MINUS, PLUS)] = a4mm;
  ampLO[2][PolKey(MINUS, PLUS, PLUS, MINUS, MINUS)] = a4pm;
  ampLO[2][PolKey(PLUS, MINUS, PLUS, MINUS, PLUS)]  = a4mp;
  ampLO[2][PolKey(PLUS, PLUS, PLUS, MINUS, MINUS)]  = a4pp;
  
  ampLO[3][PolKey(MINUS, MINUS, PLUS, MINUS, PLUS)] = a1mm;
  ampLO[3][PolKey(MINUS, PLUS, PLUS, MINUS, MINUS)] = a1pm;
  ampLO[3][PolKey(PLUS, MINUS, PLUS, MINUS, PLUS)]  = a1mp;
  ampLO[3][PolKey(PLUS, PLUS, PLUS, MINUS, MINUS)]  = a1pp;
  
  ampLO[4][PolKey(MINUS, MINUS, PLUS, MINUS, PLUS)] = a3mm;
  ampLO[4][PolKey(MINUS, PLUS, PLUS, MINUS, MINUS)] = a3pm;
  ampLO[4][PolKey(PLUS, MINUS, PLUS, MINUS, PLUS)]  = a3mp;
  ampLO[4][PolKey(PLUS, PLUS, PLUS, MINUS, MINUS)]  = a3pp;
  
  
}



void gqbttqb::EvalfDipole(Dipole & d){

  EvalfAmplitude(d.momenta);
  
  complex<double> res(0.);

  /*
   * Spin diagonal part:
   */
  for ( unsigned int ispin = 0; ispin < 32; ispin++ ){
    for ( unsigned int c1 = 1; c1 < 5; c1++){
      for ( unsigned int c2 = 1; c2 < 5; c2++){
	res += conj( ampLO[c1][ispin] ) * ampLO[c2][ispin] * d.Vdiag
	  * cij[d.spectator][d.emitter].c[c1][c2];
      }
    }
  }

  /*
   * Spin correlations:
   */
  //  if ( d.Vpm != complex<double>(0.,0.) ) {      
  if ( d.emittertype == GLUON ) {      
    for ( unsigned int ispin = 0; ispin < 32; ispin++ ){
      for ( unsigned int c1 = 1; c1 < 5; c1++){
	for ( unsigned int c2 = 1; c2 < 5; c2++){
	  res +=  conj( ampLO[c1][PolKeyFlip(ispin,d.emitter)] ) 
	    * ampLO[c2][ispin] 
	    * cij[d.spectator][d.emitter].c[c1][c2] 
	    * ( Key2Pol(ispin,d.emitter) == MINUS ? (d.Vpm) : conj(d.Vpm) );
	}
      }
    }
  }

  // factor 2 too account for the ++,+- configuration of ttb.
  d.value = 2.0 * res.real() *  pow(parms.getAlphasNLO()*4.0*M_PI,3);    
}

double gqbttqb::EvalfAmplitudeSquared(const vector<FourMomentum> & momenta ){

  EvalfAmplitude(momenta);
  
  complex<double> res(0.);
  
  for ( unsigned int ispin = 0; ispin < 32; ispin++ ){
    for ( unsigned int c1 = 1; c1 < 5; c1++){
      for ( unsigned int c2 = 1; c2 < 5; c2++){
	res += 
	  conj( ampLO[c1][ispin] ) * ampLO[c2][ispin] * cij[0][0].c[c1][c2];
      }
    }
  }
  // factor 2 too account for the ++,+- configuration of ttb.
  return( 2.0 * res.real() *  pow(parms.getAlphasNLO()*4.0*M_PI,3) );    
}


std::complex<double> gqbttqb::EvalfCorrelation(double KP[5][5]){

  complex<double> res(0.);
  
  for ( unsigned int ispin = 0; ispin < 32; ispin++ ){
    for ( unsigned int c1 = 1; c1 < 5; c1++){
      res += conj( ampLO[c1][ispin] ) * ampLO[c1][ispin] * KP[c1][c1];
      for ( unsigned int c2 = c1+1; c2 < 5; c2++){
	res += 
	  2.0 * conj( ampLO[c1][ispin] ) * ampLO[c2][ispin] * KP[c1][c2];
      }
    }
  }
  // factor 2 too account for the ++,+- configuration of ttb.
  return( 2.0 * res.real() *  pow(parms.getAlphasNLO()*4.0*M_PI,3) );    
}
