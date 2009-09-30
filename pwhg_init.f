      subroutine pwhginit
      implicit none
      include 'include/LesHouches.h'
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_pdf.h'
      include 'include/pwhg_rad.h'
      include 'include/pwhg_flg.h'
      real * 8 powheginput
      external powheginput
      integer i1,n1,n2
c     Set to true to remember and use identical values of the computed 
c     amplitudes, for Born, real and virtual contributions
      flg_smartsig=.true.
      if(powheginput("#smartsig").eq.0) flg_smartsig=.false.
c     if true also counterterm are output in NLO tests (default)
      flg_withsubtr=.true.
      if(powheginput("#withsubtr").eq.0) flg_withsubtr=.false.
c     if true use Born zero damping factor and include remnants
      flg_withdamp=.false.
      if(powheginput("#withdamp").eq.1) flg_withdamp=.true.
c     initialize random number sequence
      i1=powheginput('#iseed')
      if (i1.lt.0) i1=0
      n1=powheginput('#rand1')
      if (n1.lt.0) n1=0
      n2=powheginput('#rand2')
      if (n2.lt.0) n2=0
      call setrandom(i1,n1,n2)
c     initialize physical parameters
      call init_phys
c ID of beam particles 1 and 2 (pdg convention)
c proton:
      ebmup(1)=kn_beams(0,1)
      ebmup(2)=kn_beams(0,2)
      idbmup(1) = 2212*pdf_ih1
      idbmup(2) = 2212*pdf_ih2
c pdf group; negative to use internal herwig pdf's for showering
      pdfgup(1)=-1
      pdfgup(2)=-1
c pdf set
      pdfsup(1)=-1
      pdfsup(2)=-1
c unweighted events in input:
      idwtup = 3
c number of user subprocesses
c Irrelevant if idwtup=+-3,+-4
      nprup = 1
      call bbinit
      lprup(1)=10001
c now the cross section is available
      xsecup(1)=rad_sigtot  
      xerrup(1)=rad_sigtoterr
      xmaxup(1)=1
      end
