      subroutine init_couplings
      implicit none
      include 'PhysPars.h'
      include '../include/pwhg_st.h'
      include '../include/pwhg_math.h'
      include 'nlegborn.h'
      include '../include/pwhg_flst.h'
      include '../include/pwhg_rad.h'
c$$$      include '../include/pwhg_par.h'
      logical verbose
      parameter(verbose=.true.)
      integer aemrun
      real *8 powheginput
      external powheginput
      real *8 alfaem,pwhg_alphas,totbr
      external alfaem,pwhg_alphas
      integer i,j,idummy
      real *8 alphaem_inv
      common/calphaem_inv/alphaem_inv

c$$$      par_isrtinycsi = 1d-7
c$$$      par_isrtinyy =   1d-9
c$$$      par_fsrtinycsi = 1d-5
c$$$      par_fsrtinyy =   1d-6

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc   INDEPENDENT QUANTITIES       
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     number of light flavors
      st_nlight = 5

c     setting physical parameters
      write(*,*) 'POWHEG: loading-setting physical parameters'

c     branching ratio (used only in LH event file)
      call pickwdecay(-1000,idummy,idummy,idummy,totbr)
      rad_branching=totbr

c     top mass
      topmass_pow=175d0
      topmass_pow=powheginput('topmass')
      if(topmass_pow.lt.0) then
         write(*,*) 'Input error: topmass ',topmass_pow
         call exit(1)
      endif

c     top width
      topwidth_pow=1.7d0
      topwidth_pow=powheginput('topwidth')
      if(topwidth_pow.lt.0) then
         write(*,*) 'Input error: topwidth ',topwidth_pow
         call exit(1)
      endif

c     ew parameters
c     true inputs are wmass, alphaem(zmass), sthw2
c     alphaem is evaluated at the top mass
      wmass_pow=80.4d0
      wmass_pow=powheginput('wmass')
      if(wmass_pow.lt.0) then
         write(*,*) 'Input error: wmass ',wmass_pow
         call exit(1)
      endif

      wwidth_pow=2.141d0
      wwidth_pow=powheginput('wwidth')
      if(wwidth_pow.lt.0) then
         write(*,*) 'Input error: wwidth ',wwidth_pow
         call exit(1)
      endif

      sthw2_pow=0.23113d0
      sthw2_pow=powheginput('sthw2')
      if(sthw2_pow.lt.0) then
         write(*,*) 'Input error: sthw2 ',sthw2_pow
         call exit(1)
      endif

c     typical values for alphaem:
c     Thompson value:    1/137.0359895d0
c     at z mass (91.188) 1/127.934 (???)
c     at top mass (175)  1d0/127.011989

      aemrun=0
c     definition of alphaem_pow value, according to aemrun
      if(aemrun.eq.0) then
         alphaem_inv=137.0359895d0
         alphaem_inv=powheginput('alphaem_inv')
         if(alphaem_inv.lt.0) then
            write(*,*) 'Input error: alphaem_inv ',alphaem_inv
            call exit(1)
         endif
         alphaem_pow=1d0/alphaem_inv
         zmass_pow=91.188d0   !Not relevant in POWHEG; needed only by set_madgraph_parameters
      elseif(aemrun.eq.1) then
c     alphaem_pow is evaluated at the top mass value using the alfaem function.
c     In this case zmass is needed by the alfaem function to set a reference
c     point for the running of alphaem. zmass needed also by set_madgraph_parameters.
c     This reference value is read and used by the function alfaem itself
c     that will assume alfaem(zmass)=1/alphaem_inv.
         write(*,*) 'aemrun option not yet implemented' 
         alphaem_inv=127.934
         zmass_pow=92d0
         alphaem_pow=alfaem(topmass_pow**2)
      else
         write(*,*) 'Error while setting aemrun'
         call exit(1)
      endif

c     ckm matrix entries
      CKM_pow(1,1)= 0.9740
      CKM_pow(1,2)= 0.2225
      CKM_pow(1,3)= 1d-6
      CKM_pow(2,1)= 0.2225
      CKM_pow(2,2)= 0.9740
      CKM_pow(2,3)= 1d-6
      CKM_pow(3,1)= 1d-6
      CKM_pow(3,2)= 1d-6
      CKM_pow(3,3)= 1d0

      CKM_pow(1,1)= powheginput('CKM_Vud')
      CKM_pow(1,2)= powheginput('CKM_Vus') 
      CKM_pow(1,3)= powheginput('CKM_Vub') 
      CKM_pow(2,1)= powheginput('CKM_Vcd') 
      CKM_pow(2,2)= powheginput('CKM_Vcs') 
      CKM_pow(2,3)= powheginput('CKM_Vcb') 
      CKM_pow(3,1)= powheginput('CKM_Vtd') 
      CKM_pow(3,2)= powheginput('CKM_Vts') 
      CKM_pow(3,3)= powheginput('CKM_Vtb') 

      do i=1,3
         do j=1,3
            if(CKM_pow(i,j).lt.0d0) then
               write(*,*) 'Input error: CKM (i,j)= ',i,j
               call exit(1)
            endif
         enddo
      enddo

c$$$c     ckm matrix entries (test 2)
c$$$      CKM_pow(1,1)= 0.57735
c$$$      CKM_pow(1,2)= 0.57735
c$$$      CKM_pow(1,3)= 0.57735
c$$$      CKM_pow(2,1)= 0.57735
c$$$      CKM_pow(2,2)= 0.57735
c$$$      CKM_pow(2,3)= 0.57735
c$$$      CKM_pow(3,1)= 0.57735
c$$$      CKM_pow(3,2)= 0.57735
c$$$      CKM_pow(3,3)= 0.57735

      do i=1,6
         do j=1,6
            CKM(i,j)=0d0
         enddo
      enddo
      CKM(2,1)=CKM_pow(1,1)
      CKM(1,2)=CKM_pow(1,1)
      CKM(2,3)=CKM_pow(1,2)
      CKM(3,2)=CKM_pow(1,2)
      CKM(2,5)=CKM_pow(1,3)
      CKM(5,2)=CKM_pow(1,3)

      CKM(4,1)=CKM_pow(2,1)
      CKM(1,4)=CKM_pow(2,1)
      CKM(4,3)=CKM_pow(2,2)
      CKM(3,4)=CKM_pow(2,2)
      CKM(4,5)=CKM_pow(2,3)
      CKM(5,4)=CKM_pow(2,3)

      CKM(6,1)=CKM_pow(3,1)
      CKM(1,6)=CKM_pow(3,1)
      CKM(6,3)=CKM_pow(3,2)
      CKM(3,6)=CKM_pow(3,2)
      CKM(6,5)=CKM_pow(3,3)
      CKM(5,6)=CKM_pow(3,3)

c$$$c     setting mcnlo parameters (needed for amplitudes subroutines)
c$$$      call set_mcnlo_parameters

c     setting madgraph parameters (needed for madgraph subroutines)
      call set_madgraph_parameters

      if(verbose) then
         write(*,*) '--------------------------------------'
         write(*,*) 'POWHEG: RELEVANT PARAMETERS'
         write(*,*) 'top mass       ',topmass_pow
         write(*,*) 'top width      ',topwidth_pow      
         write(*,*) '1/alphaem      ',1.d0/alphaem_pow
         write(*,*) 'W mass         ',wmass_pow
         write(*,*) 'W width        ',wwidth_pow
         write(*,*) 'sin2w          ',sthw2_pow
         write(*,*)'CKM matrix (rows:u,c,t columns:d,s,b )'
      write(*,'(a,3(f10.7))')' ',CKM_pow(1,1),CKM_pow(1,2),CKM_pow(1,3)
      write(*,'(a,3(f10.7))')' ',CKM_pow(2,1),CKM_pow(2,2),CKM_pow(2,3)
      write(*,'(a,3(f10.7))')' ',CKM_pow(3,1),CKM_pow(3,2),CKM_pow(3,3)
         write(*,*) 'lambda_QCD     ',st_lambda5MSB
         write(*,'(1X,A,f7.3,A,f15.7)') 'alpha_s(',91.2d0,')'
     $,pwhg_alphas(91.2d0**2,st_lambda5MSB,st_nlight)
         write(*,'(1X,A,f7.3,A,f15.7)') 'alpha_s(',topmass_pow,')'
     $,pwhg_alphas(topmass_pow**2,st_lambda5MSB,st_nlight)
         write(*,*)
         write(*,*) 'top branching ratio ',totbr
         write(*,*) '--------------------------------------'
      endif

      end

c-------------------------------------------------------------------------
      function alfaem(q2)
c Alpha_em(MSbar) at the scale q2 = q^2. 
c Uses alpha_Thomson below the electron mass, alpha(mass) below
c mu_mass and m_tau, and the evolution equation above m_tau, comnsidering the b threshold
c This function is taken from the MC@NLO and modified by SA&ER
c-------------------------------------------------------------------------
      implicit none
      include '../include/pwhg_math.h'
      include 'PhysPars.h'
      integer npoints,ideg
      parameter (npoints=3,ideg=3)
      real*8 ooa(npoints),xlogmu(npoints)
c 1/alpha_em at m_e=0.000511,m_mu=0.1056,m_tau=1.777      
      data ooa     / 137.036, 135.95, 133.513 /
c logs of sqrt(q2) at m_e=0.000511,m_mu=0.1056,m_tau=1.777      
      data xlogmu  / -7.57914, -2.2481, 0.574927 /
      real *8 zm
      real*8 ooaz,xlq,b,q2
      real *8 alfaem

      real *8 alphaem_inv
      common/calphaem_inv/alphaem_inv

      zm=zmass_pow
      ooaz=alphaem_inv


      if(q2.lt.exp(2.*xlogmu(1))) then
         alfaem = 1.d0/ooa(1)	 
      elseif(q2.lt.exp(2.*xlogmu(2))) then
         xlq = log(q2)/2.d0
         alfaem = 1.d0/ooa(2)
      elseif(q2.lt.exp(2.*xlogmu(3))) then
         xlq = log(q2)/2.d0
         alfaem = 1.d0/ooa(3)
      elseif(q2.lt.5.**2) then
         b = 3 + 2*nc*(1d0/3d0)**2 + 2*nc*(2d0/3d0)**2
         xlq = log(q2) - 2.*xlogmu(3)
         alfaem = 1d0/ooa(3)/(1.d0 - 1.d0/3.d0/pi/ooa(3)*b*xlq)
      else
         b = 3 + 3*nc*(1d0/3d0)**2 + 2*nc*(2d0/3d0)**2
         xlq = log(q2/zm**2)
         alfaem = 1d0/ooaz/(1.d0 - 1.d0/3.d0/pi/ooaz*b*xlq)
      endif
      return
      end





c     setting of MADGRAPH inputs
      subroutine set_madgraph_parameters
      include 'PhysPars.h'
      include '../include/pwhg_math.h'
      include 'coupl.inc'

cccccccccccccccccccccccccccccccc    
c     common bl. originally present in lh_readin, needed
c     by my_setpara
c
c     Common to lh_readin and printout
c
      double precision  alpha, gfermi, alfas
      double precision  mtMS,mbMS,mcMS,mtaMS!MSbar masses
      double precision  Vud             !CKM matrix elements
      common/values/    alpha,gfermi,alfas,   
     &                  mtMS,mbMS,mcMS,mtaMS,
     &                  Vud
ccccccccccccccccccccccccccccccccc
c     common bl. originally present in setpar.f, needed
c     by HELAS subroutine
C
C     BEAM POLARIZATION
C
      REAL*8 POL(2)
      common/to_polarization/ POL
      data POL/1d0,1d0/
ccccccccccccccccccccccccccccccc
     
      real *8 www

      write(*,*) 'POWHEG: set_madgraph_parameters called'

c     SM INPUTS
      alpha=alphaem_pow         !Mad
      zmass=zmass_pow           !Mad
c     alfas=alphas_pow          !see tdecay

c     YUKAWA (unuseful in single top)
      mcMS=0d0                  !Mad
      mbMS=0d0                  !Mad
c     mtMS=topmass_pow          !see tdecay

c     CKM
      vud=1d0                   !Mad

c     MASSES
      bmass=0d0                 !Mad
c     tmass=topmass_pow         !see tdecay

      wmass=wmass_pow           !Mad 

c     WIDTHS
c     twidth=0d0                !see tdecay
c     wwidth=0d0                !see tdecay

c     Setting of wm MadGraph parameter. This is used only to
c     calculate the g_w (weak coupling) used in HELAS subroutines.
c     To have the same coupling of POWHEG, the following ad-hoc definition
c     of Madgraph gfermi is mandatory.
c     The following is an inversion of the assignment formula
c     for wm (see my_setpara subroutine).
      www=zmass*sqrt(1-sthw2_pow)
      gfermi=pi*zmass**2*alpha/sqrt(2.)
      gfermi=gfermi/(zmass**2*www**2 - www**4)

c     setting of other remaining couplings is done by my_setpara on
c     an event by event basis
      end


ccccccccccccccccccccccccccccccccccccccccccccc
c     !: beginning of interface subroutines to madgraph     
ccccccccccccccccccccccccccccccccccccccccccccc
c     see subroutine my_setpara
