      subroutine init_couplings
      implicit none
      include "coupl.inc"
      include 'PhysPars.h'
      include 'pwhg_par.h'
c Avoid multiple calls to this subroutine. The parameter file is opened
c but never closed ...
      logical called
      real * 8 powheginput
      external powheginput
      data called/.false./
      save called
      integer idvecbos,vdecaymode,Vdecmod
      common/cvecbos/idvecbos,vdecaymode
      real *8 lepmass(3),decmass
      common/clepmass/lepmass,decmass
      data lepmass/0.51099891d-3,0.1056583668d0,1.77684d0/
      

*********************************************************
***********         MADGRAPH                 ************
*********************************************************
c Parameters are read from the MadGraph param_card.dat,
c except the strong coupling constant, which is defined
c somewhere else
c      call setpara("param_card.dat",.true.)

      call lh_readin("none")
      call madtophys


c******************************************************
c     Choose the process to be implemented
c******************************************************
c    ID of vector boson produced
      idvecbos=powheginput('idvecbos')
c   decay products of the vector boson
      Vdecmod=powheginput('vdecaymode')
      
      if(idvecbos.eq.24) then
         select case(Vdecmod)
         case (1)
            vdecaymode=-11
         case (2)
            vdecaymode=-13
         case (3)
            vdecaymode=-15
         case default
            write(*,*) 'ERROR: The decay mode you selected' /
     $           /' is not allowed '
            stop
         end select  
         write(*,*) 
         write(*,*) ' POWHEG: Single W+ production and decay ' 
         if (vdecaymode.eq.-11) write(*,*) '         to e+ ve '
         if (vdecaymode.eq.-13) write(*,*) '         to mu+ vmu'
         if (vdecaymode.eq.-15) write(*,*) '         to tau+ vtau'
         write(*,*) 
      elseif(idvecbos.eq.-24) then
         select case(Vdecmod)
         case (1)
            vdecaymode= 11
         case (2)
            vdecaymode= 13
         case (3)
            vdecaymode= 15
         case default
            write(*,*) 'ERROR: The decay mode you selected' /
     $           /' is not allowed '
            stop
         end select
         write(*,*) 
         write(*,*) ' POWHEG: Single W- production and decay '
         if (vdecaymode.eq.11) write(*,*) '         to e- ve~ '
         if (vdecaymode.eq.13) write(*,*) '         to mu- vmu~'
         if (vdecaymode.eq.15) write(*,*) '         to tau- vtau~'
         write(*,*)    
      else
         write(*,*) 'ERROR: The ID of vector boson you selected' 
     $        //' is not allowed (24: W+ -24: W-)'
         stop
      endif

c     set lepton mass
      decmass=lepmass(Vdecmod)
    

      end


      subroutine lh_readin(param_name)
c overrides the lh_readin subroutine in MODEL/couplings.f;
c to make it work, rename or delete
c the lh_readin routine in MODEL/couplings.f
      implicit none
      character*(*) param_name
      include 'coupl.inc'
      include 'PhysPars.h'
      double precision  Two, Four, Rt2, Pi
      parameter( Two = 2.0d0, Four = 4.0d0 )
      parameter( Rt2   = 1.414213562d0 )
      parameter( Pi = 3.14159265358979323846d0 )

c
c     Common to lh_readin and printout
c
      double precision  alpha, gfermi, alfas
      double precision  mtMS,mbMS,mcMS,mtaMS!MSbar masses
      double precision  Vud,Vus,Vub,Vcd,Vcs,Vcb,Vtd,Vts,Vtb !CKM matrix elements
      common/values/    alpha,gfermi,alfas,   
     &                  mtMS,mbMS,mcMS,mtaMS,
     &                  Vud,Vus,Vub,Vcd,Vcs,Vcb,Vtd,Vts,Vtb
c
      real * 8 powheginput
c the only parameters relevant for this process are set
c via powheginput. All others are needed for the
c madgraph routines not to blow.
c      alpha= 7.7585538055706d-03
      alpha= 1/132.50698d0
      gfermi = 0.1166390d-4
      alfas = 0.119d0
      zmass = 91.188d0
c      tmass = 174.3d0
      tmass = 172.5d0
      lmass = 0d0
      mcMS = 0d0
      mbMS = 0d0
c      mtMS = 174d0
      mtMS = 172.5d0
      mtaMS = 1.777d0
      cmass = 0d0
      bmass = 0d0
      lmass=0d0
      wmass=sqrt(zmass**2/Two+
     $     sqrt(zmass**4/Four-Pi/Rt2*alpha/gfermi*zmass**2))

      twidth=1.5083d0

      ph_Wmass2low=20d0**2
      
      ph_Wmass2high=140d0**2

      hmass = powheginput('hmass')
      hwidth = powheginput('hwidth')
      
      zwidth=2.441d0
      wwidth=2.0476d0

      
      gal(1) = sqrt(alpha*4*pi)


c     POWHEG CKM matrix
c
c        d     s     b
c    u
c    c
c    t

c      Vud=0.97428d0 
c      Vus=0.2253d0  
c      Vub=0.00347d0 
c      Vcd=0.2252d0  
c      Vcs=0.97345d0 
c      Vcb=0.0410d0  
c      Vtd=0.00862d0 
c      Vts=0.0403d0  
c      Vtb=0.999152d0

      Vud=1d0
      Vus=1d-10
      Vub=1d-10
      Vcd=1d-10
      Vcs=1d0
      Vcb=1d-10
      Vtd=1d-10
      Vts=1d-10
      Vtb=1d0

      end



      subroutine madtophys
      implicit none
      include 'coupl.inc'
      include 'PhysPars.h'
      include 'pwhg_math.h'
      real * 8 e_em,g_weak
c
c     Common to lh_readin and printout
c
      double precision  alpha, gfermi, alfas
      double precision  mtMS,mbMS,mcMS,mtaMS!MSbar masses
      double precision  Vud,Vus,Vub,Vcd,Vcs,Vcb,Vtd,Vts,Vtb !CKM matrix elements
      common/values/    alpha,gfermi,alfas,   
     &                  mtMS,mbMS,mcMS,mtaMS,
     &                  Vud,Vus,Vub,Vcd,Vcs,Vcb,Vtd,Vts,Vtb
      e_em=gal(1)
      
      ph_unit_e=e_em
      ph_alphaem=e_em**2/(4*pi)

      ph_sthw2=1-(wmass/zmass)**2
      ph_sthw=sqrt(ph_sthw2)
      g_weak=e_em/ph_sthw
      ph_gfermi=sqrt(2d0)*g_weak**2/(8*wmass**2)

      ph_Zmass = zmass
      ph_Wmass = wmass
      ph_Hmass = hmass
      ph_Zwidth = zwidth
      ph_Wwidth = wwidth
      ph_Hwidth = hwidth
      ph_tmass = tmass

      ph_WmWw = ph_Wmass * ph_Wwidth
      ph_Wmass2 = ph_Wmass**2

c     CKM from PDG 2010 (eq. 11.27)
      ph_CKM(1,1)=Vud
      ph_CKM(1,2)=Vus
      ph_CKM(1,3)=Vub
      ph_CKM(2,1)=Vcd
      ph_CKM(2,2)=Vcs
      ph_CKM(2,3)=Vcb
      ph_CKM(3,1)=Vtd
      ph_CKM(3,2)=Vts
      ph_CKM(3,3)=Vtb

      end
