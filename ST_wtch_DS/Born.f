c     !ER: ttype DOVREBBE essere per ora a POSTO.
c     !ER: pickwdecay dovrebbe essere OK.
c     !ER: Reshuffling machinery AL MOMENTO NON VIENE CHIAMATA.
c     !ER: Bisogna infatti rivedere put_on_mass_shell.

c     !ER: >>>>>>> decay machinery MISSING <<<<<<<<<
c     !ER: Solo pickwdecay e' stata aggiunta.

c     !: Born.f should be very similar between DR and DS.
c     !: Differences should be confined in the decay
c     !: generation machinery.

      subroutine setborn(p,bflav,born,bornjk,bmunu)
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_math.h'
      include 'PhysPars.h'
      integer nlegs
      parameter (nlegs=nlegborn)
      real * 8 p(0:3,nlegs),bornjk(nlegs,nlegs)
      integer bflav(nlegs)
      real * 8 bmunu(0:3,0:3,nlegs),bbmunu(0:3,0:3),born,colcf
      real *8 borntmp
      integer j,k,mu,nu

      integer bflav_loc(nlegs)


ccccccccccccccccccccccccccccccccccccccc
c     charge conjugation
c     If ttype=-1, then bflav has been filled with tbar-production flavours.
c     Subroutines here work for t-production flavour assignment.
c     Therefore, invert the sign of local flavours.
      do j=1,nlegs
         bflav_loc(j)= ttype *bflav(j)
      enddo
ccccccccccccccccccccccccccccccccccccccc


      call compborn(p,bflav_loc,borntmp,bbmunu)
      born=borntmp
      do j=1,nlegborn
         if(abs(bflav(j)).le.6) then
            do mu=0,3
               do nu=0,3
                  bmunu(mu,nu,j)=0d0
               enddo
            enddo
            if(bflav(j).eq.0) then
c     leg is a gluon. Assign corresponding bmunu
               do mu=0,3
                  do nu=0,3
                     bmunu(mu,nu,j)=bbmunu(mu,nu)
                  enddo
               enddo
c$$$            write(*,*)'--------------------------------'
c$$$            write(*,*)'leg: ',j
c$$$            write(*,*) (bmunu(0,nu,j), nu=0,3)
c$$$            write(*,*) (bmunu(1,nu,j), nu=0,3)
c$$$            write(*,*) (bmunu(2,nu,j), nu=0,3)
c$$$            write(*,*) (bmunu(3,nu,j), nu=0,3)
            endif

c Colour factors for colour-correlated Born amplitudes;
c Rule from 2.98 in FNO2007, leads to B_i j=B*(C_i+C_j-C_k)/2,
c where k#i,j
            do k=j+1,nlegborn
               if(abs(bflav(k)).le.6) then
                  if(bflav(j).ne.0.and.bflav(k).ne.0) then
                     colcf=(2.*cf-ca)/2.
                  else
                     colcf=ca/2.
                  endif
                  bornjk(j,k)=born*colcf
                  bornjk(k,j)=bornjk(j,k)
               endif
            enddo
         endif
      enddo
      end


      subroutine compborn(p,bflav,born,bmunu)
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_math.h'
      include '../include/pwhg_flst.h'
      include '../include/pwhg_st.h'
      include 'PhysPars.h'
      integer nleg
      parameter (nleg=nlegborn)
      real * 8 p(0:3,nleg)
      integer bflav(nleg)
      real * 8 amp2,born,bmunu(0:3,0:3)
      integer i,j

cccccccccccccccccccccccccccccccc    
c     common bl. originally present in lh_readin, needed
c     by my_setpara
c
c     Common to lh_readin and printout
c
      double precision  alpha, gfermi, alfas
      double precision  mtMS,mbMS,mcMS,mtaMS!MSbar masses
      double precision  Vud,Vus,Vub,Vcd,Vcs,Vcb,Vtd,Vts,Vtb !CKM matrix elements
      common/values/    alpha,gfermi,alfas,   
     &                  mtMS,mbMS,mcMS,mtaMS,
     &                  Vud,Vus,Vub,Vcd,Vcs,Vcb,Vtd,Vts,Vtb
ccccccccccccccccccccccccccccccccc


cccccccccccccccccc
      integer mu,ileg
      real *8 kbcm(0:3,nleg)
      real *8 ewcoupl,s,t1,u1
      real *8 amp2_bg,amp2_gb
      real *8 dotp
      external dotp

      real *8 amp2_mad
cccccccccccccccc

      real *8 ktemp,kbcm_mad(0:3,nleg)
      integer ftemp,mflav(nleg)

      real *8 A0sq
      external A0sq

      logical ME_check
      parameter (ME_check=.false.)

      real *8 t,u,m2t,m2w,mttmp,mwtmp,tmp,tmp2,ckmtmp

      real *8 born_bmunu
      external born_bmunu

c     check
      if ((abs(bflav(3)).ne.24).or.(abs(bflav(4)).ne.6)) then
         write(*,*) 'born_ampsq: ERROR in flavor assignement'
         call exit(1)
      endif

c     local copy of variables
      do ileg=1,nleg
         do mu=0,3
            kbcm(mu,ileg)=p(mu,ileg)
         enddo
      enddo

c     ew coupling
      ewcoupl=4d0*pi*alphaem_pow/sthw2_pow


      
ccccccccccccccccccccccccccccccccccccccccccc
c     >>> WT CHANNEL <<<
ccccccccccccccccccccccccccccccccccccccccccc

c     USING MADGRAPH SUBROUTINES
      do ileg=1,4
         mflav(ileg)=bflav(ileg)
         do mu=0,3
            kbcm_mad(mu,ileg)=kbcm(mu,ileg)
         enddo
      enddo
c     to avoid bugs in HELAS, restore exact masslessness of incoming partons 
      kbcm_mad(0,1)=dabs(kbcm_mad(3,1))
      kbcm_mad(0,2)=dabs(kbcm_mad(3,2))
c     reassign here helas couplings and parameters that 
c     can change on an event-by-event basis
      alfas=st_alpha
      mtMS=sqrt(dotp(kbcm_mad(0,4),kbcm_mad(0,4)))
      tmass=mtMS
      twidth=0d0
      wwidth=0d0
      call my_setpara
c     invert 3rd and 4th particles before passing the array to
c     madgraph.
      ftemp=mflav(4)
      mflav(4)=mflav(3)
      mflav(3)=ftemp
      do mu=0,3
         ktemp=kbcm_mad(mu,4)
         kbcm_mad(mu,4)=kbcm_mad(mu,3)
         kbcm_mad(mu,3)=ktemp
      enddo
      
      call choose_born_process(kbcm_mad,mflav,amp2_mad)

ccccccccccccccccccccccccccccccccccccccccc

      if(ME_check) then
         m2w=dotp(kbcm(0,3),kbcm(0,3))
         m2t=dotp(kbcm(0,4),kbcm(0,4))

c     Mandelstam variables (as in 0805.3067)
         s=2d0*dotp(kbcm(0,1),kbcm(0,2))
         t1= -2d0*dotp(kbcm(0,1),kbcm(0,4))
         u1= -2d0*dotp(kbcm(0,1),kbcm(0,3))
     $        + dotp(kbcm(0,3),kbcm(0,3))
     $        -dotp(kbcm(0,4),kbcm(0,4)) 
         
         if(bflav(1).eq.0) then
            amp2_bg=(4*pi*st_alpha)*(ewcoupl/8.)/4./3./8.
            amp2_bg=amp2_bg
     $           *A0sq(s,u1,t1,m2t,m2w)

            if(dabs(amp2_bg/amp2_mad-1).gt.1d-5) then
               write(*,*) mflav(1),mflav(2),amp2_bg/amp2_mad
               write(*,*) 'Error 1 in Born.f'
               write(*,*) 'This check needs all CKM=1!'
               call exit(1)
            endif

         elseif(bflav(2).eq.0) then
c     the one in the MC@NLO paper
            amp2_gb=(4*pi*st_alpha)*(ewcoupl/8.)/4./3./8.
            amp2_gb=amp2_gb
     $           *A0sq(s,t1,u1,m2t,m2w)

            if(dabs(amp2_gb/amp2_mad-1).gt.1d-5) then
               write(*,*) mflav(1),mflav(2),amp2_gb/amp2_mad
               write(*,*) 'Error 2 in Born.f'
               write(*,*) 'This check needs all CKM=1!'
               call exit(1)
            endif

c     the one obtained with FeynCalc
            t=m2w-2*dotp(kbcm(0,1),kbcm(0,3))
            u=m2w+m2t-s-t
            mttmp=sqrt(m2t)
            mwtmp=sqrt(m2w)
            tmp=((2*(-2*mwtmp**6*s + mttmp**6*u - t**2*(s - t + u)*(s + t + u) - 
     -       2*mwtmp**2*t*(s**2 + t**2 - 3*t*u - 2*u**2 - s*(t + 2*u)) - 
     -       mttmp**4*(5*mwtmp**4 - 3*t**2 + (s + t)*u - mwtmp**2*(s - 3*t + 3*u)) + 
     -       mwtmp**4*(2*s**2 + 2*s*(t + u) - t*(t + 8*u)) + 
     -       mttmp**2*(mwtmp**4*(3*s + 6*t + 8*u) + mwtmp**2*(s**2 - 4*s*t + 5*t**2 - 5*s*u - 9*t*u - 4*u**2) + 
     -          t*(-2*t*(s + 2*t) + 2*s*u + u**2))))/(mwtmp**2*s*(mttmp**2 - t)**2))
            tmp=tmp*(4*pi*st_alpha)*(ewcoupl/8.)/3./8. *(3.*cf)

c            print*, 'bb',tmp/amp2_gb



            tmp2=((4*mttmp**2*mwtmp**4 + 8*mwtmp**6 - 4*mttmp**2*mwtmp**2*s - 8*mwtmp**4*s - 4*mttmp**4*t - 4*mttmp**2*mwtmp**2*t - 
     -       8*mwtmp**4*t + 4*mttmp**2*s*t + 8*mwtmp**2*s*t + 4*mttmp**2*t**2 + 
     -       2*mttmp**2*mwtmp**2*(s/2D0 + (-mwtmp**2 + t)/2D0) - 4*mwtmp**4*(s/2D0 + (-mwtmp**2 + t)/2D0) + 
     -       2*t**2*(s/2D0 + (-mwtmp**2 + t)/2D0) + 
     -       2*((mttmp**2*mwtmp**2 - 2*mwtmp**4 + t**2)*(s/2D0 + (-mwtmp**2 + t)/2D0) + 
     -          (mttmp**2 + 2*mwtmp**2)*(mwtmp**2 - t)*(-mwtmp**2 + (mwtmp**2 - t)/2D0 + (mwtmp**2 - u)/2D0)) + 
     -       2*mttmp**2*mwtmp**2*(-mwtmp**2 + (mwtmp**2 - t)/2D0 + (mwtmp**2 - u)/2D0) + 
     -       4*mwtmp**4*(-mwtmp**2 + (mwtmp**2 - t)/2D0 + (mwtmp**2 - u)/2D0) - 
     -       2*mttmp**2*t*(-mwtmp**2 + (mwtmp**2 - t)/2D0 + (mwtmp**2 - u)/2D0) - 
     -       4*mwtmp**2*t*(-mwtmp**2 + (mwtmp**2 - t)/2D0 + (mwtmp**2 - u)/2D0) + 4*mttmp**2*mwtmp**2*u - 8*mwtmp**4*u + 
     -       4*t**2*u)/(mttmp**2 - t)**2 + (2*mwtmp**4*s - 2*mwtmp**2*s**2 + 2*mttmp**2*s*(mwtmp**2 - t) + 
     -       2*mwtmp**2*s*(mwtmp**2 - t) - 2*s**2*(mwtmp**2 - t) + mttmp**2*s*t + 4*mwtmp**2*s*t - s**2*t - 
     -       4*mwtmp**2*s*(s/2D0 + (-mwtmp**2 + t)/2D0) - 3*mttmp**2*s*u - 4*mwtmp**2*s*u + 3*s**2*u - 
     -       s*(2*mwtmp**4 - 2*mwtmp**2*s - (mttmp**2 - s)*(t + u)))/s**2 - 
     -    (8*mttmp**2*mwtmp**2*s + 4*mwtmp**4*s - 4*mwtmp**2*s**2 - 2*mttmp**2*mwtmp**2*(mwtmp**2 - t) - 8*mttmp**2*mwtmp**2*t - 
     -       8*mwtmp**4*t - 2*mttmp**2*s*t - 2*mwtmp**2*s*t + 2*s**2*t + 4*mwtmp**2*(mwtmp**2 - t)*t + 4*mttmp**2*t**2 + 
     -       12*mwtmp**2*t**2 - 2*(mwtmp**2 - t)*t**2 - 4*t**3 - 12*mwtmp**4*(s/2D0 + (-mwtmp**2 + t)/2D0) + 
     -       2*mwtmp**2*t*(s/2D0 + (-mwtmp**2 + t)/2D0) + 4*t**2*(s/2D0 + (-mwtmp**2 + t)/2D0) + 
     -       4*mwtmp**2*s*(-mwtmp**2 + (mwtmp**2 - t)/2D0 + (mwtmp**2 - u)/2D0) + 
     -       2*s*t*(-mwtmp**2 + (mwtmp**2 - t)/2D0 + (mwtmp**2 - u)/2D0) + 2*mttmp**2*mwtmp**2*(mwtmp**2 - u) - 
     -       2*mttmp**2*t*(mwtmp**2 - u) + 8*mttmp**2*mwtmp**2*u + 8*mwtmp**4*u - 12*mwtmp**2*s*u - 
     -       2*mttmp**2*(mwtmp**2 - t)*u + 4*mwtmp**2*(mwtmp**2 - t)*u - 4*mttmp**2*t*u - 4*mwtmp**2*t*u - 
     -       2*(mwtmp**2 - t)*t*u + 4*mwtmp**2*(s/2D0 + (-mwtmp**2 + t)/2D0)*u + 
     -       2*t*(s/2D0 + (-mwtmp**2 + t)/2D0)*u - 8*mwtmp**2*u**2 + 4*t*u**2 - 
     -       4*mwtmp**4*(s/2D0 + (-mwtmp**2 + u)/2D0) + 2*mwtmp**2*t*(s/2D0 + (-mwtmp**2 + u)/2D0) + 
     -       2*t**2*(s/2D0 + (-mwtmp**2 + u)/2D0) + 
     -       2*(s*(2*mwtmp**2 - t)*(-mwtmp**2 + (mwtmp**2 - t)/2D0 + (mwtmp**2 - u)/2D0) - 
     -          (2*mwtmp**4 - 3*mwtmp**2*t + t**2)*(s/2D0 + (-mwtmp**2 + u)/2D0) + 
     -          (s/2D0 + (-mwtmp**2 + t)/2D0)*(2*mwtmp**4 + t*u - mwtmp**2*(4*s + t + 2*u))))/(s*(mttmp**2 - t)))/
     -  mwtmp**2
            tmp2=tmp2 *(4*pi*st_alpha)*(ewcoupl/8.)/3./8. *(3.*cf)
            


c      print*, '------',tmp/tmp2


            
         else
            write(*,*) 'Error in Born check'
            call exit(1)
         endif
      endif

ccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccc
c     assign output
      born=amp2_mad
cccccccccccccccccccccccccccccccc


cccccccccccccccccccccccccccccccccc
c     calculate and assign bmunu

      if(bflav(2).eq.0) then
c     THIS IS THE ONE 'OBTAINED' WITH FEYNCALC
c     the one obtained with FeynCalc
         m2w=dotp(kbcm(0,3),kbcm(0,3))
         m2t=dotp(kbcm(0,4),kbcm(0,4))
         s=2d0*dotp(kbcm(0,1),kbcm(0,2))
         t=m2w-2*dotp(kbcm(0,1),kbcm(0,3))
         u=m2w+m2t-s-t

         if(bflav(1).eq.1) then
            ckmtmp=Vtd**2
         elseif(bflav(1).eq.3) then
            ckmtmp=Vts**2
         elseif(bflav(1).eq.5) then
            ckmtmp=Vtb**2
         else
            write(*,*) 'Error in setborn'
            call exit(1)
         endif

         do i=0,3
            do j=0,3
               bmunu(i,j)=-born_bmunu(i,j,kbcm(0,1),kbcm(0,2),kbcm(0,3),kbcm(0,4),sqrt(m2t),sqrt(m2w),s,t,u)
     $*(4*pi*st_alpha)*(ewcoupl/8.)/3./8. *(3.*cf) *ckmtmp
            enddo
         enddo
      elseif(bflav(1).eq.0) then
         m2w=dotp(kbcm(0,3),kbcm(0,3))
         m2t=dotp(kbcm(0,4),kbcm(0,4))
         s=2d0*dotp(kbcm(0,1),kbcm(0,2))
         t=m2w-2*dotp(kbcm(0,2),kbcm(0,3))
         u=m2w+m2t-s-t

         if(bflav(2).eq.1) then
            ckmtmp=Vtd**2
         elseif(bflav(2).eq.3) then
            ckmtmp=Vts**2
         elseif(bflav(2).eq.5) then
            ckmtmp=Vtb**2
         else
            write(*,*) 'Error in setborn'
            call exit(1)
         endif

         do i=0,3
            do j=0,3
               bmunu(i,j)=-born_bmunu(i,j,kbcm(0,2),kbcm(0,1),kbcm(0,3),kbcm(0,4),sqrt(m2t),sqrt(m2w),s,t,u)
     $*(4*pi*st_alpha)*(ewcoupl/8.)/3./8. *(3.*cf) *ckmtmp
            enddo
         enddo
      endif
cccccccccccccccccccccc
      end



      function A0sq(s,t1,u1,m2t,m2w)
      implicit none
      real *8 A0sq
      real *8 s,t1,u1,m2t,m2w
      real *8 tmp

      tmp=-(s/u1+u1/s)*(1+m2t/2/m2w)
      tmp=tmp + 2*(1-m2t/2/m2w-m2t*m2t/2/m2w/m2w)*(m2w*(s+u1)*u1+m2t*m2w*s)/s/u1/u1
      tmp=tmp - (2-3*m2t/m2w+(m2t/m2w)**3)*m2w*m2w/s/u1
      tmp=tmp - m2t/m2w

      A0sq=16*3*(4./3.)*tmp
      return
      end



      subroutine borncolour_lh
c Sets up the colour for the given flavour configuration
c already filled in the Les Houches interface.
c In case there are several colour structure, one
c should pick one with a probability proportional to
c the value of the corresponding cross section, for the
c kinematics defined in the Les Houches interface
      include '../include/LesHouches.h'
      include 'nlegborn.h'
      integer ileg
      integer tgcol,bgcol
      data tgcol/501/
      data bgcol/502/

c     There are 2 possible (equivalent) colour structures
c     for single-top Wt-channel Born processes.
c     ttype is needed to handle the charge-conjugated process

c     gb
      if(abs(idup(1)).gt.5) then
c     g ggggg          ==>== t
c            g        /
c             g      /
c              -->--W
c             /      W
c     b -->--/        WWWWWW W
         icolup(1,1)=tgcol
         icolup(2,1)=bgcol
         icolup(1,2)=bgcol
         icolup(2,2)=0
c     bg
      elseif(abs(idup(2)).gt.5) then
c     b -->--\          ==>== t
c             \        /
c              \      /
c              g-->--W
c             g       W
c     g gggggg         WWWWW W
         icolup(1,1)=bgcol
         icolup(2,1)=0
         icolup(1,2)=tgcol
         icolup(2,2)=bgcol
      else
         write(*,*) 'Error 1 in borncolor_lh'
         call exit(1)
      endif

c     final state
      do ileg=3,nlegborn
         if(abs(idup(ileg)).eq.24) then
            icolup(1,ileg)=0
            icolup(2,ileg)=0
         elseif(abs(idup(ileg)).eq.6) then
            icolup(1,ileg)=tgcol
            icolup(2,ileg)=0
         else
            write(*,*) 'Error 2 in borncolor_lh'
            call exit(1)
         endif
      enddo

c     if charge-conjugated process, exchange color
c     with anticolors
      if(ttype.lt.0) then
         do ileg=1,4
            tmp=icolup(1,ileg)
            icolup(1,ileg)=icolup(2,ileg)
            icolup(2,ileg)=tmp
         enddo
      endif

      end


      subroutine resonances_lh
c     Set up the resonances whose mass must be preserved
c     on the Les Houches interface.

c     Keep the top undecayed, for now.
c     Therefore, no changes are needed.
c     Maybe the top istup have to be set to 2 ???
      end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     !: beginning of subroutines needed for the decay
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc


c     !ER: dovrebbe essere ok, ma e' da testare bene
      subroutine pickwdecay(iw1,mdecw1,iw2,mdecw2,iw3,mdecw3,iw4,mdecw4,
     $     totbr)
c     !: originally taken from POWHEG-hvq
c     Finds which decays to choose with correct probability, according
c     to topdecaymode and wdecaymode. 
c     It returns always particle ids of W+ decay and the primary W- decay.

c     iw1, iw2 refer to the pdg ids of W+ decay products 1 and 2;
c     iw3, iw4 refer to the pdg ids of W- decay products 1 and 2;

c     by convention 1 is down type (e,mu,tau,d,s) and 2 is up type.

c     topdecaymode has to be an integer with 5 digits that are either 0 or 1
c     and they represent respectively the maximum number of the following particles
c     (antiparticles) in the top decay final state:
c     e  mu tau up charm
c     Relevant examples:
c     11111    All decays
c     leptonic:
c     10000    t->(b e ve) (with the appropriate signs)
c     11000    t->(b e ve) or t->(b mu vmu) (with the appropriate signs)
c     11100    fully leptonic
c     hadronic:
c     00010    t->(b u d) (with the appropriate signs)
c     00001    t->(b c s) (with the appropriate signs)
c     00011    fully hadronic

c     Same rules apply to wdecaymode.

      implicit none
      include 'PhysPars.h'
      integer iw1,iw2,iw3,iw4
      real * 8 mdecw1,mdecw2,mdecw3,mdecw4,totbr
c     local
      integer iwa(2)
      real * 8 prbs(1:5),totprbs(0:5),mass(16),sin2cabibbo,ebr,hbr,r,
     $     prbsw(1:5),totprbsw(0:5)
      integer ini,ii(5),j,k,imode,iwp(5,2),imodew,iiw(5),iwm(5,2)
      data ini/0/
c     pdg id's of the W+ decay products for e,mu,tau,up and charm decays (ignoring CKM)
      data ((iwp(j,k),k=1,2),j=1,5)/-11,12, -13,14, -15,16, -1,2, -3,4/
c     pdg id's of the W- decay products for e,mu,tau,up and charm decays (ignoring CKM)
      data ((iwm(j,k),k=1,2),j=1,5)/11,-12, 13,-14, 15,-16, 1,-2, 3,-4/

c     external
      real * 8 random,powheginput
      external random,powheginput
c     save
      save ini,totprbs,iwp,mass,sin2cabibbo,totprbsw,iwm
      if(ini.eq.2) return
      if(ini.eq.0) then
         ini=1
c     on first run look for decay mode in powheginput
         imode=powheginput('topdecaymode')
         if(imode.le.0) then
            write(*,*) 'Invalid value for tdecaymode, in pickwdecay'
            call exit(1)
         endif
c     on first run look for W decay mode in powheginput
         imodew=powheginput('wdecaymode')
         if(imodew.le.0) then
            write(*,*) 'Invalid value for wdecaymode, in pickwdecay'
            call exit(1)
         endif
c$$$         if(imode.eq.0) then
c$$$            ini=2
c$$$            return
c$$$         endif
         ii(1)=imode/10000
         imode=imode-ii(1)*10000
         ii(2)=imode/1000
         imode=imode-ii(2)*1000
         ii(3)=imode/100
         imode=imode-ii(3)*100
         ii(4)=imode/10
         imode=imode-ii(4)*10
         ii(5)=imode

         iiw(1)=imodew/10000
         imodew=imodew-iiw(1)*10000
         iiw(2)=imodew/1000
         imodew=imodew-iiw(2)*1000
         iiw(3)=imodew/100
         imodew=imodew-iiw(3)*100
         iiw(4)=imodew/10
         imodew=imodew-iiw(4)*10
         iiw(5)=imodew

c     load from input card the branching t->(b l vl) (only one lepton flavour)
         ebr=powheginput('tdec/elbranching')
c     from ebr calculates the hadronic branching t->(b u d)
         hbr=(1-3*ebr)/2
c     Probabilities for top decay
         do j=1,5
            if(ii(j).eq.0) then
               prbs(j)=0
            else
               if(j.le.3) then
                  prbs(j)=ebr
               else
                  prbs(j)=hbr
               endif
            endif
         enddo
c     now in prbs(j) there is the branching ratio assumed by the program for the
c     j-type decay. If prbs(j)=0, the corresponding decay channel will be closed.
         totprbs(0)=0d0
         do j=1,5
            totprbs(j)=prbs(j)+totprbs(j-1)
         enddo

c     Do the same for the branching of the primary W. Same
c     branching used.
         do j=1,5
            if(iiw(j).eq.0) then
               prbsw(j)=0
            else
               if(j.le.3) then
                  prbsw(j)=ebr
               else
                  prbsw(j)=hbr
               endif
            endif
         enddo
c     now in prbsw(j) there is the branching ratio assumed by the program for the
c     j-type decay. If prbsw(j)=0, the corresponding decay channel will be closed.
         totprbsw(0)=0d0
         do j=1,5
            totprbsw(j)=prbsw(j)+totprbsw(j-1)
         enddo

ccccccccccccccccccccccccccccccccccccccccc
         totbr=totprbs(5)*totprbsw(5)
ccccccccccccccccccccccccccccccccccccccccc

c     mass of decay products. For internal consistency, here one should use
c     the masses assumed by the shower. Leptonic W decay products masses have to be
c     assigned here. The 3 light quarks are assumed massless.
         mass(11)=powheginput('tdec/emass')
         mass(13)=powheginput('tdec/mumass')
         mass(15)=powheginput('tdec/taumass')
         mass(12)=0
         mass(14)=0
         mass(16)=0
         mass(1)=0
         mass(2)=0
         mass(3)=0
c         mass(5)=powheginput('tdec/bmass')
         mass(5)=powheginput('#bottomthr')
         if (mass(5).lt.0d0) mass(5)=5.
c         mass(4)=powheginput('tdec/cmass')
         mass(4)=powheginput('#charmthr')
         if (mass(4).lt.0d0) mass(4)=1.5
         sin2cabibbo=(CKM_pow(1,2))**2
         if(iw1.eq.-1000) return
      endif
c     end initialization

ccccccccccccccccccccccccccccccccccccccccccccccccccc
c     choice for top decay mode (W from top decay)
ccccccccccccccccccccccccccccccccccccccccccccccccccc
      r=random()*totprbs(5)
      do j=1,5
         if(r.lt.totprbs(j)) goto 1
      enddo
 1    continue
c     now we have j decay mode
      if(j.gt.5) then
         write(*,*) 'Error 1 in pickwdecay, j',r,totprbs
         call exit(1)
      endif

c     W decay products
      iwa(1)=iwp(j,1)
      iwa(2)=iwp(j,2)
c     if any W decay product is down (or strange), it may turn to
c     strange (or down) with a probability sin^2 theta
      do j=1,2
         if(abs(iwa(j)).eq.1) then
            if(random().lt.sin2cabibbo) then
               iwa(j)=sign(3,iwa(j))
            endif
         elseif(abs(iwa(j)).eq.3) then
            if(random().lt.sin2cabibbo) then
               iwa(j)=sign(1,iwa(j))
            endif
         endif
      enddo
      iw1=iwa(1)
      iw2=iwa(2)
      mdecw1=mass(abs(iw1))
      mdecw2=mass(abs(iw2))

cccccccccccccccccccccccccccccccccccccc
c     choice for primary W decay mode
cccccccccccccccccccccccccccccccccccccc
      r=random()*totprbsw(5)
      do j=1,5
         if(r.lt.totprbsw(j)) goto 2
      enddo
 2    continue
c     now we have j decay mode
      if(j.gt.5) then
         write(*,*) 'Error 2 in pickwdecay, j',r,totprbsw
         call exit(1)
      endif

c     W decay products
      iwa(1)=iwm(j,1)
      iwa(2)=iwm(j,2)
c     if any W decay product is down (or strange), it may turn to
c     strange (or down) with a probability sin^2 theta
      do j=1,2
         if(abs(iwa(j)).eq.1) then
            if(random().lt.sin2cabibbo) then
               iwa(j)=sign(3,iwa(j))
            endif
         elseif(abs(iwa(j)).eq.3) then
            if(random().lt.sin2cabibbo) then
               iwa(j)=sign(1,iwa(j))
            endif
         endif
      enddo
      iw3=iwa(1)
      iw4=iwa(2)
      mdecw3=mass(abs(iw3))
      mdecw4=mass(abs(iw4))

      end



c     !: unused
c$$$      subroutine momenta_reshuffle(ires,i1,i2,decmass)
c$$$      implicit none
c$$$      end
