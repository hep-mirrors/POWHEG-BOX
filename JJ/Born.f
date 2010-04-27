      subroutine setborn(p,bflav,born,bornjk,bmunu)
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_math.h'
      include '../include/pwhg_st.h'
      include '../include/pwhg_kn.h'
      include 'PhysPars.h'

      integer nlegs
      parameter (nlegs=nlegborn)
      real * 8 p(0:3,nlegs),bornjk(nlegs,nlegs)
      integer bflav(nlegs)
      real * 8 bmunu(0:3,0:3,nlegs),born
      real *8 borntmp
      integer mu,nu,j,k
c To test against original Emanuele's program
c      real * 8 bornx,bornjkx(nlegs,nlegs),bmunux(0:3,0:3,nlegs)

      integer ileg,ioleg
C     define a real *8 value for nc in order
C     to avoid integer by integer division
      real *8 ncol
      parameter (ncol=nc)
      real *8 VC
      parameter(VC=ncol**2-1)

C     Abbreviation of (4.*pi*st_alpha)**2
      real*8 gs4

      real * 8 gtens(0:3,0:3),ap
      data gtens/1d0, 0d0, 0d0, 0d0,
     #           0d0,-1d0, 0d0, 0d0,
     #           0d0, 0d0,-1d0, 0d0,
     #           0d0, 0d0, 0d0,-1d0/
      save gtens

      real *8  HAt  ,HAu  ,HB  ,HCs  ,HCt  ,HCu
      real *8  HAtFn,HAuFn,HBFn,HCsFn,HCtFn,HCuFn,HDsFn,HDtFn,HDuFn,dotp
      external HAtFn,HAuFn,HBFn,HCsFn,HCtFn,HCuFn,HDsFn,HDtFn,HDuFn,dotp
      real * 8 symfac

c     The process class according to Kunszt Soper
      character*1  ks_label
c     The map from POWHEG-BOX to Kunszt-Soper particles and momenta.
      integer ksmap(4)
c     Kunszt-Soper momenta analogous to POWHEG-BOX.
      real*8  k1(0:3),k2(0:3),k3(0:3),k4(0:3)
c     Variables to hold spin / colour average factor (& overall +/- sign)
      real*8 spin_col_avg

C     Setting the QCD coupling squared.
      gs4 = (4.*pi*st_alpha)**2

c     Get the Kunszt Soper and Madgraph labels as well as the map
c     to Kunszt Soper conventions:
      call ks_2to2_map(bflav,nlegborn,ks_label,ksmap)  
c     Assign Kunszt Soper momenta
      do mu=0,3
        p(mu,3)=-p(mu,3)      ! temprarily flip the outgoing momenta
        p(mu,4)=-p(mu,4)      !
        k1(mu)=p(mu,ksmap(1)) ! setting the Kunszt Soper momenta using
        k2(mu)=p(mu,ksmap(2)) ! the Kunszt Soper map.
        k3(mu)=p(mu,ksmap(3)) ! 
        k4(mu)=p(mu,ksmap(4)) !
        p(mu,3)=-p(mu,3)      ! restore the powheg outgoing momenta
        p(mu,4)=-p(mu,4)      !
      enddo      

c Colour factors for colour-correlated Born amplitudes;
c Rule from Kunszt-Soper.
c First, identify the flavour structure

C --------------------------------------------------------------------
C     A-type: q + Q -> q + Q plus charge conjugations and crossings
C --------------------------------------------------------------------
      if(ks_label.eq.'A') then
         spin_col_avg = 4.*ncol*ncol

         HAt = HAtFn(k1,k2,k3,k4)

         bornjk(ksmap(1),ksmap(2)) = 2*VC/nc*HAt
         bornjk(ksmap(1),ksmap(3)) =  -VC/nc*HAt
         bornjk(ksmap(1),ksmap(4)) =   VC/nc*(nc**2-2)*HAt

         bornjk(ksmap(3),ksmap(4)) = bornjk(ksmap(1),ksmap(2))
         bornjk(ksmap(2),ksmap(4)) = bornjk(ksmap(1),ksmap(3))
         bornjk(ksmap(2),ksmap(3)) = bornjk(ksmap(1),ksmap(4))

C --------------------------------------------------------------------
C     B-type: q + q -> q + q plus charge conjugations
C --------------------------------------------------------------------
      elseif(ks_label.eq.'B') then
         spin_col_avg = 4.*ncol*ncol

         HB  = HBFn(k1,k2,k3,k4)
         HAt = HAtFn(k1,k2,k3,k4)
         HAu = HAuFn(k1,k2,k3,k4)

         bornjk(ksmap(1),ksmap(2)) = 2*VC/nc/nc *
     $                               (-HB+nc*(HAt+HAu)-nc**2*HB)
         bornjk(ksmap(1),ksmap(3)) = 2*VC/nc/nc *
     $                               (HB-nc*(HAu+0.5*HAt)+0.5*nc**3*HAu)
         bornjk(ksmap(1),ksmap(4)) = 2*VC/nc/nc *
     $                               (HB-nc*(HAt+0.5*HAu)+0.5*nc**3*HAt)

         bornjk(ksmap(3),ksmap(4)) = bornjk(ksmap(1),ksmap(2))
         bornjk(ksmap(2),ksmap(4)) = bornjk(ksmap(1),ksmap(3))
         bornjk(ksmap(2),ksmap(3)) = bornjk(ksmap(1),ksmap(4))

C --------------------------------------------------------------------
C     C-type: q + qb -> g + g plus charge conjugations & crossings
C --------------------------------------------------------------------
      elseif(ks_label.eq.'C') then

         spin_col_avg=1
         do k=1,2
            if(bflav(k).ne.0) then
c - sign for crossing fermion line
               spin_col_avg=-spin_col_avg*(2*ncol)
            else
               spin_col_avg=spin_col_avg*(2*Vc)
            endif
         enddo

         HCs = HCsFn(k1,k2,k3,k4)
         HCt = HCtFn(k1,k2,k3,k4)
         HCu = HCuFn(k1,k2,k3,k4)

         bornjk(ksmap(1),ksmap(2)) = VC*(-(HCt+HCu-HCs) + 1./nc/nc*HCs )
         bornjk(ksmap(1),ksmap(3)) = VC*(  nc**2*HCu-HCs )
         bornjk(ksmap(1),ksmap(4)) = VC*(  nc**2*HCt-HCs )

         bornjk(ksmap(2),ksmap(4)) = bornjk(ksmap(1),ksmap(3))
         bornjk(ksmap(2),ksmap(3)) = bornjk(ksmap(1),ksmap(4))
         bornjk(ksmap(3),ksmap(4)) = VC*nc**2 * (HCt+HCu)

C --------------------------------------------------------------------
C     D-type: g + g -> g + g
C --------------------------------------------------------------------
      elseif(ks_label.eq.'D') then
         spin_col_avg =  4.*VC*VC

         bornjk(ksmap(1),ksmap(2)) = 2*VC*nc**3*HDsFn(k1,k2,k3,k4)
         bornjk(ksmap(1),ksmap(3)) = 2*VC*nc**3*HDtFn(k1,k2,k3,k4)
         bornjk(ksmap(1),ksmap(4)) = 2*VC*nc**3*HDuFn(k1,k2,k3,k4)

         bornjk(ksmap(2),ksmap(4)) = bornjk(ksmap(1),ksmap(3))
         bornjk(ksmap(2),ksmap(3)) = bornjk(ksmap(1),ksmap(4))
         bornjk(ksmap(3),ksmap(4)) = bornjk(ksmap(1),ksmap(2))

      else
         write(*,*) 'setborn: could not identify flavour list!'
         call exit(1)
      endif



C --------------------------------------------------------------------
C     Symmetrize and normalise bornjk matrix
C --------------------------------------------------------------------
      do j=1,nlegborn
C - bornjk(j,j) is not used in soft
         bornjk(j,j)=0d0
         do k=j+1,nlegborn
            bornjk(ksmap(k),ksmap(j))=bornjk(ksmap(j),ksmap(k))
         enddo
      enddo

c Normalize: Kunszt and Soper have an extra 2, see eq A8 and A11 in
c PRD46-192
      if(bflav(3).eq.bflav(4)) then
         symfac=0.5d0
      else
         symfac=1
      endif
      do j=1,nlegborn
         do k=1,nlegborn
            bornjk(j,k)=bornjk(j,k)/2*symfac*gs4/spin_col_avg
         enddo
      enddo

      
      born=0
      DO J=2,nlegborn
         born=born+bornjk(1,j)
      enddo
      if(bflav(1).eq.0) then
         born=born/ca
      else
         born=born/cf
      endif
c     spin-projected here are trivial:
      do ileg=1,nlegborn
         if(bflav(ileg).eq.0) then
c find opposite leg
            if(ileg.eq.1) then
               ioleg=2
            elseif(ileg.eq.2) then
               ioleg=1
            elseif(ileg.eq.3) then
               ioleg=4
            elseif(ileg.eq.4) then
               ioleg=3
            endif
            do mu=0,3
               do nu=0,3
                  bmunu(mu,nu,ileg)=(-gtens(mu,nu)+
     1           (kn_cmpborn(mu,ileg)*kn_cmpborn(nu,ioleg)
     2           +kn_cmpborn(nu,ileg)*kn_cmpborn(mu,ioleg))/
     2            dotp(kn_cmpborn(0,ileg),kn_cmpborn(0,ioleg)))*born/2
               enddo
            enddo
         endif
      enddo
c Tested OK against original Emanuele's program
c      call setborn_ema(p,bflav,bornx,bornjkx,bmunux)
c      if(abs(bornx/born-1).gt.1d-6) then
c         write(*,*) ' Born: ',bflav,bornx/born
c      endif
c      do j=1,nlegborn
c         do k=1,nlegborn
c            if(j.ne.k.and.abs(bornjkx(j,k)/bornjk(j,k)-1).gt.1d-6) then
c               write(*,*) ' Born: ',bflav,bornjkx(j,k)/bornjk(j,k)
c            endif
c         enddo
c      enddo
      end


C - The following functions are taken from Kunszt & Soper Phys.Rev.D46,1 192 

      function HAtFn(p1,p2,p3,p4)
      real *8 HAtFn
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      real *8 s,t,u
      real *8 dotp
      external dotp
      s=2.*dotp(p1,p2)
      t=2.*dotp(p1,p3)
      u=2.*dotp(p1,p4)
      HAtFn=2.*(s**2+u**2)/t**2
      return
      end

      function HAuFn(p1,p2,p3,p4)
      real *8 HAuFn,HAtFn
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      HAuFn=HAtFn(p1,p2,p4,p3)
      return
      end


      function HBFn(p1,p2,p3,p4)
      real *8 HBFn
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      real *8 s,t,u
      real *8 dotp
      external dotp
      s=2.*dotp(p1,p2)
      t=2.*dotp(p1,p3)
      u=2.*dotp(p1,p4)
      HBFn=2.*s**2/t/u
      return
      end


      function HCtFn(p1,p2,p3,p4)
      real *8 HCtFn
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      real *8 s,t,u
      real *8 dotp
      external dotp
      s=2.*dotp(p1,p2)
      t=2.*dotp(p1,p3)
      u=2.*dotp(p1,p4)
      HCtFn=(2.*(t**2+u**2)/s**2 )*t/u
      return
      end

      function HCuFn(p1,p2,p3,p4)
      real *8 HCuFn,HCtFn
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      HCuFn=HCtFn(p1,p2,p4,p3)
      return
      end

      function HCsFn(p1,p2,p3,p4)
      real *8 HCsFn
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      real *8 s,t,u
      real *8 dotp
      external dotp
      s=2.*dotp(p1,p2)
      t=2.*dotp(p1,p3)
      u=2.*dotp(p1,p4)
      HCsFn=2.*(t**2+u**2)/t/u
      return
      end


      function HDsFn(p1,p2,p3,p4)
      real *8 HDsFn
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      real *8 s,t,u
      real *8 dotp
      external dotp
      s=2.*dotp(p1,p2)
      t=2.*dotp(p1,p3)
      u=2.*dotp(p1,p4)
      HDsFn=2.*(t**2+u**2) *(s**4+t**4+u**4)/(s*t*u)**2
      return
      end

      function HDtFn(p1,p2,p3,p4)
      real *8 HDtFn,HDsFn
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      HDtFn=HDsFn(p1,p3,p2,p4)
      return
      end

      function HDuFn(p1,p2,p3,p4)
      real *8 HDuFn,HDsFn
      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3)
      HDuFn=HDsFn(p1,p4,p2,p3)
      return
      end


c Now in external file borncolour.f. 
c Will be put back here when finished.
c      subroutine borncolour_lh
c Sets up the colour for the given flavour configuration
c already filled in the Les Houches interface.
c In case there are several colour structure, one
c should pick one with a probability proportional to
c the value of the corresponding cross section, for the
c kinematics defined in the Les Houches interface
c      implicit none
c
cc     TO BE COMPLETED
c      
c      write(*,*) "borncolour_lh TO BE COMPLETED!!" 
c      call exit(1)
c
c      end


      subroutine resonances_lh
c     Set up the resonances whose mass must be preserved
c     on the Les Houches interface.
c     Before that, call the routine that generates the decay.
c     Notice that the current subroutine is called at the end
c     of gen_leshouches (or gen_leshouches_reg). This means
c     that the overall azimuthal rotation has been already
c     performed (add_azimuth called in pwhgevent). 
      implicit none

c     TO BE COMPLETED
      
      write(*,*) "resonances_lh TO BE COMPLETED (not needed ??) " 
      call exit(1)

      end
