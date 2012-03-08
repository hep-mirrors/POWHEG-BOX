      subroutine setreal(p,fermion_flav,amp2)
      implicit none
      include 'nlegborn.h'
*
      real * 8 p(0:3,nlegreal)
      integer fermion_flav(nlegreal)
      real * 8 amp2
*
      if(fermion_flav(nlegreal).eq.22) then

          call setreal_ew(p,fermion_flav,amp2)

      else

          call setreal_st(p,fermion_flav,amp2)

      endif
*
      end


      subroutine setreal_st(p,fermion_flav,amp2)
      implicit none
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'mathx.h'
      include 'pwhg_math.h'
      include 'pwhg_st.h'
c -*- Fortran -*-
c      character *2 flav(-5:5)
      real * 8 charge(-5:5)
c      data (charge(ijkh),ijkh=-5,5) 
c      data (flav(ijkh),ijkh=-5,5) 
c      data flav
c     #     /'b~','c~','s~','u~','d~','g','d','u','s','c','b'/
      data charge
     #     / 0.33333333333333333333d0, !   1d0/3
     #      -0.66666666666666666667d0, !  -2d0/3
     #       0.33333333333333333333d0, !   1d0/3 
     #      -0.66666666666666666667d0, !   -2d0/3
     #       0.33333333333333333333d0, !   1d0/3 
     #       0d0,                      !   0d0   
     #      -0.33333333333333333333d0, !   -1d0/3
     #       0.66666666666666666667d0, !   2d0/3   
     #      -0.33333333333333333333d0, !   -1d0/3
     #       0.66666666666666666667d0, !   2d0/3 
     #      -0.33333333333333333333d0/ !   -1d0/3
c      include 'QuarkFlavs.h'
      include 'PhysPars.h'
      integer nleg
      parameter (nleg=nlegreal)
      real * 8 p(0:3,nleg)
      integer fermion_flav(nleg)
      real * 8 amp2
      integer ferm_type(nleg)
      real * 8 ferm_charge(nleg)
      integer i,j,k,l,count,tmp_type
      real *8 tmp_charge
c     vector boson id and decay
      integer idvecbos,vdecaymode
      common/cvecbos/idvecbos,vdecaymode  

c     i is the flavour index of first incoming parton
c     j is the flavour index of second incoming parton
c     k is the flavour of outgoing parton in the order particle,antiparticle,gluon
c     with the convention:
c     
c      -6  -5  -4  -3  -2  -1  0  1  2  3  4  5  6                    
c      t~  b~  c~  s~  u~  d~  g  d  u  s  c  b  t                    
      
      i = fermion_flav(1)
      j = fermion_flav(2)
      k = fermion_flav(5)
      ferm_charge(1) = charge(i)
      ferm_charge(2) = charge(j)
      ferm_charge(5) = charge(k)
      

      if (i.eq.0) then
         ferm_type(1) = 0
      else 
         ferm_type(1) = i/abs(i)
      endif 
      if (j.eq.0) then
         ferm_type(2) = 0
      else 
         ferm_type(2) = j/abs(j)
      endif   
      if (k.eq.0) then
         ferm_type(5) = 0
      else 
         ferm_type(5) = k/abs(k)
      endif   

c     antilepton-neutrino from W decay
      ferm_type(3) = fermion_flav(3)/abs(fermion_flav(3))
      ferm_charge(3) = ferm_type(3)*(-1d0)
      ferm_type(4) = -ferm_type(3)
      ferm_charge(4) = 0d0

      if(idvecbos.eq.24) then
         if (i.eq.0) then
c     g q -> W+ qp
            call g_aqp_to_al_vl_aq(p,ferm_type,ferm_charge,amp2) 
         elseif ((i.ne.0).and.(j.ne.0)) then
c     q aqp -> W+ g
            call q_aqp_to_al_vl_g(p,ferm_type,ferm_charge,amp2)
         elseif (j.eq.0) then
c     q g -> W+ qp
            call q_g_to_al_vl_qp(p,ferm_type,ferm_charge,amp2)
         else
            amp2 = 0d0
         endif
      elseif(idvecbos.eq.-24) then
         if (i.eq.0) then
c     g q -> W- qp
            call g_aqp_to_l_avl_aq(p,ferm_type,ferm_charge,amp2) 
         elseif ((i.ne.0).and.(j.ne.0)) then
c     q aqp -> W- g
            call q_aqp_to_l_avl_g(p,ferm_type,ferm_charge,amp2)
         elseif (j.eq.0) then
c     q g -> W- qp
            call q_g_to_l_avl_qp(p,ferm_type,ferm_charge,amp2)
         else
            amp2 = 0d0
         endif

      else
         write(*,*) 'ERROR: this subroutine deals only with W+ or W- '
         call exit(1)
      endif

      if (i.eq.0) i=abs(k)
      if (j.eq.0) j=abs(k)
      if(mod(abs(i),2).eq.0) then
         amp2=amp2*ph_CKM(abs(i)/2,(abs(j)+1)/2)**2
      elseif(mod(abs(i),2).eq.1) then   
         amp2=amp2*ph_CKM(abs(j)/2,(abs(i)+1)/2)**2
      endif
c     cancel as/(2pi) associated with amp2. It will be put back by real_ampsq
      amp2 = amp2/(st_alpha/(2*pi))
      
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AMPLITUDES RELATES BY CROSSING:
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine g_aqp_to_al_vl_aq(pphy,fermion_type,fermion_charge,
     #     amp2)
      implicit none
      include 'nlegborn.h'
      integer nleg
      parameter (nleg=nlegreal)
      integer fermion_type(nleg)
      real * 8 fermion_charge(nleg)
      real * 8 pphy(0:3,nleg)
      real * 8 pp(0:3,nleg), ferm_charge(nleg)
      integer ferm_type(nleg)
      real * 8 amp2
      integer mu,i

      do i = 1,nleg
         do mu=0,3
            pp(mu,i) = pphy(mu,i)
         enddo
         ferm_charge(i) = fermion_charge(i)
         ferm_type(i) = fermion_type(i)
      enddo

      do mu=0,3
c     exchange initial gluon <-> final quark
         pp(mu,5) = -pphy(mu,1)
         pp(mu,1) = -pphy(mu,5)
      enddo

c no useful information is in ferm_type(1) or ferm_charge(1), 
c since it's the gluon, BEFORE the following exchange
      ferm_type(1) = -ferm_type(5)
c NOTE the MINUS sign     !!!
      ferm_charge(1) = -ferm_charge(5)

c     if the following two lines are missing 
      ferm_type(5)=0
      ferm_charge(5)=0d0 
c     ferm_type(5) and ferm_charge(5) don't contain
c     their correct values, but this does not affect 
c     the correct call of

       call q_aqp_to_al_vl_g(pp,ferm_type,ferm_charge,
     #     amp2)

c     correct for color average
      amp2 = amp2 * 3d0/8d0
      
      end

ccccccccccccccccccccccccccccccccccccccccccccc

      subroutine q_g_to_al_vl_qp(pphy,fermion_type,fermion_charge,
     #     amp2)
   
      implicit none
      integer nleg
      parameter (nleg=5)
      integer fermion_type(nleg)
      real * 8 fermion_charge(nleg)
      real * 8 pphy(0:3,nleg)
      real * 8 pp(0:3,nleg), ferm_charge(nleg)
      integer ferm_type(nleg)
      real * 8 amp2
      integer mu,i

      do i = 1,nleg
         do mu=0,3
            pp(mu,i) = pphy(mu,i)
         enddo
         ferm_charge(i) = fermion_charge(i)
         ferm_type(i) = fermion_type(i)
      enddo

      do mu=0,3
c     exchange initial gluon <-> final quark
         pp(mu,5) = -pphy(mu,2)
         pp(mu,2) = -pphy(mu,5)
      enddo

c no useful information is in ferm_type(2) or ferm_charge(2), 
c since it's the gluon, BEFORE the following exchange
      ferm_type(2) = -ferm_type(5)
c NOTE the MINUS sign     !!!
      ferm_charge(2) = -ferm_charge(5)

c     if the following two lines are missing 
      ferm_type(5)=0
      ferm_charge(5)=0d0 
c     ferm_type(5) and ferm_charge(5) don't contain
c     their correct values, but this does not affect 
c     the correct call of

       call q_aqp_to_al_vl_g(pp,ferm_type,ferm_charge,
     #     amp2)

c     correct for color average
      amp2 = amp2 * 3d0/8d0
      
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      subroutine q_aqp_to_l_avl_g(pphy,fermion_type,fermion_charge,
     #     amp2)
   
      implicit none
      include 'nlegborn.h'
c the 5 4-momentum vectors
c p(i,1) is the i-th component of vector p1...   
      integer nleg
      parameter (nleg=nlegreal)
      integer fermion_type(nleg),i
      real * 8 fermion_charge(nleg)
      real * 8 pphy(0:3,nleg)
      real * 8 amp2
      real * 8 ferm_charge(nleg)
      integer ferm_type(nleg)

       if ((fermion_type(3).ne.1).and.(fermion_type(4).ne.-1)) then
         write(*,*) 'ERROR: this subroutine deals only with W- decay'
         stop
      endif

      do i=1,nleg
         ferm_charge(i) = -fermion_charge(i)
         ferm_type(i) = -fermion_type(i)
      enddo
            
      
      call q_aqp_to_al_vl_g(pphy,ferm_type,ferm_charge,
     #     amp2)

      end

ccccccccccccccccccccccccccccccccccccccccccccc

       subroutine g_aqp_to_l_avl_aq(pphy,fermion_type,fermion_charge,
     #     amp2)
   
      implicit none
      include 'nlegborn.h'
c the 5 4-momentum vectors
c p(i,1) is the i-th component of vector p1...   
      integer nleg
      parameter (nleg=nlegreal)
      integer fermion_type(nleg),i
      real * 8 fermion_charge(nleg)
      real * 8 pphy(0:3,nleg)
      real * 8 amp2
      real * 8 ferm_charge(nleg)
      integer ferm_type(nleg)

       if ((fermion_type(3).ne.1).and.(fermion_type(4).ne.-1)) then
         write(*,*) 'ERROR: this subroutine deals only with W- decay'
         stop
      endif

      do i=1,nleg
         ferm_charge(i) = -fermion_charge(i)
         ferm_type(i) = -fermion_type(i)
      enddo
      
      call g_aqp_to_al_vl_aq(pphy,ferm_type,ferm_charge,
     #     amp2)

      end

ccccccccccccccccccccccccccccccccccccccccccccc

       subroutine q_g_to_l_avl_qp(pphy,fermion_type,fermion_charge,
     #     amp2)
   
      implicit none
c the 5 4-momentum vectors
c p(i,1) is the i-th component of vector p1...   
      integer nleg
      parameter (nleg=5)
      integer fermion_type(nleg),i
      real * 8 fermion_charge(nleg)
      real * 8 pphy(0:3,nleg)
      real * 8 amp2
      real * 8 ferm_charge(nleg)
      integer ferm_type(nleg)

      if ((fermion_type(3).ne.1).and.(fermion_type(4).ne.-1)) then
         write(*,*) 'ERROR: this subroutine deals only with W- decay'
         stop
      endif

      do i=1,nleg
      
         ferm_charge(i) = -fermion_charge(i)
         ferm_type(i) = -fermion_type(i)
      enddo
      
      call q_g_to_al_vl_qp(pphy,ferm_type,ferm_charge,
     #     amp2)

      end
*
** Subroutine for strong real part
** mu = md = 0
**
**(pu) u \            / l+ (pl)
**        \---gl     /
**         \________/
**         /   W+   \       (gl can be emitted by both initial leg)
**        /          \
**(pd) d~/            \ nu_l (pn)
*
      subroutine q_aqp_to_al_vl_g(pphy,fermion_type,fermion_charge,amp2)
      implicit none
      include 'nlegborn.h'
      include 'PhysPars.h'
      include 'pwhg_st.h'
      include 'pwhg_kn.h'
      include 'mathx.h'
      include 'pwhg_math.h'
      include 'pwhg_physpar.h'
*
      integer nleg
      parameter (nleg=nlegreal)
      real * 8 pphy(0:3,nleg)
      integer fermion_type(nleg)
      real * 8 fermion_charge(nleg)
      real * 8 amp2
*
      real*8 dotp
      external dotp
*
      real *8 lepmass(3),decmass
      common/clepmass/lepmass,decmass
*
      real*8 p(0:3,nleg)
      integer ferm_type(nleg)
      real*8 ferm_charge(nleg)
      real*8 pu(0:3),pd(0:3),pn(0:3),pl(0:3),k(0:3)
      real*8 sp

      integer i,j,nu

      real*8 pupd,pupe,pupn,puk
      real*8 pdpe,pdpn,pdk
      real*8 pnpe,pnk
      real*8 pek
      complex*16 mw2m1,mw2m2,mw2dm1
      real*8 tmp
      real*8 ml2,ml4
      complex*16 gs2
      real*8 densp2

      integer ifirst
      data ifirst/0/
      save ifirst,ml2,ml4,mw2m1,mw2dm1,mw2m2

      if (ifirst.eq.0) then
          ifirst = 0

          ml2 = kn_masses(3)*kn_masses(3)
          ml4 = ml2*ml2
    
          mw2m1 = 1d0/mw2
          mw2dm1= conjg(mw2m1)
          mw2m2 = mw2m1*mw2dm1
      endif

      gs2 = 4d0*pi*st_alpha
*
c  copy of local variables
      do i=1,nlegreal
         do nu=0,3
            p(nu,i) = pphy(nu,i)
         enddo
         ferm_charge(i) = fermion_charge(i)
         ferm_type(i) = fermion_type(i)
      enddo

c     exchance particle 1 and 2
      if (ferm_type(1).eq.-1) then
         if (ferm_type(2).eq.1) then
            call exchange_momenta(p(0,1),p(0,2))
            tmp = ferm_charge(1)
            ferm_charge(1)=ferm_charge(2)
            ferm_charge(2)=tmp
            tmp = ferm_type(1)
            ferm_type(1)=ferm_type(2)
            ferm_type(2)=tmp
         else
            write(*,*) 'Error in the type of the quark 1-2'
            stop
         endif
      endif

      if (ferm_charge(1)*ferm_type(1).lt.0d0) then
          do nu=0,3
              pu(nu) = p(nu,2)
              pd(nu) = p(nu,1)
          enddo
      else
          do nu=0,3
              pu(nu) = p(nu,1)
              pd(nu) = p(nu,2)
          enddo
      endif

      do nu=0,3
          pl(nu) = p(nu,3)
          pn(nu) = p(nu,4)
          k (nu) = p(nu,5)
      enddo

*
      pupd = dotp(pu,pd)
      pupe = dotp(pu,pl)
      pupn = dotp(pu,pn)
      puk  = dotp(pu,k)
*
      pdpe = dotp(pd,pl)
      pdpn = dotp(pd,pn)
      pdk  = dotp(pd,k)
*
      pnpe = dotp(pl,pn)
      pnk  = dotp(pn,k)
*
      pek  = dotp(pl,k)
*
      sp = 2.d0*pnpe + ml2
*
      densp2 = 1d0/(sp*cone-ph_Wmass2+ii*ph_WmWw)/
     -             (sp*cone-ph_Wmass2-ii*ph_WmWw)

      amp2 =
     -   (densp2*(pdpe*pdpn*puk - pdpn*pek*pupd + 
     -      (-(pdpn*puk) + pnk*puk + 2*pdpn*pupd - pnk*pupd)*pupe + 
     -      pdk*(pdpn*(pek - pupe) + pupe*pupn)))/(pdk*puk)

      amp2 = amp2 * 16 
     +            * g2*conjg(g2) * gs2/4d0
     +            * CF/4d0/nc
* for initial state gluons
      if(dble(amp2).lt.0d0) amp2 = -amp2

      end

*
** Subroutine for real part
** mu = md = 0
**
**(pu) u \            / l+ (pl)
**        \          /--- g (k)
**         \________/
**         /   W+   \       (g can be emitted by every charged leg)
**        /          \
**(pd) d~/            \ nu_l (pn)
*
*
**
*

      subroutine setreal_ew(p,fermion_flav,amp2)
      implicit none
      include 'nlegborn.h'
      include 'PhysPars.h'
      include 'pwhg_st.h'
      include 'pwhg_kn.h'
      include 'pwhg_math.h'
      include 'mathx.h'
      include 'pwhg_physpar.h'
*
      real * 8 p(0:3,nlegreal)
      integer fermion_flav(nlegreal)
      real * 8 amp2
*
      complex*16 e_
      external e_

      real*8 dotp
      external dotp
*
      real*8 mlep2
      common/leptmass/mlep2
*
      real*8 qu2,qd2

      real*8 pu(0:3),pd(0:3),pn(0:3),pl(0:3),k(0:3)
      real*8 s,sp

      integer i,j,nu

      real*8 pupd,pupe,pupn,puk
      real*8 pdpe,pdpn,pdk
      real*8 pnpe,pnk
      real*8 pek
      real*8 pupn2,pdpn2,pdpe2
      real*8 pupe2,pupd2
      real*8 pnpe2
      complex*16 mw2m1,mw2m2,mw2dm1
      complex*16 epupdpek,epupdpnk,epupnpek,epdpnpek,epupdpnpe
      real*8 ml2,ml4

      complex*16 c1,c2,c3,c4,c1d,c2d,c3d,c4d
      complex*16 pukc1,pdkc2,pukc1d,pdkc2d
*
      integer ifirst
      data ifirst/0/
      save ifirst,ml2,ml4,mw2m1,mw2dm1,mw2m2

      if (ifirst.eq.0) then
          ifirst = 1

          ml2 = mlep2
          ml4 = ml2*ml2
    
          mw2m1 = 1d0/mw2
          mw2dm1= conjg(mw2m1)
          mw2m2 = mw2m1*mw2dm1
      endif

      i = fermion_flav(1)
      j = fermion_flav(2)

      if (mod(abs(i),2).eq.0) then
          do nu=0,3
              pu(nu) = p(nu,1)
              pd(nu) = p(nu,2)
          enddo
      else
          do nu=0,3
              pd(nu) = p(nu,1)
              pu(nu) = p(nu,2)
          enddo
      endif

      do nu=0,3
          pl(nu) = p(nu,3)
          pn(nu) = p(nu,4)
          k (nu) = p(nu,5)
      enddo
*
      pupd = dotp(pu,pd)
      pupe = dotp(pu,pl)
      pupn = dotp(pu,pn)
      puk  = dotp(pu,k)
*
      pdpe = dotp(pd,pl)
      pdpn = dotp(pd,pn)
      pdk  = dotp(pd,k)
*
      pnpe = dotp(pl,pn)
      pnk  = dotp(pn,k)
*
      pek  = dotp(pl,k)
*
      pupn2 = pupn*pupn
      pdpn2 = pdpn*pdpn
      pdpe2 = pdpe*pdpe
      pnpe2 = pnpe*pnpe
      pupe2 = pupe*pupe
      pupd2 = pupd*pupd
*
      epupdpek  = e_(pu,pd,pl,k)
      epupdpnk  = e_(pu,pd,pn,k)
      epupnpek  = e_(pu,pn,pl,k)
      epdpnpek  = e_(pd,pn,pl,k)
      epupdpnpe = e_(pu,pd,pn,pl)
*
      s  = 2.d0*pupd
      sp = 2.d0*pnpe + ml2

      c1 =  ii*qu/(2.d0*puk)/(sp*cone - ph_Wmass2 + ii*ph_WmWw)
      c2 =  ii*qd/(2.d0*pdk)/(sp*cone - ph_Wmass2 + ii*ph_WmWw)
      c3 = -ii/(sp*cone - ph_Wmass2 + ii*ph_WmWw)
     +        /(s*cone -  ph_Wmass2 + ii*ph_WmWw)
      c4 =  ii/(2.d0*pek)/(s*cone - ph_Wmass2 + ii*ph_WmWw)

      pukc1 =  ii*qu/2.d0/(sp*cone - ph_Wmass2 + ii*ph_WmWw)
      pdkc2 =  ii*qd/2.d0/(sp*cone - ph_Wmass2 + ii*ph_WmWw)
*
      c1d = conjg(c1)
      c2d = conjg(c2)
      c3d = conjg(c3)
      c4d = conjg(c4)

      pukc1d = conjg(pukc1)
      pdkc2d = conjg(pdkc2)
*
      amp2 =
     &  + pukc1d * (  - 16*c3*ml2*mw2m2*pnpe2*pdk - 16*c2*ml2*mw2dm1*
     &    pdpn2 - 16*c2*ml2*mw2m1*pdpn2 - 16*pupd*pnpe*c3 + 16*pupd*
     &    pnpe*c2*ml4*mw2m2 - 16*pupd*pnpe*c2*ml2*mw2dm1 - 16*pupd*
     &    pnpe*c2*ml2*mw2m1 + 32*pupd*c2*ml2*mw2m2*pnpe2 - 16*pupn*
     &    pdpn*pnpe*c2*ml2*mw2m2 + 16*pupn*pdpn*c2*ml2*mw2dm1 + 16*
     &    pupn*pdpn*c2*ml2*mw2m1 - 16*pupn*pdpe*pnpe*c2*ml2*mw2m2
     &     + 16*pupn*pdpe*c3 + 16*pupn*pdpe*c2*ml2*mw2m1 - 16*pupe
     &    *pdpn*pnpe*c2*ml2*mw2m2 - 16*pupe*pdpn*c3 - 32*pupe*
     &    pdpn*c2 + 16*pupe*pdpn*c2*ml2*mw2dm1 - 16*pupe*pdpe*
     &    pnpe*c2*ml2*mw2m2 + 32*pdpn*pdpe*pnpe*c2*ml2*mw2m2 + 32*
     &    pdpn*pdpe*c2 - 16*pdpn*pdpe*c2*ml2*mw2dm1 - 16*pdpn*
     &    pdpe*c2*ml2*mw2m1 - 16*pdpn*pnpe*c3 + 16*pdpn*pnpe*c3*
     &    ml2*mw2m2*pek + 16*pdpn*pnpe*c3*ml2*mw2m2*pnk + 8*pdpn*
     &    pnpe*c3*ml2*mw2dm1 + 32*pdpn*c3*pek + 8*pdpn*c3*ml4*mw2dm1
     &     - 16*pdpn*c3*ml2 - 8*pdpn*c3*ml2*mw2dm1*pek - 16*pdpn*c3*
     &    ml2*mw2dm1*pnk )
      amp2 = amp2 + pukc1d * (  - 8*pdpn*c3*ml2*mw2m1*pek - 16*
     &    pdpn*c3*ml2*mw2m1*pnk + 16*pdpe*pnpe*c3*ml2*mw2m2*pek + 16
     &    *pdpe*pnpe*c3*ml2*mw2m2*pnk - 8*pdpe*pnpe*c3*ml2*mw2dm1
     &     - 8*pdpe*c3*ml2*mw2dm1*pnk - 8*pdpe*c3*ml2*mw2m1*pnk - 8*
     &    pnpe*c3*ml4*mw2m2*pdk + 8*pnpe*c3*ml2*mw2dm1*pdk + 8*pnpe*
     &    c3*ml2*mw2m1*pdk + 16*pnpe*c2*ml2*mw2m2*pdpe2 + 16*pnpe*c2*
     &    ml2*mw2m2*pdpn2 + 16*pnpe*c2*ml2*mw2m2*epupdpnpe )
      amp2 = amp2 + pdkc2d * ( 16*c3*epupdpnpe + 16*c3*ml2*mw2m2*
     &    pnpe2*puk - 8*c3*ml2*mw2dm1*epupnpek + 8*c3*ml2*mw2m1*
     &    epupnpek - 16*c1*ml2*mw2dm1*pupn2 - 16*c1*ml2*mw2dm1*
     &    epupdpnpe - 16*c1*ml2*mw2m1*pupn2 + 16*c1*ml2*mw2m1*epupdpnpe
     &     + 16*pupd*pnpe*c3 + 16*pupd*pnpe*c1*ml4*mw2m2 - 16*pupd
     &    *pnpe*c1*ml2*mw2dm1 - 16*pupd*pnpe*c1*ml2*mw2m1 + 32*pupd
     &    *c1*ml2*mw2m2*pnpe2 + 32*pupn*pupe*pnpe*c1*ml2*mw2m2 + 32*
     &    pupn*pupe*c1 - 16*pupn*pupe*c1*ml2*mw2dm1 - 16*pupn*
     &    pupe*c1*ml2*mw2m1 - 16*pupn*pdpn*pnpe*c1*ml2*mw2m2 + 16*
     &    pupn*pdpn*c1*ml2*mw2dm1 + 16*pupn*pdpn*c1*ml2*mw2m1 - 16*
     &    pupn*pdpe*pnpe*c1*ml2*mw2m2 - 16*pupn*pdpe*c3 + 16*pupn
     &    *pdpe*c1*ml2*mw2dm1 - 16*pupn*pnpe*c3*ml2*mw2m2*pek - 16*
     &    pupn*pnpe*c3*ml2*mw2m2*pnk - 8*pupn*pnpe*c3*ml2*mw2dm1 - 
     &    8*pupn*c3*ml4*mw2dm1 + 8*pupn*c3*ml2*mw2dm1*pek + 16*pupn*
     &    c3*ml2*mw2dm1*pnk + 8*pupn*c3*ml2*mw2m1*pek + 16*pupn*c3*
     &    ml2*mw2m1*pnk )
      amp2 = amp2 + pdkc2d * (  - 16*pupe*pdpn*pnpe*c1*ml2*mw2m2
     &     + 16*pupe*pdpn*c3 - 32*pupe*pdpn*c1 + 16*pupe*pdpn*c1*
     &    ml2*mw2m1 - 16*pupe*pdpe*pnpe*c1*ml2*mw2m2 + 16*pupe*
     &    pnpe*c3 - 16*pupe*pnpe*c3*ml2*mw2m2*pek - 16*pupe*pnpe*
     &    c3*ml2*mw2m2*pnk + 8*pupe*pnpe*c3*ml2*mw2dm1 - 32*pupe*c3*
     &    pnk + 8*pupe*c3*ml2*mw2dm1*pnk + 8*pupe*c3*ml2*mw2m1*pnk + 
     &    8*pnpe*c3*ml4*mw2m2*puk - 8*pnpe*c3*ml2*mw2dm1*puk - 8*
     &    pnpe*c3*ml2*mw2m1*puk + 16*pnpe*c1*ml2*mw2m2*pupe2 + 16*
     &    pnpe*c1*ml2*mw2m2*pupn2 )
      amp2 = amp2 + pukc1 * (  - 16*c3d*ml2*mw2m2*pnpe2*pdk - 16*
     &    c2d*ml2*mw2dm1*pdpn2 - 16*c2d*ml2*mw2m1*pdpn2 - 32*c1d*ml2*
     &    mw2m2*pnpe2*pdk - 16*c1d*ml2*mw2dm1*epdpnpek + 16*c1d*ml2*
     &    mw2m1*epdpnpek - 16*pupd*pnpe*c3d + 16*pupd*pnpe*c2d*ml4*
     &    mw2m2 - 16*pupd*pnpe*c2d*ml2*mw2dm1 - 16*pupd*pnpe*c2d*
     &    ml2*mw2m1 + 32*pupd*c2d*ml2*mw2m2*pnpe2 - 16*pupn*pdpn*
     &    pnpe*c2d*ml2*mw2m2 + 16*pupn*pdpn*c2d*ml2*mw2dm1 + 16*
     &    pupn*pdpn*c2d*ml2*mw2m1 - 16*pupn*pdpe*pnpe*c2d*ml2*
     &    mw2m2 + 16*pupn*pdpe*c3d + 16*pupn*pdpe*c2d*ml2*mw2dm1 - 
     &    16*pupe*pdpn*pnpe*c2d*ml2*mw2m2 - 16*pupe*pdpn*c3d - 32*
     &    pupe*pdpn*c2d + 16*pupe*pdpn*c2d*ml2*mw2m1 - 16*pupe*
     &    pdpe*pnpe*c2d*ml2*mw2m2 + 32*pdpn*pdpe*pnpe*c2d*ml2*
     &    mw2m2 + 32*pdpn*pdpe*c2d - 16*pdpn*pdpe*c2d*ml2*mw2dm1 - 
     &    16*pdpn*pdpe*c2d*ml2*mw2m1 - 16*pdpn*pnpe*c3d + 16*pdpn*
     &    pnpe*c3d*ml2*mw2m2*pek + 16*pdpn*pnpe*c3d*ml2*mw2m2*pnk + 
     &    8*pdpn*pnpe*c3d*ml2*mw2m1 )
      amp2 = amp2 + pukc1 * ( 32*pdpn*pnpe*c1d*ml2*mw2m2*pek + 32
     &    *pdpn*pnpe*c1d*ml2*mw2m2*pnk + 32*pdpn*c3d*pek + 8*pdpn*
     &    c3d*ml4*mw2m1 - 16*pdpn*c3d*ml2 - 8*pdpn*c3d*ml2*mw2dm1*pek
     &     - 16*pdpn*c3d*ml2*mw2dm1*pnk - 8*pdpn*c3d*ml2*mw2m1*pek - 
     &    16*pdpn*c3d*ml2*mw2m1*pnk + 64*pdpn*c1d*pek - 16*pdpn*c1d*
     &    ml2*mw2dm1*pek - 32*pdpn*c1d*ml2*mw2dm1*pnk - 16*pdpn*c1d*
     &    ml2*mw2m1*pek - 32*pdpn*c1d*ml2*mw2m1*pnk + 16*pdpe*pnpe*
     &    c3d*ml2*mw2m2*pek + 16*pdpe*pnpe*c3d*ml2*mw2m2*pnk - 8*
     &    pdpe*pnpe*c3d*ml2*mw2m1 + 32*pdpe*pnpe*c1d*ml2*mw2m2*pek
     &     + 32*pdpe*pnpe*c1d*ml2*mw2m2*pnk - 8*pdpe*c3d*ml2*mw2dm1*
     &    pnk - 8*pdpe*c3d*ml2*mw2m1*pnk - 16*pdpe*c1d*ml2*mw2dm1*pnk
     &     - 16*pdpe*c1d*ml2*mw2m1*pnk - 8*pnpe*c3d*ml4*mw2m2*pdk + 8
     &    *pnpe*c3d*ml2*mw2dm1*pdk + 8*pnpe*c3d*ml2*mw2m1*pdk + 16*
     &    pnpe*c2d*ml2*mw2m2*pdpe2 + 16*pnpe*c2d*ml2*mw2m2*pdpn2 - 16
     &    *pnpe*c2d*ml2*mw2m2*epupdpnpe - 16*pnpe*c1d*ml4*mw2m2*pdk
     &     + 16*pnpe*c1d*ml2*mw2dm1*pdk )
      amp2 = amp2 + pukc1 * ( 16*pnpe*c1d*ml2*mw2m1*pdk )
      amp2 = amp2 + pdkc2 * (  - 16*c3d*epupdpnpe + 16*c3d*ml2*
     &    mw2m2*pnpe2*puk - 8*c3d*ml2*mw2dm1*epupnpek + 8*c3d*ml2*mw2m1
     &    *epupnpek - 32*c2d*ml2*mw2m2*pnpe2*puk + 16*c2d*ml2*mw2dm1*
     &    epupnpek - 16*c2d*ml2*mw2m1*epupnpek - 16*c1d*ml2*mw2dm1*
     &    pupn2 - 16*c1d*ml2*mw2dm1*epupdpnpe - 16*c1d*ml2*mw2m1*pupn2
     &     + 16*c1d*ml2*mw2m1*epupdpnpe + 16*pupd*pnpe*c3d + 16*pupd
     &    *pnpe*c1d*ml4*mw2m2 - 16*pupd*pnpe*c1d*ml2*mw2dm1 - 16*
     &    pupd*pnpe*c1d*ml2*mw2m1 + 32*pupd*c1d*ml2*mw2m2*pnpe2 + 32
     &    *pupn*pupe*pnpe*c1d*ml2*mw2m2 + 32*pupn*pupe*c1d - 16*
     &    pupn*pupe*c1d*ml2*mw2dm1 - 16*pupn*pupe*c1d*ml2*mw2m1 - 
     &    16*pupn*pdpn*pnpe*c1d*ml2*mw2m2 + 16*pupn*pdpn*c1d*ml2*
     &    mw2dm1 + 16*pupn*pdpn*c1d*ml2*mw2m1 - 16*pupn*pdpe*pnpe*
     &    c1d*ml2*mw2m2 - 16*pupn*pdpe*c3d + 16*pupn*pdpe*c1d*ml2*
     &    mw2m1 - 16*pupn*pnpe*c3d*ml2*mw2m2*pek - 16*pupn*pnpe*c3d
     &    *ml2*mw2m2*pnk - 8*pupn*pnpe*c3d*ml2*mw2m1 + 32*pupn*pnpe
     &    *c2d*ml2*mw2m2*pek )
      amp2 = amp2 + pdkc2 * ( 32*pupn*pnpe*c2d*ml2*mw2m2*pnk - 8*
     &    pupn*c3d*ml4*mw2m1 + 8*pupn*c3d*ml2*mw2dm1*pek + 16*pupn*
     &    c3d*ml2*mw2dm1*pnk + 8*pupn*c3d*ml2*mw2m1*pek + 16*pupn*c3d
     &    *ml2*mw2m1*pnk - 16*pupn*c2d*ml2*mw2dm1*pek - 32*pupn*c2d*
     &    ml2*mw2dm1*pnk - 16*pupn*c2d*ml2*mw2m1*pek - 32*pupn*c2d*
     &    ml2*mw2m1*pnk - 16*pupe*pdpn*pnpe*c1d*ml2*mw2m2 + 16*pupe
     &    *pdpn*c3d - 32*pupe*pdpn*c1d + 16*pupe*pdpn*c1d*ml2*
     &    mw2dm1 - 16*pupe*pdpe*pnpe*c1d*ml2*mw2m2 + 16*pupe*pnpe*
     &    c3d - 16*pupe*pnpe*c3d*ml2*mw2m2*pek - 16*pupe*pnpe*c3d*
     &    ml2*mw2m2*pnk + 8*pupe*pnpe*c3d*ml2*mw2m1 + 32*pupe*pnpe*
     &    c2d*ml2*mw2m2*pek + 32*pupe*pnpe*c2d*ml2*mw2m2*pnk - 32*
     &    pupe*c3d*pnk + 8*pupe*c3d*ml2*mw2dm1*pnk + 8*pupe*c3d*ml2*
     &    mw2m1*pnk + 64*pupe*c2d*pnk - 16*pupe*c2d*ml2*mw2dm1*pnk - 
     &    16*pupe*c2d*ml2*mw2m1*pnk + 8*pnpe*c3d*ml4*mw2m2*puk - 8*
     &    pnpe*c3d*ml2*mw2dm1*puk - 8*pnpe*c3d*ml2*mw2m1*puk - 16*
     &    pnpe*c2d*ml4*mw2m2*puk )
      amp2 = amp2 + pdkc2 * ( 16*pnpe*c2d*ml2*mw2dm1*puk + 16*
     &    pnpe*c2d*ml2*mw2m1*puk + 16*pnpe*c1d*ml2*mw2m2*pupe2 + 16*
     &    pnpe*c1d*ml2*mw2m2*pupn2 )
      amp2 = amp2 + c4d * (  - 16*c3*epupdpnpe*puk - 8*c3*ml4*mw2m1
     &    *epupdpnk - 8*c3*ml2*epupdpnpe + 8*c3*ml2*epupdpnk + 8*c3*ml2
     &    *mw2m1*pdpn2*puk + 8*c3*ml2*mw2m1*epupdpnpe*pek + 8*c3*ml2*
     &    mw2m1*epupdpnpe*pnk - 8*c3*ml2*mw2m1*epupdpnpe*puk - 8*c3*ml2
     &    *mw2m1*epdpnpek*puk + 32*c2*epupdpnpe*pek + 8*c2*ml4*mw2m1*
     &    epupdpnk - 32*c2*ml2*epupdpnk + 16*c2*ml2*mw2m1*pdpn2*puk - 
     &    16*c2*ml2*mw2m1*epupdpnpe*pek - 16*c2*ml2*mw2m1*epupdpnpe*pnk
     &     + 8*c1*ml4*mw2m1*epupdpnk - 8*pupd*pupn*pnpe*c3*ml2*mw2m1
     &     + 16*pupd*pupe*pnpe*c3 + 16*pupd*pupe*pnpe*c1*ml2*
     &    mw2m1 - 8*pupd*pdpn*pnpe*c3*ml2*mw2m1 - 16*pupd*pdpn*c3*
     &    pek + 16*pupd*pdpn*c3*ml2 + 8*pupd*pdpn*c3*ml2*mw2m1*pek
     &     + 8*pupd*pdpn*c3*ml2*mw2m1*pnk + 32*pupd*pdpn*c2*pek - 
     &    16*pupd*pdpn*c2*ml2*mw2m1*pek - 16*pupd*pdpn*c2*ml2*mw2m1
     &    *pnk - 16*pupd*pdpe*pnpe*c2*ml2*mw2m1 - 16*pupd*pnpe*c3*
     &    pek + 16*pupd*pnpe*c3*puk + 8*pupd*pnpe*c3*ml2 - 8*pupd*
     &    pnpe*c3*ml2*mw2m1*pek )
      amp2 = amp2 + c4d * ( 8*pupd*pnpe*c3*ml2*mw2m1*puk + 16*
     &    pupd*pnpe*c2*ml2*mw2m1*pek - 16*pupd*pnpe*c1*ml2*mw2m1*
     &    pek + 8*pupd*c3*ml4*mw2m1*pnk + 8*pupd*c3*ml2*pnk - 8*pupd
     &    *c2*ml4*mw2m1*pnk + 8*pupd*c1*ml4*mw2m1*pnk - 8*pupn*pupe*
     &    pdpn*c3*ml2*mw2m1 - 32*pupn*pupe*pdpn*c1*ml2*mw2m1 - 16*
     &    pupn*pupe*pdpe*c3 - 16*pupn*pupe*pdpe*c1*ml2*mw2m1 - 8*
     &    pupn*pdpn*pdpe*c3*ml2*mw2m1 + 32*pupn*pdpn*pdpe*c2*ml2*
     &    mw2m1 - 16*pupn*pdpn*c3*ml2 - 8*pupn*pdpn*c3*ml2*mw2m1*
     &    pdk - 16*pupn*pdpn*c2*ml2*mw2m1*pek + 16*pupn*pdpn*c1*ml2
     &    *mw2m1*pek + 16*pupn*pdpe*c3*pek - 16*pupn*pdpe*c3*puk - 
     &    8*pupn*pdpe*c3*ml2 + 8*pupn*pdpe*c3*ml2*mw2m1*pek + 8*
     &    pupn*pdpe*c3*ml2*mw2m1*pnk - 8*pupn*pdpe*c3*ml2*mw2m1*puk
     &     - 16*pupn*pdpe*c2*ml2*mw2m1*pek - 16*pupn*pdpe*c2*ml2*
     &    mw2m1*pnk + 16*pupn*pdpe*c1*ml2*mw2m1*pek - 8*pupn*pnpe*
     &    c3*ml2*mw2m1*pdk - 8*pupn*c3*ml4*mw2m1*pdk - 8*pupn*c3*ml2*
     &    pdk )
      amp2 = amp2 + c4d * ( 8*pupn*c3*ml2*mw2m1*epupdpnpe + 16*
     &    pupn*c2*ml2*mw2m1*pdpe2 - 8*pupn*c1*ml4*mw2m1*pdk - 64*
     &    pupe*pdpn*pdpe*c2 + 16*pupe*pdpn*pdpe*c2*ml2*mw2m1 + 32
     &    *pupe*pdpn*pnpe*c3 - 16*pupe*pdpn*c3*pek + 16*pupe*
     &    pdpn*c3*puk + 16*pupe*pdpn*c3*pdk + 24*pupe*pdpn*c3*ml2
     &     + 8*pupe*pdpn*c3*ml2*mw2m1*pek + 8*pupe*pdpn*c3*ml2*
     &    mw2m1*pnk + 8*pupe*pdpn*c3*ml2*mw2m1*puk - 8*pupe*pdpn*c3
     &    *ml2*mw2m1*pdk + 32*pupe*pdpn*c2*pek - 16*pupe*pdpn*c2*
     &    ml2*mw2m1*pek - 64*pupe*pdpn*c1*pek + 16*pupe*pdpn*c1*ml2
     &    *mw2m1*pek + 16*pupe*pdpn*c1*ml2*mw2m1*pnk - 32*pupe*pdpe
     &    *c3*pnk + 32*pupe*pdpe*c2*pnk + 32*pupe*pnpe*c3*pdk - 16*
     &    pupe*c3*epupdpnpe + 16*pupe*c3*epdpnpek + 8*pupe*c3*ml2*
     &    mw2m1*pdpn2 - 16*pupe*c1*ml2*mw2m1*epupdpnpe - 16*pupe*c1*
     &    ml2*mw2m1*epdpnpek - 16*pdpn*pdpe*c3*puk + 8*pdpn*pdpe*c3
     &    *ml2*mw2m1*puk - 32*pdpn*pdpe*c2*puk + 16*pdpn*pdpe*c2*
     &    ml2*mw2m1*puk )
      amp2 = amp2 + c4d * ( 32*pdpn*pnpe*c3*puk - 8*pdpn*pnpe*
     &    c3*ml2*mw2m1*puk - 32*pdpn*c3*puk*pek + 16*pdpn*c3*pupe2 - 
     &    16*pdpn*c3*epupdpek - 8*pdpn*c3*ml4*mw2m1*puk + 40*pdpn*c3
     &    *ml2*puk + 8*pdpn*c3*ml2*mw2m1*puk*pek + 16*pdpn*c3*ml2*
     &    mw2m1*puk*pnk + 8*pdpn*c3*ml2*mw2m1*epupdpnpe + 8*pdpn*c3*
     &    ml2*mw2m1*epupdpnk + 8*pdpn*c3*ml2*mw2m1*epupdpek + 32*pdpn
     &    *c2*epupdpek + 8*pdpn*c2*ml4*mw2m1*puk - 16*pdpn*c2*ml2*
     &    mw2m1*epupdpnk - 16*pdpn*c2*ml2*mw2m1*epupdpek + 16*pdpn*c2
     &    *ml2*mw2m1*epupnpek + 64*pdpn*c1*pupe2 - 16*pdpn*c1*ml2*
     &    mw2m1*pupe2 + 16*pdpn*c1*ml2*mw2m1*epupnpek + 16*pdpe*c3*
     &    epupnpek + 8*pdpe*c3*ml2*mw2m1*puk*pnk + 8*pdpe*c3*ml2*
     &    mw2m1*pupn2 - 32*pdpe*c2*epupnpek + 16*pdpe*c2*ml2*mw2m1*
     &    epupdpnpe + 16*pdpe*c1*ml2*mw2m1*epupnpek - 16*pnpe*c3*
     &    epupdpek - 8*pnpe*c3*ml2*mw2m1*pdk*puk - 8*pnpe*c3*ml2*
     &    mw2m1*epupdpnk + 32*pnpe*c2*epupdpek + 16*pnpe*c2*ml2*mw2m1
     &    *epupdpnk )
      amp2 = amp2 + c4d * (  - 16*pnpe*c1*ml2*mw2m1*epupdpek )
      amp2 = amp2 + c4d*pukc1 * (  - 16*ml2*mw2m1*epupdpnpe - 16*
     &    ml2*mw2m1*epdpnpek + 16*pupd*pnpe*ml2*mw2m1 - 32*pupn*
     &    pdpn*ml2*mw2m1 - 16*pupn*pdpe*ml2*mw2m1 + 64*pupe*pdpn
     &     - 16*pupe*pdpn*ml2*mw2m1 - 16*pdpn*pnpe*ml2*mw2m1 - 64*
     &    pdpn*pek - 8*pdpn*ml4*mw2m1 + 32*pdpn*ml2 + 16*pdpn*ml2*
     &    mw2m1*pek + 32*pdpn*ml2*mw2m1*pnk + 16*pdpe*ml2*mw2m1*pnk
     &     - 16*pnpe*ml2*mw2m1*pdk )
      amp2 = amp2 + c4d*pdkc2 * ( 16*pupn*pdpn*ml2*mw2m1 + 16*
     &    pupn*pnpe*ml2*mw2m1 + 8*pupn*ml4*mw2m1 - 32*pupe*pdpn + 
     &    16*pupe*pdpn*ml2*mw2m1 - 32*pupe*pnpe )
      amp2 = amp2 + c4 * ( 16*c3d*epupdpnpe*puk + 8*c3d*ml4*mw2dm1*
     &    epupdpnk + 8*c3d*ml2*epupdpnpe - 8*c3d*ml2*epupdpnk + 8*c3d*
     &    ml2*mw2dm1*pdpn2*puk - 8*c3d*ml2*mw2dm1*epupdpnpe*pek - 8*c3d
     &    *ml2*mw2dm1*epupdpnpe*pnk + 8*c3d*ml2*mw2dm1*epupdpnpe*puk + 
     &    8*c3d*ml2*mw2dm1*epdpnpek*puk - 8*c2d*ml4*mw2dm1*epupdpnk + 
     &    16*c2d*ml2*mw2dm1*pdpn2*puk + 16*c2d*ml2*mw2dm1*epupdpnpe*pek
     &     + 16*c2d*ml2*mw2dm1*epupdpnpe*pnk - 8*c1d*ml4*mw2dm1*
     &    epupdpnk - 8*pupd*pupn*pnpe*c3d*ml2*mw2dm1 + 16*pupd*
     &    pupe*pnpe*c3d + 16*pupd*pupe*pnpe*c1d*ml2*mw2dm1 - 8*
     &    pupd*pdpn*pnpe*c3d*ml2*mw2dm1 - 16*pupd*pdpn*c3d*pek + 
     &    16*pupd*pdpn*c3d*ml2 + 8*pupd*pdpn*c3d*ml2*mw2dm1*pek + 8
     &    *pupd*pdpn*c3d*ml2*mw2dm1*pnk + 32*pupd*pdpn*c2d*pek - 16
     &    *pupd*pdpn*c2d*ml2*mw2dm1*pek - 16*pupd*pdpn*c2d*ml2*
     &    mw2dm1*pnk - 16*pupd*pdpe*pnpe*c2d*ml2*mw2dm1 - 16*pupd*
     &    pnpe*c3d*pek + 16*pupd*pnpe*c3d*puk + 8*pupd*pnpe*c3d*
     &    ml2 )
      amp2 = amp2 + c4 * (  - 8*pupd*pnpe*c3d*ml2*mw2dm1*pek + 8*
     &    pupd*pnpe*c3d*ml2*mw2dm1*puk + 16*pupd*pnpe*c2d*ml2*
     &    mw2dm1*pek - 16*pupd*pnpe*c1d*ml2*mw2dm1*pek + 8*pupd*c3d*
     &    ml4*mw2dm1*pnk + 8*pupd*c3d*ml2*pnk - 8*pupd*c2d*ml4*mw2dm1
     &    *pnk + 8*pupd*c1d*ml4*mw2dm1*pnk - 8*pupn*pupe*pdpn*c3d*
     &    ml2*mw2dm1 - 32*pupn*pupe*pdpn*c1d*ml2*mw2dm1 - 16*pupn*
     &    pupe*pdpe*c3d - 16*pupn*pupe*pdpe*c1d*ml2*mw2dm1 - 8*
     &    pupn*pdpn*pdpe*c3d*ml2*mw2dm1 + 32*pupn*pdpn*pdpe*c2d*
     &    ml2*mw2dm1 - 16*pupn*pdpn*c3d*ml2 - 8*pupn*pdpn*c3d*ml2*
     &    mw2dm1*pdk - 16*pupn*pdpn*c2d*ml2*mw2dm1*pek + 16*pupn*
     &    pdpn*c1d*ml2*mw2dm1*pek + 16*pupn*pdpe*c3d*pek - 16*pupn*
     &    pdpe*c3d*puk - 8*pupn*pdpe*c3d*ml2 + 8*pupn*pdpe*c3d*ml2
     &    *mw2dm1*pek + 8*pupn*pdpe*c3d*ml2*mw2dm1*pnk - 8*pupn*
     &    pdpe*c3d*ml2*mw2dm1*puk - 16*pupn*pdpe*c2d*ml2*mw2dm1*pek
     &     - 16*pupn*pdpe*c2d*ml2*mw2dm1*pnk + 16*pupn*pdpe*c1d*ml2
     &    *mw2dm1*pek )
      amp2 = amp2 + c4 * (  - 8*pupn*pnpe*c3d*ml2*mw2dm1*pdk - 8*
     &    pupn*c3d*ml4*mw2dm1*pdk - 8*pupn*c3d*ml2*pdk - 8*pupn*c3d*
     &    ml2*mw2dm1*epupdpnpe + 16*pupn*c2d*ml2*mw2dm1*pdpe2 - 8*
     &    pupn*c1d*ml4*mw2dm1*pdk - 64*pupe*pdpn*pdpe*c2d + 16*
     &    pupe*pdpn*pdpe*c2d*ml2*mw2dm1 + 32*pupe*pdpn*pnpe*c3d
     &     - 16*pupe*pdpn*c3d*pek + 16*pupe*pdpn*c3d*puk + 16*pupe
     &    *pdpn*c3d*pdk + 24*pupe*pdpn*c3d*ml2 + 8*pupe*pdpn*c3d*
     &    ml2*mw2dm1*pek + 8*pupe*pdpn*c3d*ml2*mw2dm1*pnk + 8*pupe*
     &    pdpn*c3d*ml2*mw2dm1*puk - 8*pupe*pdpn*c3d*ml2*mw2dm1*pdk
     &     + 32*pupe*pdpn*c2d*pek - 16*pupe*pdpn*c2d*ml2*mw2dm1*pek
     &     - 64*pupe*pdpn*c1d*pek + 16*pupe*pdpn*c1d*ml2*mw2dm1*pek
     &     + 16*pupe*pdpn*c1d*ml2*mw2dm1*pnk - 32*pupe*pdpe*c3d*pnk
     &     + 32*pupe*pdpe*c2d*pnk + 32*pupe*pnpe*c3d*pdk + 16*pupe
     &    *c3d*epupdpnpe - 16*pupe*c3d*epdpnpek + 8*pupe*c3d*ml2*
     &    mw2dm1*pdpn2 + 32*pupe*c2d*epdpnpek + 16*pupe*c1d*ml2*
     &    mw2dm1*epupdpnpe )
      amp2 = amp2 + c4 * ( 16*pupe*c1d*ml2*mw2dm1*epdpnpek - 16*
     &    pdpn*pdpe*c3d*puk + 8*pdpn*pdpe*c3d*ml2*mw2dm1*puk - 32*
     &    pdpn*pdpe*c2d*puk + 16*pdpn*pdpe*c2d*ml2*mw2dm1*puk + 32*
     &    pdpn*pnpe*c3d*puk - 8*pdpn*pnpe*c3d*ml2*mw2dm1*puk - 32*
     &    pdpn*c3d*puk*pek + 16*pdpn*c3d*pupe2 + 16*pdpn*c3d*
     &    epupdpek - 8*pdpn*c3d*ml4*mw2dm1*puk + 40*pdpn*c3d*ml2*puk
     &     + 8*pdpn*c3d*ml2*mw2dm1*puk*pek + 16*pdpn*c3d*ml2*mw2dm1*
     &    puk*pnk - 8*pdpn*c3d*ml2*mw2dm1*epupdpnpe - 8*pdpn*c3d*ml2*
     &    mw2dm1*epupdpnk - 8*pdpn*c3d*ml2*mw2dm1*epupdpek - 32*pdpn*
     &    c2d*epupdpek + 8*pdpn*c2d*ml4*mw2dm1*puk + 16*pdpn*c2d*ml2*
     &    mw2dm1*epupdpnk + 16*pdpn*c2d*ml2*mw2dm1*epupdpek - 16*pdpn
     &    *c2d*ml2*mw2dm1*epupnpek + 64*pdpn*c1d*pupe2 - 16*pdpn*c1d*
     &    ml2*mw2dm1*pupe2 - 16*pdpn*c1d*ml2*mw2dm1*epupnpek - 16*
     &    pdpe*c3d*epupnpek + 8*pdpe*c3d*ml2*mw2dm1*puk*pnk + 8*pdpe
     &    *c3d*ml2*mw2dm1*pupn2 - 16*pdpe*c2d*ml2*mw2dm1*epupdpnpe - 
     &    16*pdpe*c1d*ml2*mw2dm1*epupnpek )
      amp2 = amp2 + c4 * ( 16*pnpe*c3d*epupdpek - 8*pnpe*c3d*ml2*
     &    mw2dm1*pdk*puk + 8*pnpe*c3d*ml2*mw2dm1*epupdpnk - 16*pnpe*
     &    c2d*ml2*mw2dm1*epupdpnk + 16*pnpe*c1d*ml2*mw2dm1*epupdpek )
      amp2 = amp2 + c4*pukc1d * ( 16*ml2*mw2dm1*epupdpnpe + 16*ml2*
     &    mw2dm1*epdpnpek + 16*pupd*pnpe*ml2*mw2dm1 - 32*pupn*pdpn*
     &    ml2*mw2dm1 - 16*pupn*pdpe*ml2*mw2dm1 + 64*pupe*pdpn - 16*
     &    pupe*pdpn*ml2*mw2dm1 - 16*pdpn*pnpe*ml2*mw2dm1 - 64*pdpn
     &    *pek - 8*pdpn*ml4*mw2dm1 + 32*pdpn*ml2 + 16*pdpn*ml2*
     &    mw2dm1*pek + 32*pdpn*ml2*mw2dm1*pnk + 16*pdpe*ml2*mw2dm1*
     &    pnk - 16*pnpe*ml2*mw2dm1*pdk )
      amp2 = amp2 + c4*pdkc2d * ( 16*pupn*pdpn*ml2*mw2dm1 + 16*
     &    pupn*pnpe*ml2*mw2dm1 + 8*pupn*ml4*mw2dm1 - 32*pupe*pdpn
     &     + 16*pupe*pdpn*ml2*mw2dm1 - 32*pupe*pnpe )
      amp2 = amp2 + c4*c4d * (  - 64*pupe*pdpn*ml2 + 64*pdpn*puk
     &    *pek - 64*pdpn*ml2*puk )
      amp2 = amp2 - 4*c3*c3d*ml4*mw2dm1*epupdpnk + 4*c3*c3d*ml4*
     &    mw2m1*epupdpnk - 16*c3*c3d*ml2*mw2m2*pnpe2*pdk*puk + 16*c3*
     &    c3d*ml2*mw2m2*pnpe2*pupd2 + 8*c3*c3d*ml2*mw2dm1*epupdpnpe*pek
     &     + 8*c3*c3d*ml2*mw2dm1*epupdpnpe*pnk - 4*c3*c3d*ml2*mw2dm1*
     &    epupdpnk*pek + 4*c3*c3d*ml2*mw2dm1*epupdpek*pnk - 8*c3*c3d*
     &    ml2*mw2m1*epupdpnpe*pek - 8*c3*c3d*ml2*mw2m1*epupdpnpe*pnk + 
     &    4*c3*c3d*ml2*mw2m1*epupdpnk*pek - 4*c3*c3d*ml2*mw2m1*epupdpek
     &    *pnk - 16*c2d*c3*epupdpnpe*pnk - 16*c2d*c3*ml2*mw2m2*pnpe2*
     &    pupd2 - 16*c2d*c3*ml2*mw2dm1*pdpn2*puk - 8*c2d*c3*ml2*mw2dm1*
     &    epupdpnpe*pek - 8*c2d*c3*ml2*mw2dm1*epupdpnpe*pnk - 16*c2d*c3
     &    *ml2*mw2m1*pdpn2*puk + 16*c2*c3d*epupdpnpe*pnk - 16*c2*c3d*
     &    ml2*mw2m2*pnpe2*pupd2 - 16*c2*c3d*ml2*mw2dm1*pdpn2*puk - 16*
     &    c2*c3d*ml2*mw2m1*pdpn2*puk + 8*c2*c3d*ml2*mw2m1*epupdpnpe*pek
     &     + 8*c2*c3d*ml2*mw2m1*epupdpnpe*pnk - 16*c1d*c3*epupdpnpe*pek
     &     + 16*c1d*c3*ml2*mw2m2*pnpe2*pupd2 + 16*c1d*c3*ml2*mw2dm1*
     &    pupn2*pdk
      amp2 = amp2 + 8*c1d*c3*ml2*mw2dm1*epupdpnpe*pek + 8*c1d*c3*
     &    ml2*mw2dm1*epupdpnpe*pnk + 8*c1d*c3*ml2*mw2dm1*epupdpnpe*pdk
     &     - 8*c1d*c3*ml2*mw2dm1*epupdpnk*pek + 8*c1d*c3*ml2*mw2dm1*
     &    epupdpek*pnk - 8*c1d*c3*ml2*mw2dm1*epupnpek*pdk + 16*c1d*c3*
     &    ml2*mw2m1*pupn2*pdk - 8*c1d*c3*ml2*mw2m1*epupdpnpe*pdk + 8*
     &    c1d*c3*ml2*mw2m1*epupdpnk*pek - 8*c1d*c3*ml2*mw2m1*epupdpek*
     &    pnk + 8*c1d*c3*ml2*mw2m1*epupnpek*pdk - 32*c1d*c2*ml2*mw2m2*
     &    pnpe2*pupd2 + 16*c1*c3d*epupdpnpe*pek + 16*c1*c3d*ml2*mw2m2*
     &    pnpe2*pupd2 + 16*c1*c3d*ml2*mw2dm1*pupn2*pdk + 8*c1*c3d*ml2*
     &    mw2dm1*epupdpnpe*pdk - 8*c1*c3d*ml2*mw2dm1*epupdpnk*pek + 8*
     &    c1*c3d*ml2*mw2dm1*epupdpek*pnk - 8*c1*c3d*ml2*mw2dm1*epupnpek
     &    *pdk + 16*c1*c3d*ml2*mw2m1*pupn2*pdk - 8*c1*c3d*ml2*mw2m1*
     &    epupdpnpe*pek - 8*c1*c3d*ml2*mw2m1*epupdpnpe*pnk - 8*c1*c3d*
     &    ml2*mw2m1*epupdpnpe*pdk + 8*c1*c3d*ml2*mw2m1*epupdpnk*pek - 8
     &    *c1*c3d*ml2*mw2m1*epupdpek*pnk + 8*c1*c3d*ml2*mw2m1*epupnpek*
     &    pdk
      amp2 = amp2 - 32*c1*c2d*ml2*mw2m2*pnpe2*pupd2 + 16*pupd*
     &    pupn*pupe*pnpe*c3*c3d*ml2*mw2m2 + 16*pupd*pupn*pupe*c3*
     &    c3d - 8*pupd*pupn*pupe*c3*c3d*ml2*mw2dm1 - 8*pupd*pupn*
     &    pupe*c3*c3d*ml2*mw2m1 + 16*pupd*pupn*pdpn*pnpe*c2d*c3*
     &    ml2*mw2m2 + 16*pupd*pupn*pdpn*pnpe*c2*c3d*ml2*mw2m2 - 16*
     &    pupd*pupn*pdpn*pnpe*c1d*c3*ml2*mw2m2 + 32*pupd*pupn*
     &    pdpn*pnpe*c1d*c2*ml2*mw2m2 - 16*pupd*pupn*pdpn*pnpe*c1*
     &    c3d*ml2*mw2m2 + 32*pupd*pupn*pdpn*pnpe*c1*c2d*ml2*mw2m2
     &     - 16*pupd*pupn*pdpn*c2d*c3*ml2*mw2dm1 - 16*pupd*pupn*
     &    pdpn*c2d*c3*ml2*mw2m1 - 16*pupd*pupn*pdpn*c2*c3d*ml2*
     &    mw2dm1 - 16*pupd*pupn*pdpn*c2*c3d*ml2*mw2m1 + 16*pupd*
     &    pupn*pdpn*c1d*c3*ml2*mw2dm1 + 16*pupd*pupn*pdpn*c1d*c3*
     &    ml2*mw2m1 - 32*pupd*pupn*pdpn*c1d*c2*ml2*mw2dm1 - 32*pupd
     &    *pupn*pdpn*c1d*c2*ml2*mw2m1 + 16*pupd*pupn*pdpn*c1*c3d*
     &    ml2*mw2dm1 + 16*pupd*pupn*pdpn*c1*c3d*ml2*mw2m1 - 32*pupd
     &    *pupn*pdpn*c1*c2d*ml2*mw2dm1
      amp2 = amp2 - 32*pupd*pupn*pdpn*c1*c2d*ml2*mw2m1 + 16*
     &    pupd*pupn*pdpe*pnpe*c2d*c3*ml2*mw2m2 + 16*pupd*pupn*
     &    pdpe*pnpe*c2*c3d*ml2*mw2m2 - 16*pupd*pupn*pdpe*pnpe*c1d
     &    *c3*ml2*mw2m2 + 32*pupd*pupn*pdpe*pnpe*c1d*c2*ml2*mw2m2
     &     - 16*pupd*pupn*pdpe*pnpe*c1*c3d*ml2*mw2m2 + 32*pupd*
     &    pupn*pdpe*pnpe*c1*c2d*ml2*mw2m2 + 16*pupd*pupn*pdpe*c3*
     &    c3d - 8*pupd*pupn*pdpe*c2d*c3*ml2*mw2dm1 - 8*pupd*pupn*
     &    pdpe*c2d*c3*ml2*mw2m1 - 8*pupd*pupn*pdpe*c2*c3d*ml2*
     &    mw2dm1 - 8*pupd*pupn*pdpe*c2*c3d*ml2*mw2m1 + 8*pupd*pupn
     &    *pdpe*c1d*c3*ml2*mw2dm1 + 8*pupd*pupn*pdpe*c1d*c3*ml2*
     &    mw2m1 - 16*pupd*pupn*pdpe*c1d*c2*ml2*mw2dm1 - 16*pupd*
     &    pupn*pdpe*c1d*c2*ml2*mw2m1 + 8*pupd*pupn*pdpe*c1*c3d*ml2
     &    *mw2dm1 + 8*pupd*pupn*pdpe*c1*c3d*ml2*mw2m1 - 16*pupd*
     &    pupn*pdpe*c1*c2d*ml2*mw2dm1 - 16*pupd*pupn*pdpe*c1*c2d*
     &    ml2*mw2m1 + 16*pupd*pupn*pnpe*c3*c3d + 16*pupd*pupn*
     &    pnpe*c3*c3d*ml2*mw2m2*pek
      amp2 = amp2 + 16*pupd*pupn*pnpe*c3*c3d*ml2*mw2m2*pnk + 4*
     &    pupd*pupn*pnpe*c3*c3d*ml2*mw2dm1 + 4*pupd*pupn*pnpe*c3*
     &    c3d*ml2*mw2m1 - 16*pupd*pupn*pnpe*c2d*c3*ml2*mw2m2*pek - 
     &    16*pupd*pupn*pnpe*c2d*c3*ml2*mw2m2*pnk - 16*pupd*pupn*
     &    pnpe*c2*c3d*ml2*mw2m2*pek - 16*pupd*pupn*pnpe*c2*c3d*ml2*
     &    mw2m2*pnk + 16*pupd*pupn*pnpe*c1d*c3*ml2*mw2m2*pek + 16*
     &    pupd*pupn*pnpe*c1d*c3*ml2*mw2m2*pnk + 8*pupd*pupn*pnpe*
     &    c1d*c3*ml2*mw2dm1 - 16*pupd*pupn*pnpe*c1d*c2*ml2*mw2m2*pek
     &     - 16*pupd*pupn*pnpe*c1d*c2*ml2*mw2m2*pnk + 16*pupd*pupn
     &    *pnpe*c1*c3d*ml2*mw2m2*pek + 16*pupd*pupn*pnpe*c1*c3d*ml2
     &    *mw2m2*pnk + 8*pupd*pupn*pnpe*c1*c3d*ml2*mw2m1 - 16*pupd*
     &    pupn*pnpe*c1*c2d*ml2*mw2m2*pek - 16*pupd*pupn*pnpe*c1*
     &    c2d*ml2*mw2m2*pnk + 4*pupd*pupn*c3*c3d*ml4*mw2dm1 + 4*pupd
     &    *pupn*c3*c3d*ml4*mw2m1 + 16*pupd*pupn*c3*c3d*ml2 - 8*pupd
     &    *pupn*c3*c3d*ml2*mw2dm1*pek - 16*pupd*pupn*c3*c3d*ml2*
     &    mw2dm1*pnk
      amp2 = amp2 - 8*pupd*pupn*c3*c3d*ml2*mw2m1*pek - 16*pupd*
     &    pupn*c3*c3d*ml2*mw2m1*pnk + 8*pupd*pupn*c2d*c3*ml2*mw2dm1*
     &    pek + 16*pupd*pupn*c2d*c3*ml2*mw2dm1*pnk + 8*pupd*pupn*
     &    c2d*c3*ml2*mw2m1*pek + 16*pupd*pupn*c2d*c3*ml2*mw2m1*pnk + 
     &    8*pupd*pupn*c2*c3d*ml2*mw2dm1*pek + 16*pupd*pupn*c2*c3d*
     &    ml2*mw2dm1*pnk + 8*pupd*pupn*c2*c3d*ml2*mw2m1*pek + 16*
     &    pupd*pupn*c2*c3d*ml2*mw2m1*pnk + 8*pupd*pupn*c1d*c3*ml4*
     &    mw2dm1 - 16*pupd*pupn*c1d*c3*ml2*mw2dm1*pek - 16*pupd*
     &    pupn*c1d*c3*ml2*mw2dm1*pnk - 16*pupd*pupn*c1d*c3*ml2*mw2m1
     &    *pnk + 16*pupd*pupn*c1d*c2*ml2*mw2dm1*pek + 16*pupd*pupn*
     &    c1d*c2*ml2*mw2dm1*pnk + 16*pupd*pupn*c1d*c2*ml2*mw2m1*pnk
     &     + 8*pupd*pupn*c1*c3d*ml4*mw2m1 - 16*pupd*pupn*c1*c3d*ml2
     &    *mw2dm1*pnk - 16*pupd*pupn*c1*c3d*ml2*mw2m1*pek - 16*pupd*
     &    pupn*c1*c3d*ml2*mw2m1*pnk + 16*pupd*pupn*c1*c2d*ml2*mw2dm1
     &    *pnk + 16*pupd*pupn*c1*c2d*ml2*mw2m1*pek + 16*pupd*pupn*
     &    c1*c2d*ml2*mw2m1*pnk
      amp2 = amp2 + 16*pupd*pupe*pdpn*pnpe*c2d*c3*ml2*mw2m2 + 
     &    16*pupd*pupe*pdpn*pnpe*c2*c3d*ml2*mw2m2 - 16*pupd*pupe*
     &    pdpn*pnpe*c1d*c3*ml2*mw2m2 + 32*pupd*pupe*pdpn*pnpe*c1d
     &    *c2*ml2*mw2m2 - 16*pupd*pupe*pdpn*pnpe*c1*c3d*ml2*mw2m2
     &     + 32*pupd*pupe*pdpn*pnpe*c1*c2d*ml2*mw2m2 - 16*pupd*
     &    pupe*pdpn*c3*c3d + 32*pupd*pupe*pdpn*c2d*c3 - 8*pupd*
     &    pupe*pdpn*c2d*c3*ml2*mw2dm1 - 8*pupd*pupe*pdpn*c2d*c3*
     &    ml2*mw2m1 + 32*pupd*pupe*pdpn*c2*c3d - 8*pupd*pupe*pdpn
     &    *c2*c3d*ml2*mw2dm1 - 8*pupd*pupe*pdpn*c2*c3d*ml2*mw2m1 - 
     &    32*pupd*pupe*pdpn*c1d*c3 + 8*pupd*pupe*pdpn*c1d*c3*ml2*
     &    mw2dm1 + 8*pupd*pupe*pdpn*c1d*c3*ml2*mw2m1 + 64*pupd*
     &    pupe*pdpn*c1d*c2 - 16*pupd*pupe*pdpn*c1d*c2*ml2*mw2dm1
     &     - 16*pupd*pupe*pdpn*c1d*c2*ml2*mw2m1 - 32*pupd*pupe*
     &    pdpn*c1*c3d + 8*pupd*pupe*pdpn*c1*c3d*ml2*mw2dm1 + 8*
     &    pupd*pupe*pdpn*c1*c3d*ml2*mw2m1 + 64*pupd*pupe*pdpn*c1*
     &    c2d
      amp2 = amp2 - 16*pupd*pupe*pdpn*c1*c2d*ml2*mw2dm1 - 16*
     &    pupd*pupe*pdpn*c1*c2d*ml2*mw2m1 + 16*pupd*pupe*pdpe*
     &    pnpe*c2d*c3*ml2*mw2m2 + 16*pupd*pupe*pdpe*pnpe*c2*c3d*
     &    ml2*mw2m2 - 16*pupd*pupe*pdpe*pnpe*c1d*c3*ml2*mw2m2 + 32*
     &    pupd*pupe*pdpe*pnpe*c1d*c2*ml2*mw2m2 - 16*pupd*pupe*
     &    pdpe*pnpe*c1*c3d*ml2*mw2m2 + 32*pupd*pupe*pdpe*pnpe*c1*
     &    c2d*ml2*mw2m2 - 32*pupd*pupe*pnpe*c3*c3d + 16*pupd*pupe*
     &    pnpe*c3*c3d*ml2*mw2m2*pek + 16*pupd*pupe*pnpe*c3*c3d*ml2*
     &    mw2m2*pnk - 4*pupd*pupe*pnpe*c3*c3d*ml2*mw2dm1 - 4*pupd*
     &    pupe*pnpe*c3*c3d*ml2*mw2m1 - 16*pupd*pupe*pnpe*c2d*c3*
     &    ml2*mw2m2*pek - 16*pupd*pupe*pnpe*c2d*c3*ml2*mw2m2*pnk - 
     &    16*pupd*pupe*pnpe*c2*c3d*ml2*mw2m2*pek - 16*pupd*pupe*
     &    pnpe*c2*c3d*ml2*mw2m2*pnk - 16*pupd*pupe*pnpe*c1d*c3 + 16
     &    *pupd*pupe*pnpe*c1d*c3*ml2*mw2m2*pek + 16*pupd*pupe*
     &    pnpe*c1d*c3*ml2*mw2m2*pnk - 8*pupd*pupe*pnpe*c1d*c3*ml2*
     &    mw2dm1
      amp2 = amp2 - 16*pupd*pupe*pnpe*c1d*c2*ml2*mw2m2*pek - 16*
     &    pupd*pupe*pnpe*c1d*c2*ml2*mw2m2*pnk - 16*pupd*pupe*pnpe
     &    *c1*c3d + 16*pupd*pupe*pnpe*c1*c3d*ml2*mw2m2*pek + 16*
     &    pupd*pupe*pnpe*c1*c3d*ml2*mw2m2*pnk - 8*pupd*pupe*pnpe*
     &    c1*c3d*ml2*mw2m1 - 16*pupd*pupe*pnpe*c1*c2d*ml2*mw2m2*pek
     &     - 16*pupd*pupe*pnpe*c1*c2d*ml2*mw2m2*pnk + 32*pupd*pupe
     &    *c3*c3d*pnk - 8*pupd*pupe*c3*c3d*ml2*mw2dm1*pnk - 8*pupd*
     &    pupe*c3*c3d*ml2*mw2m1*pnk - 32*pupd*pupe*c2d*c3*pnk + 8*
     &    pupd*pupe*c2d*c3*ml2*mw2dm1*pnk + 8*pupd*pupe*c2d*c3*ml2*
     &    mw2m1*pnk - 32*pupd*pupe*c2*c3d*pnk + 8*pupd*pupe*c2*c3d*
     &    ml2*mw2dm1*pnk + 8*pupd*pupe*c2*c3d*ml2*mw2m1*pnk + 32*
     &    pupd*pupe*c1d*c3*pnk - 16*pupd*pupe*c1d*c3*ml2*mw2m1*pnk
     &     - 32*pupd*pupe*c1d*c2*pnk + 16*pupd*pupe*c1d*c2*ml2*
     &    mw2m1*pnk + 32*pupd*pupe*c1*c3d*pnk - 16*pupd*pupe*c1*c3d
     &    *ml2*mw2dm1*pnk - 32*pupd*pupe*c1*c2d*pnk + 16*pupd*pupe*
     &    c1*c2d*ml2*mw2dm1*pnk
      amp2 = amp2 + 16*pupd*pdpn*pdpe*pnpe*c3*c3d*ml2*mw2m2 + 
     &    16*pupd*pdpn*pdpe*c3*c3d - 8*pupd*pdpn*pdpe*c3*c3d*ml2*
     &    mw2dm1 - 8*pupd*pdpn*pdpe*c3*c3d*ml2*mw2m1 - 32*pupd*
     &    pdpn*pnpe*c3*c3d + 16*pupd*pdpn*pnpe*c3*c3d*ml2*mw2m2*
     &    pek + 16*pupd*pdpn*pnpe*c3*c3d*ml2*mw2m2*pnk + 4*pupd*
     &    pdpn*pnpe*c3*c3d*ml2*mw2dm1 + 4*pupd*pdpn*pnpe*c3*c3d*
     &    ml2*mw2m1 + 16*pupd*pdpn*pnpe*c2d*c3 - 16*pupd*pdpn*
     &    pnpe*c2d*c3*ml2*mw2m2*pek - 16*pupd*pdpn*pnpe*c2d*c3*ml2*
     &    mw2m2*pnk - 8*pupd*pdpn*pnpe*c2d*c3*ml2*mw2dm1 + 16*pupd*
     &    pdpn*pnpe*c2*c3d - 16*pupd*pdpn*pnpe*c2*c3d*ml2*mw2m2*
     &    pek - 16*pupd*pdpn*pnpe*c2*c3d*ml2*mw2m2*pnk - 8*pupd*
     &    pdpn*pnpe*c2*c3d*ml2*mw2m1 + 16*pupd*pdpn*pnpe*c1d*c3*
     &    ml2*mw2m2*pek + 16*pupd*pdpn*pnpe*c1d*c3*ml2*mw2m2*pnk - 
     &    16*pupd*pdpn*pnpe*c1d*c2*ml2*mw2m2*pek - 16*pupd*pdpn*
     &    pnpe*c1d*c2*ml2*mw2m2*pnk + 16*pupd*pdpn*pnpe*c1*c3d*ml2*
     &    mw2m2*pek
      amp2 = amp2 + 16*pupd*pdpn*pnpe*c1*c3d*ml2*mw2m2*pnk - 16*
     &    pupd*pdpn*pnpe*c1*c2d*ml2*mw2m2*pek - 16*pupd*pdpn*pnpe
     &    *c1*c2d*ml2*mw2m2*pnk + 32*pupd*pdpn*c3*c3d*pek + 4*pupd*
     &    pdpn*c3*c3d*ml4*mw2dm1 + 4*pupd*pdpn*c3*c3d*ml4*mw2m1 - 32
     &    *pupd*pdpn*c3*c3d*ml2 - 8*pupd*pdpn*c3*c3d*ml2*mw2dm1*pek
     &     - 16*pupd*pdpn*c3*c3d*ml2*mw2dm1*pnk - 8*pupd*pdpn*c3*
     &    c3d*ml2*mw2m1*pek - 16*pupd*pdpn*c3*c3d*ml2*mw2m1*pnk - 32*
     &    pupd*pdpn*c2d*c3*pek - 8*pupd*pdpn*c2d*c3*ml4*mw2dm1 + 16
     &    *pupd*pdpn*c2d*c3*ml2 + 16*pupd*pdpn*c2d*c3*ml2*mw2dm1*
     &    pek + 16*pupd*pdpn*c2d*c3*ml2*mw2dm1*pnk + 16*pupd*pdpn*
     &    c2d*c3*ml2*mw2m1*pnk - 32*pupd*pdpn*c2*c3d*pek - 8*pupd*
     &    pdpn*c2*c3d*ml4*mw2m1 + 16*pupd*pdpn*c2*c3d*ml2 + 16*pupd
     &    *pdpn*c2*c3d*ml2*mw2dm1*pnk + 16*pupd*pdpn*c2*c3d*ml2*
     &    mw2m1*pek + 16*pupd*pdpn*c2*c3d*ml2*mw2m1*pnk + 32*pupd*
     &    pdpn*c1d*c3*pek - 8*pupd*pdpn*c1d*c3*ml2*mw2dm1*pek - 16*
     &    pupd*pdpn*c1d*c3*ml2*mw2dm1*pnk
      amp2 = amp2 - 8*pupd*pdpn*c1d*c3*ml2*mw2m1*pek - 16*pupd*
     &    pdpn*c1d*c3*ml2*mw2m1*pnk - 32*pupd*pdpn*c1d*c2*pek + 16*
     &    pupd*pdpn*c1d*c2*ml2*mw2dm1*pnk + 16*pupd*pdpn*c1d*c2*ml2
     &    *mw2m1*pek + 16*pupd*pdpn*c1d*c2*ml2*mw2m1*pnk + 32*pupd*
     &    pdpn*c1*c3d*pek - 8*pupd*pdpn*c1*c3d*ml2*mw2dm1*pek - 16*
     &    pupd*pdpn*c1*c3d*ml2*mw2dm1*pnk - 8*pupd*pdpn*c1*c3d*ml2*
     &    mw2m1*pek - 16*pupd*pdpn*c1*c3d*ml2*mw2m1*pnk - 32*pupd*
     &    pdpn*c1*c2d*pek + 16*pupd*pdpn*c1*c2d*ml2*mw2dm1*pek + 16*
     &    pupd*pdpn*c1*c2d*ml2*mw2dm1*pnk + 16*pupd*pdpn*c1*c2d*ml2
     &    *mw2m1*pnk + 16*pupd*pdpe*pnpe*c3*c3d + 16*pupd*pdpe*
     &    pnpe*c3*c3d*ml2*mw2m2*pek + 16*pupd*pdpe*pnpe*c3*c3d*ml2*
     &    mw2m2*pnk - 4*pupd*pdpe*pnpe*c3*c3d*ml2*mw2dm1 - 4*pupd*
     &    pdpe*pnpe*c3*c3d*ml2*mw2m1 - 16*pupd*pdpe*pnpe*c2d*c3*
     &    ml2*mw2m2*pek - 16*pupd*pdpe*pnpe*c2d*c3*ml2*mw2m2*pnk + 8
     &    *pupd*pdpe*pnpe*c2d*c3*ml2*mw2dm1 - 16*pupd*pdpe*pnpe*
     &    c2*c3d*ml2*mw2m2*pek
      amp2 = amp2 - 16*pupd*pdpe*pnpe*c2*c3d*ml2*mw2m2*pnk + 8*
     &    pupd*pdpe*pnpe*c2*c3d*ml2*mw2m1 + 16*pupd*pdpe*pnpe*c1d
     &    *c3*ml2*mw2m2*pek + 16*pupd*pdpe*pnpe*c1d*c3*ml2*mw2m2*pnk
     &     - 16*pupd*pdpe*pnpe*c1d*c2*ml2*mw2m2*pek - 16*pupd*pdpe
     &    *pnpe*c1d*c2*ml2*mw2m2*pnk + 16*pupd*pdpe*pnpe*c1*c3d*ml2
     &    *mw2m2*pek + 16*pupd*pdpe*pnpe*c1*c3d*ml2*mw2m2*pnk - 16*
     &    pupd*pdpe*pnpe*c1*c2d*ml2*mw2m2*pek - 16*pupd*pdpe*pnpe
     &    *c1*c2d*ml2*mw2m2*pnk - 8*pupd*pdpe*c3*c3d*ml2*mw2dm1*pnk
     &     - 8*pupd*pdpe*c3*c3d*ml2*mw2m1*pnk + 16*pupd*pdpe*c2d*c3
     &    *ml2*mw2m1*pnk + 16*pupd*pdpe*c2*c3d*ml2*mw2dm1*pnk - 8*
     &    pupd*pdpe*c1d*c3*ml2*mw2dm1*pnk - 8*pupd*pdpe*c1d*c3*ml2*
     &    mw2m1*pnk + 16*pupd*pdpe*c1d*c2*ml2*mw2dm1*pnk - 8*pupd*
     &    pdpe*c1*c3d*ml2*mw2dm1*pnk - 8*pupd*pdpe*c1*c3d*ml2*mw2m1*
     &    pnk + 16*pupd*pdpe*c1*c2d*ml2*mw2m1*pnk + 16*pupd*pnpe*c3
     &    *c3d*pek + 16*pupd*pnpe*c3*c3d*pnk - 16*pupd*pnpe*c3*c3d*
     &    puk
      amp2 = amp2 - 16*pupd*pnpe*c3*c3d*pdk - 16*pupd*pnpe*c3*
     &    c3d*ml2 + 8*pupd*pnpe*c3*c3d*ml2*mw2m2*pdpe2 + 8*pupd*
     &    pnpe*c3*c3d*ml2*mw2m2*pdpn2 + 8*pupd*pnpe*c3*c3d*ml2*mw2m2
     &    *pupe2 + 8*pupd*pnpe*c3*c3d*ml2*mw2m2*pupn2 + 4*pupd*pnpe
     &    *c3*c3d*ml2*mw2dm1*pek - 4*pupd*pnpe*c3*c3d*ml2*mw2dm1*pnk
     &     + 4*pupd*pnpe*c3*c3d*ml2*mw2m1*pek - 4*pupd*pnpe*c3*c3d*
     &    ml2*mw2m1*pnk - 16*pupd*pnpe*c2d*c3*pnk + 8*pupd*pnpe*c2d
     &    *c3*ml4*mw2m2*puk - 8*pupd*pnpe*c2d*c3*ml2*mw2dm1*pek + 8*
     &    pupd*pnpe*c2d*c3*ml2*mw2dm1*pnk - 8*pupd*pnpe*c2d*c3*ml2*
     &    mw2dm1*puk - 8*pupd*pnpe*c2d*c3*ml2*mw2m1*puk - 16*pupd*
     &    pnpe*c2*c3d*pnk + 8*pupd*pnpe*c2*c3d*ml4*mw2m2*puk - 8*
     &    pupd*pnpe*c2*c3d*ml2*mw2dm1*puk - 8*pupd*pnpe*c2*c3d*ml2*
     &    mw2m1*pek + 8*pupd*pnpe*c2*c3d*ml2*mw2m1*pnk - 8*pupd*
     &    pnpe*c2*c3d*ml2*mw2m1*puk + 16*pupd*pnpe*c1d*c3*pek - 8*
     &    pupd*pnpe*c1d*c3*ml4*mw2m2*pdk + 8*pupd*pnpe*c1d*c3*ml2*
     &    mw2dm1*pek
      amp2 = amp2 - 8*pupd*pnpe*c1d*c3*ml2*mw2dm1*pnk + 8*pupd*
     &    pnpe*c1d*c3*ml2*mw2dm1*pdk + 8*pupd*pnpe*c1d*c3*ml2*mw2m1*
     &    pdk - 16*pupd*pnpe*c1d*c2*ml2*mw2m2*epupnpek + 16*pupd*
     &    pnpe*c1*c3d*pek - 8*pupd*pnpe*c1*c3d*ml4*mw2m2*pdk + 8*
     &    pupd*pnpe*c1*c3d*ml2*mw2dm1*pdk + 8*pupd*pnpe*c1*c3d*ml2*
     &    mw2m1*pek - 8*pupd*pnpe*c1*c3d*ml2*mw2m1*pnk + 8*pupd*
     &    pnpe*c1*c3d*ml2*mw2m1*pdk + 16*pupd*pnpe*c1*c2d*ml2*mw2m2*
     &    epupnpek - 16*pupd*c3*c3d*pnpe2 - 4*pupd*c3*c3d*ml4*mw2dm1*
     &    pnk - 4*pupd*c3*c3d*ml4*mw2m1*pnk + 8*pupd*c3*c3d*ml2*pnk
     &     - 8*pupd*c3*c3d*ml2*mw2dm1*pdpn2 - 8*pupd*c3*c3d*ml2*
     &    mw2dm1*pupn2 - 8*pupd*c3*c3d*ml2*mw2dm1*epupdpnpe - 8*pupd*
     &    c3*c3d*ml2*mw2m1*pdpn2 - 8*pupd*c3*c3d*ml2*mw2m1*pupn2 + 8*
     &    pupd*c3*c3d*ml2*mw2m1*epupdpnpe + 8*pupd*c2d*c3*ml4*mw2dm1*
     &    pnk - 16*pupd*c2d*c3*ml2*pnk + 16*pupd*c2d*c3*ml2*mw2m2*
     &    pnpe2*puk + 8*pupd*c2d*c3*ml2*mw2dm1*epupdpnpe - 8*pupd*c2d
     &    *c3*ml2*mw2dm1*epupnpek
      amp2 = amp2 - 8*pupd*c2d*c3*ml2*mw2m1*epupdpnpe + 8*pupd*
     &    c2d*c3*ml2*mw2m1*epupnpek + 8*pupd*c2*c3d*ml4*mw2m1*pnk - 16
     &    *pupd*c2*c3d*ml2*pnk + 16*pupd*c2*c3d*ml2*mw2m2*pnpe2*puk
     &     + 8*pupd*c2*c3d*ml2*mw2dm1*epupdpnpe - 8*pupd*c2*c3d*ml2*
     &    mw2dm1*epupnpek - 8*pupd*c2*c3d*ml2*mw2m1*epupdpnpe + 8*
     &    pupd*c2*c3d*ml2*mw2m1*epupnpek + 16*pupd*c1d*c3*epupnpek - 
     &    8*pupd*c1d*c3*ml4*mw2dm1*pnk - 16*pupd*c1d*c3*ml2*mw2m2*
     &    pnpe2*pdk - 8*pupd*c1d*c3*ml2*mw2dm1*epupdpnpe + 8*pupd*c1d
     &    *c3*ml2*mw2m1*epupdpnpe + 16*pupd*c1d*c2*ml2*mw2dm1*
     &    epupdpnpe - 16*pupd*c1d*c2*ml2*mw2dm1*epupnpek - 16*pupd*
     &    c1d*c2*ml2*mw2m1*epupdpnpe + 16*pupd*c1d*c2*ml2*mw2m1*
     &    epupnpek - 16*pupd*c1*c3d*epupnpek - 8*pupd*c1*c3d*ml4*
     &    mw2m1*pnk - 16*pupd*c1*c3d*ml2*mw2m2*pnpe2*pdk - 8*pupd*c1*
     &    c3d*ml2*mw2dm1*epupdpnpe + 8*pupd*c1*c3d*ml2*mw2m1*epupdpnpe
     &     + 16*pupd*c1*c2d*ml2*mw2dm1*epupdpnpe - 16*pupd*c1*c2d*ml2
     &    *mw2dm1*epupnpek
      amp2 = amp2 - 16*pupd*c1*c2d*ml2*mw2m1*epupdpnpe + 16*pupd*
     &    c1*c2d*ml2*mw2m1*epupnpek + 16*pupn*pupe*pdpn*c3*c3d + 4*
     &    pupn*pupe*pdpn*c3*c3d*ml2*mw2dm1 + 4*pupn*pupe*pdpn*c3*
     &    c3d*ml2*mw2m1 + 8*pupn*pupe*pdpn*c1d*c3*ml2*mw2dm1 + 8*
     &    pupn*pupe*pdpn*c1*c3d*ml2*mw2m1 - 4*pupn*pupe*pdpe*c3*
     &    c3d*ml2*mw2dm1 - 4*pupn*pupe*pdpe*c3*c3d*ml2*mw2m1 + 16*
     &    pupn*pupe*pdpe*c1d*c3 - 8*pupn*pupe*pdpe*c1d*c3*ml2*
     &    mw2dm1 + 16*pupn*pupe*pdpe*c1*c3d - 8*pupn*pupe*pdpe*c1
     &    *c3d*ml2*mw2m1 - 32*pupn*pupe*pnpe*c1d*c3*ml2*mw2m2*pdk - 
     &    32*pupn*pupe*pnpe*c1*c3d*ml2*mw2m2*pdk - 32*pupn*pupe*
     &    c1d*c3*pdk + 16*pupn*pupe*c1d*c3*ml2*mw2dm1*pdk + 16*pupn*
     &    pupe*c1d*c3*ml2*mw2m1*pdk - 32*pupn*pupe*c1*c3d*pdk + 16*
     &    pupn*pupe*c1*c3d*ml2*mw2dm1*pdk + 16*pupn*pupe*c1*c3d*ml2
     &    *mw2m1*pdk + 4*pupn*pdpn*pdpe*c3*c3d*ml2*mw2dm1 + 4*pupn*
     &    pdpn*pdpe*c3*c3d*ml2*mw2m1 - 16*pupn*pdpn*pdpe*c2d*c3 - 
     &    8*pupn*pdpn*pdpe*c2d*c3*ml2*mw2dm1
      amp2 = amp2 - 16*pupn*pdpn*pdpe*c2*c3d - 8*pupn*pdpn*
     &    pdpe*c2*c3d*ml2*mw2m1 + 16*pupn*pdpn*pnpe*c3*c3d + 16*
     &    pupn*pdpn*c3*c3d*ml2 + 8*pupn*pdpn*c2d*c3*ml2*mw2dm1*pek
     &     + 8*pupn*pdpn*c2*c3d*ml2*mw2m1*pek - 16*pupn*pdpn*c1d*c3
     &    *pek - 8*pupn*pdpn*c1d*c3*ml2*mw2dm1*pek - 16*pupn*pdpn*
     &    c1*c3d*pek - 8*pupn*pdpn*c1*c3d*ml2*mw2m1*pek + 16*pupn*
     &    pdpe*pnpe*c3*c3d - 16*pupn*pdpe*c3*c3d*pek - 16*pupn*
     &    pdpe*c3*c3d*pnk + 16*pupn*pdpe*c3*c3d*puk + 16*pupn*pdpe
     &    *c3*c3d*pdk + 8*pupn*pdpe*c3*c3d*ml2 + 16*pupn*pdpe*c2d*
     &    c3*pnk + 8*pupn*pdpe*c2d*c3*ml2*mw2dm1*pek + 16*pupn*pdpe
     &    *c2*c3d*pnk + 8*pupn*pdpe*c2*c3d*ml2*mw2m1*pek - 16*pupn*
     &    pdpe*c1d*c3*pek + 8*pupn*pdpe*c1d*c3*ml2*mw2dm1*pnk - 16*
     &    pupn*pdpe*c1*c3d*pek + 8*pupn*pdpe*c1*c3d*ml2*mw2m1*pnk
     &     + 8*pupn*pnpe*c3*c3d*ml2*mw2m2*pdk*pek + 8*pupn*pnpe*c3*
     &    c3d*ml2*mw2m2*pdk*pnk + 4*pupn*pnpe*c3*c3d*ml2*mw2dm1*pdk
     &     + 4*pupn*pnpe*c3*c3d*ml2*mw2m1*pdk
      amp2 = amp2 - 16*pupn*pnpe*c1d*c3*ml2*mw2m2*epupdpnk - 16*
     &    pupn*pnpe*c1d*c3*ml2*mw2m2*epupdpek + 16*pupn*pnpe*c1d*c2
     &    *ml2*mw2m2*epupdpnk + 32*pupn*pnpe*c1d*c2*ml2*mw2m2*
     &    epupdpek + 16*pupn*pnpe*c1*c3d*ml2*mw2m2*epupdpnk + 16*
     &    pupn*pnpe*c1*c3d*ml2*mw2m2*epupdpek - 16*pupn*pnpe*c1*c2d
     &    *ml2*mw2m2*epupdpnk - 32*pupn*pnpe*c1*c2d*ml2*mw2m2*
     &    epupdpek - 16*pupn*c3*c3d*pdpe2 + 4*pupn*c3*c3d*ml4*mw2dm1*
     &    pdk + 4*pupn*c3*c3d*ml4*mw2m1*pdk + 8*pupn*c3*c3d*ml2*pdk
     &     - 4*pupn*c3*c3d*ml2*mw2dm1*pdk*pek - 8*pupn*c3*c3d*ml2*
     &    mw2dm1*pdk*pnk + 4*pupn*c3*c3d*ml2*mw2dm1*pdpe2 + 4*pupn*c3
     &    *c3d*ml2*mw2dm1*epupdpnpe + 8*pupn*c3*c3d*ml2*mw2dm1*
     &    epupdpek - 4*pupn*c3*c3d*ml2*mw2m1*pdk*pek - 8*pupn*c3*c3d*
     &    ml2*mw2m1*pdk*pnk + 4*pupn*c3*c3d*ml2*mw2m1*pdpe2 - 4*pupn*
     &    c3*c3d*ml2*mw2m1*epupdpnpe - 8*pupn*c3*c3d*ml2*mw2m1*
     &    epupdpek - 8*pupn*c2d*c3*ml2*mw2dm1*pdpe2 + 8*pupn*c2d*c3*
     &    ml2*mw2dm1*epdpnpek
      amp2 = amp2 - 8*pupn*c2*c3d*ml2*mw2m1*pdpe2 - 8*pupn*c2*c3d
     &    *ml2*mw2m1*epdpnpek - 16*pupn*c1d*c3*epupdpek - 8*pupn*c1d*
     &    c3*ml2*mw2dm1*epupdpnpe + 16*pupn*c1d*c3*ml2*mw2dm1*epupdpnk
     &     + 16*pupn*c1d*c3*ml2*mw2dm1*epupdpek + 16*pupn*c1d*c3*ml2*
     &    mw2m1*epupdpnk - 16*pupn*c1d*c2*ml2*mw2dm1*epupdpnk - 16*
     &    pupn*c1d*c2*ml2*mw2m1*epupdpnk - 16*pupn*c1d*c2*ml2*mw2m1*
     &    epupdpek + 16*pupn*c1*c3d*epupdpek - 16*pupn*c1*c3d*ml2*
     &    mw2dm1*epupdpnk + 8*pupn*c1*c3d*ml2*mw2m1*epupdpnpe - 16*
     &    pupn*c1*c3d*ml2*mw2m1*epupdpnk - 16*pupn*c1*c3d*ml2*mw2m1*
     &    epupdpek + 16*pupn*c1*c2d*ml2*mw2dm1*epupdpnk + 16*pupn*c1*
     &    c2d*ml2*mw2dm1*epupdpek + 16*pupn*c1*c2d*ml2*mw2m1*epupdpnk
     &     + 16*pupe*pdpn*pdpe*c3*c3d - 4*pupe*pdpn*pdpe*c3*c3d*
     &    ml2*mw2dm1 - 4*pupe*pdpn*pdpe*c3*c3d*ml2*mw2m1 + 8*pupe*
     &    pdpn*pdpe*c2d*c3*ml2*mw2dm1 + 8*pupe*pdpn*pdpe*c2*c3d*
     &    ml2*mw2m1 - 16*pupe*pdpn*pnpe*c3*c3d + 16*pupe*pdpn*c3*
     &    c3d*pek
      amp2 = amp2 + 16*pupe*pdpn*c3*c3d*pnk - 16*pupe*pdpn*c3*
     &    c3d*puk - 16*pupe*pdpn*c3*c3d*pdk - 8*pupe*pdpn*c3*c3d*
     &    ml2 - 16*pupe*pdpn*c2d*c3*pek - 16*pupe*pdpn*c2d*c3*pnk
     &     - 8*pupe*pdpn*c2d*c3*ml2*mw2dm1*pnk - 16*pupe*pdpn*c2*
     &    c3d*pek - 16*pupe*pdpn*c2*c3d*pnk - 8*pupe*pdpn*c2*c3d*
     &    ml2*mw2m1*pnk + 16*pupe*pdpn*c1d*c3*pek + 16*pupe*pdpn*
     &    c1d*c3*pnk - 8*pupe*pdpn*c1d*c3*ml2*mw2dm1*pek + 16*pupe*
     &    pdpn*c1*c3d*pek + 16*pupe*pdpn*c1*c3d*pnk - 8*pupe*pdpn*
     &    c1*c3d*ml2*mw2m1*pek + 16*pupe*pdpe*pnpe*c3*c3d + 16*pupe
     &    *pdpe*c2d*c3*pnk - 8*pupe*pdpe*c2d*c3*ml2*mw2dm1*pnk + 16*
     &    pupe*pdpe*c2*c3d*pnk - 8*pupe*pdpe*c2*c3d*ml2*mw2m1*pnk
     &     + 8*pupe*pdpe*c1d*c3*ml2*mw2dm1*pnk + 8*pupe*pdpe*c1*c3d
     &    *ml2*mw2m1*pnk - 32*pupe*pnpe*c3*c3d*pdk + 8*pupe*pnpe*c3
     &    *c3d*ml2*mw2m2*pdk*pek + 8*pupe*pnpe*c3*c3d*ml2*mw2m2*pdk*
     &    pnk - 4*pupe*pnpe*c3*c3d*ml2*mw2dm1*pdk - 4*pupe*pnpe*c3*
     &    c3d*ml2*mw2m1*pdk
      amp2 = amp2 - 16*pupe*pnpe*c1d*c3*ml2*mw2m2*epupdpnk - 16*
     &    pupe*pnpe*c1d*c3*ml2*mw2m2*epupdpek + 16*pupe*pnpe*c1d*c2
     &    *ml2*mw2m2*epupdpek + 16*pupe*pnpe*c1*c3d*ml2*mw2m2*
     &    epupdpnk + 16*pupe*pnpe*c1*c3d*ml2*mw2m2*epupdpek - 16*
     &    pupe*pnpe*c1*c2d*ml2*mw2m2*epupdpek + 16*pupe*c3*c3d*pdk*
     &    pnk - 4*pupe*c3*c3d*ml2*mw2dm1*pdk*pnk - 4*pupe*c3*c3d*ml2*
     &    mw2dm1*pdpn2 + 4*pupe*c3*c3d*ml2*mw2dm1*epupdpnpe - 8*pupe*
     &    c3*c3d*ml2*mw2dm1*epupdpnk - 4*pupe*c3*c3d*ml2*mw2m1*pdk*pnk
     &     - 4*pupe*c3*c3d*ml2*mw2m1*pdpn2 - 4*pupe*c3*c3d*ml2*mw2m1*
     &    epupdpnpe + 8*pupe*c3*c3d*ml2*mw2m1*epupdpnk + 16*pupe*c2d*
     &    c3*pdpn2 - 16*pupe*c2d*c3*epdpnpek + 8*pupe*c2d*c3*ml2*
     &    mw2dm1*pdpn2 + 8*pupe*c2d*c3*ml2*mw2dm1*epdpnpek + 16*pupe*
     &    c2*c3d*pdpn2 + 16*pupe*c2*c3d*epdpnpek + 8*pupe*c2*c3d*ml2*
     &    mw2m1*pdpn2 - 8*pupe*c2*c3d*ml2*mw2m1*epdpnpek + 16*pupe*
     &    c1d*c3*epupdpnpe - 16*pupe*c1d*c3*epupdpnk - 8*pupe*c1d*c3*
     &    ml2*mw2dm1*epupdpnpe
      amp2 = amp2 + 16*pupe*c1d*c3*ml2*mw2m1*epupdpnk + 32*pupe*
     &    c1d*c2*epupdpnk - 16*pupe*c1d*c2*ml2*mw2dm1*epupdpnk - 16*
     &    pupe*c1*c3d*epupdpnpe + 16*pupe*c1*c3d*epupdpnk - 16*pupe*
     &    c1*c3d*ml2*mw2dm1*epupdpnk + 8*pupe*c1*c3d*ml2*mw2m1*
     &    epupdpnpe - 32*pupe*c1*c2d*epupdpnk + 16*pupe*c1*c2d*ml2*
     &    mw2m1*epupdpnk + 32*pdpn*pdpe*pnpe*c2d*c3*ml2*mw2m2*puk + 
     &    32*pdpn*pdpe*pnpe*c2*c3d*ml2*mw2m2*puk + 32*pdpn*pdpe*
     &    c2d*c3*puk - 16*pdpn*pdpe*c2d*c3*ml2*mw2dm1*puk - 16*pdpn*
     &    pdpe*c2d*c3*ml2*mw2m1*puk + 32*pdpn*pdpe*c2*c3d*puk - 16*
     &    pdpn*pdpe*c2*c3d*ml2*mw2dm1*puk - 16*pdpn*pdpe*c2*c3d*ml2
     &    *mw2m1*puk - 32*pdpn*pnpe*c3*c3d*puk + 8*pdpn*pnpe*c3*c3d
     &    *ml2*mw2m2*puk*pek + 8*pdpn*pnpe*c3*c3d*ml2*mw2m2*puk*pnk
     &     + 4*pdpn*pnpe*c3*c3d*ml2*mw2dm1*puk + 4*pdpn*pnpe*c3*c3d
     &    *ml2*mw2m1*puk + 16*pdpn*pnpe*c2d*c3*ml2*mw2m2*epupdpnk + 
     &    16*pdpn*pnpe*c2d*c3*ml2*mw2m2*epupdpek - 16*pdpn*pnpe*c2*
     &    c3d*ml2*mw2m2*epupdpnk
      amp2 = amp2 - 16*pdpn*pnpe*c2*c3d*ml2*mw2m2*epupdpek - 16*
     &    pdpn*pnpe*c1d*c2*ml2*mw2m2*epupdpnk - 16*pdpn*pnpe*c1d*c2
     &    *ml2*mw2m2*epupdpek + 16*pdpn*pnpe*c1*c2d*ml2*mw2m2*
     &    epupdpnk + 16*pdpn*pnpe*c1*c2d*ml2*mw2m2*epupdpek + 16*
     &    pdpn*c3*c3d*puk*pek + 4*pdpn*c3*c3d*ml4*mw2dm1*puk + 4*
     &    pdpn*c3*c3d*ml4*mw2m1*puk - 24*pdpn*c3*c3d*ml2*puk - 4*
     &    pdpn*c3*c3d*ml2*mw2dm1*puk*pek - 8*pdpn*c3*c3d*ml2*mw2dm1*
     &    puk*pnk + 4*pdpn*c3*c3d*ml2*mw2dm1*pupe2 + 4*pdpn*c3*c3d*
     &    ml2*mw2dm1*epupdpnpe + 8*pdpn*c3*c3d*ml2*mw2dm1*epupdpek - 4
     &    *pdpn*c3*c3d*ml2*mw2m1*puk*pek - 8*pdpn*c3*c3d*ml2*mw2m1*
     &    puk*pnk + 4*pdpn*c3*c3d*ml2*mw2m1*pupe2 - 4*pdpn*c3*c3d*ml2
     &    *mw2m1*epupdpnpe - 8*pdpn*c3*c3d*ml2*mw2m1*epupdpek + 16*
     &    pdpn*c2d*c3*epupdpnpe + 32*pdpn*c2d*c3*epupdpek + 8*pdpn*
     &    c2d*c3*ml2*mw2dm1*epupdpnpe - 16*pdpn*c2d*c3*ml2*mw2dm1*
     &    epupdpnk - 16*pdpn*c2d*c3*ml2*mw2dm1*epupdpek - 16*pdpn*c2d
     &    *c3*ml2*mw2m1*epupdpnk
      amp2 = amp2 - 16*pdpn*c2*c3d*epupdpnpe - 32*pdpn*c2*c3d*
     &    epupdpek + 16*pdpn*c2*c3d*ml2*mw2dm1*epupdpnk - 8*pdpn*c2*
     &    c3d*ml2*mw2m1*epupdpnpe + 16*pdpn*c2*c3d*ml2*mw2m1*epupdpnk
     &     + 16*pdpn*c2*c3d*ml2*mw2m1*epupdpek - 16*pdpn*c1d*c3*pupe2
     &     + 16*pdpn*c1d*c3*epupnpek + 8*pdpn*c1d*c3*ml2*mw2dm1*pupe2
     &     + 8*pdpn*c1d*c3*ml2*mw2dm1*epupdpek + 8*pdpn*c1d*c3*ml2*
     &    mw2dm1*epupnpek - 8*pdpn*c1d*c3*ml2*mw2m1*epupdpek - 32*
     &    pdpn*c1d*c2*epupdpek + 16*pdpn*c1d*c2*ml2*mw2dm1*epupdpnk
     &     + 16*pdpn*c1d*c2*ml2*mw2m1*epupdpnk + 16*pdpn*c1d*c2*ml2*
     &    mw2m1*epupdpek - 16*pdpn*c1*c3d*pupe2 - 16*pdpn*c1*c3d*
     &    epupnpek + 8*pdpn*c1*c3d*ml2*mw2dm1*epupdpek + 8*pdpn*c1*
     &    c3d*ml2*mw2m1*pupe2 - 8*pdpn*c1*c3d*ml2*mw2m1*epupdpek - 8*
     &    pdpn*c1*c3d*ml2*mw2m1*epupnpek + 32*pdpn*c1*c2d*epupdpek - 
     &    16*pdpn*c1*c2d*ml2*mw2dm1*epupdpnk - 16*pdpn*c1*c2d*ml2*
     &    mw2dm1*epupdpek - 16*pdpn*c1*c2d*ml2*mw2m1*epupdpnk + 8*
     &    pdpe*pnpe*c3*c3d*ml2*mw2m2*puk*pek
      amp2 = amp2 + 8*pdpe*pnpe*c3*c3d*ml2*mw2m2*puk*pnk - 4*
     &    pdpe*pnpe*c3*c3d*ml2*mw2dm1*puk - 4*pdpe*pnpe*c3*c3d*ml2*
     &    mw2m1*puk + 16*pdpe*pnpe*c2d*c3*ml2*mw2m2*epupdpnk + 16*
     &    pdpe*pnpe*c2d*c3*ml2*mw2m2*epupdpek - 16*pdpe*pnpe*c2*c3d
     &    *ml2*mw2m2*epupdpnk - 16*pdpe*pnpe*c2*c3d*ml2*mw2m2*
     &    epupdpek - 16*pdpe*pnpe*c1d*c2*ml2*mw2m2*epupdpnk - 16*
     &    pdpe*pnpe*c1d*c2*ml2*mw2m2*epupdpek + 16*pdpe*pnpe*c1*c2d
     &    *ml2*mw2m2*epupdpnk + 16*pdpe*pnpe*c1*c2d*ml2*mw2m2*
     &    epupdpek - 16*pdpe*c3*c3d*pupn2 - 4*pdpe*c3*c3d*ml2*mw2dm1*
     &    puk*pnk - 4*pdpe*c3*c3d*ml2*mw2dm1*pupn2 + 4*pdpe*c3*c3d*
     &    ml2*mw2dm1*epupdpnpe - 8*pdpe*c3*c3d*ml2*mw2dm1*epupdpnk - 4
     &    *pdpe*c3*c3d*ml2*mw2m1*puk*pnk - 4*pdpe*c3*c3d*ml2*mw2m1*
     &    pupn2 - 4*pdpe*c3*c3d*ml2*mw2m1*epupdpnpe + 8*pdpe*c3*c3d*
     &    ml2*mw2m1*epupdpnk + 8*pdpe*c2d*c3*ml2*mw2dm1*epupdpnpe - 16
     &    *pdpe*c2d*c3*ml2*mw2m1*epupdpnk + 16*pdpe*c2*c3d*ml2*mw2dm1
     &    *epupdpnk
      amp2 = amp2 - 8*pdpe*c2*c3d*ml2*mw2m1*epupdpnpe - 8*pdpe*
     &    c1d*c3*ml2*mw2dm1*pupn2 - 8*pdpe*c1d*c3*ml2*mw2dm1*epupdpnk
     &     + 8*pdpe*c1d*c3*ml2*mw2dm1*epupnpek + 8*pdpe*c1d*c3*ml2*
     &    mw2m1*epupdpnk + 16*pdpe*c1d*c2*ml2*mw2dm1*epupdpnk - 8*
     &    pdpe*c1*c3d*ml2*mw2dm1*epupdpnk - 8*pdpe*c1*c3d*ml2*mw2m1*
     &    pupn2 + 8*pdpe*c1*c3d*ml2*mw2m1*epupdpnk - 8*pdpe*c1*c3d*
     &    ml2*mw2m1*epupnpek - 16*pdpe*c1*c2d*ml2*mw2m1*epupdpnk - 16*
     &    pnpe*c3*c3d*pupd2 - 8*pnpe*c3*c3d*ml4*mw2m2*pdk*puk + 8*
     &    pnpe*c3*c3d*ml4*mw2m2*pupd2 + 8*pnpe*c3*c3d*ml2*mw2dm1*pdk*
     &    puk - 8*pnpe*c3*c3d*ml2*mw2dm1*pupd2 - 4*pnpe*c3*c3d*ml2*
     &    mw2dm1*epupdpnk + 4*pnpe*c3*c3d*ml2*mw2dm1*epupdpek + 8*
     &    pnpe*c3*c3d*ml2*mw2m1*pdk*puk - 8*pnpe*c3*c3d*ml2*mw2m1*
     &    pupd2 + 4*pnpe*c3*c3d*ml2*mw2m1*epupdpnk - 4*pnpe*c3*c3d*
     &    ml2*mw2m1*epupdpek - 8*pnpe*c2d*c3*ml4*mw2m2*pupd2 + 16*
     &    pnpe*c2d*c3*ml2*mw2m2*pdpe2*puk + 16*pnpe*c2d*c3*ml2*mw2m2*
     &    pdpn2*puk
      amp2 = amp2 + 8*pnpe*c2d*c3*ml2*mw2dm1*pupd2 + 8*pnpe*c2d*
     &    c3*ml2*mw2m1*pupd2 - 8*pnpe*c2*c3d*ml4*mw2m2*pupd2 + 16*
     &    pnpe*c2*c3d*ml2*mw2m2*pdpe2*puk + 16*pnpe*c2*c3d*ml2*mw2m2*
     &    pdpn2*puk + 8*pnpe*c2*c3d*ml2*mw2dm1*pupd2 + 8*pnpe*c2*c3d*
     &    ml2*mw2m1*pupd2 + 8*pnpe*c1d*c3*ml4*mw2m2*pupd2 - 16*pnpe*
     &    c1d*c3*ml2*mw2m2*pupe2*pdk - 16*pnpe*c1d*c3*ml2*mw2m2*pupn2*
     &    pdk - 8*pnpe*c1d*c3*ml2*mw2dm1*pupd2 - 8*pnpe*c1d*c3*ml2*
     &    mw2m1*pupd2 - 16*pnpe*c1d*c2*ml4*mw2m2*pupd2 + 16*pnpe*c1d*
     &    c2*ml2*mw2dm1*pupd2 + 16*pnpe*c1d*c2*ml2*mw2m1*pupd2 + 8*
     &    pnpe*c1*c3d*ml4*mw2m2*pupd2 - 16*pnpe*c1*c3d*ml2*mw2m2*
     &    pupe2*pdk - 16*pnpe*c1*c3d*ml2*mw2m2*pupn2*pdk - 8*pnpe*c1*
     &    c3d*ml2*mw2dm1*pupd2 - 8*pnpe*c1*c3d*ml2*mw2m1*pupd2 - 16*
     &    pnpe*c1*c2d*ml4*mw2m2*pupd2 + 16*pnpe*c1*c2d*ml2*mw2dm1*
     &    pupd2 + 16*pnpe*c1*c2d*ml2*mw2m1*pupd2

      amp2 = amp2*
     +       g2*conjg(g2)*el2/4d0
     +       /4d0/3
     +       /(st_alpha/(2*pi))

      if(mod(abs(i),2).eq.0) then
         amp2=amp2*ph_CKM(abs(i)/2,(abs(j)+1)/2)**2
      elseif(mod(abs(i),2).eq.1) then   
         amp2=amp2*ph_CKM(abs(j)/2,(abs(i)+1)/2)**2
      endif

      return
      end 
*
      complex*16 function e_(q1,q2,q3,q4)
      implicit none
      real*8 q1(0:3),q2(0:3),q3(0:3),q4(0:3)

      complex*16 esign

      esign = -(0.d0,1.d0)

      e_ = q1(1)*q2(2)*q3(3)*q4(0) - q1(1)*q2(2)*q3(0)*q4(3) +
     .     q1(1)*q2(3)*q3(0)*q4(2) - q1(1)*q2(3)*q3(2)*q4(0) +
     .     q1(1)*q2(0)*q3(2)*q4(3) - q1(1)*q2(0)*q3(3)*q4(2) +
     .     q1(2)*q2(1)*q3(0)*q4(3) - q1(2)*q2(1)*q3(3)*q4(0) +
     .     q1(2)*q2(3)*q3(1)*q4(0) - q1(2)*q2(3)*q3(0)*q4(1) +
     .     q1(2)*q2(0)*q3(3)*q4(1) - q1(2)*q2(0)*q3(1)*q4(3) +
     .     q1(3)*q2(1)*q3(2)*q4(0) - q1(3)*q2(1)*q3(0)*q4(2) +
     .     q1(3)*q2(2)*q3(0)*q4(1) - q1(3)*q2(2)*q3(1)*q4(0) +
     .     q1(3)*q2(0)*q3(1)*q4(2) - q1(3)*q2(0)*q3(2)*q4(1) +
     .     q1(0)*q2(1)*q3(3)*q4(2) - q1(0)*q2(1)*q3(2)*q4(3) +
     .     q1(0)*q2(2)*q3(1)*q4(3) - q1(0)*q2(2)*q3(3)*q4(1) +
     .     q1(0)*q2(3)*q3(2)*q4(1) - q1(0)*q2(3)*q3(1)*q4(2)

      e_ = esign * e_ 

      return
      end function e_


