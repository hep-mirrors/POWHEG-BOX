      subroutine setborn(p,bflav,born,bornjk,bmunu)
      implicit none
      include 'pwhg_math.h'
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      integer nlegs
      parameter (nlegs=nlegborn)
      real * 8 p(0:3,nlegs),bornjk(nlegs,nlegs)
      integer bflav(nlegs)
      real * 8 bmunu(0:3,0:3,nlegs),born,colcf
      integer j,k,mu,nu


      !TM momenta from MadGraph
!      p(0:3,1)=(/0.500000000000d3,  0.000000000000d0,  
!     .         0.000000000000d0,  0.500000000000d3/)
!      p(0:3,2)=(/0.500000000000d3,  0.000000000000d0,  
!     .         0.000000000000d0, -0.500000000000d3/)
!      p(0:3,6)=(/0.885513330545d2, -0.221006902877d2,  
!     .         0.400803531917d2, -0.758054309569d2/)
!      p(0:3,5)=(/0.328329419227d3, -0.103849611883d3, 
!     .        -0.301933755390d3,  0.764949213872d2/)
!      p(0:3,4)=(/0.152358109467d3, -0.105880959667d3, 
!     .        -0.977096383270d2,  0.495483852268d2/)
!      p(0:3,3)=(/0.430761138251d3,  0.231831261838d3,  
!     .         0.359563040525d3, -0.502378756570d2/)
!
!      write(*,*)'got here into Born.f'
      call compborn(p,bflav,born,bmunu,bornjk)

      !TM momenta from MadGraph
!      p(0:3,1)=(/0.500000000000d3,  0.000000000000d0,  
!     .         0.000000000000d0,  0.500000000000d3/)
!      p(0:3,2)=(/0.500000000000d3,  0.000000000000d0,  
!     .         0.000000000000d0, -0.500000000000d3/)
!      p(0:3,4)=(/0.885513330545d2, -0.221006902877d2,  
!     .         0.400803531917d2, -0.758054309569d2/)
!      p(0:3,3)=(/0.328329419227d3, -0.103849611883d3, 
!     .        -0.301933755390d3,  0.764949213872d2/)
!      p(0:3,6)=(/0.152358109467d3, -0.105880959667d3, 
!     .        -0.977096383270d2,  0.495483852268d2/)
!      p(0:3,5)=(/0.430761138251d3,  0.231831261838d3,  
!     .         0.359563040525d3, -0.502378756570d2/)
!
!      write(*,*)'got here into Born.f'
!      call compborn(p,bflav,born,bmunu,bornjk)
!      stop 

      end


      subroutine compborn(pin,bflav,born,bmunu,bornjk)
      implicit none
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_math.h'
      integer nlegs
      parameter (nlegs=nlegborn)
      real * 8 pin(0:3,nlegborn)
      integer bflav(nlegborn)
      real * 8 born,bmunu(0:3,0:3,nlegs),bornjk(nlegs,nlegs)
      real *8 amp2,colcf
      integer ferm_type(nlegborn)
      integer i,j,k,mu,nu
      real * 8 ferm_charge(nlegborn)
      real * 8 p(12,1:4)  ! 12 = mxpart in MCFM
      double precision msq(-5:5,-5:5)
c     vector boson id and decay
      include 'cvecbos.h'

      do i=1,nlegborn
         p(i,4)   = pin(0,i) 
         p(i,1:3) = pin(1:3,i) 
      enddo
      p(1,:) = -p(1,:) 
      p(2,:) = -p(2,:) 

!      do j=1,6
!         write(*,*)'p in Born',p(j,:)
!      enddo

      call qqb_zz(p,msq)

      born = vsymfact*msq(bflav(1),bflav(2))

!      write(*,*)'BORN',born
!      stop

C     -- no gluons, so no spin correlated Born  
      do i=0,3
         do j=0,3
            bmunu(i,j,:)=0d0
         enddo
      enddo
      
      bornjk=0d0
C     -- here the bornjk is very simple B_i j = B C_i
      do j=1,nlegs
         if(abs(bflav(j)).le.6) then
            do k=j+1,nlegs
               if(abs(bflav(k)).le.6) then
                  colcf=cf
               endif
               bornjk(j,k)=born*colcf
               bornjk(k,j)=bornjk(j,k)
            enddo
         endif
      enddo
      


      end


      subroutine borncolour_lh
c Sets up the colour for the given flavour configuration
c already filled in the Les Houches interface.
c In case there are several colour structure, one
c should pick one with a probability proportional to
c the value of the corresponding cross section, for the
c kinematics defined in the Les Houches interface
      include 'LesHouches.h'
      include 'nlegborn.h'
      include 'pwhg_flst.h'
c     neutral particles
      icolup(1,3)=0
      icolup(2,3)=0
      icolup(1,4)=0
      icolup(2,4)=0
      icolup(1,5)=0
      icolup(2,5)=0
      icolup(1,6)=0
      icolup(2,6)=0
c     colored particles
      if((idup(1).gt.0).and.(idup(2).lt.0)) then
         icolup(1,1)=501
         icolup(2,1)=0
         icolup(1,2)=0
         icolup(2,2)=501
      elseif((idup(1).lt.0).and.(idup(2).gt.0)) then
         icolup(1,1)=0
         icolup(2,1)=501
         icolup(1,2)=501
         icolup(2,2)=0
      else
         write(*,*) ' invalid flavour'
         stop
      endif
      end


      subroutine finalize_lh
c     Set up the resonances whose mass must be preserved
c     on the Les Houches interface.
c     
c     lepton masses
      real *8 lepmass(3),decmass
      common/clepmass/lepmass,decmass
c     Resonance Z -> e-(3) e+(4)
      call add_resonance(23,3,4)
      call add_resonance(23,5,6)
c     The following routine also performs the reshuffling of momenta if
c     a massive decay is chosen
      end


c this subroutine compute the Born amplitude for the process
c q(p1) aq(p2) -> Z(p3+p4)   con Z -> l-(p3) l+(p4) 
c NUMERICALLY, with the bra/ket formalism, not by squaring the analytic 
c amplitude
c It gets the matrix p with all the momenta and gives   
c the amplitude squared (amp2) averaged over initial 
c polarization     
c
c         q  --->-----
c                     |
c                     |            l-
c                     |          /  
c         aq ---<-----/\/\/\/\/\/
c                       Z/gamma \
c                                \ l+
c     ferm_type = 1 fermion
c     ferm_type = -1 antifermion
c     fermion_charge = +2/3, -1/3, -2/3, +1/3

      subroutine q_aq_to_l_al(pphy,fermion_type,fermion_charge,
     #     amp2)
   
      implicit none
c the 5 4-momentum vectors
c p(i,1) is the i-th component of vector p1...   
      integer nleg
      parameter (nleg=4)
      integer fermion_type(nleg)
      real * 8 fermion_charge(nleg)
      real * 8 pphy(0:3,nleg)
      real * 8 amp2
      include 'PhysPars.h'
      include 'pwhg_math.h'      
      real * 8 p1(0:3),p2(0:3)
      real * 8 p34
      real * 8 dotp,tmp
      complex * 16 ccdotp
      complex*16 psi1(2,-1:1),psi2(2,-1:1),psi3(2,-1:1),psi4(2,-1:1)
      complex*16 jlep(0:3,-1:1),jqua(0:3,-1:1)
      complex*16 jlep_dot_jqua
      integer mu,i,hel_lep,hel_qua
      real * 8 p(0:3,nleg)
      real * 8 ferm_charge(nleg)
      integer ferm_type(nleg)
      integer utype_q,utype_l
      real * 8 q_q,v_q,a_q,q_l,v_l,a_l
      real*8 Zcoup_q(-1:1),Zcoup_l(-1:1)
      complex *16 prop34zeta,prop34gamma

      real * 8 teo_fact
      
      if ((fermion_type(3).ne.1).and.(fermion_type(4).ne.-1)) then
         write(*,*) 'ERROR: this subroutine deals only with Z decay'
         stop
      endif
     

c  copy of local variables
      do i=1,nleg
         do mu=0,3
            p(mu,i) = pphy(mu,i)
         enddo
         ferm_charge(i) = fermion_charge(i)
         ferm_type(i) = fermion_type(i)
      enddo
      
c     exchance particle 1 and 2
      if (ferm_type(1).eq.-1) then
         if (ferm_type(2).eq.1) then
            call exchange_momenta(p(0,1),p(0,2))
            tmp = ferm_charge(1)
            ferm_charge(1)=-ferm_charge(2)
            ferm_charge(2)=-tmp
            tmp = ferm_type(1)
            ferm_type(1)=ferm_type(2)
            ferm_type(2)=tmp
         else
            write(*,*) 'Error in the type of the quark 1-2'
            stop
         endif
      endif

c utype = +1 if this is an up-type quark (u,c,ubar,cbar)
c utype = -1 otherwise
      if (abs(ferm_charge(1)).eq.2d0/3) then
         utype_q = +1
         q_q = 2d0/3
      elseif (abs(ferm_charge(1)).eq.1d0/3) then
         utype_q = -1
         q_q = -1d0/3
      else
         write(*,*) 'What charge is this??', ferm_charge(1)
         stop
      endif

                  
      if (abs(ferm_charge(3)).eq.1d0) then
         utype_l = -1
         q_l = -1d0
      elseif (abs(ferm_charge(3)).eq.0d0) then
         utype_l = +1
         q_l = 0d0
      else
         write(*,*) 'What charge is this??',ferm_charge(4)
         stop
      endif
      
      v_q = utype_q*1.d0/2 - 2*q_q*ph_sthw2 
      a_q = utype_q*1.d0/2
      Zcoup_q(-1) = v_q + a_q
      Zcoup_q(+1) = v_q - a_q

      v_l = utype_l*1.d0/2 - 2*q_l*ph_sthw2 
      a_l = utype_l*1.d0/2     
      Zcoup_l(-1) = v_l + a_l
      Zcoup_l(+1) = v_l - a_l
      
c     fake momenta:
c     for bosons always outgoing
c     for fermions along fermionic current
      do mu=0,3
         p1(mu) = ferm_type(1)*p(mu,1)
         p2(mu) = ferm_type(2)*p(mu,2)
      enddo

      p34=dotp(p(0,3),p(0,4))
      
c     Z propagator (see eq 2.13 in Nason-Webber,Nucl.Phys B421 pp.473-517)

c     Here, simply use teo_fact=1
      teo_fact=1

      prop34zeta=teo_fact/dcmplx(2d0*p34-ph_Zmass2,ph_ZmZw)!*2d0*p34/ph_Zmass2)
c     teo_fact is the square root of the ratio between the experimental
c     width of the Z boson and the theoretical one.  This last is
c     evaluated by summing over species and flavours the quantity
c     (g_a)^2+(g_v)^2, multiplying then for the appropriate factor.
c     In this way one can safely push Z width to zero, avoiding
c     the cross section to diverge

c     photon propagator
      prop34gamma=1d0/(2d0*p34)
     
           
c     bra and ket are made with actual momenta 

c     q
      call ket(p(0,1),ferm_type(1),psi1)
c     aq
      call bra(p(0,2),ferm_type(2),psi2)
c     em
      call bra(p(0,3),ferm_type(3),psi3)
c     ep
      call ket(p(0,4),ferm_type(4),psi4)
      
c     leptonic currents
      call bra_gamma_ket(psi3,psi4,-1,jlep(0,-1))
      call bra_gamma_ket(psi3,psi4,+1,jlep(0,1))
      
c     quark currents
      call bra_gamma_ket(psi2,psi1,-1,jqua(0,-1))
      call bra_gamma_ket(psi2,psi1,+1,jqua(0,1))

      amp2=0d0

      do hel_lep=-1,1,2
             
         do hel_qua=-1,1,2
                                
            jlep_dot_jqua = 
     #           ccdotp(jlep(0,hel_lep),jqua(0,hel_qua))

c     to take account of the gamma/Z interference         
            
    
            jlep_dot_jqua = (
     #           q_q*q_l*prop34gamma +
     #           prop34zeta*    
     #           Zcoup_q(hel_qua)*Zcoup_l(hel_lep)/
     #           (2*ph_sthw*ph_cthw)**2 )
     #           * jlep_dot_jqua
            
            amp2 = amp2 + jlep_dot_jqua *
     #           DCONJG(jlep_dot_jqua)       
            
            
                 
         enddo      
      enddo
      
      
      amp2 = amp2*ph_unit_e**4 
c     1/4 from average over initial-state polarization
c     1/nc^2 * nc = 1/nc from average over initial-state colors and sum over 
c     quark colors
      amp2=  amp2/4d0/nc 
      
      end
