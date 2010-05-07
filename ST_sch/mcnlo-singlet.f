cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     !ER: beginning of interface functions to mcatnlo routines
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      function st_real_qq_reg(flav_real,krlab)
c     It returns the squared matrix element (summed and averaged)
c     for processes of type
c     qqbar -> tjj 
c     with the following assumptions:
c     -the massive leg is always the 3rd (with sq. mass calculated from the given momenta)
c     -the 3rd leg is always of u-type
c     -the 5th leg is a gluon
c     -|M|^2 = st_real_qq * (4*pi*as) * (4*pi*aem/sthw)^2
c                         * (CKM factor) 
      implicit none
      include 'stpcblks.h'
      real *8 st_real_qq_reg,krlab(0:3,5)
      integer flav_real(5)

c     (btilde - bbinit)
      integer chflag
      common/cchflag/chflag

      real *8 s,tk,uk,q1q,q2q,matout(8,2)
      real *8 dotp
      external dotp

      integer three_ch(-6:6)
      data three_ch /-2,1,-2,1,-2,1,0,-1,2,-1,2,-1,2/

      integer i,j

      logical verbose
      parameter (verbose=.false.)

      do i=1,8
         do j=1,2
            matout(i,j)=0d0
         enddo
      enddo


      if(three_ch(flav_real(5)).ne.0) then
         write(*,*) 'Error in st_real_qq_reg, flav_real(5) has '//
     #'to be 0'
         stop
      endif
      if(three_ch(flav_real(3)).ne.2) then
         write(*,*) 'Error in st_real_qq_reg, flav_real(3) has '//
     #'to be a u-type quark'
         stop
      endif

c The hard process is
c   a(p1)+b(p2) --> t(k1)+c(k2)+d(k)
c where a, b, c and d are light partons, t is top quark with k1^2=xm12.
c The quarks t and c are attached to the W-vertex, k2^2=xm22=0. The process 
c can be described by the same invariants as in FNR [eqs.(2.6) and (2.7)].
c
c In terms of the
c invariants, the dot products are 
c
c    p1.p2 = s/2
c    p1.k  = -tk/2
c    p2.k  = -uk/2
c    p1.k1 = -(q1q-xm12)/2
c    p2.k2 = -(q2q-xm22)/2
c    k1.k2 = (s2-xm12-xm22)/2
c    p2.k1 = -(q2c-xm12)/2
c    p1.k2 = -(q1c-xm22)/2
c    k.k1  = (w1-xm12)/2
c    k.k2  = (w2-xm22)/2

      s=   2d0*dotp(krlab(0,1),krlab(0,2))
      tk=  -2d0*dotp(krlab(0,1),krlab(0,5))
      uk=  -2d0*dotp(krlab(0,2),krlab(0,5))
      q1q= -2d0*dotp(krlab(0,1),krlab(0,3)) + xm12
      q2q= -2d0*dotp(krlab(0,2),krlab(0,4)) + xm22

      st_real_qq_reg=0d0
     
      if(chflag.eq.1) then
         
c     udx -> tdxg (s-channel)
         if ((three_ch(flav_real(1)).eq.2).and.
     #(three_ch(flav_real(2)).eq.1)) then
            call stmatr_fast_qq(s,tk,uk,q1q,q2q,matout,1,1)
            st_real_qq_reg=matout(1,1)
c     dxu -> tdxg (s-channel)
         elseif ((three_ch(flav_real(1)).eq.1).and.
     #(three_ch(flav_real(2)).eq.2)) then
            call stmatr_fast_qq(s,tk,uk,q1q,q2q,matout,3,1)
            st_real_qq_reg=matout(3,1)
         else
            write(*,*) 'Error in st_real_qq_reg, wrong flav_real'
            stop
         endif
            
      elseif(chflag.eq.2) then
         
c     ddx -> tuxg (t-channel)
         if ((three_ch(flav_real(1)).eq.-1).and.
     #(three_ch(flav_real(2)).eq.1)) then
            call stmatr_fast_qq(s,tk,uk,q1q,q2q,matout,2,2)
            st_real_qq_reg=matout(2,2)
c     dxd -> tuxg (t-channel) 
         elseif ((three_ch(flav_real(1)).eq.1).and.
     #(three_ch(flav_real(2)).eq.-1)) then
            call stmatr_fast_qq(s,tk,uk,q1q,q2q,matout,4,2)
            st_real_qq_reg=matout(4,2)
c     ud -> tdg (t-channel)
         elseif ((three_ch(flav_real(1)).eq.2).and.
     #(three_ch(flav_real(2)).eq.-1)) then
            call stmatr_fast_qq(s,tk,uk,q1q,q2q,matout,3,2)
            st_real_qq_reg=matout(3,2)
c     du -> tdg (t-channel)
         elseif ((three_ch(flav_real(1)).eq.-1).and.
     #(three_ch(flav_real(2)).eq.2)) then
            call stmatr_fast_qq(s,tk,uk,q1q,q2q,matout,1,2)
            st_real_qq_reg=matout(1,2)
         else
            write(*,*) 'Error in st_real_qq_reg, wrong flav_real'
            stop
         endif             
      else
         write(*,*) 'Error in st_real_qq_reg, wrong chflag'
         stop
      endif


      if(st_real_qq_reg.lt.0d0) then
         call increasecnt('wrong value in real matrix element')
         if(verbose) then
            write(*,*) 'Error in st_real_qq_reg, negative result: ',
     #st_real_qq_reg
            write(*,*) 'flavours: ',flav_real
            write(*,*) 's,q1q ',s,q1q
            write(*,*) 'tk,uk,q2q ',tk,uk,q2q
            call check_kinematics(krlab,5)
         endif
         st_real_qq_reg=0d0
      endif
      
      end


      function st_real_qg_reg(flav_real,krlab)
c     It returns the squared matrix element (summed and averaged)
c     for processes of type
c     qg -> tqq 
c     gq -> tqq
c     with the following assumptions:
c     -the massive leg is always the 3rd (with sq. mass calculated from the given momenta)
c     -the 3rd leg is always of u-type
c     -one (and only one) incoming leg is a gluon
c     -outgoing light legs (qq) are sorted with increasing flavour (ux,dx,d,u) 
c     -|M|^2 = st_real_qg * (4*pi*as) * (4*pi*aem/sthw)^2
c                         * (CKM factor) 
      implicit none
      include 'stpcblks.h'
      real *8 st_real_qg_reg,krlab(0:3,5)
      integer flav_real(5)

c     (btilde - bbinit)
      integer chflag
      common/cchflag/chflag

      logical cond1,cond2,cond3
      
      real *8 s,tk_s,tk_t,uk_s,uk_t,q1q,q2q_s,q2q_t,
     #matout_t(8,2),matout_s(8,2)
      real *8 dotp
      external dotp

      integer three_ch(-6:6)
      data three_ch /-2,1,-2,1,-2,1,0,-1,2,-1,2,-1,2/

      logical verbose
      parameter (verbose=.false.)

      integer i,j

      do i=1,8
         do j=1,2
            matout_t(i,j)=0d0
            matout_s(i,j)=0d0
         enddo
      enddo
            

      cond1=three_ch(flav_real(1)).eq.0 !leg1 is a gluon
      cond2=three_ch(flav_real(2)).eq.0 !leg2 is a gluon
      cond3=cond1.and.cond2     !both leg1 and leg2 are gluons
      if((cond1.or.cond2).and.(.not.cond3)) then
         continue
      else
         write(*,*) 'Error in st_real_qg_reg, wrong flav_real '//
     #'for initial state'         
         stop
      endif
      if(three_ch(flav_real(3)).ne.2) then
         write(*,*) 'Error in st_real_qg_reg, flav_real(3) has '//
     #        'to be a u-type quark'
         stop
      endif

c The hard process is
c   a(p1)+b(p2) --> t(k1)+c(k2)+d(k)
c where a, b, c and d are light partons, t is top quark with k1^2=xm12.
c The quarks t and c are attached to the W-vertex, k2^2=xm22=0. The process 
c can be described by the same invariants as in FNR [eqs.(2.6) and (2.7)].
c
c In terms of the
c invariants, the dot products are 
c
c    p1.p2 = s/2
c    p1.k  = -tk/2
c    p2.k  = -uk/2
c    p1.k1 = -(q1q-xm12)/2
c    p2.k2 = -(q2q-xm22)/2
c    k1.k2 = (s2-xm12-xm22)/2
c    p2.k1 = -(q2c-xm12)/2
c    p1.k2 = -(q1c-xm22)/2
c    k.k1  = (w1-xm12)/2
c    k.k2  = (w2-xm22)/2

      s=2d0*dotp(krlab(0,1),krlab(0,2))
      q1q=-2d0*dotp(krlab(0,1),krlab(0,3)) + xm12
c     t-channel invariants in qg subprocesses
      tk_t=-2d0*dotp(krlab(0,1),krlab(0,5))
      uk_t=-2d0*dotp(krlab(0,2),krlab(0,5))
      q2q_t=-2d0*dotp(krlab(0,2),krlab(0,4)) + xm22
         
c     s-channel invariants in qg subprocesses
      tk_s=-2d0*dotp(krlab(0,1),krlab(0,4))
      uk_s=-2d0*dotp(krlab(0,2),krlab(0,4))
      q2q_s=-2d0*dotp(krlab(0,2),krlab(0,5)) + xm22

      st_real_qg_reg=0d0
         
      if(cond1) then
c     incoming gluon, leg 1
         if(chflag.eq.1) then

c     gdx -> tuxdx (s-channel)
            if ((three_ch(flav_real(1)).eq.0).and.
     #(three_ch(flav_real(2)).eq.1)) then
               call stmatr_fast_qg(1,s,tk_s,uk_s,q1q,q2q_s,matout_s,4,1)
               st_real_qg_reg=matout_s(4,1)
c     gu -> tddx (s-channel)
            elseif ((three_ch(flav_real(1)).eq.0).and.
     #(three_ch(flav_real(2)).eq.2)) then
               call stmatr_fast_qg(1,s,tk_s,uk_s,q1q,q2q_s,matout_s,3,1)
               st_real_qg_reg=matout_s(3,1)            
            else
               write(*,*) 'Error in st_real_qg_reg, wrong flav_real (1)'
               stop
            endif     

         elseif(chflag.eq.2) then
       
c     gdx -> tuxdx (t-channel)
            if ((three_ch(flav_real(1)).eq.0).and.
     #(three_ch(flav_real(2)).eq.1)) then
               call stmatr_fast_qg(1,s,tk_t,uk_t,q1q,q2q_t,matout_t,4,2)
               st_real_qg_reg=matout_t(4,2)
c     gu -> tddx (t-channel)
            elseif ((three_ch(flav_real(1)).eq.0).and.
     #(three_ch(flav_real(2)).eq.2)) then
               call stmatr_fast_qg(1,s,tk_t,uk_t,q1q,q2q_t,matout_t,2,2)
               st_real_qg_reg=matout_t(2,2)
c     gd -> tuxd (t-channel only)
            elseif ((three_ch(flav_real(1)).eq.0).and.
     #(three_ch(flav_real(2)).eq.-1)) then
               call stmatr_fast_qg(1,s,tk_t,uk_t,q1q,q2q_t,matout_t,8,2)
               st_real_qg_reg=matout_t(8,2)
            else
               write(*,*) 'Error in st_real_qg_reg, wrong flav_real (1)'
               stop
            endif                              
            
         else
            write(*,*) 'Error in st_real_qq_reg, wrong chflag'
            stop
         endif
            

      elseif(cond2) then
c     incoming gluon, leg 2
         if(chflag.eq.1) then

c     dxg -> tuxdx (s-channel) 
            if ((three_ch(flav_real(1)).eq.1).and.
     #(three_ch(flav_real(2)).eq.0)) then
               call stmatr_fast_qg(2,s,tk_s,uk_s,q1q,q2q_s,matout_s,2,1)
               st_real_qg_reg=matout_s(2,1)
c     ug -> tddx (s-channel)
            elseif ((three_ch(flav_real(1)).eq.2).and.
     #(three_ch(flav_real(2)).eq.0)) then
               call stmatr_fast_qg(2,s,tk_s,uk_s,q1q,q2q_s,matout_s,1,1)
               st_real_qg_reg=matout_s(1,1)
            else
               write(*,*) 'Error in st_real_qg_reg, wrong flav_real (2)'
               stop
            endif                              

         elseif(chflag.eq.2) then

c     dxg -> tuxdx (t-channel) 
            if ((three_ch(flav_real(1)).eq.1).and.
     #(three_ch(flav_real(2)).eq.0)) then
               call stmatr_fast_qg(2,s,tk_t,uk_t,q1q,q2q_t,matout_t,7,2)
               st_real_qg_reg=matout_t(7,2)
c     ug -> tddx (t-channel)
            elseif ((three_ch(flav_real(1)).eq.2).and.
     #(three_ch(flav_real(2)).eq.0)) then
               call stmatr_fast_qg(2,s,tk_t,uk_t,q1q,q2q_t,matout_t,5,2)
               st_real_qg_reg=matout_t(5,2)
c     dg -> tuxd (t-channel only)
            elseif ((three_ch(flav_real(1)).eq.-1).and.
     #(three_ch(flav_real(2)).eq.0)) then
               call stmatr_fast_qg(2,s,tk_t,uk_t,q1q,q2q_t,matout_t,3,2)
               st_real_qg_reg=matout_t(3,2)
            else
               write(*,*) 'Error in st_real_qg_reg, wrong flav_real (2)'
               stop
            endif                              

         else
            write(*,*) 'Error in st_real_qq_reg, wrong chflag'
            stop
         endif

      endif

c     !ER: WARNING:
c     matout_t(1,2) and matout_t(6,2) correspond respectively to 
c     subprocesses
c     (1,2) - dg -> tdux 
c     (6,2) - gd -> tdux 
c     These are not used because the program integrates the corresponding ones
c     with final state tuxd, with a proper momentum assignment and a proper
c     S function. For this last reason, the damping factor that was present
c     in the original version of mcnlo subroutines has been removed (see comments
c     in the subroutines for further details)

      if(st_real_qg_reg.lt.0d0) then
         call increasecnt('wrong value in real matrix element')
         if(verbose) then
            write(*,*) 'Error in st_real_qg_reg, negative result: ',
     #st_real_qg_reg
            write(*,*) 'flavours: ',flav_real
            write(*,*) 's,q1q ',s,q1q
            write(*,*) 'tk_t,uk_t,q2q_t ',tk_t,uk_t,q2q_t
            write(*,*) 'tk_s,uk_s,q2q_s ',tk_s,uk_s,q2q_s
            call check_kinematics(krlab,5)
         endif
         st_real_qg_reg=0d0
      endif
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     !ER: end of interface functions to mcatnlo routines      
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc      

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     !ER: beginning of mcatnlo routines (taken from version 3.3)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      subroutine fborn(s,t,jproc,xmatout)
c     !ER: the only relevant modification is that here the flux factor
c          has been removed. For other minor changes, see comments.
c     Returns the partonic Born contribution, with the following normalization
c     dsigma_born = g_W^4 |V_ij|^2 |V_tk|^2 fborn dphi_2 /(2s)
c     with g_W^4 and CKM factors inserted in the main codes. See freal
c     for the conventions used for the array xmatout
c
c With p1 denoting the momentum of the parton coming from the left
c (= +z-component) and k1 the momentum of the top quark, we define
c t = (p1-k1)^2 = (p2-k2)^2
c u = (p1-k2)^2 = (p2-k1)^2
c Note that for the (jproc=2) qqbar(i=1) and qbarq (i=3)
c channels, p1 labels the quark and antiquark respectively.
c
c s = (p1+p2)^2 and is therefore unambiguous
c
      implicit none
      include 'stpcblks.h'
      real*8 s,t,u,Wprop_s,Wprop_t,Wprop_u,xmatout(8,2),pi
      integer jproc,i,j,idrmax(2:3,2)
      common/cidrmax/idrmax
      parameter (pi=3.14159265358979312D0)
c
      if(jproc.ne.2.and.jproc.ne.3)then
        write(*,*)'Error in fborn: jproc=',jproc
        stop
      endif
c
cccccccccccccccccccccccccccccccccccccccccccccccc
c     !ER: added to set to 0 everything, and to have correct idrmax
c     as in the original code
c$$$      do j=1,2
c$$$        do i=1,idrmax(jproc,j)
c$$$          xmatout(i,j) = 0d0
c$$$        enddo
c$$$      enddo
      do j=1,2
         do i=1,8
            xmatout(i,j) = 0d0
        enddo
      enddo
      idrmax(2,1)=4
      idrmax(2,2)=4
ccccccccccccccccccccccccccccccccccccccccccccccc

      if(jproc.eq.2)then
        u = xm12+xm22-s-t
        Wprop_s = s-xmW2
        Wprop_t = t-xmW2
        Wprop_u = u-xmW2
cccccccccccccccccccccccccccccccccccccccccccccc
c     !ER: removed flux factor 1/(2*s)
c$$$c s-channel
c$$$        xmatout(1,1) = ( (u-xm12)*(u-xm22)/4d0/(Wprop_s**2) )/(2*s)
c$$$        xmatout(3,1) = ( (t-xm12)*(t-xm22)/4d0/(Wprop_s**2) )/(2*s)
c$$$c t-channel: 1 and 2 have the same CKM weights as 3 and 4 respectively
c$$$        xmatout(1,2) = ( s*(s-xm12-xm22)/4d0/(Wprop_t**2) )/(2*s)
c$$$        xmatout(2,2) = ( u*(u-xm12-xm22)/4d0/(Wprop_t**2) )/(2*s)
c$$$        xmatout(3,2) = ( s*(s-xm12-xm22)/4d0/(Wprop_u**2) )/(2*s)
c$$$        xmatout(4,2) = ( t*(t-xm12-xm22)/4d0/(Wprop_u**2) )/(2*s)
c s-channel
        xmatout(1,1) = ( (u-xm12)*(u-xm22)/4d0/(Wprop_s**2) )
        xmatout(3,1) = ( (t-xm12)*(t-xm22)/4d0/(Wprop_s**2) )
c t-channel: 1 and 2 have the same CKM weights as 3 and 4 respectively
        xmatout(1,2) = ( s*(s-xm12-xm22)/4d0/(Wprop_t**2) )
        xmatout(2,2) = ( u*(u-xm12-xm22)/4d0/(Wprop_t**2) )
        xmatout(3,2) = ( s*(s-xm12-xm22)/4d0/(Wprop_u**2) )
        xmatout(4,2) = ( t*(t-xm12-xm22)/4d0/(Wprop_u**2) )
      endif
c
      return
      end


      subroutine f2sv(xs,xt,jproc,xmatout)
c     !ER: the only relevant modification is that here the flux factor
c          has been removed and the definition of xmatout is changed
c          accordingly. 
c     Returns sig_2pv of FKS. The normalization is such that
c     dsigma^{(sv)} = g_W^4 as/(2*pi) |V_ij|^2 |V_tk|^2 f2sv dphi_2  /(2s)
      implicit none
      real * 8 xs,xt,u,xmatout(1:8,1:2)
      include 'stpcblks.h'
      real * 8 tiny,pi,zeta2,s,t,vcf,xicut,deltai,deltao,svsbfac2,
     # svsnonfac,svtbfac2,svtnonfac,xmatsb,xmatin(8,2),xmattb(1:4),
     # xmatsnonfac(1:4),xmattnonfac(1:4)
      integer i,j,jproc,itwo,idrmax(2:3,2)
      common/cidrmax/idrmax
      common/parsub/xicut,deltai,deltao
      parameter (tiny=1.d-8)
      parameter (pi=3.14159265358979312D0)
      parameter (zeta2=3.14159265358979312D0**2/6d0)
      parameter (vcf=4.d0/3.d0)
      parameter (itwo=2)


cccccccccccccccccccccccccccccccccccccccccccccccc
c     !ER: added to set to 0 everything, and to have correct idrmax
c     as in the original code
c$$$      do j=1,2
c$$$        do i=1,idrmax(jproc,j)
c$$$          xmatout(i,j) = 0d0
c$$$        enddo
c$$$      enddo
      do j=1,2
         do i=1,8
            xmatout(i,j) = 0d0
        enddo
      enddo
      idrmax(2,1)=4
      idrmax(2,2)=4
ccccccccccccccccccccccccccccccccccccccccccccccc


c
      if(jproc.eq.2)then
        s=xs
        t=xt
        u=xm12+xm22-s-t
        if(abs(xmuf2h1-xmuf2h2).gt.tiny .or.
     #     abs(xmuf2h1-xmur2).gt.tiny)then
          write(*,*)'Error in f2sv: no such scale choice'
          stop
        endif
        call fborn(s,t,itwo,xmatin)
c     !ER: now this xmatin does NOT include the flux factor.
c          For this reason, in the following, xmatsnonfac and xmattnonfac are
c          no more divided by the flux factor.

c s-channel: one term is proportional to the Born (svsbfac2), one
c term has another dependence on s and t (svsnonfac)
        xmatsb = svsbfac2(s)
        xmatsnonfac(1) = svsnonfac(s,t,u)
        xmatsnonfac(2) = 0.
        xmatsnonfac(3) = svsnonfac(s,u,t)
        xmatsnonfac(4) = 0.
        do i=1,idrmax(jproc,1)
          xmatout(i,1)=vcf*( xmatsb*xmatin(i,1) + xmatsnonfac(i) )
        enddo
c t-channel: one term is proportional to the Born (svtbfac2), one
c term has another dependence on s and t (svtnonfac)
        xmattb(1) = svtbfac2(s,t)
        xmattb(2) = svtbfac2(s,t)
        xmattb(3) = svtbfac2(s,u)
        xmattb(4) = svtbfac2(s,u)
        xmattnonfac(1) = -svtnonfac(s,t,u)
        xmattnonfac(2) = -svtnonfac(u,t,s)
        xmattnonfac(3) = -svtnonfac(s,u,t)
        xmattnonfac(4) = -svtnonfac(t,u,s)
        do i=1,idrmax(jproc,2)
          xmatout(i,2)=vcf*( xmattb(i)*xmatin(i,2) + xmattnonfac(i) )
        enddo
      else
        do j=1,2
          do i=1,idrmax(jproc,j)
            xmatout(i,j)=0.
          enddo
        enddo
      endif

      return
      end


      function svsbfac2(s)
c The proportional to Born in the FKS finite part, s-channel
      implicit none
      include 'stpcblks.h'
      real*8 svsbfac2,s
      real*8 xicut,deltai,deltao
      common/parsub/xicut,deltai,deltao
      real*8 m2,mu2,xlnxic,xlndoo2,xlnmu2om2,xlnm2os,xlnsomu2,
     #  xln1mm2os,xlnsom2m1,tmp1,tmp2,tmp3,pi,pi2,ddilog
      parameter (pi=3.14159265358979312D0)
      parameter (pi2=pi**2)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc
c     !ER: set xicut=1 and deltai=deltao=2, as suggested in FNO paper
      xicut=1d0
      deltai=2d0
      deltao=2d0
cccccccccccccccccccccccccccccccccccccccccccccccccccc

      m2=xm12
      mu2=xmuf2h1
      xlnxic=log(xicut)
      xlndoo2=log(deltao/2.d0)
      xlnmu2om2=log(mu2/m2)
      xlnm2os=log(m2/s)
      xlnsomu2=log(s/mu2)
      xln1mm2os=log(1-m2/s)
      xlnsom2m1=log(s/m2-1)
c
      tmp1=(-1-xlndoo2+xlnmu2om2+3*xlnsomu2)*xlnxic+2*xlnxic**2
      tmp2=(-3/4.d0+xln1mm2os)*xlndoo2
      tmp3=-15/4.d0+pi2/3.d0+ddilog(s/(s-m2))+ddilog(1-s/m2)-
     #   1/2.d0*xlnsom2m1**2+1/4.d0*xlnsom2m1*(6-2*m2/s+4*xlnmu2om2)+
     #   xln1mm2os**2-1/2.d0*xln1mm2os*(3+2*xlnmu2om2+2*xlnm2os)+
     #   xlnm2os**2+xlnm2os*((m2+3*s)/(4*(m2-s))+xlnmu2om2)-
     #   3/2.d0*xlnmu2om2
      svsbfac2=2*(tmp1+tmp2+tmp3)
      return
      end


      function svtbfac2(s,t)
c The proportional to Born in the FKS finite part, t-channel
      implicit none
      include 'stpcblks.h'
      real*8 svtbfac2,s,t
      real*8 xicut,deltai,deltao
      common/parsub/xicut,deltai,deltao
      real*8 m2,mu2,xlnxic,xlndoo2,xlnmu2om2,xlnm2os,xlnsomu2,
     #  xln1mm2os,xlnsom2m1,xlnmtom2,xlnm2mtom2s,xlnmtosmm2,
     #  xln1ptosmm2,xln1mtom2,tmp1,tmp2,tmp3,pi,pi2,ddilog
      parameter (pi=3.14159265358979312D0)
      parameter (pi2=pi**2)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccc
c     !ER: set xicut=1 and deltai=deltao=2, as suggested in FNO paper
      xicut=1d0
      deltai=2d0
      deltao=2d0
cccccccccccccccccccccccccccccccccccccccccccccccccccc

      m2=xm12
      mu2=xmuf2h1
      xlnxic=log(xicut)
      xlndoo2=log(deltao/2.d0)
      xlnmu2om2=log(mu2/m2)
      xlnm2os=log(m2/s)
      xlnsomu2=log(s/mu2)
      xln1mm2os=log(1-m2/s)
      xlnsom2m1=log(s/m2-1)
      xlnmtom2=log(-t/m2)
      xlnm2mtom2s=log((m2-t)/sqrt(m2*s))
      xlnmtosmm2=log(-t/(s-m2))
      xln1ptosmm2=log(1+t/(s-m2))
      xln1mtom2=log(1-t/m2)
c
      tmp1=2*(2*xlnsomu2+2*xlnm2mtom2s+2*xlnmtosmm2-xlndoo2-1)*xlnxic+
     #     4*xlnxic**2
      tmp2=(2*xln1mm2os-3/2.d0)*xlndoo2
      tmp3=-15/2.d0-pi2+2*ddilog(t/m2)-2*ddilog(-t/(s-m2))+
     #     2*ddilog(t/(t-m2))-2*ddilog(-(s+t-m2)/(m2-t))+
     #     2*xln1mm2os**2-xln1mm2os*(3+2*xlnmu2om2+2*xlnm2os)+
     #     xlnmtosmm2**2-2*xlnmtosmm2*(xlnmu2om2+xlnm2os+xln1ptosmm2)+
     #     xln1mtom2*(3-m2/t+2*xlnmu2om2)-
     #     2*xlnm2mtom2s*(xlnmu2om2+xlnm2os)-
     #     xlnmtom2**2+xlnmtom2*(3+2*xlnmu2om2)+
     #     xlnm2os**2+xlnm2os*(3*xlnmu2om2+(3*s-7*m2)/(2*(s-m2)))-
     #     3*xlnmu2om2
      svtbfac2=tmp1+tmp2+tmp3
      return
      end


      function svsnonfac(s,t,u)
c The not proportional to Born in the FKS finite part, s-channel
      implicit none
      include 'stpcblks.h'
      real*8 svsnonfac,s,t,u,s1,m2,lns1m2
c
      m2 = xm12
      s1 = s-m2
      u = m2-s-t
      lns1m2 = log(s1/m2)
      svsnonfac = (s-xmW2)**(-2)*( 16*s**(-1)*t*u*m2*lns1m2 )/64d0
c
      return
      end



      function svtnonfac(s,t,u)
c The not proportional to Born in the FKS finite part, t-channel
      implicit none
      include 'stpcblks.h'
      real*8 svtnonfac,s,t,u,t1,m2,lnt1m2
c
      m2 = xm12
      t1 = t-m2
      lnt1m2 = log(-t1/m2)
      svtnonfac = (t-xmW2)**(-2)* ( 16*s*t**(-1)*u*m2*lnt1m2 )/64d0
c
      return
      end


c$$$      subroutine stmatr_qq(s,tk,uk,q1q,q2q,xmatout)
c$$$c Real matrix element squared, without damping and flux factors; qq processes
c$$$      implicit none
c$$$      include '../include/stpcblks.h'
c$$$      real*8 s,tk,uk,q1q,q2q,xmatout(8,2),q1c,q2c,w1,w2,s2,
c$$$     # t12,t13,t14,t15,t23,t24,t25,t34,t35,t45,pi,vcf,
c$$$     # Prop1_1,Prop2_1,Prop1_2,Prop2_2,Prop1_3,Prop2_3,Prop1_4,Prop2_4,
c$$$     # MApMB2,MCpMD2,
c$$$     # rMApMB2_s1,rMCpMD2_s1,rMApMB2_s3,rMCpMD2_s3,
c$$$     # rMApMB2_t1,rMCpMD2_t1,rMApMB2_t2,rMCpMD2_t2,
c$$$     # rMApMB2_t3,rMCpMD2_t3,rMApMB2_t4,rMCpMD2_t4
c$$$      parameter (pi=3.14159265358979312D0)
c$$$      parameter (vcf=4.d0/3.d0)
c$$$      integer i,j,idrmax(2:3,2)
c$$$      common/cidrmax/idrmax
c$$$c
c$$$      do j=1,2
c$$$        do i=1,idrmax(2,j)
c$$$          xmatout(i,j) = 0d0
c$$$        enddo
c$$$      enddo
c$$$c
c$$$      s2 = s+tk+uk
c$$$      q1c = xm12 + xm22 - s - tk - q1q
c$$$      q2c = xm12 + xm22 - s - uk - q2q
c$$$      w1  = xm12 - q1q + q2q - tk
c$$$      w2  = xm22 - q2q + q1q - uk
c$$$c
c$$$c Map FNR invariants into Eric's sij's, according to the following rules
c$$$c  p1->-q1;  p2->-q2;  k1->q3;  k2->q4; k->q5
c$$$c This is just a renaming scheme for FNR invariants, and it is not
c$$$c associated with any specific short-distance process. In this way,
c$$$c 1 and 2 in sij are always associated with initial-state partons,
c$$$c 3 is the top, 4 is the (anti)bottom, 5 is the gluon.
c$$$c Note the minus signs in front of q1 and q2: all q's momenta are in the
c$$$c final state, and sij are therefore in a non-physical configuration.
c$$$c Since the matrix element routines MApMB2 and MCpMD2 assume the
c$$$c invariants to be in a non-physical configuration, no further
c$$$c explicit crossing (i.e. adding minus signs) is required. The
c$$$c crossing is implicit in the definitions of the sij's above.
c$$$c
c$$$      t12 = s
c$$$      t13 = q1q-xm12
c$$$      t14 = q1c-xm22
c$$$      t15 = tk
c$$$      t23 = q2c-xm12
c$$$      t24 = q2q-xm22
c$$$      t25 = uk
c$$$      t34 = s2-xm12-xm22
c$$$      t35 = w1-xm12
c$$$      t45 = w2-xm22
c$$$c
c$$$c s-channel
c$$$c
c$$$      Prop1_1 = 1d0/(t12+t15+t25-xmW2)      
c$$$      Prop2_1 = 1d0/(t12-xmW2)      
c$$$      rMApMB2_s1 = MApMB2(xm12,t12,t13,t14,t15,
c$$$     #                    t23,t24,t25,t34,t35,t45)
c$$$      rMCpMD2_s1 = MCpMD2(xm12,t12,t13,t14,t15,
c$$$     #                    t23,t24,t25,t34,t35,t45)
c$$$c
c$$$      Prop1_3 = Prop1_1
c$$$      Prop2_3 = Prop2_1
c$$$      rMApMB2_s3 = MApMB2(xm12,t12,t23,t24,t25,
c$$$     #                    t13,t14,t15,t34,t35,t45)
c$$$      rMCpMD2_s3 = MCpMD2(xm12,t12,t23,t24,t25,
c$$$     #                    t13,t14,t15,t34,t35,t45)
c$$$c
c$$$      xmatout(1,1) = vcf*( Prop1_1**2*rMApMB2_s1 + 
c$$$     #                     Prop2_1**2*rMCpMD2_s1 )/4.d0
c$$$      xmatout(3,1) = vcf*( Prop1_3**2*rMApMB2_s3 + 
c$$$     #                     Prop2_3**2*rMCpMD2_s3 )/4.d0
c$$$c
c$$$c t-channel
c$$$c
c$$$      Prop1_1 = 1d0/(t24+t25+t45-xmW2)      
c$$$      Prop2_1 = 1d0/(t24-xmW2)      
c$$$      rMApMB2_t1 = MApMB2(xm12,t24,t23,t12,t25,
c$$$     #                    t34,t14,t45,t13,t35,t15)
c$$$      rMCpMD2_t1 = MCpMD2(xm12,t24,t23,t12,t25,
c$$$     #                    t34,t14,t45,t13,t35,t15)
c$$$c
c$$$      Prop1_2 = 1d0/(t24+t45+t25-xmW2)      
c$$$      Prop2_2 = 1d0/(t24-xmW2)      
c$$$      rMApMB2_t2 = MApMB2(xm12,t24,t34,t14,t45,
c$$$     #                    t23,t12,t25,t13,t35,t15)
c$$$      rMCpMD2_t2 = MCpMD2(xm12,t24,t34,t14,t45,
c$$$     #                    t23,t12,t25,t13,t35,t15)
c$$$c
c$$$      Prop1_3 = 1d0/(t14+t15+t45-xmW2)      
c$$$      Prop2_3 = 1d0/(t14-xmW2)      
c$$$      rMApMB2_t3 = MApMB2(xm12,t14,t13,t12,t15,
c$$$     #                    t34,t24,t45,t23,t35,t25)
c$$$      rMCpMD2_t3 = MCpMD2(xm12,t14,t13,t12,t15,
c$$$     #                    t34,t24,t45,t23,t35,t25)
c$$$c
c$$$      Prop1_4 = 1d0/(t14+t45+t15-xmW2)      
c$$$      Prop2_4 = 1d0/(t14-xmW2)      
c$$$      rMApMB2_t4 = MApMB2(xm12,t14,t34,t24,t45,
c$$$     #                    t13,t12,t15,t23,t35,t25)
c$$$      rMCpMD2_t4 = MCpMD2(xm12,t14,t34,t24,t45,
c$$$     #                    t13,t12,t15,t23,t35,t25)
c$$$c
c$$$      xmatout(1,2) = vcf*( Prop1_1**2*rMApMB2_t1 + 
c$$$     #                     Prop2_1**2*rMCpMD2_t1 )/4.d0
c$$$      xmatout(2,2) = vcf*( Prop1_2**2*rMApMB2_t2 + 
c$$$     #                     Prop2_2**2*rMCpMD2_t2 )/4.d0
c$$$      xmatout(3,2) = vcf*( Prop1_3**2*rMApMB2_t3 + 
c$$$     #                     Prop2_3**2*rMCpMD2_t3 )/4.d0
c$$$      xmatout(4,2) = vcf*( Prop1_4**2*rMApMB2_t4 + 
c$$$     #                     Prop2_4**2*rMCpMD2_t4 )/4.d0
c$$$c
c$$$      return
c$$$      end
c$$$
c$$$
c$$$      subroutine stmatr_qg(gleg,s,tk,uk,q1q,q2q,xmatout)
c$$$c     !ER: IMPORTANT changes
c$$$c     1) Kept a distinction between gluon on the leg 1 or 2 (with the flag gleg). 
c$$$c        So only relevant amplitudes are evaluated.
c$$$c     2) Removed damping factors in some t-channel amplitudes (dg and gd suprocesses).
c$$$c Real matrix element squared, without damping and flux factors; qg processes
c$$$      implicit none
c$$$      include '../include/stpcblks.h'
c$$$      real*8 s,tk,uk,q1q,q2q,xmatout(8,2),q1c,q2c,w1,w2,s2,
c$$$     # t12,t13,t14,t15,t23,t24,t25,t34,t35,t45,pi,vtf,
c$$$     # Prop1_1,Prop1_2,Prop1_5,Prop1_6,
c$$$     # Prop1_3,Prop1_4,Prop1_7,Prop1_8,
c$$$     # MApMB2,MCpMD2,rMApMB2_s1,rMApMB2_s2,rMApMB2_s3,
c$$$     # rMApMB2_s4,rMApMB2_t1,rMCpMD2_t2,rMApMB2_t3,rMCpMD2_t4,
c$$$     # rMCpMD2_t5,rMApMB2_t6,rMCpMD2_t7,rMApMB2_t8,xdampd,xdampr
c$$$      parameter (pi=3.14159265358979312D0)
c$$$      parameter (vtf=1.d0/2.d0)
c$$$      integer i,j,idrmax(2:3,2)
c$$$      common/cidrmax/idrmax
c$$$c
c$$$      integer gleg
c$$$      do j=1,2
c$$$         do i=1,8
c$$$            xmatout(i,j) = 0d0
c$$$         enddo
c$$$      enddo
c$$$c
c$$$      s2 = s+tk+uk
c$$$      q1c = xm12 + xm22 - s - tk - q1q
c$$$      q2c = xm12 + xm22 - s - uk - q2q
c$$$      w1  = xm12 - q1q + q2q - tk
c$$$      w2  = xm22 - q2q + q1q - uk
c$$$c
c$$$c Map FNR invariants into Eric's sij's, according to the following rules
c$$$c  p1->-q1;  p2->-q2;  k1->q3;  k2->q4; k->q5
c$$$c This is just a renaming scheme for FNR invariants, and it is not
c$$$c associated with any specific short-distance process. In this way,
c$$$c 1 and 2 in sij are always associated with initial-state partons,
c$$$c 3 is the top, 4 is the (anti)bottom, 5 is the gluon.
c$$$c Note the minus signs in front of q1 and q2: all q's momenta are in the
c$$$c final state, and sij are therefore in a non-physical configuration.
c$$$c Since the matrix element routines MApMB2 and MCpMD2 assume the
c$$$c invariants to be in a non-physical configuration, no further
c$$$c explicit crossing (i.e. adding minus signs) is required. The
c$$$c crossing is implicit in the definitions of the sij's above.
c$$$c
c$$$      t12 = s
c$$$      t13 = q1q-xm12
c$$$      t14 = q1c-xm22
c$$$      t15 = tk
c$$$      t23 = q2c-xm12
c$$$      t24 = q2q-xm22
c$$$      t25 = uk
c$$$      t34 = s2-xm12-xm22
c$$$      t35 = w1-xm12
c$$$      t45 = w2-xm22
c$$$
c$$$ccccccccccccccccccccccccccccccccccccccccccc
c$$$c     !ER: changes.
c$$$c$$$      xdampd = q2q*q1c/( q2q*q1c+tk*uk )
c$$$c$$$      xdampr = xdampd
c$$$c$$$c The following is an alternative form, which gives identical results
c$$$c$$$c up to differences which vanish by increasing the statistics. Use the
c$$$c$$$c former since it gives slightly better numerical performances
c$$$c$$$c      xdampd = q2q/( q2q+uk )
c$$$c$$$c      xdampr = q1c/( q1c+tk )
c$$$      xdampd=1d0
c$$$      xdampr=1d0
c$$$cccccccccccccccccccccccccccccccccccccccccccc
c$$$      if(gleg.eq.1) then
c$$$c
c$$$c s-channel
c$$$c
c$$$         Prop1_3 = 1d0/(t25+t12+t15-xmW2)      
c$$$         rMApMB2_s3 = -MApMB2(xm12,t25,t23,t24,t12,
c$$$     #t35,t45,t15,t34,t13,t14)
c$$$         Prop1_4 = 1d0/(t25+t15+t12-xmW2)      
c$$$         rMApMB2_s4 = -MApMB2(xm12,t25,t35,t45,t15,
c$$$     #t23,t24,t12,t34,t13,t14)
c$$$         
c$$$         xmatout(3,1) = vtf*Prop1_3**2*rMApMB2_s3/4.d0
c$$$         xmatout(4,1) = vtf*Prop1_4**2*rMApMB2_s4/4.d0
c$$$         
c$$$c     
c$$$c     t-channel
c$$$c     
c$$$         Prop1_2 = 1d0/(t24-xmW2)      
c$$$         rMCpMD2_t2 = -MCpMD2(xm12,t24,t23,t25,t12,
c$$$     #t34,t45,t14,t35,t13,t15)
c$$$         Prop1_4 = 1d0/(t24-xmW2)
c$$$         rMCpMD2_t4 = -MCpMD2(xm12,t24,t34,t45,t14,
c$$$     #t23,t25,t12,t35,t13,t15)
c$$$         Prop1_8 = 1d0/(t14+t45+t15-xmW2)
c$$$         rMApMB2_t8 = -MApMB2(xm12,t45,t34,t24,t14,
c$$$     #t35,t25,t15,t23,t13,t12)
c$$$         rMApMB2_t8 = rMApMB2_t8 * xdampr
c$$$         
c$$$         xmatout(2,2) = vtf*Prop1_2**2*rMCpMD2_t2/4.d0
c$$$         xmatout(4,2) = vtf*Prop1_4**2*rMCpMD2_t4/4.d0 
c$$$         xmatout(8,2) = vtf*Prop1_8**2*rMApMB2_t8/4.d0
c$$$         
c$$$      elseif(gleg.eq.2) then
c$$$c     
c$$$c     s-channel
c$$$c     
c$$$         Prop1_1 = 1d0/(t15+t12+t25-xmW2)      
c$$$         rMApMB2_s1 = -MApMB2(xm12,t15,t13,t14,t12,
c$$$     #t35,t45,t25,t34,t23,t24)
c$$$         Prop1_2 = 1d0/(t15+t25+t12-xmW2)      
c$$$         rMApMB2_s2 = -MApMB2(xm12,t15,t35,t45,t25,
c$$$     #t13,t14,t12,t34,t23,t24)
c$$$         
c$$$         xmatout(1,1) = vtf*Prop1_1**2*rMApMB2_s1/4.d0
c$$$         xmatout(2,1) = vtf*Prop1_2**2*rMApMB2_s2/4.d0
c$$$         
c$$$c     
c$$$c     t-channel
c$$$c     
c$$$         Prop1_3 = 1d0/(t24+t45+t25-xmW2)
c$$$         rMApMB2_t3 = -MApMB2(xm12,t45,t34,t14,t24,
c$$$     #t35,t15,t25,t13,t23,t12)
c$$$         rMApMB2_t3 = rMApMB2_t3 * xdampd
c$$$         Prop1_5 = 1d0/(t14-xmW2)      
c$$$         rMCpMD2_t5 = -MCpMD2(xm12,t14,t13,t15,t12,
c$$$     #t34,t45,t24,t35,t23,t25)
c$$$         Prop1_7 = 1d0/(t14-xmW2)
c$$$         rMCpMD2_t7 = -MCpMD2(xm12,t14,t34,t45,t24,
c$$$     #t13,t15,t12,t35,t23,t25)
c$$$         
c$$$         xmatout(3,2) = vtf*Prop1_3**2*rMApMB2_t3/4.d0
c$$$         xmatout(5,2) = vtf*Prop1_5**2*rMCpMD2_t5/4.d0
c$$$         xmatout(7,2) = vtf*Prop1_7**2*rMCpMD2_t7/4.d0
c$$$      endif
c$$$      
c$$$      return
c$$$      end




      subroutine stmatr_fast_qq(s,tk,uk,q1q,q2q,xmatout,iii,jjj)
c Real matrix element squared, without damping and flux factors; qq processes
      implicit none
      include 'stpcblks.h'
      real*8 s,tk,uk,q1q,q2q,xmatout(8,2),q1c,q2c,w1,w2,s2,
     # t12,t13,t14,t15,t23,t24,t25,t34,t35,t45,pi,vcf,
     # Prop1_1,Prop2_1,Prop1_2,Prop2_2,Prop1_3,Prop2_3,Prop1_4,Prop2_4,
     # MApMB2,MCpMD2,
     # rMApMB2_s1,rMCpMD2_s1,rMApMB2_s3,rMCpMD2_s3,
     # rMApMB2_t1,rMCpMD2_t1,rMApMB2_t2,rMCpMD2_t2,
     # rMApMB2_t3,rMCpMD2_t3,rMApMB2_t4,rMCpMD2_t4
      parameter (pi=3.14159265358979312D0)
      parameter (vcf=4.d0/3.d0)
      integer i,j,idrmax(2:3,2)
      common/cidrmax/idrmax

      integer iii,jjj
c
      do j=1,2
        do i=1,idrmax(2,j)
          xmatout(i,j) = 0d0
        enddo
      enddo
c
      s2 = s+tk+uk
      q1c = xm12 + xm22 - s - tk - q1q
      q2c = xm12 + xm22 - s - uk - q2q
      w1  = xm12 - q1q + q2q - tk
      w2  = xm22 - q2q + q1q - uk
c
c Map FNR invariants into Eric's sij's, according to the following rules
c  p1->-q1;  p2->-q2;  k1->q3;  k2->q4; k->q5
c This is just a renaming scheme for FNR invariants, and it is not
c associated with any specific short-distance process. In this way,
c 1 and 2 in sij are always associated with initial-state partons,
c 3 is the top, 4 is the (anti)bottom, 5 is the gluon.
c Note the minus signs in front of q1 and q2: all q's momenta are in the
c final state, and sij are therefore in a non-physical configuration.
c Since the matrix element routines MApMB2 and MCpMD2 assume the
c invariants to be in a non-physical configuration, no further
c explicit crossing (i.e. adding minus signs) is required. The
c crossing is implicit in the definitions of the sij's above.
c
      t12 = s
      t13 = q1q-xm12
      t14 = q1c-xm22
      t15 = tk
      t23 = q2c-xm12
      t24 = q2q-xm22
      t25 = uk
      t34 = s2-xm12-xm22
      t35 = w1-xm12
      t45 = w2-xm22
c
c s-channel
c
c     11
      if((iii.eq.1).and.(jjj.eq.1)) then
         Prop1_1 = 1d0/(t12+t15+t25-xmW2)      
         Prop2_1 = 1d0/(t12-xmW2)      
         rMApMB2_s1 = MApMB2(xm12,t12,t13,t14,t15,
     #t23,t24,t25,t34,t35,t45)
         rMCpMD2_s1 = MCpMD2(xm12,t12,t13,t14,t15,
     #t23,t24,t25,t34,t35,t45)
         xmatout(1,1) = vcf*( Prop1_1**2*rMApMB2_s1 + 
     #Prop2_1**2*rMCpMD2_s1 )/4.d0
        
c     31
      elseif((iii.eq.3).and.(jjj.eq.1)) then
         Prop1_3 = 1d0/(t12+t15+t25-xmW2)      
         Prop2_3 = 1d0/(t12-xmW2)      
         rMApMB2_s3 = MApMB2(xm12,t12,t23,t24,t25,
     #t13,t14,t15,t34,t35,t45)
         rMCpMD2_s3 = MCpMD2(xm12,t12,t23,t24,t25,
     #t13,t14,t15,t34,t35,t45)
         xmatout(3,1) = vcf*( Prop1_3**2*rMApMB2_s3 + 
     #Prop2_3**2*rMCpMD2_s3 )/4.d0
c
c t-channel
c
c     12
      elseif((iii.eq.1).and.(jjj.eq.2)) then
         Prop1_1 = 1d0/(t24+t25+t45-xmW2)      
         Prop2_1 = 1d0/(t24-xmW2)      
         rMApMB2_t1 = MApMB2(xm12,t24,t23,t12,t25,
     #t34,t14,t45,t13,t35,t15)
         rMCpMD2_t1 = MCpMD2(xm12,t24,t23,t12,t25,
     #t34,t14,t45,t13,t35,t15)
         xmatout(1,2) = vcf*( Prop1_1**2*rMApMB2_t1 + 
     #Prop2_1**2*rMCpMD2_t1 )/4.d0

c     22
      elseif((iii.eq.2).and.(jjj.eq.2)) then
         Prop1_2 = 1d0/(t24+t45+t25-xmW2)      
         Prop2_2 = 1d0/(t24-xmW2)      
         rMApMB2_t2 = MApMB2(xm12,t24,t34,t14,t45,
     #t23,t12,t25,t13,t35,t15)
         rMCpMD2_t2 = MCpMD2(xm12,t24,t34,t14,t45,
     #t23,t12,t25,t13,t35,t15)
         xmatout(2,2) = vcf*( Prop1_2**2*rMApMB2_t2 + 
     #Prop2_2**2*rMCpMD2_t2 )/4.d0

c     32
      elseif((iii.eq.3).and.(jjj.eq.2)) then
      Prop1_3 = 1d0/(t14+t15+t45-xmW2)      
      Prop2_3 = 1d0/(t14-xmW2)      
      rMApMB2_t3 = MApMB2(xm12,t14,t13,t12,t15,
     #                    t34,t24,t45,t23,t35,t25)
      rMCpMD2_t3 = MCpMD2(xm12,t14,t13,t12,t15,
     #                    t34,t24,t45,t23,t35,t25)
         xmatout(3,2) = vcf*( Prop1_3**2*rMApMB2_t3 + 
     #Prop2_3**2*rMCpMD2_t3 )/4.d0

c     42
      elseif((iii.eq.4).and.(jjj.eq.2)) then
         Prop1_4 = 1d0/(t14+t45+t15-xmW2)      
         Prop2_4 = 1d0/(t14-xmW2)      
         rMApMB2_t4 = MApMB2(xm12,t14,t34,t24,t45,
     #t13,t12,t15,t23,t35,t25)
         rMCpMD2_t4 = MCpMD2(xm12,t14,t34,t24,t45,
     #t13,t12,t15,t23,t35,t25)
         xmatout(4,2) = vcf*( Prop1_4**2*rMApMB2_t4 + 
     #Prop2_4**2*rMCpMD2_t4 )/4.d0

      else
         write(*,*) 'Error in stmatr_fast_qq, wrong iii,jjj'
         write(*,*) '(iii,jjj)= ',iii,jjj
         stop
      endif
      return
      end


      subroutine stmatr_fast_qg(gleg,s,tk,uk,q1q,q2q,xmatout,iii,jjj)
c     !ER: IMPORTANT changes
c     1) Kept a distinction between gluon on the leg 1 or 2 (with the flag gleg). 
c        So only relevant amplitudes are evaluated.
c     2) Removed damping factors in some t-channel amplitudes (dg and gd suprocesses).
c Real matrix element squared, without damping and flux factors; qg processes
      implicit none
      include 'stpcblks.h'
      real*8 s,tk,uk,q1q,q2q,xmatout(8,2),q1c,q2c,w1,w2,s2,
     # t12,t13,t14,t15,t23,t24,t25,t34,t35,t45,pi,vtf,
     # Prop1_1,Prop1_2,Prop1_5,Prop1_6,
     # Prop1_3,Prop1_4,Prop1_7,Prop1_8,
     # MApMB2,MCpMD2,rMApMB2_s1,rMApMB2_s2,rMApMB2_s3,
     # rMApMB2_s4,rMApMB2_t1,rMCpMD2_t2,rMApMB2_t3,rMCpMD2_t4,
     # rMCpMD2_t5,rMApMB2_t6,rMCpMD2_t7,rMApMB2_t8,xdampd,xdampr
      parameter (pi=3.14159265358979312D0)
      parameter (vtf=1.d0/2.d0)
      integer i,j,idrmax(2:3,2)
      common/cidrmax/idrmax
c
      integer gleg
      integer iii,jjj
      do j=1,2
         do i=1,8
            xmatout(i,j) = 0d0
         enddo
      enddo
c
      s2 = s+tk+uk
      q1c = xm12 + xm22 - s - tk - q1q
      q2c = xm12 + xm22 - s - uk - q2q
      w1  = xm12 - q1q + q2q - tk
      w2  = xm22 - q2q + q1q - uk
c
c Map FNR invariants into Eric's sij's, according to the following rules
c  p1->-q1;  p2->-q2;  k1->q3;  k2->q4; k->q5
c This is just a renaming scheme for FNR invariants, and it is not
c associated with any specific short-distance process. In this way,
c 1 and 2 in sij are always associated with initial-state partons,
c 3 is the top, 4 is the (anti)bottom, 5 is the gluon.
c Note the minus signs in front of q1 and q2: all q's momenta are in the
c final state, and sij are therefore in a non-physical configuration.
c Since the matrix element routines MApMB2 and MCpMD2 assume the
c invariants to be in a non-physical configuration, no further
c explicit crossing (i.e. adding minus signs) is required. The
c crossing is implicit in the definitions of the sij's above.
c
      t12 = s
      t13 = q1q-xm12
      t14 = q1c-xm22
      t15 = tk
      t23 = q2c-xm12
      t24 = q2q-xm22
      t25 = uk
      t34 = s2-xm12-xm22
      t35 = w1-xm12
      t45 = w2-xm22

ccccccccccccccccccccccccccccccccccccccccccc
c     !ER: changes.
c$$$      xdampd = q2q*q1c/( q2q*q1c+tk*uk )
c$$$      xdampr = xdampd
c$$$c The following is an alternative form, which gives identical results
c$$$c up to differences which vanish by increasing the statistics. Use the
c$$$c former since it gives slightly better numerical performances
c$$$c      xdampd = q2q/( q2q+uk )
c$$$c      xdampr = q1c/( q1c+tk )
      xdampd=1d0
      xdampr=1d0
cccccccccccccccccccccccccccccccccccccccccccc
      if(gleg.eq.1) then
c
c s-channel
c
c     31
         if((iii.eq.3).and.(jjj.eq.1)) then
            Prop1_3 = 1d0/(t25+t12+t15-xmW2)      
            rMApMB2_s3 = -MApMB2(xm12,t25,t23,t24,t12,
     #t35,t45,t15,t34,t13,t14)
            xmatout(3,1) = vtf*Prop1_3**2*rMApMB2_s3/4.d0
            
c     41
         elseif((iii.eq.4).and.(jjj.eq.1)) then
            Prop1_4 = 1d0/(t25+t15+t12-xmW2)      
            rMApMB2_s4 = -MApMB2(xm12,t25,t35,t45,t15,
     #t23,t24,t12,t34,t13,t14)
            xmatout(4,1) = vtf*Prop1_4**2*rMApMB2_s4/4.d0
c     
c     t-channel
c     
c     22
         elseif((iii.eq.2).and.(jjj.eq.2)) then            
            Prop1_2 = 1d0/(t24-xmW2)      
            rMCpMD2_t2 = -MCpMD2(xm12,t24,t23,t25,t12,
     #t34,t45,t14,t35,t13,t15)
            xmatout(2,2) = vtf*Prop1_2**2*rMCpMD2_t2/4.d0

c     42
         elseif((iii.eq.4).and.(jjj.eq.2)) then            
            Prop1_4 = 1d0/(t24-xmW2)
            rMCpMD2_t4 = -MCpMD2(xm12,t24,t34,t45,t14,
     #t23,t25,t12,t35,t13,t15)
            xmatout(4,2) = vtf*Prop1_4**2*rMCpMD2_t4/4.d0 

c     82
         elseif((iii.eq.8).and.(jjj.eq.2)) then            
            Prop1_8 = 1d0/(t14+t45+t15-xmW2)
            rMApMB2_t8 = -MApMB2(xm12,t45,t34,t24,t14,
     #t35,t25,t15,t23,t13,t12)
            rMApMB2_t8 = rMApMB2_t8 * xdampr
            xmatout(8,2) = vtf*Prop1_8**2*rMApMB2_t8/4.d0
            
         else
            write(*,*) 'Error in stmatr_fast_qg, wrong iii,jjj'
            write(*,*) '(iii,jjj)= ',iii,jjj
            stop
         endif
         
      elseif(gleg.eq.2) then
c     
c     s-channel
c     
c     11
         if((iii.eq.1).and.(jjj.eq.1)) then            
            Prop1_1 = 1d0/(t15+t12+t25-xmW2)      
            rMApMB2_s1 = -MApMB2(xm12,t15,t13,t14,t12,
     #t35,t45,t25,t34,t23,t24)
            xmatout(1,1) = vtf*Prop1_1**2*rMApMB2_s1/4.d0
            
c     21
         elseif((iii.eq.2).and.(jjj.eq.1)) then            
            Prop1_2 = 1d0/(t15+t25+t12-xmW2)      
            rMApMB2_s2 = -MApMB2(xm12,t15,t35,t45,t25,
     #t13,t14,t12,t34,t23,t24)
            xmatout(2,1) = vtf*Prop1_2**2*rMApMB2_s2/4.d0
c     
c     t-channel
c     
c     32
         elseif((iii.eq.3).and.(jjj.eq.2)) then            
            Prop1_3 = 1d0/(t24+t45+t25-xmW2)
            rMApMB2_t3 = -MApMB2(xm12,t45,t34,t14,t24,
     #t35,t15,t25,t13,t23,t12)
            rMApMB2_t3 = rMApMB2_t3 * xdampd
            xmatout(3,2) = vtf*Prop1_3**2*rMApMB2_t3/4.d0

c     52
         elseif((iii.eq.5).and.(jjj.eq.2)) then            
            Prop1_5 = 1d0/(t14-xmW2)      
            rMCpMD2_t5 = -MCpMD2(xm12,t14,t13,t15,t12,
     #t34,t45,t24,t35,t23,t25)
            xmatout(5,2) = vtf*Prop1_5**2*rMCpMD2_t5/4.d0

c     72
         elseif((iii.eq.7).and.(jjj.eq.2)) then            
            Prop1_7 = 1d0/(t14-xmW2)
            rMCpMD2_t7 = -MCpMD2(xm12,t14,t34,t45,t24,
     #t13,t15,t12,t35,t23,t25)
            xmatout(7,2) = vtf*Prop1_7**2*rMCpMD2_t7/4.d0

         else
            write(*,*) 'Error in stmatr_fast_qg, wrong iii,jjj'
            write(*,*) '(iii,jjj)= ',iii,jjj
            stop
         endif

      endif
      
      return
      end


c
c In the functions MApMB2 and MCpMD2, all partons are outgoing, with
c   1=qbar,  2=q,  3=top,  4=bbar,  5=gluon
c
      real*8 function MApMB2(m2,s12,s13,s14,s15,s23,
     #                       s24,s25,s34,s35,s45)
c Square of the corrections to the light-quark weak current
      implicit real*8(a-z)
      MApMB2=
     &    2*s15**(-1)*s25**(-1)*s35*s14*s12 + 2*s15**(-1)*s25**(-1)*s45
     &    *s23*s12 + 4*s15**(-1)*s25**(-1)*s14*s23*s12 + 2*s15**(-1)*
     &    s45*s23 - 2*s15**(-1)*s14*s13 + 2*s15**(-1)*s14*s23 + 2*
     &    s25**(-1)*s35*s14 + 2*s25**(-1)*s14*s23 - 2*s25**(-1)*s24*s23
      return
      end

      real*8 function MCpMD2(m2,s12,s13,s14,s15,s23,
     #                       s24,s25,s34,s35,s45)
c Square of the corrections to the heavy-quark weak current
      implicit real*8(a-z)
      MCpMD2=
     &  - 4*m2*s25*s35**(-2)*s14 - 4*m2*s35**(-2)*s14*s23 + 2*s15*
     &    s35**(-1)*s45**(-1)*s34*s23 + 2*s15*s45**(-1)*s23 + 2*s25*
     &    s35**(-1)*s45**(-1)*s14*s34 + 2*s25*s35**(-1)*s14 + 4*
     &    s35**(-1)*s45**(-1)*s14*s34*s23 + 2*s35**(-1)*s14*s23 - 2*
     &    s35**(-1)*s13*s23 - 2*s45**(-1)*s14*s24 + 2*s45**(-1)*s14*s23
      return
      end

c     From the jet package, Altarelli-Parisi kernels and change of scheme
c     
c     
      function ap_kern(x,index)
c     This function returns the quantity (1-x)*P_{ab}(x), where
c     P_{ab} are the Altarelli-Parisi kernels, and the splitting partons
c     {ab} are defined with the following conventions
c     
c     index          ab
c     
c     1            gg
c     2            qg
c     3            gq
c     4            qq
c     
      implicit real * 8 (a-h,o-z)
      parameter (vcf=4.d0/3.d0)
      parameter (vtf=1.d0/2.d0)
      parameter (vca=3.d0)
c     
      if(index.eq.1)then
         ap_kern=2*vca*(x+(1-x)**2/x+x*(1-x)**2)
      elseif(index.eq.2)then
         ap_kern=vtf*(1-x)*(x**2+(1-x)**2)
      elseif(index.eq.3)then
         ap_kern=vcf*(1-x)*(1+(1-x)**2)/x
      elseif(index.eq.4)then
         ap_kern=vcf*(1+x**2)
      else
         write(6,*)'Error in ap_kern: wrong index value'
         stop
      endif
      return
      end

      function apprime_kern(x,index)
c This function returns the quantity (1-x)*P_{ab}^{prime}(x), where
c P_{ab}^{prime} is the ep-dependent part of the Altarelli-Parisi kernels, 
c and the codes for the splitting partons {ab} are defined above
      implicit real * 8 (a-h,o-z)
      parameter (vcf=4.d0/3.d0)
      parameter (vtf=1.d0/2.d0)
      parameter (vca=3.d0)
c
      if(index.eq.1)then
        apprime_kern=0.d0
      elseif(index.eq.2)then
        apprime_kern=-2*vtf*x*(1-x)**2
      elseif(index.eq.3)then
        apprime_kern=-vcf*(1-x)*x
      elseif(index.eq.4)then
        apprime_kern=-vcf*(1-x)**2
      else
        write(6,*)'Error in apprime_kern: wrong index value'
        stop
      endif
      return
      end

cccccccccccccccccccccccccccccccccccccccccccccccc
c     !: taken from mcnlo
c$$$      subroutine zzchvar(parth1,cth1,xjac,ro)
c$$$c
c$$$c Given 0<parth1<1 returns -1<cth1<1
c$$$c and multiplies xjac times the d cth1 / d parth1 jacobian
c$$$c
c$$$      implicit none
c$$$      real * 8 parth1,cth1,xjac,ro,bb,xlgbb,yy,expyy
c$$$      bb = 1-ro**2/16
c$$$      xlgbb = log((1+bb)/(1-bb))
c$$$      yy = ( parth1 * 2 - 1 ) * xlgbb
c$$$      xjac = xjac * 2 * xlgbb
c$$$      expyy = exp(-yy)
c$$$      cth1 = (1-expyy)/(1+expyy)/bb
c$$$      xjac = xjac * 2 * expyy/(1+expyy)**2 / bb
c$$$      return
c$$$      end
ccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccc
c     !ER: end of mcatnlo routines (taken from version 3.3)
ccccccccccccccccccccccccccccccccccccccccc




