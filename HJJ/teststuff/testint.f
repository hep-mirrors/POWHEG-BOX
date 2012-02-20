      implicit none
      include 'brinclude.h'
      include 'pwhg_kn.h'
      include 'coupl.inc'
      include 'PhysPars.h'
      integer ndim,ncall,itmx,nprn
      real * 8 xl,xu,acc
      integer maxdim
      parameter (maxdim=20)
      common/bveg1/xl(maxdim),xu(maxdim),acc,ndim,ncall,itmx,nprn
      real * 8 xi,si,si2,swgt,schi
      integer ndo,it
      common/bveg2/xi(50,maxdim),si,si2,swgt,schi,ndo,it
      real * 8 avgi,sd,chi2a,fun0,fun1
      integer n,j
      external fun0,fun1
      hmass=200
      hwidth=0.004
      kn_sbeams=500**2
      kn_beams(0,1)=sqrt(kn_sbeams)/2
      kn_beams(0,2)=sqrt(kn_sbeams)/2
      kn_beams(1,1)=0
      kn_beams(1,2)=0
      kn_beams(2,1)=0
      kn_beams(2,2)=0
      kn_beams(3,1)= kn_beams(0,1)
      kn_beams(3,2)=-kn_beams(0,2)

      ncall=200000
      ndim=10
c      call vegas(fun0,avgi,sd,chi2a)
c      write(*,*) avgi,sd,chi2a

      call init_couplings


      ndim=11
      call vegas(fun0,avgi,sd,chi2a)
      write(*,*) avgi,sd,chi2a
      ndim=10
      call vegas(fun1,avgi,sd,chi2a)
      write(*,*) avgi,sd,chi2a
      ndim=11
      call vegas(fun0,avgi,sd,chi2a)
      write(*,*) avgi,sd,chi2a
      ndim=10
      call vegas(fun1,avgi,sd,chi2a)
      write(*,*) avgi,sd,chi2a
      end


      function fun0(x)
      implicit none
      include 'brinclude.h'
      include 'pwhg_kn.h'
      integer bflav(5)
      real * 8 fun0,x(*)
      real * 8 bbb
      call born_phsp(x)
      fun0=kn_jacborn*bbb()
      end

      function bbb()
      implicit none
      real * 8 bbb
      include 'pwhg_math.h'
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_kn.h'
      integer nlegs
      parameter (nlegs=nlegborn)
      real * 8 p(0:3,nlegs),bornjk(nlegs,nlegs)
      integer bflav(nlegs)
      real * 8 bmunu(0:3,0:3,nlegs),bbmunu(0:3,0:3),born,colcf
      integer HWW,HZZ
      call particle_identif(HWW,HZZ)
      bflav(1)=2
      bflav(2)=1
      bflav(3)=HZZ
      bflav(4)=2
      bflav(5)=1
      call setborn(kn_pborn,bflav,born,bornjk,bmunu)
      bbb=born
      end


      function fun1(x)
      implicit none
      include 'nlegbornvbf.h'
      include 'pwhg_kn.h'
      real * 8 fun1,x(*)
      real * 8 et,xm(3),p(4,3),wt 
      real * 8 bbb
      call born_phspvbf(x)
      fun1=kn_jacborn*bbb()
      end
