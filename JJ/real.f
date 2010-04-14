      subroutine setreal(p,rflav,amp2)
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_st.h'
      include '../include/pwhg_math.h'
      include 'PhysPars.h'

      real * 8 p(0:3,nlegreal)
      integer rflav(nlegreal)
      real * 8 amp2,amp2mad
      integer nleg
      parameter (nleg=nlegreal)
      integer mu,ileg

      real *8 kr_mad(0:3,nleg)


c     set madgraph parameters that can change on an event-by-event basis
      call mad_setparam

      do ileg=1,5
         do mu=0,3
            kr_mad(mu,ileg)=p(mu,ileg)
         enddo
      enddo
c     to avoid bugs in HELAS, restore exact masslessness of  incoming partons 
      kr_mad(0,1)=dabs(kr_mad(3,1))
      kr_mad(0,2)=dabs(kr_mad(3,2))

c$$$c     also for outgoig ?
c$$$      kr_mad(0,3)=sqrt(dabs(kr_mad(1,3)**2+kr_mad(2,3)**2+kr_mad(3,3)
c$$$     $     **2))
c$$$      kr_mad(0,4)=sqrt(dabs(kr_mad(1,4)**2+kr_mad(2,4)**2+kr_mad(3,4)
c$$$     $     **2))
c$$$      kr_mad(0,5)=sqrt(dabs(kr_mad(1,5)**2+kr_mad(2,5)**2+kr_mad(3,5)
c$$$     $     **2))

      
      call compreal(kr_mad,rflav,amp2mad)

      amp2=amp2mad/(st_alpha/2./pi)

      return
      end


C     THE FOLLOWING FUNCTIONS ARE TAKEN FROM ELLIS-SEXTON
c$$$      function Afunc(p1,p2,p3,p4,p5)
c$$$      implicit none
c$$$      include '../include/pwhg_st.h'
c$$$      include '../include/pwhg_math.h'
c$$$      real *8 Afunc
c$$$      real *8 p1(0:3),p2(0:3),p3(0:3),p4(0:3),p5(0:3)
c$$$      real *8 dotp,s,t,u,spp,tpp,upp,sp,tp,up,sm,tm,um,v1,v2
c$$$      external dotp
c$$$      real *8 p15,p25,p35,p45
c$$$
c$$$      v1=64./3.
c$$$      v2=8./3.
c$$$      s=2*dotp(p1,p2)
c$$$      t=2*dotp(p1,p3)
c$$$      u=2*dotp(p1,p4)
c$$$      spp=2*dotp(p3,p4)
c$$$      tpp=2*dotp(p2,p4)
c$$$      upp=2*dotp(p2,p3)
c$$$      sp=(s+spp)/2.
c$$$      tp=(t+tpp)/2.
c$$$      up=(u+upp)/2.
c$$$      sm=(s-spp)/2.
c$$$      tm=(t-tpp)/2.
c$$$      um=(u-upp)/2.
c$$$
c$$$      Afunc=
c$$$     $+v1*(
c$$$     $(sp**2+sm**2+up**2+um**2)*
c$$$     $(0.5*up*(s*spp+t*tpp-u*upp)-0.25*u*(s*t+spp*tpp)
c$$$     $     +0.25*upp*(s*tpp+spp*t))
c$$$     $)
c$$$     $-v2*(
c$$$     $(sp**2+sm**2+up**2+um**2)*
c$$$     $(0.5*sp*(s*spp-t*tpp-u*upp)+up*t*tpp+tp*u*upp)
c$$$     $)    
c$$$
c$$$      p15=dotp(p1,p5)
c$$$      p25=dotp(p2,p5)
c$$$      p35=dotp(p3,p5)
c$$$      p45=dotp(p4,p5)
c$$$
c$$$      Afunc=Afunc/t/tpp/p15/p25/p35/p45
c$$$
c$$$
c$$$      Afunc=Afunc*(4.*pi*st_alpha)**3
c$$$
c$$$
c$$$      end
