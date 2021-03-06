      subroutine borncolour_lh
c     Sets up the colour for the given flavour configuration
c     already filled in the Les Houches interface.
c     In case there are several colour structure, one
c     should pick one with a probability proportional to
c     the value of the corresponding cross section, for the
c     kinematics defined in the Les Houches interface
      implicit none
      include 'LesHouches.h'
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_kn.h'
      include 'PhysPars.h'
      integer iq,ia,iq1,iq2,ia1,ia2,ig1,ig2,j,itmp
      real * 8 s,t,u,xm2
      real * 8 t512,t152,t125,t521,t251,t215,t1r,t2r,t5r
      integer ichoice
      integer ipick
      external ipick
      

      xm2=ph_topmass2

c g g Q Q~ g
      if(idup(1).eq.21.and.idup(2).eq.21) then
         call ggplanar(pup(1,1),1,pup(1,2),1,pup(1,5),1, pup(1,3),
     $   pup(1,4),xm2,t512,t152,t125,t521,t251,t215)
         ichoice=ipick(t512,t152,t125,t521,t251,t215)
         if(ichoice.eq.1) then
            call clinkqggga(
     #icolup(1,3),icolup(1,5),icolup(1,1),icolup(1,2),icolup(1,4))
         elseif(ichoice.eq.2) then
            call clinkqggga(
     #icolup(1,3),icolup(1,1),icolup(1,5),icolup(1,2),icolup(1,4))
         elseif(ichoice.eq.3) then
            call clinkqggga(
     #icolup(1,3),icolup(1,1),icolup(1,2),icolup(1,5),icolup(1,4))
         elseif(ichoice.eq.4) then
            call clinkqggga(
     #icolup(1,3),icolup(1,5),icolup(1,2),icolup(1,1),icolup(1,4))
         elseif(ichoice.eq.5) then
            call clinkqggga(
     #icolup(1,3),icolup(1,2),icolup(1,5),icolup(1,1),icolup(1,4))
         elseif(ichoice.eq.6) then
            call clinkqggga(
     #icolup(1,3),icolup(1,2),icolup(1,1),icolup(1,5),icolup(1,4))
         endif
c     1 and 2 are incoming
         call conjcolor(icolup(1,1))
         call conjcolor(icolup(1,2))
      elseif((idup(1).ne.21).and.(idup(2).ne.21).and.(idup(1).gt.0))
     $        then
c     quark antiquark incoming: antiquark-quark outgoing
         call qqbplanar(pup(1,1),1,pup(1,2),1,pup(1,5),1,
     #pup(1,3),pup(1,4),xm2,t1r,t2r)
         ichoice=ipick(t1r,t2r,0d0,0d0,0d0,0d0)
         if(ichoice.eq.1) then
            call clinkqga(icolup(1,3),icolup(1,5),icolup(1,1))
            call clinkqa(icolup(1,2),icolup(1,4))
         else
            call clinkqa(icolup(1,3),icolup(1,1))
            call clinkqga(icolup(1,2),icolup(1,5),icolup(1,4))
         endif
         call conjcolor(icolup(1,1))
         call conjcolor(icolup(1,2))
      elseif((idup(1).lt.0).and.(idup(2).ne.21)) then
c     antiquark-quark
         call qqbplanar(pup(1,2),1,pup(1,1),1,pup(1,5),1,
     #pup(1,3),pup(1,4),xm2,t2r,t1r)
         ichoice=ipick(t2r,t1r,0d0,0d0,0d0,0d0)
         if(ichoice.eq.1) then
            call clinkqga(icolup(1,3),icolup(1,5),icolup(1,2))
            call clinkqa(icolup(1,1),icolup(1,4))
         else
            call clinkqa(icolup(1,3),icolup(1,2))
            call clinkqga(icolup(1,1),icolup(1,5),icolup(1,4))
         endif
         call conjcolor(icolup(1,1))
         call conjcolor(icolup(1,2))
      elseif((idup(1).ne.21).and.(idup(2).eq.21).and.(idup(1).gt.0))
     $        then
c     quark-gluone
         call qqbplanar(pup(1,1),1,pup(1,5),-1,pup(1,2),-1,
     #pup(1,3),pup(1,4),xm2,t1r,t5r)
         ichoice=ipick(t1r,t5r,0d0,0d0,0d0,0d0)
         if(ichoice.eq.1) then
            call clinkqga(icolup(1,3),icolup(1,2),icolup(1,1))
            call clinkqa(icolup(1,5),icolup(1,4))
         else
            call clinkqa(icolup(1,3),icolup(1,1))
            call clinkqga(icolup(1,5),icolup(1,2),icolup(1,4))
         endif
         call conjcolor(icolup(1,1))
         call conjcolor(icolup(1,2))
      elseif((idup(1).lt.0).and.(idup(2).eq.21)) then
c     antiquark-gluon
         call qqbplanar(pup(1,5),-1,pup(1,1),1,pup(1,2),-1,
     #pup(1,3),pup(1,4),xm2,t5r,t1r)
         ichoice=ipick(t5r,t1r,0d0,0d0,0d0,0d0)
         if(ichoice.eq.1) then
            call clinkqga(icolup(1,3),icolup(1,2),icolup(1,5))
            call clinkqa(icolup(1,1),icolup(1,4))
         else
            call clinkqa(icolup(1,3),icolup(1,5))
            call clinkqga(icolup(1,1),icolup(1,2),icolup(1,4))
         endif
         call conjcolor(icolup(1,1))
         call conjcolor(icolup(1,2))
      elseif((idup(1).eq.21).and.(idup(2).ne.21).and.(idup(2).gt.0))
     $        then
c     gluon-quark
         call qqbplanar(pup(1,2),1,pup(1,5),-1,pup(1,1),-1,
     #pup(1,3),pup(1,4),xm2,t2r,t5r)
         ichoice=ipick(t2r,t5r,0d0,0d0,0d0,0d0)
         if(ichoice.eq.1) then
            call clinkqga(icolup(1,3),icolup(1,1),icolup(1,2))
            call clinkqa(icolup(1,5),icolup(1,4))
         else
            call clinkqa(icolup(1,3),icolup(1,2))
            call clinkqga(icolup(1,5),icolup(1,1),icolup(1,4))
         endif
         call conjcolor(icolup(1,1))
         call conjcolor(icolup(1,2))
      elseif((idup(1).eq.21).and.(idup(2).lt.0)) then
c     gluon-antiquark
         call qqbplanar(pup(1,5),-1, pup(1,2),1,pup(1,1),-1,
     #pup(1,3),pup(1,4),xm2,t5r,t2r)
         ichoice=ipick(t5r,t2r,0d0,0d0,0d0,0d0)
         if(ichoice.eq.1) then
            call clinkqga(icolup(1,3),icolup(1,1),icolup(1,5))
            call clinkqa(icolup(1,2),icolup(1,4))
         else
            call clinkqa(icolup(1,3),icolup(1,5))
            call clinkqga(icolup(1,2),icolup(1,1),icolup(1,4))
         endif
         call conjcolor(icolup(1,1))
         call conjcolor(icolup(1,2))
      endif
      end

      subroutine ggplanar(p1,ip1,p2,ip2,k,ik,k1,k2,m2,
     # tr12,t1r2,t12r,tr21,t2r1,t21r)
      implicit none
      real * 8 p1(4),p2(4),k(4),k1(4),k2(4)
      REAL * 8 s,tk,uk,q1,q2,q1c,q2c,w1,w2,m,s2,
     # m2,tr12,t1r2,t12r,tr21,t2r1,t21r
      integer ip1,ip2,ik
      real * 8 dotprod
      external dotprod
      m=sqrt(m2)
      s=2*dotprod(p1,p2)*ip1*ip2
      tk=-2*dotprod(p1,k)*ip1*ik
      uk=-2*dotprod(p2,k)*ip2*ik
      q1=-2*dotprod(p1,k1)*ip1
      q2=-2*dotprod(p2,k2)*ip2
c Planar invariant squared amplitudes, summed over spins and color   
c for the planar g g -> q qbar g process; misses a factor Ncolor^4   
c k1: quark momentum
c k2: anti-quark momentum   
c p1: incoming gluon momentum   
c p2: incoming gluon momentum   
c k: radiated gluon momentum   
c   
c Planar configurations are obtained with the gluon all on the same
c side of the fermion line; a given colour flow is specified by
c the ordering of the attachment of the gluons on the fermion
c line. For example: k1,k,p1,p2,k2 is the color structure
c                             
c    k1 -<----||--<--  ---<---  ---<--- k2
c             ||     ||       ||       
c             ^V     ^V       ^V       
c             ||     ||       ||       
c             ||     ||       ||       
c             k      p1       p2
c
c
c return values:   
c tr12: planar configuration k1,k,p1,p2,k2   
c t1r2:                      k1,p1,k,p2,k2   
c t12r:                      k1,p1,p2,k,k2   
c tr21:                      k1,k,p2,p1,k2   
c t2r1:                      k1,p2,k,p1,k2   
c t21r:                      k1,p2,p1,k,k2   
c txyz: xyz stand for incoming gluon 1, 2, and radiated gluon (r)   
c   
c    d(p1,p1)=0   
c    d(p2,p2)=0   
c    d(k,k)=0   
c    d(k1,k1)=m2   
c    d(k2,k2)=m2   
c    d(p1,p2)=s/2   
c    d(p1,k)=-tk/2   
c    d(p2,k)=-uk/2   
c    d(p1,k1)=-q1/2   
c    d(p2,k2)=-q2/2   
c    d(k1,k2)=(s2-2*m2)/2   
c    d(p1,k2)=-q1c/2   
c    d(p2,k1)=-q2c/2   
c    d(k,k1)=w1/2   
c    d(k,k2)=w2/2   
c   
c  Relations among invariants   
c   
      s2=s+tk+uk   
      q1c=-s-tk-q1   
      q2c=-s-uk-q2   
      w1=-q1+q2-tk   
      w2=q1-q2-uk   
      TR12 = -(8*Q2*S*S2*TK*W1**5+(8*Q2*S*S2*TK**2+(8*Q2*S*S2**2+(-16*Q2
     1   *S**2-16*Q2**2*S)*S2)*TK-8*M**2*S**2*S2**2)*W1**4+(6*Q2*S*S2*TK
     2   **3+((8*M**2*Q2-24*Q2**2)*S-12*Q2*S**2)*S2*TK**2+(6*Q2*S*S2**3+
     3   ((-12*Q2-16*M**2)*S**2+8*M**2*Q2*S)*S2**2+(12*Q2*S**3+(24*Q2**2
     4   -16*M**2*Q2)*S**2+24*Q2**3*S)*S2)*TK+16*M**2*Q2*S**2*S2**2-16*M
     5   **2*Q2*S**3*S2)*W1**3+((2*Q2*S*S2-8*M**2*Q2**2)*TK**4+((-6*Q2*S
     6   **2+(8*M**2*Q2-12*Q2**2)*S+16*M**2*Q2**2)*S2-16*M**2*Q2**2*S)*T
     7   K**3+((-12*M**2*S**2-8*M**2*Q2**2)*S2**2+(6*Q2*S**3+(24*Q2**2-1
     8   6*M**2*Q2)*S**2+(24*Q2**3-8*M**2*Q2**2)*S)*S2-24*M**2*Q2**2*S**
     9   2)*TK**2+(2*Q2*S*S2**4+(8*M**2*Q2*S-6*Q2*S**2)*S2**3+(6*Q2*S**3
     :   +(-8*M**2*Q2-16*M**4)*S**2)*S2**2+(-4*Q2*S**4+(-12*Q2**2-16*M**
     ;   2*Q2)*S**3+(-24*Q2**3-8*M**2*Q2**2)*S**2-16*Q2**4*S)*S2-16*M**2
     <   *Q2**2*S**3)*TK-8*M**2*Q2**2*S**2*S2**2+16*M**2*Q2**2*S**3*S2-8
     =   *M**2*Q2**2*S**4)*W1**2+((-2*Q2*S**2-4*Q2**2*S)*S2*TK**4+((6*Q2
     >   **2*S-4*M**2*S**2)*S2**2+((6*Q2**2-4*M**2*Q2)*S**2+(12*Q2**3-16
     ?   *M**2*Q2**2+16*M**4*Q2)*S-16*M**2*Q2**3)*S2)*TK**3+(-6*Q2**2*S*
     @   S2**3+((-12*M**2*Q2-16*M**4)*S**2+(-12*Q2**3-8*M**2*Q2**2-16*M*
     1   *4*Q2)*S+16*M**2*Q2**3)*S2**2+(-2*Q2*S**4+(-6*Q2**2-4*M**2*Q2)*
     2   S**3+(-12*Q2**3-16*M**2*Q2**2+16*M**4*Q2)*S**2+(-16*Q2**4-16*M*
     3   *2*Q2**3)*S)*S2)*TK**2+(2*Q2**2*S*S2**4+(6*Q2**3+8*M**2*Q2**2+1
     4   6*M**4*Q2)*S*S2**3+((8*Q2**4+8*M**2*Q2**3)*S-16*M**4*Q2*S**2)*S
     5   2**2+(2*Q2**2*S**4+(6*Q2**3+8*M**2*Q2**2+16*M**4*Q2)*S**3+(8*Q2
     6   **4+8*M**2*Q2**3)*S**2+8*Q2**5*S)*S2)*TK)*W1+(-4*M**2*Q2*S**3+(
     7   -12*M**2*Q2**2-16*M**4*Q2-16*M**6)*S**2+(-16*M**2*Q2**3-16*M**4
     8   *Q2**2)*S-8*M**2*Q2**4)*S2**2*TK**2)/(Q2**2*S**2*S2**2*TK**2*W1
     9   **2)
      T1R2 = ((2*Q1*Q2*S2*TK**2+(4*Q1**2*Q2-2*Q1*Q2**2)*S2*TK+8*M**2*Q1*
     1   *2*Q2**2)*UK**4+((4*M**2*Q2*S2**2+(6*Q1*Q2**2+(4*M**2*Q1-6*Q1**
     2   2)*Q2)*S2)*TK**2+(-6*Q1**2*Q2*S2**2+(-6*Q1*Q2**3+(12*Q1**2-8*M*
     3   *2*Q1)*Q2**2+(-12*Q1**3+16*M**2*Q1**2-16*M**4*Q1)*Q2)*S2+16*M**
     4   2*Q1**2*Q2**2)*TK+(16*M**2*Q1**3*Q2-16*M**2*Q1**2*Q2**2)*S2)*UK
     5   **3+(2*Q1*Q2*S2*TK**4+(4*M**2*Q1*S2**2+((6*Q1**2+4*M**2*Q1)*Q2-
     6   6*Q1*Q2**2)*S2)*TK**3+((12*M**2*Q2**2+(12*M**2*Q1+16*M**4)*Q2+1
     7   2*M**2*Q1**2+16*M**4*Q1+16*M**6)*S2**2+(12*Q1*Q2**3+(16*M**2*Q1
     8   -24*Q1**2)*Q2**2+(12*Q1**3+16*M**2*Q1**2-16*M**4*Q1)*Q2)*S2+24*
     9   M**2*Q1**2*Q2**2)*TK**2+(6*Q1**2*Q2*S2**3+((12*Q1**3+8*M**2*Q1*
     :   *2+16*M**4*Q1)*Q2+16*M**2*Q1**3+16*M**4*Q1**2)*S2**2+(-8*Q1*Q2*
     ;   *4+(24*Q1**2-8*M**2*Q1)*Q2**3+(8*M**2*Q1**2-24*Q1**3)*Q2**2+(16
     <   *Q1**4+16*M**2*Q1**3)*Q2)*S2)*TK+(8*M**2*Q1**2*Q2**2-16*M**2*Q1
     =   **3*Q2+8*M**2*Q1**4)*S2**2)*UK**2+((4*Q1*Q2**2-2*Q1**2*Q2)*S2*T
     >   K**4+(-6*Q1*Q2**2*S2**2+(-12*Q1*Q2**3+(12*Q1**2+16*M**2*Q1)*Q2*
     ?   *2+(-6*Q1**3-8*M**2*Q1**2-16*M**4*Q1)*Q2)*S2+16*M**2*Q1**2*Q2**
     @   2)*TK**3+(6*Q1*Q2**2*S2**3+((12*Q1+16*M**2)*Q2**3+(8*M**2*Q1+16
     1   *M**4)*Q2**2+16*M**4*Q1*Q2)*S2**2+(16*Q1*Q2**4+(16*M**2*Q1-24*Q
     2   1**2)*Q2**3+(24*Q1**3+8*M**2*Q1**2)*Q2**2+(-8*Q1**4-8*M**2*Q1**
     3   3)*Q2)*S2)*TK**2+((-2*Q1*Q2**2-2*Q1**2*Q2)*S2**4+(-6*Q1*Q2**3-8
     4   *M**2*Q1*Q2**2+(-6*Q1**3-8*M**2*Q1**2-16*M**4*Q1)*Q2)*S2**3+(-8
     5   *Q1*Q2**4-8*M**2*Q1*Q2**3+(-8*Q1**4-8*M**2*Q1**3)*Q2)*S2**2+(-8
     6   *Q1*Q2**5+16*Q1**2*Q2**4-24*Q1**3*Q2**3+16*Q1**4*Q2**2-8*Q1**5*
     7   Q2)*S2)*TK)*UK+8*M**2*Q1**2*Q2**2*TK**4+(16*M**2*Q1*Q2**3-16*M*
     8   *2*Q1**2*Q2**2)*S2*TK**3+(8*M**2*Q2**4-16*M**2*Q1*Q2**3+8*M**2*
     9   Q1**2*Q2**2)*S2**2*TK**2)/(Q1**2*Q2**2*S2**2*TK**2*UK**2)
      T12R = -(8*Q1*S*S2*UK*W2**5+(8*Q1*S*S2*UK**2+(8*Q1*S*S2**2+(-16*Q1
     1   *S**2-16*Q1**2*S)*S2)*UK-8*M**2*S**2*S2**2)*W2**4+(6*Q1*S*S2*UK
     2   **3+((8*M**2*Q1-24*Q1**2)*S-12*Q1*S**2)*S2*UK**2+(6*Q1*S*S2**3+
     3   ((-12*Q1-16*M**2)*S**2+8*M**2*Q1*S)*S2**2+(12*Q1*S**3+(24*Q1**2
     4   -16*M**2*Q1)*S**2+24*Q1**3*S)*S2)*UK+16*M**2*Q1*S**2*S2**2-16*M
     5   **2*Q1*S**3*S2)*W2**3+((2*Q1*S*S2-8*M**2*Q1**2)*UK**4+((-6*Q1*S
     6   **2+(8*M**2*Q1-12*Q1**2)*S+16*M**2*Q1**2)*S2-16*M**2*Q1**2*S)*U
     7   K**3+((-12*M**2*S**2-8*M**2*Q1**2)*S2**2+(6*Q1*S**3+(24*Q1**2-1
     8   6*M**2*Q1)*S**2+(24*Q1**3-8*M**2*Q1**2)*S)*S2-24*M**2*Q1**2*S**
     9   2)*UK**2+(2*Q1*S*S2**4+(8*M**2*Q1*S-6*Q1*S**2)*S2**3+(6*Q1*S**3
     :   +(-8*M**2*Q1-16*M**4)*S**2)*S2**2+(-4*Q1*S**4+(-12*Q1**2-16*M**
     ;   2*Q1)*S**3+(-24*Q1**3-8*M**2*Q1**2)*S**2-16*Q1**4*S)*S2-16*M**2
     <   *Q1**2*S**3)*UK-8*M**2*Q1**2*S**2*S2**2+16*M**2*Q1**2*S**3*S2-8
     =   *M**2*Q1**2*S**4)*W2**2+((-2*Q1*S**2-4*Q1**2*S)*S2*UK**4+((6*Q1
     >   **2*S-4*M**2*S**2)*S2**2+((6*Q1**2-4*M**2*Q1)*S**2+(12*Q1**3-16
     ?   *M**2*Q1**2+16*M**4*Q1)*S-16*M**2*Q1**3)*S2)*UK**3+(-6*Q1**2*S*
     @   S2**3+((-12*M**2*Q1-16*M**4)*S**2+(-12*Q1**3-8*M**2*Q1**2-16*M*
     1   *4*Q1)*S+16*M**2*Q1**3)*S2**2+(-2*Q1*S**4+(-6*Q1**2-4*M**2*Q1)*
     2   S**3+(-12*Q1**3-16*M**2*Q1**2+16*M**4*Q1)*S**2+(-16*Q1**4-16*M*
     3   *2*Q1**3)*S)*S2)*UK**2+(2*Q1**2*S*S2**4+(6*Q1**3+8*M**2*Q1**2+1
     4   6*M**4*Q1)*S*S2**3+((8*Q1**4+8*M**2*Q1**3)*S-16*M**4*Q1*S**2)*S
     5   2**2+(2*Q1**2*S**4+(6*Q1**3+8*M**2*Q1**2+16*M**4*Q1)*S**3+(8*Q1
     6   **4+8*M**2*Q1**3)*S**2+8*Q1**5*S)*S2)*UK)*W2+(-4*M**2*Q1*S**3+(
     7   -12*M**2*Q1**2-16*M**4*Q1-16*M**6)*S**2+(-16*M**2*Q1**3-16*M**4
     8   *Q1**2)*S-8*M**2*Q1**4)*S2**2*UK**2)/(Q1**2*S**2*S2**2*UK**2*W2
     9   **2)
      TR21 = -(8*Q1C*S*S2*UK*W1**5+(8*Q1C*S*S2*UK**2+(8*Q1C*S*S2**2+(-16
     1   *Q1C*S**2-16*Q1C**2*S)*S2)*UK-8*M**2*S**2*S2**2)*W1**4+(6*Q1C*S
     2   *S2*UK**3+((8*M**2*Q1C-24*Q1C**2)*S-12*Q1C*S**2)*S2*UK**2+(6*Q1
     3   C*S*S2**3+((-12*Q1C-16*M**2)*S**2+8*M**2*Q1C*S)*S2**2+(12*Q1C*S
     4   **3+(24*Q1C**2-16*M**2*Q1C)*S**2+24*Q1C**3*S)*S2)*UK+16*M**2*Q1
     5   C*S**2*S2**2-16*M**2*Q1C*S**3*S2)*W1**3+((2*Q1C*S*S2-8*M**2*Q1C
     6   **2)*UK**4+((-6*Q1C*S**2+(8*M**2*Q1C-12*Q1C**2)*S+16*M**2*Q1C**
     7   2)*S2-16*M**2*Q1C**2*S)*UK**3+((-12*M**2*S**2-8*M**2*Q1C**2)*S2
     8   **2+(6*Q1C*S**3+(24*Q1C**2-16*M**2*Q1C)*S**2+(24*Q1C**3-8*M**2*
     9   Q1C**2)*S)*S2-24*M**2*Q1C**2*S**2)*UK**2+(2*Q1C*S*S2**4+(8*M**2
     :   *Q1C*S-6*Q1C*S**2)*S2**3+(6*Q1C*S**3+(-8*M**2*Q1C-16*M**4)*S**2
     ;   )*S2**2+(-4*Q1C*S**4+(-12*Q1C**2-16*M**2*Q1C)*S**3+(-24*Q1C**3-
     <   8*M**2*Q1C**2)*S**2-16*Q1C**4*S)*S2-16*M**2*Q1C**2*S**3)*UK-8*M
     =   **2*Q1C**2*S**2*S2**2+16*M**2*Q1C**2*S**3*S2-8*M**2*Q1C**2*S**4
     >   )*W1**2+((-2*Q1C*S**2-4*Q1C**2*S)*S2*UK**4+((6*Q1C**2*S-4*M**2*
     ?   S**2)*S2**2+((6*Q1C**2-4*M**2*Q1C)*S**2+(12*Q1C**3-16*M**2*Q1C*
     @   *2+16*M**4*Q1C)*S-16*M**2*Q1C**3)*S2)*UK**3+(-6*Q1C**2*S*S2**3+
     1   ((-12*M**2*Q1C-16*M**4)*S**2+(-12*Q1C**3-8*M**2*Q1C**2-16*M**4*
     2   Q1C)*S+16*M**2*Q1C**3)*S2**2+(-2*Q1C*S**4+(-6*Q1C**2-4*M**2*Q1C
     3   )*S**3+(-12*Q1C**3-16*M**2*Q1C**2+16*M**4*Q1C)*S**2+(-16*Q1C**4
     4   -16*M**2*Q1C**3)*S)*S2)*UK**2+(2*Q1C**2*S*S2**4+(6*Q1C**3+8*M**
     5   2*Q1C**2+16*M**4*Q1C)*S*S2**3+((8*Q1C**4+8*M**2*Q1C**3)*S-16*M*
     6   *4*Q1C*S**2)*S2**2+(2*Q1C**2*S**4+(6*Q1C**3+8*M**2*Q1C**2+16*M*
     7   *4*Q1C)*S**3+(8*Q1C**4+8*M**2*Q1C**3)*S**2+8*Q1C**5*S)*S2)*UK)*
     8   W1+(-4*M**2*Q1C*S**3+(-12*M**2*Q1C**2-16*M**4*Q1C-16*M**6)*S**2
     9   +(-16*M**2*Q1C**3-16*M**4*Q1C**2)*S-8*M**2*Q1C**4)*S2**2*UK**2)
     :   /(Q1C**2*S**2*S2**2*UK**2*W1**2)
      T2R1 = ((2*Q1C*Q2C*S2*TK**2+(4*Q1C**2*Q2C-2*Q1C*Q2C**2)*S2*TK+8*M*
     1   *2*Q1C**2*Q2C**2)*UK**4+((4*M**2*Q2C*S2**2+(6*Q1C*Q2C**2+(4*M**
     2   2*Q1C-6*Q1C**2)*Q2C)*S2)*TK**2+(-6*Q1C**2*Q2C*S2**2+(-6*Q1C*Q2C
     3   **3+(12*Q1C**2-8*M**2*Q1C)*Q2C**2+(-12*Q1C**3+16*M**2*Q1C**2-16
     4   *M**4*Q1C)*Q2C)*S2+16*M**2*Q1C**2*Q2C**2)*TK+(16*M**2*Q1C**3*Q2
     5   C-16*M**2*Q1C**2*Q2C**2)*S2)*UK**3+(2*Q1C*Q2C*S2*TK**4+(4*M**2*
     6   Q1C*S2**2+((6*Q1C**2+4*M**2*Q1C)*Q2C-6*Q1C*Q2C**2)*S2)*TK**3+((
     7   12*M**2*Q2C**2+(12*M**2*Q1C+16*M**4)*Q2C+12*M**2*Q1C**2+16*M**4
     8   *Q1C+16*M**6)*S2**2+(12*Q1C*Q2C**3+(16*M**2*Q1C-24*Q1C**2)*Q2C*
     9   *2+(12*Q1C**3+16*M**2*Q1C**2-16*M**4*Q1C)*Q2C)*S2+24*M**2*Q1C**
     :   2*Q2C**2)*TK**2+(6*Q1C**2*Q2C*S2**3+((12*Q1C**3+8*M**2*Q1C**2+1
     ;   6*M**4*Q1C)*Q2C+16*M**2*Q1C**3+16*M**4*Q1C**2)*S2**2+(-8*Q1C*Q2
     <   C**4+(24*Q1C**2-8*M**2*Q1C)*Q2C**3+(8*M**2*Q1C**2-24*Q1C**3)*Q2
     =   C**2+(16*Q1C**4+16*M**2*Q1C**3)*Q2C)*S2)*TK+(8*M**2*Q1C**2*Q2C*
     >   *2-16*M**2*Q1C**3*Q2C+8*M**2*Q1C**4)*S2**2)*UK**2+((4*Q1C*Q2C**
     ?   2-2*Q1C**2*Q2C)*S2*TK**4+(-6*Q1C*Q2C**2*S2**2+(-12*Q1C*Q2C**3+(
     @   12*Q1C**2+16*M**2*Q1C)*Q2C**2+(-6*Q1C**3-8*M**2*Q1C**2-16*M**4*
     1   Q1C)*Q2C)*S2+16*M**2*Q1C**2*Q2C**2)*TK**3+(6*Q1C*Q2C**2*S2**3+(
     2   (12*Q1C+16*M**2)*Q2C**3+(8*M**2*Q1C+16*M**4)*Q2C**2+16*M**4*Q1C
     3   *Q2C)*S2**2+(16*Q1C*Q2C**4+(16*M**2*Q1C-24*Q1C**2)*Q2C**3+(24*Q
     4   1C**3+8*M**2*Q1C**2)*Q2C**2+(-8*Q1C**4-8*M**2*Q1C**3)*Q2C)*S2)*
     5   TK**2+((-2*Q1C*Q2C**2-2*Q1C**2*Q2C)*S2**4+(-6*Q1C*Q2C**3-8*M**2
     6   *Q1C*Q2C**2+(-6*Q1C**3-8*M**2*Q1C**2-16*M**4*Q1C)*Q2C)*S2**3+(-
     7   8*Q1C*Q2C**4-8*M**2*Q1C*Q2C**3+(-8*Q1C**4-8*M**2*Q1C**3)*Q2C)*S
     8   2**2+(-8*Q1C*Q2C**5+16*Q1C**2*Q2C**4-24*Q1C**3*Q2C**3+16*Q1C**4
     9   *Q2C**2-8*Q1C**5*Q2C)*S2)*TK)*UK+8*M**2*Q1C**2*Q2C**2*TK**4+(16
     :   *M**2*Q1C*Q2C**3-16*M**2*Q1C**2*Q2C**2)*S2*TK**3+(8*M**2*Q2C**4
     ;   -16*M**2*Q1C*Q2C**3+8*M**2*Q1C**2*Q2C**2)*S2**2*TK**2)/(Q1C**2*
     <   Q2C**2*S2**2*TK**2*UK**2)
      T21R = -(8*Q2C*S*S2*TK*W2**5+(8*Q2C*S*S2*TK**2+(8*Q2C*S*S2**2+(-16
     1   *Q2C*S**2-16*Q2C**2*S)*S2)*TK-8*M**2*S**2*S2**2)*W2**4+(6*Q2C*S
     2   *S2*TK**3+((8*M**2*Q2C-24*Q2C**2)*S-12*Q2C*S**2)*S2*TK**2+(6*Q2
     3   C*S*S2**3+((-12*Q2C-16*M**2)*S**2+8*M**2*Q2C*S)*S2**2+(12*Q2C*S
     4   **3+(24*Q2C**2-16*M**2*Q2C)*S**2+24*Q2C**3*S)*S2)*TK+16*M**2*Q2
     5   C*S**2*S2**2-16*M**2*Q2C*S**3*S2)*W2**3+((2*Q2C*S*S2-8*M**2*Q2C
     6   **2)*TK**4+((-6*Q2C*S**2+(8*M**2*Q2C-12*Q2C**2)*S+16*M**2*Q2C**
     7   2)*S2-16*M**2*Q2C**2*S)*TK**3+((-12*M**2*S**2-8*M**2*Q2C**2)*S2
     8   **2+(6*Q2C*S**3+(24*Q2C**2-16*M**2*Q2C)*S**2+(24*Q2C**3-8*M**2*
     9   Q2C**2)*S)*S2-24*M**2*Q2C**2*S**2)*TK**2+(2*Q2C*S*S2**4+(8*M**2
     :   *Q2C*S-6*Q2C*S**2)*S2**3+(6*Q2C*S**3+(-8*M**2*Q2C-16*M**4)*S**2
     ;   )*S2**2+(-4*Q2C*S**4+(-12*Q2C**2-16*M**2*Q2C)*S**3+(-24*Q2C**3-
     <   8*M**2*Q2C**2)*S**2-16*Q2C**4*S)*S2-16*M**2*Q2C**2*S**3)*TK-8*M
     =   **2*Q2C**2*S**2*S2**2+16*M**2*Q2C**2*S**3*S2-8*M**2*Q2C**2*S**4
     >   )*W2**2+((-2*Q2C*S**2-4*Q2C**2*S)*S2*TK**4+((6*Q2C**2*S-4*M**2*
     ?   S**2)*S2**2+((6*Q2C**2-4*M**2*Q2C)*S**2+(12*Q2C**3-16*M**2*Q2C*
     @   *2+16*M**4*Q2C)*S-16*M**2*Q2C**3)*S2)*TK**3+(-6*Q2C**2*S*S2**3+
     1   ((-12*M**2*Q2C-16*M**4)*S**2+(-12*Q2C**3-8*M**2*Q2C**2-16*M**4*
     2   Q2C)*S+16*M**2*Q2C**3)*S2**2+(-2*Q2C*S**4+(-6*Q2C**2-4*M**2*Q2C
     3   )*S**3+(-12*Q2C**3-16*M**2*Q2C**2+16*M**4*Q2C)*S**2+(-16*Q2C**4
     4   -16*M**2*Q2C**3)*S)*S2)*TK**2+(2*Q2C**2*S*S2**4+(6*Q2C**3+8*M**
     5   2*Q2C**2+16*M**4*Q2C)*S*S2**3+((8*Q2C**4+8*M**2*Q2C**3)*S-16*M*
     6   *4*Q2C*S**2)*S2**2+(2*Q2C**2*S**4+(6*Q2C**3+8*M**2*Q2C**2+16*M*
     7   *4*Q2C)*S**3+(8*Q2C**4+8*M**2*Q2C**3)*S**2+8*Q2C**5*S)*S2)*TK)*
     8   W2+(-4*M**2*Q2C*S**3+(-12*M**2*Q2C**2-16*M**4*Q2C-16*M**6)*S**2
     9   +(-16*M**2*Q2C**3-16*M**4*Q2C**2)*S-8*M**2*Q2C**4)*S2**2*TK**2)
     :   /(Q2C**2*S**2*S2**2*TK**2*W2**2)
      END


      subroutine qqbplanar(p1,ip1,p2,ip2,k,ik,k1,k2,m2,
     # t1r,t2r)
      implicit none
      real * 8 p1(4),p2(4),k(4),k1(4),k2(4)
      REAL * 8 s,tk,uk,q1,q2,q1c,q2c,w1,w2,m,s2,
     # m2,t1r,t2r
      real * 8 dotprod
      external dotprod
      integer ip1,ip2,ik
c Planar invariant squared amplitudes, summed over spins and color 
c for the planar lq lqb -> q qbar g process; misses a factor Ncolor^4 
c k1: quark momentum 
c k2: anti-quark momentum 
c p1: incoming light quark momentum 
c p2: incoming light antiquark momentum 
c k: radiated gluon momentum 
c 
c return values: 
c t1r: planar configuration p1,k,k1 
c t2r: planar configuration p2,k,k2 
c 
c    d(p1,p1)=0 
c    d(p2,p2)=0 
c    d(k,k)=0 
c    d(k1,k1)=m2 
c    d(k2,k2)=m2 
c    d(p1,p2)=s/2 
c    d(p1,k)=-tk/2 
c    d(p2,k)=-uk/2 
c    d(p1,k1)=-q1/2 
c    d(p2,k2)=-q2/2 
c    d(k1,k2)=(s2-2*m2)/2 
c    d(p1,k2)=-q1c/2 
c    d(p2,k1)=-q2c/2 
c    d(k,k1)=w1/2 
c    d(k,k2)=w2/2 
c 
c  Relations among invariants 
c 
      m=sqrt(m2)
      s=2*dotprod(p1,p2)*ip1*ip2
      tk=-2*dotprod(p1,k)*ip1*ik
      uk=-2*dotprod(p2,k)*ip2*ik
      q1=-2*dotprod(p1,k1)*ip1
      q2=-2*dotprod(p2,k2)*ip2
      s2=s+tk+uk 
      q1c=-s-tk-q1 
      q2c=-s-uk-q2 
      w1=-q1+q2-tk 
      w2=q1-q2-uk 
      T1R = -(4*S*S2*W1**4+(8*S*S2*TK+(-4*S**2-12*Q2*S)*S2)*W1**3+(8*M**
     1   2*TK**3+((8*S-16*M**2)*S2+16*M**2*S)*TK**2+((8*M**2-4*S)*S2**2+
     2   ((-20*Q2-12*M**2)*S-4*S**2)*S2+12*M**2*S**2)*TK+2*S*S2**3+(4*Q2
     3   +4*M**2)*S*S2**2+(2*S**3+(8*Q2+4*M**2)*S**2+16*Q2**2*S)*S2+4*M*
     4   *2*S**3)*W1**2+(4*S*S2*TK**3+(((8*M**2-12*Q2)*S+16*M**2*Q2)*S2-
     5   4*S*S2**2)*TK**2+(2*S*S2**3+(8*Q2*S-16*M**2*Q2)*S2**2+(2*S**3+(
     6   4*Q2+12*M**2)*S**2+(16*Q2**2+16*M**2*Q2)*S)*S2)*TK-2*Q2*S*S2**3
     7   +(-4*Q2**2-4*M**2*Q2)*S*S2**2+(-2*Q2*S**3+(-4*Q2**2-4*M**2*Q2)*
     8   S**2-8*Q2**3*S)*S2)*W1+(4*M**2*S**2+(8*M**2*Q2+8*M**4)*S+8*M**2
     9   *Q2**2)*S2**2*TK)/(S**2*S2**2*TK*W1**2)
      T2R = -(4*S*S2*W2**4+(8*S*S2*UK+(-4*S**2-12*Q1*S)*S2)*W2**3+(8*M**
     1   2*UK**3+((8*S-16*M**2)*S2+16*M**2*S)*UK**2+((8*M**2-4*S)*S2**2+
     2   ((-20*Q1-12*M**2)*S-4*S**2)*S2+12*M**2*S**2)*UK+2*S*S2**3+(4*Q1
     3   +4*M**2)*S*S2**2+(2*S**3+(8*Q1+4*M**2)*S**2+16*Q1**2*S)*S2+4*M*
     4   *2*S**3)*W2**2+(4*S*S2*UK**3+(((8*M**2-12*Q1)*S+16*M**2*Q1)*S2-
     5   4*S*S2**2)*UK**2+(2*S*S2**3+(8*Q1*S-16*M**2*Q1)*S2**2+(2*S**3+(
     6   4*Q1+12*M**2)*S**2+(16*Q1**2+16*M**2*Q1)*S)*S2)*UK-2*Q1*S*S2**3
     7   +(-4*Q1**2-4*M**2*Q1)*S*S2**2+(-2*Q1*S**3+(-4*Q1**2-4*M**2*Q1)*
     8   S**2-8*Q1**3*S)*S2)*W2+(4*M**2*S**2+(8*M**2*Q1+8*M**4)*S+8*M**2
     9   *Q1**2)*S2**2*UK)/(S**2*S2**2*UK*W2**2)
      END

      function dotprod(p,q)
      implicit none
      real * 8 dotprod,p(4),q(4)
      dotprod=p(4)*q(4)-p(1)*q(1)-p(2)*q(2)-p(3)*q(3)
      end

c Subroutine to link colours for
c quark - gluon -gluon  -gluon - aquark
c in planar order
      subroutine clinkqggga(ic1,ic2,ic3,ic4,ic5)
      integer newcolor
      integer ic1(2),ic2(2),ic3(2),ic4(2),ic5(2)
      call getnewcolor(newcolor)
c ic1 is a quark: has colour, zero anticolor
      ic1(1)=newcolor
      ic1(2)=0
c ic2 is a gluon: link to quark
      ic2(2)=newcolor
      call getnewcolor(newcolor)
      ic2(1)=newcolor
c ic3 is a gluon
      ic3(2)=newcolor
      call getnewcolor(newcolor)
      ic3(1)=newcolor
c ic4 is an gluon
      ic4(2)=newcolor
      call getnewcolor(newcolor)
      ic4(1)=newcolor
c ic5 is an antiquark
      ic5(1)=0
      ic5(2)=newcolor
      end

c Subroutine to link colours for
c quark - gluon -gluon - aquark
c in planar order
      subroutine clinkqgga(ic1,ic2,ic3,ic4)
      integer newcolor
      integer ic1(2),ic2(2),ic3(2),ic4(2)
      call getnewcolor(newcolor)
c ic1 is a quark: has colour, zero anticolor
      ic1(1)=newcolor
      ic1(2)=0
c ic2 is a gluon: link to quark
      ic2(2)=newcolor
      call getnewcolor(newcolor)
      ic2(1)=newcolor
c ic3 is a gluon
      ic3(2)=newcolor
      call getnewcolor(newcolor)
      ic3(1)=newcolor
c ic4 is an anti-quark
      ic4(1)=0
      ic4(2)=newcolor
      end

c Subroutine to link colours for
c quark - gluon - aquark
c in planar order
      subroutine clinkqga(ic1,ic2,ic3)
      integer newcolor
      integer ic1(2),ic2(2),ic3(2)
      call getnewcolor(newcolor)
c ic1 is a quark: has colour, zero anticolor
      ic1(1)=newcolor
      ic1(2)=0
c ic2 is a gluon: link to quark
      ic2(2)=newcolor
      call getnewcolor(newcolor)
      ic2(1)=newcolor
c ic3 is an antiquark
      ic3(2)=newcolor
      ic3(1)=0
      end

c Subroutine to link colours for
c quark - aquark
c in planar order
      subroutine clinkqa(ic1,ic2)
      integer newcolor
      integer ic1(2),ic2(2)
      call getnewcolor(newcolor)
c ic1 is a quark: has colour, zero anticolor
      ic1(1)=newcolor
      ic1(2)=0
c ic2 is an antiquark
      ic2(1)=0
      ic2(2)=newcolor
      end

      subroutine conjcolor(cl)
      integer cl(2),i
      i=cl(1)
      cl(1)=cl(2)
      cl(2)=i
      end

      function ipick(y1,y2,y3,y4,y5,y6)
      implicit none
      integer ipick
      real * 8 y1,y2,y3,y4,y5,y6
      real * 8 t1,t2,t3,t4,t5,t6,r
      real * 8 random
      external random
      t1=y1
      t2=t1+y2
      t3=t2+y3
      t4=t3+y4
      t5=t4+y5
      t6=t5+y6
      r=t6*random()
      if(r.lt.t1) then
         ipick=1
      elseif(r.lt.t2) then
         ipick=2
      elseif(r.lt.t3) then
         ipick=3
      elseif(r.lt.t4) then
         ipick=4
      elseif(r.lt.t5) then
         ipick=5
      else
         ipick=6
      endif
      end

