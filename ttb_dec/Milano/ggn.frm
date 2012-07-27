#call start
CF zaba,zba,zanb;
S pp,mm,mp,pm,a0,tag,h3,c1,c2,s12,s1t,s2t,c6,c8,tag1,tag2,tagA,tagB,mtsq;
V q1,q2,px,py,n,b1,p12,e1p,e1m,pt,ptb,pt1,pt2,t,a;
V q1,q2,q3,q4,q5,q6,q7,q8,e1,e2,p0;
I C1;

L A01np=-i_*A0qq(-1,n,e2p,+1)*tag1*tagA;
L A01nm=-i_*A0qq(-1,n,e2m,+1)*tag1*tagA;
L A02np=-i_*A0qq(-1,e1p,n,+1)*tag2*tagA;
L A02nm=-i_*A0qq(-1,e1m,n,+1)*tag2*tagA;

L B01np=-i_*A0qq(-1,n,e2p,+1)*tag1*tagB;
L B01nm=-i_*A0qq(-1,n,e2m,+1)*tag1*tagB;
L B02np=-i_*A0qq(-1,e1p,n,+1)*tag2*tagB;
L B02nm=-i_*A0qq(-1,e1m,n,+1)*tag2*tagB;


Id,A0qq(h3?,e1?,e2?,h4?)=
  +T(C1,C2)*Ub(pt,h3)*VF(j,e1)*prop(pt,p1)*VF(j,e2)*V(ptb,h4)
  +T(C2,C1)*Ub(pt,h3)*VF(j,e2)*prop(pt,p2)*VF(j,e1)*V(ptb,h4)
  +i_*(T(C1,C2)-T(C2,C1))*Ub(pt,h3)*VF(j,mu)*V(ptb,h4)*V3(mu,e1,e2,-p12,p1,p2)*(-i_)/2/p1.p2;

Id,prop(pt,p1)=i_*(g_(j,pt)+g_(j,p1)+mt)/2/pt.p1;
Id,prop(pt,p2)=i_*(g_(j,pt)+g_(j,p2)+mt)/2/pt.p2;
Id,prop(p3?,p1?)=i_*(g_(j,p3)+g_(j,p1)+mt)/2/p3.p1;
Id,p1.pt^-1=2/s1t;
Id,p2.pt^-1=2/s2t;
Id,p1.p2^-1=2/s12;
Id,V3(mu0?,mu1?,mu2?,p0?,p1?,p2?)=-gs*(
  +d_(mu0,mu1)*(-p2(mu2)-2*p1(mu2))
  +d_(mu1,mu2)*(2*p1(mu0)-0*p2(mu0))
  +d_(mu2,mu0)*(2*p2(mu1)+p1(mu1)));
Id,VF(j?,mu?)=-i_*gs*g_(j,mu);
#call polinsert1
Id,g_(j,p1?{p1,p2,q3,q4,q5,q6,q7,q8,t,a})=ket(p1,-1)*bra(p1,-1)+ket(p1,+1)*bra(p1,+1);
*---Now render pt massless wrt q4;
*---Now render pbt massless wrt q7;
Id,g_(j,pt)=g_(j,t)+c1*g_(j,q4);
Id,g_(j,ptb)=g_(j,a)+c2*g_(j,q7);
Id,g_(j,p1?{p1,p2,q3,q4,q5,q6,q7,q8,t,a})=ket(p1,-1)*bra(p1,-1)+ket(p1,+1)*bra(p1,+1);
#call zfix
Id,zab(p1?,pt,p2?)=za(p1,t)*zb(t,p2)+c1*za(p1,q4)*zb(q4,p2);
Id,zab(p1?,ptb,p2?)=za(p1,a)*zb(a,p2)+c2*za(p1,q4)*zb(q4,p2);
#call zcom

Id,p1?.n=1/2*zab(p1,n,p1);
Id,zab(p1?,n,p2?)=zanb(p1,p2);
*id,T(C2,C1)=0;
*id,T(C1,C2)=1;

multiply,gs^-2/rt2;

B zab;
Print +s;
.sort
id,tag1=1;
id,tag2=1;
multiply,za(p1,p2)*zb(p2,p1)/s12;
id,mt^2=mtsq;
#call zcom

id,tagA*T(C1,C2)=1;
id,tagA*T(C2,C1)=0;
id,tagB*T(C1,C2)=0;
id,tagB*T(C2,C1)=1;



B c1,c2,gs,mt,T,rt2,iza,izb,mtsq,s1t,s2t,zanb;
Print +s;
format doublefortran;
.sort
#write <Bmunusrc.f> "      subroutine Bmunusrc(q,s1t,s2t,s12,c1,c2,Bmunu)"
#write <Bmunusrc.f> "      implicit none"
#write <Bmunusrc.f> "c----Matrix element for tt production"
#write <Bmunusrc.f> "C----averaged over initial colours and and one spins"
#write <Bmunusrc.f> "c    line in contracted with the vector n(mu)"
#write <Bmunusrc.f> "C in is the label of the contracted line"
#write <Bmunusrc.f> "c     g(-p1)+g(-p2)--> t(p3,p4,p5)+tb(p6,p7,p8)"
#write <Bmunusrc.f> "      include \'constants.f\'"
#write <Bmunusrc.f> "      include \'zprods_com.f\'"
#write <Bmunusrc.f> "      include \'masses.f\'"
#write <Bmunusrc.f> "      double complex zanb(mxpart,mxpart),zbna(mxpart,mxpart)"
#write <Bmunusrc.f> "      integer in,p1,p2,t,q4,a,q7,h1,nu,ro,si"
#write <Bmunusrc.f> "      double precision s1t,s2t,s12,c1,c2,mtsq,"
#write <Bmunusrc.f> "     & n(4),n4(4),n1(4),n2(4),n3(4),Bmunu(4,4,2),q(mxpart,4)"
#write <Bmunusrc.f> "      double complex loab(4,2),loba(4,2),loqed(4,2)"
#write <Bmunusrc.f> "      parameter(p1=1,p2=2,t=3,q4=4,a=5,q7=6)"
#write <Bmunusrc.f> "      data n1/-1d0,0d0,0d0,0d0/"
#write <Bmunusrc.f> "      data n2/0d0,-1d0,0d0,0d0/"
#write <Bmunusrc.f> "      data n3/0d0,0d0,-1d0,0d0/"
#write <Bmunusrc.f> "      data n4/0d0,0d0,0d0,+1d0/"
#write <Bmunusrc.f> "      save n1,n2,n3,n4"
#write <Bmunusrc.f> ""
#write <Bmunusrc.f> "      mtsq=mt**2"
#write <Bmunusrc.f> "      Bmunu(:,:,:)=0d0"
#write <Bmunusrc.f> ""
#write <Bmunusrc.f> "      do in=1,2"
#write <Bmunusrc.f> "      do nu=1,4"
#write <Bmunusrc.f> "      if (nu .eq. 1) n(:)=n1(:)"
#write <Bmunusrc.f> "      if (nu .eq. 2) n(:)=n2(:)"
#write <Bmunusrc.f> "      if (nu .eq. 3) n(:)=n3(:)"
#write <Bmunusrc.f> "      if (nu .eq. 4) n(:)=n4(:)"
#write <Bmunusrc.f> "      call spinork(6,q,zanb,zbna,n)"
#write <Bmunusrc.f> "C  polarization is polarization of other gluon"
#write <Bmunusrc.f> "      if	  (in .eq. 1) then"
#write <Bmunusrc.f> "        loab(nu,1)=%e",A01nm(loab(nu,1))
#write <Bmunusrc.f> "        loab(nu,2)=%e",A01np(loab(nu,2))
#write <Bmunusrc.f> "        loba(nu,1)=%e",B01nm(loba(nu,1))
#write <Bmunusrc.f> "        loba(nu,2)=%e",B01np(loba(nu,2))
#write <Bmunusrc.f> "      elseif (in .eq. 2) then"
#write <Bmunusrc.f> "        loab(nu,1)=%e",A02nm(loab(nu,1))
#write <Bmunusrc.f> "        loab(nu,2)=%e",A02np(loab(nu,2))
#write <Bmunusrc.f> "        loba(nu,1)=%e",B02nm(loba(nu,1))
#write <Bmunusrc.f> "        loba(nu,2)=%e",B02np(loba(nu,2))
#write <Bmunusrc.f> "      else"
#write <Bmunusrc.f> "        write(6,*) \'Bmunusrc: Unimplemented value of in\',in"
#write <Bmunusrc.f> "        stop"
#write <Bmunusrc.f> "      endif"
#write <Bmunusrc.f> "      enddo"
#write <Bmunusrc.f> ""
#write <Bmunusrc.f> "      loqed(:,:)=loab(:,:)+loba(:,:)"
#write <Bmunusrc.f> ""
#write <Bmunusrc.f> "      do h1=1,2"
#write <Bmunusrc.f> "      do ro=1,4"
#write <Bmunusrc.f> "      do si=1,4"
#write <Bmunusrc.f> "      Bmunu(ro,si,in)=Bmunu(ro,si,in)"
#write <Bmunusrc.f> "     & +dble(loba(ro,h1)*dconjg(loba(si,h1)))"
#write <Bmunusrc.f> "     & +dble(loab(ro,h1)*dconjg(loab(si,h1)))"
#write <Bmunusrc.f> "     & -dble(loqed(ro,h1)*dconjg(loqed(si,h1)))/xnsq"
#write <Bmunusrc.f> "      enddo"
#write <Bmunusrc.f> "      enddo"
#write <Bmunusrc.f> "      enddo"
#write <Bmunusrc.f> "      enddo"
#write <Bmunusrc.f> "      return"
#write <Bmunusrc.f> "      end"
Print +s;
.end      
      subroutine ggttww1n
c--- Helicity amplitudes for the sub-process gg -> t+tbar,
c--- including decays (see qqb_QQbdk.f for calling details).
      implicit none
      include 'constants.f'
      include 'zprods_com.f'
      include 'masses.f'
      integer q1,q2,q3,q4,q5,q6,q7,q8
      double complex loab(2,2),loba(2,2)
      double complex x1mm,x1pp,x1pm,x1mp
      double complex x2mm,x2pp,x2pm,x2mp
      double complex x3mm,x3pp
      double precision mt2,s1t,s2t,s12
      parameter(q1=1,q2=2,q3=3,q4=4,q5=5,q6=6,q7=7,q8=8)

      mt2=mt**2

      x1pm=-zb(q1,q6)*za(q2,q6)/(s12*s1t)
     &*(za(q7,q5)*za(q2,q3)*zb(q1,q5)*zb(q3,q4)-za(q7,q2)*zb(q1,q4)*mt2)
      x2pm=-za(q2,q8)*zb(q1,q8)/(s12*s2t)
     &*(za(q7,q5)*za(q2,q3)*zb(q1,q5)*zb(q3,q4)-za(q7,q2)*zb(q1,q4)*mt2)

      x1mp=-za(q1,q6)*zb(q2,q6)/(s12*s1t)
     &*(za(q7,q5)*za(q1,q3)*zb(q2,q5)*zb(q3,q4)-za(q7,q1)*zb(q2,q4)*mt2)
      x2mp=-za(q1,q8)*zb(q2,q8)/(s12*s2t)
     &*(za(q7,q5)*za(q1,q3)*zb(q2,q5)*zb(q3,q4)-za(q7,q1)*zb(q2,q4)*mt2)

      x1mm=
     & za(q7,q5)*za(q1,q3)*zb(q3,q4)*zb(q4,q5)/(zb(q1,q4)*zb(q2,q4)*s1t)
     & *(za(q1,q2)*zb(q1,q4)+za(q2,q8)*zb(q4,q8))
      x2mm=
     & za(q7,q5)*za(q2,q3)*zb(q3,q4)*zb(q4,q5)/(zb(q1,q4)*zb(q2,q4)*s2t)
     & *(-za(q1,q2)*zb(q2,q4)+za(q1,q6)*zb(q4,q6))
      x3mm=
     & za(q7,q5)*za(q1,q2)*zb(q3,q4)*zb(q4,q5)/(zb(q1,q4)*zb(q2,q4)*s12)
     & *(za(q1,q3)*zb(q1,q4)+za(q2,q3)*zb(q2,q4))

      x1pp=za(q7,q3)*za(q7,q5)*za(q7,q6)*zb(q1,q6)*zb(q2,q5)*zb(q3,q4)
     & /(za(q7,q1)*za(q7,q2)*s1t)
      x2pp=za(q7,q3)*za(q7,q5)*za(q7,q8)*zb(q1,q5)*zb(q2,q8)*zb(q3,q4)
     & /(za(q7,q1)*za(q7,q2)*s2t)
      x3pp=-za(q7,q3)*za(q7,q5)*zb(q1,q2)*zb(q3,q4)
     &    *(za(q7,q1)*zb(q1,q5)+za(q7,q2)*zb(q2,q5))
     & /(za(q7,q1)*za(q7,q2)*s12)

      loab(1,1)=x1mm+x3mm
      loab(1,2)=x1mp
      loab(2,1)=x1pm
      loab(2,2)=x1pp+x3pp

      loba(1,1)=x2mm-x3mm
      loba(1,2)=x2mp
      loba(2,1)=x2pm
      loba(2,2)=x2pp-x3pp
 
    
      return
      end
