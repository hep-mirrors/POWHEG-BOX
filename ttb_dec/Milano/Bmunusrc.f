      subroutine Bmunusrc(q,s1t,s2t,s12,c1,c2,Bmunu)
      implicit none
c----Matrix element for tt production
C----averaged over initial colours and and one spins
c    line in contracted with the vector n(mu)
C in is the label of the contracted line
c     g(-p1)+g(-p2)--> t(p3,p4,p5)+tb(p6,p7,p8)
      include 'constants.f'
      include 'zprods_com.f'
      include 'masses.f'
      double complex zanb(mxpart,mxpart),zbna(mxpart,mxpart)
      integer in,p1,p2,t,q4,a,q7,h1,nu,ro,si
      double precision s1t,s2t,s12,c1,c2,mtsq,
     & n(4),n4(4),n1(4),n2(4),n3(4),Bmunu(4,4,2),q(mxpart,4)
      double complex loab(4,2),loba(4,2),loqed(4,2)
      parameter(p1=1,p2=2,t=3,q4=4,a=5,q7=6)
      data n1/-1d0,0d0,0d0,0d0/
      data n2/0d0,-1d0,0d0,0d0/
      data n3/0d0,0d0,-1d0,0d0/
      data n4/0d0,0d0,0d0,+1d0/
      save n1,n2,n3,n4

      mtsq=mt**2
      Bmunu(:,:,:)=0d0

      do in=1,2
      do nu=1,4
      if (nu .eq. 1) n(:)=n1(:)
      if (nu .eq. 2) n(:)=n2(:)
      if (nu .eq. 3) n(:)=n3(:)
      if (nu .eq. 4) n(:)=n4(:)
      call spinork(6,q,zanb,zbna,n)
C  polarization is polarization of other gluon
      if	  (in .eq. 1) then
        loab(nu,1)= + zanb(q4,q4)*c1*s1t**(-1)*mtsq * ( za(p1,p2)*za(p2
     &    ,q7)*zb(q4,p1)*s12**(-1) )
      loab(nu,1) = loab(nu,1) + zanb(t,q4)*s1t**(-1)*mtsq * ( za(p1,p2)
     &    *za(p2,q7)*zb(t,p1)*s12**(-1) )
      loab(nu,1) = loab(nu,1) + zanb(t,q4)*c1*s1t**(-1) * ( za(q4,p2)*
     &    za(a,q7)*za(p1,p2)*zb(q4,t)*zb(a,p1)*s12**(-1) )
      loab(nu,1) = loab(nu,1) + zanb(t,t)*s1t**(-1) * ( za(t,p2)*za(a,
     &    q7)*za(p1,p2)*zb(q4,t)*zb(a,p1)*s12**(-1) )
      loab(nu,1) = loab(nu,1) + zanb(t,p1)*s1t**(-1) * ( za(a,q7)*za(p1
     &    ,p2)**2*zb(q4,t)*zb(a,p1)*s12**(-1) )
      loab(nu,1) = loab(nu,1) + zanb(t,p1)*s1t**(-1)*mtsq * ( za(p1,p2)
     &    *za(p2,q7)*zb(q4,t)*s12**(-1) )
      loab(nu,1) = loab(nu,1) + zanb(p1,p1)*mtsq * (  - 1.D0/2.D0*za(p1
     &    ,p2)*za(p2,q7)*zb(q4,p1)*s12**(-2) )
      loab(nu,1) = loab(nu,1) + zanb(p1,p1) * (  - 1.D0/2.D0*za(t,p2)*
     &    za(a,q7)*za(p1,p2)*zb(q4,t)*zb(a,p1)*s12**(-2) )
      loab(nu,1) = loab(nu,1) + zanb(p2,q4)*s1t**(-1)*mtsq * ( za(a,q7)
     &    *za(p1,p2)*zb(a,p1)*s12**(-1) )
      loab(nu,1) = loab(nu,1) + zanb(p2,p1)*mtsq * (  - za(p1,p2)*za(p1
     &    ,q7)*zb(q4,p1)*s12**(-2) )
      loab(nu,1) = loab(nu,1) + zanb(p2,p1) * (  - za(t,p1)*za(a,q7)*
     &    za(p1,p2)*zb(q4,t)*zb(a,p1)*s12**(-2) )
      loab(nu,1) = loab(nu,1) + zanb(p2,p2)*mtsq * (  - za(p1,p2)*za(p2
     &    ,q7)*zb(q4,p1)*s12**(-2) )
      loab(nu,1) = loab(nu,1) + zanb(p2,p2) * (  - za(t,p2)*za(a,q7)*
     &    za(p1,p2)*zb(q4,t)*zb(a,p1)*s12**(-2) )

        loab(nu,2)= + zanb(q4,q4)*c1*s1t**(-1)*mtsq * (  - za(p1,q7)*
     &    zb(q4,p2)*zb(p1,p2)*s12**(-1) )
      loab(nu,2) = loab(nu,2) + zanb(t,q4)*s1t**(-1)*mtsq * (  - za(p1,
     &    q7)*zb(t,p2)*zb(p1,p2)*s12**(-1) )
      loab(nu,2) = loab(nu,2) + zanb(t,q4)*c1*s1t**(-1) * (  - za(q4,p1
     &    )*za(a,q7)*zb(q4,t)*zb(a,p2)*zb(p1,p2)*s12**(-1) )
      loab(nu,2) = loab(nu,2) + zanb(t,t)*s1t**(-1) * (  - za(t,p1)*za(
     &    a,q7)*zb(q4,t)*zb(a,p2)*zb(p1,p2)*s12**(-1) )
      loab(nu,2) = loab(nu,2) + zanb(t,p2)*s1t**(-1)*mtsq * (  - za(p1,
     &    q7)*zb(q4,t)*zb(p1,p2)*s12**(-1) )
      loab(nu,2) = loab(nu,2) + zanb(p1,q4)*s1t**(-1)*mtsq * (  - za(a,
     &    q7)*zb(a,p2)*zb(p1,p2)*s12**(-1) - za(p1,q7)*zb(p1,p2)**2*
     &    s12**(-1) )
      loab(nu,2) = loab(nu,2) + zanb(p1,p1)*mtsq * ( 1.D0/2.D0*za(p1,q7
     &    )*zb(q4,p2)*zb(p1,p2)*s12**(-2) )
      loab(nu,2) = loab(nu,2) + zanb(p1,p1) * ( 1.D0/2.D0*za(t,p1)*za(a
     &    ,q7)*zb(q4,t)*zb(a,p2)*zb(p1,p2)*s12**(-2) )
      loab(nu,2) = loab(nu,2) + zanb(p1,p2)*mtsq * ( za(p1,q7)*zb(q4,p1
     &    )*zb(p1,p2)*s12**(-2) )
      loab(nu,2) = loab(nu,2) + zanb(p1,p2) * ( za(t,p1)*za(a,q7)*zb(q4
     &    ,t)*zb(a,p1)*zb(p1,p2)*s12**(-2) )
      loab(nu,2) = loab(nu,2) + zanb(p2,p2)*mtsq * ( za(p1,q7)*zb(q4,p2
     &    )*zb(p1,p2)*s12**(-2) )
      loab(nu,2) = loab(nu,2) + zanb(p2,p2) * ( za(t,p1)*za(a,q7)*zb(q4
     &    ,t)*zb(a,p2)*zb(p1,p2)*s12**(-2) )

        loba(nu,1)= + zanb(q4,a)*c1*s2t**(-1) * ( za(t,p2)*za(a,q7)*za(
     &    p1,p2)*zb(q4,t)*zb(q4,p1)*s12**(-1) )
      loba(nu,1) = loba(nu,1) + zanb(t,a)*s2t**(-1) * ( za(t,p2)*za(a,
     &    q7)*za(p1,p2)*zb(q4,t)*zb(t,p1)*s12**(-1) )
      loba(nu,1) = loba(nu,1) + zanb(p1,p1)*mtsq * ( 1.D0/2.D0*za(p1,p2
     &    )*za(p2,q7)*zb(q4,p1)*s12**(-2) )
      loba(nu,1) = loba(nu,1) + zanb(p1,p1) * ( 1.D0/2.D0*za(t,p2)*za(a
     &    ,q7)*za(p1,p2)*zb(q4,t)*zb(a,p1)*s12**(-2) )
      loba(nu,1) = loba(nu,1) + zanb(p2,a)*s2t**(-1) * (  - za(t,p2)*
     &    za(a,q7)*za(p1,p2)*zb(q4,t)*zb(p1,p2)*s12**(-1) )
      loba(nu,1) = loba(nu,1) + zanb(p2,a)*s2t**(-1)*mtsq * (  - za(a,
     &    q7)*za(p1,p2)*zb(q4,p1)*s12**(-1) )
      loba(nu,1) = loba(nu,1) + zanb(p2,p1)*mtsq * ( za(p1,p2)*za(p1,q7
     &    )*zb(q4,p1)*s12**(-2) )
      loba(nu,1) = loba(nu,1) + zanb(p2,p1) * ( za(t,p1)*za(a,q7)*za(p1
     &    ,p2)*zb(q4,t)*zb(a,p1)*s12**(-2) )
      loba(nu,1) = loba(nu,1) + zanb(p2,p2)*mtsq * ( za(p1,p2)*za(p2,q7
     &    )*zb(q4,p1)*s12**(-2) )
      loba(nu,1) = loba(nu,1) + zanb(p2,p2) * ( za(t,p2)*za(a,q7)*za(p1
     &    ,p2)*zb(q4,t)*zb(a,p1)*s12**(-2) )
      loba(nu,1) = loba(nu,1) + zanb(q7,q4)*c1*s2t**(-1)*mtsq * (  - 
     &    za(q4,p2)*za(p1,p2)*zb(q4,p1)*s12**(-1) )
      loba(nu,1) = loba(nu,1) + zanb(q7,t)*s2t**(-1)*mtsq * (  - za(t,
     &    p2)*za(p1,p2)*zb(q4,p1)*s12**(-1) )
      loba(nu,1) = loba(nu,1) + zanb(q7,p1)*s2t**(-1)*mtsq * ( za(t,p2)
     &    *za(p1,p2)*zb(q4,t)*s12**(-1) )

        loba(nu,2)= + zanb(q4,a)*c1*s2t**(-1) * (  - za(t,p1)*za(a,q7)*
     &    zb(q4,t)*zb(q4,p2)*zb(p1,p2)*s12**(-1) )
      loba(nu,2) = loba(nu,2) + zanb(t,a)*s2t**(-1) * (  - za(t,p1)*za(
     &    a,q7)*zb(q4,t)*zb(t,p2)*zb(p1,p2)*s12**(-1) )
      loba(nu,2) = loba(nu,2) + zanb(p1,a)*s2t**(-1)*mtsq * ( za(a,q7)*
     &    zb(q4,p2)*zb(p1,p2)*s12**(-1) )
      loba(nu,2) = loba(nu,2) + zanb(p1,p1)*mtsq * (  - 1.D0/2.D0*za(p1
     &    ,q7)*zb(q4,p2)*zb(p1,p2)*s12**(-2) )
      loba(nu,2) = loba(nu,2) + zanb(p1,p1) * (  - 1.D0/2.D0*za(t,p1)*
     &    za(a,q7)*zb(q4,t)*zb(a,p2)*zb(p1,p2)*s12**(-2) )
      loba(nu,2) = loba(nu,2) + zanb(p1,p2)*mtsq * (  - za(p1,q7)*zb(q4
     &    ,p1)*zb(p1,p2)*s12**(-2) )
      loba(nu,2) = loba(nu,2) + zanb(p1,p2) * (  - za(t,p1)*za(a,q7)*
     &    zb(q4,t)*zb(a,p1)*zb(p1,p2)*s12**(-2) )
      loba(nu,2) = loba(nu,2) + zanb(p2,p2)*mtsq * (  - za(p1,q7)*zb(q4
     &    ,p2)*zb(p1,p2)*s12**(-2) )
      loba(nu,2) = loba(nu,2) + zanb(p2,p2) * (  - za(t,p1)*za(a,q7)*
     &    zb(q4,t)*zb(a,p2)*zb(p1,p2)*s12**(-2) )
      loba(nu,2) = loba(nu,2) + zanb(q7,q4)*c1*s2t**(-1)*mtsq * ( za(q4
     &    ,p1)*zb(q4,p2)*zb(p1,p2)*s12**(-1) )
      loba(nu,2) = loba(nu,2) + zanb(q7,t)*s2t**(-1)*mtsq * ( za(t,p1)*
     &    zb(q4,p2)*zb(p1,p2)*s12**(-1) )
      loba(nu,2) = loba(nu,2) + zanb(q7,p2)*s2t**(-1)*mtsq * (  - za(t,
     &    p1)*zb(q4,t)*zb(p1,p2)*s12**(-1) - za(p1,p2)*zb(q4,p2)*zb(p1,
     &    p2)*s12**(-1) )

      elseif (in .eq. 2) then
        loab(nu,1)= + zanb(q4,a)*c1*s1t**(-1) * (  - za(t,p1)*za(a,q7)*
     &    za(p1,p2)*zb(q4,t)*zb(q4,p2)*s12**(-1) )
      loab(nu,1) = loab(nu,1) + zanb(t,a)*s1t**(-1) * (  - za(t,p1)*za(
     &    a,q7)*za(p1,p2)*zb(q4,t)*zb(t,p2)*s12**(-1) )
      loab(nu,1) = loab(nu,1) + zanb(p1,a)*s1t**(-1) * (  - za(t,p1)*
     &    za(a,q7)*za(p1,p2)*zb(q4,t)*zb(p1,p2)*s12**(-1) )
      loab(nu,1) = loab(nu,1) + zanb(p1,a)*s1t**(-1)*mtsq * ( za(a,q7)*
     &    za(p1,p2)*zb(q4,p2)*s12**(-1) )
      loab(nu,1) = loab(nu,1) + zanb(p1,p1)*mtsq * (  - za(p1,p2)*za(p1
     &    ,q7)*zb(q4,p2)*s12**(-2) )
      loab(nu,1) = loab(nu,1) + zanb(p1,p1) * (  - za(t,p1)*za(a,q7)*
     &    za(p1,p2)*zb(q4,t)*zb(a,p2)*s12**(-2) )
      loab(nu,1) = loab(nu,1) + zanb(p1,p2)*mtsq * ( za(p1,p2)*za(p1,q7
     &    )*zb(q4,p1)*s12**(-2) )
      loab(nu,1) = loab(nu,1) + zanb(p1,p2) * ( za(t,p1)*za(a,q7)*za(p1
     &    ,p2)*zb(q4,t)*zb(a,p1)*s12**(-2) )
      loab(nu,1) = loab(nu,1) + zanb(p2,p2)*mtsq * (  - 1.D0/2.D0*za(p1
     &    ,p2)*za(p1,q7)*zb(q4,p2)*s12**(-2) )
      loab(nu,1) = loab(nu,1) + zanb(p2,p2) * (  - 1.D0/2.D0*za(t,p1)*
     &    za(a,q7)*za(p1,p2)*zb(q4,t)*zb(a,p2)*s12**(-2) )
      loab(nu,1) = loab(nu,1) + zanb(q7,q4)*c1*s1t**(-1)*mtsq * ( za(q4
     &    ,p1)*za(p1,p2)*zb(q4,p2)*s12**(-1) )
      loab(nu,1) = loab(nu,1) + zanb(q7,t)*s1t**(-1)*mtsq * ( za(t,p1)*
     &    za(p1,p2)*zb(q4,p2)*s12**(-1) )
      loab(nu,1) = loab(nu,1) + zanb(q7,p2)*s1t**(-1)*mtsq * (  - za(t,
     &    p1)*za(p1,p2)*zb(q4,t)*s12**(-1) )

        loab(nu,2)= + zanb(q4,a)*c1*s1t**(-1) * ( za(t,p2)*za(a,q7)*zb(
     &    q4,t)*zb(q4,p1)*zb(p1,p2)*s12**(-1) )
      loab(nu,2) = loab(nu,2) + zanb(t,a)*s1t**(-1) * ( za(t,p2)*za(a,
     &    q7)*zb(q4,t)*zb(t,p1)*zb(p1,p2)*s12**(-1) )
      loab(nu,2) = loab(nu,2) + zanb(p1,p1)*mtsq * ( za(p2,q7)*zb(q4,p1
     &    )*zb(p1,p2)*s12**(-2) )
      loab(nu,2) = loab(nu,2) + zanb(p1,p1) * ( za(t,p2)*za(a,q7)*zb(q4
     &    ,t)*zb(a,p1)*zb(p1,p2)*s12**(-2) )
      loab(nu,2) = loab(nu,2) + zanb(p2,a)*s1t**(-1)*mtsq * (  - za(a,
     &    q7)*zb(q4,p1)*zb(p1,p2)*s12**(-1) )
      loab(nu,2) = loab(nu,2) + zanb(p2,p1)*mtsq * (  - za(p1,q7)*zb(q4
     &    ,p1)*zb(p1,p2)*s12**(-2) )
      loab(nu,2) = loab(nu,2) + zanb(p2,p1) * (  - za(t,p1)*za(a,q7)*
     &    zb(q4,t)*zb(a,p1)*zb(p1,p2)*s12**(-2) )
      loab(nu,2) = loab(nu,2) + zanb(p2,p2)*mtsq * ( 1.D0/2.D0*za(p2,q7
     &    )*zb(q4,p1)*zb(p1,p2)*s12**(-2) )
      loab(nu,2) = loab(nu,2) + zanb(p2,p2) * ( 1.D0/2.D0*za(t,p2)*za(a
     &    ,q7)*zb(q4,t)*zb(a,p1)*zb(p1,p2)*s12**(-2) )
      loab(nu,2) = loab(nu,2) + zanb(q7,q4)*c1*s1t**(-1)*mtsq * (  - 
     &    za(q4,p2)*zb(q4,p1)*zb(p1,p2)*s12**(-1) )
      loab(nu,2) = loab(nu,2) + zanb(q7,t)*s1t**(-1)*mtsq * (  - za(t,
     &    p2)*zb(q4,p1)*zb(p1,p2)*s12**(-1) )
      loab(nu,2) = loab(nu,2) + zanb(q7,p1)*s1t**(-1)*mtsq * ( za(t,p2)
     &    *zb(q4,t)*zb(p1,p2)*s12**(-1) - za(p1,p2)*zb(q4,p1)*zb(p1,p2)
     &    *s12**(-1) )

        loba(nu,1)= + zanb(q4,q4)*c1*s2t**(-1)*mtsq * (  - za(p1,p2)*
     &    za(p1,q7)*zb(q4,p2)*s12**(-1) )
      loba(nu,1) = loba(nu,1) + zanb(t,q4)*s2t**(-1)*mtsq * (  - za(p1,
     &    p2)*za(p1,q7)*zb(t,p2)*s12**(-1) )
      loba(nu,1) = loba(nu,1) + zanb(t,q4)*c1*s2t**(-1) * (  - za(q4,p1
     &    )*za(a,q7)*za(p1,p2)*zb(q4,t)*zb(a,p2)*s12**(-1) )
      loba(nu,1) = loba(nu,1) + zanb(t,t)*s2t**(-1) * (  - za(t,p1)*za(
     &    a,q7)*za(p1,p2)*zb(q4,t)*zb(a,p2)*s12**(-1) )
      loba(nu,1) = loba(nu,1) + zanb(t,p2)*s2t**(-1) * ( za(a,q7)*za(p1
     &    ,p2)**2*zb(q4,t)*zb(a,p2)*s12**(-1) )
      loba(nu,1) = loba(nu,1) + zanb(t,p2)*s2t**(-1)*mtsq * (  - za(p1,
     &    p2)*za(p1,q7)*zb(q4,t)*s12**(-1) )
      loba(nu,1) = loba(nu,1) + zanb(p1,q4)*s2t**(-1)*mtsq * (  - za(a,
     &    q7)*za(p1,p2)*zb(a,p2)*s12**(-1) )
      loba(nu,1) = loba(nu,1) + zanb(p1,p1)*mtsq * ( za(p1,p2)*za(p1,q7
     &    )*zb(q4,p2)*s12**(-2) )
      loba(nu,1) = loba(nu,1) + zanb(p1,p1) * ( za(t,p1)*za(a,q7)*za(p1
     &    ,p2)*zb(q4,t)*zb(a,p2)*s12**(-2) )
      loba(nu,1) = loba(nu,1) + zanb(p1,p2)*mtsq * (  - za(p1,p2)*za(p1
     &    ,q7)*zb(q4,p1)*s12**(-2) )
      loba(nu,1) = loba(nu,1) + zanb(p1,p2) * (  - za(t,p1)*za(a,q7)*
     &    za(p1,p2)*zb(q4,t)*zb(a,p1)*s12**(-2) )
      loba(nu,1) = loba(nu,1) + zanb(p2,p2)*mtsq * ( 1.D0/2.D0*za(p1,p2
     &    )*za(p1,q7)*zb(q4,p2)*s12**(-2) )
      loba(nu,1) = loba(nu,1) + zanb(p2,p2) * ( 1.D0/2.D0*za(t,p1)*za(a
     &    ,q7)*za(p1,p2)*zb(q4,t)*zb(a,p2)*s12**(-2) )

        loba(nu,2)= + zanb(q4,q4)*c1*s2t**(-1)*mtsq * ( za(p2,q7)*zb(q4
     &    ,p1)*zb(p1,p2)*s12**(-1) )
      loba(nu,2) = loba(nu,2) + zanb(t,q4)*s2t**(-1)*mtsq * ( za(p2,q7)
     &    *zb(t,p1)*zb(p1,p2)*s12**(-1) )
      loba(nu,2) = loba(nu,2) + zanb(t,q4)*c1*s2t**(-1) * ( za(q4,p2)*
     &    za(a,q7)*zb(q4,t)*zb(a,p1)*zb(p1,p2)*s12**(-1) )
      loba(nu,2) = loba(nu,2) + zanb(t,t)*s2t**(-1) * ( za(t,p2)*za(a,
     &    q7)*zb(q4,t)*zb(a,p1)*zb(p1,p2)*s12**(-1) )
      loba(nu,2) = loba(nu,2) + zanb(t,p1)*s2t**(-1)*mtsq * ( za(p2,q7)
     &    *zb(q4,t)*zb(p1,p2)*s12**(-1) )
      loba(nu,2) = loba(nu,2) + zanb(p1,p1)*mtsq * (  - za(p2,q7)*zb(q4
     &    ,p1)*zb(p1,p2)*s12**(-2) )
      loba(nu,2) = loba(nu,2) + zanb(p1,p1) * (  - za(t,p2)*za(a,q7)*
     &    zb(q4,t)*zb(a,p1)*zb(p1,p2)*s12**(-2) )
      loba(nu,2) = loba(nu,2) + zanb(p2,q4)*s2t**(-1)*mtsq * ( za(a,q7)
     &    *zb(a,p1)*zb(p1,p2)*s12**(-1) - za(p2,q7)*zb(p1,p2)**2*
     &    s12**(-1) )
      loba(nu,2) = loba(nu,2) + zanb(p2,p1)*mtsq * ( za(p1,q7)*zb(q4,p1
     &    )*zb(p1,p2)*s12**(-2) )
      loba(nu,2) = loba(nu,2) + zanb(p2,p1) * ( za(t,p1)*za(a,q7)*zb(q4
     &    ,t)*zb(a,p1)*zb(p1,p2)*s12**(-2) )
      loba(nu,2) = loba(nu,2) + zanb(p2,p2)*mtsq * (  - 1.D0/2.D0*za(p2
     &    ,q7)*zb(q4,p1)*zb(p1,p2)*s12**(-2) )
      loba(nu,2) = loba(nu,2) + zanb(p2,p2) * (  - 1.D0/2.D0*za(t,p2)*
     &    za(a,q7)*zb(q4,t)*zb(a,p1)*zb(p1,p2)*s12**(-2) )

      else
        write(6,*) 'Bmunusrc: Unimplemented value of in',in
        stop
      endif
      enddo

      loqed(:,:)=loab(:,:)+loba(:,:)

      do h1=1,2
      do ro=1,4
      do si=1,4
      Bmunu(ro,si,in)=Bmunu(ro,si,in)
     & +dble(loba(ro,h1)*dconjg(loba(si,h1)))
     & +dble(loab(ro,h1)*dconjg(loab(si,h1)))
     & -dble(loqed(ro,h1)*dconjg(loqed(si,h1)))/xnsq
      enddo
      enddo
      enddo
      enddo
      return
      end
