      subroutine btildevirt(resvirt)
      implicit none
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_br.h'
      include 'include/pwhg_math.h'
      include 'include/pwhg_st.h'
      include 'include/pwhg_flg.h'
      real * 8 resvirt(flst_nborn)
      real * 8 c(-6:6),gamma(-6:6),gammap(-6:6)
      integer j,jb,fl1,fl2,fl,leg,legi,legj
      real * 8 Q,I,s,etot,e,eij,virt_arr(flst_nborn),tot,kij,arglog,
     #     loglog
      real * 8 pdfb1(-6:6),pdfb2(-6:6)      
      real * 8 xicut
      parameter (xicut=par_csicut)
      logical ini
      data ini/.true./
      real * 8 ddilog,dotp
      external ddilog,dotp
      save c,gamma,gammap,ini
      real * 8 tiny
      parameter (tiny=1d-14)
      real * 8 ll
      real * 8 Intm_ep,Int0m_0,Int0m_ep,Intmm_0,Intmm_ep
      external Intm_ep,Int0m_0,Int0m_ep,Intmm_0,Intmm_ep
c from 2.100 of FNO2007
      if(ini) then
         do j=-6,6
            if(j.eq.0) then
               c(j)=ca
               gamma(j)=(11*ca-4*tf*st_nlight)/6
               gammap(j)=(67d0/9-2*pi**2/3)*ca-23d0/9*tf*st_nlight
            else
               c(j)=cf
               gamma(j)=3d0/2*cf
               gammap(j)=(13d0/2-2*pi**2/3)*cf
            endif
         enddo
         ini=.false.
      endif
c get pdfs
      call pdfcall(1,kn_xb1,pdfb1)
      call pdfcall(2,kn_xb2,pdfb2)      
c      call sigvirtual_fast(virt_arr)
      call sigvirtual(virt_arr)
c      write(*,*) 'finite VIRT 1'
c      write(*,*) virt_arr
c      call sigvirtual(virt_arr)
c      write(*,*) 'finite VIRT 2'
c      write(*,*) virt_arr

      etot=2*kn_cmpborn(0,1)
      s=etot**2
      tot=0d0
      ll = log(xicut**2*s/st_muren2)
      do jb=1,flst_nborn
         fl1=flst_born(1,jb)
         fl2=flst_born(2,jb)
c     initial-state parton contribution
         Q=-log(st_mufact2/st_muren2)*(gamma(fl1)+2*c(fl1)*log(xicut)
     #                              +  gamma(fl2)+2*c(fl2)*log(xicut))
c     loop on final-state massless partons
         do leg=flst_lightpart,nlegborn
            fl=flst_born(leg,jb)
            e=kn_cmpborn(0,leg)
            Q=Q+gammap(fl)
     #   -log(s/st_muren2)*(gamma(fl)-2*c(fl)*log(2*e/(xicut*etot)))
     #   +2*c(fl)*(log(2*e/etot)**2-log(xicut)**2)
     #   -2*gamma(fl)*log(2*e/etot)
         enddo
c     loop on final-state massive partons
         do leg=3,flst_lightpart-1
            if (abs(flst_born(leg,jb)).le.6.and.kn_masses(leg).gt.0) 
     #              then
               Q = Q - c(flst_born(leg,jb))*
     #              (ll-0.5*Intm_ep(kn_cmpborn(0,leg)))               
            endif
         enddo
         Q=Q*br_born(jb)
         I=0
         do legi=1,nlegborn
         do legj=legi+1,nlegborn
c     both particles are colored
         if (abs(flst_born(legi,jb)).le.6.and.
     #           abs(flst_born(legj,jb)).le.6) then
c     massless-massless case
            if (kn_masses(legi).eq.0.and.kn_masses(legj).eq.0) then
               kij=dotp(kn_cmpborn(0,legi),kn_cmpborn(0,legj))
               eij=kn_cmpborn(0,legi)*kn_cmpborn(0,legj)
               arglog = abs(1-kij/(2*eij))
               if (arglog.lt.tiny) then
                  loglog = 0d0
               else
                  loglog = log(arglog)*log(kij/(2*eij))
               endif                     
               I=I+(1d0/2*ll**2
     #              +ll*log(kij/(2*eij))
     #              -ddilog(kij/(2*eij))+1d0/2*log(kij/(2*eij))**2
     #              -loglog)*
     #              br_bornjk(legi,legj,jb)
            endif
c     massless-massive case
            if (kn_masses(legi).eq.0.and.kn_masses(legj).gt.0) then
               I = I + (0.5*(0.5*ll**2 - pi**2/6)
     #            +0.5*Int0m_0(kn_cmpborn(0,legi),kn_cmpborn(0,legj))*ll
     #            -0.5*Int0m_ep(kn_cmpborn(0,legi),kn_cmpborn(0,legj)))*
     #              br_bornjk(legi,legj,jb)
            endif
c     massive-massive case
            if (kn_masses(legi).gt.0.and.kn_masses(legj).gt.0) then
               I = I + (0.5*ll*
     #            Intmm_0(kn_cmpborn(0,legi),kn_cmpborn(0,legj))
     #            -0.5*Intmm_ep(kn_cmpborn(0,legi),kn_cmpborn(0,legj)))
     #            *br_bornjk(legi,legj,jb)
               
            endif
         endif
         enddo
         enddo
c         write(*,*) 'jb,Q,I',jb,Q,I
            
c we only summed over j>i, multiply by 2
         I=I*2
         resvirt(jb)=(Q+I+virt_arr(jb))*st_alpha/(2*pi)
     #       *pdfb1(fl1)*pdfb2(fl2)*kn_jacborn
         tot=tot+resvirt(jb)
      enddo
      end



c I_ep(k,m)
      function Int0m_ep(k,m)
c               /                                           [                  ]
c               |           d phi                           [   k.m       k0   ]
c Int0m_ep = -2 | d cos th  ----- log(sin th sin phi)  l0^2 [ ------- - ------ ]
c               |             pi                            [ k.l m.l   k.l l0 ]
c               /                                           [                  ]
c the range in phi is 0<phi<pi.
      implicit none
      real * 8 Int0m_ep, k(0:3),m(0:3)
      real * 8 kh(0:3),mh(0:3),khmh,b
      integer j
      real * 8 ddilog,dotp
      external ddilog,dotp
      do j=0,3
         kh(j)=k(j)/k(0)
         mh(j)=m(j)/m(0)
      enddo
      b=sqrt(1-dotp(mh,mh))
      khmh=dotp(kh,mh)
      Int0m_ep=-2*(log((1-b)/(1+b))**2/4+log(khmh/(1+b))*log(khmh/(1-b))
     # +ddilog(1-khmh/(1+b))+ddilog(1-khmh/(1-b)))
      end



c I_0(k,m)
c            /                       [                  ]
c            |           d phi       [   k.m       k0   ]
c  I_0     = | d cos th  -----  l0^2 [ ------- - ------ ]
c            |             pi        [ k.l m.l   k.l l0 ]
c            /                       [                  ]
c the range in phi is 0<phi<pi.
      function Int0m_0(k,m)
      implicit none
      real * 8 Int0m_0, k(0:3),m(0:3)
      real * 8 kh(0:3),mh(0:3)
      integer j
      real * 8 dotp
      external ddilog,dotp
      do j=0,3
         kh(j)=k(j)/k(0)
         mh(j)=m(j)/m(0)
      enddo
      Int0m_0=log( dotp(kh,mh)**2/dotp(mh,mh) )
      end



      function Intm_ep(p)
c               /                                              2
c               |           d phi                             p  
c  Intm_ep = -2 | d cos th  ----- log(sin th sin phi) l0^2  -------
c               |             pi                            (p.l)^2
c               /
c the range in phi is 0<phi<pi.
      implicit none
      real * 8 p(0:3),Intm_ep
      real * 8 beta2,beta
      integer j
      beta2=0
      do j=1,3
         beta2=beta2+(p(j)/p(0))**2
      enddo
      beta=sqrt(beta2)
      Intm_ep=2*log((1+beta)/(1-beta))/beta
      end



      function Intmm_ep(p1,p2)
c               / 
c               |           d phi                             p1.p2  
c Intmm_ep = -2 | d cos th  ----- log(sin th sin phi) k0^2 ----------
c               |             pi                           p1.k  p2.k
c               /
c p1^2>0, p2^2>0.
c The range in phi is 0<phi<pi.
      implicit none
      real * 8 p1(0:3),p2(0:3),Intmm_ep
      real * 8 i,z,zm,zp,z1,z2,a,b,x1,x2,c,alph,beta1(3),beta2(3),
     1 betasq1,betasq2,beta12,rootb
      integer j
      real * 8 ddilog
      external ddilog
      i(z)=-1d0/2*log((z-zm)*(zp-z)/((zp+z)*(z+zm)))**2
     # -2*ddilog(2*zm/(zp-zm)*(zp-z)/(zm+z))
     # -2*ddilog(-2*zp/(zp-zm)*(zm+z)/(zp-z))
      do j=1,3
         beta1(j)=p1(j)/p1(0)
         beta2(j)=p2(j)/p2(0)
      enddo
      betasq1=0
      betasq2=0
      beta12=0
      do j=1,3
         betasq1=betasq1+beta1(j)**2
         betasq2=betasq2+beta2(j)**2
         beta12=beta12+beta1(j)*beta2(j)
      enddo
      a=betasq1+betasq2-2*beta12
      x1=(betasq1-beta12)/a
      x2=(betasq2-beta12)/a
      b=(betasq1*betasq2-beta12**2)/a
      c=sqrt(b/(4*a))
      rootb=sqrt(b)
      zp=(1+sqrt(1-b))/rootb
      zm=(1-sqrt(1-b))/rootb
      z1=(sqrt(x1**2+4*c**2)-x1)/(2*c)
      z2=(sqrt(x2**2+4*c**2)+x2)/(2*c)
      Intmm_ep=(i(z2)-i(z1))*(1-beta12)
     #  /sqrt(a*(1-b))
      end

      function Intmm_0(p1,p2)
c            / 
c            |           d phi        p1.p2  
c  Intmm_0 = | d cos th  ----- k0^2 ----------
c            |             pi       p1.k  p2.k
c            /
c the range in phi is 0<phi<pi.
      implicit none
      real * 8 p1(0:3),p2(0:3), Intmm_0
      real * 8 kh(3),gh(3),k2,g2,kg,beta
      integer j
      do j=1,3
         kh(j)=p1(j)/p1(0)
         gh(j)=p2(j)/p2(0)
      enddo
      k2=0
      g2=0
      kg=0
      do j=1,3
         k2=k2+kh(j)**2
         g2=g2+gh(j)**2
         kg=kg+kh(j)*gh(j)
      enddo
      beta=sqrt(1-(1-k2)*(1-g2)/(1-kg)**2)
      Intmm_0=log((1+beta)/(1-beta))/beta
      end

