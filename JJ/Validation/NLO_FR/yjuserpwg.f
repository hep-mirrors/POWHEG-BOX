      subroutine inijet
      call init_hist
      end

      subroutine outfun(www,x1)
      implicit none
      real * 8 www,x1
      real * 8 xkt_cm(1:3),xeta_cm(1:3),xphi_cm(1:3)
      common/cmkin/xkt_cm,xeta_cm,xphi_cm
      include '../../../include/hepevt.h'
      integer j
c build momenta on hepevt common block
      nhep=0
      do j=1,3
         if(xkt_cm(j).gt.0) then
            nhep=nhep+1
            isthep(nhep)=1
            phep(1,nhep)=xkt_cm(j)*cos(xphi_cm(j))
            phep(2,nhep)=xkt_cm(j)*sin(xphi_cm(j))
            phep(3,nhep)=xkt_cm(j)*sinh(xeta_cm(j))
            phep(4,nhep)=xkt_cm(j)*cosh(xeta_cm(j))
         endif
      enddo
      call analysis(www)
      end



c-----------------------------------------------------------------------
c **************************** USER ROUTINES ***************************
c-----------------------------------------------------------------------
c
c Weizsacker-Williams function for photon in electron. See also
c S. Frixione, M. Mangano, P. Nason, G. Ridolfi, Phys. Lett. B319(1993)339.
c
c THIS FUNCTION IS IRRELEVANT IN PROCESSES OTHER THAN PHOTON-HADRON COLLISIONS
c
      function phdistr(x,xmu2)
      implicit real * 8 (a-z)
      parameter (alfaem=1/137.d0)
      parameter (pi=3.14159265358979312D0)
      parameter (aemo2pi=alfaem/(2*pi))
      parameter (xme = 0.511d-3)
      parameter (xme2 = xme**2)
c cuts on E_gamma/E_e. x_inf and x_sup can be freely changed,
c provided that   0 <= x_inf < x_sup <= 1  (<= means less than or equal to)
      parameter (x_inf=0.2)
      parameter (x_sup=0.8)
      common/shadr/sh
c
c q2eff is the effective Weizsaecker-Williams scale SQUARED (in GeV^2). 
c It can be set in this function or using ZGMU2. In the latter case, 
c uncomment the following line and comment the next one
c      q2eff = xmu2
      q2eff = 4.d0
      tmp=0.d0
      if(x.gt.x_inf.and.x.lt.x_sup)
     #  tmp = (1+(1-x)**2)/x * log( q2eff*(1-x)/(xme*x)**2 )
     #        +2*xme2*x*( 1/q2eff-(1-x)/(xme*x)**2 )
      phdistr=aemo2pi*tmp
      end


c-------------------------------------------------------------------------
      function zgmu2()
c sets the mass scales and returns the strong coupling squared
c-------------------------------------------------------------------------
      implicit real * 8 (a-h,o-z)
      parameter (pi=3.14159265358979312D0)
c xsc_min2 = minimum transverse energy squared
c xlam = lambda_QCD (5 flavours)
c xmuf2 = factorization scale squared
c xmur2 = renormalization scale squared
c xmues2 = Ellis-Sexton scale squared
c xmuww2 = Weizsaecker-Williams scale squared
c q2ww = Weizsaecker-Williams scale squared
c zg = strong coupling = sqrt(4*pi*alfas)
c ze2 = electron electric charge squared
      common/fixvar/xsc_min2,xlam,xmuf2,xmur2,xmues2,xmuww2,zg,ze2
      common/q2ww/q2ww
c sclf2=xmuf2/(reference scale)**2
c sclr2=xmur2/(reference scale)**2
c scles2=xmues2/(reference scale)**2
      common/scalef/sclf2,sclr2,scles2
c xkt=modulus of the transverse momentum of parton # i (i=3,4,5)
c xeta=pseudorapidity of parton # i (i=3,4,5)
c xphi=azimuthal angle of parton # i (i=3,4,5)
c Define the reference scale in terms of these quantities
c WITH A BOOST-INVARIANT PRESCRIPTION
c NOTICE that the index i runs over the values 3,4,5 and NOT over 1,2,3
c as in the subroutine OUTFUN. xkt(*), xeta(*) and xphi(*) also have
c entry # 6. DO NOT USE IT
      common/partkin/xkt,xeta,xphi
c icls=0 ==> final state parton are well separated in the phase space
c icls=1 ==> parton # 3 and parton # 4 are close to each other.
c Two partons are close to each other in the sense of the definition
c adopted for the P-functions. Although not completely accurate, one
c can assume that, when a jet is obtained by the merging of two partons,
c these partons are labeled, in this function ZGMU2, as 3 and 4
      common/icls/icls
c number of light flavours
      common/nl/nl
      dimension xkt(3:6),xeta(3:6),xphi(3:6)
c
c one possible definition
      xpt = (xkt(3)+xkt(4)+xkt(5))/2.d0
c another possible definition
c      if(icls.eq.0)then
c        xpt = max(xkt(3),xkt(4),xkt(5))
c      else
c        xpt = max(xkt(3)+xkt(4),xkt(5))
c      endif
c reference scale squared
c      xmu2 = xpt**2
c P.N. POWHEG
      xmu2=100
c factorization scale squared
      xmuf2 = sclf2 * xmu2
c renormalization scale squared
      xmur2 = sclr2 * xmu2
c Ellis-Sexton scale squared
      xmues2 = scles2 * xmu2
c Weiszacker-Williams scale squared
      xmuww2 = xmu2
      q2ww = xmuww2
c alpha_s at xmur2
      as = alfas(xmur2,xlam,nl)
      zgmu2 = 4.d0*pi*as
      zg = sqrt(zgmu2)
      end


c-----------------------------------------------------------------------
c ********************* JET-FINDING ROUTINES ***************************
c-----------------------------------------------------------------------
c
c Cone algorithm
c
      subroutine cone(ykt,yeta,yphi,zkt,zeta,zphi,r_cone)
c This subroutine returns jet variables (zkt, zeta, zphi) defined
c in terms of partonic variables (ykt, yeta, yphi) in the cone
c algorithm. 
      implicit real * 8 (a-h,o-z)
      parameter (pi=3.14159265358979312D0)
      common/rcone/rcone
      common/imtt2/imtt2
      dimension ykt(1:3),yeta(1:3),yphi(1:3)
      dimension zkt(1:3),zeta(1:3),zphi(1:3)
      dimension xr(1:2,2:3),xp(1:2,2:3)
      integer imtt2(3:5,3:5)
c
      rcone=r_cone
      i2bd=1
      do i=1,3
        if(ykt(i).eq.0.d0)i2bd=0
      enddo
      if(i2bd.eq.0)then
c 2-body kinematics
        do i=1,3
          zkt(i)=ykt(i)
          zeta(i)=yeta(i)
          zphi(i)=yphi(i)
        enddo
      else
c 3-body kinematics
        xr(1,2)=rdist(yeta(1),yphi(1),yeta(2),yphi(2))
        xp(1,2)=ptdist(ykt(1),ykt(2))
        xr(1,3)=rdist(yeta(1),yphi(1),yeta(3),yphi(3))
        xp(1,3)=ptdist(ykt(1),ykt(3))
        xr(2,3)=rdist(yeta(2),yphi(2),yeta(3),yphi(3))
        xp(2,3)=ptdist(ykt(2),ykt(3))
        imerge=0
        do i=1,2
          do j=i+1,3
            if(xr(i,j).lt.xp(i,j))then
              if(imerge.eq.1)then
                write(6,*)'Fatal error in subroutine cone'
                stop
              endif
              imerge=1
              n1=i
              n2=j
            endif
          enddo
        enddo
        if(imerge.eq.0)then
c no merging
          do i=1,3
            zkt(i)=ykt(i)
            zeta(i)=yeta(i)
            zphi(i)=yphi(i)
          enddo
        else
          n3=imtt2(n1+2,n2+2)-2
          zkt(n3)=ykt(n3)
          zeta(n3)=yeta(n3)
          zphi(n3)=yphi(n3)
          call xmerge(ykt(n1),ykt(n2),tmpkt,
     #                yeta(n1),yeta(n2),tmpeta,
     #                yphi(n1),yphi(n2),tmpphi)
          zkt(n1)=tmpkt
          zeta(n1)=tmpeta
          zphi(n1)=tmpphi
          zkt(n2)=0.d0
          zeta(n2)=1.d8
          zphi(n2)=0.d0
        endif
      endif
      return
      end


      function rdist(eta1,phi1,eta2,phi2)
c
c Square root of eq. (2.22)
c
      implicit real * 8 (a-h,o-z)
      parameter (pi=3.14159265358979312D0)
c
      dteta=eta1-eta2
      dtphi=phi1-phi2
      dtphi=dacos(dcos(dtphi))
      rdist=dsqrt(dteta**2+dtphi**2)
      return
      end


      function ptdist(pt1,pt2)
c
c (pt1+pt2)*rcone/max(pt1,pt2), for the cone algorithm
c
      implicit real * 8 (a-h,o-z)
      common/rcone/rcone
c
      tmp=(pt1+pt2)/max(pt1,pt2)
      ptdist=tmp*rcone
      return
      end
c
c End of the cone algorithm
c
c 
c k_T clustering algorithm
c
      subroutine esjet(ykt,yeta,yphi,zkt,zeta,zphi,d_jet)
c This subroutine returns jet variables (zkt, zeta, zphi) defined
c in terms of partonic variables (ykt, yeta, yphi) in the Ellis-Soper
c algorithm. The parameter D must be set in the function ddist (in this file)
      implicit real * 8 (a-h,o-z)
      parameter (pi=3.14159265358979312D0)
      common/djet/djet
      common/imtt/imtt
      common/imtt2/imtt2
      dimension ykt(1:3),yeta(1:3),yphi(1:3)
      dimension zkt(1:3),zeta(1:3),zphi(1:3)
      dimension xd(1:3),xd2(1:2,2:3)
      integer imtt(3:4,3:5)
      integer imtt2(3:5,3:5)
c
      djet=d_jet
      i2bd=1
      do i=1,3
        if(ykt(i).eq.0.d0)i2bd=0
      enddo
      if(i2bd.eq.0)then
c 2-body kinematics
        do i=1,3
          zkt(i)=ykt(i)
          zeta(i)=yeta(i)
          zphi(i)=yphi(i)
        enddo
      else
c 3-body kinematics
        xd(1)=ykt(1)**2
        xd(2)=ykt(2)**2
        xd(3)=ykt(3)**2
        xd2(1,2)=ddist(ykt(1),yeta(1),yphi(1),ykt(2),yeta(2),yphi(2))
        xd2(1,3)=ddist(ykt(1),yeta(1),yphi(1),ykt(3),yeta(3),yphi(3))
        xd2(2,3)=ddist(ykt(2),yeta(2),yphi(2),ykt(3),yeta(3),yphi(3))
        xmin=min(xd(1),xd(2),xd(3),xd2(1,2),xd2(1,3),xd2(2,3))
        imerge=-1
        do i=1,3
          if(xd(i).eq.xmin)then
            imerge=0
            n1=i
          endif
        enddo
        if(imerge.eq.-1)then
          do i=1,2
            do j=i+1,3
              if(xd2(i,j).eq.xmin)then
                imerge=1
                n1=i
                n2=j
              endif
            enddo
          enddo
        endif
        if(imerge.eq.0)then
c no merging
          n2=imtt(3,n1+2)-2
          n3=imtt(4,n1+2)-2
          if(xd2(n2,n3).lt.min(xd(n2),xd(n3)))then
            write(6,*)'This is S2 contribution'
            stop
          endif
          do i=1,3
            zkt(i)=ykt(i)
            zeta(i)=yeta(i)
            zphi(i)=yphi(i)
          enddo
        elseif(imerge.eq.1)then
          n3=imtt2(n1+2,n2+2)-2
          zkt(n3)=ykt(n3)
          zeta(n3)=yeta(n3)
          zphi(n3)=yphi(n3)
          call xmerge(ykt(n1),ykt(n2),tmpkt,
     #                yeta(n1),yeta(n2),tmpeta,
     #                yphi(n1),yphi(n2),tmpphi)
          zkt(n1)=tmpkt
          zeta(n1)=tmpeta
          zphi(n1)=tmpphi
          zkt(n2)=0.d0
          zeta(n2)=1.d8
          zphi(n2)=0.d0
        else
          write(6,*)'Fatal error in subroutine esjet'
          write(6,*)'imerge=',imerge
          stop
        endif
      endif
      return
      end


      function rdist2(eta1,phi1,eta2,phi2)
c
c Eq. (2.22)
c
      implicit real * 8 (a-h,o-z)
      parameter (pi=3.14159265358979312D0)
c
      dteta=eta1-eta2
      dtphi=phi1-phi2
      dtphi=dacos(dcos(dtphi))
      rdist2=dteta**2+dtphi**2
      return
      end


      function ddist(pt1,eta1,phi1,pt2,eta2,phi2)
c
c Eq. (2.23)
c
      implicit real * 8 (a-h,o-z)
      common/djet/djet
c
      tmp=min(pt1**2,pt2**2)*rdist2(eta1,phi1,eta2,phi2)
      ddist=tmp/djet**2
      return
      end
c
c End of the k_T clustering algorithm
c
      subroutine xmerge
     #    (xkt1,xkt2,xkt3,xeta1,xeta2,xeta3,xphi1,xphi2,xphi3)
c
c Defines the jet variables (xkt3,xeta3,xphi3) in terms of the
c partonic variables when two partons are merged
c
      implicit real * 8 (a-h,o-z)
      parameter (pi=3.14159265358979312D0)
c
      xkt3=xkt1+xkt2
      xeta3=(xkt1*xeta1+xkt2*xeta2)/xkt3
      if(abs(xphi1-xphi2).lt.pi)then
        xphi3=(xkt1*xphi1+xkt2*xphi2)/xkt3
      else
        if(xphi2.lt.xphi1)then
          xphi3=(xkt1*xphi1+xkt2*(xphi2+2*pi))/xkt3
        else
          xphi3=(xkt1*xphi1+xkt2*(xphi2-2*pi))/xkt3
        endif
      endif
      xphi3=atan2(sin(xphi3),cos(xphi3))
      return
      end




c interfaccia a mbook
      subroutine pwhginihist
      call inihist
      end


      subroutine pwhgbookup(diag,str1,str2,bin,xl,xh)
      integer diag
      character *(*) str1,str2
      real * 8 bin,xl,xh
      real * 4 sbin,sxl,sxh
      character * 3 scales(200)
      common/plotsc/scales
      scales(1+4*(diag-1))=str2
      sbin=bin
      sxl=xl
      sxh=xh
      call bookup(1+4*(diag-1),str1,sbin,sxl,sxh)
      end

      subroutine topout
      character * 3 tag
      character * 3 scales(200)
      common/plotsc/scales
c      do i=1,200
c         call mfinal(i)
c      enddo
      do i=1,200         
         call gettag(i,tag)
         call mfinal(i)
         call mfinal(i+3)
         if(tag.eq.'YST') then
            call multitop(i,i+3,2,3,' ',' ',scales(i))
         endif
      enddo
      end            

      subroutine pwhgfill(diag,x,y)
      integer diag
      real * 8 x,y
      real * 4 sx,sy
      sx=x
c compensate for 10^-6 (pico to micro) in powheg analysis
      sy=y*1d6
      call mfill(1+(diag-1)*4,sx,sy)
      end
