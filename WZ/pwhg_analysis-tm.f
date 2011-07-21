c  The next subroutines, open some histograms and prepare them 
c      to receive data 
c  You can substitute these  with your favourite ones
c  init   :  opens the histograms
c  topout :  closes them
c  pwhgfill  :  fills the histograms with data

      subroutine init_hist
      implicit none
      include  'LesHouches.h'
      include '../pwhg_book.h'

      call inihists

c     total cross section sanity check
      call bookupeqbins('total',1d0,0d0,3d0)

      call  bookupeqbins('dS/dHT',20d0,0d0,500d0)
      call  bookupeqbins('dS/deta3',0.2d0,-4d0,4d0)
      call  bookupeqbins('dS/dpt3',5d0,0d0,150d0)
      call  bookupeqbins('dS/deta4',0.2d0,-4d0,4d0)
      call  bookupeqbins('dS/dpt4a',0.5d0,25d0,50d0)
      call  bookupeqbins('dS/dpt4b',5d0,0d0,150d0)
      call  bookupeqbins('dS/dpt4c',20d0,0d0,600d0)
      call  bookupeqbins('dS/deta34',0.4d0,-4d0,4d0)
      call  bookupeqbins('dS/dy34',0.4d0,-4d0,4d0)
      call  bookupeqbins('dS/dpt34',2d0,0d0,100d0)
      call  bookupeqbins('dS/dm34a',5d0,0d0,200d0)
      call  bookupeqbins('dS/dm34b',10d0,0d0,500d0)
      call  bookupeqbins('dS/deta5a',0.2d0,-4d0,4d0)
      call  bookupeqbins('dS/deta5b',0.5d0,-10d0,10d0)
      call  bookupeqbins('dS/dpt5',10d0,0d0,500d0)
      call  bookupeqbins('dS/dpt345',5d0,0d0,200d0)
      call  bookupeqbins('dS/deta345',0.2d0,-4d0,4d0)
      call  bookupeqbins('dS/dy345',0.2d0,-4d0,4d0)
      call  bookupeqbins('dS/dm345',2d0,108d0,308d0)
      call  bookupeqbins('dS/deta6',0.2d0,-4d0,4d0)
      call  bookupeqbins('dS/dpt6',10d0,0d0,500d0)
      call  bookupeqbins('dS/deta56',0.2d0,-4d0,4d0)
      call  bookupeqbins('dS/dpt56',10d0,10d0,150d0)
      call  bookupeqbins('dS/dm56a',5d0,0d0,200d0)
      call  bookupeqbins('dS/dm56b',50d0,0d0,2000d0)
      call  bookupeqbins('dS/dr56',0.1d0,0d0,4d0)
      call  bookupeqbins('dS/dphi56',0.314d0,0d0,3.14d0)
      call  bookupeqbins('dS/deta5-eta6',0.2d0,-6d0,6d0)
      call  bookupeqbins('dS/deta7',0.2d0,-4d0,4d0)
      call  bookupeqbins('dS/dpt7a',5d0,0d0,100d0)
      call  bookupeqbins('dS/dpt7b',20d0,0d0,600d0)
      call  bookupeqbins('dS/dr57',0.1d0,0d0,4d0)
      call  bookupeqbins('dS/dr67',0.1d0,0d0,4d0)
      call  bookupeqbins('dS/dm567',2d0,108d0,308d0)
      call  bookupeqbins('dS/deta diff',0.2d0,-6d0,6d0)




!$$
!$$
!$$
!$$      call bookupeqbins('pt(l+)',1d0,0d0,100d0)
!$$
!$$      call bookupeqbins('Z1pt',2d0,0d0,200d0)
!$$
!$$      call bookupeqbins('Wmll',2d0,0d0,130d0)
!$$
!$$      call bookupeqbins('Z1yrap',0.1d0,-4d0,4d0)
!$$
!$$      call bookupeqbins('yZ1mz2',0.1d0,-4d0,4d0)
!$$
!$$      call bookupeqbins('ZZmll1',2d0,0d0,130d0)
!$$
!$$      call bookupeqbins('ZZcoscm',0.02d0,-1d0,1d0)
!$$
!$$      call bookupeqbins('zlepcos',0.02d0,-1d0,1d0)
!$$
!$$      call bookupeqbins('cosplane',0.02d0,-1d0,1d0)
!$$
!$$      call bookupeqbins('jetpt',1d0,0d0,100d0)
!$$
!$$
      end
     
      subroutine analysis(dsig)
      implicit none
      real * 8 dsig
      include 'hepevt.h'
      include 'pwhg_math.h' 
      include  'LesHouches.h'
      include 'nwz.f'
      real *8 pt_lplus,pt_lminus,eta_lplus,eta_lminus,
     $delphi,mt_v,mv,ptv,yv
      real *8 pt_muplus,pt_muminus,eta_muplus,eta_muminus
      logical passcuts
      integer ihep,mu
      logical ini
      data ini/.true./
      save ini
c     binsize
c     we need to tell to this analysis file which program is running it
      integer   maxjet,mjets
      parameter (maxjet=2048)
      real * 8  ktj(maxjet),etaj(maxjet),rapj(maxjet),
     1    phij(maxjet),pj(4,maxjet),rr
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
      data WHCPRG/'NLO   '/
      integer vdecaytemp
      integer vdecaytemp1,vdecaytemp2,vdecaytemp3,vdecaytemp4
      real * 8 httot,y,eta,pt,mass,dy,deta,dphi,dr,eta5,eta6,eta7,etadif
      integer i3,i4,i5,i6,j,nu
      double precision ZZcoscm,ZZpt3,ZZmll1,ZZmll2,yz1mz2,yrapz1,yrapz2
      double precision ZZcosplane,l3xl4(3),l5xl6(3)
      double precision pz1(4),pz2(4),pZZcm(4),pz1_cm(4),pz2_cm(4)
      double precision ZZpcm(6,4),Zlepcos,pz34cm(4),p3zcm(4)
      double precision Wmll



      if (ini) then
         write (*,*)
         write (*,*) '********************************************'
         if(whcprg.eq.'NLO') then
            write(*,*) '       NLO analysis'
         elseif(WHCPRG.eq.'LHE   ') then
            write(*,*) '       LHE analysis'
         elseif(WHCPRG.eq.'HERWIG') then
            write (*,*) '           HERWIG ANALYSIS            '
            write(*,*) 'not implemented analysis'
            write(*,*) 'no plots will be present at the end of the run'
         elseif(WHCPRG.eq.'PYTHIA') then
            write (*,*) '           PYTHIA ANALYSIS            '
            write(*,*) 'not implemented analysis'
            write(*,*) 'no plots will be present at the end of the run'
         endif
         write(*,*) '*****************************'
!         vdecaytemp=lprup(1)-10000 ! Z decay product, with positive id
         vdecaytemp=11 ! Z decay product, with positive id

         if(vdecaytemp.eq.11.or.vdecaytemp.eq.13
     $        .or.vdecaytemp.eq.15) then
            continue
         else
            write(*,*) '**************************************'
            write(*,*) ' template analysis works only for e, mu and tau'
            write(*,*) '                 STOP     '
            write(*,*) '**************************************'
            call exit(1)
         endif
         ini=.false.
      endif


      if (nwz.eq.1) then
         vdecaytemp1=-11
         vdecaytemp2=12
         vdecaytemp3=13
         vdecaytemp4=-13
      elseif (nwz.eq.-1) then
         vdecaytemp1=11
         vdecaytemp2=-12
         vdecaytemp3=13
         vdecaytemp4=-13
      endif

      if((WHCPRG.eq.'NLO   ').or.(WHCPRG.eq.'LHE   ')) then
c     find Z decay products
         do ihep=1,nhep
            if(isthep(ihep).eq.1) then
               if(idhep(ihep).eq.vdecaytemp1) then
                  i3=ihep
               elseif(idhep(ihep).eq.vdecaytemp2) then
                  i4=ihep
               elseif(idhep(ihep).eq.vdecaytemp3) then
                  i5=ihep
               elseif(idhep(ihep).eq.vdecaytemp4) then
                  i6=ihep
               endif
            endif
         enddo
      else
         return
      endif


      rr=0.7
      call buildjets(1,mjets,rr,ktj,etaj,rapj,phij,pj)

      call getyetaptmass(phep(:,i3),y,eta_lminus,pt_lminus,mass)
      call getyetaptmass(phep(:,i4),y,eta_lplus,pt_lplus,mass)
      call getyetaptmass(phep(:,i5),y,eta_muminus,pt_muminus,mass)
      call getyetaptmass(phep(:,i6),y,eta_muplus,pt_muplus,mass)

      httot=pt_lminus+pt_lplus+pt_muminus+pt_muplus
      do j=1,mjets
         httot=httot+ktj(j)
      enddo


c MCFM  plots
      call filld('total',1.5d0,dsig)

      passcuts=.true.
      call getyetaptmass(phep(:,i3)+phep(:,i4),y,eta,pt,mass)
      call getyetaptmass(phep(:,i5)+phep(:,i6),y,eta,pt,mass)

      if(passcuts) then
         call  filld('dS/dHT',httot,dsig)
         call getyetaptmass(phep(:,i3),y,eta,pt,mass)
         call filld('dS/deta3',eta,dsig)
         call filld('dS/dpt3',pt,dsig)
         call getyetaptmass(phep(:,i4),y,eta,pt,mass)
         call filld('dS/deta4',eta,dsig)
         call filld('dS/dpt4a',pt,dsig)
         call filld('dS/dpt4b',pt,dsig)
         call filld('dS/dpt4c',pt,dsig)
         call getyetaptmass(phep(:,i3)+phep(:,i4),y,eta,pt,mass)
         call filld('dS/deta34',eta,dsig)
         call filld('dS/dy34',y,dsig)
         call filld('dS/dpt34',pt,dsig)
         call filld('dS/dm34a',mass,dsig)
         call filld('dS/dm34b',mass,dsig)
         call getyetaptmass(phep(:,i5),y,eta,pt,mass)
         eta5 = eta 
         call filld('dS/deta5a',eta,dsig)
         call filld('dS/deta5b',eta,dsig)
         call filld('dS/dpt5',pt,dsig)
         call getyetaptmass(phep(:,i3)+phep(:,i4)+phep(:,i5),
     1        y,eta,pt,mass)
         call filld('dS/dpt345',pt,dsig)
         call filld('dS/deta345',eta,dsig)
         call filld('dS/dy345',y,dsig)
         call filld('dS/dm345',mass,dsig)
         call getyetaptmass(phep(:,i6),y,eta,pt,mass)
         eta6 = eta 
         call filld('dS/deta6',eta,dsig)
         call filld('dS/dpt6',pt,dsig)
         call getyetaptmass(phep(:,i5)+phep(:,i6),y,eta,pt,mass)
         call filld('dS/deta56',eta,dsig)
         call filld('dS/dpt56',pt,dsig)
         call filld('dS/dm56a',mass,dsig)
         call filld('dS/dm56b',mass,dsig)
         call getdydetadphidr(phep(:,i5),phep(:,i6),dy,deta,dphi,dr)
         call filld('dS/dr56',dr,dsig)
         call filld('dS/dphi56',dphi,dsig)
         call filld('dS/deta5-eta6',deta,dsig)
         if(mjets.ge.1) then
            call getyetaptmass(pj(:,1),y,eta,pt,mass)
            eta7 = eta 
           call filld('dS/deta7',eta,dsig)
           call filld('dS/dpt7a',pt,dsig)
           call filld('dS/dpt7b',pt,dsig)
           call getdydetadphidr(phep(:,i5),pj(1:4,1),dy,deta,dphi,dr)
           call filld('dS/dr57',dr,dsig)
           call getdydetadphidr(phep(:,i6),pj(1:4,1),dy,deta,dphi,dr)
           call filld('dS/dr67',dr,dsig)
           call getyetaptmass(phep(1:4,i5)+phep(1:4,i6)+pj(:,1),
     .     y,eta,pt,mass)
           call filld('dS/dm567',mass,dsig)
           etadif = eta7-(eta6+eta5)/2d0 
           call filld('dS/deta diff',etadif,dsig)
        endif
      endif


!$$
!$$!     usual histograms for lepton 3 and 4
!$$      call getyetaptmass(phep(:,3),y,eta,pt,mass)
!$$      call filld('pt(l+)',pt,dsig)
!$$      
!$$
!$$      do nu=1,4
!$$         pz1(nu) = phep(nu,3)+phep(nu,4)
!$$         pz2(nu) = phep(nu,5)+phep(nu,6)
!$$      enddo
!$$
!$$!     usual histograms for Z1 momenta
!$$      call getyetaptmass(pz1,y,eta,pt,mass)
!$$      call filld('Z1pt',pt,dsig)
!$$      call filld('Wmll',mass,dsig)
!$$      call filld('Z1yrap',y,dsig)
!$$      yz1mz2 = y
!$$      call getyetaptmass(pz2,y,eta,pt,mass)
!$$      yz1mz2 = yz1mz2 - y
!$$      call filld('yZ1mz2',yz1mz2,dsig)
!$$      call filld('ZZmll1',mass,dsig)
!$$
!$$      
!$$!     calculate angle in the CM frame of the ZZ pair
!$$      call ZZcomangle(pz1,pz2,ZZcoscm)
!$$      if (abs(ZZcoscm).gt.1d0) then
!$$         stop 'abs zzcoscm gt 1'
!$$      endif
!$$      call filld('ZZcoscm',ZZcoscm,dsig)
!$$
!$$!     calc lepton 3 in the CM frame of Z
!$$      call ZZlepinZrf(pZ1,pZ2,phep(1:4,3),Zlepcos)
!$$      if (abs(Zlepcos).gt.1d0) then
!$$         stop 'abs zlepcos gt 1'
!$$      endif
!$$      call filld('zlepcos',Zlepcos,dsig)
!$$
!$$!     calc angle between lepton planes in ZZ COM frame
!$$      call ZZplane(phep(1:4,3),phep(1:4,4),phep(1:4,5),
!$$     .             phep(1:4,6),ZZcosplane)
!$$      if (abs(ZZcosplane).gt.1d0) then
!$$         stop 'abs zzcosplane gt 1'
!$$      endif
!$$      call filld('cosplane',ZZcosplane,dsig)
!$$
!$$      
!$$      
!$$      
!$$      if (mjets.ge.1) then
!$$         call getyetaptmass(pj(:,1),y,eta,pt,mass)
!$$         call filld('jetpt',pt,dsig)
!$$      endif
!$$


      end


      subroutine getyetaptmass(p,y,eta,pt,mass)
      implicit none
      real * 8 p(*),y,eta,pt,mass,pv
      y=0.5d0*log((p(4)+p(3))/(p(4)-p(3)))
      pt=sqrt(p(1)**2+p(2)**2)
      pv=sqrt(pt**2+p(3)**2)
      eta=0.5d0*log((pv+p(3))/(pv-p(3)))
      mass=sqrt(abs(p(4)**2-pv**2))
      end

      subroutine getdydetadphidr(p1,p2,dy,deta,dphi,dr)
      implicit none
      include 'pwhg_math.h' 
      real * 8 p1(*),p2(*),dy,deta,dphi,dr
      real * 8 y1,eta1,pt1,mass1,phi1
      real * 8 y2,eta2,pt2,mass2,phi2
      call getyetaptmass(p1,y1,eta1,pt1,mass1)
      call getyetaptmass(p2,y2,eta2,pt2,mass2)
      dy=y1-y2
      deta=eta1-eta2
      phi1=atan2(p1(1),p1(2))
      phi2=atan2(p2(1),p2(2))
      dphi=abs(phi1-phi2)
      dphi=min(dphi,2d0*pi-dphi)
      dr=sqrt(deta**2+dphi**2)
      end

      subroutine getrapidity(p,y)
      implicit none
      real * 8 p(0:3),y
      y=0.5d0*log((p(0)+p(3))/(p(0)-p(3)))
      end

      subroutine getinvmass(p,m)
      implicit none
      real * 8 p(0:3),m
      m=sqrt(abs(p(0)**2-p(1)**2-p(2)**2-p(3)**2))
      end

      subroutine get_pseudorap(p,eta)
      implicit none
      real*8 p(0:3),eta,pt,th
      real *8 tiny
      parameter (tiny=1.d-5)

      pt=sqrt(p(1)**2+p(2)**2)
      if(pt.lt.tiny.and.abs(p(3)).lt.tiny)then
         eta=sign(1.d0,p(3))*1.d8
      elseif(pt.lt.tiny) then   !: added this elseif
         eta=sign(1.d0,p(3))*1.d8
      else
         th=atan2(pt,p(3))
         eta=-log(tan(th/2.d0))
      endif
      end



      subroutine buildjets(iflag,mjets,rr,kt,eta,rap,phi,pjet)
c     arrays to reconstruct jets, radius parameter rr
      implicit none
      integer iflag,mjets
      real * 8  rr,kt(*),eta(*),rap(*),
     1     phi(*),pjet(4,*)
      include   'hepevt.h'
      include  'LesHouches.h'
      integer   maxtrack,maxjet
      parameter (maxtrack=2048,maxjet=2048)
      real * 8  ptrack(4,maxtrack),pj(4,maxjet)
      integer   jetvec(maxtrack),itrackhep(maxtrack)
      integer   ntracks,njets
      integer   j,k,mu,jb
      real * 8 r,palg,ptmin,pp,tmp
      logical islept
      external islept
C - Initialize arrays and counters for output jets
      do j=1,maxtrack
         do mu=1,4
            ptrack(mu,j)=0d0
         enddo
         jetvec(j)=0
      enddo      
      ntracks=0
      do j=1,maxjet
         do mu=1,4
            pjet(mu,j)=0d0
            pj(mu,j)=0d0
         enddo
      enddo
      if(iflag.eq.1) then
C     - Extract final state particles to feed to jet finder
         do j=1,nhep
            if (isthep(j).eq.1.and..not.islept(idhep(j))) then
               if(ntracks.eq.maxtrack) then
                  write(*,*) 'analyze: need to increase maxtrack!'
                  write(*,*) 'ntracks: ',ntracks
                  stop
               endif
               ntracks=ntracks+1
               do mu=1,4
                  ptrack(mu,ntracks)=phep(mu,j)
               enddo
               itrackhep(ntracks)=j
            endif
         enddo
      else
         do j=1,nup
            if (istup(j).eq.1.and..not.islept(idup(j))) then
               if(ntracks.eq.maxtrack) then
                  write(*,*) 'analyze: need to increase maxtrack!'
                  write(*,*) 'ntracks: ',ntracks
                  stop
               endif
               ntracks=ntracks+1
               do mu=1,4
                  ptrack(mu,ntracks)=pup(mu,j)
               enddo
               itrackhep(ntracks)=j
            endif
         enddo
      endif
      if (ntracks.eq.0) then
         mjets=0
         return
      endif
C --------------------------------------------------------------------- C
C - Inclusive jet pT and Y spectra are to be compared to CDF data:    - C    
C --------------------------------------------------------------------- C
C     R = 0.7   radius parameter
C     f = 0.75  overlapping fraction
c palg=1 is standard kt, -1 is antikt
      palg=1
      r=rr
      ptmin=20
      call fastjetppgenkt(ptrack,ntracks,r,palg,ptmin,pjet,njets,
     $                        jetvec)
      mjets=njets
      if(njets.eq.0) return
c check consistency
      do k=1,ntracks
         if(jetvec(k).gt.0) then
            do mu=1,4
               pj(mu,jetvec(k))=pj(mu,jetvec(k))+ptrack(mu,k)
            enddo
         endif
      enddo
      tmp=0
      do j=1,mjets
         do mu=1,4
            tmp=tmp+abs(pj(mu,j)-pjet(mu,j))
         enddo
      enddo
      if(tmp.gt.1d-4) then
         write(*,*) ' bug!'
      endif
C --------------------------------------------------------------------- C
C - Computing arrays of useful kinematics quantities for hardest jets - C
C --------------------------------------------------------------------- C
      do j=1,mjets
         kt(j)=sqrt(pjet(1,j)**2+pjet(2,j)**2)
         pp = sqrt(kt(j)**2+pjet(3,j)**2)
         eta(j)=0.5d0*log((pjet(4,j)+pjet(3,j))/(pjet(4,j)-pjet(3,j)))
         rap(j)=0.5d0*log((pjet(4,j)+pjet(3,j))/(pjet(4,j)-pjet(3,j)))
         phi(j)=atan2(pjet(2,j),pjet(1,j))
      enddo
      end


      function islept(j)
      implicit none
      logical islept
      integer j
      if(abs(j).ge.11.and.abs(j).le.15) then
         islept = .true.
      else
         islept = .false.
      endif
      end


      subroutine boostx(p_in,pt,ptt,p_out)
      implicit none
c--- Boost input vector p_in to output vector p_out using the same
c--- transformation as required to boost massive vector pt to ptt
      double precision p_in(4),pt(4),ptt(4),p_out(4),
     . p_tmp(4),beta(3),mass,gam,bdotp
      integer j
    
      mass=pt(4)**2-pt(1)**2-pt(2)**2-pt(3)**2  
      if (mass .lt. 0d0) then
        write(6,*) 'mass**2 .lt. 0 in boostx.f, mass**2=',mass,pt
        stop
      endif
      mass=dsqrt(mass)

c--- boost to the rest frame of pt
      gam=pt(4)/mass

      bdotp=0d0
      do j=1,3
        beta(j)=-pt(j)/pt(4)
        bdotp=bdotp+beta(j)*p_in(j)
      enddo
      p_tmp(4)=gam*(p_in(4)+bdotp)
      do j=1,3
        p_tmp(j)=p_in(j)+gam*beta(j)/(1d0+gam)*(p_in(4)+p_tmp(4))
      enddo     

c--- boost from rest frame of pt to frame in which pt is identical
c--- with ptt, thus completing the transformation          
      gam=ptt(4)/mass

      bdotp=0d0
      do j=1,3
        beta(j)=+ptt(j)/ptt(4)
        bdotp=bdotp+beta(j)*p_tmp(j)
      enddo
      p_out(4)=gam*(p_tmp(4)+bdotp)
      do j=1,3
        p_out(j)=p_tmp(j)+gam*beta(j)/(1d0+gam)*(p_out(4)+p_tmp(4))
      enddo

      return
      end
      



!     calculate coscm angle
      subroutine ZZcomangle(p1,p2,costh)
      implicit none
      double precision p1(4),p2(4),costh
c     -------
      double precision pZZcm(4),pz1_cm(4)

      pZZcm = 0d0
      pZZcm(4) = dsqrt( (p1(4)+p2(4))**2 -
     .     (p1(1)+p2(1))**2-
     .     (p1(2)+p2(2))**2-
     .     (p1(3)+p2(3))**2 )

      call boostx(p1,p1+p2,pZZcm,pz1_cm)
      costh=pz1_cm(3)/dsqrt(pz1_cm(1)**2+pz1_cm(2)**2+pz1_cm(3)**2)

      return
      end

!     angle between lepton 3 in Z r.f. and Z motion in ZZ rest frame
      subroutine ZZlepinZrf(pZ1,pZ2,pl,costh)
      implicit none
      double precision pZ1(4),pZ2(4),pl(4),costh
c     -------
      double precision pZrf(4),plrf(4),pZcm(4),pZZcm(4)

      
      !calculate Z1 momenta in ZZ C.O.M. frame
      pZZcm = 0d0
      pZZcm(4) = dsqrt( (pZ1(4)+pZ2(4))**2 -
     .     (pZ1(1)+pZ2(1))**2-
     .     (pZ1(2)+pZ2(2))**2-
     .     (pZ1(3)+pZ2(3))**2 )
      call boostx(pZ1,pZ1+pZ2,pZZcm,pZcm)

      !calculate lepton momenta in Z rest frame
      pZrf=0d0
      pZrf(4)=dsqrt(pZ1(4)**2-pZ1(1)**2-pZ1(2)**2-pZ1(3)**2)
      call boostx(pl,pz1,pZrf,plrf)
      
            
      costh = pZcm(1)*plrf(1)+pZcm(2)*plrf(2)+
     .     pZcm(3)*plrf(3)
      
      costh = costh/dsqrt(pZcm(1)**2+pZcm(2)**2+pZcm(3)**2)
      costh = costh/dsqrt(plrf(1)**2+plrf(2)**2+
     .     plrf(3)**2)

      return
      end

      subroutine ZZplane(pl1,pl2,pl3,pl4,cosph)
      implicit none
      double precision pl1(4),pl2(4),pl3(4),pl4(4),cosph
c     -------
      double precision pZZcm(4),plcm(4,4),l1xl2(4),l3xl4(4)

      pZZcm=0d0
      pZZcm(4) = dsqrt( (pl1(4)+pl2(4)+pl3(4)+pl4(4))**2 -
     .                  (pl1(1)+pl2(1)+pl3(1)+pl4(1))**2 -
     .                  (pl1(2)+pl2(2)+pl3(2)+pl4(2))**2 -
     .                  (pl1(3)+pl2(3)+pl3(3)+pl4(3))**2 )

      ! get lepton momenta in ZZ C.O.M. frame
      call boostx(pl1,pl1+pl2+pl3+pl4,pZZcm,plcm(1,:))
      call boostx(pl2,pl1+pl2+pl3+pl4,pZZcm,plcm(2,:))
      call boostx(pl3,pl1+pl2+pl3+pl4,pZZcm,plcm(3,:))
      call boostx(pl4,pl1+pl2+pl3+pl4,pZZcm,plcm(4,:))

      
      
!     calculate angle between l1l1bar and l2l2bar planes
!     cross product of l_3 and l_4
      l1xl2(1) = plcm(1,2)*plcm(2,3)-plcm(1,3)*plcm(2,2)
      l1xl2(2) = plcm(1,3)*plcm(2,1)-plcm(1,1)*plcm(2,3)
      l1xl2(3) = plcm(1,1)*plcm(2,2)-plcm(1,2)*plcm(2,1)
!     cross product of l_5 and l_6
      l3xl4(1) = plcm(3,2)*plcm(4,3)-plcm(3,3)*plcm(4,2)
      l3xl4(2) = plcm(3,3)*plcm(4,1)-plcm(3,1)*plcm(4,3)
      l3xl4(3) = plcm(3,1)*plcm(4,2)-plcm(3,2)*plcm(4,1)
      
      cosph=l1xl2(1)*l3xl4(1)+l1xl2(2)*l3xl4(2)+
     .     l1xl2(3)*l3xl4(3)
      
      cosph = cosph/dsqrt(l1xl2(1)**2+l1xl2(2)**2+
     .     l1xl2(3)**2)
      cosph = cosph/dsqrt(l3xl4(1)**2+l3xl4(2)**2+
     .     l3xl4(3)**2)

      return
      end
      
