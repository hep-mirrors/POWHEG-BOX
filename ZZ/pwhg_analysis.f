c  The next subroutines, open some histograms and prepare them 
c      to receive data 
c  You can substitute these  with your favourite ones
c  init   :  opens the histograms
c  topout :  closes them
c  pwhgfill  :  fills the histograms with data

      subroutine init_hist
      implicit none
      include  'LesHouches.h'
      include 'pwhg_math.h'

      call inihists

c     total cross section sanity check
      call bookupeqbins('total',1d0,0d0,3d0)

      call bookupeqbins('mt(Z)',1d0,50d0,100d0)

      call bookupeqbins('m(Z)',1d0,60d0,120d0)

      call bookupeqbins('pt(l+)',1d0,0d0
     $     ,100d0)

      call bookupeqbins('eta(l+)',0.2d0,-3d0,3d0)

      call bookupeqbins('pt(Z)',1d0,0d0,100d0)

      call bookupeqbins('y(Z)',0.2d0,-3d0,3d0)

      call bookupeqbins('pt(ZZ)',1d0,0d0,300d0)

      call bookupeqbins('M(ZZ)',5d0,20d0,500d0)

      call bookupeqbins('eta(ZZ)',0.2d0,-4d0,4d0)

      call bookupeqbins('y(ZZ)',0.2d0,-4d0,4d0)

c MCFM standard plots
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
      call  bookupeqbins('dS/dr56',0.1d0,0d0,4d0)
      call  bookupeqbins('dS/dphi56',0.314d0,0d0,3.14d0)
      call  bookupeqbins('dS/dm56a',5d0,0d0,200d0)
      call  bookupeqbins('dS/dm56b',50d0,0d0,2000d0)
      call  bookupeqbins('dS/deta5-eta6',0.2d0,-6d0,6d0)
      call  bookupeqbins('dS/deta7',0.2d0,-4d0,4d0)
      call  bookupeqbins('dS/dpt7a',5d0,0d0,100d0)
      call  bookupeqbins('dS/dpt7b',20d0,0d0,600d0)
      call  bookupeqbins('dS/dr57',0.1d0,0d0,4d0)
      call  bookupeqbins('dS/dr67',0.1d0,0d0,4d0)
      call  bookupeqbins('dS/dm567',2d0,108d0,308d0)
      call  bookupeqbins('dS/deta diff',0.2d0,-6d0,6d0)

c Old ZZ plots
      call  bookupeqbins('OZpt',2d0,0d0,200d0)
      call  bookupeqbins('OZlogpt',0.05d0,-1d0,3d0)
      call  bookupeqbins('OZy',0.2d0,0d0,4d0)
      call  bookupeqbins('OZZdeltay',0.2d0,0d0,4d0)
      call  bookupeqbins('OZZdeltaeta',0.2d0,0d0,4d0)
      call  bookupeqbins('OZZmass',10d0,0d0,1000d0)
      call  bookupeqbins('OZZy',0.2d0,0d0,4d0)
      call  bookupeqbins('OZZpt',2d0,0d0,200d0)
      call  bookupeqbins('OZZlg10pt',0.05d0,-0.5d0,3d0)
      call  bookupeqbins('OZZazi',pi/20,0d0,pi)
      call  bookupeqbins('OZZlpimazi',0.05d0,-4d0,1d0)
      call  bookupeqbins('OZZdr',0.25d0,0d0,10d0)
      call  bookupeqbins('OZZptB',0.5d0,0d0,50d0)

      end
     
      subroutine analysis(dsig)
      implicit none
      real * 8 dsig
      include 'hepevt.h'
      include 'pwhg_math.h' 
      include  'LesHouches.h'
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
      integer vdecaytemp1,vdecaytemp2
      real * 8 httot,y,eta,pt,mass,dy,deta,dphi,dr,eta5,eta6,eta7,etadif
      integer i3,i4,i5,i6,j
      real * 8 ptz1(4),ptz2(4)

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
         vdecaytemp=lprup(1)-10000 ! Z decay product, with positive id

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

      vdecaytemp1=11
      vdecaytemp2=13


      if((WHCPRG.eq.'NLO   ').or.(WHCPRG.eq.'LHE   ')) then
c     find Z decay products
         do ihep=1,nhep
            if(isthep(ihep).eq.1) then
               if(idhep(ihep).eq.vdecaytemp1) then
                  i3=ihep
               elseif(idhep(ihep).eq.-vdecaytemp1) then
                  i4=ihep
               elseif(idhep(ihep).eq.vdecaytemp2) then
                  i5=ihep
               elseif(idhep(ihep).eq.-vdecaytemp2) then
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

      passcuts = .true.

      if (pt_lminus.lt.15d0) passcuts = .false.
      if (pt_lplus.lt.15d0) passcuts = .false.
      if (pt_muminus.lt.15d0) passcuts = .false.
      if (pt_muplus.lt.15d0) passcuts = .false.

      if (abs(eta_lminus).gt.2.5d0) passcuts = .false.
      if (abs(eta_lplus).gt.2.5d0) passcuts = .false.
      if (abs(eta_muminus).gt.2.5d0) passcuts = .false.
      if (abs(eta_muplus).gt.2.5d0) passcuts = .false.


      if (passcuts) then



c     invariant mass, rapidity, pt of e^+e^- system
      call getyetaptmass(phep(:,i3)+phep(:,i4),yv,eta,ptv,mv)

c     azimuthal separation between e^+e^-
      call getdydetadphidr(phep(:,i3),phep(:,i4),dy,deta,delphi,dr)
c     transverse mass of e^+e^- system
      mt_v=sqrt(2*pt_lplus*pt_lminus*(1d0-dcos(delphi)))





C     ***********TM CUTS HERE



!      write(*,*)'pts',pt_lminus,pt_lplus,pt_muminus,pt_muplus
!      write(*,*)'etas',eta_lminus,eta_lplus,eta_muminus,eta_muplus
!      write(*,*)'PASSCUTS',passcuts




c     total sigma
      call filld('total',1.5d0,dsig)

c     transverse mass of the charged lepton system
      call filld('mt(Z)',mt_v,dsig)

c     invariant mass of the charged lepton system
      call filld('m(Z)',mv,dsig)

c     pt(l+)
      call filld('pt(l+)',pt_lplus,dsig)

c     eta(l+)
      call filld('eta(l+)',eta_lplus,dsig)

c     pt(v)
      call filld('pt(Z)',ptv,dsig)

c     y(v)
      call filld('y(Z)',yv,dsig)

      endif !END IF PASSCUTS

      call getyetaptmass(phep(:,i3)+phep(:,i4)+phep(:,i5)+phep(:,i6),
     1     y,eta,pt,mass)
      call filld('pt(ZZ)',pt,dsig)
      call filld('M(ZZ)',mass,dsig)
      call filld('eta(ZZ)',eta,dsig)
      call filld('y(ZZ)',y,dsig)


c MCFM  plots
      passcuts=.true.
      call getyetaptmass(phep(:,i3)+phep(:,i4),y,eta,pt,mass)
      passcuts=passcuts.and.mass.gt.20.and.mass.lt.14000
      call getyetaptmass(phep(:,i5)+phep(:,i6),y,eta,pt,mass)
      passcuts=passcuts.and.mass.gt.20.and.mass.lt.14000

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
     1           y,eta,pt,mass)
            call filld('dS/dm567',mass,dsig)
            etadif = eta7-(eta6+eta5)/2d0 
            call filld('dS/deta diff',etadif,dsig)
         endif
      endif

c Old ZZ plots
      ptz1=phep(1:4,i3)+phep(1:4,i4)
      ptz2=phep(1:4,i5)+phep(1:4,i6)
      call getyetaptmass(ptz1,y,eta,pt,mass)
      call filld('OZpt',pt,dsig/2)
      call filld('OZlogpt',log10(pt),dsig/2)
      call filld('OZy',y,dsig/2)
      call getyetaptmass(ptz2,y,eta,pt,mass)
      call filld('OZpt',pt,dsig/2)
      call filld('OZlogpt',log10(pt),dsig/2)
      call filld('OZy',y,dsig/2)
      call getdydetadphidr(ptz1,ptz2,dy,deta,dphi,dr)
      call filld('OZZdeltay',abs(dy),dsig)
      call filld('OZZdeltaeta',abs(deta),dsig)
      call filld('OZZazi',dphi,dsig)
      call filld('OZZlpimazi',log10(abs(pi-dphi)),dsig)
      call filld('OZZdr',dr,dsig)
      call getyetaptmass(ptz1+ptz2,y,eta,pt,mass)
      call filld('OZZmass',mass,dsig)
      call filld('OZZy',y,dsig)
      call filld('OZZpt',pt,dsig)
      call filld('OZZptB',pt,dsig)
      call filld('OZZlg10pt',log10(pt),dsig)
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
