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
      integer j,k,i
      real * 8 dy,dylep,dpt,dr
      character * 1 cnum(9)
      data cnum/'1','2','3','4','5','6','7','8','9'/
      integer maxjet
      parameter (maxjet=3)
      integer nptmin
      parameter (nptmin=1)
      character * 4 cptmin(nptmin)
      real * 8 ptminarr(nptmin)
c      data cptmin/  '-025',  '-050',  '-100'/
c      data ptminarr/   25d0,    50d0,   100d0/
      data cptmin/  '-0'/
      data ptminarr/   0d0/
      common/infohist/ptminarr,cnum,cptmin
      save /infohist/
      real * 8 Hmass,Hwidth,powheginput
      external powheginput

      call inihists

      dy=0.5d0
      dylep=0.4d0
      dpt=10d0
      dr=0.2d0

      Hmass = powheginput('hmass')
      Hwidth = powheginput('hwidth')

      
      do i=1,nptmin
c     total cross section sanity check
      call bookupeqbins('Njet'//cptmin(i),1d0,-0.5d0,5.5d0)

      call bookupeqbins('H-y'//cptmin(i),dy,-5d0,5d0)
      call bookupeqbins('H-eta'//cptmin(i),dy,-5d0,5d0)
      call bookupeqbins('H-pt'//cptmin(i),dpt,0d0,400d0)
c      call bookupeqbins('H-m'//cptmin(i),Hwidth,Hmass-20*Hwidth,
c     $     Hmass+20*Hwidth)
      call bookupeqbins('H-m'//cptmin(i),0.2d-2,124.98d0,125.020d0)

      call bookupeqbins('W-y'//cptmin(i),dy,-5d0,5d0)
      call bookupeqbins('W-eta'//cptmin(i),dy,-5d0,5d0)
      call bookupeqbins('W-pt'//cptmin(i),dpt,0d0,400d0)
      call bookupeqbins('W-m'//cptmin(i),dpt,0d0,200d0)

      call bookupeqbins('lept-eta'//cptmin(i),dylep,-4d0,4d0)
      call bookupeqbins('lept-pt'//cptmin(i),dpt,0d0,500d0)
      call bookupeqbins('miss-pt'//cptmin(i),dpt,0d0,500d0)

      call bookupeqbins('HW-y'//cptmin(i),dy,-5d0,5d0)
      call bookupeqbins('HW-eta'//cptmin(i),dy,-5d0,5d0)
      call bookupeqbins('HW-pt'//cptmin(i),dpt,0d0,400d0)
      call bookupeqbins('HW-ptzoom'//cptmin(i),2,1d0,151d0)
      call bookupeqbins('HW-ptzoom2'//cptmin(i),0.5,0d0,20d0)
      call bookupeqbins('HW-m'//cptmin(i),dpt,0d0,400d0)


      do j=1,maxjet
         call bookupeqbins('j'//cnum(j)//'-y'//cptmin(i),dy,-5d0,5d0)
         call bookupeqbins('j'//cnum(j)//'-eta'//cptmin(i),dy,-5d0,5d0)
         call bookupeqbins('j'//cnum(j)//'-pt'//cptmin(i),dpt,0d0,400d0)
         call bookupeqbins('j'//cnum(j)//'-ptzoom'//cptmin(i),
     $        2d0,1d0,151d0)
         call bookupeqbins('j'//cnum(j)//'-m'//cptmin(i),dpt,0d0,400d0) 
         call bookupeqbins('j'//cnum(j)//'-ptzoom2'//cptmin(i),
     $        0.5d0,0d0,20d0)
      enddo


      do j=1,maxjet-1
         do k=j+1,maxjet
            call bookupeqbins('j'//cnum(j)//'j'//cnum(k)//
     1           '-y'//cptmin(i),dy,-5d0,5d0)  
            call bookupeqbins('j'//cnum(j)//'j'//cnum(k)//
     1           '-eta'//cptmin(i),dy,-5d0,5d0)
            call bookupeqbins('j'//cnum(j)//'j'//cnum(k)//
     1           '-pt'//cptmin(i),dpt,0d0,400d0)
            call bookupeqbins('j'//cnum(j)//'j'//cnum(k)//
     1           '-m'//cptmin(i),dpt,0d0,400d0)  
         enddo
      enddo
  
 
      do j=1,maxjet-1
         do k=j+1,maxjet
            call bookupeqbins('j'//cnum(j)//'j'//cnum(k)//
     1           '-dy'//cptmin(i),dy,-5d0,5d0)  
            call bookupeqbins('j'//cnum(j)//'j'//cnum(k)//
     1           '-deta'//cptmin(i),dy,-5d0,5d0)
            call bookupeqbins('j'//cnum(j)//'j'//cnum(k)//
     1           '-delphi'//cptmin(i),pi/20,0d0,pi)
            call bookupeqbins('j'//cnum(j)//'j'//cnum(k)//
     1           '-dr'//cptmin(i),dr,0d0,10d0)  
         enddo
      enddo
  
      do j=1,maxjet-1
            call bookupeqbins('j'//cnum(j)//'lept'//
     1           '-dy'//cptmin(i),dy,-5d0,5d0)  
            call bookupeqbins('j'//cnum(j)//'lept'//
     1           '-deta'//cptmin(i),dy,-5d0,5d0)
            call bookupeqbins('j'//cnum(j)//'lept'//
     1           '-delphi'//cptmin(i),pi/20,0d0,pi)
            call bookupeqbins('j'//cnum(j)//'lept'//
     1           '-dr'//cptmin(i),dr,0d0,10d0)  
      enddo

    
c      do j=1,maxjet-1
c      do k=j+1,maxjet
c         call bookupeqbins('Hj'//cnum(j)//'-j'//cnum(k)//
c     1        '-dy'//cptmin(i),dy,-5d0,5d0)  
c         call bookupeqbins('Hj'//cnum(j)//'-j'//cnum(k)//
c     1        '-deta'//cptmin(i),dy,-5d0,5d0)
c         call bookupeqbins('Hj'//cnum(j)//'-j'//cnum(k)//
c     1        '-delphi'//cptmin(i),pi/20,0d0,pi)
c         call bookupeqbins('Hj'//cnum(j)//'-j'//cnum(k)//
c     1        '-dr'//cptmin(i),dr,0d0,20d0)  
c      enddo
c      enddo

c      if(maxjet.ge.3) then
c         call bookupeqbins('Hj1j2-j3-dy'//cptmin(i),dy,-5d0,5d0)  
c         call bookupeqbins('Hj1j2-j3-deta'//cptmin(i),dy,-5d0,5d0)
c         call bookupeqbins('Hj1j2-j3-delphi'//cptmin(i),pi/20,0d0,pi)
c         call bookupeqbins('Hj1j2-j3-dr'//cptmin(i),dr,0d0,20d0)
c      endif

c      do j=1,maxjet
c         call bookupeqbins('ptrel'//cnum(j)//cptmin(i),0.5d0,0d0,20d0)
c      enddo      
c$$$
c$$$      do j=1,maxjet
c$$$         call bookupeqbins('ptrel'//cnum(j)//'qqqq'//cptmin(i),
c$$$     $        0.5d0,0d0,20d0)
c$$$         call bookupeqbins('ptrel'//cnum(j)//'qqgg'//cptmin(i),
c$$$     $        0.5d0,0d0,20d0)
c$$$         call bookupeqbins('ptrel'//cnum(j)//'ggqq'//cptmin(i),
c$$$     $        0.5d0,0d0,20d0)
c$$$         call bookupeqbins('ptrel'//cnum(j)//'gggg'//cptmin(i),
c$$$     $        0.5d0,0d0,20d0)
c$$$         call bookupeqbins('ptrel'//cnum(j)//'qgqg'//cptmin(i),
c$$$     $        0.5d0,0d0,20d0)
c$$$      enddo      
c$$$

      enddo
      end
     
      subroutine analysis(dsig)
      implicit none
      real * 8 dsig
      include 'hepevt.h'
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_math.h' 
      include 'pwhg_rad.h' 
      include 'pwhg_flg.h'
      include 'LesHouches.h'
      logical ini
      data ini/.true./
      save ini
      integer   maxjet,mjets,njets,ntracks
      parameter (maxjet=2048)
      real * 8  ktj(maxjet),etaj(maxjet),rapj(maxjet),
     1    phij(maxjet),pj(4,maxjet),rr,ptrel(4)

      integer maxtrack
      parameter (maxtrack=2048)
      real * 8  ptrack(4,maxtrack)
      integer   jetvec(maxtrack),itrackhep(maxtrack)


      character * 1 cnum(9)
c      data cnum/'1','2','3','4','5','6','7','8','9'/
c      save cnum
      integer nptmin
      parameter (nptmin=1)
      character * 4 cptmin(nptmin)
      real * 8 ptminarr(nptmin)      
      common/infohist/ptminarr,cnum,cptmin
      save /infohist/
      integer j,k,i,jj
c     we need to tell to this analysis file which program is running it
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
      data WHCPRG/'NLO   '/
      integer ih,il,inu
      real * 8 ph(4),pl(4),pnu(4),pw(4)
      real * 8 httot,y,eta,pt,m
      real * 8 dy,deta,delphi,dr
      integer ihep
      real * 8 powheginput,dotp
      external powheginput,dotp
      real * 8 ptmin
      integer idvecbos,idl,idnu
      save idvecbos,idl,idnu
      integer maxnumlep
      parameter (maxnumlep=10)
      real * 8 pvl(4,maxnumlep),plep(4,maxnumlep)
      integer mu,ilep,ivl,nlep,nvl
      logical is_W
      real * 8 mV2,ptvb,mvb,ptlep,ptmin_fastkt,ptvl,R,ylep,yvb,yvl
      real * 8 Wmass,Wwidth,Wmasslow,Wmasshigh
      integer jpart, jjet


      if(dsig.eq.0) return

      if (flg_minlo) then
         flg_processid='HW'
      else
         include 'pwhg_processid.h'
      endif
         

      if (ini) then
         idvecbos=powheginput('idvecbos')
         if (idvecbos.eq.24) then
            idl=-11
            idnu=12
         else
            idl=11
            idnu=-12
         endif        
c         if ((WHCPRG.ne.'NLO   ').or.(WHCPRG.ne.'LHE   ')) then 
c     set values if analysis file is run by HERWIG and PYTHIA
c            Wmass = 80.398d0
c            Wwidth = 2.141d0
c            Wmasslow = (Wmass-10*Wwidth)
c            Wmasshigh = (Wmass+10*Wwidth)
c         endif      
c     write(*,*) '**************************************************'
c         write(*,*) '**************************************************'
c     write(*,*) '                ANALYSIS CUTS                     '
c     write(*,*) '**************************************************'
c         write(*,*) '**************************************************'
c     write(*,*)   Wmasslow,' < M_W < ',Wmasshigh
c     write(*,*) '**************************************************'
c     write(*,*) '**************************************************'
         ini=.false.
      endif

      ilep=0
      ih=0
      ivl=0

      if ((WHCPRG.eq.'NLO   ').or.(WHCPRG.eq.'LHE   ')) then 
         do ihep=1,nhep            
            if(idhep(ihep).eq.idl) then
               ilep=ihep
               do mu=1,4
                  plep(mu,1)=phep(mu,ihep)
               enddo
            elseif(idhep(ihep).eq.idnu) then
               ivl=ihep
               do mu=1,4
                  pvl(mu,1)=phep(mu,ihep)
               enddo              
            elseif(idhep(ihep).eq.25) then
               ih=ihep
               do mu=1,4
                  ph(mu)=phep(mu,ihep)
               enddo              
            endif
         enddo
      endif


c     Analysis after MC shower
      if((WHCPRG.eq.'HERWIG').or.(WHCPRG.eq.'PYTHIA')) then
c     Loop again over final state particles to find products of W decay, by
c     looking into the shower branchings.
         nlep=0
         nvl=0
         do ihep=1,nhep
c     works for POWHEG+HERWIG, POWHEG+PYHIA, HERWIG, PYTHIA and real in
c     MC@NLO
            if(idhep(ihep).eq.25.and.isthep(ihep).eq.1) then
               ph=phep(1:4,ihep)
               ih=ihep
            endif
            if (isthep(ihep).eq.1.and.(idhep(ihep).eq.idl .or.
     $           idhep(ihep).eq.idnu))
     1           then
               is_W = idhep(jmohep(1,jmohep(1,ihep))).eq.idvecbos .or.
     $         idhep(jmohep(1,jmohep(1,jmohep(1, ihep)))).eq.idvecbos
c               is_W = .true.
               if (is_W) then
c     find first decay product
                  if(idhep(ihep).eq.idl) then
                     ilep=ihep
                     nlep=nlep+1
c     find second decay product
                  elseif(idhep(ihep).eq.idnu) then
                     ivl=ihep
                     nvl=nvl+1
                  endif
               endif
            endif
         enddo
         if(nvl.ne.1.or.nlep.ne.1) then
            write(*,*) 'Problems with leptons from W decay'
c            write(*,*) 'PROGRAM ABORT'
            write(*,*) 'nvl= ',nvl, 'nlep= ',nlep
c            call exit(1)
            return
         endif
         do mu=1,4
            plep(mu,nlep)=phep(mu,ilep)
            pvl(mu,nvl)=phep(mu,ivl)
         enddo
      endif

      if (ilep*ih*ivl.eq.0) then
         write(*,*) 
     $        'ERROR... have NOT found the electron/neutrino/Higgs'
         return
      endif
         
c     change status of l vu and Higgs
      isthep(ilep)=10000
      isthep(ivl)=10000
      isthep(ih)=10000
      
      
c$$$      do i=1,maxnumlep
c$$$         lepvec(i) = 0
c$$$         vlvec(i) = 0
c$$$      enddo
c$$$
c$$$
c$$$      if (WHCPRG.eq.'PYTHIA') then
c$$$         nlep=0 
c$$$         nvl=0 
c$$$c     Loop over final state particles to find leptons 
c$$$         do ihep=1,nhep
c$$$            if (isthep(ihep).eq.1) then
c$$$               if(idhep(ihep).eq.idl) then
c$$$                  if (pass_lept_cuts(phep(1,ihep))) then 
c$$$                     nlep=nlep+1
c$$$                     lepvec(nlep)=ihep
c$$$                     do mu=1,4
c$$$                        plep(mu,nlep)=phep(mu,ihep)
c$$$                     enddo
c$$$                  endif
c$$$               elseif(idhep(ihep).eq.idnu) then
c$$$                  if (pass_lept_cuts(phep(1,ihep))) then 
c$$$                     nvl=nvl+1
c$$$                     vlvec(nvl)=ihep
c$$$                     do mu=1,4
c$$$                        pvl(mu,nvl)=phep(mu,ihep)
c$$$                     enddo
c$$$                  endif
c$$$               endif
c$$$            endif         
c$$$         enddo
c$$$
c$$$c correct here pvl and plep for collinear photons (in the electron case)
c$$$
c$$$         if (nlep*nvl.eq.0) then
c$$$c     no two opposite-sign leptons to reconstruct the W mass peak
c$$$            return
c$$$         endif
c$$$
c$$$         dist = 1d36
c$$$         ilep=0
c$$$         ivl=0
c$$$         do nleps=1,nlep
c$$$            do nvls=1,nvl
c$$$               do i=1,4
c$$$c in case of electron, we may need to add collinear photons here
c$$$                  pw(i)=plep(i,nleps)+pvl(i,nvls)
c$$$               enddo
c$$$               call getinvmass(pw,inv_mV)
c$$$               if (inv_mV.gt.Wmasslow.and.inv_mV.lt.Wmasshigh.and.
c$$$     #                 abs(inv_mV-Wmass).lt.dist) then
c$$$                  dist = abs(inv_mV-Wmass)
c$$$                  ilep = nleps
c$$$                  ivl = nvls
c$$$               endif
c$$$            enddo
c$$$         enddo
c$$$         
c$$$         if (ilep*ivl.eq.0) then
c$$$c     no reconstructed W mass peak in the experimental window
c$$$            return
c$$$         else            
c$$$            do i=1,4
c$$$               pw(i)=plep(i,ilep)+pvl(i,ivl)
c$$$            enddo        
c$$$         endif         
c$$$      endif


c     W momentum
      do mu=1,4
         pw(mu)=plep(mu,1) + pvl(mu,1)
      enddo
c      mV2 = pw(4)**2-pw(1)**2-pw(2)**2-pw(3)**2
c      write(*,*) '>>>>>>>>>>>>>>>>>>>>>>>>>>',sqrt(mV2)
c      ptvb=sqrt(pw(1)**2+pw(2)**2)

c      call getinvmass(pw,mvb)
c      call getrapidity(pw,yvb)
c      ptvl=sqrt(phep(1,ivl)**2+phep(2,ivl)**2)
c      call getrapidity(phep(1,ivl),yvl)
c      ptlep=sqrt(phep(1,ilep)**2+phep(2,ilep)**2)
c      call getrapidity(phep(1,ilep),ylep)

c$$$      call build_jets(njets,pjet)

c     set up arrays for jet finding
      do jpart=1,maxtrack
         do mu=1,4
            ptrack(mu,jpart)=0d0
         enddo
         jetvec(jpart)=0
      enddo      
      do jjet=1,maxjet
         do mu=1,4
            pj(mu,jjet)=0d0
         enddo
      enddo

      ntracks=0
      njets=0
c     Loop over final state particles to find jets 
      do ihep=1,nhep
         if (isthep(ihep).eq.1) then
           if (ntracks.eq.maxtrack) then
              write(*,*) 'Too many particles. Increase maxtrack.'//
     #             ' PROGRAM ABORTS'
              call exit(1)
           endif
c     copy momenta to construct jets 
           ntracks=ntracks+1
           do mu=1,4
              ptrack(mu,ntracks)=phep(mu,ihep)
           enddo
        endif
      enddo
      if (ntracks.eq.0) then
         return
      else
**********************************************************************
c     R = 0.7  radius parameter
c     f = 0.5  overlapping fraction
c.....run the clustering        
c      call fastjetsiscone(ptrack,ntracks,0.7d0,0.5d0,pjet,njets) 
************************************************************************
*     fastkt algorithm
**********************************************************************
c      R = 0.7  Radius parameter
c.....run the clustering 
         R = 0.7d0          
         ptmin_fastkt = 1d0
         call fastjetktwhich(ptrack,ntracks,ptmin_fastkt,R,
     $        pj,njets,jetvec) 
c     ... now we have the jets
      endif


c     Since ptminarr(1) is the smallest value, the following is correct
      if (flg_processid.eq.'HWJ') then
         if(njets.eq.0) return
      endif


c      if (WHCPRG.eq.'NLO   '.and. njets.eq.2) then
c         call getyetaptmass(pj(:,2),y,eta,pt,m)
c         if (pt.lt.10) then 
c            return
c         endif
c      endif
      


      do i=1,nptmin        
        
      if(njets.eq.0) then
         call filld('Njet'//cptmin(i),0d0,dsig)
      elseif(njets.eq.1) then
         call filld('Njet'//cptmin(i),1d0,dsig)
      elseif(njets.eq.2) then
         call filld('Njet'//cptmin(i),2d0,dsig)
      elseif(njets.eq.3) then
         call filld('Njet'//cptmin(i),3d0,dsig)
      elseif(njets.eq.4) then
         call filld('Njet'//cptmin(i),4d0,dsig)
      elseif(njets.eq.5) then
         call filld('Njet'//cptmin(i),5d0,dsig)
      else
c         write(*,*) ' Njet?',mjets
      endif


      mjets=min(njets,2)
  
c Higgs
      call getyetaptmass(ph,y,eta,pt,m)
      call filld('H-y'//cptmin(i),    y, dsig)
      call filld('H-eta'//cptmin(i),eta, dsig)
      call filld('H-pt'//cptmin(i),  pt, dsig)
      call filld('H-m'//cptmin(i), m, dsig)
c W
      call getyetaptmass(pw,y,eta,pt,m)
      call filld('W-y'//cptmin(i),    y, dsig)
      call filld('W-eta'//cptmin(i),eta, dsig)
      call filld('W-pt'//cptmin(i),  pt, dsig)
      call filld('W-m'//cptmin(i), m, dsig)
c lepton
      call getyetaptmass(plep(:,1),y,eta,pt,m)
      call filld('lept-eta'//cptmin(i),eta, dsig)
      call filld('lept-pt'//cptmin(i),  pt, dsig)
c neutrino
      call getyetaptmass(pvl(:,1),y,eta,pt,m)
      call filld('miss-pt'//cptmin(i),  pt, dsig)
c HW
      call getyetaptmass(ph+pw,y,eta,pt,m)
      call filld('HW-y'//cptmin(i),    y, dsig)
      call filld('HW-eta'//cptmin(i),eta, dsig)
      call filld('HW-pt'//cptmin(i),  pt, dsig)
      call filld('HW-ptzoom'//cptmin(i),  pt, dsig)
      call filld('HW-ptzoom2'//cptmin(i),  pt, dsig)
      call filld('HW-m'//cptmin(i), m, dsig)

c jets
      do j=1,mjets
         call getyetaptmass(pj(:,j),y,eta,pt,m)
         call filld('j'//cnum(j)//'-y'//cptmin(i),     y, dsig)
         call filld('j'//cnum(j)//'-eta'//cptmin(i), eta, dsig)
         call filld('j'//cnum(j)//'-pt'//cptmin(i),   pt, dsig)
         call filld('j'//cnum(j)//'-ptzoom'//cptmin(i),   pt, dsig)
         call filld('j'//cnum(j)//'-ptzoom2'//cptmin(i),   pt, dsig)
         call filld('j'//cnum(j)//'-m'//cptmin(i),     m, dsig)
c         call filld('ptrel'//cnum(j)//cptmin(i),ptrel(j), dsig)         
      enddo



      do j=1,mjets
         do k=j+1,mjets
            call getyetaptmass(pj(:,j)+pj(:,k),y,eta,pt,m)
            call filld('j'//cnum(j)//'j'//cnum(k)//'-y'//cptmin(i),
     $           y, dsig)
            call filld('j'//cnum(j)//'j'//cnum(k)//'-eta'//cptmin(i),
     $           eta, dsig)
            call filld('j'//cnum(j)//'j'//cnum(k)//'-pt'//cptmin(i),
     $           pt, dsig)
            call filld('j'//cnum(j)//'j'//cnum(k)//'-m'//cptmin(i), 
     $           m, dsig)
         enddo
      enddo

      
      do j=1,mjets
         do k=j+1,mjets
            call deltaplot(pj(:,j),pj(:,k),dsig,
     1           'j'//cnum(j)//'j'//cnum(k),cptmin(i))
         enddo
      enddo

      do j=1,mjets
         call deltaplot(pj(:,j),plep(:,1),dsig,
     1        'j'//cnum(j)//'lept',cptmin(i))
      enddo
      
      
      enddo
      end




c      subroutine yetaptmassplot(p,dsig,prefix)
c      implicit none
c      real * 8 p(4),dsig
c      character *(*) prefix
c      real * 8 y,eta,pt,m
c      call getyetaptmass(p,y,eta,pt,m)
c      call filld(prefix//'-y',y,dsig)
c      call filld(prefix//'-eta',eta,dsig)
c      call filld(prefix//'-pt',pt,dsig)
c      call filld(prefix//'-m',m,dsig)
c      end

      subroutine deltaplot(p1,p2,dsig,prefix,postfix)
      implicit none
      real * 8 p1(4),p2(4),dsig
      character *(*) prefix,postfix
      real * 8 dy,deta,delphi,dr
      call getdydetadphidr(p1,p2,dy,deta,delphi,dr)
      call filld(prefix//'-dy'//postfix,dy,dsig)
      call filld(prefix//'-deta'//postfix,deta,dsig)
      call filld(prefix//'-delphi'//postfix,delphi,dsig)
      call filld(prefix//'-dr'//postfix,dr,dsig)
      end

      subroutine getyetaptmass(p,y,eta,pt,mass)
      implicit none
      real * 8 p(4),y,eta,pt,mass,pv
      real *8 tiny
      parameter (tiny=1.d-5)
      y=0.5d0*log((p(4)+p(3))/(p(4)-p(3)))
      pt=sqrt(p(1)**2+p(2)**2)
      pv=sqrt(pt**2+p(3)**2)
      if(pt.lt.tiny)then
         eta=sign(1.d0,p(3))*1.d8
      else
         eta=0.5d0*log((pv+p(3))/(pv-p(3)))
      endif
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


      subroutine sortbypt(n,iarr)
      implicit none
      integer n,iarr(n)
      include 'hepevt.h'
      integer j,k
      real * 8 tmp,pt(nmxhep)
      logical touched
      do j=1,n
         pt(j)=sqrt(phep(1,iarr(j))**2+phep(2,iarr(j))**2)
      enddo
c bubble sort
      touched=.true.
      do while(touched)
         touched=.false.
         do j=1,n-1
            if(pt(j).lt.pt(j+1)) then
               k=iarr(j)
               iarr(j)=iarr(j+1)
               iarr(j+1)=k
               tmp=pt(j)
               pt(j)=pt(j+1)
               pt(j+1)=tmp
               touched=.true.
            endif
         enddo
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

      function get_ptrel(pin,pjet)
      implicit none
      real * 8 get_ptrel,pin(0:3),pjet(0:3)
      real * 8 pin2,pjet2,cth2,scalprod
      pin2  = pin(1)**2 + pin(2)**2 + pin(3)**2
      pjet2 = pjet(1)**2 + pjet(2)**2 + pjet(3)**2
      scalprod = pin(1)*pjet(1) + pin(2)*pjet(2) + pin(3)*pjet(3)
      cth2 = scalprod**2/pin2/pjet2
      get_ptrel = sqrt(pin2*abs(1d0 - cth2))
      end

      subroutine pwhgfinalopshist
      end
