c     The next subroutines, open some histograms and prepare them 
c     to receive data 
c  You can substitute these  with your favourite ones
c  init   :  opens the histograms
c  topout :  closes them
c  pwhgfill  :  fills the histograms with data

      subroutine init_hist
      implicit none
      include '../pwhg_book.h'
      real * 8 pi,pi2
      parameter(pi = 3.141592653589793D0, pi2 = 9.869604401089358D0)
      integer diag
c     binsize
      real * 8 bsz(100)
      common/pwhghistcommon/bsz

      call pwhginihist

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     CDF
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      diag=0

      diag=diag+1
      bsz(diag)=5d0
      call pwhgbookup(diag,'CDF jetclu ptj1 >=1jet','LOG',
     $     bsz(diag),20d0,350d0)

      diag=diag+1
      bsz(diag)=5d0
      call pwhgbookup(diag,'CDF jetclu ptj2 >=2jet','LOG',
     $     bsz(diag),20d0,190d0)

      diag=diag+1
      bsz(diag)=5d0
      call pwhgbookup(diag,'CDF jetclu ptj3 >=3jet','LOG',
     $     bsz(diag),20d0,80d0)

      diag=diag+1
      bsz(diag)=5d0
      call pwhgbookup(diag,'CDF jetclu ptj4 >=4jet','LOG',
     $     bsz(diag),20d0,35d0)

      diag=5

      bsz(diag)=1d0
      call pwhgbookup(diag,'CDF jetclu total >=njet','LOG',
     $     bsz(diag),0.5d0,4.5d0)

      end

      
     
      subroutine analysis(dsig)
      implicit none
      real * 8 dsig
      real * 8 pi
      parameter(pi = 3.141592653589793D0)
      include '../include/hepevt.h'
c arrays to reconstruct jets
      integer maxtrack,maxjet
      parameter (maxtrack=2048,maxjet=2048)
      real *8 pjet(4,maxjet)
      real *8 ktjet(maxjet)
      integer j,njets,mu,ihep,ilep,ivl,nlep,nvl
      real *8 plep(4),pv(4)
      real *8 Et_e,Et_v,eta_e,dphi_ev,Mt_W,p_e,p_v,Et_j,eta_j,phi_e,
     $     phi_v,pt_e,pt_v,y_e,y_v
      real *8 CDF_Et_e,CDF_eta_e,CDF_Et_v,CDF_Mt_W,
     $     CDF_Et_j,CDF_eta_j,CDF_dR_je
      integer foundhardjets,jj(maxjet),nregjet
      integer diag


      character *6 WHCPRG
      common/cWHCPRG/WHCPRG
      data WHCPRG/'POWHEG'/
c     binsize
      real *8 bsz(100)
      common/pwhghistcommon/bsz
      character *20 process
      real * 8 deltaphi,rsep_azi_eta
      external deltaphi,rsep_azi_eta

c     find electron and neutrino from W (needed for the CDF analysis)
      nlep=0
      nvl=0
      if(WHCPRG.eq.'POWHEG') then
         do ihep=1,nhep
            if(isthep(ihep).eq.1) then
               if(abs(idhep(ihep)).eq.11) then
                  ilep=ihep
                  nlep=nlep+1
               elseif(abs(idhep(ihep)).eq.12) then
                  ivl=ihep
                  nvl=nvl+1
               endif
            endif
         enddo
      elseif(WHCPRG.eq.'PYTHIA') then
         do ihep=1,nhep
            if(isthep(ihep).eq.1) then
               if((abs(idhep(ihep)).eq.11).and.
     $              (abs(idhep(jmohep(1,jmohep(1,ihep)))).eq.24)) then
                  ilep=ihep
                  nlep=nlep+1
               elseif((abs(idhep(ihep)).eq.12).and.
     $               (abs(idhep(jmohep(1,jmohep(1,ihep)))).eq.24)) then
                  ivl=ihep
                  nvl=nvl+1
               endif
            endif
         enddo
         if(nvl.ne.1.or.nlep.ne.1) then
            write(*,*) 'Problems with leptons from W decay: ',nlep,nvl
            write(*,*) 'SKIP EVENT'
            goto 666            ! reject event
         endif
      elseif (WHCPRG.eq.'HERWIG') then
         write(*,*) 'HERWIG analysis not implemented'
         call exit(1)
      else
         write(*,*) 'Wrong WHCPRG: ',WHCPRG
         call exit(1)
      endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     CDF analysis (arXiv:0711.4044)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     compute electron and neutrino relevant variables
      do mu=1,4
         plep(mu)=phep(mu,ilep)
         pv(mu)=phep(mu,ivl)
      enddo
      call getktyphi(1,plep,pt_e,y_e,phi_e)
      call getktyphi(1,pv,  pt_v,y_v,phi_v)
      p_e=sqrt(plep(1)**2+plep(2)**2+plep(3)**2)
      p_v=sqrt(pv(1)**2+pv(2)**2+pv(3)**2)
      Et_e=plep(4)*pt_e/p_e
      Et_v=pv(4)  *pt_v/p_v
      dphi_ev = abs(phi_e-phi_v)
c     make sure it is below pi
      if (dphi_ev.gt.pi) then
         dphi_ev = 2*pi-dphi_ev
      endif
      if (dphi_ev.lt.0 .or. dphi_ev.gt.pi) then
         write(*,*) ' problem in dphi_ev: ',dphi_ev
      endif
      call getpseudorapidity(plep,eta_e)
      Mt_w=sqrt(2.*Et_e*Et_v*(1.-cos(dphi_ev)))
      
c     'leptonic cuts'
      CDF_Et_e  = 20d0
      CDF_eta_e = 1.1
      CDF_Et_v  = 30d0
      CDF_Mt_W  = 20d0
      
      if((Et_e.lt.CDF_Et_e).or.
     $     (abs(eta_e).gt.CDF_eta_e).or.
     $     (Et_v.lt.CDF_Et_v).or.
     $     (Mt_w.lt.CDF_Mt_W)) then
         goto 666               ! reject event
      endif
      
c     if 'leptonic' cuts passed, build jets:
c     here reconstruct jets with CDF jetclu algo
      process="CDF jetclu"
c     build jets
      call buildjets(njets,pjet,process)
c     sort jet in decreasing Et (by finding their position
c     in pjet matrix)
      call find_hardest_jets(njets,pjet,njets,'ET',
     $     foundhardjets,jj,ktjet)
      
c     CDF plots dsigma/dEt_j.
      CDF_Et_j  = 20d0
      CDF_eta_j = 2d0
      CDF_dR_je = 0.000001 !!!!!0.52d0
      
c     reject event unless all jets that pass the jet cuts
c     are far from the charged lepton. Meanwhile,
c     also count the number of jets that pass the jet cuts.
      nregjet=0
      do j=1,njets
         Et_j=ktjet(jj(j))
         call getpseudorapidity(pjet(1,jj(j)),eta_j)
         if((Et_j.ge.CDF_Et_j).and.
     $        (abs(eta_j).le.CDF_eta_j)) then
            if(rsep_azi_eta(plep,pjet(1,jj(j))).le.CDF_dR_je) then
               goto 666         ! reject event
            else
               nregjet=nregjet+1
            endif
         endif
      enddo
      
      if(nregjet.eq.0) then
c     this happens if no jets pass the (Et,eta) cut
         goto 666               ! reject event
      elseif((nregjet.ge.1).and.(nregjet.le.4)) then
         diag=nregjet
      else
         diag=4
      endif
      call pwhgfill(diag,ktjet(jj(diag)),dsig/bsz(diag))
      
c$$$         print*, diag
c$$$         print*, ktjet(1),ktjet(2),ktjet(3),ktjet(4)
c$$$         print*, jj(1),jj(2),jj(3),jj(4)

         
c     CDF plots sigma(>= n jet)
      CDF_Et_j  = 25d0
c$$$  CDF_eta_j = 2d0
c$$$  CDF_dR_je = 0.52d0
      if(ktjet(jj(diag)).ge.CDF_Et_j) then
         call pwhgfill(5,dble(diag),dsig/bsz(5))
      endif
      
 666  continue
      end


      subroutine buildjets(njets,pjet,process)
c     arrays to reconstruct jets
      implicit none
      include '../include/hepevt.h'
      integer njets
      real * 8 pjet(4,*)
      character * 20 process
      integer maxtrack,maxjet
      parameter (maxtrack=2048,maxjet=2048)
      real *8 ptrack(4,maxtrack)
      integer jetvec(maxtrack)
      integer ihep,ntracks,jpart,jjet,mu
      real * 8 found
      real *8 r,ptmin,f

c     set up arrays for jet finding
      do jpart=1,maxtrack
         do mu=1,4
            ptrack(mu,jpart)=0d0
         enddo
         jetvec(jpart)=0
      enddo      
      do jjet=1,maxjet
         do mu=1,4
            pjet(mu,jjet)=0d0
         enddo
      enddo

      found=0
      ntracks=0
      njets=0

c     loop over final state particles to find valid tracks
      do ihep=1,nhep
c     exclude leptons, gauge and higgs bosons, but include gluons
         if ((isthep(ihep).eq.1).and.
     1        (((abs(idhep(ihep)).le.10).or.(abs(idhep(ihep)).ge.40))
     2        .or.(abs(idhep(ihep)).eq.21))) then
            if(ntracks.eq.maxtrack) then
               write(*,*)
     $              'analyze: too many particles, increase maxtrack'
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
         njets=0
         return
      endif
      if (process.eq."CDF jetclu") then
*******************************************
c     CDF jetclu
*******************************************
         R     = 0.4d0
         f     = 0.75d0
         ptmin = 0.1d0
         call fastjetCDFJetClu(ptrack,ntracks,R,ptmin,f,
     $        pjet,njets,jetvec)
      else
         write(*,*) 'JET ANALYSIS TO USE UNKNOWN:',process
         call exit(1)
      endif
         
      end

      subroutine getinvmass(p,m)
      implicit none
      real * 8 p(4),m
      real * 8 m2
      m2 = p(4)**2-p(1)**2-p(2)**2-p(3)**2
      if (m2.ge.0d0) then
         m = sqrt(abs(m2))
      else
         m = -sqrt(abs(m2))
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



      function getrapidity0(p)
      implicit none
      real * 8 p(0:3),getrapidity0
      getrapidity0=0.5d0*log((p(0)+p(3))/(p(0)-p(3)))
      end
      subroutine getrapidity(p,y)
      implicit none
      real * 8 p(4),y
      y=0.5d0*log((p(4)+p(3))/(p(4)-p(3)))
      end

      function pseudorapidity(p)
      implicit none
      real * 8 p(0:3),pseudorapidity
      real * 8 mod, costh
      mod = sqrt(p(1)**2+p(2)**2+p(3)**2)
      costh = p(3)/mod
      pseudorapidity=0.5*log((1+costh)/(1-costh))
      end



c     calculate the separation in the lego plot between the two momenta p1 and p2 
c     in azi and rapidity
      function rsep_azi_y(p1,p2)
      implicit none
      real * 8 pi,pi2
      parameter(pi = 3.141592653589793D0, pi2 = 9.869604401089358D0)
      real * 8 rsep_azi_y,p1(4),p2(4)
      real * 8 y1,phi1,y2,phi2,kt1,kt2
      real * 8 delphi

      call getktyphi(1,p1,kt1,y1,phi1)
      call getktyphi(1,p2,kt2,y2,phi2)

      delphi = abs(phi1-phi2)
c make sure it is below 2 pi
c      delphi=delphi-2*pi*int(delphi/(2*pi))
      if (delphi.gt.pi) then
         delphi = 2*pi-delphi
      endif
      if (delphi.lt.0 .or. delphi.gt.pi) then
         print*,' problem in rsep_azi_y. delphi = ',delphi
      endif
      rsep_azi_y = sqrt( (y1-y2)**2 + delphi**2 )
      end

      function azi(p)
      implicit none
      real * 8 pi,pi2
      parameter(pi = 3.141592653589793D0, pi2 = 9.869604401089358D0)
      real * 8 azi,p(0:3)
      azi = atan(p(2)/p(1))
      if (p(1).lt.0d0) then
         if (azi.gt.0d0) then               
            azi = azi - pi
         else
            azi = azi + pi
         endif
      endif    
      end






      subroutine getktyphi(njets,pjet,ktjet,yjet,phijet)
      implicit none
      integer njets
      real * 8 pjet(4,njets),ktjet(njets),yjet(njets),phijet(njets)
      integer j
      do j=1,njets
         ktjet(j)=sqrt(pjet(1,j)**2+pjet(2,j)**2)        
         yjet(j)=0.5d0*log((pjet(4,j)+pjet(3,j))/(pjet(4,j)-pjet(3,j)))
         phijet(j)=atan2(pjet(2,j),pjet(1,j))
      enddo
      end
      

c     calculate the separation in the lego plot between the two momenta p1 and p2 
c     in azi and pseudorapidity
      function rsep_azi_eta(p1,p2)
      implicit none
      real * 8 rsep_azi_eta,p1(4),p2(4)
      real * 8 pi,pi2
      parameter(pi = 3.141592653589793D0, pi2 = 9.869604401089358D0)
      real * 8 y1,phi1,y2,phi2,kt1,kt2,eta1,eta2
      real * 8 delphi

      call getktyphi(1,p1,kt1,y1,phi1)
      call getktyphi(1,p2,kt2,y2,phi2)

      delphi = abs(phi1-phi2)
c make sure it is below 2 pi
c      delphi=delphi-2*pi*int(delphi/(2*pi))
      if (delphi.gt.pi) then
         delphi = 2*pi-delphi
      endif
      if (delphi.lt.0 .or. delphi.gt.pi) then
         print*,' problem in rsep_azi_eta. delphi = ',delphi
      endif
      call getpseudorapidity(p1,eta1)
      call getpseudorapidity(p2,eta2)
      rsep_azi_eta = sqrt( (eta1-eta2)**2 + delphi**2 )
      end



      subroutine getpseudorapidity(p,eta)
      implicit none
      real * 8 p(1:4),eta
      real * 8 mod, costh
      mod = sqrt(p(1)**2+p(2)**2+p(3)**2)
      costh = p(3)/mod
      if (costh.eq.1d0) then
         eta=1d+30
      else
         eta=0.5*log(abs((1+costh)/(1-costh)))
      endif
      end



      function deltaphi(phi1,phi2)
      implicit none
      real * 8 deltaphi,phi1,phi2
      real * 8 pi
      parameter(pi = 3.141592653589793D0)
      deltaphi = abs(phi1-phi2)
      deltaphi=deltaphi-2*pi*int(deltaphi/(2*pi))
      if (deltaphi.gt.pi) then
         deltaphi = 2*pi-deltaphi
      endif
      if (deltaphi.lt.0 .or. deltaphi.gt.pi) then
         print*,' problem in rsep_azi_y. deltaphi = ',deltaphi
      endif      
      end



c     find the first "nhardjets" hardest jets in pjet (that contains njets)
c     and return their position.
c     criterium = "ET" or "PT": order them according to their 
c     transverse energy or momentum 
c     foundhardjets is the number of found hard jets (.le.nhardjets)
c     jj(1) is the position of the first jet, jj(2) the position of the 
c     second one...
c     ptj(1) = pt (or ET) of the first jet, etc
      subroutine find_hardest_jets(njets,pjet,nhardjets,criterium,
     #     foundhardjets,jj,ptj)
      implicit none
      integer njets
      real *8 pjet(4,njets) 
      integer nhardjets,jj(nhardjets)
      real * 8 ptj(nhardjets),pt,modp
      integer ijet,hjet,foundhardjets,i
      character * 2 criterium
      logical is_i_in_array
      external is_i_in_array
      
      if (.not.(criterium.eq.'ET'.or.criterium.eq.'PT')) then
         write(*,*) 'WRONG criterium in find_hardest_jets: ',criterium
         call exit(10)
      endif
      if (njets.eq.0) then
c         write(*,*) 'WARNING!!!!!!!!!!!  EMPTY  PJET ARRAY'
         foundhardjets=0
         return
      endif

      do hjet=1,nhardjets
         jj(hjet)=0d0
         ptj(hjet)=0d0
      enddo
      foundhardjets=1
      do ijet=1,njets
         pt=sqrt(pjet(1,ijet)**2 + pjet(2,ijet)**2)
         if (criterium.eq.'ET') then
c     pt is then the transverse energy
            modp = sqrt(pjet(1,ijet)**2+pjet(2,ijet)**2+pjet(3,ijet)**2)
            pt = pjet(4,ijet) * pt/modp
         endif
         do hjet=1,min(foundhardjets,nhardjets)
            if (pt.gt.ptj(hjet).and.
     $           .not.is_i_in_array(nhardjets,ijet,jj)) then
               foundhardjets = foundhardjets + 1
               do i=nhardjets,hjet+1,-1
                  ptj(i)=ptj(i-1)
                  jj(i)=jj(i-1)
               enddo
               ptj(hjet)=pt
               jj(hjet)=ijet
            endif
         enddo
      enddo
c     set number of jets found
      foundhardjets = min(foundhardjets-1,nhardjets)
      end

      function is_i_in_array(nhardjets,i,jj)
      implicit none
      logical is_i_in_array
      integer nhardjets,i,jj(nhardjets)
      integer j
      is_i_in_array = .false.
      do j=1,nhardjets
         if (i.eq.jj(j)) then
            is_i_in_array = .true.
            return
         endif
      enddo
      end



      subroutine getEt(p,Et)
      implicit none
      real * 8 p(4),Et
      real * 8 pt,modp
      pt=sqrt(p(1)**2 + p(2)**2)
      modp = sqrt(p(1)**2+p(2)**2+p(3)**2)
      Et = p(4) * pt/modp
      end
