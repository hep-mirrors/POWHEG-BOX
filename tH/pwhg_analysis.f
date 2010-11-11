c  The next subroutines, open some histograms and prepare them 
c      to receive data 
c  You can substitute these  with your favourite ones
c  init   :  opens the histograms
c  topout :  closes them
c  pwhgfill  :  fills the histograms with data

      subroutine init_hist
      implicit none
      include '../pwhg_book.h'
      include 'PhysPars.h'
      real * 8 pi,pi2
      parameter(pi = 3.141592653589793D0, pi2 = 9.869604401089358D0)
      real * 8 ptvbcut
      common/cptvbcut/ptvbcut
      character * 10 cut
      integer i,nsigma,diag
      real * 8 step,invmasslow,invmasshigh,ymax
      real * 8 binsize(100)
      common/pwhghistcommon/binsize
      logical ini
      data ini/.true./
      save ini

      if (ini.and.ph_Hmass.eq.0d0) then
         write(*,*) '********************************************'
         write(*,*) '********************************************'
         write(*,*) 'Higgs boson mass plot done assuming the '
         write(*,*) 'following values'
         ph_mch  = 200d0
         write(*,*) 'ph_mch = ',ph_mch
      endif

      ymax=3d0

      call pwhginihist
      diag=1
      binsize(diag) = 10d0
      call pwhgbookup(diag,'pt H ','LIN',binsize(diag),0d0,400d0)

      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'y H ','LIN',binsize(diag),-ymax,ymax)

      diag=diag+1
      binsize(diag) = 10d0
      call pwhgbookup(diag,'pt top ','LIN',binsize(diag),0d0,400d0)

      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'y top ','LIN',binsize(diag),-ymax,ymax)

      diag=diag+1
      binsize(diag) = 10d0
      call pwhgbookup(diag,'tH inv. mass','LIN',binsize(diag),0d0,1000d0)

      diag=diag+1
      binsize(diag) = 10d0
      call pwhgbookup(diag,'pt tH','LOG',binsize(diag),0d0,400d0)

      diag=diag+1
      binsize(diag) = 0.2d0
      call pwhgbookup(diag,'y tH','LIN',binsize(diag),-10d0,10d0)

      diag=diag+1
      binsize(diag) = 10d0
      call pwhgbookup(diag,'pt j1','LOG',binsize(diag),0d0,400d0)

      diag=diag+1
      binsize(diag) = 0.4d0
      call pwhgbookup(diag,'y j1','LIN',binsize(diag),-ymax,ymax)
 

      diag=diag+1
      binsize(diag) = 0.4d0
      call pwhgbookup(diag,'yj1-yth','LIN',binsize(diag),-ymax,ymax)
      
      diag = diag+1
      binsize(diag) = 0.05d0
      call pwhgbookup(diag,'delta phi','LIN',binsize(diag),0d0,3.3d0)
      end

      
     
      subroutine analysis(dsig)
      implicit none
      real * 8 dsig
      include '../include/hepevt.h'
      include '../include/pwhg_math.h'      
c arrays to reconstruct jets
      integer maxtrack,maxjet
      parameter (maxtrack=2048,maxjet=2048)
      real *8 ptrack(4,maxtrack)
      integer maxtagjets
      parameter (maxtagjets=10)
      integer jet,lep,tagjets
      real *8 ptj(maxtagjets),yj(maxtagjets)
      real *8 pjet(4,maxjet) 
      integer mu,jpart,jjet,njj(maxtagjets),found,njets,
     #     ihep,ntracks,ijet,j1,j2
      real * 8 vec(3),pjetin(0:3),pjetout(0:3),beta,ptrel,get_ptrel,
     #     ptrackin(0:3),ptrackout(0:3)
      integer i,diag,njets_passcut
      external get_ptrel
      real * 8 R,ptmin_fastkt
      integer jetvec(maxtrack)
      logical ini
      data ini/.true./
      save ini
      integer HZZ,HWW
      integer idH
      real * 8 pH(0:3),pth,yH,inv_mH,Edec,thl,phil,plepCM(0:3,2),
     $     plep(0:3,2),mod_vecH,pj(0:3,3),pT_lep(2),y_lep(2),pHjj(0:3),
     $     mHjj,ptop(0:3),phiggs(0:3),pttop,ytop,
     $     pthiggs(0:3),ptth,yth,thmass, yb, cosphi, dphi, pjet1(0:3)
      real * 8 random,rsepn,getrapidity0,mjj,azi
      external random,rsepn,getrapidity0,mjj,azi
      real * 8 Rsep(2,2),Rmin,delphi_jj
      logical pass_cuts,pass_cuts_no_mjjmin,pass_cuts_no_deltayjjmin
      real * 8 ptjetmin,ptalljetmin,yjetmax,deltay_jjmin,ptlepmin,
     #     ylepmax,mjjmin,Rsep_jlmin
      logical ylep_between_jets,jet_opphem,exist3rdjet
      logical onlyquarks,Z_exchange
      real * 8 binsize(100)
      
      integer inc, inctot, incb, nothing
      
      common/pwhghistcommon/binsize
      logical iniptcut
      save iniptcut
      data iniptcut/.true./
      logical higgsfound,topfound, isbq1, isbq2, oneb, twob, bok
      integer ihiggs,itop, ibq1, ibq2
c     we need to tell to the this analysis file which program is running it
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
      data WHCPRG/'POWHEG'/
     
c      return
c     DISABLE ALL CUTS


      higgsfound = .false.
      topfound = .false.
      isbq1 = .false.
      isbq2 = .false.
      oneb = .false.
      twob = .false.
      bok = .true.
      
      do ihep=1,nhep
c     set up different searching stategies according to the shower Monte Carlo
c     program used 
         if(idhep(ihep).eq.-37) then
c	 	write(*,*)'CH status: ', isthep(ihep)
c	 	if(isthep(ihep).eq.1)then
c		write(*,*)'Thank God, charged Higgs not decayed!'
c		endif
            ihiggs=ihep
             higgsfound = .true.
         elseif (abs(idhep(ihep)).eq.6) then
            itop=ihep
            topfound=.true.
c	    write(*,*)'top in there! Decay: ', JDAHEP(1,ihep), ' ',  JDAHEP(2,ihep)	    
         endif
c isthep(i) is 1 for final state particles	 
	 if( (isthep(ihep).gt.140).and.(isthep(ihep).lt.145) ) then
c	 write(*,*)'jet idhep: ' , idhep(ihep)
		
	 endif
c	 write(*,*)'id: ' ,idhep(ihep) 
c	write(*,*)'isthep: ', isthep(ihep)
          if( (isthep(ihep).eq.199).and.(.not.isbq1) ) then
	  	isbq1=.true.
		ibq1 = ihep
c		write(*,*)'flavor of the first decayed heavy hadron: ',idhep(ihep)
	 elseif ( (isthep(ihep).eq.199).and.(isbq1) ) then
	  	isbq2=.true.	
		ibq2 = ihep
c		write(*,*)'flavor of the second decayed heavy hadron: ',idhep(ihep)
       endif
	
	
	if( (abs(idhep(ihep)).gt.510).and.(abs(idhep(ihep)).lt.552) )then
	call getrapidity(phep(1,ihep),yb)
		if( (abs(yb).gt.2.5))then
		bok = .false.
		endif
c	write(*,*)'some b hadron: ',  idhep(ihep), ' in a horrible state: ', isthep(ihep)
c	write(*,*)'with rapidity: ', yb 
c	write(*,*)'and its mother was a ', jmohep(1,ihep), ' or a ', jmohep(2,ihep), ' or a hamster'
	endif	
	 
      enddo
         

      if (.not.higgsfound) then
         write(*,*) 'HIGGS NOT FOUND'
         call exit(2)
      endif

      if (.not.topfound) then
         write(*,*) 'TOP NOT FOUND'
         call exit(2)
      endif
      
      
      if(bok)then
      inc = inc+1
      else
      nothing = nothing+1
      endif
      
      inctot = inctot+1
      write(*,*)'inc: ', inc, ' nothing: ', nothing, ' inctot:', inctot  
      
      
      
      do mu=1,3
         pH(mu) = phep(mu,ihiggs)
      enddo
      pH(0) = phep(4,ihiggs)
      
      ptH = sqrt(pH(1)**2+pH(2)**2)
      call getrapidity(phep(1,ihiggs),yH)
      	      
      
      diag=1
      call pwhgfill(diag,ptH,dsig/binsize(diag))
      
      diag=diag+1
c this is the right one, replace later      
      call pwhgfill(diag,yH,dsig/binsize(diag))
	
    
c   end of test 



      do mu=1,3
         ptop(mu) = phep(mu,itop)
      enddo
      ptop(0) = phep(4,itop)
      
      pttop = sqrt(ptop(1)**2+ptop(2)**2)
      call getrapidity(phep(1,itop),ytop)
      
      diag=diag+1
      call pwhgfill(diag,pttop,dsig/binsize(diag))
      diag=diag+1
      call pwhgfill(diag,ytop,dsig/binsize(diag))
      do mu=0,3
         pthiggs(mu)=ph(mu)+ptop(mu)
      enddo
      call getinvmass(pthiggs,thmass)
      
      ptth=sqrt(pthiggs(1)**2+pthiggs(2)**2)
      yth=getrapidity0(pthiggs)
           

      diag=diag+1
      call pwhgfill(diag,thmass,dsig/binsize(diag))
      diag=diag+1
      call pwhgfill(diag,ptth,dsig/binsize(diag))
      diag=diag+1
      call pwhgfill(diag,yth,dsig/binsize(diag))

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
      j1=0
      found=0
      ntracks=0
      njets=0
c     loop over final state particles to find jets 
      do ihep=1,nhep
         if ((isthep(ihep).eq.1).and.
c     exclude leptons, gauge and Higgs bosons
     #        (((abs(idhep(ihep)).le.10).or.(abs(idhep(ihep)).ge.40))
c     but include gluons 
     #        .or.(abs(idhep(ihep)).eq.21))) then
            if(ntracks.eq.maxtrack) then
               write(*,*)
     #              'hwanal: too many particles, increase maxtrack'
               stop
            endif
c     copy momenta to construct jets 
            ntracks=ntracks+1
            do mu=1,4
               ptrack(mu,ntracks)=phep(mu,ihep)
c	       write(*,*)'ihep: ', ihep,' phepmu: ', phep(mu,ihep)
            enddo
         endif
      enddo
      
      if (ntracks.eq.0) then
         return
      endif
c      return
************************************************************************
*     siscone algorithm
**********************************************************************
c     R = 0.7  radius parameter
c     f = 0.5  overlapping fraction
c.....run the clustering        
      call fastjetsiscone(ptrack,ntracks,0.7d0,0.5d0,pjet,njets) 
c      write(*,*)'njets after siscone: ', njets
c      write(*,*)pjet(1,1)
************************************************************************
*     fastkt algorithm
**********************************************************************
*      R = 0.7  Radius parameter
c.....run the clustering 
      R = 0.7d0          
      ptmin_fastkt = 0d0
c      call fastjetktwhich(ptrack,ntracks,ptmin_fastkt,R,
c     #     pjet,njets,jetvec) 
      
      if (njets.gt.0) then
c     find the first TWO hardest jets, if any
         call find_hardest_jets(njets,pjet,2,tagjets,njj)
c	 write(*,*) tagjets

c     at least TWO tagging jets to continue
         if (tagjets.le.1) then
            return
         endif
         
         do ijet=1,tagjets
            do mu=1,3
               pj(mu,ijet)=pjet(mu,njj(ijet))
            enddo
            pj(0,ijet)=pjet(4,njj(ijet))
c	    write(*,*)'pj0: ', pj(0,ijet)
         enddo
         

         do ijet=1,tagjets
            ptj(ijet) = sqrt(pj(1,ijet)**2 + pj(2,ijet)**2)
            yj(ijet) = getrapidity0(pj(0,ijet))
c	    write(*,*)'ijet: ', ijet, ' pt: ', ptj(ijet), ' y: ', yj(ijet)
         enddo
      endif
      
c  implement cut on jet pt, otherwise it hits the pole at low pt and the whole thing doesn't behave itself      
      
      diag=diag+1
      call pwhgfill(diag,ptj(1),dsig/binsize(diag))
      diag=diag+1
      call pwhgfill(diag,yj(1),dsig/binsize(diag))
      diag=diag+1
      call pwhgfill(diag,yj(1)-yth,dsig/binsize(diag))
        
c      do mu=1,3
c      pjet1(mu)=pj(mu,1)
c      enddo
      
c      write(*,*)'test: ', ptop(0), ' ', pj(0,1)
      if(ptop(0).ne.pj(0,1)) then
      	do mu=1,3
      	pjet1(mu)=pj(mu,1)
      	enddo
      else
c      	write(*,*)'coucou'
        do mu=1,3
      	pjet1(mu)=pj(mu,2)
      	enddo
      endif
      
      
      
      call getCosPhy(ptop,pH,cosphi)
      dphi = acos(cosphi)
c      write(*,*)'dphi: ', dphi
      diag=diag+1
      call pwhgfill(diag,dphi,dsig/binsize(diag))

          
      end

      
      subroutine getCosPhy(p1,p2,CosPhy)
      implicit none
      real *8 p1(0:3), p2(0:3)
      real *8 CosPhy
      CosPhy = (p1(1)*p2(1)+p1(2)*p2(2))/(
     -  	sqrt(p1(1)*p1(1)+p1(2)*p1(2))*sqrt(p2(1)*p2(1)+p2(2)*p2(2)) )
      end


      subroutine getrapidity(p,y)
      implicit none
      real * 8 p(4),y
      y=0.5d0*log((p(4)+p(3))/(p(4)-p(3)))
      end

      function getrapidity0(p)
      implicit none
      real * 8 p(0:3),getrapidity0
      getrapidity0=0.5d0*log((p(0)+p(3))/(p(0)-p(3)))
      end

      subroutine getinvmass(p,m)
      implicit none
      real * 8 p(4),m
      m=sqrt(abs(p(4)**2-p(1)**2-p(2)**2-p(3)**2))
      end



c      subroutine pwhgfillup(n,x,y)
c      implicit none
c      real * 8 x,y
c      integer n
c      call pwhgfill(n,x,y)
c      call pwhgfill(n+100,x,y*y)
c      end





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
      

      function azi(p)
      implicit none
      include '../include/pwhg_math.h'  
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

c     calculate the separation in the lego plot between the two momenta
c     p1 and p2
      function rsepn(p1,p2)
      implicit none
      include '../include/pwhg_math.h'  
      real * 8 rsepn,p1(0:3),p2(0:3)
      real * 8 y1,phi1,y2,phi2
      real * 8 delphi
      real * 8 getrapidity0,azi
      external getrapidity0,azi

      phi1 = azi(p1)   
      phi2 = azi(p2)
      y1 = getrapidity0(p1)
      y2 = getrapidity0(p2)

      delphi = abs(phi1-phi2)
      if (delphi.gt.pi) then
         delphi = 2*pi-delphi
      endif
      if (delphi.lt.0 .or. delphi.gt.pi) then
         print*,' problem in rsepn. delphi = ',delphi
      endif
      rsepn = sqrt( (y1-y2)**2 + delphi**2 )
      end



c mjj^2 = (p1+p2)^2 = p1^2 + p2^2 + 2*dotp(p1,p2)
      function mjj(p1,p2)
      implicit none
      real * 8 mjj,p1(0:3),p2(0:3)
      real * 8 p(0:3)
      integer mu
      do mu=0,3
         p(mu)=p1(mu)+p2(mu)
      enddo
      mjj = sqrt(abs(p(0)**2-p(1)**2-p(2)**2-p(3)**2))
      end




c     find the first "nhardjets" hardest jets in pjet (that contains njets)
c     and return their position.
c     foundhardjets is the number of found hard jets (.le.nhardjets)
      subroutine find_hardest_jets(njets,pjet,nhardjets,
     #     foundhardjets,jj)
      implicit none
      integer njets
      real *8 pjet(4,njets) 
      integer nhardjets,jj(nhardjets)
      real * 8 ptj(nhardjets),pt
      integer ijet,hjet,foundhardjets,i
      logical is_i_in_array
      external is_i_in_array

      if (njets.eq.0) then
         write(*,*) 'WARNING!!!!!!!!!!!  EMPTY  PJET ARRAY'
         nhardjets=0
         return
      endif

      do hjet=1,nhardjets
         jj(hjet)=0d0
         ptj(hjet)=0d0
      enddo
      foundhardjets=1
      do ijet=1,njets   
         pt=sqrt(pjet(1,ijet)**2 + pjet(2,ijet)**2)
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



c      subroutine topout
c      implicit none
c      include '../include/hepevt.h'
c      character * 50 title
c      integer i
c      integer maxnumplot
c      common/cmaxnumplot/maxnumplot
c      logical lin(100)
c      common/lin_scale/lin
c      character * 3 scale
cc     
cc     If histogram I contains accumulated weights and
cc     histogram I+100 contains its squared values,
cc     then a temporary copy of both is made in order
cc     to safely have intermediate results
c
c      do i=1,maxnumplot
c	call pwhgfinal(i)
c        call pwhgcopy(i,i+200)
c        call pwhgcopy(i+100,i+300)
c        call pwhgopera(i+200,'F',i+200,i+200,1d0/dble(nevhep),0d0)
c        call pwhgerror(i+200,i+300,dble(nevhep))
c        call pwhgfinal(i+200)
c        call pwhgfinal(i+300)
c      enddo
c      do i=1,maxnumplot
c         call pwhggettitle(i+200,title)
c         if (lin(i)) then
c            scale = 'LIN'
c         else
c            scale = 'LOG'
c         endif
c         call pwhgmultitop(i+200,i+300,2,3,title,' ',scale)
c      enddo
c      end            





