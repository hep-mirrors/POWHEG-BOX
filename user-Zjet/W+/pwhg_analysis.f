c  The next subroutines, open some histograms and prepare them 
c      to receive data 
c  You can substitute these  with your favourite ones
c  init   :  opens the histograms
c  topout :  closes them
c  pwhgfill  :  fills the histograms with data

      subroutine init_hist
      implicit none
      real * 8 pi,pi2
      parameter(pi = 3.141592653589793D0, pi2 = 9.869604401089358D0)
      integer maxnumplot
      common/cmaxnumplot/maxnumplot
      real * 8 ptvbcut
      common/cptvbcut/ptvbcut
      character * 10 cut
      integer i

      write(unit=cut,fmt="(f5.2)") ptvbcut
      call pwhginihist

      call pwhgbookup(1,'pt W ptW>'//cut,20d0,0d0,800d0)
      call pwhgbookup(2,'pt J1 ptW>'//cut,20d0,0d0,800d0)
      call pwhgbookup(3,'pt J2 ptW>'//cut,20d0,0d0,800d0)
      call pwhgbookup(4,'inv mass W ptW>'//cut,1d0,60d0,120d0)

      call pwhgbookup(5,'y W, ptW>10 ',0.4d0,-5d0,5d0)
      call pwhgbookup(6,'y W, ptW>20 ',0.4d0,-5d0,5d0)
      call pwhgbookup(7,'y W, ptW>40 ',0.4d0,-5d0,5d0)
      call pwhgbookup(8,'y W, ptW>60 ',0.4d0,-5d0,5d0)
      call pwhgbookup(9,'y W, ptW>80 ',0.4d0,-5d0,5d0)
      call pwhgbookup(10,'y W, ptW>100 ',0.4d0,-5d0,5d0)
      call pwhgbookup(11,'y W, ptW>150 ',0.4d0,-5d0,5d0)
      call pwhgbookup(12,'y W, ptW>200 ',0.4d0,-5d0,5d0)
      call pwhgbookup(13,'y W, ptW>300 ',0.4d0,-5d0,5d0)

      call pwhgbookup(14,'y WJ1, ptJ1>10 ptW>'//cut,0.4d0,-5d0,5d0)
      call pwhgbookup(15,'y WJ1, ptJ1>20 ptW>'//cut,0.4d0,-5d0,5d0)
      call pwhgbookup(16,'y WJ1, ptJ1>40 ptW>'//cut,0.4d0,-5d0,5d0)
      call pwhgbookup(17,'y WJ1, ptJ1>60 ptW>'//cut,0.4d0,-5d0,5d0)
      call pwhgbookup(18,'y WJ1, ptJ1>80 ptW>'//cut,0.4d0,-5d0,5d0)
      call pwhgbookup(19,'y WJ1, ptJ1>100 ptW>'//cut,0.4d0,-5d0,5d0)
      call pwhgbookup(20,'y WJ1, ptJ1>150 ptW>'//cut,0.4d0,-5d0,5d0)
      call pwhgbookup(21,'y WJ1, ptJ1>200 ptW>'//cut,0.4d0,-5d0,5d0)
      call pwhgbookup(22,'y WJ1, ptJ1>300 ptW>'//cut,0.4d0,-5d0,5d0)

      call pwhgbookup(23,'dy W-J1, ptW>'//cut,0.4d0,-10d0,10d0)
      call pwhgbookup(24,'dy W-J1, ptJ1>20 ptW>'//cut,0.4d0,-10d0,10d0)
      call pwhgbookup(25,'dy W-J1, ptJ1>40 ptW>'//cut,0.4d0,-10d0,10d0)
      call pwhgbookup(26,'dy W-J1, ptJ1>60 ptW>'//cut,0.4d0,-10d0,10d0)
      call pwhgbookup(27,'dy W-J1, ptJ1>80 ptW>'//cut,0.4d0,-10d0,10d0)
      call pwhgbookup(28,'dy W-J1, ptJ1>100 ptW>'//cut,0.4d0,-10d0,10d0)
      call pwhgbookup(29,'dy W-J1, ptJ1>150 ptW>'//cut,0.4d0,-10d0,10d0)
      call pwhgbookup(30,'dy W-J1, ptJ1>200 ptW>'//cut,0.4d0,-10d0,10d0)
      call pwhgbookup(31,'dy W-J1, ptJ1>300 ptW>'//cut,0.4d0,-10d0,10d0)


      call pwhgbookup(32,'pt e+ ptW>'//cut,20d0,0d0,800d0)
      call pwhgbookup(33,'pt ve ptW>'//cut,20d0,0d0,800d0)
      call pwhgbookup(34,'y e+, pt e+>10 ptW>'//cut,0.4d0,-5d0,5d0)
      call pwhgbookup(35,'y e+, pt e+>20 ptW>'//cut,0.4d0,-5d0,5d0)
      call pwhgbookup(36,'y e+, pt e+>40 ptW>'//cut,0.4d0,-5d0,5d0)
      call pwhgbookup(37,'y e+, pt e+>60 ptW>'//cut,0.4d0,-5d0,5d0)
      call pwhgbookup(38,'y e+, pt e+>80 ptW>'//cut,0.4d0,-5d0,5d0)
      call pwhgbookup(39,'y ve, pt ve>100 ptW>'//cut,0.4d0,-5d0,5d0)
      call pwhgbookup(40,'y ve, pt ve>10 ptW>'//cut,0.4d0,-5d0,5d0)
      call pwhgbookup(41,'y ve, pt ve>20 ptW>'//cut,0.4d0,-5d0,5d0)
      call pwhgbookup(42,'y ve, pt ve>40 ptW>'//cut,0.4d0,-5d0,5d0)
      call pwhgbookup(43,'y ve, pt ve>60 ptW>'//cut,0.4d0,-5d0,5d0)
      call pwhgbookup(44,'y ve, pt ve>80 ptW>'//cut,0.4d0,-5d0,5d0)
      call pwhgbookup(45,'y ve, pt ve>100 ptW>'//cut,0.4d0,-5d0,5d0)     

      call pwhgbookup(46,'y e+,ptW>'//cut,0.4d0,-5d0,5d0)
      call pwhgbookup(47,'y ve,ptW>'//cut,0.4d0,-5d0,5d0)
      call pwhgbookup(48,'pt e+, zoom ptW>'//cut,4d0,0d0,100d0)
      call pwhgbookup(49,'pt ve, zoom ptW>'//cut,4d0,0d0,100d0)
      call pwhgbookup(50,'pt W, zoom ptW>'//cut,2.5d0,0d0,100d0)
      call pwhgbookup(51,'pt W, zoom2 ptW>'//cut,0.5d0,0d0,20d0)
      call pwhgbookup(52,'pt J1, zoom ptW>'//cut,0.5d0,0d0,20d0)
      call pwhgbookup(53,'pt J2, zoom ptW>'//cut,0.5d0,0d0,20d0)
      call pwhgbookup(54,'pt_rel J1',0.5d0,0d0,15d0)
      call pwhgbookup(55,'pt_rel J2',0.5d0,0d0,15d0)
      call pwhgbookup(56,'azimuth W+',0.1d0,-3.15d0,3.15d0)
      call pwhgbookup(57,'total x-sec',1d0,0d0,1d0)    
      maxnumplot = 57

      if (maxnumplot.ge.100) then
         write(*,*) 'Booking more than 100 histograms'
         write(*,*) 'Errors may occur in hisogramming package'
         stop
      endif

      end

      
     
      subroutine analysis(dsig)
      implicit none
      real * 8 dsig
      include '../include/hepevt.h'
c arrays to reconstruct jets
      integer maxtrack,maxjet
      parameter (maxtrack=2048,maxjet=2048)
      real *8 ptrack(4,maxtrack)
      real *8 ptj1,ptj2,yj1,yj2,ptvb,yvb,yvbj1
      real *8 ptep,ptem,yep,yem
      real *8 pjet(4,maxjet) 
      real * 8 mvb,pvb(4),pvbj1(4),tmp
      real * 8 ptvbcut
      common/cptvbcut/ptvbcut
      data ptvbcut/0d0/

      integer mu,jpart,jjet,jeminus,jeplus,j1,j2,found,njets,
     1     neplus,neminus,ihep,ntracks,ijet
      logical buildjets
      parameter (buildjets=.true.)

      real * 8 vec(3),pjetin(0:3),pjetout(0:3),beta,ptrel,get_ptrel,
     #     ptrackin(0:3),ptrackout(0:3)
      integer i
      external get_ptrel
      real * 8 R,ptmin_fastkt
      integer jetvec(maxtrack),jj
      logical ini
      data ini/.true./
      save ini
      integer maxnumlep
      parameter (maxnumlep=10)
      integer emvec(maxnumlep),epvec(maxnumlep),iep,iem,ep,em
      real * 8  Wmass,Wwidth,Wmass2low,Wmass2high,mV2ref,mV2
      logical foundlep
      integer jem_true,jep_true,found_truedec
      logical findmother
      parameter (findmother=.false.)
      logical findinvmass
      parameter (findinvmass=.true.)

c CAVEAT.... 
      Wmass = 91.188d0
      Wwidth = 2.486d0
      Wmass2low = (Wmass-10*Wwidth)**2
      Wmass2high = (Wmass+10*Wwidth)**2

      if (ini) then
         write(*,*) '**************************************************'
         write(*,*) '**************************************************'
         write(*,*) '                ANALYSIS CUTS                     '
         write(*,*) '**************************************************'
         write(*,*) '**************************************************'
         write(*,*)   sqrt(Wmass2low),' < M_W < ',sqrt(Wmass2high)
         write(*,*) '**************************************************'
         write(*,*) '**************************************************'
      endif

      neminus=0
      neplus=0
      do i=1,maxnumlep
         emvec(i) = 0
         epvec(i) = 0
      enddo
      do ihep=1,nhep
         if (isthep(ihep).eq.1) then
            if(idhep(ihep).eq.-11) then
               neminus=neminus+1
               emvec(neminus)=ihep
            elseif(idhep(ihep).eq.12) then
               neplus=neplus+1
               epvec(neplus)=ihep
            endif
         endif         
      enddo
      
      if(neminus.eq.0.or.neplus.eq.0) then
         write(*,*)" not enough leptons! drop event"
         stop
      endif

      if (findinvmass) then
c     'Realistic' analysis
c-------------------------
         foundlep = .false.
         mV2ref = 1d30
         do em=1,neminus
            do ep=1,neplus
               iem=emvec(em)
               iep=epvec(ep)
               mV2 = (phep(4,iem)+phep(4,iep))**2
     #              -(phep(1,iem)+phep(1,iep))**2
     #              -(phep(2,iem)+phep(2,iep))**2
     #              -(phep(3,iem)+phep(3,iep))**2          
               if ((Wmass2low.lt.mV2).and.(mV2.lt.Wmass2high))  then
                  if (foundlep.and..not.findmother) then
                     write(*,*) 
     #                 'two lepton couples satisfy W mass window'
                     write(*,*) 'event dropped!'
                     return
                  endif
                  foundlep=.true.
                  if (abs(mV2-Wmass**2).lt.abs(mV2ref-Wmass**2)) then
                     mV2ref = mV2
                     jeminus = iem
                     jeplus = iep               
                  endif
               endif
            enddo
         enddo
      endif
      if(findmother) then
c     'Exact' analysis
c---------------------
c     Loop again over final state particles to find products of W decay, by
c     looking into the shower branchings.
c     !!!!!!!!! It does NOT work at the parton level. !!!!!!!!!!!!!!
         found_truedec=0
         do ihep=1,nhep
c     works for POWHEG+HERWIG,POWHEG+PYHIA,HERWIG,PYTHIA and  real in MC@NLO
            if ((isthep(ihep).eq.1).and.
     #              ((idhep(jmohep(1,jmohep(1,ihep))).eq.23))) then
c     find first decay product
               if(idhep(ihep).eq.-11) then
                  jem_true=ihep
                  found_truedec=found_truedec+1
c     find second decay product
               elseif(idhep(ihep).eq.12) then
                  jep_true=ihep
                  found_truedec=found_truedec+1
               endif
            endif
         enddo
         if(found_truedec.ne.2) then
            write(*,*) 'lepton couple from W decay not found'
            write(*,*) '!!!! Error !!!!'
            stop
         endif
      endif
      if(findmother.and.findinvmass) then
c     check the two analysis methods
c-----------------------------------
         if((jem_true.ne.jeminus).or.(jep_true.ne.jeplus)) then
            write(*,*) '--------------------------------'
            write(*,*) 'Error when looking for leptons from W decay'
            write(*,*) 'Wrong inv mass: ',sqrt(mV2ref),
     #           '<--',jeminus,jeplus
            write(*,*) '\t e+ -->',phep(4,jeminus),phep(1,jeminus),
     #           phep(2,jeminus),phep(3,jeminus)
            write(*,*) '\t ve -->',phep(4,jeplus),phep(1,jeplus),
     #           phep(2,jeplus),phep(3,jeplus)
            write(*,*) 'True inv mass: ',sqrt(
     #           (phep(4,jem_true)+phep(4,jep_true))**2
     #           -(phep(1,jem_true)+phep(1,jep_true))**2
     #           -(phep(2,jem_true)+phep(2,jep_true))**2
     #           -(phep(3,jem_true)+phep(3,jep_true))**2   ),
     #           '<--',jem_true,jep_true
            write(*,*) '\t e+ -->',phep(4,jem_true),phep(1,jem_true),
     #           phep(2,jem_true),phep(3,jem_true)
            write(*,*) '\t ve -->',phep(4,jep_true),phep(1,jep_true),
     #           phep(2,jep_true),phep(3,jep_true)
            call hwuepr
            write(*,*) '\n !!!! Event dropped !!!!\n '
            return
         endif
c     assign correct values
c-----------------------------------
         jeminus=jem_true
         jeplus=jep_true
      endif

      do mu=1,4
         pvb(mu)=phep(mu,jeminus)+phep(mu,jeplus)         
      enddo
      ptvb=sqrt(pvb(1)**2+pvb(2)**2)
   
      if (ini) then
         write(*,*) '**************************************************'
         write(*,*) '**************************************************'
         write(*,*) '            PT_V min = ',ptvbcut
         write(*,*) '**************************************************'
         write(*,*) '**************************************************'
         ini=.false.
      endif
      
      if (ptvb.lt.ptvbcut) return

      call getinvmass(pvb,mvb)
      call getrapidity(pvb,yvb)
      ptep=sqrt(phep(1,jeplus)**2+phep(2,jeplus)**2)
      call getrapidity(phep(1,jeplus),yep)
      ptem=sqrt(phep(1,jeminus)**2+phep(2,jeminus)**2)
      call getrapidity(phep(1,jeminus),yem)

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
      j1=0
      found=0
      ntracks=0
      njets=0
c     Loop over final state particles to find jets 
      do ihep=1,nhep
         if ((isthep(ihep).eq.1).and.
     1    (((abs(idhep(ihep)).le.10).or.(abs(idhep(ihep)).ge.40))
c     exclude leptons, gauge and higgs bosons
     2    .or.(abs(idhep(ihep)).eq.21)))
c     but  include gluons 
     3           then
            if(ntracks.eq.maxtrack) then
               write(*,*)
     #              'hwanal: too many particles, increase maxtrack'
               stop
            endif
c     copy momenta to construct jets 
            ntracks=ntracks+1
            do mu=1,4
               ptrack(mu,ntracks)=phep(mu,ihep)
            enddo
         endif
      enddo
      
      call pwhgfill(1,ptvb,dsig)
      call pwhgfill(4,mvb,dsig)

      if(buildjets.and.ntracks.gt.0) then
************************************************************************
*     siscone algorithm
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
      ptmin_fastkt = 0d0
      call fastjetktwhich(ptrack,ntracks,ptmin_fastkt,R,
     #     pjet,njets,jetvec) 
c     
c     ... now we have the jets
      if (njets.gt.0) then
         ptj1=0d0
         ptj2=0d0
         yj1=0
         yj2=0
         j1=0
         j2=0
         do ijet=1,njets
c............find the hardest jet
            tmp=sqrt(pjet(1,ijet)**2 + pjet(2,ijet)**2)
            if (tmp.gt.ptj1) then
               ptj1=tmp
               j1=ijet
            elseif(tmp.gt.ptj2) then
               ptj2=tmp
               j2=ijet
            endif
         enddo
         if(j1.gt.0) call getrapidity(pjet(1,j1),yj1)
         if(j2.gt.0) call getrapidity(pjet(1,j2),yj2)
         if(j1.gt.0) then
            do mu=1,4
               pvbj1(mu)=pvb(mu)+pjet(mu,j1)
            enddo
            call getrapidity(pvbj1,yvbj1)
         endif
         
         if(j1.gt.0) then
            call pwhgfill(2,ptj1,dsig)
            call pwhgfill(52,ptj1,dsig)
         endif
         if(j2.gt.0) then
            call pwhgfill(3,ptj2,dsig)
            call pwhgfill(53,ptj2,dsig)
         endif
         
         if(ptvb.gt.10) call pwhgfill(5,yvb,dsig)
         if(ptvb.gt.20) call pwhgfill(6,yvb,dsig)
         if(ptvb.gt.40) call pwhgfill(7,yvb,dsig)
         if(ptvb.gt.60) call pwhgfill(8,yvb,dsig)
         if(ptvb.gt.80) call pwhgfill(9,yvb,dsig)
         if(ptvb.gt.100) call pwhgfill(10,yvb,dsig)
         if(ptvb.gt.150) call pwhgfill(11,yvb,dsig)
         if(ptvb.gt.200) call pwhgfill(12,yvb,dsig)
         if(ptvb.gt.300) call pwhgfill(13,yvb,dsig)
         
         if(j1.gt.0) then
            if(ptj1.gt.10) call pwhgfill(14,yj1,dsig)
            if(ptj1.gt.20) call pwhgfill(15,yj1,dsig)
            if(ptj1.gt.40) call pwhgfill(16,yj1,dsig)
            if(ptj1.gt.60) call pwhgfill(17,yj1,dsig)
            if(ptj1.gt.80) call pwhgfill(18,yj1,dsig)
            if(ptj1.gt.100) call pwhgfill(19,yj1,dsig)
            if(ptj1.gt.150) call pwhgfill(20,yj1,dsig)
            if(ptj1.gt.200) call pwhgfill(21,yj1,dsig)
            if(ptj1.gt.300) call pwhgfill(22,yj1,dsig)
         endif
         if(j2.gt.0) then
            if(ptj2.gt.10) call pwhgfill(23,yvbj1-yj2,dsig)
            if(ptj2.gt.20) call pwhgfill(24,yvbj1-yj2,dsig)
            if(ptj2.gt.40) call pwhgfill(25,yvbj1-yj2,dsig)
            if(ptj2.gt.60) call pwhgfill(26,yvbj1-yj2,dsig)
            if(ptj2.gt.80) call pwhgfill(27,yvbj1-yj2,dsig)
            if(ptj2.gt.100) call pwhgfill(28,yvbj1-yj2,dsig)
            if(ptj2.gt.150) call pwhgfill(29,yvbj1-yj2,dsig)
            if(ptj2.gt.200) call pwhgfill(30,yvbj1-yj2,dsig)
            if(ptj2.gt.300) call pwhgfill(31,yvbj1-yj2,dsig)
         endif

c     loop on the hardest and next-to-hardest jet
         do ijet=1,min(njets,2)
            if (ijet.eq.1) then
               jj=j1 
            else 
               jj=j2 
            endif            
            do mu=1,3
               pjetin(mu) = pjet(mu,jj)
            enddo
            pjetin(0) = pjet(4,jj)         
            vec(1)=0d0
            vec(2)=0d0
            vec(3)=1d0
            beta = -pjet(3,jj)/pjet(4,jj)
            call mboost(1,vec,beta,pjetin,pjetout)         
c     write(*,*) pjetout
            ptrel = 0
            do i=1,ntracks
               if (jetvec(i).eq.jj) then
                  do mu=1,3
                     ptrackin(mu) = ptrack(mu,i)
                  enddo
                  ptrackin(0) = ptrack(4,i)
                  call mboost(1,vec,beta,ptrackin,ptrackout) 
                  ptrel = ptrel + get_ptrel(ptrackout,pjetout)
               endif
            enddo
            if (ijet.eq.1) then 
               call pwhgfill(54,ptrel,dsig)
            else
               call pwhgfill(55,ptrel,dsig)
            endif
         enddo         
      endif            
c     endif buildjets
      endif
      
      call pwhgfill(32,ptem,dsig)
      call pwhgfill(33,ptep,dsig)
      
      if(ptem.gt.10) call pwhgfill(34,yem,dsig)
      if(ptem.gt.20) call pwhgfill(35,yem,dsig)
      if(ptem.gt.40) call pwhgfill(36,yem,dsig)
      if(ptem.gt.60) call pwhgfill(37,yem,dsig)
      if(ptem.gt.80) call pwhgfill(38,yem,dsig)
      if(ptem.gt.100) call pwhgfill(39,yem,dsig)
      
      if(ptep.gt.10) call pwhgfill(40,yep,dsig)
      if(ptep.gt.20) call pwhgfill(41,yep,dsig)
      if(ptep.gt.40) call pwhgfill(42,yep,dsig)
      if(ptep.gt.60) call pwhgfill(43,yep,dsig)
      if(ptep.gt.80) call pwhgfill(44,yep,dsig)
      if(ptep.gt.100) call pwhgfill(45,yep,dsig)
      
      call pwhgfill(46,yem,dsig)
      call pwhgfill(47,yep,dsig)
      
      call pwhgfill(48,ptem,dsig)
      call pwhgfill(49,ptep,dsig)
      call pwhgfill(50,ptvb,dsig)
      call pwhgfill(51,ptvb,dsig)
      
      call pwhgfill(56,atan2(pvb(2),pvb(1)),dsig)

      call pwhgfill(57,0.5d0,dsig)
      
      end

      subroutine getrapidity(p,y)
      implicit none
      real * 8 p(4),y
      y=0.5d0*log((p(4)+p(3))/(p(4)-p(3)))
      end

      subroutine getinvmass(p,m)
      implicit none
      real * 8 p(4),m
      m=sqrt(abs(p(4)**2-p(1)**2-p(2)**2-p(3)**2))
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

   
 
      subroutine topout
      implicit none
      include '../include/hepevt.h'
      character * 50 title
      integer i
      integer maxnumplot
      common/cmaxnumplot/maxnumplot

      call pwhgstatup 
      do i=1,maxnumplot
        call pwhgfinal(i)
        call pwhgfinal(i+300)
        call pwhggettitle(i,title)
        call pwhgmultitop(i,i+300,2,3,title,' ','LOG')
      enddo
      end             
    



     
      
