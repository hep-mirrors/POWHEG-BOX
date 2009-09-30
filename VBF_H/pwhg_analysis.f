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
      
      call pwhgbook(1,'pt Z ptZ>'//cut,20d0,0d0,800d0)
      call pwhgbook(2,'pt J1 ptZ>'//cut,20d0,0d0,800d0)
      call pwhgbook(3,'pt J2 ptZ>'//cut,20d0,0d0,800d0)
      call pwhgbook(4,'inv mass Z ptZ>'//cut,1d0,60d0,120d0)

      call pwhgbook(5,'y Z, ptZ>10 ',0.4d0,-5d0,5d0)
      call pwhgbook(6,'y Z, ptZ>20 ',0.4d0,-5d0,5d0)
      call pwhgbook(7,'y Z, ptZ>40 ',0.4d0,-5d0,5d0)
      call pwhgbook(8,'y Z, ptZ>60 ',0.4d0,-5d0,5d0)
      call pwhgbook(9,'y Z, ptZ>80 ',0.4d0,-5d0,5d0)
      call pwhgbook(10,'y Z, ptZ>100 ',0.4d0,-5d0,5d0)
      call pwhgbook(11,'y Z, ptZ>150 ',0.4d0,-5d0,5d0)
      call pwhgbook(12,'y Z, ptZ>200 ',0.4d0,-5d0,5d0)
      call pwhgbook(13,'y Z, ptZ>300 ',0.4d0,-5d0,5d0)

      call pwhgbook(14,'y ZJ1, ptJ1>10 ptZ>'//cut,0.4d0,-5d0,5d0)
      call pwhgbook(15,'y ZJ1, ptJ1>20 ptZ>'//cut,0.4d0,-5d0,5d0)
      call pwhgbook(16,'y ZJ1, ptJ1>40 ptZ>'//cut,0.4d0,-5d0,5d0)
      call pwhgbook(17,'y ZJ1, ptJ1>60 ptZ>'//cut,0.4d0,-5d0,5d0)
      call pwhgbook(18,'y ZJ1, ptJ1>80 ptZ>'//cut,0.4d0,-5d0,5d0)
      call pwhgbook(19,'y ZJ1, ptJ1>100 ptZ>'//cut,0.4d0,-5d0,5d0)
      call pwhgbook(20,'y ZJ1, ptJ1>150 ptZ>'//cut,0.4d0,-5d0,5d0)
      call pwhgbook(21,'y ZJ1, ptJ1>200 ptZ>'//cut,0.4d0,-5d0,5d0)
      call pwhgbook(22,'y ZJ1, ptJ1>300 ptZ>'//cut,0.4d0,-5d0,5d0)

      call pwhgbook(23,'dy ZJ1-j2, ptJ2>10 ptZ>'//cut,0.4d0,-10d0,10d0)
      call pwhgbook(24,'dy ZJ1-j2, ptJ2>20 ptZ>'//cut,0.4d0,-10d0,10d0)
      call pwhgbook(25,'dy ZJ1-j2, ptJ2>40 ptZ>'//cut,0.4d0,-10d0,10d0)
      call pwhgbook(26,'dy ZJ1-j2, ptJ2>60 ptZ>'//cut,0.4d0,-10d0,10d0)
      call pwhgbook(27,'dy ZJ1-j2, ptJ2>80 ptZ>'//cut,0.4d0,-10d0,10d0)
      call pwhgbook(28,'dy ZJ1-j2, ptJ2>100 ptZ>'//cut,0.4d0,-10d0,10d0)
      call pwhgbook(29,'dy ZJ1-j2, ptJ2>150 ptZ>'//cut,0.4d0,-10d0,10d0)
      call pwhgbook(30,'dy ZJ1-j2, ptJ2>200 ptZ>'//cut,0.4d0,-10d0,10d0)
      call pwhgbook(31,'dy ZJ1-j2, ptJ2>300 ptZ>'//cut,0.4d0,-10d0,10d0)


      call pwhgbook(32,'pt e- ptZ>'//cut,20d0,0d0,800d0)
      call pwhgbook(33,'pt e+ ptZ>'//cut,20d0,0d0,800d0)
      call pwhgbook(34,'y e-, pt e->10 ptZ>'//cut,0.4d0,-5d0,5d0)
      call pwhgbook(35,'y e-, pt e->20 ptZ>'//cut,0.4d0,-5d0,5d0)
      call pwhgbook(36,'y e-, pt e->40 ptZ>'//cut,0.4d0,-5d0,5d0)
      call pwhgbook(37,'y e-, pt e->60 ptZ>'//cut,0.4d0,-5d0,5d0)
      call pwhgbook(38,'y e-, pt e->80 ptZ>'//cut,0.4d0,-5d0,5d0)
      call pwhgbook(39,'y e-, pt e->100 ptZ>'//cut,0.4d0,-5d0,5d0)
      call pwhgbook(40,'y e+, pt e+>10 ptZ>'//cut,0.4d0,-5d0,5d0)
      call pwhgbook(41,'y e+, pt e+>20 ptZ>'//cut,0.4d0,-5d0,5d0)
      call pwhgbook(42,'y e+, pt e+>40 ptZ>'//cut,0.4d0,-5d0,5d0)
      call pwhgbook(43,'y e+, pt e+>60 ptZ>'//cut,0.4d0,-5d0,5d0)
      call pwhgbook(44,'y e+, pt e+>80 ptZ>'//cut,0.4d0,-5d0,5d0)
      call pwhgbook(45,'y e+, pt e+>100 ptZ>'//cut,0.4d0,-5d0,5d0)     

      call pwhgbook(46,'y e-,ptZ>'//cut,0.4d0,-5d0,5d0)
      call pwhgbook(47,'y e+,ptZ>'//cut,0.4d0,-5d0,5d0)
      call pwhgbook(48,'pt e-, zoom ptZ>'//cut,4d0,0d0,100d0)
      call pwhgbook(49,'pt e+, zoom ptZ>'//cut,4d0,0d0,100d0)
      call pwhgbook(50,'pt Z, zoom ptZ>'//cut,2.5d0,0d0,100d0)
      call pwhgbook(51,'pt Z, zoom2 ptZ>'//cut,0.5d0,0d0,20d0)
      call pwhgbook(52,'pt J1, zoom ptZ>'//cut,0.5d0,0d0,20d0)
      call pwhgbook(53,'pt J2, zoom ptZ>'//cut,0.5d0,0d0,20d0)
      call pwhgbook(54,'pt_rel J1',0.5d0,0d0,15d0)
      call pwhgbook(55,'pt_rel J2',0.5d0,0d0,15d0)
      call pwhgbook(56,'azimuth Z',0.1d0,-3.15d0,3.15d0)    
      maxnumplot = 56

      if (maxnumplot.ge.100) then
         write(*,*) 'Booking more than 100 histograms'
         write(*,*) 'Errors may occur in hisogramming package'
         stop
      endif

c     boook histograms to accumulate squared values
      do i = 1,maxnumplot
         call pwhgcopy(i,i+100)
      enddo 

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
      data ptvbcut/15d0/

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
      real * 8  Zmass,Zwidth,Zmass2low,Zmass2high,mV2ref,mV2
      logical foundlep
      integer jem_true,jep_true,found_truedec
      logical findmother
      parameter (findmother=.false.)
      logical findinvmass
      parameter (findinvmass=.true.)

c CAVEAT.... 
      Zmass = 91.188d0
      Zwidth = 2.486d0
      Zmass2low = (Zmass-10*Zwidth)**2
      Zmass2high = (Zmass+10*Zwidth)**2

      if (ini) then
         write(*,*) '**************************************************'
         write(*,*) '**************************************************'
         write(*,*) '                ANALYSIS CUTS                     '
         write(*,*) '**************************************************'
         write(*,*) '**************************************************'
         write(*,*)   sqrt(Zmass2low),' < M_Z < ',sqrt(Zmass2high)
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
            if(idhep(ihep).eq.11) then
               neminus=neminus+1
               emvec(neminus)=ihep
            elseif(idhep(ihep).eq.-11) then
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
               if ((Zmass2low.lt.mV2).and.(mV2.lt.Zmass2high))  then
                  if (foundlep.and..not.findmother) then
                     write(*,*) 
     #                 'two lepton couples satisfy Z mass window'
                     write(*,*) 'event dropped!'
                     return
                  endif
                  foundlep=.true.
                  if (abs(mV2-Zmass**2).lt.abs(mV2ref-Zmass**2)) then
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
c     Loop again over final state particles to find products of Z decay, by
c     looking into the shower branchings.
c     !!!!!!!!! It does NOT work at the parton level. !!!!!!!!!!!!!!
         found_truedec=0
         do ihep=1,nhep
c     works for POWHEG+HERWIG,POWHEG+PYHIA,HERWIG,PYTHIA and  real in MC@NLO
            if ((isthep(ihep).eq.1).and.(abs(idhep(ihep)).eq.11).and.
     #              ((idhep(jmohep(1,jmohep(1,ihep))).eq.23))) then
c     find first decay product
               if(idhep(ihep).eq.11) then
                  jem_true=ihep
                  found_truedec=found_truedec+1
c     find second decay product
               elseif(idhep(ihep).eq.-11) then
                  jep_true=ihep
                  found_truedec=found_truedec+1
               endif
            endif
         enddo
         if(found_truedec.ne.2) then
            write(*,*) 'lepton couple from Z decay not found'
            write(*,*) '!!!! Error !!!!'
            stop
         endif
      endif
      if(findmother.and.findinvmass) then
c     check the two analysis methods
c-----------------------------------
         if((jem_true.ne.jeminus).or.(jep_true.ne.jeplus)) then
            write(*,*) '--------------------------------'
            write(*,*) 'Error when looking for leptons from Z decay'
            write(*,*) 'Wrong inv mass: ',sqrt(mV2ref),
     #           '<--',jeminus,jeplus
            write(*,*) '\t e- -->',phep(4,jeminus),phep(1,jeminus),
     #           phep(2,jeminus),phep(3,jeminus)
            write(*,*) '\t e+ -->',phep(4,jeplus),phep(1,jeplus),
     #           phep(2,jeplus),phep(3,jeplus)
            write(*,*) 'True inv mass: ',sqrt(
     #           (phep(4,jem_true)+phep(4,jep_true))**2
     #           -(phep(1,jem_true)+phep(1,jep_true))**2
     #           -(phep(2,jem_true)+phep(2,jep_true))**2
     #           -(phep(3,jem_true)+phep(3,jep_true))**2   ),
     #           '<--',jem_true,jep_true
            write(*,*) '\t e- -->',phep(4,jem_true),phep(1,jem_true),
     #           phep(2,jem_true),phep(3,jem_true)
            write(*,*) '\t e+ -->',phep(4,jep_true),phep(1,jep_true),
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
      
      call pwhgfillup(1,ptvb,dsig)
c      call pwhgfillup(4,mvb,dsig)

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
            call pwhgfillup(2,ptj1,dsig)
            call pwhgfillup(52,ptj1,dsig)
         endif
         if(j2.gt.0) then
            call pwhgfillup(3,ptj2,dsig)
            call pwhgfillup(53,ptj2,dsig)
         endif
         
         if(ptvb.gt.10) call pwhgfillup(5,yvb,dsig)
         if(ptvb.gt.20) call pwhgfillup(6,yvb,dsig)
         if(ptvb.gt.40) call pwhgfillup(7,yvb,dsig)
         if(ptvb.gt.60) call pwhgfillup(8,yvb,dsig)
         if(ptvb.gt.80) call pwhgfillup(9,yvb,dsig)
         if(ptvb.gt.100) call pwhgfillup(10,yvb,dsig)
         if(ptvb.gt.150) call pwhgfillup(11,yvb,dsig)
         if(ptvb.gt.200) call pwhgfillup(12,yvb,dsig)
         if(ptvb.gt.300) call pwhgfillup(13,yvb,dsig)
         
         if(j1.gt.0) then
            if(ptj1.gt.10) call pwhgfillup(14,yj1,dsig)
            if(ptj1.gt.20) call pwhgfillup(15,yj1,dsig)
            if(ptj1.gt.40) call pwhgfillup(16,yj1,dsig)
            if(ptj1.gt.60) call pwhgfillup(17,yj1,dsig)
            if(ptj1.gt.80) call pwhgfillup(18,yj1,dsig)
            if(ptj1.gt.100) call pwhgfillup(19,yj1,dsig)
            if(ptj1.gt.150) call pwhgfillup(20,yj1,dsig)
            if(ptj1.gt.200) call pwhgfillup(21,yj1,dsig)
            if(ptj1.gt.300) call pwhgfillup(22,yj1,dsig)
         endif
         if(j2.gt.0) then
            if(ptj2.gt.10) call pwhgfillup(23,yvbj1-yj2,dsig)
            if(ptj2.gt.20) call pwhgfillup(24,yvbj1-yj2,dsig)
            if(ptj2.gt.40) call pwhgfillup(25,yvbj1-yj2,dsig)
            if(ptj2.gt.60) call pwhgfillup(26,yvbj1-yj2,dsig)
            if(ptj2.gt.80) call pwhgfillup(27,yvbj1-yj2,dsig)
            if(ptj2.gt.100) call pwhgfillup(28,yvbj1-yj2,dsig)
            if(ptj2.gt.150) call pwhgfillup(29,yvbj1-yj2,dsig)
            if(ptj2.gt.200) call pwhgfillup(30,yvbj1-yj2,dsig)
            if(ptj2.gt.300) call pwhgfillup(31,yvbj1-yj2,dsig)
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
               call pwhgfillup(54,ptrel,dsig)
            else
               call pwhgfillup(55,ptrel,dsig)
            endif
         enddo         
      endif            
c     endif buildjets
      endif
      
      call pwhgfillup(32,ptem,dsig)
      call pwhgfillup(33,ptep,dsig)
      
      if(ptem.gt.10) call pwhgfillup(34,yem,dsig)
      if(ptem.gt.20) call pwhgfillup(35,yem,dsig)
      if(ptem.gt.40) call pwhgfillup(36,yem,dsig)
      if(ptem.gt.60) call pwhgfillup(37,yem,dsig)
      if(ptem.gt.80) call pwhgfillup(38,yem,dsig)
      if(ptem.gt.100) call pwhgfillup(39,yem,dsig)
      
      if(ptep.gt.10) call pwhgfillup(40,yep,dsig)
      if(ptep.gt.20) call pwhgfillup(41,yep,dsig)
      if(ptep.gt.40) call pwhgfillup(42,yep,dsig)
      if(ptep.gt.60) call pwhgfillup(43,yep,dsig)
      if(ptep.gt.80) call pwhgfillup(44,yep,dsig)
      if(ptep.gt.100) call pwhgfillup(45,yep,dsig)
      
      call pwhgfillup(46,yem,dsig)
      call pwhgfillup(47,yep,dsig)
      
      call pwhgfillup(48,ptem,dsig)
      call pwhgfillup(49,ptep,dsig)
      call pwhgfillup(50,ptvb,dsig)
      call pwhgfillup(51,ptvb,dsig)
      
      call pwhgfillup(56,atan2(pvb(2),pvb(1)),dsig)
      
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



      subroutine pwhgfillup(n,x,y)
      implicit none
      real * 8 x,y
      integer n
      call pwhgfill(n,x,y)
      call pwhgfill(n+100,x,y*y)
      end


      subroutine topout
      implicit none
      include '../include/hepevt.h'
      character * 50 title
      integer i
      integer maxnumplot
      common/cmaxnumplot/maxnumplot
c     
c     If histogram I contains accumulated weights and
c     histogram I+100 contains its squared values,
c     then a temporary copy of both is made in order
c     to safely have intermediate results

      do i=1,maxnumplot
	call pwhgfinal(i)
        call pwhgcopy(i,i+200)
        call pwhgcopy(i+100,i+300)
        call pwhgopera(i+200,'F',i+200,i+200,1d0/dble(nevhep),0d0)
        call pwhgerror(i+200,i+300,dble(nevhep))
        call pwhgfinal(i+200)
        call pwhgfinal(i+300)
      enddo
      do i=1,maxnumplot
         call pwhggettitle(i+200,title)
         call pwhgmultitop(i+200,i+300,2,3,title,' ','LOG')
      enddo
      end            


      SUBROUTINE PWHGERROR(I,J,N)
      implicit none
      include '../pwhg_book.h'
      integer i,j,l
      real * 8 N,avg2
     
      DO L=1,NBIN(I)
         avg2=hist(i,l)**2
         hist(j,l)=sqrt(abs(hist(j,l)/N-avg2))/sqrt(N)
      enddo
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
      
