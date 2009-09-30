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
      character * 50 cuts
      integer i,m,j,k
      character * 3 part,apart
      logical applycuts


      call pwhginihist

      part=' e+'
      apart=' ve' 
      
      applycuts=.false.
      m=0
      do i=1,1
         if(i.eq.2) then
            applycuts=.true.
            m=50
         endif
         if (applycuts) then
            cuts=' cuts: pt_lep>20 GeV, |y_lep|<2.5'
         else
            cuts=' '
         endif

         do j=1,2
            k=0
            if (j.eq.2) k=100
            call pwhgbook(1+k+m,'pt'//part//cuts,2d0,0d0,200d0)
            call pwhgbook(2+k+m,'y'//part//cuts,0.4d0,-3d0,3d0)
            call pwhgbook(3+k+m,'pt'//apart//cuts,2d0,0d0,200d0)
            call pwhgbook(4+k+m,'y'//apart//cuts,0.4d0,-3d0,3d0)
            call pwhgbook(5+k+m,'pt W '//cuts,4d0,0d0,200d0)
            call pwhgbook(6+k+m,'pt W '//cuts,2d-1,0d0,20d0)
            call pwhgbook(7+k+m,'inv. mass  W '//cuts,1d0,50d0,110d0)
            call pwhgbook(8+k+m,'y W '//cuts,0.2d0,-4d0,4d0)
            call pwhgbook(9+k+m,'cos(th*)*sign(y_W)'//cuts,2d0/30,
     #           -1d0,1d0)
            call pwhgbook(10+k+m,'deltaphi  Pt_W'//part//cuts,pi/30,
     #           0d0,pi)
            call pwhgbook(11+k+m,'deltaphi '//part//apart//cuts,
     #           pi/30,0d0,pi)
            call pwhgbook(12+k+m,'log10(pi-deltaphi) '
     #           //part//apart//cuts,1d-1,-4d0,1d0)
c     cone algo
            call pwhgbook(13+k+m,'Pt_jet   cone algo'//
     #           ' Et_jet>10GeV'//cuts,6d0,0d0,300d0)
            call pwhgbook(14+k+m,'y jet   cone algo'//
     #           ' Et_jet>10GeV ',.2d0,-5d0,5d0)
            call pwhgbook(15+k+m,'Delta_y W-jet  cone algo'//
     #           ' Et_jet>10GeV'//cuts
     #           ,.2d0,-5d0,5d0)
            call pwhgbook(16+k+m,'Pt_jet   cone algo'//
     #           ' Et_jet>20GeV'//cuts,6d0,0d0,300d0)
            call pwhgbook(17+k+m,'y jet   cone algo'//
     #           ' Et_jet>20GeV ',.2d0,-5d0,5d0)
            call pwhgbook(18+k+m,'Delta_y W-jet  cone algo'//
     #           ' Et_jet>20GeV'//cuts
     #           ,.2d0,-5d0,5d0)
            call pwhgbook(19+k+m,'Pt_jet   cone algo'//
     #           ' Et_jet>40GeV'//cuts,6d0,0d0,300d0)
            call pwhgbook(20+k+m,'y jet   cone algo'//
     #           ' Et_jet>40GeV ',.2d0,-5d0,5d0)
            call pwhgbook(21+k+m,'Delta_y W-jet  cone algo'//
     #           ' Et_jet>40GeV'//cuts
     #           ,.2d0,-5d0,5d0)
            call pwhgbook(22+k+m,'Pt_jet   cone algo'//
     #           ' Et_jet>60GeV'//cuts,6d0,0d0,300d0)
            call pwhgbook(23+k+m,'y jet   cone algo'//
     #           ' Et_jet>60GeV ',.2d0,-5d0,5d0)
            call pwhgbook(24+k+m,'Delta_y W-jet  cone algo'//
     #           ' Et_jet>60GeV'//cuts
     #           ,.2d0,-5d0,5d0)
            call pwhgbook(25+k+m,'Pt_jet   cone algo'//
     #           ' Et_jet>80GeV'//cuts,6d0,0d0,300d0)
            call pwhgbook(26+k+m,'y jet   cone algo'//
     #           ' Et_jet>80GeV ',.2d0,-5d0,5d0)
            call pwhgbook(27+k+m,'Delta_y W-jet  cone algo'//
     #           ' Et_jet>80GeV'//cuts
     #           ,.2d0,-5d0,5d0)
            call pwhgbook(28+k+m,'Pt_jet   cone algo'//
     #           ' Et_jet>100GeV'//cuts,6d0,0d0,300d0)
            call pwhgbook(29+k+m,'y jet   cone algo'//
     #           ' Et_jet>100GeV ',.2d0,-5d0,5d0)
            call pwhgbook(30+k+m,'Delta_y W-jet  cone algo'//
     #           ' Et_jet>100GeV'//cuts
     #           ,.2d0,-5d0,5d0)
c     kt algo
            call pwhgbook(31+k+m,'Pt_jet   kt algo'//
     #           ' Et_jet>10GeV'//cuts,6d0,0d0,300d0)
            call pwhgbook(32+k+m,'y jet   kt algo'//
     #           ' Et_jet>10GeV ',.2d0,-5d0,5d0)
            call pwhgbook(33+k+m,'Delta_y W-jet  kt algo'//
     #           ' Et_jet>10GeV'//cuts
     #           ,.2d0,-5d0,5d0)
            call pwhgbook(34+k+m,'Pt_jet   kt algo'//
     #           ' Et_jet>20GeV'//cuts,6d0,0d0,300d0)
            call pwhgbook(35+k+m,'y jet   kt algo'//
     #           ' Et_jet>20GeV ',.2d0,-5d0,5d0)
            call pwhgbook(36+k+m,'Delta_y W-jet  kt algo'//
     #           ' Et_jet>20GeV'//cuts
     #           ,.2d0,-5d0,5d0)
            call pwhgbook(37+k+m,'Pt_jet   kt algo'//
     #           ' Et_jet>40GeV'//cuts,6d0,0d0,300d0)
            call pwhgbook(38+k+m,'y jet   kt algo'//
     #           ' Et_jet>40GeV ',.2d0,-5d0,5d0)
            call pwhgbook(39+k+m,'Delta_y W-jet  kt algo'//
     #           ' Et_jet>40GeV'//cuts
     #           ,.2d0,-5d0,5d0)
            call pwhgbook(40+k+m,'Pt_jet   kt algo'//
     #           ' Et_jet>60GeV'//cuts,6d0,0d0,300d0)
            call pwhgbook(41+k+m,'y jet   kt algo'//
     #           ' Et_jet>60GeV ',.2d0,-5d0,5d0)
            call pwhgbook(42+k+m,'Delta_y W-jet  kt algo'//
     #           ' Et_jet>60GeV'//cuts
     #           ,.2d0,-5d0,5d0)
            call pwhgbook(43+k+m,'Pt_jet   kt algo'//
     #           ' Et_jet>80GeV'//cuts,6d0,0d0,300d0)
            call pwhgbook(44+k+m,'y jet   kt algo'//
     #           ' Et_jet>80GeV ',.2d0,-5d0,5d0)
            call pwhgbook(45+k+m,'Delta_y W-jet  kt algo'//
     #           ' Et_jet>80GeV'//cuts
     #           ,.2d0,-5d0,5d0)
            call pwhgbook(46+k+m,'Pt_jet   kt algo'//
     #           ' Et_jet>100GeV'//cuts,6d0,0d0,300d0)
            call pwhgbook(47+k+m,'y jet   kt algo'//
     #           ' Et_jet>100GeV ',.2d0,-5d0,5d0)
            call pwhgbook(48+k+m,'Delta_y W-jet  kt algo'//
     #           ' Et_jet>100GeV'//cuts
     #           ,.2d0,-5d0,5d0)            
         enddo
      enddo
      
      
      maxnumplot = 48

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
      real * 8 xwgtup
      include '../include/hepevt.h'
      real * 8 pi
      parameter (pi=3.141592653589793238462643383279502884197D0)
      integer vdecaytemp,vdecay2temp
      real*8 m12,pt12,y12,ye1,ye2
      real*8 e1,e2,px1,px2,py1,py2,pz1,pz2,p1,p2,pt1,pt2
      integer j
      integer j1,j2
      real * 8 beta(1:3),pep(0:3),pw(0:3)
      real *8 cosths,deltaphi,deltaphi2
      integer ifs,ihep
      integer maxtrack,maxjet
      parameter (maxtrack=2048)
      parameter (maxjet=2048)
      
      real *8 jetetcut,frenco,yktcut
      real *8 ktdeno,hacone,rparam

      integer njet,nsub,hj
      integer i,m,found,ij

      real *8 ptrack(4,maxtrack)
      real *8 ptjet,yjet,tmp
      real *8 yjetcut(maxjet)
      real *8 pjet(4,maxjet) 
      integer jetvec(maxtrack)
      integer ierr,ipass(maxtrack)
      integer ijmul(maxjet),macjet(maxjet)

      logical buildjets
      parameter (buildjets=.true.)

      integer ini
      data ini/0/
      save ini

      real * 8 ptlepcut,ylepcut
      parameter (ptlepcut=20d0) ! leptons pt cut
      parameter (ylepcut=2.5d0) ! leptons rapidity cut
      logical cutspassed
      
      xwgtup=dsig
c    
      vdecaytemp=-11 ! id of the charged decay product of the W
      if(vdecaytemp.lt.0) then  ! id of the neutral decay product of the W
         vdecay2temp=-vdecaytemp+1 
      elseif(vdecaytemp.gt.0) then
         vdecay2temp=-(vdecaytemp+1)
      else
         write(*,*) 'Error in decay mode in analize-W'
         stop
      endif

      ini=ini+1
      m=50

c     useful parameters
      
      jetetcut=10d0    ! jet transverse energy cut
      
      hacone=0.35d0  ! half angle of cone (in rad)
      frenco=0.05d0  ! energy fraction for overlapping jets in cone algo

      yktcut=jetetcut**2 !y scale cut for kt algo in jet recostruction
      ktdeno=1d0       !denominator of kt measure 
      rparam=2.8d0*hacone ! jet radius fot kt algo : 40% bigger than 2*half cone    

c     set to zero everything
      do i=1,maxtrack
         do j=1,4
            ptrack(j,i)=0d0
         enddo
         ipass(i)=0
         jetvec(i)=0
      enddo
      do i=1,maxjet
         do j=1,4
            pjet(j,i)=0d0
         enddo
         ijmul(i)=0
         yjetcut(i)=0d0
         macjet(i)=0
      enddo
      
      j1=0
      j2=0
      found=0
      ifs=0
      njet=0

c     Loop over final state particles to find products of W decay
      do ihep=1,nhep
         if ((isthep(ihep).eq.1)
c.and.(
cc     works for POWHEG+HERWIG,POWHEG+PYHIA,HERWIG,PYTHIA and  real in MC@NLO
c     #    (abs(idhep(jmohep(1,jmohep(1,ihep)))).eq.24).or.
cc     works for born in MC@NLO
c     #    ((abs(idhep(jmohep(1,jmohep(1,ihep)))).eq.0).and.
c     #    (isthep(jmohep(1,jmohep(1,ihep))).eq.120))
c     #    )) then
     #    ) then

c     find first decay product
            if(idhep(ihep).eq.vdecaytemp) then
               j1=ihep
               found=found+1
c     find second decay product
            elseif(idhep(ihep).eq.vdecay2temp) then
               j2=ihep
               found=found+1
            endif
         endif
      enddo
      
      if(found.lt.2) then
         write(*,*) 'ERROR: products of W decay not found'
         stop
      elseif(found.gt.2) then
          write(*,*) 'ERROR: more W-like decay products found'
         stop
      endif
      
c Loop over final state particles to find jets (excluding products of W decay)
      do ihep=1,nhep
         if ((isthep(ihep).eq.1).and.((ihep.ne.j1).and.(ihep.ne.j2)))
     #           then
            if(ifs.eq.maxtrack) then
               write(*,*)
     #              'hwanal: too many particles, increase maxtrack'
               stop
            endif
c     copy momenta to construct jets 
            ifs=ifs+1
            do j=1,4
               ptrack(j,ifs)=phep(j,ihep)
            enddo
         endif
      enddo

c     positron
      e1=phep(4,j1)
      px1=phep(1,j1)
      py1=phep(2,j1)
      pz1=phep(3,j1)
      p1=sqrt(px1**2+py1**2+pz1**2)
      pt1=sqrt(px1**2+py1**2)
      ye1=log((e1+pz1)/(e1-pz1))/2d0
      
c     neutrino      
      e2=phep(4,j2)
      px2=phep(1,j2)
      py2=phep(2,j2)
      pz2=phep(3,j2)
      p2=sqrt(px2**2+py2**2+pz2**2)
      pt2=sqrt(px2**2+py2**2)
      ye2=log((e2+pz2)/(e2-pz2))/2d0

c     APPLY CUTS ON LEPTON MOMENTA
      cutspassed=.false.
c      if (((pt1.ge.ptlepcut).and.(abs(ye1).le.ylepcut).and.
c     #        ((pt2.ge.ptlepcut).and.(abs(ye2).le.ylepcut)))) then
c         cutspassed=.true.
c      endif         
      
      call pwhgfillup(1,pt1,xwgtup)
      call pwhgfillup(2,ye1,xwgtup)
      call pwhgfillup(3,pt2,xwgtup)
      call pwhgfillup(4,ye2,xwgtup)      
      if(cutspassed) then
         call pwhgfillup(1+m,pt1,xwgtup)
         call pwhgfillup(2+m,ye1,xwgtup)
         call pwhgfillup(3+m,pt2,xwgtup)
         call pwhgfillup(4+m,ye2,xwgtup)
      endif         
      
c     reconstructed w
      pt12=sqrt((px1+px2)**2+(py1+py2)**2)
      m12=sqrt((e1+e2)**2-(px1+px2)**2-(py1+py2)**2-(pz1+pz2)**2)
      y12=log((e1+e2+pz1+pz2)/(e1+e2-pz1-pz2))/2d0
      call pwhgfillup(5,pt12,xwgtup)
      call pwhgfillup(6,pt12,xwgtup)
      call pwhgfillup(7,m12,xwgtup)
      call pwhgfillup(8,y12,xwgtup)
      if(cutspassed) then
         call pwhgfillup(5+m,pt12,xwgtup)
         call pwhgfillup(6+m,pt12,xwgtup)
         call pwhgfillup(7+m,m12,xwgtup)
         call pwhgfillup(8+m,y12,xwgtup)        
      endif   

c     acceptance
c      if((abs(ye1).lt.2.5d0).and.(e2.gt.20d0).and.(pt1.gt.20d0)) then
c         do ij=1,60
c            if ((20d0+(ij-0.5d0)).lt.pt1) then
c               call pwhgfillup(49,20d0+(ij-0.5),1d0)
c            endif
c         enddo
c      endif
               
c     cosths is the cos of the angle between positron and z axis
c     in the W rest frame reached with a composition
c     of a longitudinal and a transverse boost      
      pep(0)=e1
      pw(0)=e1+e2
      do j=1,3
         pep(j)=phep(j,j1)
         pw(j)=phep(j,j1)+phep(j,j2)
      enddo
      
c     longitudinal boost: x,y components unchanged
      beta(1)=0d0
      beta(2)=0d0
      beta(3)=-pw(3)/pw(0)
      call boost(beta,pep,pep)
      call boost(beta,pw,pw)
      
c     transverse boost: z component unchanged
      beta(1)=-pw(1)/pw(0)
      beta(2)=-pw(2)/pw(0)
      beta(3)=0d0
      call boost(beta,pep,pep)
      
      cosths=pep(3)/sqrt(pep(1)**2+pep(2)**2+pep(3)**2)
      
      call pwhgfillup(9,cosths*sign(1d0,y12),xwgtup)
      if(cutspassed) call pwhgfillup(9+m,cosths*sign(1d0,y12),xwgtup)
c     azimuthal separation  W e+    
      deltaphi=abs(atan2(py1,px1)-atan2(pw(2),pw(1)))
      deltaphi=min(deltaphi,2d0*pi-deltaphi)
      call pwhgfillup(10,deltaphi,xwgtup)
      if(cutspassed)  call pwhgfillup(10+m,deltaphi,xwgtup)
c     azimuthal separation e+ ve     
      deltaphi2=abs(atan2(py1,px1)-atan2(py2,px2))
      deltaphi2=min(deltaphi2,2d0*pi-deltaphi2)
      call pwhgfillup(11,deltaphi2,xwgtup)
      if(cutspassed) call pwhgfillup(11+m,deltaphi2,xwgtup)
      if ((pi-deltaphi2).gt.1d-13) then
         call pwhgfillup(12,log10(pi-deltaphi2),xwgtup)
         if(cutspassed) then
            call pwhgfillup(12+m,log10(pi-deltaphi2),xwgtup)
         endif
      endif


      if(buildjets.and.ifs.gt.0) then
************************************************************************
*     cone algorithm
**********************************************************************
         call pxcone(2,ifs,4,ptrack,hacone,jetetcut,frenco,maxjet,
     #        njet,pjet,ipass,ijmul,ierr)
         
         if (njet.gt.0) then
            ptjet=0d0
            hj=0
            do i=1,njet
c............find the hardest jet
               tmp=sqrt(pjet(1,i)**2 + pjet(2,i)**2)
               if (tmp.gt.ptjet) then
                  ptjet=tmp
                  hj=i
               endif
            enddo
            if(ptjet.gt.10d0) call pwhgfillup(13,ptjet,xwgtup)
            if(ptjet.gt.20d0) call pwhgfillup(16,ptjet,xwgtup)
            if(ptjet.gt.40d0) call pwhgfillup(19,ptjet,xwgtup)
            if(ptjet.gt.60d0) call pwhgfillup(22,ptjet,xwgtup)
            if(ptjet.gt.80d0) call pwhgfillup(25,ptjet,xwgtup)
            if(ptjet.gt.100d0) call pwhgfillup(28,ptjet,xwgtup)
            if(cutspassed) then
               if(ptjet.gt.10d0) call pwhgfillup(13+m, ptjet,xwgtup)
               if(ptjet.gt.20d0) call pwhgfillup(16+m ,ptjet,xwgtup)
               if(ptjet.gt.40d0) call pwhgfillup(19+m ,ptjet,xwgtup)
               if(ptjet.gt.60d0) call pwhgfillup(22+m ,ptjet,xwgtup)
               if(ptjet.gt.80d0) call pwhgfillup(25+m ,ptjet,xwgtup)
               if(ptjet.gt.100d0) call pwhgfillup(28+m ,ptjet,xwgtup)
            endif

            yjet=log((pjet(4,hj)+pjet(3,hj))
     #           /(pjet(4,hj)-pjet(3,hj)))/2d0

            if(ptjet.gt.10d0 )call pwhgfillup(14,yjet,xwgtup)
            if(ptjet.gt.20d0) call pwhgfillup(17,yjet,xwgtup)
            if(ptjet.gt.40d0) call pwhgfillup(20,yjet,xwgtup)
            if(ptjet.gt.60d0) call pwhgfillup(23,yjet,xwgtup)
            if(ptjet.gt.80d0) call pwhgfillup(26,yjet,xwgtup)
            if(ptjet.gt.100d0) call pwhgfillup(29,yjet,xwgtup)
            if(cutspassed) then
               if(ptjet.gt.10d0) call pwhgfillup(14+m, yjet,xwgtup)
               if(ptjet.gt.20d0) call pwhgfillup(17+m ,yjet,xwgtup)
               if(ptjet.gt.40d0) call pwhgfillup(20+m ,yjet,xwgtup)
               if(ptjet.gt.60d0) call pwhgfillup(23+m ,yjet,xwgtup)
               if(ptjet.gt.80d0) call pwhgfillup(26+m ,yjet,xwgtup)
               if(ptjet.gt.100d0) call pwhgfillup(29+m ,yjet,xwgtup)
            endif

            if(ptjet.gt.10d0)  call pwhgfillup(15,yjet-y12,xwgtup)
            if(ptjet.gt.20d0) call pwhgfillup(18,yjet-y12,xwgtup)
            if(ptjet.gt.40d0) call pwhgfillup(21,yjet-y12,xwgtup)
            if(ptjet.gt.60d0) call pwhgfillup(24,yjet-y12,xwgtup)
            if(ptjet.gt.80d0) call pwhgfillup(27,yjet-y12,xwgtup)
            if(ptjet.gt.100d0) call pwhgfillup(30,yjet-y12,xwgtup)
            if(cutspassed) then
               if(ptjet.gt.10d0) call pwhgfillup(15+m,yjet-y12,xwgtup)
               if(ptjet.gt.20d0) call pwhgfillup(18+m ,yjet-y12,xwgtup)
               if(ptjet.gt.40d0) call pwhgfillup(21+m ,yjet-y12,xwgtup)
               if(ptjet.gt.60d0) call pwhgfillup(24+m ,yjet-y12,xwgtup)
               if(ptjet.gt.80d0) call pwhgfillup(27+m ,yjet-y12,xwgtup)
               if(ptjet.gt.100d0) call pwhgfillup(30+m ,yjet-y12,xwgtup)
            endif

         endif

************************************************************************
*     kt algorithm
**********************************************************************
         call ktclur(7,ptrack,ifs,rparam,ktdeno,yjetcut,666)
         call ktwich(ktdeno,yktcut,jetvec,njet)
         if(njet.ge.maxjet) then
            write(*,*)
     #        'hwanal: too many jets found in kt algo,increase maxjet'
            stop
         endif

         if (njet.gt.0)then            
            call ktreco(3,ptrack,ifs,ktdeno,yktcut,1d0
     #           ,pjet,macjet,njet,nsub)            
            ptjet=0d0
            hj=0
            do i=1,njet
c............find the hardest jet
               tmp=sqrt(pjet(1,i)**2 + pjet(2,i)**2)
               if (tmp.gt.ptjet) then
                  ptjet=tmp
                  hj=i
               endif
            enddo
            
            if(ptjet.gt.10d0) call pwhgfillup(31,ptjet,xwgtup)
            if(ptjet.gt.20d0) call pwhgfillup(34,ptjet,xwgtup)
            if(ptjet.gt.40d0) call pwhgfillup(37,ptjet,xwgtup)
            if(ptjet.gt.60d0) call pwhgfillup(40,ptjet,xwgtup)
            if(ptjet.gt.80d0) call pwhgfillup(43,ptjet,xwgtup)
            if(ptjet.gt.100d0) call pwhgfillup(46,ptjet,xwgtup)
            if(cutspassed) then
               if(ptjet.gt.10d0) call pwhgfillup(31+m, ptjet,xwgtup)
               if(ptjet.gt.20d0) call pwhgfillup(34+m ,ptjet,xwgtup)
               if(ptjet.gt.40d0) call pwhgfillup(37+m ,ptjet,xwgtup)
               if(ptjet.gt.60d0) call pwhgfillup(40+m ,ptjet,xwgtup)
               if(ptjet.gt.80d0) call pwhgfillup(43+m ,ptjet,xwgtup)
               if(ptjet.gt.100d0) call pwhgfillup(46+m ,ptjet,xwgtup)
            endif
            
            yjet=log((pjet(4,hj)+pjet(3,hj))
     #           /(pjet(4,hj)-pjet(3,hj)))/2d0

            if(ptjet.gt.10d0)call pwhgfillup(32,yjet,xwgtup)
            if(ptjet.gt.20d0) call pwhgfillup(35,yjet,xwgtup)
            if(ptjet.gt.40d0) call pwhgfillup(38,yjet,xwgtup)
            if(ptjet.gt.60d0) call pwhgfillup(41,yjet,xwgtup)
            if(ptjet.gt.80d0) call pwhgfillup(44,yjet,xwgtup)
            if(ptjet.gt.100d0) call pwhgfillup(47,yjet,xwgtup)
            if(cutspassed) then
               if(ptjet.gt.10d0) call pwhgfillup(32+m, yjet,xwgtup)
               if(ptjet.gt.20d0) call pwhgfillup(35+m ,yjet,xwgtup)
               if(ptjet.gt.40d0) call pwhgfillup(38+m ,yjet,xwgtup)
               if(ptjet.gt.60d0) call pwhgfillup(41+m ,yjet,xwgtup)
               if(ptjet.gt.80d0) call pwhgfillup(44+m ,yjet,xwgtup)
               if(ptjet.gt.100d0) call pwhgfillup(47+m ,yjet,xwgtup)
            endif

            if(ptjet.gt.10d0) call pwhgfillup(33,yjet-y12,xwgtup)
            if(ptjet.gt.20d0) call pwhgfillup(36,yjet-y12,xwgtup)
            if(ptjet.gt.40d0) call pwhgfillup(39,yjet-y12,xwgtup)
            if(ptjet.gt.60d0) call pwhgfillup(42,yjet-y12,xwgtup)
            if(ptjet.gt.80d0) call pwhgfillup(45,yjet-y12,xwgtup)
            if(ptjet.gt.100d0) call pwhgfillup(48,yjet-y12,xwgtup)
            if(cutspassed) then
               if(ptjet.gt.10d0) call pwhgfillup(33+m,yjet-y12,xwgtup)
               if(ptjet.gt.20d0) call pwhgfillup(36+m ,yjet-y12,xwgtup)
               if(ptjet.gt.40d0) call pwhgfillup(39+m ,yjet-y12,xwgtup)
               if(ptjet.gt.60d0) call pwhgfillup(42+m ,yjet-y12,xwgtup)
               if(ptjet.gt.80d0) call pwhgfillup(45+m ,yjet-y12,xwgtup)
               if(ptjet.gt.100d0) call pwhgfillup(48+m ,yjet-y12,xwgtup)
            endif

            goto 999
 666        write(*,*) 'WARNING: Too many tracks in kt algo.'// 
     #           'Event not processed'
 999        continue
         endif
      endif
      END


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
      




C+PATCH,PXCONE.
C+DECK,PXCONE.
      SUBROUTINE PXCONE(MODE,NTRAK,ITKDM,PTRAK,CONER,EPSLON,OVLIM,MXJET,
     +                   NJET,PJET,IPASS,IJMUL,IERR)
*.*********************************************************
*. ------
*. PXCONE
*. ------
*.
*.********** Pre Release Version 26.2.93
*.
*. Driver for the Cone  Jet finding algorithm of L.A. del Pozo.
*. Based on algorithm from D.E. Soper.
*. Finds jets inside cone of half angle CONER with energy > EPSLON.
*. Jets which receive more than a fraction OVLIM of their energy from
*. overlaps with other jets are excluded.
*. Output jets are ordered in energy.
*. If MODE.EQ.2 momenta are stored as (eta,phi,<empty>,pt)
*. Usage     :
*.
*.      INTEGER  ITKDM,MXTRK
*.      PARAMETER  (ITKDM=4.or.more,MXTRK=1.or.more)
*.      INTEGER  MXJET, MXTRAK, MXPROT
*.      PARAMETER  (MXJET=10,MXTRAK=500,MXPROT=500)
*.      INTEGER  IPASS (MXTRAK),IJMUL (MXJET)
*.      INTEGER  NTRAK,NJET,IERR,MODE
*.      DOUBLE PRECISION  PTRAK (ITKDM,MXTRK),PJET (4,MXJET)
*.      DOUBLE PRECISION  CONER, EPSLON, OVLIM
*.      NTRAK = 1.to.MXTRAK
*.      CONER   = ...
*.      EPSLON  = ...
*.      OVLIM   = ...
*.      CALL PXCONE (MODE,NTRAK,ITKDM,PTRAK,CONER,EPSLON,OVLIM,MXJET,
*.     +             NJET,PJET,IPASS,IJMUL,IERR)
*.
*. INPUT     :  MODE      1=>e+e-, 2=>hadron-hadron
*. INPUT     :  NTRAK     Number of particles
*. INPUT     :  ITKDM     First dimension of PTRAK array
*. INPUT     :  PTRAK     Array of particle 4-momenta (Px,Py,Pz,E)
*. INPUT     :  CONER     Cone size (half angle) in radians
*. INPUT     :  EPSLON    Minimum Jet energy (GeV)
*. INPUT     :  OVLIM     Maximum fraction of overlap energy in a jet
*. INPUT     :  MXJET     Maximum possible number of jets
*. OUTPUT    :  NJET      Number of jets found
*. OUTPUT    :  PJET      4-vectors of jets
*. OUTPUT    :  IPASS(k)  Particle k belongs to jet number IPASS(k)
*.                        IPASS = -1 if not assosciated to a jet
*. OUTPUT    :  IJMUL(i)  Jet i contains IJMUL(i) particles
*. OUTPUT    :  IERR      = 0 if all is OK ;   = -1 otherwise
*.
*. CALLS     : PXSEAR, PXSAME, PXNEW, PXTRY, PXORD, PXUVEC, PXOLAP
*. CALLED    : User
*.
*. AUTHOR    :  L.A. del Pozo
*. CREATED   :  26-Feb-93
*. LAST MOD  :   2-Mar-93
*.
*. Modification Log.
*. 4-Apr-93: M H Seymour  - Change 2d arrays to 1d in PXTRY & PXNEW
*. 2-Apr-93: M H Seymour  - Major changes to add boost-invariant mode
*. 1-Apr-93: M H Seymour  - Increase all array sizes
*. 30-Mar-93: M H Seymour - Change all REAL variables to DOUBLE PRECISION
*. 30-Mar-93: M H Seymour - Change OVLIM into an input parameter
*. 2-Mar-93: L A del Pozo - Fix Bugs in PXOLAP
*. 1-Mar-93: L A del Pozo - Remove Cern library routine calls
*. 1-Mar-93: L A del Pozo - Add Print out of welcome and R and Epsilon
*.
*.*********************************************************
C+SEQ,DECLARE.
*** External Arrays
      INTEGER  ITKDM,MXJET,NTRAK,NJET,IERR,MODE
      INTEGER  IPASS (*),IJMUL (MXJET)
      DOUBLE PRECISION  PTRAK (ITKDM,*),PJET (4,*), CONER, EPSLON, OVLIM
*** Internal Arrays
      INTEGER MXPROT, MXTRAK
      PARAMETER (MXPROT=5000, MXTRAK=5000)
      DOUBLE PRECISION PP(4,MXTRAK), PU(3,MXTRAK), PJ(4,MXPROT)
      LOGICAL*1 JETLIS(MXPROT,MXTRAK)
*** Used in the routine.
      DOUBLE PRECISION COSR,COS2R, VSEED(3), VEC1(3), VEC2(3),PTSQ,PPSQ,
     +     COSVAL,PXMDPI
      LOGICAL*1 UNSTBL
      INTEGER I,J,N,MU,N1,N2, ITERR
      INTEGER NCALL, NPRINT
      DOUBLE PRECISION ROLD, EPSOLD, OVOLD
      SAVE NCALL,NPRINT,ROLD, EPSOLD, OVOLD
      DATA NCALL,NPRINT /0,0/
      IERR=0
*
*** INITIALIZE
      IF(NCALL.LE.0)  THEN
         ROLD = 0.
         EPSOLD = 0.
         OVOLD = 0.
      ENDIF
      NCALL = NCALL + 1
*
*** Print welcome and Jetfinder parameters
      IF((CONER.NE.ROLD .OR. EPSLON.NE.EPSOLD .OR. OVLIM.NE.OVOLD)
     +     .AND. NPRINT.LE.10) THEN
         WRITE(6,*)
         WRITE(6,1003)' ********** PXCONE: Cone Jet-finder **********'
         WRITE(6,1003)'    Written by Luis Del Pozo of OPAL'
         IF (MODE.EQ.2)
     $   WRITE(6,1003)'    Modified for eta-phi by Mike Seymour'
         WRITE(6,1000)'   Cone Size R = ',CONER,' Radians'
         WRITE(6,1001)'   Min Jet energy Epsilon = ',EPSLON,' GeV'
         WRITE(6,1002)'   Overlap fraction parameter = ',OVLIM
         WRITE(6,1003)' *********************************************'
         WRITE(6,*)
1000     FORMAT(A18,F5.2,A10)
1001     FORMAT(A29,F5.2,A5)
1002     FORMAT(A33,F5.2)
1003     FORMAT(A)
         NPRINT = NPRINT + 1
         ROLD=CONER
         EPSOLD=EPSLON
         OVOLD=OVLIM
      ENDIF
*
*** Copy calling array PTRAK  to internal array PP(4,NTRAK)
*
      IF (NTRAK .GT. MXTRAK) THEN
         WRITE (6,*) ' PXCONE: Ntrak too large'
         IERR=-1
         RETURN
      ENDIF
      IF (MODE.NE.2) THEN
         DO  100 I=1, NTRAK
            DO  101 J=1,4
               PP(J,I)=PTRAK(J,I)
101         CONTINUE
100      CONTINUE
      ELSE
*** Converting to eta,phi,pt if necessary
         DO  104 I=1,NTRAK
            PTSQ=PTRAK(1,I)**2+PTRAK(2,I)**2
            PPSQ=(SQRT(PTSQ+PTRAK(3,I)**2)+ABS(PTRAK(3,I)))**2
            IF (PTSQ.LE.4.25E-18*PPSQ) THEN
               PP(1,I)=20
            ELSE
               PP(1,I)=0.5*LOG(PPSQ/PTSQ)
            ENDIF
            PP(1,I)=SIGN(PP(1,I),PTRAK(3,I))
            IF (PTSQ.EQ.0) THEN
               PP(2,I)=0
            ELSE
               PP(2,I)=ATAN2(PTRAK(2,I),PTRAK(1,I))
            ENDIF
            PP(3,I)=0
            PP(4,I)=SQRT(PTSQ)
            PU(1,I)=PP(1,I)
            PU(2,I)=PP(2,I)
            PU(3,I)=PP(3,I)
104      CONTINUE
      ENDIF
*
*** Zero output variables
*
      NJET=0
      DO 102 I = 1, NTRAK
         DO 103 J = 1, MXPROT
           JETLIS(J,I) = .FALSE.
103      CONTINUE
102   CONTINUE
      CALL PXZERV(4*MXPROT,PJ)
      CALL PXZERI(MXJET,IJMUL)
*
      IF (MODE.NE.2) THEN
         COSR = COS(CONER)
         COS2R = COS(2*CONER)
      ELSE
*** Purely for convenience, work in terms of 1-R**2
         COSR = 1-CONER**2
         COS2R = 1-4*CONER**2
      ENDIF
      UNSTBL = .FALSE.
      IF (MODE.NE.2) THEN
         CALL PXUVEC(NTRAK,PP,PU,IERR)
         IF (IERR .NE. 0) RETURN
      ENDIF
*** Look for jets using particle diretions as seed axes
*
      DO 110 N = 1,NTRAK
        DO 120 MU = 1,3
          VSEED(MU) = PU(MU,N)
120     CONTINUE
        CALL PXSEAR(MODE,COSR,NTRAK,PU,PP,VSEED,
     &                   NJET,JETLIS,PJ,UNSTBL,IERR)
         IF (IERR .NE. 0) RETURN
110   CONTINUE
*** Now look between all pairs of jets as seed axes.
      NTMP=NJET
      DO 140 N1 = 1,NTMP-1
         VEC1(1)=PJ(1,N1)
         VEC1(2)=PJ(2,N1)
         VEC1(3)=PJ(3,N1)
         IF (MODE.NE.2) CALL PXNORV(3,VEC1,VEC1,ITERR)
         DO 150 N2 = N1+1,NTMP
            VEC2(1)=PJ(1,N2)
            VEC2(2)=PJ(2,N2)
            VEC2(3)=PJ(3,N2)
            IF (MODE.NE.2) CALL PXNORV(3,VEC2,VEC2,ITERR)
            CALL PXADDV(3,VEC1,VEC2,VSEED,ITERR)
            IF (MODE.NE.2) THEN
               CALL PXNORV(3,VSEED,VSEED,ITERR)
            ELSE
               VSEED(1)=VSEED(1)/2
               VSEED(2)=VSEED(2)/2
            ENDIF
C---ONLY BOTHER IF THEY ARE BETWEEN 1 AND 2 CONE RADII APART
            IF (MODE.NE.2) THEN
              COSVAL=VEC1(1)*VEC2(1)+VEC1(2)*VEC2(2)+VEC1(3)*VEC2(3)
            ELSE
              IF (ABS(VEC1(1)).GE.20.OR.ABS(VEC2(1)).GE.20) THEN
                COSVAL=-1000
              ELSE
                COSVAL=1-
     +               ((VEC1(1)-VEC2(1))**2+PXMDPI(VEC1(2)-VEC2(2))**2)
              ENDIF
            ENDIF
            IF (COSVAL.LE.COSR.AND.COSVAL.GE.COS2R)
     +      CALL PXSEAR(MODE,COSR,NTRAK,PU,PP,VSEED,NJET,
     +      JETLIS,PJ,UNSTBL,IERR)
            IF (IERR .NE. 0) RETURN
150      CONTINUE
140   CONTINUE
      IF (UNSTBL) THEN
        IERR=-1
        WRITE (6,*) ' PXCONE: Too many iterations to find a proto-jet'
        RETURN
      ENDIF
*** Now put the jet list into order by jet energy, eliminating jets
*** with energy less than EPSLON.
       CALL PXORD(EPSLON,NJET,NTRAK,JETLIS,PJ)
*
*** Take care of jet overlaps
       CALL PXOLAP(MODE,NJET,NTRAK,JETLIS,PJ,PP,OVLIM)
*
*** Order jets again as some have been eliminated, or lost energy.
       CALL PXORD(EPSLON,NJET,NTRAK,JETLIS,PJ)
*
*** All done!, Copy output into output arrays
      IF (NJET .GT. MXJET) THEN
         WRITE (6,*) ' PXCONE:  Found more than MXJET jets'
         IERR=-1
         GOTO 99
      ENDIF
      IF (MODE.NE.2) THEN
         DO 300 I=1, NJET
            DO 310 J=1,4
               PJET(J,I)=PJ(J,I)
310         CONTINUE
300      CONTINUE
      ELSE
         DO 315 I=1, NJET
            PJET(1,I)=PJ(4,I)*COS(PJ(2,I))
            PJET(2,I)=PJ(4,I)*SIN(PJ(2,I))
            PJET(3,I)=PJ(4,I)*SINH(PJ(1,I))
            PJET(4,I)=PJ(4,I)*COSH(PJ(1,I))
 315     CONTINUE
      ENDIF
      DO 320 I=1, NTRAK
         IPASS(I)=-1
         DO 330 J=1, NJET
            IF (JETLIS(J,I)) THEN
               IJMUL(J)=IJMUL(J)+1
               IPASS(I)=J
            ENDIF
330      CONTINUE
320   CONTINUE
99    RETURN
      END
C+DECK,PXSEAR.
******............................................................******
      SUBROUTINE PXSEAR(MODE,COSR,NTRAK,PU,PP,VSEED,NJET,
     +                JETLIS,PJ,UNSTBL,IERR)
******............................................................******
*
C+SEQ,DECLARE.
      INTEGER MXTRAK, MXPROT
      PARAMETER (MXTRAK=5000,MXPROT=5000)
      INTEGER NTRAK, IERR, MODE
      DOUBLE PRECISION COSR,PU(3,MXTRAK),PP(4,MXTRAK),VSEED(3)
      LOGICAL*1 UNSTBL
      LOGICAL*1 JETLIS(MXPROT,MXTRAK)
      INTEGER NJET
      DOUBLE PRECISION  PJ(4,MXPROT)
*** Using VSEED as a trial axis , look for a stable jet.
*** Check stable jets against those already found and add to PJ.
*** Will try up to MXITER iterations to get a stable set of particles
*** in the cone.
      INTEGER MU,N,ITER
      LOGICAL*1 PXSAME,PXNEW,OK
      LOGICAL*1 NEWLIS(MXTRAK),OLDLIS(MXTRAK)
      DOUBLE PRECISION OAXIS(3),NAXIS(3),PNEW(4)
      INTEGER MXITER
      PARAMETER(MXITER = 30)
*
      DO 100 MU=1,3
        OAXIS(MU) = VSEED(MU)
100   CONTINUE
      DO 110 N = 1,NTRAK
        OLDLIS(N) = .FALSE.
110   CONTINUE
      DO 120 ITER = 1,MXITER
        CALL PXTRY(MODE,COSR,NTRAK,PU,PP,OAXIS,NAXIS,PNEW,NEWLIS,OK)
*** Return immediately if there were no particles in the cone.
       IF (.NOT.OK) THEN
         RETURN
       ENDIF
       IF(PXSAME(NEWLIS,OLDLIS,NTRAK)) THEN
*** We have a stable jet.
             IF (PXNEW(NEWLIS,JETLIS,NTRAK,NJET)) THEN
*** And the jet is a new one. So add it to our arrays.
*** Check arrays are big anough...
             IF (NJET .EQ. MXPROT) THEN
             WRITE (6,*) ' PXCONE:  Found more than MXPROT proto-jets'
                IERR = -1
                RETURN
             ENDIF
               NJET = NJET + 1
               DO 130 N = 1,NTRAK
                 JETLIS(NJET,N) = NEWLIS(N)
130            CONTINUE
               DO 140 MU=1,4
                 PJ(MU,NJET)=PNEW(MU)
140          CONTINUE
             ENDIF
             RETURN
       ENDIF
*** The jet was not stable, so we iterate again
       DO 150 N=1,NTRAK
         OLDLIS(N)=NEWLIS(N)
150    CONTINUE
       DO 160 MU=1,3
         OAXIS(MU)=NAXIS(MU)
160    CONTINUE
120   CONTINUE
      UNSTBL = .TRUE.
      RETURN
      END
*
C+DECK,PXSAME.
******............................................................******
       LOGICAL*1 FUNCTION PXSAME(LIST1,LIST2,N)
******............................................................******
*
       LOGICAL*1 LIST1(*),LIST2(*)
       INTEGER N
*** Returns T if the first N elements of LIST1 are the same as the
*** first N elements of LIST2.
       INTEGER I
*
       PXSAME = .TRUE.
       DO 100 I = 1,N
        IF ( LIST1(I).NEQV.LIST2(I) ) THEN
          PXSAME = .FALSE.
          RETURN
        ENDIF
100    CONTINUE
       RETURN
       END
*
C+DECK,PXNEW.
******............................................................******
       LOGICAL*1 FUNCTION PXNEW(TSTLIS,JETLIS,NTRAK,NJET)
******............................................................******
*
      INTEGER MXTRAK,MXPROT
      PARAMETER (MXTRAK=5000,MXPROT=5000)
       INTEGER NTRAK,NJET
*** Note that although JETLIS is assumed to be a 2d array, it
*** it is used as 1d in this routine for efficiency
       LOGICAL*1 TSTLIS(MXTRAK),JETLIS(MXPROT*MXTRAK)
*** Checks to see if TSTLIS entries correspond to a jet already found
*** and entered in JETLIS
       INTEGER N, I, IN
       LOGICAL*1 MATCH
*
       PXNEW = .TRUE.
       DO 100 I = 1,NJET
          MATCH = .TRUE.
          IN=I-MXPROT
          DO 110 N = 1,NTRAK
            IN=IN+MXPROT
            IF(TSTLIS(N).NEQV.JETLIS(IN)) THEN
             MATCH = .FALSE.
             GO TO 100
            ENDIF
110       CONTINUE
          IF (MATCH) THEN
           PXNEW = .FALSE.
           RETURN
          ENDIF
100    CONTINUE
       RETURN
       END
*
C+DECK,PXTRY.
******............................................................******
       SUBROUTINE PXTRY(MODE,COSR,NTRAK,PU,PP,OAXIS,NAXIS,
     +                  PNEW,NEWLIS,OK)
******............................................................******
*
C+SEQ,DECLARE.
      INTEGER MXTRAK
      PARAMETER (MXTRAK=5000)
       INTEGER NTRAK,MODE
*** Note that although PU and PP are assumed to be 2d arrays, they
*** are used as 1d in this routine for efficiency
       DOUBLE PRECISION COSR,PU(3*MXTRAK),PP(4*MXTRAK),OAXIS(3),PXMDPI
       LOGICAL*1 OK
       LOGICAL*1 NEWLIS(MXTRAK)
       DOUBLE PRECISION NAXIS(3),PNEW(4)
*** Finds all particles in cone of size COSR about OAXIS direction.
*** Calculates 4-momentum sum of all particles in cone (PNEW) , and
*** returns this as new jet axis NAXIS (Both unit Vectors)
       INTEGER N,MU,NPU,NPP
       DOUBLE PRECISION COSVAL,NORMSQ,NORM
*
       OK = .FALSE.
       DO 100 MU=1,4
          PNEW(MU)=0.0
100    CONTINUE
       NPU=-3
       NPP=-4
       DO 110 N=1,NTRAK
          NPU=NPU+3
          NPP=NPP+4
          IF (MODE.NE.2) THEN
             COSVAL=0.0
             DO 120 MU=1,3
                COSVAL=COSVAL+OAXIS(MU)*PU(MU+NPU)
120          CONTINUE
          ELSE
             IF (ABS(PU(1+NPU)).GE.20.OR.ABS(OAXIS(1)).GE.20) THEN
                COSVAL=-1000
             ELSE
                COSVAL=1-
     +           ((OAXIS(1)-PU(1+NPU))**2+PXMDPI(OAXIS(2)-PU(2+NPU))**2)
             ENDIF
          ENDIF
          IF (COSVAL.GE.COSR)THEN
             NEWLIS(N) = .TRUE.
             OK = .TRUE.
             IF (MODE.NE.2) THEN
                DO 130 MU=1,4
                   PNEW(MU) = PNEW(MU) + PP(MU+NPP)
130             CONTINUE
             ELSE
                PNEW(1)=PNEW(1)
     +              + PP(4+NPP)/(PP(4+NPP)+PNEW(4))*(PP(1+NPP)-PNEW(1))
                PNEW(2)=PNEW(2)
     +              + PP(4+NPP)/(PP(4+NPP)+PNEW(4))
     +               *PXMDPI(PP(2+NPP)-PNEW(2))
                PNEW(4)=PNEW(4)+PP(4+NPP)
             ENDIF
          ELSE
             NEWLIS(N)=.FALSE.
          ENDIF
110   CONTINUE
*** If there are particles in the cone, calc new jet axis
       IF (OK) THEN
          IF (MODE.NE.2) THEN
             NORMSQ = 0.0
             DO 140 MU = 1,3
                NORMSQ = NORMSQ + PNEW(MU)**2
140          CONTINUE
             NORM = SQRT(NORMSQ)
          ELSE
             NORM = 1
          ENDIF
          DO 150 MU=1,3
             NAXIS(MU) = PNEW(MU)/NORM
150       CONTINUE
       ENDIF
       RETURN
       END
*
C+DECK,PXORD.
******............................................................******
       SUBROUTINE PXORD(EPSLON,NJET,NTRAK,JETLIS,PJ)
******............................................................******
*
*** Routine to put jets into order and eliminate tose less than EPSLON
C+SEQ,DECLARE.
      INTEGER MXTRAK,MXPROT
      PARAMETER (MXTRAK=5000,MXPROT=5000)
       INTEGER I, J, INDEX(MXPROT)
       DOUBLE PRECISION PTEMP(4,MXPROT), ELIST(MXPROT)
       INTEGER NJET,NTRAK
       LOGICAL*1 JETLIS(MXPROT,MXTRAK)
       LOGICAL*1 LOGTMP(MXPROT,MXTRAK)
       DOUBLE PRECISION EPSLON,PJ(4,MXPROT)
*** Puts jets in order of energy: 1 = highest energy etc.
*** Then Eliminate jets with energy below EPSLON
*
*** Copy input arrays.
      DO 100 I=1,NJET
         DO 110 J=1,4
            PTEMP(J,I)=PJ(J,I)
110      CONTINUE
         DO 120 J=1,NTRAK
            LOGTMP(I,J)=JETLIS(I,J)
120      CONTINUE
100   CONTINUE
      DO 150 I=1,NJET
         ELIST(I)=PJ(4,I)
150   CONTINUE
*** Sort the energies...
      CALL PXSORV(NJET,ELIST,INDEX,'I')
*** Fill PJ and JETLIS according to sort ( sort is in ascending order!!)
      DO 200 I=1, NJET
         DO 210 J=1,4
            PJ(J,I)=PTEMP(J,INDEX(NJET+1-I))
210      CONTINUE
         DO 220 J=1,NTRAK
            JETLIS(I,J)=LOGTMP(INDEX(NJET+1-I),J)
220      CONTINUE
200   CONTINUE
** Jets are now in order
*** Now eliminate jets with less than Epsilon energy
      DO 300, I=1, NJET
         IF (PJ(4,I) .LT. EPSLON) THEN
            NJET=NJET-1
            PJ(4,I)=0.
         ENDIF
300   CONTINUE
      RETURN
      END
C+DECK,PXUVEC.
*
******............................................................******
       SUBROUTINE PXUVEC(NTRAK,PP,PU,IERR)
******............................................................******
*
*** Routine to calculate unit vectors PU of all particles PP
C+SEQ,DECLARE.
      INTEGER MXTRAK
      PARAMETER (MXTRAK=5000)
      INTEGER NTRAK, IERR
      DOUBLE PRECISION PP(4,MXTRAK)
      DOUBLE PRECISION PU(3,MXTRAK)
      INTEGER N,MU
      DOUBLE PRECISION MAG
       DO 100 N=1,NTRAK
          MAG=0.0
          DO 110 MU=1,3
             MAG=MAG+PP(MU,N)**2
110       CONTINUE
          MAG=SQRT(MAG)
          IF (MAG.EQ.0.0) THEN
             WRITE(6,*)' PXCONE: An input particle has zero mod(p)'
             IERR=-1
             RETURN
          ENDIF
          DO 120 MU=1,3
           PU(MU,N)=PP(MU,N)/MAG
120       CONTINUE
100    CONTINUE
       RETURN
       END
*
C+DECK,PXOLAP.
******............................................................******
      SUBROUTINE PXOLAP(MODE,NJET,NTRAK,JETLIS,PJ,PP,OVLIM)
******............................................................******
*
*** Looks for particles assigned to more than 1 jet, and reassigns them
*** If more than a fraction OVLIM of a jet's energy is contained in
*** higher energy jets, that jet is neglected.
*** Particles assigned to the jet closest in angle (a la CDF, Snowmass).
C+SEQ,DECLARE.
      INTEGER MXTRAK, MXPROT
      PARAMETER (MXTRAK=5000,MXPROT=5000)
      INTEGER NJET, NTRAK, MODE
      LOGICAL*1 JETLIS(MXPROT,MXTRAK)
      DOUBLE PRECISION PJ(4,MXPROT),PP(4,MXTRAK),PXMDPI
      INTEGER I,J,N,MU
      LOGICAL*1 OVELAP
      DOUBLE PRECISION EOVER
      DOUBLE PRECISION OVLIM
      INTEGER ITERR, IJMIN, IJET(MXPROT), NJ
      DOUBLE PRECISION VEC1(3), VEC2(3), COST, THET, THMIN
*
      IF (NJET.LE.1) RETURN
*** Look for jets with large overlaps with higher energy jets.
      DO 100 I = 2,NJET
*** Find overlap energy between jets I and all higher energy jets.
       EOVER = 0.0
       DO 110 N = 1,NTRAK
         OVELAP = .FALSE.
         DO 120 J= 1,I-1
           IF (JETLIS(I,N).AND.JETLIS(J,N)) THEN
            OVELAP = .TRUE.
           ENDIF
120      CONTINUE
         IF (OVELAP) THEN
           EOVER = EOVER + PP(4,N)
         ENDIF
110     CONTINUE
*** Is the fraction of energy shared larger than OVLIM?
        IF (EOVER.GT.OVLIM*PJ(4,I)) THEN
*** De-assign all particles from Jet I
            DO 130 N = 1,NTRAK
              JETLIS(I,N) = .FALSE.
130         CONTINUE
         ENDIF
100   CONTINUE
*** Now there are no big overlaps, assign every particle in
*** more than 1 jet to the closet jet.
*** Any particles now in more than 1 jet are assigned to the CLOSET
*** jet (in angle).
      DO 140 I=1,NTRAK
         NJ=0
         DO 150 J=1, NJET
         IF(JETLIS(J,I)) THEN
            NJ=NJ+1
            IJET(NJ)=J
         ENDIF
150      CONTINUE
         IF (NJ .GT. 1) THEN
*** Particle in > 1 jet - calc angles...
            VEC1(1)=PP(1,I)
            VEC1(2)=PP(2,I)
            VEC1(3)=PP(3,I)
            THMIN=0.
            DO 160 J=1,NJ
               VEC2(1)=PJ(1,IJET(J))
               VEC2(2)=PJ(2,IJET(J))
               VEC2(3)=PJ(3,IJET(J))
               IF (MODE.NE.2) THEN
                  CALL PXANG3(VEC1,VEC2,COST,THET,ITERR)
               ELSE
                  THET=(VEC1(1)-VEC2(1))**2+PXMDPI(VEC1(2)-VEC2(2))**2
               ENDIF
               IF (J .EQ. 1) THEN
                  THMIN=THET
                  IJMIN=IJET(J)
               ELSEIF (THET .LT. THMIN) THEN
                  THMIN=THET
                  IJMIN=IJET(J)
               ENDIF
160         CONTINUE
*** Assign track to IJMIN
            DO 170 J=1,NJET
               JETLIS(J,I) = .FALSE.
170         CONTINUE
            JETLIS(IJMIN,I)=.TRUE.
         ENDIF
140   CONTINUE
*** Recompute PJ
      DO 200 I = 1,NJET
        DO 210 MU = 1,4
          PJ(MU,I) = 0.0
210     CONTINUE
        DO 220 N = 1,NTRAK
          IF( JETLIS(I,N) ) THEN
             IF (MODE.NE.2) THEN
                DO 230 MU = 1,4
                   PJ(MU,I) = PJ(MU,I) + PP(MU,N)
230             CONTINUE
             ELSE
                PJ(1,I)=PJ(1,I)
     +               + PP(4,N)/(PP(4,N)+PJ(4,I))*(PP(1,N)-PJ(1,I))
                PJ(2,I)=PJ(2,I)
     +               + PP(4,N)/(PP(4,N)+PJ(4,I))*PXMDPI(PP(2,N)-PJ(2,I))
                PJ(4,I)=PJ(4,I)+PP(4,N)
             ENDIF
          ENDIF
220     CONTINUE
200   CONTINUE
      RETURN
      END
C-----------------------------------------------------------------------
C     This is a set of routines written by Mike Seymour to provide the
C     services presumably normally provided by standard OPAL routines
C     PXZERV zeroes a vector
C     PXZERI zeroes a vector of integers
C     PXNORV normalizes a vector
C     PXADDV adds two vectors
C     PXSORV sorts a vector (copied from HERWIG)
C     PXANG3 finds the angle (and its cosine) between two vectors
C     PXMDPI moves its argument onto the range [-pi,pi)
C-----------------------------------------------------------------------
      SUBROUTINE PXZERV(N,A)
      INTEGER I,N
      DOUBLE PRECISION A(N)
      DO 10 I=1,N
        A(I)=0
 10   CONTINUE
      END
C-----------------------------------------------------------------------
      SUBROUTINE PXZERI(N,A)
      INTEGER I,N,A(N)
      DO 10 I=1,N
        A(I)=0
 10   CONTINUE
      END
C-----------------------------------------------------------------------
      SUBROUTINE PXNORV(N,A,B,ITERR)
      INTEGER I,N,ITERR
      DOUBLE PRECISION A(N),B(N),C
      C=0
      DO 10 I=1,N
        C=C+A(I)**2
 10   CONTINUE
      IF (C.LE.0) RETURN
      C=1/SQRT(C)
      DO 20 I=1,N
        B(I)=A(I)*C
 20   CONTINUE
      END
C-----------------------------------------------------------------------
      SUBROUTINE PXADDV(N,A,B,C,ITERR)
      INTEGER I,N,ITERR
      DOUBLE PRECISION A(N),B(N),C(N)
      DO 10 I=1,N
        C(I)=A(I)+B(I)
 10   CONTINUE
      END
C-----------------------------------------------------------------------
      SUBROUTINE PXANG3(A,B,COST,THET,ITERR)
      INTEGER ITERR
      DOUBLE PRECISION A(3),B(3),C,COST,THET
      C=(A(1)**2+A(2)**2+A(3)**2)*(B(1)**2+B(2)**2+B(3)**2)
      IF (C.LE.0) RETURN
      C=1/SQRT(C)
      COST=(A(1)*B(1)+A(2)*B(2)+A(3)*B(3))*C
      THET=ACOS(COST)
      END
C-----------------------------------------------------------------------
      SUBROUTINE PXSORV(N,A,K,OPT)
C     Sort A(N) into ascending order
C     OPT = 'I' : return index array K only
C     OTHERWISE : return sorted A and index array K
C-----------------------------------------------------------------------
      INTEGER NMAX
      PARAMETER (NMAX=5000)
      INTEGER N,I,J,K(N),IL(NMAX),IR(NMAX)
      CHARACTER OPT
      DOUBLE PRECISION A(N),B(NMAX)
      IF (N.GT.NMAX) STOP 'Sorry, not enough room in Mike''s PXSORV'
      IF (N.EQ.0) RETURN
      IL(1)=0
      IR(1)=0
      DO 10 I=2,N
      IL(I)=0
      IR(I)=0
      J=1
   2  IF(A(I).GT.A(J)) GO TO 5
   3  IF(IL(J).EQ.0) GO TO 4
      J=IL(J)
      GO TO 2
   4  IR(I)=-J
      IL(J)=I
      GO TO 10
   5  IF(IR(J).LE.0) GO TO 6
      J=IR(J)
      GO TO 2
   6  IR(I)=IR(J)
      IR(J)=I
  10  CONTINUE
      I=1
      J=1
      GO TO 8
  20  J=IL(J)
   8  IF(IL(J).GT.0) GO TO 20
   9  K(I)=J
      B(I)=A(J)
      I=I+1
      IF(IR(J)) 12,30,13
  13  J=IR(J)
      GO TO 8
  12  J=-IR(J)
      GO TO 9
  30  IF(OPT.EQ.'I') RETURN
      DO 31 I=1,N
  31  A(I)=B(I)
 999  END
C-----------------------------------------------------------------------
      FUNCTION PXMDPI(PHI)
      IMPLICIT NONE
C---RETURNS PHI, MOVED ONTO THE RANGE [-PI,PI)
      DOUBLE PRECISION PXMDPI,PHI,PI,TWOPI,THRPI,EPS
      PARAMETER (PI=3.14159265358979324D0,TWOPI=6.28318530717958648D0,
     &     THRPI=9.42477796076937972D0)
      PARAMETER (EPS=1D-15)
      PXMDPI=PHI
      IF (PXMDPI.LE.PI) THEN
        IF (PXMDPI.GT.-PI) THEN
          GOTO 100
        ELSEIF (PXMDPI.GT.-THRPI) THEN
          PXMDPI=PXMDPI+TWOPI
        ELSE
          PXMDPI=-MOD(PI-PXMDPI,TWOPI)+PI
        ENDIF
      ELSEIF (PXMDPI.LE.THRPI) THEN
        PXMDPI=PXMDPI-TWOPI
      ELSE
        PXMDPI=MOD(PI+PXMDPI,TWOPI)-PI
      ENDIF
 100  IF (ABS(PXMDPI).LT.EPS) PXMDPI=0
      END
C-----------------------------------------------------------------------


C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     KTCLUS: written by Mike Seymour, July 1992.
C     Last modified November 2000.
C     Please send comments or suggestions to Mike.Seymour@rl.ac.uk
C
C     This is a general-purpose kt clustering package.
C     It can handle ee, ep and pp collisions.
C     It is loosely based on the program of Siggi Bethke.
C
C     The time taken (on a 10MIP machine) is (0.2microsec)*N**3
C     where N is the number of particles.
C     Over 90 percent of this time is used in subroutine KTPMIN, which
C     simply finds the minimum member of a one-dimensional array.
C     It is well worth thinking about optimization: on the SPARCstation
C     a factor of two increase was obtained simply by increasing the
C     optimization level from its default value.
C
C     The approach is to separate the different stages of analysis.
C     KTCLUS does all the clustering and records a merging history.
C     It returns a simple list of the y values at which each merging
C     occured. Then the following routines can be called to give extra
C     information on the most recently analysed event.
C     KTCLUR is identical but includes an R parameter, see below.
C     KTYCUT gives the number of jets at each given YCUT value.
C     KTYSUB gives the number of sub-jets at each given YCUT value.
C     KTBEAM gives same info as KTCLUS but only for merges with the beam
C     KTJOIN gives same info as KTCLUS but for merges of sub-jets.
C     KTRECO reconstructs the jet momenta at a given value of YCUT.
C     It also gives information on which jets at scale YCUT belong to
C     which macro-jets at scale YMAC, for studying sub-jet properties.
C     KTINCL reconstructs the jet momenta according to the inclusive jet
C     definition of Ellis and Soper.
C     KTISUB, KTIJOI and KTIREC are like KTYSUB, KTJOIN and KTRECO,
C     except that they only apply to one inclusive jet at a time,
C     with the pt of that jet automatically used for ECUT.
C     KTWICH gives a list of which particles ended up in which jets.
C     KTWCHS gives the same thing, but only for subjets.
C     Note that the numbering of jets used by these two routines is
C     guaranteed to be the same as that used by KTRECO.
C
C     The collision type and analysis type are indicated by the first
C     argument of KTCLUS. IMODE=<TYPE><ANGLE><MONO><RECOM> where
C     TYPE:  1=>ee, 2=>ep with p in -z direction, 3=>pe, 4=>pp
C     ANGLE: 1=>angular kt def., 2=>DeltaR, 3=>f(DeltaEta,DeltaPhi)
C            where f()=2(cosh(eta)-cos(phi)) is the QCD emission metric
C     MONO:  1=>derive relative pseudoparticle angles from jets
C            2=>monotonic definitions of relative angles
C     RECOM: 1=>E recombination scheme, 2=>pt scheme, 3=>pt**2 scheme
C
C     There are also abbreviated forms for the most common combinations:
C     IMODE=1 => E scheme in e+e-                              (=1111)
C           2 => E scheme in ep                                (=2111)
C           3 => E scheme in pe                                (=3111)
C           4 => E scheme in pp                                (=4111)
C           5 => covariant E scheme in pp                      (=4211)
C           6 => covariant pt-scheme in pp                     (=4212)
C           7 => covariant monotonic pt**2-scheme in pp        (=4223)
C
C     KTRECO no longer needs to reconstruct the momenta according to the
C     same recombination scheme in which they were clustered. Its first
C     argument gives the scheme, taking the same values as RECOM above.
C
C     Note that unlike previous versions, all variables which hold y
C     values have been named in a consistent way:
C     Y()  is the output scale at which jets were merged,
C     YCUT is the input scale at which jets should be counted, and
C          jet-momenta reconstructed etc,
C     YMAC is the input macro-jet scale, used in determining whether
C          or not each jet is a sub-jet.
C     The original scheme defined in our papers is equivalent to always
C     setting YMAC=1.
C     Whenever a YCUT or YMAC variable is used, it is rounded down
C     infinitesimally, so that for example, setting YCUT=Y(2) refers
C     to the scale where the event is 2-jet, even if rounding errors
C     have shifted its value slightly.
C
C     An R parameter can be used in hadron-hadron collisions by
C     calling KTCLUR instead of KTCLUS.  This is as suggested by
C     Ellis and Soper, but implemented slightly differently,
C     as in M.H. Seymour, LU TP 94/2 (submitted to Nucl. Phys. B.).
C     R**2 multiplies the single Kt everywhere it is used.
C     Calling KTCLUR with R=1 is identical to calling KTCLUS.
C     R plays a similar role to the jet radius in a cone-type algorithm,
C     but is scaled up by about 40% (ie R=0.7 in a cone algorithm is
C     similar to this algorithm with R=1).
C     Note that R.EQ.1 must be used for the e+e- and ep versions,
C     and is strongly recommended for the hadron-hadron version.
C     However, R values smaller than 1 have been found to be useful for
C     certain applications, particularly the mass reconstruction of
C     highly-boosted colour-singlets such as high-pt hadronic Ws,
C     as in M.H. Seymour, LU TP 93/8 (to appear in Z. Phys. C.).
C     Situations in which R<1 is useful are likely to also be those in
C     which the inclusive reconstruction method is more useful.
C
C     Also included is a set of routines for doing Lorentz boosts:
C     KTLBST finds the boost matrix to/from the cm frame of a 4-vector
C     KTRROT finds the rotation matrix from one vector to another
C     KTMMUL multiplies together two matrices
C     KTVMUL multiplies a vector by a matrix
C     KTINVT inverts a transformation matrix (nb NOT a general 4 by 4)
C     KTFRAM boosts a list of vectors between two arbitrary frames
C     KTBREI boosts a list of vectors between the lab and Breit frames
C     KTHADR boosts a list of vectors between the lab and hadronic cmf
C       The last two need the momenta in the +z direction of the lepton
C       and hadron beams, and the 4-momentum of the outgoing lepton.
C
C     The main reference is:
C       S. Catani, Yu.L. Dokshitzer, M.H. Seymour and B.R. Webber,
C         Nucl.Phys.B406(1993)187.
C     The ep version was proposed in:
C       S. Catani, Yu.L. Dokshitzer and B.R. Webber,
C         Phys.Lett.285B(1992)291.
C     The inclusive reconstruction method was proposed in:
C       S.D. Ellis and D.E. Soper,
C         Phys.Rev.D48(1993)3160.
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE KTCLUS(IMODE,PP,NN,ECUT,Y,*)
      IMPLICIT NONE
C---DO CLUSTER ANALYSIS OF PARTICLES IN PP
C
C   IMODE   = INPUT  : DESCRIBED ABOVE
C   PP(I,J) = INPUT  : 4-MOMENTUM OF Jth PARTICLE: I=1,4 => PX,PY,PZ,E
C   NN      = INPUT  : NUMBER OF PARTICLES
C   ECUT    = INPUT  : DENOMINATOR OF KT MEASURE. IF ZERO, ETOT IS USED
C   Y(J)    = OUTPUT : VALUE OF Y FOR WHICH EVENT CHANGES FROM BEING
C                        J JET TO J-1 JET
C   LAST ARGUMENT IS LABEL TO JUMP TO IF FOR ANY REASON THE EVENT
C   COULD NOT BE PROCESSED (MOST LIKELY DUE TO TOO MANY PARTICLES)
C
C   NOTE THAT THE MOMENTA ARE DECLARED DOUBLE PRECISION,
C   AND ALL OTHER FLOATING POINT VARIABLES ARE DECLARED DOUBLE PRECISION
C
      INTEGER IMODE,NN
      DOUBLE PRECISION PP(4,*)
      DOUBLE PRECISION ECUT,Y(*),ONE
      ONE=1
      CALL KTCLUR(IMODE,PP,NN,ONE,ECUT,Y,*999)
      RETURN
 999  RETURN 1
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTCLUR(IMODE,PP,NN,R,ECUT,Y,*)
      IMPLICIT NONE
C---DO CLUSTER ANALYSIS OF PARTICLES IN PP
C
C   IMODE   = INPUT  : DESCRIBED ABOVE
C   PP(I,J) = INPUT  : 4-MOMENTUM OF Jth PARTICLE: I=1,4 => PX,PY,PZ,E
C   NN      = INPUT  : NUMBER OF PARTICLES
C   R       = INPUT  : ELLIS AND SOPER'S R PARAMETER, SEE ABOVE.
C   ECUT    = INPUT  : DENOMINATOR OF KT MEASURE. IF ZERO, ETOT IS USED
C   Y(J)    = OUTPUT : VALUE OF Y FOR WHICH EVENT CHANGES FROM BEING
C                        J JET TO J-1 JET
C   LAST ARGUMENT IS LABEL TO JUMP TO IF FOR ANY REASON THE EVENT
C   COULD NOT BE PROCESSED (MOST LIKELY DUE TO TOO MANY PARTICLES)
C
C   NOTE THAT THE MOMENTA ARE DECLARED DOUBLE PRECISION,
C   AND ALL OTHER FLOATING POINT VARIABLES ARE DECLARED DOUBLE PRECISION
C
      INTEGER NMAX,IM,IMODE,TYPE,ANGL,MONO,RECO,N,I,J,NN,
     &     IMIN,JMIN,KMIN,NUM,HIST,INJET,IABBR,NABBR
      PARAMETER (NMAX=2048,NABBR=7)
      DOUBLE PRECISION PP(4,*)
      DOUBLE PRECISION R,ECUT,Y(*),P,KT,ETOT,RSQ,KTP,KTS,KTPAIR,KTSING,
     &     KTMIN,ETSQ,KTLAST,KTMAX,KTTMP
      LOGICAL FIRST
      CHARACTER TITLE(4,4)*10
C---KT RECORDS THE KT**2 OF EACH MERGING.
C---KTLAST RECORDS FOR EACH MERGING, THE HIGHEST ECUT**2 FOR WHICH THE
C   RESULT IS NOT MERGED WITH THE BEAM (COULD BE LARGER THAN THE
C   KT**2 AT WHICH IT WAS MERGED IF THE KT VALUES ARE NOT MONOTONIC).
C   THIS MAY SOUND POINTLESS, BUT ITS USEFUL FOR DETERMINING WHETHER
C   SUB-JETS SURVIVED TO SCALE Y=YMAC OR NOT.
C---HIST RECORDS MERGING HISTORY:
C   N=>DELETED TRACK N, M*NMAX+N=>MERGED TRACKS M AND N (M<N).
      COMMON /KTCOMM/ETOT,RSQ,P(9,NMAX),KTP(NMAX,NMAX),KTS(NMAX),
     &  KT(NMAX),KTLAST(NMAX),HIST(NMAX),NUM
      DIMENSION INJET(NMAX),IABBR(NABBR)
      DATA FIRST,TITLE,IABBR/.TRUE.,
     &     'e+e-      ','ep        ','pe        ','pp        ',
     &     'angle     ','DeltaR    ','f(DeltaR) ','**********',
     &     'no        ','yes       ','**********','**********',
     &     'E         ','Pt        ','Pt**2     ','**********',
     &     1111,2111,3111,4111,4211,4212,4223/
C---CHECK INPUT
      IM=IMODE
      IF (IM.GE.1.AND.IM.LE.NABBR) IM=IABBR(IM)
      TYPE=MOD(IM/1000,10)
      ANGL=MOD(IM/100 ,10)
      MONO=MOD(IM/10  ,10)
      RECO=MOD(IM     ,10)
      IF (NN.GT.NMAX.OR.NN.LT.1.OR.(NN.LT.2.AND.TYPE.EQ.1))
     &     CALL KTWARN('KTCLUS',100,*999)
      IF (TYPE.LT.1.OR.TYPE.GT.4.OR.ANGL.LT.1.OR.ANGL.GT.3.OR.
     &    MONO.LT.1.OR.MONO.GT.2.OR.RECO.LT.1.OR.RECO.GT.3)
     &     CALL KTWARN('KTCLUS',101,*999)
      IF (FIRST) THEN
         WRITE (6,'(/,1X,54(''*'')/A)')
     &   ' KTCLUS: written by Mike Seymour, July 1992.'
         WRITE (6,'(A)')
     &   ' Last modified November 2000.'
         WRITE (6,'(A)')
     &   ' Please send comments or suggestions to Mike.Seymour@rl.ac.uk'
         WRITE (6,'(/A,I2,2A)')
     &   '       Collision type =',TYPE,' = ',TITLE(TYPE,1)
         WRITE (6,'(A,I2,2A)')
     &   '     Angular variable =',ANGL,' = ',TITLE(ANGL,2)
         WRITE (6,'(A,I2,2A)')
     &   ' Monotonic definition =',MONO,' = ',TITLE(MONO,3)
         WRITE (6,'(A,I2,2A)')
     &   ' Recombination scheme =',RECO,' = ',TITLE(RECO,4)
         IF (R.NE.1) THEN
         WRITE (6,'(A,F5.2)')
     &   '     Radius parameter =',R
         IF (TYPE.NE.4) WRITE (6,'(A)')
     &   ' R.NE.1 is strongly discouraged for this collision type!'
         ENDIF
         WRITE (6,'(1X,54(''*'')/)')
         FIRST=.FALSE.
      ENDIF
C---COPY PP TO P
      N=NN
      NUM=NN
      CALL KTCOPY(PP,N,P,(RECO.NE.1))
      ETOT=0
      DO 100 I=1,N
         ETOT=ETOT+P(4,I)
 100  CONTINUE
      IF (ETOT.EQ.0) CALL KTWARN('KTCLUS',102,*999)
      IF (ECUT.EQ.0) THEN
         ETSQ=1/ETOT**2
      ELSE
         ETSQ=1/ECUT**2
      ENDIF
      RSQ=R**2
C---CALCULATE ALL PAIR KT's
      DO 210 I=1,N-1
         DO 200 J=I+1,N
            KTP(J,I)=-1
            KTP(I,J)=KTPAIR(ANGL,P(1,I),P(1,J),KTP(J,I))
 200     CONTINUE
 210  CONTINUE
C---CALCULATE ALL SINGLE KT's
      DO 230 I=1,N
         KTS(I)=KTSING(ANGL,TYPE,P(1,I))
 230  CONTINUE
      KTMAX=0
C---MAIN LOOP
 300  CONTINUE
C---FIND MINIMUM MEMBER OF KTP
      CALL KTPMIN(KTP,NMAX,N,IMIN,JMIN)
C---FIND MINIMUM MEMBER OF KTS
      CALL KTSMIN(KTS,NMAX,N,KMIN)
C---STORE Y VALUE OF TRANSITION FROM N TO N-1 JETS
      KTMIN=KTP(IMIN,JMIN)
      KTTMP=RSQ*KTS(KMIN)
      IF ((TYPE.GE.2.AND.TYPE.LE.4).AND.
     &     (KTTMP.LE.KTMIN.OR.N.EQ.1))
     &     KTMIN=KTTMP
      KT(N)=KTMIN
      Y(N)=KT(N)*ETSQ
C---IF MONO.GT.1, SEQUENCE IS SUPPOSED TO BE MONOTONIC, IF NOT, WARN
      IF (KTMIN.LT.KTMAX.AND.MONO.GT.1) CALL KTWARN('KTCLUS',1,*999)
      IF (KTMIN.GE.KTMAX) KTMAX=KTMIN
C---IF LOWEST KT IS TO A BEAM, THROW IT AWAY AND MOVE LAST ENTRY UP
      IF (KTMIN.EQ.KTTMP) THEN
         CALL KTMOVE(P,KTP,KTS,NMAX,N,KMIN,1)
C---UPDATE HISTORY AND CROSS-REFERENCES
         HIST(N)=KMIN
         INJET(N)=KMIN
         DO 400 I=N,NN
            IF (INJET(I).EQ.KMIN) THEN
               KTLAST(I)=KTMAX
               INJET(I)=0
            ELSEIF (INJET(I).EQ.N) THEN
               INJET(I)=KMIN
            ENDIF
 400     CONTINUE
C---OTHERWISE MERGE JETS IMIN AND JMIN AND MOVE LAST ENTRY UP
      ELSE
         CALL KTMERG(P,KTP,KTS,NMAX,IMIN,JMIN,N,TYPE,ANGL,MONO,RECO)
         CALL KTMOVE(P,KTP,KTS,NMAX,N,JMIN,1)
C---UPDATE HISTORY AND CROSS-REFERENCES
         HIST(N)=IMIN*NMAX+JMIN
         INJET(N)=IMIN
         DO 600 I=N,NN
            IF (INJET(I).EQ.JMIN) THEN
               INJET(I)=IMIN
            ELSEIF (INJET(I).EQ.N) THEN
               INJET(I)=JMIN
            ENDIF
 600     CONTINUE
      ENDIF
C---THATS ALL THERE IS TO IT
      N=N-1
      IF (N.GT.1 .OR. N.GT.0.AND.(TYPE.GE.2.AND.TYPE.LE.4)) GOTO 300
      IF (N.EQ.1) THEN
         KT(N)=1D20
         Y(N)=KT(N)*ETSQ
      ENDIF
      RETURN
 999  RETURN 1
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTYCUT(ECUT,NY,YCUT,NJET,*)
      IMPLICIT NONE
C---COUNT THE NUMBER OF JETS AT EACH VALUE OF YCUT, FOR EVENT WHICH HAS
C   ALREADY BEEN ANALYSED BY KTCLUS.
C
C   ECUT    = INPUT : DENOMINATOR OF KT MEASURE. IF ZERO, ETOT IS USED
C   NY      = INPUT : NUMBER OF YCUT VALUES
C   YCUT(J) = INPUT : Y VALUES AT WHICH NUMBERS OF JETS ARE COUNTED
C   NJET(J) =OUTPUT : NUMBER OF JETS AT YCUT(J)
C   LAST ARGUMENT IS LABEL TO JUMP TO IF FOR ANY REASON THE EVENT
C   COULD NOT BE PROCESSED
C
C   NOTE THAT ALL FLOATING POINT VARIABLES ARE DECLARED DOUBLE PRECISION
C
      INTEGER NY,NJET(NY),NMAX,HIST,I,J,NUM
      PARAMETER (NMAX=2048)
      DOUBLE PRECISION YCUT(NY),ETOT,RSQ,P,KT,KTP,KTS,ETSQ,ECUT,KTLAST,
     &     ROUND
      PARAMETER (ROUND=0.99999D0)
      COMMON /KTCOMM/ETOT,RSQ,P(9,NMAX),KTP(NMAX,NMAX),KTS(NMAX),
     &  KT(NMAX),KTLAST(NMAX),HIST(NMAX),NUM
      IF (ETOT.EQ.0) CALL KTWARN('KTYCUT',100,*999)
      IF (ECUT.EQ.0) THEN
         ETSQ=1/ETOT**2
      ELSE
         ETSQ=1/ECUT**2
      ENDIF
      DO 100 I=1,NY
         NJET(I)=0
 100  CONTINUE
      DO 210 I=NUM,1,-1
         DO 200 J=1,NY
            IF (NJET(J).EQ.0.AND.KT(I)*ETSQ.GE.ROUND*YCUT(J)) NJET(J)=I
 200     CONTINUE
 210  CONTINUE
      RETURN
 999  RETURN 1
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTYSUB(ECUT,NY,YCUT,YMAC,NSUB,*)
      IMPLICIT NONE
C---COUNT THE NUMBER OF SUB-JETS AT EACH VALUE OF YCUT, FOR EVENT WHICH
C   HAS ALREADY BEEN ANALYSED BY KTCLUS.
C   REMEMBER THAT A SUB-JET IS DEFINED AS A JET AT Y=YCUT WHICH HAS NOT
C   YET BEEN MERGED WITH THE BEAM AT Y=YMAC.
C
C   ECUT    = INPUT : DENOMINATOR OF KT MEASURE. IF ZERO, ETOT IS USED
C   NY      = INPUT : NUMBER OF YCUT VALUES
C   YCUT(J) = INPUT : Y VALUES AT WHICH NUMBERS OF SUB-JETS ARE COUNTED
C   YMAC    = INPUT : Y VALUE USED TO DEFINE MACRO-JETS, TO DETERMINE
C                       WHICH JETS ARE SUB-JETS
C   NSUB(J) =OUTPUT : NUMBER OF SUB-JETS AT YCUT(J)
C   LAST ARGUMENT IS LABEL TO JUMP TO IF FOR ANY REASON THE EVENT
C   COULD NOT BE PROCESSED
C
C   NOTE THAT ALL FLOATING POINT VARIABLES ARE DECLARED DOUBLE PRECISION
C
      INTEGER NY,NSUB(NY),NMAX,HIST,I,J,NUM
      PARAMETER (NMAX=2048)
      DOUBLE PRECISION YCUT(NY),YMAC,ETOT,RSQ,P,KT,KTP,KTS,ETSQ,ECUT,
     &     KTLAST,ROUND
      PARAMETER (ROUND=0.99999D0)
      COMMON /KTCOMM/ETOT,RSQ,P(9,NMAX),KTP(NMAX,NMAX),KTS(NMAX),
     &  KT(NMAX),KTLAST(NMAX),HIST(NMAX),NUM
      IF (ETOT.EQ.0) CALL KTWARN('KTYSUB',100,*999)
      IF (ECUT.EQ.0) THEN
         ETSQ=1/ETOT**2
      ELSE
         ETSQ=1/ECUT**2
      ENDIF
      DO 100 I=1,NY
         NSUB(I)=0
 100  CONTINUE
      DO 210 I=NUM,1,-1
         DO 200 J=1,NY
            IF (NSUB(J).EQ.0.AND.KT(I)*ETSQ.GE.ROUND*YCUT(J)) NSUB(J)=I
            IF (NSUB(J).NE.0.AND.KTLAST(I)*ETSQ.LT.ROUND*YMAC)
     &          NSUB(J)=NSUB(J)-1
 200     CONTINUE
 210  CONTINUE
      RETURN
 999  RETURN 1
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTBEAM(ECUT,Y,*)
      IMPLICIT NONE
C---GIVE SAME INFORMATION AS LAST CALL TO KTCLUS EXCEPT THAT ONLY
C   TRANSITIONS WHERE A JET WAS MERGED WITH THE BEAM JET ARE RECORDED
C
C   ECUT    = INPUT : DENOMINATOR OF KT MEASURE. IF ZERO, ETOT IS USED
C   Y(J)    =OUTPUT : Y VALUE WHERE Jth HARDEST JET WAS MERGED WITH BEAM
C   LAST ARGUMENT IS LABEL TO JUMP TO IF FOR ANY REASON THE EVENT
C   COULD NOT BE PROCESSED
C
C   NOTE THAT ALL FLOATING POINT VARIABLES ARE DECLARED DOUBLE PRECISION
C
      INTEGER NMAX,HIST,NUM,I,J
      PARAMETER (NMAX=2048)
      DOUBLE PRECISION ETOT,RSQ,P,KT,KTP,KTS,ECUT,ETSQ,Y(*),KTLAST
      COMMON /KTCOMM/ETOT,RSQ,P(9,NMAX),KTP(NMAX,NMAX),KTS(NMAX),
     &  KT(NMAX),KTLAST(NMAX),HIST(NMAX),NUM
      IF (ETOT.EQ.0) CALL KTWARN('KTBEAM',100,*999)
      IF (ECUT.EQ.0) THEN
         ETSQ=1/ETOT**2
      ELSE
         ETSQ=1/ECUT**2
      ENDIF
      J=1
      DO 100 I=1,NUM
         IF (HIST(I).LE.NMAX) THEN
            Y(J)=ETSQ*KT(I)
            J=J+1
         ENDIF
 100  CONTINUE
      DO 200 I=J,NUM
         Y(I)=0
 200  CONTINUE
      RETURN
 999  RETURN 1
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTJOIN(ECUT,YMAC,Y,*)
      IMPLICIT NONE
C---GIVE SAME INFORMATION AS LAST CALL TO KTCLUS EXCEPT THAT ONLY
C   TRANSITIONS WHERE TWO SUB-JETS WERE JOINED ARE RECORDED
C   REMEMBER THAT A SUB-JET IS DEFINED AS A JET AT Y=YCUT WHICH HAS NOT
C   YET BEEN MERGED WITH THE BEAM AT Y=YMAC.
C
C   ECUT    = INPUT : DENOMINATOR OF KT MEASURE. IF ZERO, ETOT IS USED
C   YMAC    = INPUT : VALUE OF Y USED TO DEFINE MACRO-JETS
C   Y(J)    =OUTPUT : Y VALUE WHERE EVENT CHANGED FROM HAVING
C                         N+J SUB-JETS TO HAVING N+J-1, WHERE N IS
C                         THE NUMBER OF MACRO-JETS AT SCALE YMAC
C   LAST ARGUMENT IS LABEL TO JUMP TO IF FOR ANY REASON THE EVENT
C   COULD NOT BE PROCESSED
C
C   NOTE THAT ALL FLOATING POINT VARIABLES ARE DECLARED DOUBLE PRECISION
C
      INTEGER NMAX,HIST,NUM,I,J
      PARAMETER (NMAX=2048)
      DOUBLE PRECISION ETOT,RSQ,P,KT,KTP,KTS,ECUT,ETSQ,Y(*),YMAC,KTLAST,
     &     ROUND
      PARAMETER (ROUND=0.99999D0)
      COMMON /KTCOMM/ETOT,RSQ,P(9,NMAX),KTP(NMAX,NMAX),KTS(NMAX),
     &  KT(NMAX),KTLAST(NMAX),HIST(NMAX),NUM
      IF (ETOT.EQ.0) CALL KTWARN('KTJOIN',100,*999)
      IF (ECUT.EQ.0) THEN
         ETSQ=1/ETOT**2
      ELSE
         ETSQ=1/ECUT**2
      ENDIF
      J=1
      DO 100 I=1,NUM
         IF (HIST(I).GT.NMAX.AND.ETSQ*KTLAST(I).GE.ROUND*YMAC) THEN
            Y(J)=ETSQ*KT(I)
            J=J+1
         ENDIF
 100  CONTINUE
      DO 200 I=J,NUM
         Y(I)=0
 200  CONTINUE
      RETURN
 999  RETURN 1
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTRECO(RECO,PP,NN,ECUT,YCUT,YMAC,PJET,JET,NJET,NSUB,*)
      IMPLICIT NONE
C---RECONSTRUCT KINEMATICS OF JET SYSTEM, WHICH HAS ALREADY BEEN
C   ANALYSED BY KTCLUS. NOTE THAT NO CONSISTENCY CHECK IS MADE: USER
C   IS TRUSTED TO USE THE SAME PP VALUES AS FOR KTCLUS
C
C   RECO     = INPUT : RECOMBINATION SCHEME (NEED NOT BE SAME AS KTCLUS)
C   PP(I,J)  = INPUT : 4-MOMENTUM OF Jth PARTICLE: I=1,4 => PX,PY,PZ,E
C   NN       = INPUT : NUMBER OF PARTICLES
C   ECUT     = INPUT : DENOMINATOR OF KT MEASURE. IF ZERO, ETOT IS USED
C   YCUT     = INPUT : Y VALUE AT WHICH TO RECONSTRUCT JET MOMENTA
C   YMAC     = INPUT : Y VALUE USED TO DEFINE MACRO-JETS, TO DETERMINE
C                        WHICH JETS ARE SUB-JETS
C   PJET(I,J)=OUTPUT : 4-MOMENTUM OF Jth JET AT SCALE YCUT
C   JET(J)   =OUTPUT : THE MACRO-JET WHICH CONTAINS THE Jth JET,
C                        SET TO ZERO IF JET IS NOT A SUB-JET
C   NJET     =OUTPUT : THE NUMBER OF JETS
C   NSUB     =OUTPUT : THE NUMBER OF SUB-JETS (EQUAL TO THE NUMBER OF
C                        NON-ZERO ENTRIES IN JET())
C   LAST ARGUMENT IS LABEL TO JUMP TO IF FOR ANY REASON THE EVENT
C   COULD NOT BE PROCESSED
C
C   NOTE THAT THE MOMENTA ARE DECLARED DOUBLE PRECISION,
C   AND ALL OTHER FLOATING POINT VARIABLES ARE DECLARED DOUBLE PRECISION
C
      INTEGER NMAX,RECO,NUM,N,NN,NJET,NSUB,JET(*),HIST,IMIN,JMIN,I,J
      PARAMETER (NMAX=2048)
      DOUBLE PRECISION PP(4,*),PJET(4,*)
      DOUBLE PRECISION ECUT,P,KT,KTP,KTS,ETOT,RSQ,ETSQ,YCUT,YMAC,KTLAST,
     &     ROUND
      PARAMETER (ROUND=0.99999D0)
      COMMON /KTCOMM/ETOT,RSQ,P(9,NMAX),KTP(NMAX,NMAX),KTS(NMAX),
     &  KT(NMAX),KTLAST(NMAX),HIST(NMAX),NUM
C---CHECK INPUT
      IF (RECO.LT.1.OR.RECO.GT.3) CALL KTWARN('KTRECO',100,*999)
C---COPY PP TO P
      N=NN
      IF (NUM.NE.NN) CALL KTWARN('KTRECO',101,*999)
      CALL KTCOPY(PP,N,P,(RECO.NE.1))
      IF (ECUT.EQ.0) THEN
         ETSQ=1/ETOT**2
      ELSE
         ETSQ=1/ECUT**2
      ENDIF
C---KEEP MERGING UNTIL YCUT
 100  IF (ETSQ*KT(N).LT.ROUND*YCUT) THEN
         IF (HIST(N).LE.NMAX) THEN
            CALL KTMOVE(P,KTP,KTS,NMAX,N,HIST(N),0)
         ELSE
            IMIN=HIST(N)/NMAX
            JMIN=HIST(N)-IMIN*NMAX
            CALL KTMERG(P,KTP,KTS,NMAX,IMIN,JMIN,N,0,0,0,RECO)
            CALL KTMOVE(P,KTP,KTS,NMAX,N,JMIN,0)
         ENDIF
         N=N-1
         IF (N.GT.0) GOTO 100
      ENDIF
C---IF YCUT IS TOO LARGE THERE ARE NO JETS
      NJET=N
      NSUB=N
      IF (N.EQ.0) RETURN
C---SET UP OUTPUT MOMENTA
      DO 210 I=1,NJET
         IF (RECO.EQ.1) THEN
            DO 200 J=1,4
               PJET(J,I)=P(J,I)
 200        CONTINUE
         ELSE
            PJET(1,I)=P(6,I)*COS(P(8,I))
            PJET(2,I)=P(6,I)*SIN(P(8,I))
            PJET(3,I)=P(6,I)*SINH(P(7,I))
            PJET(4,I)=P(6,I)*COSH(P(7,I))
         ENDIF
         JET(I)=I
 210  CONTINUE
C---KEEP MERGING UNTIL YMAC TO FIND THE FATE OF EACH JET
 300  IF (ETSQ*KT(N).LT.ROUND*YMAC) THEN
         IF (HIST(N).LE.NMAX) THEN
            IMIN=0
            JMIN=HIST(N)
            NSUB=NSUB-1
         ELSE
            IMIN=HIST(N)/NMAX
            JMIN=HIST(N)-IMIN*NMAX
            IF (ETSQ*KTLAST(N).LT.ROUND*YMAC) NSUB=NSUB-1
         ENDIF
         DO 310 I=1,NJET
            IF (JET(I).EQ.JMIN) JET(I)=IMIN
            IF (JET(I).EQ.N) JET(I)=JMIN
 310     CONTINUE
         N=N-1
         IF (N.GT.0) GOTO 300
      ENDIF
      RETURN
 999  RETURN 1
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTINCL(RECO,PP,NN,PJET,JET,NJET,*)
      IMPLICIT NONE
C---RECONSTRUCT KINEMATICS OF JET SYSTEM, WHICH HAS ALREADY BEEN
C   ANALYSED BY KTCLUS ACCORDING TO THE INCLUSIVE JET DEFINITION. NOTE
C   THAT NO CONSISTENCY CHECK IS MADE: USER IS TRUSTED TO USE THE SAME
C   PP VALUES AS FOR KTCLUS
C
C   RECO     = INPUT : RECOMBINATION SCHEME (NEED NOT BE SAME AS KTCLUS)
C   PP(I,J)  = INPUT : 4-MOMENTUM OF Jth PARTICLE: I=1,4 => PX,PY,PZ,E
C   NN       = INPUT : NUMBER OF PARTICLES
C   PJET(I,J)=OUTPUT : 4-MOMENTUM OF Jth JET AT SCALE YCUT
C   JET(J)   =OUTPUT : THE JET WHICH CONTAINS THE Jth PARTICLE
C   NJET     =OUTPUT : THE NUMBER OF JETS
C   LAST ARGUMENT IS LABEL TO JUMP TO IF FOR ANY REASON THE EVENT
C   COULD NOT BE PROCESSED
C
C   NOTE THAT THE MOMENTA ARE DECLARED DOUBLE PRECISION,
C   AND ALL OTHER FLOATING POINT VARIABLES ARE DECLARED DOUBLE PRECISION
C
      INTEGER NMAX,RECO,NUM,N,NN,NJET,JET(*),HIST,IMIN,JMIN,I,J
      PARAMETER (NMAX=2048)
      DOUBLE PRECISION PP(4,*),PJET(4,*)
      DOUBLE PRECISION P,KT,KTP,KTS,ETOT,RSQ,KTLAST
      COMMON /KTCOMM/ETOT,RSQ,P(9,NMAX),KTP(NMAX,NMAX),KTS(NMAX),
     &  KT(NMAX),KTLAST(NMAX),HIST(NMAX),NUM
C---CHECK INPUT
      IF (RECO.LT.1.OR.RECO.GT.3) CALL KTWARN('KTINCL',100,*999)
C---COPY PP TO P
      N=NN
      IF (NUM.NE.NN) CALL KTWARN('KTINCL',101,*999)
      CALL KTCOPY(PP,N,P,(RECO.NE.1))
C---INITIALLY EVERY PARTICLE IS IN ITS OWN JET
      DO 100 I=1,NN
         JET(I)=I
 100  CONTINUE
C---KEEP MERGING TO THE BITTER END
      NJET=0
 200  IF (N.GT.0) THEN
         IF (HIST(N).LE.NMAX) THEN
            IMIN=0
            JMIN=HIST(N)
            NJET=NJET+1
            IF (RECO.EQ.1) THEN
               DO 300 J=1,4
                  PJET(J,NJET)=P(J,JMIN)
 300           CONTINUE
            ELSE
               PJET(1,NJET)=P(6,JMIN)*COS(P(8,JMIN))
               PJET(2,NJET)=P(6,JMIN)*SIN(P(8,JMIN))
               PJET(3,NJET)=P(6,JMIN)*SINH(P(7,JMIN))
               PJET(4,NJET)=P(6,JMIN)*COSH(P(7,JMIN))
            ENDIF
            CALL KTMOVE(P,KTP,KTS,NMAX,N,JMIN,0)
         ELSE
            IMIN=HIST(N)/NMAX
            JMIN=HIST(N)-IMIN*NMAX
            CALL KTMERG(P,KTP,KTS,NMAX,IMIN,JMIN,N,0,0,0,RECO)
            CALL KTMOVE(P,KTP,KTS,NMAX,N,JMIN,0)
         ENDIF
         DO 400 I=1,NN
            IF (JET(I).EQ.JMIN) JET(I)=IMIN
            IF (JET(I).EQ.N) JET(I)=JMIN
            IF (JET(I).EQ.0) JET(I)=-NJET
 400     CONTINUE
         N=N-1
         GOTO 200
      ENDIF
C---FINALLY EVERY PARTICLE MUST BE IN AN INCLUSIVE JET
      DO 500 I=1,NN
C---IF THERE ARE ANY UNASSIGNED PARTICLES SOMETHING MUST HAVE GONE WRONG
         IF (JET(I).GE.0) CALL KTWARN('KTINCL',102,*999)
         JET(I)=-JET(I)
 500  CONTINUE
      RETURN
 999  RETURN 1
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTISUB(N,NY,YCUT,NSUB,*)
      IMPLICIT NONE
C---COUNT THE NUMBER OF SUB-JETS IN THE Nth INCLUSIVE JET OF AN EVENT
C   THAT HAS ALREADY BEEN ANALYSED BY KTCLUS.
C
C   N       = INPUT : WHICH INCLUSIVE JET TO USE
C   NY      = INPUT : NUMBER OF YCUT VALUES
C   YCUT(J) = INPUT : Y VALUES AT WHICH NUMBERS OF SUB-JETS ARE COUNTED
C   NSUB(J) =OUTPUT : NUMBER OF SUB-JETS AT YCUT(J)
C   LAST ARGUMENT IS LABEL TO JUMP TO IF FOR ANY REASON THE EVENT
C   COULD NOT BE PROCESSED
C
C   NOTE THAT ALL FLOATING POINT VARIABLES ARE DECLARED DOUBLE PRECISION
C
      INTEGER N,NY,NSUB(NY),NMAX,HIST,I,J,NUM,NM
      PARAMETER (NMAX=2048)
      DOUBLE PRECISION YCUT(NY),ETOT,RSQ,P,KT,KTP,KTS,KTLAST,ROUND,EPS
      PARAMETER (ROUND=0.99999D0)
      COMMON /KTCOMM/ETOT,RSQ,P(9,NMAX),KTP(NMAX,NMAX),KTS(NMAX),
     &  KT(NMAX),KTLAST(NMAX),HIST(NMAX),NUM
      DATA EPS/1D-6/
      DO 100 I=1,NY
         NSUB(I)=0
 100  CONTINUE
C---FIND WHICH MERGING CORRESPONDS TO THE NTH INCLUSIVE JET
      NM=0
      J=0
      DO 110 I=NUM,1,-1
        IF (HIST(I).LE.NMAX) J=J+1
        IF (J.EQ.N) THEN
          NM=I
          GOTO 120
        ENDIF
 110  CONTINUE
 120  CONTINUE
C---GIVE UP IF THERE ARE LESS THAN N INCLUSIVE JETS
      IF (NM.EQ.0) CALL KTWARN('KTISUB',100,*999)
      DO 210 I=NUM,1,-1
         DO 200 J=1,NY
            IF (NSUB(J).EQ.0.AND.RSQ*KT(I).GE.ROUND*YCUT(J)*KT(NM))
     &          NSUB(J)=I
            IF (NSUB(J).NE.0.AND.ABS(KTLAST(I)-KTLAST(NM)).GT.EPS)
     &          NSUB(J)=NSUB(J)-1
 200     CONTINUE
 210  CONTINUE
      RETURN
 999  RETURN 1
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTIJOI(N,Y,*)
      IMPLICIT NONE
C---GIVE SAME INFORMATION AS LAST CALL TO KTCLUS EXCEPT THAT ONLY
C   MERGES OF TWO SUB-JETS INSIDE THE Nth INCLUSIVE JET ARE RECORDED
C
C   N       = INPUT : WHICH INCLUSIVE JET TO USE
C   Y(J)    =OUTPUT : Y VALUE WHERE JET CHANGED FROM HAVING
C                         J+1 SUB-JETS TO HAVING J
C   LAST ARGUMENT IS LABEL TO JUMP TO IF FOR ANY REASON THE EVENT
C   COULD NOT BE PROCESSED
C
C   NOTE THAT ALL FLOATING POINT VARIABLES ARE DECLARED DOUBLE PRECISION
C
      INTEGER NMAX,HIST,NUM,I,J,N,NM
      PARAMETER (NMAX=2048)
      DOUBLE PRECISION ETOT,RSQ,P,KT,KTP,KTS,Y(*),KTLAST,EPS
      COMMON /KTCOMM/ETOT,RSQ,P(9,NMAX),KTP(NMAX,NMAX),KTS(NMAX),
     &  KT(NMAX),KTLAST(NMAX),HIST(NMAX),NUM
      DATA EPS/1D-6/
C---FIND WHICH MERGING CORRESPONDS TO THE NTH INCLUSIVE JET
      NM=0
      J=0
      DO 100 I=NUM,1,-1
        IF (HIST(I).LE.NMAX) J=J+1
        IF (J.EQ.N) THEN
          NM=I
          GOTO 105
        ENDIF
 100  CONTINUE
 105  CONTINUE
C---GIVE UP IF THERE ARE LESS THAN N INCLUSIVE JETS
      IF (NM.EQ.0) CALL KTWARN('KTIJOI',100,*999)
      J=1
      DO 110 I=1,NUM
         IF (HIST(I).GT.NMAX.AND.ABS(KTLAST(I)-KTLAST(NM)).LT.EPS) THEN
            Y(J)=RSQ*KT(I)/KT(NM)
            J=J+1
         ENDIF
 110  CONTINUE
      DO 200 I=J,NUM
         Y(I)=0
 200  CONTINUE
      RETURN
 999  RETURN 1
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTIREC(RECO,PP,NN,N,YCUT,PSUB,NSUB,*)
      IMPLICIT NONE
C---RECONSTRUCT KINEMATICS OF SUB-JET SYSTEM IN THE Nth INCLUSIVE JET
C   OF AN EVENT THAT HAS ALREADY BEEN ANALYSED BY KTCLUS
C
C   RECO     = INPUT : RECOMBINATION SCHEME (NEED NOT BE SAME AS KTCLUS)
C   PP(I,J)  = INPUT : 4-MOMENTUM OF Jth PARTICLE: I=1,4 => PX,PY,PZ,E
C   NN       = INPUT : NUMBER OF PARTICLES
C   N        = INPUT : WHICH INCLUSIVE JET TO USE
C   YCUT     = INPUT : Y VALUE AT WHICH TO RECONSTRUCT JET MOMENTA
C   PSUB(I,J)=OUTPUT : 4-MOMENTUM OF Jth SUB-JET AT SCALE YCUT
C   NSUB     =OUTPUT : THE NUMBER OF SUB-JETS
C   LAST ARGUMENT IS LABEL TO JUMP TO IF FOR ANY REASON THE EVENT
C   COULD NOT BE PROCESSED
C
C   NOTE THAT THE MOMENTA ARE DECLARED DOUBLE PRECISION,
C   AND ALL OTHER FLOATING POINT VARIABLES ARE DECLARED DOUBLE PRECISION
C
      INTEGER NMAX,RECO,NUM,NN,NJET,NSUB,JET,HIST,I,J,N,NM
      PARAMETER (NMAX=2048)
      DOUBLE PRECISION PP(4,*),PSUB(4,*)
      DOUBLE PRECISION ECUT,P,KT,KTP,KTS,ETOT,RSQ,YCUT,YMAC,KTLAST
      COMMON /KTCOMM/ETOT,RSQ,P(9,NMAX),KTP(NMAX,NMAX),KTS(NMAX),
     &  KT(NMAX),KTLAST(NMAX),HIST(NMAX),NUM
      DIMENSION JET(NMAX)
C---FIND WHICH MERGING CORRESPONDS TO THE NTH INCLUSIVE JET
      NM=0
      J=0
      DO 100 I=NUM,1,-1
         IF (HIST(I).LE.NMAX) J=J+1
         IF (J.EQ.N) THEN
            NM=I
            GOTO 110
         ENDIF
 100  CONTINUE
 110  CONTINUE
C---GIVE UP IF THERE ARE LESS THAN N INCLUSIVE JETS
      IF (NM.EQ.0) CALL KTWARN('KTIREC',102,*999)
C---RECONSTRUCT THE JETS AT THE APPROPRIATE SCALE
      ECUT=SQRT(KT(NM)/RSQ)
      YMAC=RSQ
      CALL KTRECO(RECO,PP,NN,ECUT,YCUT,YMAC,PSUB,JET,NJET,NSUB,*999)
C---GET RID OF THE ONES THAT DO NOT END UP IN THE JET WE WANT
      NSUB=0
      DO 210 I=1,NJET
         IF (JET(I).EQ.HIST(NM)) THEN
            NSUB=NSUB+1
            DO 200 J=1,4
               PSUB(J,NSUB)=PSUB(J,I)
 200        CONTINUE
         ENDIF
 210  CONTINUE
      RETURN
 999  RETURN 1
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTWICH(ECUT,YCUT,JET,NJET,*)
      IMPLICIT NONE
C---GIVE A LIST OF WHICH JET EACH ORIGINAL PARTICLE ENDED UP IN AT SCALE
C   YCUT, TOGETHER WITH THE NUMBER OF JETS AT THAT SCALE.
C
C   ECUT     = INPUT : DENOMINATOR OF KT MEASURE. IF ZERO, ETOT IS USED
C   YCUT     = INPUT : Y VALUE AT WHICH TO DEFINE JETS
C   JET(J)   =OUTPUT : THE JET WHICH CONTAINS THE Jth PARTICLE,
C                        SET TO ZERO IF IT WAS PUT INTO THE BEAM JETS
C   NJET     =OUTPUT : THE NUMBER OF JETS AT SCALE YCUT (SO JET()
C                        ENTRIES WILL BE IN THE RANGE 0 -> NJET)
C   LAST ARGUMENT IS LABEL TO JUMP TO IF FOR ANY REASON THE EVENT
C   COULD NOT BE PROCESSED
C
C   NOTE THAT ALL FLOATING POINT VARIABLES ARE DECLARED DOUBLE PRECISION
C
      INTEGER JET(*),NJET,NTEMP
      DOUBLE PRECISION ECUT,YCUT
      CALL KTWCHS(ECUT,YCUT,YCUT,JET,NJET,NTEMP,*999)
      RETURN
 999  RETURN 1
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTWCHS(ECUT,YCUT,YMAC,JET,NJET,NSUB,*)
      IMPLICIT NONE
C---GIVE A LIST OF WHICH SUB-JET EACH ORIGINAL PARTICLE ENDED UP IN AT
C   SCALE YCUT, WITH MACRO-JET SCALE YMAC, TOGETHER WITH THE NUMBER OF
C   JETS AT SCALE YCUT AND THE NUMBER OF THEM WHICH ARE SUB-JETS.
C
C   ECUT     = INPUT : DENOMINATOR OF KT MEASURE. IF ZERO, ETOT IS USED
C   YCUT     = INPUT : Y VALUE AT WHICH TO DEFINE JETS
C   YMAC     = INPUT : Y VALUE AT WHICH TO DEFINE MACRO-JETS
C   JET(J)   =OUTPUT : THE JET WHICH CONTAINS THE Jth PARTICLE,
C                        SET TO ZERO IF IT WAS PUT INTO THE BEAM JETS
C   NJET     =OUTPUT : THE NUMBER OF JETS AT SCALE YCUT (SO JET()
C                        ENTRIES WILL BE IN THE RANGE 0 -> NJET)
C   NSUB     =OUTPUT : THE NUMBER OF SUB-JETS AT SCALE YCUT, WITH
C                        MACRO-JETS DEFINED AT SCALE YMAC (SO ONLY NSUB
C                        OF THE JETS 1 -> NJET WILL APPEAR IN JET())
C   LAST ARGUMENT IS LABEL TO JUMP TO IF FOR ANY REASON THE EVENT
C   COULD NOT BE PROCESSED
C
C   NOTE THAT ALL FLOATING POINT VARIABLES ARE DECLARED DOUBLE PRECISION
C
      INTEGER NMAX,JET(*),NJET,NSUB,HIST,NUM,I,J,JSUB
      PARAMETER (NMAX=2048)
      DOUBLE PRECISION P1(4,NMAX),P2(4,NMAX)
      DOUBLE PRECISION ECUT,YCUT,YMAC,ZERO,ETOT,RSQ,P,KTP,KTS,KT,KTLAST
      COMMON /KTCOMM/ETOT,RSQ,P(9,NMAX),KTP(NMAX,NMAX),KTS(NMAX),
     &  KT(NMAX),KTLAST(NMAX),HIST(NMAX),NUM
      DIMENSION JSUB(NMAX)
C---THE MOMENTA HAVE TO BEEN GIVEN LEGAL VALUES,
C   EVEN THOUGH THEY WILL NEVER BE USED
      DATA ((P1(J,I),I=1,NMAX),J=1,4),ZERO
     &  /NMAX*1,NMAX*0,NMAX*0,NMAX*1,0/
C---FIRST GET A LIST OF WHICH PARTICLE IS IN WHICH JET AT YCUT
      CALL KTRECO(1,P1,NUM,ECUT,ZERO,YCUT,P2,JET,NJET,NSUB,*999)
C---THEN FIND OUT WHICH JETS ARE SUBJETS
      CALL KTRECO(1,P1,NUM,ECUT,YCUT,YMAC,P2,JSUB,NJET,NSUB,*999)
C---AND MODIFY JET() ACCORDINGLY
      DO 10 I=1,NUM
        IF (JET(I).NE.0) THEN
          IF (JSUB(JET(I)).EQ.0) JET(I)=0
        ENDIF
 10   CONTINUE
      RETURN
 999  RETURN 1
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTFRAM(IOPT,CMF,SIGN,Z,XZ,N,P,Q,*)
      IMPLICIT NONE
C---BOOST PARTICLES IN P TO/FROM FRAME GIVEN BY CMF, Z, XZ.
C---IN THIS FRAME CMZ IS STATIONARY,
C                   Z IS ALONG THE (SIGN)Z-AXIS (SIGN=+ OR -)
C                  XZ IS IN THE X-Z PLANE (WITH POSITIVE X COMPONENT)
C---IF Z HAS LENGTH ZERO, OR SIGN=0, NO ROTATION IS PERFORMED
C---IF XZ HAS ZERO COMPONENT PERPENDICULAR TO Z IN THAT FRAME,
C   NO AZIMUTHAL ROTATION IS PERFORMED
C
C   IOPT    = INPUT  : 0=TO FRAME, 1=FROM FRAME
C   CMF(I)  = INPUT  : 4-MOMENTUM WHICH IS STATIONARY IN THE FRAME
C   SIGN    = INPUT  : DIRECTION OF Z IN THE FRAME, NOTE THAT
C                        ONLY ITS SIGN IS USED, NOT ITS MAGNITUDE
C   Z(I)    = INPUT  : 4-MOMENTUM WHICH LIES ON THE (SIGN)Z-AXIS
C   XZ(I)   = INPUT  : 4-MOMENTUM WHICH LIES IN THE X-Z PLANE
C   N       = INPUT  : NUMBER OF PARTICLES IN P
C   P(I,J)  = INPUT  : 4-MOMENTUM OF JTH PARTICLE BEFORE
C   Q(I,J)  = OUTPUT : 4-MOMENTUM OF JTH PARTICLE AFTER
C   LAST ARGUMENT IS LABEL TO JUMP TO IF FOR ANY REASON THE EVENT
C   COULD NOT BE PROCESSED
C
C   NOTE THAT ALL MOMENTA ARE DOUBLE PRECISION
C
C   NOTE THAT IT IS SAFE TO CALL WITH P=Q
C   
      INTEGER IOPT,I,N
      DOUBLE PRECISION CMF(4),SIGN,Z(4),XZ(4),P(4,N),Q(4,N),
     &  R(4,4),NEW(4),OLD(4)
      IF (IOPT.LT.0.OR.IOPT.GT.1) CALL KTWARN('KTFRAM',200,*999)
C---FIND BOOST TO GET THERE FROM LAB
      CALL KTUNIT(R)
      CALL KTLBST(0,R,CMF,*999)
C---FIND ROTATION TO PUT BOOSTED Z ON THE (SIGN)Z AXIS
      IF (SIGN.NE.0) THEN
        CALL KTVMUL(R,Z,OLD)
        IF (OLD(1).NE.0.OR.OLD(2).NE.0.OR.OLD(3).NE.0) THEN
          NEW(1)=0
          NEW(2)=0
          NEW(3)=SIGN
          NEW(4)=ABS(SIGN)
          CALL KTRROT(R,OLD,NEW,*999)
C---FIND ROTATION TO PUT BOOSTED AND ROTATED XZ INTO X-Z PLANE
          CALL KTVMUL(R,XZ,OLD)
          IF (OLD(1).NE.0.OR.OLD(2).NE.0) THEN
            NEW(1)=1
            NEW(2)=0
            NEW(3)=0
            NEW(4)=1
            OLD(3)=0
C---NOTE THAT A POTENTIALLY AWKWARD SPECIAL CASE IS AVERTED, BECAUSE IF
C   OLD AND NEW ARE EXACTLY BACK-TO-BACK, THE ROTATION AXIS IS UNDEFINED
C   BUT IN THAT CASE KTRROT WILL USE THE Z AXIS, AS REQUIRED
            CALL KTRROT(R,OLD,NEW,*999)
          ENDIF
        ENDIF
      ENDIF
C---INVERT THE TRANSFORMATION IF NECESSARY
      IF (IOPT.EQ.1) CALL KTINVT(R,R)
C---APPLY THE RESULT TO ALL THE VECTORS
      DO 30 I=1,N
        CALL KTVMUL(R,P(1,I),Q(1,I))
 30   CONTINUE
      RETURN
 999  RETURN 1
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTBREI(IOPT,PLEP,PHAD,POUT,N,P,Q,*)
      IMPLICIT NONE
C---BOOST PARTICLES IN P TO/FROM BREIT FRAME
C
C   IOPT    = INPUT  : 0/2=TO BREIT FRAME, 1/3=FROM BREIT FRAME
C                      0/1=NO AZIMUTHAL ROTATION AFTERWARDS
C                      2/3=LEPTON PLANE ROTATED INTO THE X-Z PLANE
C   PLEP    = INPUT  : MOMENTUM OF INCOMING LEPTON IN +Z DIRECTION
C   PHAD    = INPUT  : MOMENTUM OF INCOMING HADRON IN +Z DIRECTION
C   POUT(I) = INPUT  : 4-MOMENTUM OF OUTGOING LEPTON
C   N       = INPUT  : NUMBER OF PARTICLES IN P
C   P(I,J)  = INPUT  : 4-MOMENTUM OF JTH PARTICLE BEFORE
C   Q(I,J)  = OUTPUT : 4-MOMENTUM OF JTH PARTICLE AFTER
C   LAST ARGUMENT IS LABEL TO JUMP TO IF FOR ANY REASON THE EVENT
C   COULD NOT BE PROCESSED (MOST LIKELY DUE TO PARTICLES HAVING SMALLER
C   ENERGY THAN MOMENTUM)
C
C   NOTE THAT ALL MOMENTA ARE DOUBLE PRECISION
C
C   NOTE THAT IT IS SAFE TO CALL WITH P=Q
C   
      INTEGER IOPT,N
      DOUBLE PRECISION PLEP,PHAD,POUT(4),P(4,N),Q(4,N),
     &  CMF(4),Z(4),XZ(4),DOT,QDQ
C---CHECK INPUT
      IF (IOPT.LT.0.OR.IOPT.GT.3) CALL KTWARN('KTBREI',200,*999)
C---FIND 4-MOMENTUM OF BREIT FRAME (TIMES AN ARBITRARY FACTOR)
      DOT=ABS(PHAD)*(ABS(PLEP)-POUT(4))-PHAD*(PLEP-POUT(3))
      QDQ=(ABS(PLEP)-POUT(4))**2-(PLEP-POUT(3))**2-POUT(2)**2-POUT(1)**2
      CMF(1)=DOT*(         -POUT(1))
      CMF(2)=DOT*(         -POUT(2))
      CMF(3)=DOT*(    PLEP -POUT(3))-QDQ*    PHAD
      CMF(4)=DOT*(ABS(PLEP)-POUT(4))-QDQ*ABS(PHAD)
C---FIND ROTATION TO PUT INCOMING HADRON BACK ON Z-AXIS
      Z(1)=0
      Z(2)=0
      Z(3)=PHAD
      Z(4)=ABS(PHAD)
      XZ(1)=0
      XZ(2)=0
      XZ(3)=0
      XZ(4)=0
C---DO THE BOOST
      IF (IOPT.LE.1) THEN
        CALL KTFRAM(IOPT,CMF,PHAD,Z,XZ,N,P,Q,*999)
      ELSE
        CALL KTFRAM(IOPT-2,CMF,PHAD,Z,POUT,N,P,Q,*999)
      ENDIF
      RETURN
 999  RETURN 1
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTHADR(IOPT,PLEP,PHAD,POUT,N,P,Q,*)
      IMPLICIT NONE
C---BOOST PARTICLES IN P TO/FROM HADRONIC CMF
C
C   ARGUMENTS ARE EXACTLY AS FOR KTBREI
C
C   NOTE THAT ALL MOMENTA ARE DOUBLE PRECISION
C
C   NOTE THAT IT IS SAFE TO CALL WITH P=Q
C   
      INTEGER IOPT,N
      DOUBLE PRECISION PLEP,PHAD,POUT(4),P(4,N),Q(4,N),
     &  CMF(4),Z(4),XZ(4)
C---CHECK INPUT
      IF (IOPT.LT.0.OR.IOPT.GT.3) CALL KTWARN('KTHADR',200,*999)
C---FIND 4-MOMENTUM OF HADRONIC CMF
      CMF(1)=         -POUT(1)
      CMF(2)=         -POUT(2)
      CMF(3)=    PLEP -POUT(3)+    PHAD
      CMF(4)=ABS(PLEP)-POUT(4)+ABS(PHAD)
C---FIND ROTATION TO PUT INCOMING HADRON BACK ON Z-AXIS
      Z(1)=0
      Z(2)=0
      Z(3)=PHAD
      Z(4)=ABS(PHAD)
      XZ(1)=0
      XZ(2)=0
      XZ(3)=0
      XZ(4)=0
C---DO THE BOOST
      IF (IOPT.LE.1) THEN
        CALL KTFRAM(IOPT,CMF,PHAD,Z,XZ,N,P,Q,*999)
      ELSE
        CALL KTFRAM(IOPT-2,CMF,PHAD,Z,POUT,N,P,Q,*999)
      ENDIF
      RETURN
 999  RETURN 1
      END
C-----------------------------------------------------------------------
      FUNCTION KTPAIR(ANGL,P,Q,ANGLE)
      IMPLICIT NONE
C---CALCULATE LOCAL KT OF PAIR, USING ANGULAR SCHEME:
C   1=>ANGULAR, 2=>DeltaR, 3=>f(DeltaEta,DeltaPhi)
C   WHERE f(eta,phi)=2(COSH(eta)-COS(phi)) IS THE QCD EMISSION METRIC
C---IF ANGLE<0, IT IS SET TO THE ANGULAR PART OF THE LOCAL KT ON RETURN
C   IF ANGLE>0, IT IS USED INSTEAD OF THE ANGULAR PART OF THE LOCAL KT
      INTEGER ANGL
      DOUBLE PRECISION P(9),Q(9),KTPAIR,R,KTMDPI,ANGLE,ETA,PHI,ESQ
C---COMPONENTS OF MOMENTA ARE PX,PY,PZ,E,1/P,PT,ETA,PHI,PT**2
      R=ANGLE
      IF (ANGL.EQ.1) THEN
         IF (R.LE.0) R=2*(1-(P(1)*Q(1)+P(2)*Q(2)+P(3)*Q(3))*(P(5)*Q(5)))
         ESQ=MIN(P(4),Q(4))**2
      ELSEIF (ANGL.EQ.2.OR.ANGL.EQ.3) THEN
         IF (R.LE.0) THEN
            ETA=P(7)-Q(7)
            PHI=KTMDPI(P(8)-Q(8))
            IF (ANGL.EQ.2) THEN
               R=ETA**2+PHI**2
            ELSE
               R=2*(COSH(ETA)-COS(PHI))
            ENDIF
         ENDIF
         ESQ=MIN(P(9),Q(9))
      ELSE
         CALL KTWARN('KTPAIR',200,*999)
         STOP
      ENDIF
      KTPAIR=ESQ*R
      IF (ANGLE.LT.0) ANGLE=R
 999  END
C-----------------------------------------------------------------------
      FUNCTION KTSING(ANGL,TYPE,P)
      IMPLICIT NONE
C---CALCULATE KT OF PARTICLE, USING ANGULAR SCHEME:
C   1=>ANGULAR, 2=>DeltaR, 3=>f(DeltaEta,DeltaPhi)
C---TYPE=1 FOR E+E-, 2 FOR EP, 3 FOR PE, 4 FOR PP
C   FOR EP, PROTON DIRECTION IS DEFINED AS -Z
C   FOR PE, PROTON DIRECTION IS DEFINED AS +Z
      INTEGER ANGL,TYPE
      DOUBLE PRECISION P(9),KTSING,COSTH,R,SMALL
      DATA SMALL/1D-4/
      IF (ANGL.EQ.1) THEN
         COSTH=P(3)*P(5)
         IF (TYPE.EQ.2) THEN
            COSTH=-COSTH
         ELSEIF (TYPE.EQ.4) THEN
            COSTH=ABS(COSTH)
         ELSEIF (TYPE.NE.1.AND.TYPE.NE.3) THEN
            CALL KTWARN('KTSING',200,*999)
            STOP
         ENDIF
         R=2*(1-COSTH)
C---IF CLOSE TO BEAM, USE APPROX 2*(1-COS(THETA))=SIN**2(THETA)
         IF (R.LT.SMALL) R=(P(1)**2+P(2)**2)*P(5)**2
         KTSING=P(4)**2*R
      ELSEIF (ANGL.EQ.2.OR.ANGL.EQ.3) THEN
         KTSING=P(9)
      ELSE
         CALL KTWARN('KTSING',201,*999)
         STOP
      ENDIF
 999  END
C-----------------------------------------------------------------------
      SUBROUTINE KTPMIN(A,NMAX,N,IMIN,JMIN)
      IMPLICIT NONE
C---FIND THE MINIMUM MEMBER OF A(NMAX,NMAX) WITH IMIN < JMIN <= N
      INTEGER NMAX,N,IMIN,JMIN,KMIN,I,J,K
C---REMEMBER THAT A(X+(Y-1)*NMAX)=A(X,Y)
C   THESE LOOPING VARIABLES ARE J=Y-2, I=X+(Y-1)*NMAX
      DOUBLE PRECISION A(*),AMIN
      K=1+NMAX
      KMIN=K
      AMIN=A(KMIN)
      DO 110 J=0,N-2
         DO 100 I=K,K+J
            IF (A(I).LT.AMIN) THEN
               KMIN=I
               AMIN=A(KMIN)
            ENDIF
 100     CONTINUE
         K=K+NMAX
 110  CONTINUE
      JMIN=KMIN/NMAX+1
      IMIN=KMIN-(JMIN-1)*NMAX
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTSMIN(A,NMAX,N,IMIN)
      IMPLICIT NONE
C---FIND THE MINIMUM MEMBER OF A
      INTEGER N,NMAX,IMIN,I
      DOUBLE PRECISION A(NMAX)
      IMIN=1
      DO 100 I=1,N
         IF (A(I).LT.A(IMIN)) IMIN=I
 100  CONTINUE
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTCOPY(A,N,B,ONSHLL)
      IMPLICIT NONE
C---COPY FROM A TO B. 5TH=1/(3-MTM), 6TH=PT, 7TH=ETA, 8TH=PHI, 9TH=PT**2
C   IF ONSHLL IS .TRUE. PARTICLE ENTRIES ARE PUT ON-SHELL BY SETTING E=P
      INTEGER I,N
      DOUBLE PRECISION A(4,N)
      LOGICAL ONSHLL
      DOUBLE PRECISION B(9,N),ETAMAX,SINMIN,EPS
      DATA ETAMAX,SINMIN,EPS/10,0,1D-6/
C---SINMIN GETS CALCULATED ON FIRST CALL
      IF (SINMIN.EQ.0) SINMIN=1/COSH(ETAMAX)
      DO 100 I=1,N
         B(1,I)=A(1,I)
         B(2,I)=A(2,I)
         B(3,I)=A(3,I)
         B(4,I)=A(4,I)
         B(5,I)=SQRT(A(1,I)**2+A(2,I)**2+A(3,I)**2)
         IF (ONSHLL) B(4,I)=B(5,I)
         IF (B(5,I).EQ.0) B(5,I)=1D-10
         B(5,I)=1/B(5,I)
         B(9,I)=A(1,I)**2+A(2,I)**2
         B(6,I)=SQRT(B(9,I))
         B(7,I)=B(6,I)*B(5,I)
         IF (B(7,I).GT.SINMIN) THEN
            B(7,I)=A(4,I)**2-A(3,I)**2
            IF (B(7,I).LE.EPS*B(4,I)**2.OR.ONSHLL) B(7,I)=B(9,I)
            B(7,I)=LOG((B(4,I)+ABS(B(3,I)))**2/B(7,I))/2
         ELSE
            B(7,I)=ETAMAX+2
         ENDIF
         B(7,I)=SIGN(B(7,I),B(3,I))
         IF (A(1,I).EQ.0 .AND. A(2,I).EQ.0) THEN
            B(8,I)=0
         ELSE
            B(8,I)=ATAN2(A(2,I),A(1,I))
         ENDIF
 100  CONTINUE
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTMERG(P,KTP,KTS,NMAX,I,J,N,TYPE,ANGL,MONO,RECO)
      IMPLICIT NONE
C---MERGE THE Jth PARTICLE IN P INTO THE Ith PARTICLE
C   J IS ASSUMED GREATER THAN I. P CONTAINS N PARTICLES BEFORE MERGING.
C---ALSO RECALCULATING THE CORRESPONDING KTP AND KTS VALUES IF MONO.GT.0
C   FROM THE RECOMBINED ANGULAR MEASURES IF MONO.GT.1
C---NOTE THAT IF MONO.LE.0, TYPE AND ANGL ARE NOT USED
      INTEGER ANGL,RECO,TYPE,I,J,K,N,NMAX,MONO
      DOUBLE PRECISION P(9,NMAX),KTP(NMAX,NMAX),KTS(NMAX),PT,PTT,
     &     KTMDPI,KTUP,PI,PJ,ANG,KTPAIR,KTSING,ETAMAX,EPS
      KTUP(I,J)=KTP(MAX(I,J),MIN(I,J))
      DATA ETAMAX,EPS/10,1D-6/
      IF (J.LE.I) CALL KTWARN('KTMERG',200,*999)
C---COMBINE ANGULAR MEASURES IF NECESSARY
      IF (MONO.GT.1) THEN
         DO 100 K=1,N
            IF (K.NE.I.AND.K.NE.J) THEN
               IF (RECO.EQ.1) THEN
                  PI=P(4,I)
                  PJ=P(4,J)
               ELSEIF (RECO.EQ.2) THEN
                  PI=P(6,I)
                  PJ=P(6,J)
               ELSEIF (RECO.EQ.3) THEN
                  PI=P(9,I)
                  PJ=P(9,J)
               ELSE
                  CALL KTWARN('KTMERG',201,*999)
                  STOP
               ENDIF
               IF (PI.EQ.0.AND.PJ.EQ.0) THEN
                  PI=1
                  PJ=1
               ENDIF
               KTP(MAX(I,K),MIN(I,K))=
     &              (PI*KTUP(I,K)+PJ*KTUP(J,K))/(PI+PJ)
            ENDIF
 100     CONTINUE
      ENDIF
      IF (RECO.EQ.1) THEN
C---VECTOR ADDITION
         P(1,I)=P(1,I)+P(1,J)
         P(2,I)=P(2,I)+P(2,J)
         P(3,I)=P(3,I)+P(3,J)
         P(4,I)=P(4,I)+P(4,J)
         P(5,I)=SQRT(P(1,I)**2+P(2,I)**2+P(3,I)**2)
         IF (P(5,I).EQ.0) THEN
            P(5,I)=1
         ELSE
            P(5,I)=1/P(5,I)
         ENDIF
      ELSEIF (RECO.EQ.2) THEN
C---PT WEIGHTED ETA-PHI ADDITION
         PT=P(6,I)+P(6,J)
         IF (PT.EQ.0) THEN
            PTT=1
         ELSE
            PTT=1/PT
         ENDIF
         P(7,I)=(P(6,I)*P(7,I)+P(6,J)*P(7,J))*PTT
         P(8,I)=KTMDPI(P(8,I)+P(6,J)*PTT*KTMDPI(P(8,J)-P(8,I)))
         P(6,I)=PT
         P(9,I)=PT**2
      ELSEIF (RECO.EQ.3) THEN
C---PT**2 WEIGHTED ETA-PHI ADDITION
         PT=P(9,I)+P(9,J)
         IF (PT.EQ.0) THEN
            PTT=1
         ELSE
            PTT=1/PT
         ENDIF
         P(7,I)=(P(9,I)*P(7,I)+P(9,J)*P(7,J))*PTT
         P(8,I)=KTMDPI(P(8,I)+P(9,J)*PTT*KTMDPI(P(8,J)-P(8,I)))
         P(6,I)=P(6,I)+P(6,J)
         P(9,I)=P(6,I)**2
      ELSE
         CALL KTWARN('KTMERG',202,*999)
         STOP
      ENDIF
C---IF MONO.GT.0 CALCULATE NEW KT MEASURES. IF MONO.GT.1 USE ANGULAR ONES.
      IF (MONO.LE.0) RETURN
C---CONVERTING BETWEEN 4-MTM AND PT,ETA,PHI IF NECESSARY
      IF (ANGL.NE.1.AND.RECO.EQ.1) THEN
         P(9,I)=P(1,I)**2+P(2,I)**2
         P(7,I)=P(4,I)**2-P(3,I)**2
         IF (P(7,I).LE.EPS*P(4,I)**2) P(7,I)=P(9,I)
         IF (P(7,I).GT.0) THEN
            P(7,I)=LOG((P(4,I)+ABS(P(3,I)))**2/P(7,I))/2
            IF (P(7,I).GT.ETAMAX) P(7,I)=ETAMAX+2
         ELSE
            P(7,I)=ETAMAX+2
         ENDIF
         P(7,I)=SIGN(P(7,I),P(3,I))
         IF (P(1,I).NE.0.AND.P(2,I).NE.0) THEN
            P(8,I)=ATAN2(P(2,I),P(1,I))
         ELSE
            P(8,I)=0
         ENDIF
      ELSEIF (ANGL.EQ.1.AND.RECO.NE.1) THEN
         P(1,I)=P(6,I)*COS(P(8,I))
         P(2,I)=P(6,I)*SIN(P(8,I))
         P(3,I)=P(6,I)*SINH(P(7,I))
         P(4,I)=P(6,I)*COSH(P(7,I))
         IF (P(4,I).NE.0) THEN
            P(5,I)=1/P(4,I)
         ELSE
            P(5,I)=1
         ENDIF
      ENDIF
      ANG=0
      DO 200 K=1,N
         IF (K.NE.I.AND.K.NE.J) THEN
            IF (MONO.GT.1) ANG=KTUP(I,K)
            KTP(MIN(I,K),MAX(I,K))=
     &           KTPAIR(ANGL,P(1,I),P(1,K),ANG)
         ENDIF
 200  CONTINUE
      KTS(I)=KTSING(ANGL,TYPE,P(1,I))
 999  END
C-----------------------------------------------------------------------
      SUBROUTINE KTMOVE(P,KTP,KTS,NMAX,N,J,IOPT)
      IMPLICIT NONE
C---MOVE THE Nth PARTICLE IN P TO THE Jth POSITION
C---ALSO MOVING KTP AND KTS IF IOPT.GT.0
      INTEGER I,J,N,NMAX,IOPT
      DOUBLE PRECISION P(9,NMAX),KTP(NMAX,NMAX),KTS(NMAX)
      DO 100 I=1,9
         P(I,J)=P(I,N)
 100  CONTINUE
      IF (IOPT.LE.0) RETURN
      DO 110 I=1,J-1
         KTP(I,J)=KTP(I,N)
         KTP(J,I)=KTP(N,I)
 110  CONTINUE
      DO 120 I=J+1,N-1
         KTP(J,I)=KTP(I,N)
         KTP(I,J)=KTP(N,I)
 120  CONTINUE
      KTS(J)=KTS(N)
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTUNIT(R)
      IMPLICIT NONE
C   SET R EQUAL TO THE 4 BY 4 IDENTITY MATRIX
      DOUBLE PRECISION R(4,4)
      INTEGER I,J
      DO 20 I=1,4
        DO 10 J=1,4
          R(I,J)=0
          IF (I.EQ.J) R(I,J)=1
 10     CONTINUE
 20   CONTINUE
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTLBST(IOPT,R,A,*)
      IMPLICIT NONE
C   PREMULTIPLY R BY THE 4 BY 4 MATRIX TO
C   LORENTZ BOOST TO/FROM THE CM FRAME OF A
C   IOPT=0 => TO
C   IOPT=1 => FROM
C
C   LAST ARGUMENT IS LABEL TO JUMP TO IF A IS NOT TIME-LIKE
C
      INTEGER IOPT,I,J
      DOUBLE PRECISION R(4,4),A(4),B(4),C(4,4),M
      DO 10 I=1,4
        B(I)=A(I)
 10   CONTINUE
      M=B(4)**2-B(1)**2-B(2)**2-B(3)**2
      IF (M.LE.0) CALL KTWARN('KTLBST',100,*999)
      M=SQRT(M)
      B(4)=B(4)+M
      M=1/(M*B(4))
      IF (IOPT.EQ.0) THEN
        B(4)=-B(4)
      ELSEIF (IOPT.NE.1) THEN
        CALL KTWARN('KTLBST',200,*999)
        STOP
      ENDIF
      DO 30 I=1,4
        DO 20 J=1,4
          C(I,J)=B(I)*B(J)*M
          IF (I.EQ.J) C(I,J)=C(I,J)+1
 20     CONTINUE
 30   CONTINUE
      C(4,4)=C(4,4)-2
      CALL KTMMUL(C,R,R)
      RETURN
 999  RETURN 1
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTRROT(R,A,B,*)
      IMPLICIT NONE
C   PREMULTIPLY R BY THE 4 BY 4 MATRIX TO
C   ROTATE FROM VECTOR A TO VECTOR B BY THE SHORTEST ROUTE
C   IF THEY ARE EXACTLY BACK-TO-BACK, THE ROTATION AXIS IS THE VECTOR
C   WHICH IS PERPENDICULAR TO THEM AND THE X AXIS, UNLESS THEY ARE
C   PERPENDICULAR TO THE Y AXIS, WHEN IT IS THE VECTOR WHICH IS
C   PERPENDICULAR TO THEM AND THE Y AXIS.
C   NOTE THAT THESE CONDITIONS GUARANTEE THAT IF BOTH ARE PERPENDICULAR
C   TO THE Z AXIS, IT WILL BE USED AS THE ROTATION AXIS.
C
C   LAST ARGUMENT IS LABEL TO JUMP TO IF EITHER HAS LENGTH ZERO
C
      DOUBLE PRECISION R(4,4),M(4,4),A(4),B(4),C(4),D(4),AL,BL,CL,DL,EPS
C---SQRT(2*EPS) IS THE ANGLE IN RADIANS OF THE SMALLEST ALLOWED ROTATION
C   NOTE THAT IF YOU CONVERT THIS PROGRAM TO SINGLE PRECISION, YOU WILL
C   NEED TO INCREASE EPS TO AROUND 0.5E-4
      PARAMETER (EPS=0.5D-6)
      AL=A(1)**2+A(2)**2+A(3)**2
      BL=B(1)**2+B(2)**2+B(3)**2
      IF (AL.LE.0.OR.BL.LE.0) CALL KTWARN('KTRROT',100,*999)
      AL=1/SQRT(AL)
      BL=1/SQRT(BL)
      CL=(A(1)*B(1)+A(2)*B(2)+A(3)*B(3))*AL*BL
C---IF THEY ARE COLLINEAR, DON'T NEED TO DO ANYTHING
      IF (CL.GE.1-EPS) THEN
        RETURN
C---IF THEY ARE BACK-TO-BACK, USE THE AXIS PERP TO THEM AND X AXIS
      ELSEIF (CL.LE.-1+EPS) THEN
        IF (ABS(B(2)).GT.EPS) THEN
          C(1)= 0
          C(2)=-B(3)
          C(3)= B(2)
C---UNLESS THEY ARE PERPENDICULAR TO THE Y AXIS,
        ELSE
          C(1)= B(3)
          C(2)= 0
          C(3)=-B(1)
        ENDIF
C---OTHERWISE FIND ROTATION AXIS
      ELSE
        C(1)=A(2)*B(3)-A(3)*B(2)
        C(2)=A(3)*B(1)-A(1)*B(3)
        C(3)=A(1)*B(2)-A(2)*B(1)
      ENDIF
      CL=C(1)**2+C(2)**2+C(3)**2
      IF (CL.LE.0) CALL KTWARN('KTRROT',101,*999)
      CL=1/SQRT(CL)
C---FIND ROTATION TO INTERMEDIATE AXES FROM A
      D(1)=A(2)*C(3)-A(3)*C(2)
      D(2)=A(3)*C(1)-A(1)*C(3)
      D(3)=A(1)*C(2)-A(2)*C(1)
      DL=AL*CL
      M(1,1)=A(1)*AL
      M(1,2)=A(2)*AL
      M(1,3)=A(3)*AL
      M(1,4)=0
      M(2,1)=C(1)*CL
      M(2,2)=C(2)*CL
      M(2,3)=C(3)*CL
      M(2,4)=0
      M(3,1)=D(1)*DL
      M(3,2)=D(2)*DL
      M(3,3)=D(3)*DL
      M(3,4)=0
      M(4,1)=0
      M(4,2)=0
      M(4,3)=0
      M(4,4)=1
      CALL KTMMUL(M,R,R)
C---AND ROTATION FROM INTERMEDIATE AXES TO B
      D(1)=B(2)*C(3)-B(3)*C(2)
      D(2)=B(3)*C(1)-B(1)*C(3)
      D(3)=B(1)*C(2)-B(2)*C(1)
      DL=BL*CL
      M(1,1)=B(1)*BL
      M(2,1)=B(2)*BL
      M(3,1)=B(3)*BL
      M(1,2)=C(1)*CL
      M(2,2)=C(2)*CL
      M(3,2)=C(3)*CL
      M(1,3)=D(1)*DL
      M(2,3)=D(2)*DL
      M(3,3)=D(3)*DL
      CALL KTMMUL(M,R,R)
      RETURN
 999  RETURN 1
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTVMUL(M,A,B)
      IMPLICIT NONE
C   4 BY 4 MATRIX TIMES 4 VECTOR: B=M*A.
C   ALL ARE DOUBLE PRECISION
C   IT IS SAFE TO CALL WITH B=A
C   FIRST SUBSCRIPT=ROWS, SECOND=COLUMNS
      DOUBLE PRECISION M(4,4),A(4),B(4),C(4)
      INTEGER I,J
      DO 20 I=1,4
        C(I)=0
        DO 10 J=1,4
          C(I)=C(I)+M(I,J)*A(J)
 10     CONTINUE
 20   CONTINUE
      DO 30 I=1,4
        B(I)=C(I)
 30   CONTINUE
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTMMUL(A,B,C)
      IMPLICIT NONE
C   4 BY 4 MATRIX MULTIPLICATION: C=A*B.
C   ALL ARE DOUBLE PRECISION
C   IT IS SAFE TO CALL WITH C=A OR B.
C   FIRST SUBSCRIPT=ROWS, SECOND=COLUMNS
      DOUBLE PRECISION A(4,4),B(4,4),C(4,4),D(4,4)
      INTEGER I,J,K
      DO 30 I=1,4
        DO 20 J=1,4
          D(I,J)=0
          DO 10 K=1,4
            D(I,J)=D(I,J)+A(I,K)*B(K,J)
 10       CONTINUE
 20     CONTINUE
 30   CONTINUE
      DO 50 I=1,4
        DO 40 J=1,4
          C(I,J)=D(I,J)
 40     CONTINUE
 50   CONTINUE
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTINVT(A,B)
      IMPLICIT NONE
C---INVERT TRANSFORMATION MATRIX A
C
C   A = INPUT  : 4 BY 4 TRANSFORMATION MATRIX
C   B = OUTPUT : INVERTED TRANSFORMATION MATRIX
C
C   IF A IS NOT A TRANSFORMATION MATRIX YOU WILL GET STRANGE RESULTS
C
C   NOTE THAT IT IS SAFE TO CALL WITH A=B
C
      DOUBLE PRECISION A(4,4),B(4,4),C(4,4)
      INTEGER I,J
C---TRANSPOSE
      DO 20 I=1,4
        DO 10 J=1,4
          C(I,J)=A(J,I)
 10     CONTINUE
 20   CONTINUE
C---NEGATE ENERGY-MOMENTUM MIXING TERMS
      DO 30 I=1,3
        C(4,I)=-C(4,I)
        C(I,4)=-C(I,4)
 30   CONTINUE
C---OUTPUT
      DO 50 I=1,4
        DO 40 J=1,4
          B(I,J)=C(I,J)
 40     CONTINUE
 50   CONTINUE
      END
C-----------------------------------------------------------------------
      FUNCTION KTMDPI(PHI)
      IMPLICIT NONE
C---RETURNS PHI, MOVED ONTO THE RANGE [-PI,PI)
      DOUBLE PRECISION KTMDPI,PHI,PI,TWOPI,THRPI,EPS
      PARAMETER (PI=3.14159265358979324D0,TWOPI=6.28318530717958648D0,
     &     THRPI=9.42477796076937972D0)
      PARAMETER (EPS=1D-15)
      KTMDPI=PHI
      IF (KTMDPI.LE.PI) THEN
        IF (KTMDPI.GT.-PI) THEN
          GOTO 100
        ELSEIF (KTMDPI.GT.-THRPI) THEN
          KTMDPI=KTMDPI+TWOPI
        ELSE
          KTMDPI=-MOD(PI-KTMDPI,TWOPI)+PI
        ENDIF
      ELSEIF (KTMDPI.LE.THRPI) THEN
        KTMDPI=KTMDPI-TWOPI
      ELSE
        KTMDPI=MOD(PI+KTMDPI,TWOPI)-PI
      ENDIF
 100  IF (ABS(KTMDPI).LT.EPS) KTMDPI=0
      END
C-----------------------------------------------------------------------
      SUBROUTINE KTWARN(SUBRTN,ICODE,*)
C     DEALS WITH ERRORS DURING EXECUTION
C     SUBRTN = NAME OF CALLING SUBROUTINE
C     ICODE  = ERROR CODE:    - 99 PRINT WARNING & CONTINUE
C                          100-199 PRINT WARNING & JUMP
C                          200-    PRINT WARNING & STOP DEAD
C-----------------------------------------------------------------------
      INTEGER ICODE
      CHARACTER*6 SUBRTN
      WRITE (6,10) SUBRTN,ICODE
   10 FORMAT(/' KTWARN CALLED FROM SUBPROGRAM ',A6,': CODE =',I4/)
      IF (ICODE.LT.100) RETURN
      IF (ICODE.LT.200) RETURN 1
      STOP
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------





      subroutine boost(beta,pin,pout)
      implicit none
      real * 8 beta(1:3),pin(0:3),pout(0:3)
      real * 8 vec(1:3),b,bsq,gamma,bdv,pin0
      integer j
c
      vec(1) = pin(1)
      vec(2) = pin(2)
      vec(3) = pin(3)
c beta value
      bsq = beta(1)**2+beta(2)**2+beta(3)**2
      b = sqrt(bsq)
      if (b**2.ge.1d0) then
         write(*,*) 'ERROR: boost vector has norm bigger than 1'
         write(*,*) 'RETURN 0'
         pout(0) = 0d0
         pout(1) = 0d0
         pout(2) = 0d0
         pout(3) = 0d0
         RETURN
      elseif (b.eq.0d0) then
         pout(0) = pin(0)
         pout(1) = pin(1)
         pout(2) = pin(2)
         pout(3) = pin(3)
         RETURN
      endif
      
      gamma = 1d0/sqrt(1d0-bsq)
c beta . vec
      bdv = beta(1)*vec(1)+beta(2)*vec(2)+beta(3)*vec(3)
c boost
      do j=1,3
         vec(j) = vec(j)+gamma*beta(j)/b*(bdv/b*(1-1/gamma)+b*pin(0))
c         vec(j) = gamma*beta(j)*pin(0) + vec(j) + 
c     #        beta(j)*bdv*(gamma-1d0)/vsq
      enddo
      pin0 = gamma*(pin(0)+bdv)
c return values
      pout(0) = pin0
      pout(1) = vec(1)
      pout(2) = vec(2)
      pout(3) = vec(3)
      end

