      program merge_Z_Zj_events
      implicit none
      character * 50 nameZj,nameZ,nameZjZ,nameZjZtmp,namehdr,
     #     nameZj_merged,nameZ_merged,nameZjZ_top
      integer iunZj,iunZ,iunZjZ,iunZjZtmp,iunZjZhdr,
     #     iunZ_merged,iunZj_merged,iun,iuntop
      integer eventsZj,eventsZ,j,ios,lun,i,k,max_num_events
      character * 100 string,line
      character * 4 stringa
      character * 10 str_final
      include '../include/LesHouches.h'
      real * 8 XsecZj,XsecZ,ptcut,pt,Xsec_comb,Xsectot,ratio_Xsecs
      integer nev_Zj1,nev_Zj2,nev_Zj3,nev_Zj
      integer nev_Z1,nev_Z2,nev_Z3,nev_Z
      integer iunZj1,iunZj2,iunZj3,iunZ1,iunZ2,iunZ3
      real * 8 ptl,pth
      common /cptlimits/ptl,pth
      real * 8 H,random
      external H,random
      real * 8 rn
      integer event_read_Zj,event_read_Z,nev_ZjZ
c      logical write_mode
      integer id(2)
      real * 8 eb(2)
      real * 8 ptV,ptj1
      common/cptV/ptV,ptj1
      INTEGER JSEED(2)
      logical use_ptV,write_separated,write_evnts
      integer ptlint,pthint
      character * 2 cptlint,cpthint
      real * 8 mass_lepton
      common /cmass_lepton/mass_lepton      
      character * 2 prog
      logical make_plots
      include '../include/hepevt.h'
      logical end_of_file

      ptl = 10d0
      pth = 30d0      

c     if true, the program uses the pt of the vector boson as merging
c     criterium. Otherwise use pt of the leading jet
      use_ptV = .false.

c     if true, the program makes the plots while building the merged file. 
      make_plots=.true.

c      mass_lepton=0.51099891d-3
      mass_lepton=0d0
      

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
cccccccc               MORE SPECIFIC OPTIONS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c     if true, frite event file
      write_evnts = .false.

c     Write, in addition to the merged event file, two more files with 
c     the merged events, separated according to their orgin, i.e. if
c     they come from the Z or from the Z+j sample 
      write_separated = .false.

      nameZj         = 'pwgevents_Zj.lhe'
      nameZ          = 'pwgevents_Z.lhe'
      ptlint = ptl
      pthint = pth
      write(cptlint,'(i2)') ptlint
      write(cpthint,'(i2)') pthint
      str_final = '_'//cptlint//'-'//cpthint//'.lhe'

      if (use_ptV) then
         stringa='_ptZ'
      else
         stringa='_ptj'
      endif
      call running_prog(prog)
      
      nameZjZ        = 'pwgevents_ZjZ_'//prog//stringa//str_final
      nameZjZtmp     = 'pwgevents_ZjZ_'//prog//'_tmp.lhe'
      namehdr        = 'pwgevents_ZjZ_'//prog//'_hdr.lhe'
      nameZjZ_top    = 'pwgevents_ZjZ_'//prog//stringa//
     #     '_'//cptlint//'-'//cpthint//'.top'
      if (write_separated) then
         nameZj_merged  = 'pwgevents_Zj_'//prog//'merged'//stringa//
     #        str_final
         nameZ_merged   = 'pwgevents_Z_'//prog//'merged'//stringa//
     #        str_final      
      endif

c      call rescale_topfile(nameZjZ_top,100d0)
c      stop


      write(*,*) '==============================================='
      write(*,*) 'READING FILE Zj events ', nameZj
      write(*,*) 'READING FILE  Z events ', nameZ
      write(*,*) 'OUTPUT FILE    ', nameZjZ
      if (use_ptV) then
         write(*,*) 'MERGING RANGE ',ptl,' < ptZ < ',pth
      else
         write(*,*) 'MERGING RANGE ',ptl,' < ptJ1 < ',pth
      endif
      write(*,*) '==============================================='
            
      call newunit(iunZ)
      open(unit=iunZ,file=nameZ,status='old',iostat=ios)
       if(ios.ne.0) then
         write(*,*)' Problems opening file '//nameZ
         stop
      endif
      call newunit(iunZj)
      open(unit=iunZj,file=nameZj,status='old',iostat=ios)
      if(ios.ne.0) then
         write(*,*)' Problems opening file '//nameZj
         stop
      endif

      call newunit(iunZjZ)
      open(unit=iunZjZ,file=nameZjZ,status='unknown')      

      call newunit(iunZjZtmp)
c      open(unit=iunZjZtmp,status='SCRATCH')      
      open(unit=iunZjZtmp,file=nameZjZtmp,status='unknown')      

      call newunit(iunZjZhdr)
c      open(unit=iunZjZhdr,status='SCRATCH')      
      open(unit=iunZjZhdr,file=namehdr,status='unknown')      

      if (write_separated) then
         call newunit(iunZ_merged)
         open(unit=iunZ_merged,file=nameZ_merged,status='unknown')      

         call newunit(iunZj_merged)
         open(unit=iunZj_merged,file=nameZj_merged,status='unknown')      
      endif

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      max_num_events = 100000000
      eventsZj = max_num_events
      eventsZ  = max_num_events
      write(*,*) ''
      write(*,*) ''
      write(*,*) ''
      write(*,*) '***********************************************'
      write(*,*) '***********************************************'
      write(*,*) ' WILL USE ONLY ',max_num_events,' Z+1j events'
      write(*,*) ' WILL USE ONLY ',max_num_events,' Z events'
      write(*,*) '***********************************************'
      write(*,*) '***********************************************'
      write(*,*) ''
      write(*,*) ''
      write(*,*) ''
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      goto 712
      call countevents(iunZj,eventsZj)
      call countevents(iunZ,eventsZ)
      write(*,*) 'number of Z+j events ',eventsZj
      write(*,*) 'number of  Z events ',eventsZ
 712  continue


      XsecZj=0d0
      XsecZ =0d0
      call lhefreadhdr(iunZj)
      XsecZj = xsecup(1)
      id(1) = idbmup(1)
      id(2) = idbmup(2)
      eb(1) = ebmup(1)
      eb(2) = ebmup(2)
      
      call lhefreadhdr(iunZ)
      XsecZ = xsecup(1)
c     sanity check
      if (idbmup(1).ne.id(1).and.idbmup(2).ne.id(2).and.
     #     ebmup(1).ne.eb(1).and.ebmup(2).ne.eb(2)) then
         write(*,*) 'The two input files have different beam energy '//
     #        'or beam particles'
         call exit(1)
      endif

      ratio_Xsecs=XsecZ/(XsecZ+XsecZj)

      write(*,*) 'Z+j Xsec ',XsecZj
      write(*,*) 'Z   Xsec ',XsecZ
      
      rewind(iunZj)
      rewind(iunZ)

      call SMC_initialize
      if (make_plots) then
         call init_hist 
      endif
      
      event_read_Zj=0
      event_read_Z=0
      nev_Z=0
      nev_Zj=0
      nev_ZjZ = 0

      write(*,*) 'BEGIN MERGING...'      

 111  continue
      rn = random()
      ptV = -1d0
      ptj1 = -1d0
      if (rn.gt.ratio_Xsecs) then
c     Z+j event
 888     call SMC_hadronize(iunZj,end_of_file) 
         if (end_of_file.or.nev_ZjZ.ge.max_num_events) then
c     no more Z+j events: end merging
            goto 123
         endif
         if (use_ptV) then
            pt = ptV
         else
            pt = ptj1
         endif
         if (pt.lt.0d0) then
            write(*,*) 'Event KILLED by HERWIG!!!'
            goto 888
         endif
         event_read_Zj = event_read_Zj + 1
c     accept event with probability (1-H)
         rn = random()
         if (rn.lt.1-H(pt)) then
c     event accepted
            nev_Zj=nev_Zj+1
            if (write_evnts) call lhefwritev(iunZjZtmp)     
            if (write_separated) call lhefwritev(iunZj_merged)     
            if (make_plots) then
               call draw_plots(1d0)
            endif
         endif
      else
c     Z event 
 778     call SMC_hadronize(iunZ,end_of_file) 
         if (end_of_file.or.nev_Zj.ge.max_num_events) then
c     no more Z events: end merging
            goto 123
         endif
         if (use_ptV) then
            pt = ptV
         else
            pt = ptj1
         endif
         if (pt.lt.0d0) then
            write(*,*) 'Event KILLED by HERWIG!!!'
            goto 778
         endif
         event_read_Z = event_read_Z + 1
c     accept event with probability H
         rn = random()
         if (rn.lt.H(pt)) then
c     event accepted
            nev_Z=nev_Z+1
            if (write_evnts) call lhefwritev(iunZjZtmp)     
            if (write_separated) call lhefwritev(iunZ_merged)    
            if (make_plots) then
               call draw_plots(1d0)
            endif 
         endif
      endif
      
      nev_ZjZ = nev_Zj + nev_Z

      if (mod(nev_ZjZ,20000).eq.0
     #        .and.event_read_Zj.gt.0.and.event_read_Z.gt.0) then
         write(*,*) '****************************************'
         write(*,*) ' Z events read ',event_read_Z,' / ',eventsZ
         write(*,*) ' Z events kept ',nev_Z,'   % = ', 
     #        (1d0*nev_Z)/event_read_Z
         write(*,*) 'Zj events read ',event_read_Zj,' / ',eventsZj
         write(*,*) 'Zj events kept ',nev_Zj,'   % = ', 
     #        (1d0*nev_Zj)/event_read_Zj       
         Xsectot = (XsecZ/event_read_Z)*(nev_Zj + nev_Z)
         write(*,*) 'Total merged X sec',Xsectot
         write(*,*) 'must be approx equal',XsecZ/event_read_Z,
     #        XsecZj/event_read_Zj           
         write(*,*) '****************************************'
c     write temporary header
         xsecup(1) = Xsectot
         call lhefwritehdr(iunZjZhdr)
         call flush(iunZjZhdr)
         rewind(iunZjZhdr)
         call close_plots(nameZjZ_top,Xsectot)
      endif

      goto 111

 123  continue
      nev_ZjZ = nev_Zj + nev_Z
      if (write_separated) then
c     these two files will be rewritten
         rewind(iunZj_merged)
         rewind(iunZ_merged)
      endif

c     compute total cross section      
      Xsectot = (XsecZ/event_read_Z)*nev_ZjZ
      write(*,*) 'Total merged X sec',Xsectot

      call close_plots(nameZjZ_top,Xsectot)
      call SMC_finalize

c     save info in the top file
      call newunit(iuntop)
      open(unit=iuntop,file=nameZjZ_top,status='unknown')
      do i=1,100000000
         read(iuntop,'(a)',err=711,end=711) string         
      enddo
 711  backspace(iuntop)      

      do i=1,2
         if (i.eq.1) then
            iun = 6
         else
            iun = iuntop
         endif
         write(iun,*) '( Z events read ',event_read_Z,' / ',eventsZ
         write(iun,*) '( Z events kept ',nev_Z,'   % = ', 
     #        (1d0*nev_Z)/event_read_Z
         write(iun,*) '( Zj events read ',event_read_Zj,' / ',eventsZj
         write(iun,*) '( Zj events kept ',nev_Zj,'   % = ', 
     #        (1d0*nev_Zj)/event_read_Zj       
         write(iun,*) '( Total merged X sec',Xsectot
         write(iun,*) '( must be approx equal',XsecZ/event_read_Z,
     #        XsecZj/event_read_Zj           
      enddo


c     write header
      xsecup(1) = Xsectot
      call lhefwritehdr(iunZjZ)
      if (write_separated) then
         call lhefwritehdr(iunZ_merged)
         call lhefwritehdr(iunZj_merged)
      endif
c     call randomrestore
      if (write_evnts) then
         rewind(iunZjZtmp)
         do i=1,nev_ZjZ
            call lhefreadev(iunZjZtmp) 
cccccccccccccccccccccccccccccccccccccccccccc
c     fix idprup equal throughout the merged file
            idprup=lprup(1)
cccccccccccccccccccccccccccccccccccccccccccc
            call lhefwritev(iunZjZ)
         enddo
         string='</LesHouchesEvents>'
         write(iunZjZ,'(a)') string
      endif
      close(iunZj)
      close(iunZ)
      close(iunZjZ)
      close(iunZjZhdr,STATUS='DELETE')
      close(iunZjZtmp,STATUS='DELETE')
      if (write_separated) then
         close(iunZj_merged)
         close(iunZ_merged)
      endif

      write(*,*) 'DONE'
c      if (make_plots) then
c         write(*,*) '******************************************'
c         write(*,*) '******************************************'
c         write(*,*) ' Rescale all the plots in the file '//nameZjZ_top
c         write(*,*) ' by the factor ',Xsectot
c         write(*,*) '******************************************'
c         write(*,*) '******************************************'
c         write(*,*) ' DELETE file: rm ',namehdr
c         write(*,*) '******************************************'
c         write(*,*) '******************************************'
c      endif

      end

      function length(string)
      implicit none
      integer length
      character * (*) string
      integer i,len
      do i=len(string),1,-1
         if (string(i:i).ne.' ') goto 10            
      enddo
 10   length=i
      end


      subroutine rescale_topfile(nfile,scale)
      implicit none
      integer LUN
      parameter (LUN=50)
      character * 50 nfile
      real * 8 scale
      integer iunr,iunw,l,m,j,k
      integer NMAX
      parameter (NMAX = 500)
      real * 8 xtab(NMAX),ytab(NMAX),etab(NMAX)
      real * 8 error
      integer nent,karr
      character * 80 string
      real * 8 rarr(10)
      real * 8 ymin, ymax
      integer length
      external length

      call newunit(iunr)
      open(unit=iunr,file=nfile,status='old')  
      call newunit(iunw)
c      open(unit=iunw,status='SCRATCH')
      open(unit=iunw,file='nuovo.top',status='unknown')
 107  FORMAT('  SET LIMITS Y ',1PE10.3,' ',1PE10.3)
     
 111  nent = NMAX 
      do j=1,NMAX
         xtab(j)=0
         ytab(j)=0
         etab(j)=0
      enddo
      do k=1,10000000
         read(iunr,'(a)',err=999,end=999) string
         if (string(3:14).eq.'SET LIMITS Y') then
            read(string(16:26),*) ymin  
            read(string(27:37),*) ymax  
c            write(*,*) ymin,ymax
            write(iunw,107) ymin*scale,ymax*scale
c            write(*,107) ymin*scale,ymax*scale
         elseif (string(20:26).eq.'ENTRIES') then
            write(iunw,'(a)') string(1:length(string))
            do m=1,NMAX
               read(iunr,'(a)',err=999) string
               call reads(string,80,rarr,3,karr)
               if(karr.ne.3) then
                  goto 888
               endif
               l = m
               xtab(l)=rarr(1)
               ytab(l)=rarr(2)
               etab(l)=rarr(3)
            enddo
         else
            write(iunw,'(a)') string(1:length(string))
         endif
      enddo
 888  continue
      nent=m-1
      do k=1,nent
         write(iunw,'(3X,F10.4,2(2X,E15.4))'),
     #        xtab(k),ytab(k)*scale,etab(k)*scale
      enddo
      write(iunw,'(a)') string(1:length(string))
      goto 111
 999  continue

      goto 444

      close(iunr)   
      rewind(iunw)
      open(unit=iunr,file=nfile,status='old') 
      do k=1,10000000
         read(iunw,'(a)',err=444,end=444) string
         write(iunw,'(a)') string(1:length(string))
      enddo
 444  continue
      close(iunr)
      close(iunw)
      end
      


      subroutine countevents(iun,numev)
      implicit none
      include '../include/hepevt.h'
      integer iun,numev
      character * 8 string
      numev=0
 1    read(unit=iun,fmt='(a)',end=2) string
      if(string.eq.'</event>') then
         numev=numev+1
         goto 1
      endif
      goto 1
 2    continue
      write(*,*) ' found ',numev,' events in file'
      rewind(iun)
      end



c...reads event information from a les houches events file on unit nlf. 
      subroutine lhefreadev(nlf)
      implicit none
      integer nlf
      character * 100 string
      include '../include/LesHouches.h'
      integer i,j
 1    read(nlf,fmt='(a)',err=998,end=998) string
      if(string.eq.'</LesHouchesEvents>') then
         goto 998
      endif
      if(string(1:6).eq.'<event') then
         read(nlf,*) nup,idprup,xwgtup,scalup,aqedup,aqcdup
         do i=1,nup
            read(nlf,*) idup(i),istup(i),mothup(1,i),
     &           mothup(2,i),icolup(1,i),icolup(2,i),(pup(j,i),j=1,5),
     &           vtimup(i),spinup(i)
         enddo
         goto 999
      else
         goto 1
      endif
c no event found:
 998  nup=0
 999  end



c...lhefeader(nlf)
c...writes event information to a les houches events file on unit nlf. 
      subroutine lhefwritev(nlf)
      implicit none
      integer nlf
      include '../include/LesHouches.h'
      integer i,j
      write(nlf,'(a)')'<event>'
      write(nlf,210) nup,idprup,xwgtup,scalup,aqedup,aqcdup
      do 200 i=1,nup
         write(nlf,220) idup(i),istup(i),mothup(1,i),
     & mothup(2,i),icolup(1,i),icolup(2,i),(pup(j,i),j=1,5),
     & vtimup(i),spinup(i)
 200  continue
      write(nlf,'(a)')'</event>'      
 210  format(1p,2(1x,i6),4(1x,e12.5))
 220  format(1p,i8,5(1x,i5),5(1x,e16.9),1x,e12.5,1x,e10.3)
      end



c...lhefheader(nlf)
c...reads initialization information from a les houches events file on unit nlf. 
      subroutine lhefreadhdr(nlf)
      implicit none
      integer nlf
      character * 100 string
      integer ipr
      include '../include/LesHouches.h'
 1    read(nlf,fmt='(a)',err=998,end=998) string
      if(string(1:5).eq.'<init') then
         read(nlf,*) idbmup(1),idbmup(2),ebmup(1),ebmup(2),
     &        pdfgup(1),pdfgup(2),pdfsup(1),pdfsup(2),idwtup,nprup
         do ipr=1,nprup
            read(nlf,*) xsecup(ipr),xerrup(ipr),xmaxup(ipr),
     &           lprup(ipr)
         enddo
         goto 999
      else
         goto 1
      endif
 998  write(*,*) 'lhefreadhdr: could not find <init> data'
      call exit(1)
 999  end


c...lhefheader(nlf)
c...writes initialization information to a les houches events file on unit nlf. 
      subroutine lhefwritehdr(nlf)
      implicit none
      integer nlf
      real * 8 version
      common/pwghvq/version
      integer ipr,iran,n1ran,n2ran
      include '../include/LesHouches.h'
      write(nlf,'(a)') '<init>'
      write(nlf,110) idbmup(1),idbmup(2),ebmup(1),ebmup(2),
     &pdfgup(1),pdfgup(2),pdfsup(1),pdfsup(2),idwtup,nprup
      do 100 ipr=1,nprup
         write(nlf,120) xsecup(ipr),xerrup(ipr),xmaxup(ipr),
     &        lprup(ipr)
 100  continue
      write(nlf,'(a)') '</init>'
 110  format(1p,2(1x,i8),2(1x,e12.5),6(1x,i6))
 120  format(1p,3(1x,e12.5),1x,i6)
      end




      subroutine  readheader(iun,xsec)
      implicit none
      integer iun
      real * 8 xsec
      character * 100 string,line
      include '../include/LesHouches.h'
      integer i,lun
 1    read(unit=iun,fmt='(a)') string
      lun = 1
      do i=len(string),1,-1
         if (string(i:i).ne.' ') then
            lun = i
            goto 3
         endif
      enddo
 3    line = string(1:lun)      
      if(string.eq.'<init>') then
         rewind(iun)
         call lhefreadhdr(iun)
         xsec = xsecup(1)
c         call lhefwritehdr(iunZjZ)
      endif
      goto 1
      end




c...reads event information from a les houches events file on unit nlf. 
      subroutine compute_ptZ(ptZ)
      implicit none
      real * 8 ptZ
      integer i
      include '../include/LesHouches.h'
      do i=1,nup
         if (idup(i).eq.23) then
c     it the Z boson
            ptZ = sqrt(pup(1,i)**2 + pup(2,i)**2)
            return
         endif
      enddo
      end


      function H(pt)
      implicit none
      real * 8 pt,H
      real * 8 pi
      parameter (pi=3.141592653589793238462643383279502884197D0)      
      real * 8 ptl,pth
      common /cptlimits/ptl,pth
      if (pt.lt.ptl) then
         H = 1d0
      elseif (pt.gt.pth) then
         H = 0d0
      else
         H = 0.5*(-sin((pt-(ptl+pth)/2)*pi/(pth-ptl))+1)
      endif
      end




      subroutine pre_analysis(dsig)
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

      integer nptZcutmax,nptZcut,numplots
      parameter(nptZcutmax = 10)
      real * 8 ptZcuts(0:nptZcutmax-1)
      common/cptvbcut/ptZcuts,nptZcut,numplots
      integer ncut

      integer mu,jpart,jjet,j1,j2,found,njets,
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
      integer nem,nep
      logical findmother
      parameter (findmother=.false.)
      logical findinvmass
      parameter (findinvmass=.true.)
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
c      data WHCPRG/'POWHEG'/
      logical is_Z
      integer nfoundjets
      integer maxjets
      parameter (maxjets=10)
      integer njj(maxjets)      
      real *8 ptj(maxjets),yj(maxjets),pj(0:3,maxjets)
c     binsize
      real * 8 bsz(100)
      common/pwhghistcommon/bsz
      real * 8 getrapidity0
      external getrapidity0
      real * 8 rsep,rsepn_p
      external rsepn_p
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      real * 8 ptV
      common/cptV/ptV,ptj1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      if (WHCPRG.ne.'POWHEG') then 
c     set values if analysis file is run by HERWIG and PYTHIA
         Zmass = 91.188d0
         Zwidth = 2.486d0
         Zmass2low = (Zmass-10*Zwidth)**2
         Zmass2high = (Zmass+10*Zwidth)**2
      endif
      if (ini) then
         write(*,*) '**************************************************'
         write(*,*) '**************************************************'
         write(*,*) '                ANALYSIS CUTS                     '
         write(*,*) '**************************************************'
         write(*,*) '**************************************************'
         write(*,*)   sqrt(Zmass2low),' < M_Z < ',sqrt(Zmass2high)
         write(*,*) '**************************************************'
         write(*,*) '**************************************************'
         ini = .false.
      endif

      if (WHCPRG.eq.'POWHEG') then 
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
         
         if (neminus.ne.1.or.neplus.ne.1) then
            write(*,*) "Too many leptons found. PROGRAM ABORT"
            call exit(1)
         else 
            iem=emvec(1)
            iep=epvec(1)
         endif
c     Analysis after MC shower
      elseif (WHCPRG.eq.'HERWIG') then
c     Loop again over final state particles to find products of Z decay, by
c     looking into the shower branchings.
         nem=0
         nep=0
         do ihep=1,nhep
c     works for POWHEG+HERWIG, POWHEG+PYHIA, HERWIG, PYTHIA and real in
c     MC@NLO
            if (isthep(ihep).eq.1.and.abs(idhep(ihep)).eq.11) then
               is_Z = idhep(jmohep(1,jmohep(1,ihep))).eq.23
               if (.not.is_Z) then
                  is_Z = idhep(jmohep(1,jmohep(1,jmohep(1,ihep)))).eq.23
               endif
               if (is_Z) then
c     find first decay product
                  if(idhep(ihep).eq.11) then
                     iem=ihep
                     nem=nem+1
c     find second decay product
                  elseif(idhep(ihep).eq.-11) then
                     iep=ihep
                     nep=nep+1
                  endif
               endif
            endif
         enddo
         if(nep.ne.1.or.nem.ne.1) then
            write(*,*) 'Problems with leptons from Z decay'
            write(*,*) 'PROGRAM ABORT'
            call exit(1)
         endif         
      elseif (WHCPRG.eq.'PYTHIA') then
c     Loop again over final state particles to find products of Z decay, by
c     looking into the shower branchings.
         nem=0
         nep=0
         do ihep=1,nhep
c     works both for POWHEG+HERWIG and POWHEG+PYHIA
            if (isthep(ihep).eq.1.and.abs(idhep(ihep)).eq.11) then
               is_Z = idhep(jmohep(1,jmohep(1,ihep))).eq.23
               if (.not.is_Z) then
                  is_Z = idhep(jmohep(1,jmohep(1,jmohep(1,ihep)))).eq.23
               endif
               if (is_Z) then
c     find first decay product
                  if(idhep(ihep).eq.11) then
                     iem=ihep
                     nem=nem+1
c     find second decay product
                  elseif(idhep(ihep).eq.-11) then
                     iep=ihep
                     nep=nep+1
                  endif
               endif
            endif
         enddo
         if(nep.ne.1.or.nem.ne.1) then
            write(*,*) 'Problems with leptons from Z decay'
            write(*,*) 'PROGRAM ABORT'
            call exit(1)
         endif
      else
         write(*,*) 'PROGRAM ABORT'
         call exit(1)            
         stop
      endif


c     Z momentum
      do mu=1,4
         pvb(mu)=phep(mu,iem)+phep(mu,iep)         
      enddo

      ptvb=sqrt(pvb(1)**2+pvb(2)**2)

      ptV=ptvb


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
c     Loop over final state particles to find jets 
      do ihep=1,nhep
         if ((isthep(ihep).eq.1).and.
c     exclude leptons, gauge and higgs bosons 
     1        (((abs(idhep(ihep)).le.10).or.(abs(idhep(ihep)).ge.40))
c     but include gluons
     2        .or.(abs(idhep(ihep)).eq.21))) then
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
     #        pjet,njets,jetvec) 
c     
c     ... now we have the jets
         if (njets.gt.0) then
c     find the first 2 hardest jets, if any
            call find_hardest_jets(njets,pjet,2,nfoundjets,njj)
         endif
         if (nfoundjets.gt.0) then
            j1 = njj(1)
            ptj1 = sqrt(pjet(1,j1)**2 + pjet(2,j1)**2)
         endif
      endif

      end



c     find the first "nhardjets" hardest jets in pjet (that contains njets)
c     and return their position.
c     foundhardjets is the number of found hard jets (.le.nhardjets)
      subroutine find_hardest_jets_notused(njets,pjet,nhardjets,
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

c      function is_i_in_array(nhardjets,i,jj)
c      implicit none
c      logical is_i_in_array
c      integer nhardjets,i,jj(nhardjets)
c      integer j
c      is_i_in_array = .false.
c      do j=1,nhardjets
c         if (i.eq.jj(j)) then
c            is_i_in_array = .true.
c            return
c         endif
c      enddo
c      end



      subroutine lhuptohepevt
      implicit none
      include '../include/hepevt.h'
      include '../include/LesHouches.h'
      integer ihep,mu      
      nhep=nup
      do ihep=1,nhep
         isthep(ihep)=istup(ihep)
         idhep(ihep)=idup(ihep)
         do mu=1,5
            phep(mu,ihep)=pup(mu,ihep)
         enddo
      enddo
      end



c      subroutine open_plots
c      implicit none
c      call init_hist
c      end
c
c      subroutine close_plots
c      implicit none
c      character * 2 prog      
c      call running_prog(prog)      
c      open(unit=99,file='merge_'//prog//'-output.top',status='unknown')
c      call pwhgsetout
c      call pwhgtopout
c      close(99)
c      end
c      
c      subroutine do_plots
c      implicit none
cc      xwgtup=xwgtup*xsecup(1)
c      call analysis(xwgtup)
c      call pwhgaccumup 
c      end


      
      subroutine draw_plots(sig)
      implicit none
      real * 8 sig     
      call analysis(sig)
      call pwhgaccumup
      end


      subroutine close_plots(nameZjZ_top,scale)
      implicit none
      character * 50 nameZjZ_top
      real * 8 scale
      open(unit=99,file=nameZjZ_top,status='unknown')
      call pwhgsetout
      call pwhgrescale(scale)
      call pwhgtopout
      close(99)
      end




c Program to read numbers from strings
      subroutine reads(string,nstr,rarr,narr,karr)
      implicit real * 8 (a-h,o-z)
      dimension rarr(narr),isign(2)
      real * 8 num(2)
      character * (*) string
      character * 1 ch
      karr=0
c get token
      istr=1
 1    continue
c skip blanks
      if(istr.le.nstr.and.string(istr:istr).eq.' ') then
         istr=istr+1
         goto 1
      endif
      if(istr.gt.nstr) goto 999
      istart=istr
c find next blank
 2    if(istr.le.nstr.and.string(istr:istr).ne.' ') then
         istr=istr+1
         goto 2
      endif
      iend=istr-1 
      iperiod=0
c value
      num(1)=0
c exponent
      num(2)=0
      k=1
      js=istart
 10   if(string(js:js).eq.'-') then
         isign(k)=-1
         js=js+1
      elseif(string(js:js).eq.'+') then
         isign(k)=1
         js=js+1
      else
         isign(k)=1
      endif
      do j=js,iend
         ch=string(j:j)
         if(ch.le.'9'.and.ch.ge.'0') then
            num(k)=num(k)*10+ichar(ch)-ichar('0')
         elseif(ch.eq.'.') then
            if(iperiod.ne.0.or.k.eq.2)goto 998
            iperiod=j-iend
         elseif(ch.eq.'e'.or.ch.eq.'E'.or.ch.eq.'d'.or.ch.eq.'D')then
            if(j.eq.1.or.k.eq.2) goto 998
            if(iperiod.ne.0) iperiod=iperiod+iend-j+1
            k=2
            js=j+1
            goto 10
         else
            goto 999
         endif
       enddo
       karr=karr+1
       rarr(karr)=isign(1)*num(1)*(10.d0)**(isign(2)*num(2)+iperiod)
       if(karr.eq.narr) goto 999
       if(iend.lt.nstr) goto 1
       goto 999
 998   continue
       stop
 999   end

c Program to read integers from strings
      subroutine ireads(string,nstr,iarr,narr,karr)
      implicit real * 8 (a-h,o-z)
      dimension iarr(narr)
      character * (*) string
      character * 1 ch
      karr=0
c get token
      istr=1
 1    continue
c skip blanks
      if(string(istr:istr).eq.' '.and.istr.le.nstr) then
         istr=istr+1
         goto 1
      endif
      if(istr.gt.nstr) goto 999
      istart=istr
c find next blank
 2    if(string(istr:istr).ne.' '.and.istr.le.nstr) then
         istr=istr+1
         goto 2
      endif
      iend=istr-1 
c value
      num=0
      js=istart
 10   if(string(js:js).eq.'-') then
         isign=-1
         js=js+1
      elseif(string(js:js).eq.'+') then
         isign=1
         js=js+1
      else
         isign=1
      endif
      do j=js,iend
         ch=string(j:j)
         if(ch.le.'9'.and.ch.ge.'0') then
            num=num*10+ichar(ch)-ichar('0')
         else
            goto 999
         endif
      enddo
      karr=karr+1
      iarr(karr)=isign*num
      if(karr.eq.narr) goto 999
      if(iend.lt.nstr) goto 1
      goto 999
 998  continue
      stop
 999  end


c     program to read numbers from input line
      subroutine iread(iun,iarr,nel,nread)
      dimension iarr(40)
      character * 80 str
      
      
      read(iun,'(a)') str
      
      call ireads(str,80,iarr,nel,nread)
      end
      




c     i1<i2
      subroutine momenta_reshuffle(ires,i1,i2,decmass)
      implicit none
      integer ires,i1,i2
      real * 8 decmass
      include '../include/LesHouches.h'
      integer j,ii,i,mu
      real * 8 ptemp(0:3),ptemp1(0:3),beta(3),betainv(3),modbeta
c construct boosts to vector boson rest frame 
      do j=1,3
         beta(j)=-pup(j,ires)/pup(4,ires)
      enddo
      modbeta=sqrt(beta(1)**2+beta(2)**2+beta(3)**2)
      do j=1,3
         beta(j)=beta(j)/modbeta
         betainv(j)=-beta(j)
      enddo
      do i=1,2
         if (i.eq.1) then 
            ii=i1
         else
            ii=i2
         endif
c     decay products
         ptemp(0)=pup(4,ii)
         do j=1,3
            ptemp(j)=pup(j,ii)
         enddo
         call mboost(1,beta,modbeta,ptemp,ptemp)
         ptemp1(0)=0.5d0*pup(5,ires)
         do j=1,3
            ptemp1(j)=ptemp(j)/ptemp(0)*sqrt(ptemp1(0)+decmass)
     #           *sqrt(ptemp1(0) - decmass)
         enddo
         call mboost(1,betainv,modbeta,ptemp1,ptemp)
         do j=1,3
            pup(j,ii)=ptemp(j)
         enddo
c         pup(4,ii)=ptemp(0)
cc     abs to avoid tiny negative values in case of neutrinos
c         pup(5,ii)=sqrt(abs(pup(4,ii)**2-pup(1,ii)**2
c     $        -pup(2,ii)**2-pup(3,ii)**2))

c     preserve final-state decay product mass
         pup(5,ii) = decmass
         pup(4,ii) = sqrt(decmass**2 + pup(1,ii)**2 + pup(2,ii)**2 
     #        +pup(3,ii)**2)
      enddo
      end





c     i1<i2
      subroutine momenta_reshuffle_old(ires,i1,i2,decmass)
      implicit none
      include '../include/LesHouches.h'
      integer ires,i1,i2,j,ii,i
      real * 8 ptemp(0:3),ptemp1(0:3),beta(3),betainv(3),modbeta,decmass
      if (i1.ge.i2) then
         write(*,*) 'wrong sequence in momenta_reshuffle'
         stop
      endif
c construct boosts to vector boson rest frame 
      do j=1,3
         beta(j)=-pup(j,ires)/pup(4,ires)
      enddo
      modbeta=sqrt(beta(1)**2+beta(2)**2+beta(3)**2)
      do j=1,3
         beta(j)=beta(j)/modbeta
         betainv(j)=-beta(j)
      enddo
      do i=1,2
         if (i.eq.1) then 
            ii=i1
         else
            ii=i2
         endif
C     first decay product 
         ptemp(0)=pup(4,ii)
         do j=1,3
            ptemp(j)=pup(j,ii)
         enddo
         call mboost(1,beta,modbeta,ptemp,ptemp)
         ptemp1(0)=0.5d0*pup(5,ires)
         do j=1,3
            ptemp1(j)=ptemp(j)/ptemp(0)*sqrt(ptemp1(0)**2 - decmass**2)
         enddo
         call mboost(1,betainv,modbeta,ptemp1,ptemp)
         do j=1,3
            pup(j,ii)=ptemp(j)
         enddo
         pup(4,ii)=ptemp(0)
c     abs to avoid tiny negative values in case of neutrinos
         pup(5,ii)=sqrt(abs(pup(4,ii)**2-pup(1,ii)**2
     $        -pup(2,ii)**2-pup(3,ii)**2))
      enddo
      end


