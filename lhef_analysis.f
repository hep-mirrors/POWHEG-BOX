      program leshouchesanal
      implicit none
      include 'LesHouches.h'
      integer j,nev
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
c     let the analysis subroutine know that it is run by this program
      WHCPRG='LHE   '
      call opencount(nev)
      call upinit
      call init_hist 
      do j=1,nev
         call upevnt
         call lhuptohepevt(j)
         if(abs(idwtup).eq.3) xwgtup=xwgtup*xsecup(1)
         call analysis(xwgtup)
         call pwhgaccumup
         if (mod(j,20000).eq.0) then
            write(*,*) "# of events processed =",j
            open(unit=99,file='LHEF_analysis.top')
            call pwhgsetout
            call pwhgtopout
            close(99)
         endif
      enddo
      open(unit=99,file='LHEF_analysis.top')
      call pwhgsetout
      call pwhgtopout
      close(99)
      write(*,*) 'EVENTS FOUND : ',nev
      end
      
      subroutine UPINIT
      implicit none
      call lhefreadhdr(97)
      end

      subroutine UPEVNT
      call lhefreadev(97)
      end

      subroutine lhuptohepevt(n)
      implicit none
      include 'hepevt.h'
      include 'LesHouches.h'
      integer ihep,mu,n
      
      nhep=nup
      nevhep=n
      do ihep=1,nhep
         isthep(ihep)=istup(ihep)
         idhep(ihep)=idup(ihep)
         do mu=1,5
            phep(mu,ihep)=pup(mu,ihep)
         enddo
      enddo
      end
