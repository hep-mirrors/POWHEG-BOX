      subroutine SMC_initialize
      implicit none
      include 'LesHouches.h'
      character * 6 WHCPRG
      common/cWHCPRG/WHCPRG
      WHCPRG='POWHEG'      
c     set conventinally lprup(1) to 666
      lprup(1)=666      
      end

      subroutine SMC_finalize
      implicit none
      end

      subroutine SMC_hadronize(iun,end_of_file)
      implicit none
      integer iun
      logical end_of_file
      include 'LesHouches.h'
      end_of_file = .false.
      call lhefreadev(iun)
      if(nup.eq.0) then
c     reached the last event         
         end_of_file = .true.
         return
      endif
      call lhuptohepevt
      call pre_analysis(1d0)            
      end



      subroutine running_prog(stringa)
      implicit none
      character * 2 stringa
      stringa = 'PW'
      end
