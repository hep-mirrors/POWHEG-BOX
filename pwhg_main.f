      implicit none
      include 'include/LesHouches.h'
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_rad.h'
      include 'include/pwhg_st.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_flg.h'
      integer j,iun,nev
      real * 8 powheginput
      character * 20 pwgprefix
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      nev=powheginput('numevts')
      call pwhginit
      call newunit(iun)
      open(unit=iun,file=pwgprefix(1:lprefix)//'events.lhe')
      call lhefwritehdr(iun)
      call init_hist 
      do j=1,nev
         call pwhgevent
c         write(*,*)  j,' / ',nev,' pt = ',sqrt(st_muren2)
         call lhefwritev(iun)
         if(flg_debug) call lhefwritextra(iun)
         if(kn_csi.eq.0d0) then
            call analysis_driver(rad_sigtot,0)
         else
            call analysis_driver(rad_sigtot,1)
         endif
         call pwhgaccumup
      enddo
      if (powheginput('#testplots').eq.1d0) then
         open(unit=99,file=pwgprefix(1:lprefix)//'pwhgalone-output.top')
         call pwhgtopout(nev)
         close(99)
      endif
      call lhefwritetrailer(iun)
      close(iun)
      call newunit(iun)
      open(unit=iun,file=pwgprefix(1:lprefix)//'counters.dat'
     #     ,status='unknown')
      call printcnt(iun)
      close(iun)
      end
