      subroutine setlocalscales(iuborn,imode,rescfac)
      implicit none
      integer iuborn,imode
      real * 8 rescfac
      write(*,*) 'setlocalscales should NOT be called by HW'
      write(*,*) 'The POWHEG BOX exits'
      call pwhg_exit(1)
      end
