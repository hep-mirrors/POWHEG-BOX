      function pt2max_regular()
      implicit none
      real * 8 pt2max_regular
      include 'nlegborn.h'
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_rad.h'

c      write(*,*) ' dummy function; replace with your own'
c      call exit(1)
c for testing
	pt2max_regular=(kn_sreal/4)*(1-kn_y**2)*kn_csi**2
	write(*,*) 'pt2maxreg=',pt2max_regular
      end
