c     returns 2 Re(M_B * M_V)/(as/(2pi)), 
c     where M_B is the Born amplitude and 
c     M_V is the finite part of the virtual amplitude
c     The as/(2pi) factor is attached at a later point
      subroutine setvirtual(p,vflav,virtual)
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_st.h'
      include '../include/pwhg_math.h'

      real * 8 p(0:3,nlegborn)
      integer vflav(nlegborn)
      real * 8 virtual

      real * 8 born,dummy(0:3,0:3) 

      call compborn(p,vflav,born,dummy)

      virtual=cf *(pi**2 -8) * born

      end


c     returns 2 Re(M_B * M_V)/(as/(2pi)), 
c     where M_B is the Born amplitude and 
c     M_V is the finite part of the virtual amplitude
c     The as/(2pi) factor is attached at a later point

c     Like setvirtual, but it calculates only the independent
c     virtual contributions and fills directly the array 'virt_arr'.
c     
c     At the moment (revision 12), no calls to setvirtual_fast are present
c     in the main code. Therefore, this routine is left dummy.

      subroutine setvirtual_fast(virt_arr)
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_flst.h'
      real * 8 virt_arr(flst_nborn)

      write(*,*) 'Error: setvirtual_fast is not implemented yet'
      stop

      end


