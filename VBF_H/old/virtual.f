c     returns 2 Re(M_B * M_V)/(as/(2pi)), 
c     where M_B is the Born amplitude and 
c     M_V is the finite part of the virtual amplitude
c     The as/(2pi) factor is attached at a later point
      subroutine virtual(res)
      implicit none
      include '../include/pwhg_flst.h'
      include '../include/pwhg_math.h'
      include '../include/pwhg_br.h'
      real * 8 res(flst_nborn)
      integer j
      do j=1,flst_nborn
         res(j)= 2d0*cf*br_born(j)
      enddo
      end
