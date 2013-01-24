module     p6_ubbar_hepneg_d37h0l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p6_
   ! ubbar_hepneg/helicity0/d37h0l1d.f90
   ! generator: haggies (1.1)
   use p6_ubbar_hepneg_config, only: ki
   use p6_ubbar_hepneg_util, only: cond, d => metric_tensor
   implicit none
   private

   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   real(ki), dimension(4), private :: qshift


   public :: derivative, reconstruct_d37
contains

!---#[ function derivative:
   function derivative(mu2, i1) result(numerator)
      use p6_ubbar_hepneg_globalsl1, only: epspow
      use p6_ubbar_hepneg_kinematics
      use p6_ubbar_hepneg_abbrevh0
      implicit none
      complex(ki), intent(in) :: mu2
      integer, intent(in), optional :: i1
      complex(ki) :: numerator
      complex(ki) :: loc
      integer :: deg
      complex(ki), dimension(4), parameter :: Q = (/&
        &(0.0_ki,0.0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki)/)
      qshift = -k3-k5-k4
      numerator = 0.0_ki
      deg = 0
      if(present(i1)) then
         iv1 = i1
         deg = 1
      else
         iv1 = 1
      end if
      if(deg.eq.0) then
         numerator = (abb37n10+abb37n11*(spvak2k4(1)*qshift(1)-spvak2k4(2)*qshif&
         &t(2)-spvak2k4(3)*qshift(3)-spvak2k4(4)*qshift(4))+abb37n12*(spvak2k6(1&
         &)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-spvak2k6(4)*qs&
         &hift(4))+abb37n13*(spvak2k1(1)*qshift(1)-spvak2k1(2)*qshift(2)-spvak2k&
         &1(3)*qshift(3)-spvak2k1(4)*qshift(4))+abb37n14*(spvak6k1(1)*qshift(1)-&
         &spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-spvak6k1(4)*qshift(4))+abb&
         &37n15*(spvak1k6(1)*qshift(1)-spvak1k6(2)*qshift(2)-spvak1k6(3)*qshift(&
         &3)-spvak1k6(4)*qshift(4))+abb37n16*dotproduct(k6, qshift)+abb37n17*dot&
         &product(k1, qshift))
         return
      end if
      if(deg.eq.1) then
         numerator = (k1(iv1)*abb37n17+k6(iv1)*abb37n16+spvak1k6(iv1)*abb37n15+s&
         &pvak2k1(iv1)*abb37n13+spvak2k4(iv1)*abb37n11+spvak2k6(iv1)*abb37n12+sp&
         &vak6k1(iv1)*abb37n14)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d37:
   subroutine     reconstruct_d37(coeffs)
      use p6_ubbar_hepneg_groups, only: tensrec_info_group3
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)

      type(tensrec_info_group3), intent(out) :: coeffs
      ! rank 1 case:
      !---[# reconstruct coeffs%coeffs_37:
      coeffs%coeffs_37%c0 = derivative(czip)
      coeffs%coeffs_37%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_37%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_37%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_37%c1(4,1) = -derivative(czip,4)
      !---#] reconstruct coeffs%coeffs_37:
   end subroutine reconstruct_d37
!---#] subroutine reconstruct_d37:
end module p6_ubbar_hepneg_d37h0l1d
