module     p6_ubbar_hepneg_d37h1l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p6_
   ! ubbar_hepneg/helicity1/d37h1l1d.f90
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
      use p6_ubbar_hepneg_abbrevh1
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
         numerator = (abb37n8+abb37n10*(spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift&
         &(2)-spvak6k2(3)*qshift(3)-spvak6k2(4)*qshift(4))+abb37n11*dotproduct(k&
         &1, qshift)+abb37n12*dotproduct(k6, qshift)+abb37n13*(spvak6k1(1)*qshif&
         &t(1)-spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-spvak6k1(4)*qshift(4)&
         &)+abb37n9*(spvak6k4(1)*qshift(1)-spvak6k4(2)*qshift(2)-spvak6k4(3)*qsh&
         &ift(3)-spvak6k4(4)*qshift(4)))
         return
      end if
      if(deg.eq.1) then
         numerator = (k1(iv1)*abb37n11+k6(iv1)*abb37n12+spvak6k1(iv1)*abb37n13+s&
         &pvak6k2(iv1)*abb37n10+spvak6k4(iv1)*abb37n9)
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
end module p6_ubbar_hepneg_d37h1l1d
