module     p0_dbaru_hepneg_d37h1l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p0_
   ! dbaru_hepneg/helicity1/d37h1l1d.f90
   ! generator: haggies (1.1)
   use p0_dbaru_hepneg_config, only: ki
   use p0_dbaru_hepneg_util, only: cond, d => metric_tensor
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
      use p0_dbaru_hepneg_globalsl1, only: epspow
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_abbrevh1
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
         numerator = (abb37n8+abb37n10*(spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift&
         &(2)-spvak5k2(3)*qshift(3)-spvak5k2(4)*qshift(4))+abb37n11*(spvak6k1(1)&
         &*qshift(1)-spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-spvak6k1(4)*qsh&
         &ift(4))+abb37n12*dotproduct(k6, qshift)+abb37n13*dotproduct(k1, qshift&
         &)+abb37n9*(spvak1k6(1)*qshift(1)-spvak1k6(2)*qshift(2)-spvak1k6(3)*qsh&
         &ift(3)-spvak1k6(4)*qshift(4)))
         return
      end if
      if(deg.eq.1) then
         numerator = (k1(iv1)*abb37n13+k6(iv1)*abb37n12+spvak1k6(iv1)*abb37n9+sp&
         &vak5k2(iv1)*abb37n10+spvak6k1(iv1)*abb37n11)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d37:
   subroutine     reconstruct_d37(coeffs)
      use p0_dbaru_hepneg_groups, only: tensrec_info_group3
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
end module p0_dbaru_hepneg_d37h1l1d
