module     p5_usbar_hepneg_d40h1l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p5_
   ! usbar_hepneg/helicity1/d40h1l1d.f90
   ! generator: haggies (1.1)
   use p5_usbar_hepneg_config, only: ki
   use p5_usbar_hepneg_util, only: cond, d => metric_tensor
   implicit none
   private

   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   real(ki), dimension(4), private :: qshift


   public :: derivative, reconstruct_d40
contains

!---#[ function derivative:
   function derivative(mu2, i1) result(numerator)
      use p5_usbar_hepneg_globalsl1, only: epspow
      use p5_usbar_hepneg_kinematics
      use p5_usbar_hepneg_abbrevh1
      implicit none
      complex(ki), intent(in) :: mu2
      integer, intent(in), optional :: i1
      complex(ki) :: numerator
      complex(ki) :: loc
      integer :: deg
      complex(ki), dimension(4), parameter :: Q = (/&
        &(0.0_ki,0.0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki)/)
      qshift = k6
      numerator = 0.0_ki
      deg = 0
      if(present(i1)) then
         iv1 = i1
         deg = 1
      else
         iv1 = 1
      end if
      if(deg.eq.0) then
         numerator = (abb40n9+abb40n16*(spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift&
         &(2)-spvak5k2(3)*qshift(3)-spvak5k2(4)*qshift(4))+abb40n17*(spvak5k4(1)&
         &*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-spvak5k4(4)*qsh&
         &ift(4))+abb40n18*(spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2&
         &(3)*qshift(3)-spvak6k2(4)*qshift(4))+abb40n19*dotproduct(k2, qshift)+a&
         &bb40n20*dotproduct(k6, qshift)+abb40n21*(spvak5k1(1)*qshift(1)-spvak5k&
         &1(2)*qshift(2)-spvak5k1(3)*qshift(3)-spvak5k1(4)*qshift(4)))
         return
      end if
      if(deg.eq.1) then
         numerator = (k2(iv1)*abb40n12+k6(iv1)*abb40n11+spvak5k1(iv1)*abb40n10+s&
         &pvak5k2(iv1)*abb40n15+spvak5k4(iv1)*abb40n14+spvak6k2(iv1)*abb40n13)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d40:
   subroutine     reconstruct_d40(coeffs)
      use p5_usbar_hepneg_groups, only: tensrec_info_group2
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)

      type(tensrec_info_group2), intent(out) :: coeffs
      ! rank 1 case:
      !---[# reconstruct coeffs%coeffs_40:
      coeffs%coeffs_40%c0 = derivative(czip)
      coeffs%coeffs_40%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_40%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_40%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_40%c1(4,1) = -derivative(czip,4)
      !---#] reconstruct coeffs%coeffs_40:
   end subroutine reconstruct_d40
!---#] subroutine reconstruct_d40:
end module p5_usbar_hepneg_d40h1l1d
