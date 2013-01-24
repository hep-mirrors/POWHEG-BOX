module     p1_dbarc_hepneg_d40h1l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p1_
   ! dbarc_hepneg/helicity1/d40h1l1d.f90
   ! generator: haggies (1.1)
   use p1_dbarc_hepneg_config, only: ki
   use p1_dbarc_hepneg_util, only: cond, d => metric_tensor
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
      use p1_dbarc_hepneg_globalsl1, only: epspow
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_abbrevh1
      implicit none
      complex(ki), intent(in) :: mu2
      integer, intent(in), optional :: i1
      complex(ki) :: numerator
      complex(ki) :: loc
      complex(ki) :: t1
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
      t1 = 0.0_ki
      if(deg.eq.0) then
         numerator = t1
         return
      end if
      if(deg.eq.1) then
         numerator = t1
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d40:
   subroutine     reconstruct_d40(coeffs)
      use p1_dbarc_hepneg_groups, only: tensrec_info_group2
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
end module p1_dbarc_hepneg_d40h1l1d
