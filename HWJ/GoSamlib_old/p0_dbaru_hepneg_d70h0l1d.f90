module     p0_dbaru_hepneg_d70h0l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p0_
   ! dbaru_hepneg/helicity0/d70h0l1d.f90
   ! generator: haggies (1.1)
   use p0_dbaru_hepneg_config, only: ki
   use p0_dbaru_hepneg_util, only: cond, d => metric_tensor
   implicit none
   private

   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   integer, private :: iv2
   integer, private :: iv3
   real(ki), dimension(4), private :: qshift


   public :: derivative, reconstruct_d70
contains

!---#[ function derivative:
   function derivative(mu2, i1, i2, i3) result(numerator)
      use p0_dbaru_hepneg_globalsl1, only: epspow
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_abbrevh0
      implicit none
      complex(ki), intent(in) :: mu2
      integer, intent(in), optional :: i1
      integer, intent(in), optional :: i2
      integer, intent(in), optional :: i3
      complex(ki) :: numerator
      complex(ki) :: loc
      complex(ki) :: t1
      complex(ki) :: t2
      integer :: deg
      complex(ki), dimension(4), parameter :: Q = (/&
        &(0.0_ki,0.0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki)/)
      qshift(:) = 0.0_ki
      numerator = 0.0_ki
      deg = 0
      if(present(i1)) then
         iv1 = i1
         deg = 1
      else
         iv1 = 1
      end if
      if(present(i2)) then
         iv2 = i2
         deg = 2
      else
         iv2 = 1
      end if
      if(present(i3)) then
         iv3 = i3
         deg = 3
      else
         iv3 = 1
      end if
      if(deg.eq.0) then
         numerator = abb70n16
         return
      end if
      t1 = spvak2k6(iv1)
      if(deg.eq.1) then
         numerator = (k2(iv1)*abb70n24+k6(iv1)*abb70n23+spvak1k6(iv1)*abb70n28+s&
         &pvak2k4(iv1)*abb70n25+spvak2l3(iv1)*abb70n26+spvak5k6(iv1)*abb70n21+sp&
         &val3k6(iv1)*abb70n22+t1*abb70n27)
         return
      end if
      t2 = spvak2k6(iv2)
      if(deg.eq.2) then
         numerator = ((2.0_ki)*d(iv1,iv2)*abb70n19+spvak1k2(iv1)*t2*abb70n18+spv&
         &ak1k2(iv2)*t1*abb70n18+spvak1k4(iv1)*t2*abb70n17+spvak1k4(iv2)*t1*abb7&
         &0n17+spvak5k2(iv1)*t2*abb70n20+spvak5k2(iv2)*t1*abb70n20)
         return
      end if
      if(deg.eq.3) then
         numerator = (0.0_ki)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d70:
   subroutine     reconstruct_d70(coeffs)
      use p0_dbaru_hepneg_groups, only: tensrec_info_group0
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)

      type(tensrec_info_group0), intent(out) :: coeffs
      ! rank 3 case:
      !---[# reconstruct coeffs%coeffs_70:
      coeffs%coeffs_70%c0 = derivative(czip)
      coeffs%coeffs_70%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_70%c1(1,2) = derivative(czip,1,1) / 2.0_ki
      coeffs%coeffs_70%c1(1,3) = derivative(czip,1,1,1) / 6.0_ki
      coeffs%coeffs_70%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_70%c1(2,2) = derivative(czip,2,2) / 2.0_ki
      coeffs%coeffs_70%c1(2,3) = -derivative(czip,2,2,2) / 6.0_ki
      coeffs%coeffs_70%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_70%c1(3,2) = derivative(czip,3,3) / 2.0_ki
      coeffs%coeffs_70%c1(3,3) = -derivative(czip,3,3,3) / 6.0_ki
      coeffs%coeffs_70%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_70%c1(4,2) = derivative(czip,4,4) / 2.0_ki
      coeffs%coeffs_70%c1(4,3) = -derivative(czip,4,4,4) / 6.0_ki
      coeffs%coeffs_70%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_70%c2(1,2) = derivative(czip,1,2,2) / 2.0_ki
      coeffs%coeffs_70%c2(1,3) = -derivative(czip,1,1,2) / 2.0_ki
      coeffs%coeffs_70%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_70%c2(2,2) = derivative(czip,1,3,3) / 2.0_ki
      coeffs%coeffs_70%c2(2,3) = -derivative(czip,1,1,3) / 2.0_ki
      coeffs%coeffs_70%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_70%c2(3,2) = derivative(czip,1,4,4) / 2.0_ki
      coeffs%coeffs_70%c2(3,3) = -derivative(czip,1,1,4) / 2.0_ki
      coeffs%coeffs_70%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_70%c2(4,2) = -derivative(czip,2,3,3) / 2.0_ki
      coeffs%coeffs_70%c2(4,3) = -derivative(czip,2,2,3) / 2.0_ki
      coeffs%coeffs_70%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_70%c2(5,2) = -derivative(czip,2,4,4) / 2.0_ki
      coeffs%coeffs_70%c2(5,3) = -derivative(czip,2,2,4) / 2.0_ki
      coeffs%coeffs_70%c2(6,1) = derivative(czip,3,4)
      coeffs%coeffs_70%c2(6,2) = -derivative(czip,3,4,4) / 2.0_ki
      coeffs%coeffs_70%c2(6,3) = -derivative(czip,3,3,4) / 2.0_ki
      coeffs%coeffs_70%c3(1,1) = derivative(czip,1,2,3)
      coeffs%coeffs_70%c3(2,1) = derivative(czip,1,2,4)
      coeffs%coeffs_70%c3(3,1) = derivative(czip,1,3,4)
      coeffs%coeffs_70%c3(4,1) = -derivative(czip,2,3,4)
      !---#] reconstruct coeffs%coeffs_70:
   end subroutine reconstruct_d70
!---#] subroutine reconstruct_d70:
end module p0_dbaru_hepneg_d70h0l1d
