module     p0_dbaru_hepneg_d30h1l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p0_
   ! dbaru_hepneg/helicity1/d30h1l1d.f90
   ! generator: haggies (1.1)
   use p0_dbaru_hepneg_config, only: ki
   use p0_dbaru_hepneg_util, only: cond, d => metric_tensor
   implicit none
   private

   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   integer, private :: iv2
   real(ki), dimension(4), private :: qshift


   public :: derivative, reconstruct_d30
contains

!---#[ function derivative:
   function derivative(mu2, i1, i2) result(numerator)
      use p0_dbaru_hepneg_globalsl1, only: epspow
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_abbrevh1
      implicit none
      complex(ki), intent(in) :: mu2
      integer, intent(in), optional :: i1
      integer, intent(in), optional :: i2
      complex(ki) :: numerator
      complex(ki) :: loc
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3
      complex(ki) :: t4
      complex(ki) :: t5
      complex(ki) :: t6
      integer :: deg
      complex(ki), dimension(4), parameter :: Q = (/&
        &(0.0_ki,0.0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki)/)
      qshift = -k2
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
      t1 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-sp&
      &vak1k2(4)*qshift(4))
      t2 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t3 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      if(deg.eq.0) then
         numerator = (abb30n14+t1*abb30n21+t2*abb30n25+abb30n16*dotproduct(qshif&
         &t, qshift)+abb30n18*dotproduct(k1, qshift)+abb30n19*(spvak1k6(1)*qshif&
         &t(1)-spvak1k6(2)*qshift(2)-spvak1k6(3)*qshift(3)-spvak1k6(4)*qshift(4)&
         &)+abb30n20*(spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qs&
         &hift(3)-spvak5k2(4)*qshift(4))+abb30n22*(spvak6k1(1)*qshift(1)-spvak6k&
         &1(2)*qshift(2)-spvak6k1(3)*qshift(3)-spvak6k1(4)*qshift(4))+abb30n23*d&
         &otproduct(k6, qshift)+t1*t3*abb30n17+t2*t3*abb30n15)
         return
      end if
      t4 = spvak1k2(iv1)
      t5 = spvak6k2(iv1)
      t6 = spvak5k4(iv1)
      if(deg.eq.1) then
         numerator = (k1(iv1)*abb30n18+k6(iv1)*abb30n23+qshift(iv1)*abb30n24+spv&
         &ak1k6(iv1)*abb30n19+spvak5k2(iv1)*abb30n20+spvak6k1(iv1)*abb30n22+t4*a&
         &bb30n21+t5*abb30n25+t1*t6*abb30n17+t2*t6*abb30n15+t3*t4*abb30n17+t3*t5&
         &*abb30n15)
         return
      end if
      t1 = spvak5k4(iv2)
      if(deg.eq.2) then
         numerator = ((2.0_ki)*d(iv1,iv2)*abb30n16+spvak1k2(iv2)*t6*abb30n17+spv&
         &ak6k2(iv2)*t6*abb30n15+t1*t4*abb30n17+t1*t5*abb30n15)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d30:
   subroutine     reconstruct_d30(coeffs)
      use p0_dbaru_hepneg_groups, only: tensrec_info_group3
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)

      type(tensrec_info_group3), intent(out) :: coeffs
      ! rank 2 case:
      !---[# reconstruct coeffs%coeffs_30:
      coeffs%coeffs_30%c0 = derivative(czip)
      coeffs%coeffs_30%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_30%c1(1,2) = derivative(czip,1,1) / 2.0_ki
      coeffs%coeffs_30%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_30%c1(2,2) = derivative(czip,2,2) / 2.0_ki
      coeffs%coeffs_30%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_30%c1(3,2) = derivative(czip,3,3) / 2.0_ki
      coeffs%coeffs_30%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_30%c1(4,2) = derivative(czip,4,4) / 2.0_ki
      coeffs%coeffs_30%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_30%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_30%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_30%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_30%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_30%c2(6,1) = derivative(czip,3,4)
      !---#] reconstruct coeffs%coeffs_30:
   end subroutine reconstruct_d30
!---#] subroutine reconstruct_d30:
end module p0_dbaru_hepneg_d30h1l1d