module     p0_dbaru_hepneg_d34h0l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p0_
   ! dbaru_hepneg/helicity0/d34h0l1d.f90
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


   public :: derivative, reconstruct_d34
contains

!---#[ function derivative:
   function derivative(mu2, i1, i2) result(numerator)
      use p0_dbaru_hepneg_globalsl1, only: epspow
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_abbrevh0
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
      complex(ki) :: t7
      real(ki) :: t8
      integer :: deg
      complex(ki), dimension(4), parameter :: Q = (/&
        &(0.0_ki,0.0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki)/)
      qshift = k2
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
      t1 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t8 = dotproduct(k6, qshift)
      t2 = (spvak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-sp&
      &vak2k6(4)*qshift(4))
      t3 = (spvak1k6(1)*qshift(1)-spvak1k6(2)*qshift(2)-spvak1k6(3)*qshift(3)-sp&
      &vak1k6(4)*qshift(4))
      if(deg.eq.0) then
         numerator = (abb34n14+t1*abb34n25+t2*abb34n33+t8*abb34n32+abb34n22*dotp&
         &roduct(qshift, qshift)+abb34n26*dotproduct(k2, qshift)+abb34n28*dotpro&
         &duct(k1, qshift)+abb34n29*(spvak1k4(1)*qshift(1)-spvak1k4(2)*qshift(2)&
         &-spvak1k4(3)*qshift(3)-spvak1k4(4)*qshift(4))+t1*t2*abb34n15+t1*t3*abb&
         &34n19+t1*t8*abb34n18)
         return
      end if
      t4 = k6(iv1)
      t5 = spvak2k6(iv1)
      t6 = spvak5k4(iv1)
      t7 = spvak1k6(iv1)
      if(deg.eq.1) then
         numerator = (k1(iv1)*abb34n21+k2(iv1)*abb34n23+qshift(iv1)*abb34n27+spv&
         &ak1k4(iv1)*abb34n20+t4*abb34n17+t5*abb34n16+t6*abb34n24+t1*t4*abb34n31&
         &+t1*t5*abb34n34+t1*t7*abb34n30+t2*t6*abb34n34+t3*t6*abb34n30+t6*t8*abb&
         &34n31)
         return
      end if
      t1 = spvak5k4(iv2)
      if(deg.eq.2) then
         numerator = ((2.0_ki)*d(iv1,iv2)*abb34n22+k6(iv2)*t6*abb34n18+spvak1k6(&
         &iv2)*t6*abb34n19+spvak2k6(iv2)*t6*abb34n15+t1*t4*abb34n18+t1*t5*abb34n&
         &15+t1*t7*abb34n19)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d34:
   subroutine     reconstruct_d34(coeffs)
      use p0_dbaru_hepneg_groups, only: tensrec_info_group2
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)

      type(tensrec_info_group2), intent(out) :: coeffs
      ! rank 2 case:
      !---[# reconstruct coeffs%coeffs_34:
      coeffs%coeffs_34%c0 = derivative(czip)
      coeffs%coeffs_34%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_34%c1(1,2) = derivative(czip,1,1) / 2.0_ki
      coeffs%coeffs_34%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_34%c1(2,2) = derivative(czip,2,2) / 2.0_ki
      coeffs%coeffs_34%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_34%c1(3,2) = derivative(czip,3,3) / 2.0_ki
      coeffs%coeffs_34%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_34%c1(4,2) = derivative(czip,4,4) / 2.0_ki
      coeffs%coeffs_34%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_34%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_34%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_34%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_34%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_34%c2(6,1) = derivative(czip,3,4)
      !---#] reconstruct coeffs%coeffs_34:
   end subroutine reconstruct_d34
!---#] subroutine reconstruct_d34:
end module p0_dbaru_hepneg_d34h0l1d
