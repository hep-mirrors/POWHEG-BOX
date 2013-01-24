module     p12_cbbar_hepneg_d34h1l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p12
   ! _cbbar_hepneg/helicity1/d34h1l1d.f90
   ! generator: haggies (1.1)
   use p12_cbbar_hepneg_config, only: ki
   use p12_cbbar_hepneg_util, only: cond, d => metric_tensor
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
      use p12_cbbar_hepneg_globalsl1, only: epspow
      use p12_cbbar_hepneg_kinematics
      use p12_cbbar_hepneg_abbrevh1
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
      t1 = (spvak6k1(1)*qshift(1)-spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-sp&
      &vak6k1(4)*qshift(4))
      t2 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      if(deg.eq.0) then
         numerator = (abb34n12+t1*abb34n27+abb34n18*dotproduct(qshift, qshift)+a&
         &bb34n21*(spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshif&
         &t(3)-spvak5k2(4)*qshift(4))+abb34n22*(spvak6k2(1)*qshift(1)-spvak6k2(2&
         &)*qshift(2)-spvak6k2(3)*qshift(3)-spvak6k2(4)*qshift(4))+abb34n24*dotp&
         &roduct(k6, qshift)+abb34n25*(spvak5k1(1)*qshift(1)-spvak5k1(2)*qshift(&
         &2)-spvak5k1(3)*qshift(3)-spvak5k1(4)*qshift(4))+abb34n26*(spvak2k1(1)*&
         &qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-spvak2k1(4)*qshi&
         &ft(4))+t1*t2*abb34n13)
         return
      end if
      t3 = spvak6k1(iv1)
      t4 = spvak5k4(iv1)
      if(deg.eq.1) then
         numerator = (k6(iv1)*abb34n17+qshift(iv1)*abb34n23+spvak2k1(iv1)*abb34n&
         &15+spvak5k1(iv1)*abb34n16+spvak5k2(iv1)*abb34n20+spvak6k2(iv1)*abb34n1&
         &9+t3*abb34n14+t1*t4*abb34n28+t2*t3*abb34n28)
         return
      end if
      if(deg.eq.2) then
         numerator = ((2.0_ki)*d(iv1,iv2)*abb34n18+spvak5k4(iv2)*t3*abb34n13+spv&
         &ak6k1(iv2)*t4*abb34n13)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d34:
   subroutine     reconstruct_d34(coeffs)
      use p12_cbbar_hepneg_groups, only: tensrec_info_group2
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
end module p12_cbbar_hepneg_d34h1l1d
