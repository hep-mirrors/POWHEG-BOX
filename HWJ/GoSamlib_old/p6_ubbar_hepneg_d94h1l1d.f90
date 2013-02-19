module     p6_ubbar_hepneg_d94h1l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p6_
   ! ubbar_hepneg/helicity1/d94h1l1d.f90
   ! generator: haggies (1.1)
   use p6_ubbar_hepneg_config, only: ki
   use p6_ubbar_hepneg_util, only: cond, d => metric_tensor
   implicit none
   private

   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   integer, private :: iv2
   real(ki), dimension(4), private :: qshift


   public :: derivative, reconstruct_d94
contains

!---#[ function derivative:
   function derivative(mu2, i1, i2) result(numerator)
      use p6_ubbar_hepneg_globalsl1, only: epspow
      use p6_ubbar_hepneg_kinematics
      use p6_ubbar_hepneg_abbrevh1
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
      real(ki) :: t6
      integer :: deg
      complex(ki), dimension(4), parameter :: Q = (/&
        &(0.0_ki,0.0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki)/)
      qshift = -k3-k6-k5-k4
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
      t1 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t6 = dotproduct(k1, qshift)
      t2 = (spvak6k1(1)*qshift(1)-spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-sp&
      &vak6k1(4)*qshift(4))
      if(deg.eq.0) then
         numerator = (abb94n10+t1*abb94n14+t2*abb94n16+t6*abb94n15+abb94n13*dotp&
         &roduct(qshift, qshift)+abb94n15*dotproduct(k6, qshift)+t1*t2*abb94n11+&
         &t1*t6*abb94n12)
         return
      end if
      t3 = k1(iv1)
      t4 = spvak6k1(iv1)
      t5 = spvak6k2(iv1)
      if(deg.eq.1) then
         numerator = (k6(iv1)*abb94n15+qshift(iv1)*abb94n17+t3*abb94n15+t4*abb94&
         &n16+t5*abb94n14+t1*t3*abb94n12+t1*t4*abb94n11+t2*t5*abb94n11+t5*t6*abb&
         &94n12)
         return
      end if
      t1 = spvak6k2(iv2)
      if(deg.eq.2) then
         numerator = ((2.0_ki)*d(iv1,iv2)*abb94n13+k1(iv2)*t5*abb94n12+spvak6k1(&
         &iv2)*t5*abb94n11+t1*t3*abb94n12+t1*t4*abb94n11)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d94:
   subroutine     reconstruct_d94(coeffs)
      use p6_ubbar_hepneg_groups, only: tensrec_info_group3
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)

      type(tensrec_info_group3), intent(out) :: coeffs
      ! rank 2 case:
      !---[# reconstruct coeffs%coeffs_94:
      coeffs%coeffs_94%c0 = derivative(czip)
      coeffs%coeffs_94%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_94%c1(1,2) = derivative(czip,1,1) / 2.0_ki
      coeffs%coeffs_94%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_94%c1(2,2) = derivative(czip,2,2) / 2.0_ki
      coeffs%coeffs_94%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_94%c1(3,2) = derivative(czip,3,3) / 2.0_ki
      coeffs%coeffs_94%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_94%c1(4,2) = derivative(czip,4,4) / 2.0_ki
      coeffs%coeffs_94%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_94%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_94%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_94%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_94%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_94%c2(6,1) = derivative(czip,3,4)
      !---#] reconstruct coeffs%coeffs_94:
   end subroutine reconstruct_d94
!---#] subroutine reconstruct_d94:
end module p6_ubbar_hepneg_d94h1l1d
