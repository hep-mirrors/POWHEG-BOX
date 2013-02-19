module     p1_dbarc_hepneg_d94h0l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p1_
   ! dbarc_hepneg/helicity0/d94h0l1d.f90
   ! generator: haggies (1.1)
   use p1_dbarc_hepneg_config, only: ki
   use p1_dbarc_hepneg_util, only: cond, d => metric_tensor
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
      use p1_dbarc_hepneg_globalsl1, only: epspow
      use p1_dbarc_hepneg_kinematics
      use p1_dbarc_hepneg_abbrevh0
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
      complex(ki) :: t8
      complex(ki) :: t9
      complex(ki) :: t10
      real(ki) :: t11
      real(ki) :: t12
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
      t1 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t2 = (spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-sp&
      &vak5k2(4)*qshift(4))
      t11 = dotproduct(k1, qshift)
      t3 = (spvak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-sp&
      &vak2k6(4)*qshift(4))
      t12 = dotproduct(k6, qshift)
      t4 = (spvak1k6(1)*qshift(1)-spvak1k6(2)*qshift(2)-spvak1k6(3)*qshift(3)-sp&
      &vak1k6(4)*qshift(4))
      if(deg.eq.0) then
         numerator = (abb94n16+t1*abb94n23+t11*abb94n25+t12*abb94n28+t2*abb94n24&
         &+t3*abb94n26+t4*abb94n29+abb94n22*dotproduct(qshift, qshift)+t1*t3*abb&
         &94n19+t11*t3*abb94n20+t12*t3*abb94n21+t2*t3*abb94n18+t3*t4*abb94n17)
         return
      end if
      t5 = k1(iv1)
      t6 = k6(iv1)
      t7 = spvak1k6(iv1)
      t8 = spvak2k6(iv1)
      t9 = spvak5k2(iv1)
      t10 = spvak5k4(iv1)
      if(deg.eq.1) then
         numerator = (qshift(iv1)*abb94n27+t10*abb94n23+t5*abb94n25+t6*abb94n28+&
         &t7*abb94n29+t8*abb94n26+t9*abb94n24+t1*t8*abb94n19+t10*t3*abb94n19+t11&
         &*t8*abb94n20+t12*t8*abb94n21+t2*t8*abb94n18+t3*t5*abb94n20+t3*t6*abb94&
         &n21+t3*t7*abb94n17+t3*t9*abb94n18+t4*t8*abb94n17)
         return
      end if
      t1 = spvak2k6(iv2)
      if(deg.eq.2) then
         numerator = ((2.0_ki)*d(iv1,iv2)*abb94n22+k1(iv2)*t8*abb94n20+k6(iv2)*t&
         &8*abb94n21+spvak1k6(iv2)*t8*abb94n17+spvak5k2(iv2)*t8*abb94n18+spvak5k&
         &4(iv2)*t8*abb94n19+t1*t10*abb94n19+t1*t5*abb94n20+t1*t6*abb94n21+t1*t7&
         &*abb94n17+t1*t9*abb94n18)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d94:
   subroutine     reconstruct_d94(coeffs)
      use p1_dbarc_hepneg_groups, only: tensrec_info_group3
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
end module p1_dbarc_hepneg_d94h0l1d
