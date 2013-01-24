module     p1_dbarc_hepneg_d30h0l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p1_
   ! dbarc_hepneg/helicity0/d30h0l1d.f90
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


   public :: derivative, reconstruct_d30
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
      t2 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      if(deg.eq.0) then
         numerator = (abb30n12+t1*abb30n17+abb30n14*dotproduct(qshift, qshift)+a&
         &bb30n15*(spvak5k6(1)*qshift(1)-spvak5k6(2)*qshift(2)-spvak5k6(3)*qshif&
         &t(3)-spvak5k6(4)*qshift(4))+abb30n16*(spvak5k2(1)*qshift(1)-spvak5k2(2&
         &)*qshift(2)-spvak5k2(3)*qshift(3)-spvak5k2(4)*qshift(4))+abb30n18*dotp&
         &roduct(k1, qshift)+abb30n20*(spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(&
         &2)-spvak6k2(3)*qshift(3)-spvak6k2(4)*qshift(4))+abb30n21*(spvak1k6(1)*&
         &qshift(1)-spvak1k6(2)*qshift(2)-spvak1k6(3)*qshift(3)-spvak1k6(4)*qshi&
         &ft(4))+t1*t2*abb30n13)
         return
      end if
      t3 = spvak1k2(iv1)
      t4 = spvak5k4(iv1)
      if(deg.eq.1) then
         numerator = (k1(iv1)*abb30n18+qshift(iv1)*abb30n19+spvak1k6(iv1)*abb30n&
         &21+spvak5k2(iv1)*abb30n16+spvak5k6(iv1)*abb30n15+spvak6k2(iv1)*abb30n2&
         &0+t3*abb30n17+t1*t4*abb30n13+t2*t3*abb30n13)
         return
      end if
      if(deg.eq.2) then
         numerator = ((2.0_ki)*d(iv1,iv2)*abb30n14+spvak1k2(iv2)*t4*abb30n13+spv&
         &ak5k4(iv2)*t3*abb30n13)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d30:
   subroutine     reconstruct_d30(coeffs)
      use p1_dbarc_hepneg_groups, only: tensrec_info_group3
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
end module p1_dbarc_hepneg_d30h0l1d
