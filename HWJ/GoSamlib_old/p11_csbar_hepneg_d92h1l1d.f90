module     p11_csbar_hepneg_d92h1l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p11
   ! _csbar_hepneg/helicity1/d92h1l1d.f90
   ! generator: haggies (1.1)
   use p11_csbar_hepneg_config, only: ki
   use p11_csbar_hepneg_util, only: cond, d => metric_tensor
   implicit none
   private

   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   integer, private :: iv2
   real(ki), dimension(4), private :: qshift


   public :: derivative, reconstruct_d92
contains

!---#[ function derivative:
   function derivative(mu2, i1, i2) result(numerator)
      use p11_csbar_hepneg_globalsl1, only: epspow
      use p11_csbar_hepneg_kinematics
      use p11_csbar_hepneg_abbrevh1
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
      if(deg.eq.0) then
         numerator = abb92n15
         return
      end if
      t1 = k2(iv1)
      t2 = k6(iv1)
      t3 = spvak5k1(iv1)
      t4 = spvak5k4(iv1)
      t5 = spvak6k2(iv1)
      if(deg.eq.1) then
         numerator = (t1*abb92n25+t2*abb92n21+t3*abb92n24+t4*abb92n26+t5*abb92n2&
         &3)
         return
      end if
      t6 = spvak6k2(iv2)
      if(deg.eq.2) then
         numerator = ((2.0_ki)*d(iv1,iv2)*abb92n22+k2(iv2)*t5*abb92n20+k6(iv2)*t&
         &5*abb92n19+spvak2k6(iv1)*t6*abb92n18+spvak2k6(iv2)*t5*abb92n18+spvak5k&
         &1(iv2)*t5*abb92n16+spvak5k4(iv2)*t5*abb92n17+t1*t6*abb92n20+t2*t6*abb9&
         &2n19+t3*t6*abb92n16+t4*t6*abb92n17)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d92:
   subroutine     reconstruct_d92(coeffs)
      use p11_csbar_hepneg_groups, only: tensrec_info_group2
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)

      type(tensrec_info_group2), intent(out) :: coeffs
      ! rank 2 case:
      !---[# reconstruct coeffs%coeffs_92:
      coeffs%coeffs_92%c0 = derivative(czip)
      coeffs%coeffs_92%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_92%c1(1,2) = derivative(czip,1,1) / 2.0_ki
      coeffs%coeffs_92%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_92%c1(2,2) = derivative(czip,2,2) / 2.0_ki
      coeffs%coeffs_92%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_92%c1(3,2) = derivative(czip,3,3) / 2.0_ki
      coeffs%coeffs_92%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_92%c1(4,2) = derivative(czip,4,4) / 2.0_ki
      coeffs%coeffs_92%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_92%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_92%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_92%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_92%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_92%c2(6,1) = derivative(czip,3,4)
      !---#] reconstruct coeffs%coeffs_92:
   end subroutine reconstruct_d92
!---#] subroutine reconstruct_d92:
end module p11_csbar_hepneg_d92h1l1d
