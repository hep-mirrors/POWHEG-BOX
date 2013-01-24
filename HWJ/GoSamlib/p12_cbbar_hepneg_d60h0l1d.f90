module     p12_cbbar_hepneg_d60h0l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p12
   ! _cbbar_hepneg/helicity0/d60h0l1d.f90
   ! generator: haggies (1.1)
   use p12_cbbar_hepneg_config, only: ki
   use p12_cbbar_hepneg_util, only: cond, d => metric_tensor
   implicit none
   private

   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   integer, private :: iv2
   integer, private :: iv3
   real(ki), dimension(4), private :: qshift


   public :: derivative, reconstruct_d60
contains

!---#[ function derivative:
   function derivative(mu2, i1, i2, i3) result(numerator)
      use p12_cbbar_hepneg_globalsl1, only: epspow
      use p12_cbbar_hepneg_kinematics
      use p12_cbbar_hepneg_abbrevh0
      implicit none
      complex(ki), intent(in) :: mu2
      integer, intent(in), optional :: i1
      integer, intent(in), optional :: i2
      integer, intent(in), optional :: i3
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
      complex(ki) :: t11
      complex(ki) :: t12
      complex(ki) :: t13
      complex(ki) :: t14
      complex(ki) :: t15
      complex(ki) :: t16
      complex(ki) :: t17
      complex(ki) :: t18
      complex(ki) :: t19
      real(ki) :: t20
      real(ki) :: t21
      real(ki) :: t22
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
      if(present(i3)) then
         iv3 = i3
         deg = 3
      else
         iv3 = 1
      end if
      t20 = dotproduct(qshift, qshift)
      t1 = (spvak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-sp&
      &vak2k6(4)*qshift(4))
      t2 = (spvak2k1(1)*qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-sp&
      &vak2k1(4)*qshift(4))
      t21 = dotproduct(k2, qshift)
      t22 = dotproduct(k6, qshift)
      t3 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t4 = (spvak5k1(1)*qshift(1)-spvak5k1(2)*qshift(2)-spvak5k1(3)*qshift(3)-sp&
      &vak5k1(4)*qshift(4))
      t5 = (spvak6k1(1)*qshift(1)-spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-sp&
      &vak6k1(4)*qshift(4))
      if(deg.eq.0) then
         numerator = (abb60n20+t1*abb60n33+t1*t1*abb60n28+t2*abb60n30+t20*abb60n&
         &27+t21*abb60n31+t22*abb60n32+t3*abb60n34+t4*abb60n35+t1*t2*abb60n25+t1&
         &*t22*abb60n29+t1*t3*abb60n26+t1*t4*abb60n21+t1*t5*abb60n23+t2*t20*abb6&
         &0n38+t2*t21*abb60n24+t2*t22*abb60n22+t1*t2*t3*abb60n40)
         return
      end if
      t6 = k2(iv1)
      t7 = k6(iv1)
      t8 = qshift(iv1)
      t9 = spvak2k1(iv1)
      t10 = spvak2k6(iv1)
      t11 = spvak5k1(iv1)
      t12 = spvak5k4(iv1)
      t13 = spvak6k1(iv1)
      if(deg.eq.1) then
         numerator = (t10*abb60n33+t11*abb60n35+t12*abb60n34+t6*abb60n31+t7*abb6&
         &0n32+t8*abb60n37+t9*abb60n30+t1*t10*abb60n36+t1*t11*abb60n21+t1*t12*ab&
         &b60n26+t1*t13*abb60n23+t1*t7*abb60n29+t1*t9*abb60n25+t10*t2*abb60n25+t&
         &10*t22*abb60n29+t10*t3*abb60n26+t10*t4*abb60n21+t10*t5*abb60n23+t2*t6*&
         &abb60n24+t2*t7*abb60n22+t2*t8*abb60n39+t20*t9*abb60n38+t21*t9*abb60n24&
         &+t22*t9*abb60n22+t1*t12*t2*abb60n40+t1*t3*t9*abb60n40+t10*t2*t3*abb60n&
         &40)
         return
      end if
      t20 = d(iv1,iv2)
      t4 = spvak2k6(iv2)
      t5 = spvak2k1(iv2)
      t14 = k6(iv2)
      t15 = t4*t9
      t16 = t10*t5
      t17 = spvak5k4(iv2)
      t18 = t10*t17
      t19 = t12*t4
      if(deg.eq.2) then
         numerator = ((2.0_ki)*(t20*abb60n27+t10*t4*abb60n28+t2*t20*abb60n38)+t1&
         &5*abb60n25+t16*abb60n25+t18*abb60n26+t19*abb60n26+k2(iv2)*t9*abb60n24+&
         &qshift(iv2)*t9*abb60n39+spvak5k1(iv2)*t10*abb60n21+spvak6k1(iv2)*t10*a&
         &bb60n23+t10*t14*abb60n29+t11*t4*abb60n21+t13*t4*abb60n23+t14*t9*abb60n&
         &22+t15*t3*abb60n40+t16*t3*abb60n40+t18*t2*abb60n40+t19*t2*abb60n40+t4*&
         &t7*abb60n29+t5*t6*abb60n24+t5*t7*abb60n22+t5*t8*abb60n39+t1*t12*t5*abb&
         &60n40+t1*t17*t9*abb60n40)
         return
      end if
      t1 = spvak2k1(iv3)
      t2 = spvak5k4(iv3)
      t3 = spvak2k6(iv3)
      if(deg.eq.3) then
         numerator = ((2.0_ki)*(d(iv1,iv3)*t5*abb60n38+d(iv2,iv3)*t9*abb60n38+t1&
         &*t20*abb60n38)+t15*t2*abb60n40+t16*t2*abb60n40+t1*t10*t17*abb60n40+t1*&
         &t12*t4*abb60n40+t12*t3*t5*abb60n40+t17*t3*t9*abb60n40)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d60:
   subroutine     reconstruct_d60(coeffs)
      use p12_cbbar_hepneg_groups, only: tensrec_info_group2
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)

      type(tensrec_info_group2), intent(out) :: coeffs
      ! rank 3 case:
      !---[# reconstruct coeffs%coeffs_60:
      coeffs%coeffs_60%c0 = derivative(czip)
      coeffs%coeffs_60%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_60%c1(1,2) = derivative(czip,1,1) / 2.0_ki
      coeffs%coeffs_60%c1(1,3) = derivative(czip,1,1,1) / 6.0_ki
      coeffs%coeffs_60%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_60%c1(2,2) = derivative(czip,2,2) / 2.0_ki
      coeffs%coeffs_60%c1(2,3) = -derivative(czip,2,2,2) / 6.0_ki
      coeffs%coeffs_60%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_60%c1(3,2) = derivative(czip,3,3) / 2.0_ki
      coeffs%coeffs_60%c1(3,3) = -derivative(czip,3,3,3) / 6.0_ki
      coeffs%coeffs_60%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_60%c1(4,2) = derivative(czip,4,4) / 2.0_ki
      coeffs%coeffs_60%c1(4,3) = -derivative(czip,4,4,4) / 6.0_ki
      coeffs%coeffs_60%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_60%c2(1,2) = derivative(czip,1,2,2) / 2.0_ki
      coeffs%coeffs_60%c2(1,3) = -derivative(czip,1,1,2) / 2.0_ki
      coeffs%coeffs_60%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_60%c2(2,2) = derivative(czip,1,3,3) / 2.0_ki
      coeffs%coeffs_60%c2(2,3) = -derivative(czip,1,1,3) / 2.0_ki
      coeffs%coeffs_60%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_60%c2(3,2) = derivative(czip,1,4,4) / 2.0_ki
      coeffs%coeffs_60%c2(3,3) = -derivative(czip,1,1,4) / 2.0_ki
      coeffs%coeffs_60%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_60%c2(4,2) = -derivative(czip,2,3,3) / 2.0_ki
      coeffs%coeffs_60%c2(4,3) = -derivative(czip,2,2,3) / 2.0_ki
      coeffs%coeffs_60%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_60%c2(5,2) = -derivative(czip,2,4,4) / 2.0_ki
      coeffs%coeffs_60%c2(5,3) = -derivative(czip,2,2,4) / 2.0_ki
      coeffs%coeffs_60%c2(6,1) = derivative(czip,3,4)
      coeffs%coeffs_60%c2(6,2) = -derivative(czip,3,4,4) / 2.0_ki
      coeffs%coeffs_60%c2(6,3) = -derivative(czip,3,3,4) / 2.0_ki
      coeffs%coeffs_60%c3(1,1) = derivative(czip,1,2,3)
      coeffs%coeffs_60%c3(2,1) = derivative(czip,1,2,4)
      coeffs%coeffs_60%c3(3,1) = derivative(czip,1,3,4)
      coeffs%coeffs_60%c3(4,1) = -derivative(czip,2,3,4)
      !---#] reconstruct coeffs%coeffs_60:
   end subroutine reconstruct_d60
!---#] subroutine reconstruct_d60:
end module p12_cbbar_hepneg_d60h0l1d
