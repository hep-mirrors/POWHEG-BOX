module     p0_dbaru_hepneg_d60h1l1d
   ! file:      /home/oleari/fortran/POWHEG-BOX/HWJ_CKM/GoSam_POWHEG/Virtual/p0_
   ! dbaru_hepneg/helicity1/d60h1l1d.f90
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


   public :: derivative, reconstruct_d60
contains

!---#[ function derivative:
   function derivative(mu2, i1, i2, i3) result(numerator)
      use p0_dbaru_hepneg_globalsl1, only: epspow
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_abbrevh1
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
      complex(ki) :: t20
      complex(ki) :: t21
      complex(ki) :: t22
      complex(ki) :: t23
      real(ki) :: t24
      real(ki) :: t25
      real(ki) :: t26
      real(ki) :: t27
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
      t24 = dotproduct(qshift, qshift)
      t1 = (spvak6k2(1)*qshift(1)-spvak6k2(2)*qshift(2)-spvak6k2(3)*qshift(3)-sp&
      &vak6k2(4)*qshift(4))
      t2 = (spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-sp&
      &vak5k2(4)*qshift(4))
      t25 = dotproduct(k2, qshift)
      t3 = (spvak1k4(1)*qshift(1)-spvak1k4(2)*qshift(2)-spvak1k4(3)*qshift(3)-sp&
      &vak1k4(4)*qshift(4))
      t26 = dotproduct(k6, qshift)
      t27 = dotproduct(k1, qshift)
      t4 = (spvak1k2(1)*qshift(1)-spvak1k2(2)*qshift(2)-spvak1k2(3)*qshift(3)-sp&
      &vak1k2(4)*qshift(4))
      t5 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      if(deg.eq.0) then
         numerator = (abb60n17+t1*abb60n27+t1*t1*abb60n23+t2*abb60n26+t24*abb60n&
         &22+t25*abb60n28+t1*t2*abb60n19+t1*t24*abb60n29+t1*t26*abb60n24+t1*t27*&
         &abb60n24+t1*t3*abb60n18+t1*t4*abb60n25+t2*t24*abb60n33+t2*t25*abb60n21&
         &+t2*t26*abb60n20+t1*t4*t5*abb60n35)
         return
      end if
      t6 = k2(iv1)
      t7 = qshift(iv1)
      t8 = spvak5k2(iv1)
      t9 = spvak6k2(iv1)
      t10 = k1(iv1)
      t11 = k6(iv1)
      t12 = spvak1k2(iv1)
      t13 = spvak1k4(iv1)
      t14 = t9*abb60n24
      t15 = spvak5k4(iv1)
      if(deg.eq.1) then
         numerator = (t14*t26+t14*t27+t6*abb60n28+t7*abb60n32+t8*abb60n26+t9*abb&
         &60n27+t1*t10*abb60n24+t1*t11*abb60n24+t1*t12*abb60n25+t1*t13*abb60n18+&
         &t1*t7*abb60n30+t1*t8*abb60n19+t1*t9*abb60n31+t11*t2*abb60n20+t2*t6*abb&
         &60n21+t2*t7*abb60n34+t2*t9*abb60n19+t24*t8*abb60n33+t24*t9*abb60n29+t2&
         &5*t8*abb60n21+t26*t8*abb60n20+t3*t9*abb60n18+t4*t9*abb60n25+t1*t12*t5*&
         &abb60n35+t1*t15*t4*abb60n35+t4*t5*t9*abb60n35)
         return
      end if
      t24 = d(iv1,iv2)
      t3 = spvak6k2(iv2)
      t14 = spvak5k2(iv2)
      t16 = k6(iv2)
      t17 = qshift(iv2)
      t18 = t12*t3
      t19 = spvak1k2(iv2)
      t20 = t19*t9
      t21 = spvak5k4(iv2)
      t22 = t12*t21
      t23 = t15*t19
      if(deg.eq.2) then
         numerator = ((2.0_ki)*(t24*abb60n22+t1*t24*abb60n29+t2*t24*abb60n33+t3*&
         &t9*abb60n23)+t18*abb60n25+t20*abb60n25+k1(iv2)*t9*abb60n24+k2(iv2)*t8*&
         &abb60n21+spvak1k4(iv2)*t9*abb60n18+t1*t22*abb60n35+t1*t23*abb60n35+t10&
         &*t3*abb60n24+t11*t14*abb60n20+t11*t3*abb60n24+t13*t3*abb60n18+t14*t6*a&
         &bb60n21+t14*t7*abb60n34+t14*t9*abb60n19+t16*t8*abb60n20+t16*t9*abb60n2&
         &4+t17*t8*abb60n34+t17*t9*abb60n30+t18*t5*abb60n35+t20*t5*abb60n35+t3*t&
         &7*abb60n30+t3*t8*abb60n19+t15*t3*t4*abb60n35+t21*t4*t9*abb60n35)
         return
      end if
      t25 = d(iv2,iv3)
      t26 = d(iv1,iv3)
      t1 = spvak6k2(iv3)
      t2 = spvak5k4(iv3)
      t4 = spvak1k2(iv3)
      if(deg.eq.3) then
         numerator = ((2.0_ki)*(spvak5k2(iv3)*t24*abb60n33+t1*t24*abb60n29+t14*t&
         &26*abb60n33+t25*t8*abb60n33+t25*t9*abb60n29+t26*t3*abb60n29)+t1*t22*ab&
         &b60n35+t1*t23*abb60n35+t12*t2*t3*abb60n35+t15*t3*t4*abb60n35+t19*t2*t9&
         &*abb60n35+t21*t4*t9*abb60n35)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d60:
   subroutine     reconstruct_d60(coeffs)
      use p0_dbaru_hepneg_groups, only: tensrec_info_group2
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
end module p0_dbaru_hepneg_d60h1l1d
