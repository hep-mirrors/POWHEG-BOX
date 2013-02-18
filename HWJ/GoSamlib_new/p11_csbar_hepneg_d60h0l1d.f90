module     p11_csbar_hepneg_d60h0l1d
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! mlib_New/Virtual/p11_csbar_hepneg/helicity0/d60h0l1d.f90
   ! generator: haggies (1.1)
   use p11_csbar_hepneg_config, only: ki
   use p11_csbar_hepneg_util, only: cond, d => metric_tensor
   

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
!---#[ function brack_1:
   pure function brack_1(Q) result(brack)
      use p11_csbar_hepneg_model
      use p11_csbar_hepneg_kinematics
      use p11_csbar_hepneg_color
      use p11_csbar_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki) :: brack
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3
      complex(ki) :: t4
      real(ki) :: t5
      real(ki) :: t6
      real(ki) :: t7
      t1 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t5 = dotproduct(qshift, qshift)
      t2 = (spvak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-sp&
      &vak2k6(4)*qshift(4))
      t6 = dotproduct(k2, qshift)
      t7 = dotproduct(k6, qshift)
      t3 = (spvak5k1(1)*qshift(1)-spvak5k1(2)*qshift(2)-spvak5k1(3)*qshift(3)-sp&
      &vak5k1(4)*qshift(4))
      t4 = (spvak2k1(1)*qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-sp&
      &vak2k1(4)*qshift(4))
      brack = (t1*abb60n18+t2*abb60n30+t2*t2*abb60n26+t3*abb60n31+t5*abb60n25+t6&
      &*abb60n28+t7*abb60n29+t1*t2*abb60n24+t2*t3*abb60n19+t2*t4*abb60n23+t2*t7*&
      &abb60n27+t2*abb60n21*(spvak6k1(1)*qshift(1)-spvak6k1(2)*qshift(2)-spvak6k&
      &1(3)*qshift(3)-spvak6k1(4)*qshift(4))+t4*t5*abb60n34+t4*t6*abb60n22+t4*t7&
      &*abb60n20+t1*t2*t4*abb60n36)
   end  function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q) result(brack)
      use p11_csbar_hepneg_model
      use p11_csbar_hepneg_kinematics
      use p11_csbar_hepneg_color
      use p11_csbar_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki) :: brack
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
      t1 = k2(iv1)
      t2 = k6(iv1)
      t3 = qshift(iv1)
      t4 = spvak2k6(iv1)
      t5 = spvak5k1(iv1)
      t6 = spvak5k4(iv1)
      t7 = (spvak2k1(1)*qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-sp&
      &vak2k1(4)*qshift(4))
      t8 = (spvak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-sp&
      &vak2k6(4)*qshift(4))
      t9 = spvak2k1(iv1)
      t11 = dotproduct(k6, qshift)
      t10 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-s&
      &pvak5k4(4)*qshift(4))
      brack = (t1*abb60n28+t2*abb60n29+t3*abb60n33+t4*abb60n30+t5*abb60n31+t6*ab&
      &b60n18+spvak6k1(iv1)*t8*abb60n21+t1*t7*abb60n22+t10*t4*abb60n24+t11*t4*ab&
      &b60n27+t11*t9*abb60n20+t2*t7*abb60n20+t2*t8*abb60n27+t3*t7*abb60n35+t4*t7&
      &*abb60n23+t4*t8*abb60n32+t4*abb60n19*(spvak5k1(1)*qshift(1)-spvak5k1(2)*q&
      &shift(2)-spvak5k1(3)*qshift(3)-spvak5k1(4)*qshift(4))+t4*abb60n21*(spvak6&
      &k1(1)*qshift(1)-spvak6k1(2)*qshift(2)-spvak6k1(3)*qshift(3)-spvak6k1(4)*q&
      &shift(4))+t5*t8*abb60n19+t6*t8*abb60n24+t8*t9*abb60n23+t9*abb60n22*dotpro&
      &duct(k2, qshift)+t9*abb60n34*dotproduct(qshift, qshift)+t10*t4*t7*abb60n3&
      &6+t10*t8*t9*abb60n36+t6*t7*t8*abb60n36)
   end  function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q) result(brack)
      use p11_csbar_hepneg_model
      use p11_csbar_hepneg_kinematics
      use p11_csbar_hepneg_color
      use p11_csbar_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki) :: brack
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
      real(ki) :: t16
      t16 = d(iv1,iv2)
      t1 = spvak2k6(iv1)
      t2 = spvak2k6(iv2)
      t3 = (spvak2k1(1)*qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-sp&
      &vak2k1(4)*qshift(4))
      t4 = spvak2k1(iv2)
      t5 = spvak2k1(iv1)
      t6 = k6(iv1)
      t7 = k6(iv2)
      t8 = t2*t5
      t9 = t1*t4
      t10 = spvak5k4(iv2)
      t11 = t1*t10
      t12 = spvak5k4(iv1)
      t13 = t12*t2
      t14 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-s&
      &pvak5k4(4)*qshift(4))
      t15 = (spvak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spvak2k6(3)*qshift(3)-s&
      &pvak2k6(4)*qshift(4))
      brack = ((2.0_ki)*(t16*abb60n25+t1*t2*abb60n26+t16*t3*abb60n34)+t11*abb60n&
      &24+t13*abb60n24+t8*abb60n23+t9*abb60n23+k2(iv1)*t4*abb60n22+k2(iv2)*t5*ab&
      &b60n22+qshift(iv1)*t4*abb60n35+qshift(iv2)*t5*abb60n35+spvak5k1(iv1)*t2*a&
      &bb60n19+spvak5k1(iv2)*t1*abb60n19+spvak6k1(iv1)*t2*abb60n21+spvak6k1(iv2)&
      &*t1*abb60n21+t1*t7*abb60n27+t11*t3*abb60n36+t13*t3*abb60n36+t14*t8*abb60n&
      &36+t14*t9*abb60n36+t2*t6*abb60n27+t4*t6*abb60n20+t5*t7*abb60n20+t10*t15*t&
      &5*abb60n36+t12*t15*t4*abb60n36)
   end  function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q) result(brack)
      use p11_csbar_hepneg_model
      use p11_csbar_hepneg_kinematics
      use p11_csbar_hepneg_color
      use p11_csbar_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki) :: brack
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3
      complex(ki) :: t4
      complex(ki) :: t5
      complex(ki) :: t6
      complex(ki) :: t7
      complex(ki) :: t8
      complex(ki) :: t9
      t1 = spvak2k1(iv1)
      t2 = spvak2k1(iv2)
      t3 = spvak2k1(iv3)
      t4 = spvak2k6(iv2)
      t5 = spvak5k4(iv3)
      t6 = spvak2k6(iv3)
      t7 = spvak5k4(iv2)
      t8 = spvak2k6(iv1)
      t9 = spvak5k4(iv1)
      brack = ((2.0_ki)*(d(iv1,iv2)*t3*abb60n34+d(iv1,iv3)*t2*abb60n34+d(iv2,iv3&
      &)*t1*abb60n34)+t1*t4*t5*abb60n36+t1*t6*t7*abb60n36+t2*t5*t8*abb60n36+t2*t&
      &6*t9*abb60n36+t3*t4*t9*abb60n36+t3*t7*t8*abb60n36)
   end  function brack_4
!---#] function brack_4:

!---#[ function derivative:
   function derivative(mu2, i1, i2, i3) result(numerator)
      use p11_csbar_hepneg_globalsl1, only: epspow
      use p11_csbar_hepneg_kinematics
      use p11_csbar_hepneg_abbrevh0
      implicit none
      complex(ki), intent(in) :: mu2
      integer, intent(in), optional :: i1
      integer, intent(in), optional :: i2
      integer, intent(in), optional :: i3
      complex(ki) :: numerator
      complex(ki) :: loc
      integer :: t1
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
      t1 = 0
      if(deg.eq.0) then
         numerator = (cond(epspow.eq.t1,brack_1(Q),Q,mu2))
         return
      end if
      if(deg.eq.1) then
         numerator = (cond(epspow.eq.t1,brack_2(Q),Q,mu2))
         return
      end if
      if(deg.eq.2) then
         numerator = (cond(epspow.eq.t1,brack_3(Q),Q,mu2))
         return
      end if
      if(deg.eq.3) then
         numerator = (cond(epspow.eq.t1,brack_4(Q),Q,mu2))
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d60:
   subroutine     reconstruct_d60(coeffs)
      use p11_csbar_hepneg_groups, only: tensrec_info_group2
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
end module p11_csbar_hepneg_d60h0l1d
