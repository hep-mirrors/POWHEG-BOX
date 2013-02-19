module     p6_ubbar_hepneg_d30h0l1d
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! m/Virtual/p6_ubbar_hepneg/helicity0/d30h0l1d.f90
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


   public :: derivative, reconstruct_d30
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p6_ubbar_hepneg_model
      use p6_ubbar_hepneg_kinematics
      use p6_ubbar_hepneg_color
      use p6_ubbar_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3
      real(ki) :: t4
      real(ki) :: t5
      t1 = (e6(1)*qshift(1)-e6(2)*qshift(2)-e6(3)*qshift(3)-e6(4)*qshift(4))
      t2 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t3 = (spvae6k1(1)*qshift(1)-spvae6k1(2)*qshift(2)-spvae6k1(3)*qshift(3)-sp&
      &vae6k1(4)*qshift(4))
      t4 = dotproduct(k1, qshift)
      t5 = dotproduct(k6, qshift)
      brack = (abb30n18+t1*abb30n26+t2*abb30n27+t3*abb30n31+t4*abb30n32+t5*abb30&
      &n33+abb30n24*dotproduct(qshift, qshift)+abb30n28*(spvak2k4(1)*qshift(1)-s&
      &pvak2k4(2)*qshift(2)-spvak2k4(3)*qshift(3)-spvak2k4(4)*qshift(4))+abb30n2&
      &9*dotproduct(k2, qshift)+t1*t2*abb30n25+t2*t3*abb30n23+t2*t4*abb30n20+t2*&
      &t5*abb30n21+t2*abb30n19*(spvak2k6(1)*qshift(1)-spvak2k6(2)*qshift(2)-spva&
      &k2k6(3)*qshift(3)-spvak2k6(4)*qshift(4))+t2*abb30n22*(spvak2k1(1)*qshift(&
      &1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-spvak2k1(4)*qshift(4)))
   end  function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p6_ubbar_hepneg_model
      use p6_ubbar_hepneg_kinematics
      use p6_ubbar_hepneg_color
      use p6_ubbar_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3
      complex(ki) :: t4
      complex(ki) :: t5
      complex(ki) :: t6
      t1 = e6(iv1)
      t2 = k1(iv1)
      t3 = k6(iv1)
      t4 = spvae6k1(iv1)
      t5 = spvak5k4(iv1)
      t6 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      brack = (k2(iv1)*abb30n29+qshift(iv1)*abb30n30+spvak2k4(iv1)*abb30n28+t1*a&
      &bb30n26+t2*abb30n32+t3*abb30n33+t4*abb30n31+t5*abb30n27+spvak2k1(iv1)*t6*&
      &abb30n22+spvak2k6(iv1)*t6*abb30n19+t1*t6*abb30n25+t2*t6*abb30n20+t3*t6*ab&
      &b30n21+t4*t6*abb30n23+t5*abb30n19*(spvak2k6(1)*qshift(1)-spvak2k6(2)*qshi&
      &ft(2)-spvak2k6(3)*qshift(3)-spvak2k6(4)*qshift(4))+t5*abb30n20*dotproduct&
      &(k1, qshift)+t5*abb30n21*dotproduct(k6, qshift)+t5*abb30n22*(spvak2k1(1)*&
      &qshift(1)-spvak2k1(2)*qshift(2)-spvak2k1(3)*qshift(3)-spvak2k1(4)*qshift(&
      &4))+t5*abb30n23*(spvae6k1(1)*qshift(1)-spvae6k1(2)*qshift(2)-spvae6k1(3)*&
      &qshift(3)-spvae6k1(4)*qshift(4))+t5*abb30n25*(e6(1)*qshift(1)-e6(2)*qshif&
      &t(2)-e6(3)*qshift(3)-e6(4)*qshift(4)))
   end  function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p6_ubbar_hepneg_model
      use p6_ubbar_hepneg_kinematics
      use p6_ubbar_hepneg_color
      use p6_ubbar_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: t1
      complex(ki) :: t2
      t1 = spvak5k4(iv2)
      t2 = spvak5k4(iv1)
      brack = ((2.0_ki)*d(iv1,iv2)*abb30n24+e6(iv1)*t1*abb30n25+e6(iv2)*t2*abb30&
      &n25+k1(iv1)*t1*abb30n20+k1(iv2)*t2*abb30n20+k6(iv1)*t1*abb30n21+k6(iv2)*t&
      &2*abb30n21+spvae6k1(iv1)*t1*abb30n23+spvae6k1(iv2)*t2*abb30n23+spvak2k1(i&
      &v1)*t1*abb30n22+spvak2k1(iv2)*t2*abb30n22+spvak2k6(iv1)*t1*abb30n19+spvak&
      &2k6(iv2)*t2*abb30n19)
   end  function brack_3
!---#] function brack_3:

!---#[ function derivative:
   function derivative(mu2, i1, i2) result(numerator)
      use p6_ubbar_hepneg_globalsl1, only: epspow
      use p6_ubbar_hepneg_kinematics
      use p6_ubbar_hepneg_abbrevh0
      implicit none
      complex(ki), intent(in) :: mu2
      integer, intent(in), optional :: i1
      integer, intent(in), optional :: i2
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
      t1 = 0
      if(deg.eq.0) then
         numerator = (cond(epspow.eq.t1,brack_1,Q,mu2))
         return
      end if
      if(deg.eq.1) then
         numerator = (cond(epspow.eq.t1,brack_2,Q,mu2))
         return
      end if
      if(deg.eq.2) then
         numerator = (cond(epspow.eq.t1,brack_3,Q,mu2))
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d30:
   subroutine     reconstruct_d30(coeffs)
      use p6_ubbar_hepneg_groups, only: tensrec_info_group3
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
end module p6_ubbar_hepneg_d30h0l1d
