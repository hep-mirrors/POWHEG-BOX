module     p0_dbaru_hepneg_d94h0l1d
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! m/Virtual/p0_dbaru_hepneg/helicity0/d94h0l1d.f90
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


   public :: derivative, reconstruct_d94
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p0_dbaru_hepneg_model
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_color
      use p0_dbaru_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: t1
      complex(ki) :: t2
      complex(ki) :: t3
      real(ki) :: t4
      real(ki) :: t5
      t1 = (spvak5k4(1)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-sp&
      &vak5k4(4)*qshift(4))
      t2 = (spvak5k2(1)*qshift(1)-spvak5k2(2)*qshift(2)-spvak5k2(3)*qshift(3)-sp&
      &vak5k2(4)*qshift(4))
      t3 = (e6(1)*qshift(1)-e6(2)*qshift(2)-e6(3)*qshift(3)-e6(4)*qshift(4))
      t4 = dotproduct(k6, qshift)
      t5 = dotproduct(k1, qshift)
      brack = (abb94n16+t1*abb94n23+t2*abb94n24+t3*abb94n25+t4*abb94n27+t5*abb94&
      &n28+abb94n18*dotproduct(qshift, qshift)+abb94n26*(spvak1e6(1)*qshift(1)-s&
      &pvak1e6(2)*qshift(2)-spvak1e6(3)*qshift(3)-spvak1e6(4)*qshift(4))+t1*t3*a&
      &bb94n20+t2*t3*abb94n19+t3*t4*abb94n22+t3*t5*abb94n21+t3*abb94n17*(spvak1k&
      &6(1)*qshift(1)-spvak1k6(2)*qshift(2)-spvak1k6(3)*qshift(3)-spvak1k6(4)*qs&
      &hift(4)))
   end  function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p0_dbaru_hepneg_model
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_color
      use p0_dbaru_hepneg_abbrevh0
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
      t4 = spvak5k2(iv1)
      t5 = spvak5k4(iv1)
      t6 = (e6(1)*qshift(1)-e6(2)*qshift(2)-e6(3)*qshift(3)-e6(4)*qshift(4))
      brack = (qshift(iv1)*abb94n29+spvak1e6(iv1)*abb94n26+t1*abb94n25+t2*abb94n&
      &28+t3*abb94n27+t4*abb94n24+t5*abb94n23+spvak1k6(iv1)*t6*abb94n17+t1*abb94&
      &n17*(spvak1k6(1)*qshift(1)-spvak1k6(2)*qshift(2)-spvak1k6(3)*qshift(3)-sp&
      &vak1k6(4)*qshift(4))+t1*abb94n19*(spvak5k2(1)*qshift(1)-spvak5k2(2)*qshif&
      &t(2)-spvak5k2(3)*qshift(3)-spvak5k2(4)*qshift(4))+t1*abb94n20*(spvak5k4(1&
      &)*qshift(1)-spvak5k4(2)*qshift(2)-spvak5k4(3)*qshift(3)-spvak5k4(4)*qshif&
      &t(4))+t1*abb94n21*dotproduct(k1, qshift)+t1*abb94n22*dotproduct(k6, qshif&
      &t)+t2*t6*abb94n21+t3*t6*abb94n22+t4*t6*abb94n19+t5*t6*abb94n20)
   end  function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p0_dbaru_hepneg_model
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_color
      use p0_dbaru_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: t1
      complex(ki) :: t2
      t1 = e6(iv1)
      t2 = e6(iv2)
      brack = ((2.0_ki)*d(iv1,iv2)*abb94n18+k1(iv1)*t2*abb94n21+k1(iv2)*t1*abb94&
      &n21+k6(iv1)*t2*abb94n22+k6(iv2)*t1*abb94n22+spvak1k6(iv1)*t2*abb94n17+spv&
      &ak1k6(iv2)*t1*abb94n17+spvak5k2(iv1)*t2*abb94n19+spvak5k2(iv2)*t1*abb94n1&
      &9+spvak5k4(iv1)*t2*abb94n20+spvak5k4(iv2)*t1*abb94n20)
   end  function brack_3
!---#] function brack_3:

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
      integer :: t1
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
!---#[ subroutine reconstruct_d94:
   subroutine     reconstruct_d94(coeffs)
      use p0_dbaru_hepneg_groups, only: tensrec_info_group3
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
end module p0_dbaru_hepneg_d94h0l1d
