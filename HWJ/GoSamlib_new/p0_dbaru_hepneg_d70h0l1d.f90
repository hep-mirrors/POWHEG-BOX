module     p0_dbaru_hepneg_d70h0l1d
   ! file:      /home/pcl305a/luisonig/Documents/GoSamPowheg/POWHEG-BOX/HWJ/GoSa
   ! m/Virtual/p0_dbaru_hepneg/helicity0/d70h0l1d.f90
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


   public :: derivative, reconstruct_d70
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
      brack = abb70n23
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
      brack = (e6(iv1)*abb70n32+k6(iv1)*abb70n38+spvae6k2(iv1)*abb70n31+spvae6k4&
      &(iv1)*abb70n34+spvae6k6(iv1)*abb70n33+spvae6l3(iv1)*abb70n30+spvak1e6(iv1&
      &)*abb70n42+spvak1k6(iv1)*abb70n40+spvak2e6(iv1)*abb70n41+spvak5e6(iv1)*ab&
      &b70n28+spvak5k6(iv1)*abb70n29+spvak6e6(iv1)*abb70n37+spvak6k2(iv1)*abb70n&
      &35+spvak6k4(iv1)*abb70n39+spval3e6(iv1)*abb70n36)
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
      brack = ((2.0_ki)*d(iv1,iv2)*abb70n25+spvak1k2(iv1)*t2*abb70n26+spvak1k2(i&
      &v2)*t1*abb70n26+spvak1k4(iv1)*t2*abb70n24+spvak1k4(iv2)*t1*abb70n24+spvak&
      &5k2(iv1)*t2*abb70n27+spvak5k2(iv2)*t1*abb70n27)
   end  function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q, mu2) result(brack)
      use p0_dbaru_hepneg_model
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_color
      use p0_dbaru_hepneg_abbrevh0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      brack = (0.0_ki,0.0_ki)
   end  function brack_4
!---#] function brack_4:

!---#[ function derivative:
   function derivative(mu2, i1, i2, i3) result(numerator)
      use p0_dbaru_hepneg_globalsl1, only: epspow
      use p0_dbaru_hepneg_kinematics
      use p0_dbaru_hepneg_abbrevh0
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
      if(present(i3)) then
         iv3 = i3
         deg = 3
      else
         iv3 = 1
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
      if(deg.eq.3) then
         numerator = (0.0_ki)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d70:
   subroutine     reconstruct_d70(coeffs)
      use p0_dbaru_hepneg_groups, only: tensrec_info_group0
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)

      type(tensrec_info_group0), intent(out) :: coeffs
      ! rank 3 case:
      !---[# reconstruct coeffs%coeffs_70:
      coeffs%coeffs_70%c0 = derivative(czip)
      coeffs%coeffs_70%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_70%c1(1,2) = derivative(czip,1,1) / 2.0_ki
      coeffs%coeffs_70%c1(1,3) = derivative(czip,1,1,1) / 6.0_ki
      coeffs%coeffs_70%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_70%c1(2,2) = derivative(czip,2,2) / 2.0_ki
      coeffs%coeffs_70%c1(2,3) = -derivative(czip,2,2,2) / 6.0_ki
      coeffs%coeffs_70%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_70%c1(3,2) = derivative(czip,3,3) / 2.0_ki
      coeffs%coeffs_70%c1(3,3) = -derivative(czip,3,3,3) / 6.0_ki
      coeffs%coeffs_70%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_70%c1(4,2) = derivative(czip,4,4) / 2.0_ki
      coeffs%coeffs_70%c1(4,3) = -derivative(czip,4,4,4) / 6.0_ki
      coeffs%coeffs_70%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_70%c2(1,2) = derivative(czip,1,2,2) / 2.0_ki
      coeffs%coeffs_70%c2(1,3) = -derivative(czip,1,1,2) / 2.0_ki
      coeffs%coeffs_70%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_70%c2(2,2) = derivative(czip,1,3,3) / 2.0_ki
      coeffs%coeffs_70%c2(2,3) = -derivative(czip,1,1,3) / 2.0_ki
      coeffs%coeffs_70%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_70%c2(3,2) = derivative(czip,1,4,4) / 2.0_ki
      coeffs%coeffs_70%c2(3,3) = -derivative(czip,1,1,4) / 2.0_ki
      coeffs%coeffs_70%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_70%c2(4,2) = -derivative(czip,2,3,3) / 2.0_ki
      coeffs%coeffs_70%c2(4,3) = -derivative(czip,2,2,3) / 2.0_ki
      coeffs%coeffs_70%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_70%c2(5,2) = -derivative(czip,2,4,4) / 2.0_ki
      coeffs%coeffs_70%c2(5,3) = -derivative(czip,2,2,4) / 2.0_ki
      coeffs%coeffs_70%c2(6,1) = derivative(czip,3,4)
      coeffs%coeffs_70%c2(6,2) = -derivative(czip,3,4,4) / 2.0_ki
      coeffs%coeffs_70%c2(6,3) = -derivative(czip,3,3,4) / 2.0_ki
      coeffs%coeffs_70%c3(1,1) = derivative(czip,1,2,3)
      coeffs%coeffs_70%c3(2,1) = derivative(czip,1,2,4)
      coeffs%coeffs_70%c3(3,1) = derivative(czip,1,3,4)
      coeffs%coeffs_70%c3(4,1) = -derivative(czip,2,3,4)
      !---#] reconstruct coeffs%coeffs_70:
   end subroutine reconstruct_d70
!---#] subroutine reconstruct_d70:
end module p0_dbaru_hepneg_d70h0l1d