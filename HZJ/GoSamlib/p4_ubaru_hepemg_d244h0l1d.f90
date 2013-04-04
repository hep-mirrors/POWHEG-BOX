module     p4_ubaru_hepemg_d244h0l1d
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p4_ubaru_hepemg/helicity0d244h0l1d.f90
   ! generator: buildfortran_d.py
   use p4_ubaru_hepemg_config, only: ki
   use p4_ubaru_hepemg_util, only: cond, d => metric_tensor
   implicit none
   private
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   integer, private :: iv2
   integer, private :: iv3
   real(ki), dimension(4), private :: qshift
   public :: derivative , reconstruct_d244
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p4_ubaru_hepemg_model
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_color
      use p4_ubaru_hepemg_abbrevd244h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(1) :: acd244
      complex(ki) :: brack
      acd244(1)=abb244(8)
      brack=acd244(1)
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p4_ubaru_hepemg_model
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_color
      use p4_ubaru_hepemg_abbrevd244h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(45) :: acd244
      complex(ki) :: brack
      acd244(1)=k6(iv1)
      acd244(2)=abb244(20)
      acd244(3)=e6(iv1)
      acd244(4)=abb244(14)
      acd244(5)=spvak1k6(iv1)
      acd244(6)=abb244(16)
      acd244(7)=spvak5k6(iv1)
      acd244(8)=abb244(22)
      acd244(9)=spvak6k2(iv1)
      acd244(10)=abb244(18)
      acd244(11)=spvak6k4(iv1)
      acd244(12)=abb244(21)
      acd244(13)=spvak1e6(iv1)
      acd244(14)=abb244(15)
      acd244(15)=spvak2e6(iv1)
      acd244(16)=abb244(10)
      acd244(17)=spvae6k2(iv1)
      acd244(18)=abb244(9)
      acd244(19)=spval3e6(iv1)
      acd244(20)=abb244(27)
      acd244(21)=spvae6l3(iv1)
      acd244(22)=abb244(29)
      acd244(23)=spvae6k4(iv1)
      acd244(24)=abb244(25)
      acd244(25)=spvak5e6(iv1)
      acd244(26)=abb244(23)
      acd244(27)=spvak6e6(iv1)
      acd244(28)=abb244(31)
      acd244(29)=spvae6k6(iv1)
      acd244(30)=abb244(28)
      acd244(31)=-acd244(2)*acd244(1)
      acd244(32)=-acd244(4)*acd244(3)
      acd244(33)=-acd244(6)*acd244(5)
      acd244(34)=-acd244(8)*acd244(7)
      acd244(35)=-acd244(10)*acd244(9)
      acd244(36)=-acd244(12)*acd244(11)
      acd244(37)=-acd244(14)*acd244(13)
      acd244(38)=-acd244(16)*acd244(15)
      acd244(39)=-acd244(18)*acd244(17)
      acd244(40)=-acd244(20)*acd244(19)
      acd244(41)=-acd244(22)*acd244(21)
      acd244(42)=-acd244(24)*acd244(23)
      acd244(43)=-acd244(26)*acd244(25)
      acd244(44)=-acd244(28)*acd244(27)
      acd244(45)=-acd244(30)*acd244(29)
      brack=acd244(31)+acd244(32)+acd244(33)+acd244(34)+acd244(35)+acd244(36)+a&
      &cd244(37)+acd244(38)+acd244(39)+acd244(40)+acd244(41)+acd244(42)+acd244(&
      &43)+acd244(44)+acd244(45)
   end function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p4_ubaru_hepemg_model
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_color
      use p4_ubaru_hepemg_abbrevd244h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(17) :: acd244
      complex(ki) :: brack
      acd244(1)=d(iv1,iv2)
      acd244(2)=abb244(17)
      acd244(3)=e6(iv1)
      acd244(4)=spvak1k2(iv2)
      acd244(5)=abb244(12)
      acd244(6)=spvak1k4(iv2)
      acd244(7)=abb244(13)
      acd244(8)=spvak5k2(iv2)
      acd244(9)=abb244(11)
      acd244(10)=e6(iv2)
      acd244(11)=spvak1k2(iv1)
      acd244(12)=spvak1k4(iv1)
      acd244(13)=spvak5k2(iv1)
      acd244(14)=acd244(4)*acd244(5)
      acd244(15)=acd244(6)*acd244(7)
      acd244(16)=acd244(8)*acd244(9)
      acd244(14)=acd244(16)+acd244(15)+acd244(14)
      acd244(14)=acd244(3)*acd244(14)
      acd244(15)=acd244(11)*acd244(5)
      acd244(16)=acd244(12)*acd244(7)
      acd244(17)=acd244(13)*acd244(9)
      acd244(15)=acd244(17)+acd244(16)+acd244(15)
      acd244(15)=acd244(10)*acd244(15)
      acd244(16)=acd244(2)*acd244(1)
      brack=acd244(14)+acd244(15)+2.0_ki*acd244(16)
   end function brack_3
!---#] function brack_3:
!---#[ function brack_4:
   pure function brack_4(Q, mu2) result(brack)
      use p4_ubaru_hepemg_model
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_color
      use p4_ubaru_hepemg_abbrevd244h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(1) :: acd244
      complex(ki) :: brack
      brack=0.0_ki
   end function brack_4
!---#] function brack_4:
!---#[ function derivative:
   function derivative(mu2,i1,i2,i3) result(numerator)
      use p4_ubaru_hepemg_globalsl1, only: epspow
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_abbrevd244h0
      implicit none
      complex(ki), intent(in) :: mu2
      integer, intent(in), optional :: i1
      integer, intent(in), optional :: i2
      integer, intent(in), optional :: i3
      complex(ki) :: numerator
      complex(ki) :: loc
      integer :: t1
      integer :: deg
      complex(ki), dimension(4), parameter :: Q = (/ (0.0_ki,0.0_ki),(0.0_ki,0.&
      &0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki)/)
      qshift = 0
      numerator = 0.0_ki
      deg = 0
      if(present(i1)) then
          iv1=i1
          deg=1
      else
          iv1=1
      end if
      if(present(i2)) then
          iv2=i2
          deg=2
      else
          iv2=1
      end if
      if(present(i3)) then
          iv3=i3
          deg=3
      else
          iv3=1
      end if
      t1 = 0
      if(deg.eq.0) then
         numerator = cond(epspow.eq.t1,brack_1,Q,mu2)
         return
      end if
      if(deg.eq.1) then
         numerator = cond(epspow.eq.t1,brack_2,Q,mu2)
         return
      end if
      if(deg.eq.2) then
         numerator = cond(epspow.eq.t1,brack_3,Q,mu2)
         return
      end if
      if(deg.eq.3) then
         numerator = cond(epspow.eq.t1,brack_4,Q,mu2)
         return
      end if
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d244:
   subroutine     reconstruct_d244(coeffs)
      use p4_ubaru_hepemg_groups, only: tensrec_info_group3
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group3), intent(out) :: coeffs
      ! rank 3 case :
      !---[# reconstruct coeffs%coeffs_244:
      coeffs%coeffs_244%c0 = derivative(czip)
      coeffs%coeffs_244%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_244%c1(1,2) = derivative(czip,1,1)/ 2.0_ki
      coeffs%coeffs_244%c1(1,3) = derivative(czip,1,1,1)/ 6.0_ki
      coeffs%coeffs_244%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_244%c1(2,2) = derivative(czip,2,2)/ 2.0_ki
      coeffs%coeffs_244%c1(2,3) = -derivative(czip,2,2,2)/ 6.0_ki
      coeffs%coeffs_244%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_244%c1(3,2) = derivative(czip,3,3)/ 2.0_ki
      coeffs%coeffs_244%c1(3,3) = -derivative(czip,3,3,3)/ 6.0_ki
      coeffs%coeffs_244%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_244%c1(4,2) = derivative(czip,4,4)/ 2.0_ki
      coeffs%coeffs_244%c1(4,3) = -derivative(czip,4,4,4)/ 6.0_ki
      coeffs%coeffs_244%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_244%c2(1,2) = derivative(czip,1,2,2)/ 2.0_ki
      coeffs%coeffs_244%c2(1,3) = -derivative(czip,1,1,2)/ 2.0_ki
      coeffs%coeffs_244%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_244%c2(2,2) = derivative(czip,1,3,3)/ 2.0_ki
      coeffs%coeffs_244%c2(2,3) = -derivative(czip,1,1,3)/ 2.0_ki
      coeffs%coeffs_244%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_244%c2(3,2) = derivative(czip,1,4,4)/ 2.0_ki
      coeffs%coeffs_244%c2(3,3) = -derivative(czip,1,1,4)/ 2.0_ki
      coeffs%coeffs_244%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_244%c2(4,2) = -derivative(czip,2,3,3)/ 2.0_ki
      coeffs%coeffs_244%c2(4,3) = -derivative(czip,2,2,3)/ 2.0_ki
      coeffs%coeffs_244%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_244%c2(5,2) = -derivative(czip,2,4,4)/ 2.0_ki
      coeffs%coeffs_244%c2(5,3) = -derivative(czip,2,2,4)/ 2.0_ki
      coeffs%coeffs_244%c2(6,1) = derivative(czip,3,4)
      coeffs%coeffs_244%c2(6,2) = -derivative(czip,3,4,4)/ 2.0_ki
      coeffs%coeffs_244%c2(6,3) = -derivative(czip,3,3,4)/ 2.0_ki
      coeffs%coeffs_244%c3(1,1) = derivative(czip,1,2,3)
      coeffs%coeffs_244%c3(2,1) = derivative(czip,1,2,4)
      coeffs%coeffs_244%c3(3,1) = derivative(czip,1,3,4)
      coeffs%coeffs_244%c3(4,1) = -derivative(czip,2,3,4)
      !---#] reconstruct coeffs%coeffs_244:
   end subroutine reconstruct_d244
!---#] subroutine reconstruct_d244:
end module     p4_ubaru_hepemg_d244h0l1d
