module     p12_sbars_hepemg_d150h1l1d
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p12_sbars_hepemg/helicity1d150h1l1d.f90
   ! generator: buildfortran_d.py
   use p12_sbars_hepemg_config, only: ki
   use p12_sbars_hepemg_util, only: cond, d => metric_tensor
   implicit none
   private
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   integer, private :: iv2
   real(ki), dimension(4), private :: qshift
   public :: derivative , reconstruct_d150
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd150h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(28) :: acd150
      complex(ki) :: brack
      acd150(1)=dotproduct(k1,qshift)
      acd150(2)=abb150(8)
      acd150(3)=dotproduct(k6,qshift)
      acd150(4)=abb150(6)
      acd150(5)=dotproduct(qshift,qshift)
      acd150(6)=abb150(14)
      acd150(7)=dotproduct(qshift,spvak1k6)
      acd150(8)=abb150(5)
      acd150(9)=dotproduct(qshift,spvak2k1)
      acd150(10)=dotproduct(qshift,spvak5k4)
      acd150(11)=abb150(7)
      acd150(12)=abb150(9)
      acd150(13)=dotproduct(qshift,spvak2k6)
      acd150(14)=abb150(17)
      acd150(15)=dotproduct(qshift,spvak2k4)
      acd150(16)=abb150(18)
      acd150(17)=abb150(16)
      acd150(18)=dotproduct(qshift,spvak6k1)
      acd150(19)=abb150(15)
      acd150(20)=abb150(10)
      acd150(21)=acd150(10)*acd150(14)
      acd150(21)=acd150(21)-acd150(17)
      acd150(21)=acd150(13)*acd150(21)
      acd150(22)=-acd150(18)*acd150(19)
      acd150(23)=-acd150(15)*acd150(16)
      acd150(24)=-acd150(7)*acd150(8)
      acd150(25)=acd150(5)*acd150(6)
      acd150(26)=-acd150(3)*acd150(4)
      acd150(27)=-acd150(1)*acd150(2)
      acd150(28)=acd150(10)*acd150(11)
      acd150(28)=-acd150(12)+acd150(28)
      acd150(28)=acd150(9)*acd150(28)
      brack=acd150(20)+acd150(21)+acd150(22)+acd150(23)+acd150(24)+acd150(25)+a&
      &cd150(26)+acd150(27)+acd150(28)
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd150h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(31) :: acd150
      complex(ki) :: brack
      acd150(1)=k1(iv1)
      acd150(2)=abb150(8)
      acd150(3)=k6(iv1)
      acd150(4)=abb150(6)
      acd150(5)=qshift(iv1)
      acd150(6)=abb150(14)
      acd150(7)=spvak1k6(iv1)
      acd150(8)=abb150(5)
      acd150(9)=spvak2k1(iv1)
      acd150(10)=dotproduct(qshift,spvak5k4)
      acd150(11)=abb150(7)
      acd150(12)=abb150(9)
      acd150(13)=spvak5k4(iv1)
      acd150(14)=dotproduct(qshift,spvak2k1)
      acd150(15)=dotproduct(qshift,spvak2k6)
      acd150(16)=abb150(17)
      acd150(17)=spvak2k4(iv1)
      acd150(18)=abb150(18)
      acd150(19)=spvak2k6(iv1)
      acd150(20)=abb150(16)
      acd150(21)=spvak6k1(iv1)
      acd150(22)=abb150(15)
      acd150(23)=acd150(16)*acd150(15)
      acd150(24)=acd150(11)*acd150(14)
      acd150(23)=acd150(24)+acd150(23)
      acd150(23)=acd150(13)*acd150(23)
      acd150(24)=acd150(10)*acd150(16)
      acd150(24)=acd150(24)-acd150(20)
      acd150(24)=acd150(19)*acd150(24)
      acd150(25)=-acd150(21)*acd150(22)
      acd150(26)=-acd150(17)*acd150(18)
      acd150(27)=-acd150(7)*acd150(8)
      acd150(28)=acd150(5)*acd150(6)
      acd150(29)=-acd150(3)*acd150(4)
      acd150(30)=-acd150(1)*acd150(2)
      acd150(31)=acd150(10)*acd150(11)
      acd150(31)=-acd150(12)+acd150(31)
      acd150(31)=acd150(9)*acd150(31)
      brack=acd150(23)+acd150(24)+acd150(25)+acd150(26)+acd150(27)+2.0_ki*acd15&
      &0(28)+acd150(29)+acd150(30)+acd150(31)
   end function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd150h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(13) :: acd150
      complex(ki) :: brack
      acd150(1)=d(iv1,iv2)
      acd150(2)=abb150(14)
      acd150(3)=spvak2k1(iv1)
      acd150(4)=spvak5k4(iv2)
      acd150(5)=abb150(7)
      acd150(6)=spvak2k1(iv2)
      acd150(7)=spvak5k4(iv1)
      acd150(8)=spvak2k6(iv2)
      acd150(9)=abb150(17)
      acd150(10)=spvak2k6(iv1)
      acd150(11)=acd150(9)*acd150(8)
      acd150(12)=acd150(5)*acd150(6)
      acd150(11)=acd150(12)+acd150(11)
      acd150(11)=acd150(7)*acd150(11)
      acd150(12)=acd150(9)*acd150(10)
      acd150(13)=acd150(5)*acd150(3)
      acd150(12)=acd150(12)+acd150(13)
      acd150(12)=acd150(4)*acd150(12)
      acd150(13)=acd150(1)*acd150(2)
      brack=acd150(11)+acd150(12)+2.0_ki*acd150(13)
   end function brack_3
!---#] function brack_3:
!---#[ function derivative:
   function derivative(mu2,i1,i2) result(numerator)
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd150h1
      implicit none
      complex(ki), intent(in) :: mu2
      integer, intent(in), optional :: i1
      integer, intent(in), optional :: i2
      complex(ki) :: numerator
      complex(ki) :: loc
      integer :: t1
      integer :: deg
      complex(ki), dimension(4), parameter :: Q = (/ (0.0_ki,0.0_ki),(0.0_ki,0.&
      &0_ki),(0.0_ki,0.0_ki),(0.0_ki,0.0_ki)/)
      qshift = -k2
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
   end function derivative
!---#] function derivative:
!---#[ subroutine reconstruct_d150:
   subroutine     reconstruct_d150(coeffs)
      use p12_sbars_hepemg_groups, only: tensrec_info_group2
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group2), intent(out) :: coeffs
      ! rank 2 case :
      !---[# reconstruct coeffs%coeffs_150:
      coeffs%coeffs_150%c0 = derivative(czip)
      coeffs%coeffs_150%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_150%c1(1,2) = derivative(czip,1,1)/ 2.0_ki
      coeffs%coeffs_150%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_150%c1(2,2) = derivative(czip,2,2)/ 2.0_ki
      coeffs%coeffs_150%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_150%c1(3,2) = derivative(czip,3,3)/ 2.0_ki
      coeffs%coeffs_150%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_150%c1(4,2) = derivative(czip,4,4)/ 2.0_ki
      coeffs%coeffs_150%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_150%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_150%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_150%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_150%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_150%c2(6,1) = derivative(czip,3,4)
      !---#] reconstruct coeffs%coeffs_150:
   end subroutine reconstruct_d150
!---#] subroutine reconstruct_d150:
end module     p12_sbars_hepemg_d150h1l1d
