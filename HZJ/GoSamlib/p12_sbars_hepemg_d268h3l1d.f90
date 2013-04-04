module     p12_sbars_hepemg_d268h3l1d
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p12_sbars_hepemg/helicity3d268h3l1d.f90
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
   public :: derivative , reconstruct_d268
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd268h3
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(28) :: acd268
      complex(ki) :: brack
      acd268(1)=dotproduct(k1,qshift)
      acd268(2)=dotproduct(e6,qshift)
      acd268(3)=abb268(8)
      acd268(4)=abb268(5)
      acd268(5)=dotproduct(k6,qshift)
      acd268(6)=abb268(20)
      acd268(7)=abb268(9)
      acd268(8)=dotproduct(qshift,spvak2k5)
      acd268(9)=abb268(11)
      acd268(10)=dotproduct(qshift,spvak4k5)
      acd268(11)=abb268(15)
      acd268(12)=dotproduct(qshift,spvak6k1)
      acd268(13)=abb268(16)
      acd268(14)=abb268(10)
      acd268(15)=dotproduct(qshift,qshift)
      acd268(16)=abb268(7)
      acd268(17)=abb268(6)
      acd268(18)=abb268(19)
      acd268(19)=dotproduct(qshift,spvae6k1)
      acd268(20)=abb268(12)
      acd268(21)=abb268(14)
      acd268(22)=acd268(3)*acd268(1)
      acd268(23)=acd268(6)*acd268(5)
      acd268(24)=acd268(9)*acd268(8)
      acd268(25)=acd268(11)*acd268(10)
      acd268(26)=acd268(13)*acd268(12)
      acd268(22)=-acd268(14)+acd268(26)+acd268(25)+acd268(24)+acd268(23)+acd268&
      &(22)
      acd268(22)=acd268(2)*acd268(22)
      acd268(23)=-acd268(4)*acd268(1)
      acd268(24)=-acd268(7)*acd268(5)
      acd268(25)=acd268(16)*acd268(15)
      acd268(26)=-acd268(17)*acd268(8)
      acd268(27)=-acd268(18)*acd268(10)
      acd268(28)=-acd268(20)*acd268(19)
      brack=acd268(21)+acd268(22)+acd268(23)+acd268(24)+acd268(25)+acd268(26)+a&
      &cd268(27)+acd268(28)
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd268h3
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(34) :: acd268
      complex(ki) :: brack
      acd268(1)=k1(iv1)
      acd268(2)=dotproduct(e6,qshift)
      acd268(3)=abb268(8)
      acd268(4)=abb268(5)
      acd268(5)=k6(iv1)
      acd268(6)=abb268(20)
      acd268(7)=abb268(9)
      acd268(8)=e6(iv1)
      acd268(9)=dotproduct(k1,qshift)
      acd268(10)=dotproduct(k6,qshift)
      acd268(11)=dotproduct(qshift,spvak2k5)
      acd268(12)=abb268(11)
      acd268(13)=dotproduct(qshift,spvak4k5)
      acd268(14)=abb268(15)
      acd268(15)=dotproduct(qshift,spvak6k1)
      acd268(16)=abb268(16)
      acd268(17)=abb268(10)
      acd268(18)=qshift(iv1)
      acd268(19)=abb268(7)
      acd268(20)=spvak2k5(iv1)
      acd268(21)=abb268(6)
      acd268(22)=spvak4k5(iv1)
      acd268(23)=abb268(19)
      acd268(24)=spvak6k1(iv1)
      acd268(25)=spvae6k1(iv1)
      acd268(26)=abb268(12)
      acd268(27)=acd268(16)*acd268(24)
      acd268(28)=acd268(14)*acd268(22)
      acd268(29)=acd268(12)*acd268(20)
      acd268(30)=acd268(5)*acd268(6)
      acd268(31)=acd268(1)*acd268(3)
      acd268(27)=acd268(31)+acd268(30)+acd268(29)+acd268(27)+acd268(28)
      acd268(27)=acd268(2)*acd268(27)
      acd268(28)=acd268(16)*acd268(15)
      acd268(29)=acd268(14)*acd268(13)
      acd268(30)=acd268(12)*acd268(11)
      acd268(31)=acd268(6)*acd268(10)
      acd268(32)=acd268(3)*acd268(9)
      acd268(28)=acd268(32)+acd268(31)+acd268(30)+acd268(29)-acd268(17)+acd268(&
      &28)
      acd268(28)=acd268(8)*acd268(28)
      acd268(29)=-acd268(25)*acd268(26)
      acd268(30)=acd268(18)*acd268(19)
      acd268(31)=-acd268(22)*acd268(23)
      acd268(32)=-acd268(20)*acd268(21)
      acd268(33)=-acd268(5)*acd268(7)
      acd268(34)=-acd268(1)*acd268(4)
      brack=acd268(27)+acd268(28)+acd268(29)+2.0_ki*acd268(30)+acd268(31)+acd26&
      &8(32)+acd268(33)+acd268(34)
   end function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd268h3
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(25) :: acd268
      complex(ki) :: brack
      acd268(1)=d(iv1,iv2)
      acd268(2)=abb268(7)
      acd268(3)=k1(iv1)
      acd268(4)=e6(iv2)
      acd268(5)=abb268(8)
      acd268(6)=k1(iv2)
      acd268(7)=e6(iv1)
      acd268(8)=k6(iv1)
      acd268(9)=abb268(20)
      acd268(10)=k6(iv2)
      acd268(11)=spvak2k5(iv2)
      acd268(12)=abb268(11)
      acd268(13)=spvak4k5(iv2)
      acd268(14)=abb268(15)
      acd268(15)=spvak6k1(iv2)
      acd268(16)=abb268(16)
      acd268(17)=spvak2k5(iv1)
      acd268(18)=spvak4k5(iv1)
      acd268(19)=spvak6k1(iv1)
      acd268(20)=acd268(16)*acd268(15)
      acd268(21)=acd268(14)*acd268(13)
      acd268(22)=acd268(12)*acd268(11)
      acd268(23)=acd268(9)*acd268(10)
      acd268(24)=acd268(5)*acd268(6)
      acd268(20)=acd268(24)+acd268(23)+acd268(22)+acd268(20)+acd268(21)
      acd268(20)=acd268(7)*acd268(20)
      acd268(21)=acd268(16)*acd268(19)
      acd268(22)=acd268(14)*acd268(18)
      acd268(23)=acd268(12)*acd268(17)
      acd268(24)=acd268(9)*acd268(8)
      acd268(25)=acd268(5)*acd268(3)
      acd268(21)=acd268(25)+acd268(24)+acd268(23)+acd268(21)+acd268(22)
      acd268(21)=acd268(4)*acd268(21)
      acd268(22)=acd268(1)*acd268(2)
      brack=acd268(20)+acd268(21)+2.0_ki*acd268(22)
   end function brack_3
!---#] function brack_3:
!---#[ function derivative:
   function derivative(mu2,i1,i2) result(numerator)
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd268h3
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
      qshift = -k3-k6-k5-k4
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
!---#[ subroutine reconstruct_d268:
   subroutine     reconstruct_d268(coeffs)
      use p12_sbars_hepemg_groups, only: tensrec_info_group2
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group2), intent(out) :: coeffs
      ! rank 2 case :
      !---[# reconstruct coeffs%coeffs_268:
      coeffs%coeffs_268%c0 = derivative(czip)
      coeffs%coeffs_268%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_268%c1(1,2) = derivative(czip,1,1)/ 2.0_ki
      coeffs%coeffs_268%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_268%c1(2,2) = derivative(czip,2,2)/ 2.0_ki
      coeffs%coeffs_268%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_268%c1(3,2) = derivative(czip,3,3)/ 2.0_ki
      coeffs%coeffs_268%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_268%c1(4,2) = derivative(czip,4,4)/ 2.0_ki
      coeffs%coeffs_268%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_268%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_268%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_268%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_268%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_268%c2(6,1) = derivative(czip,3,4)
      !---#] reconstruct coeffs%coeffs_268:
   end subroutine reconstruct_d268
!---#] subroutine reconstruct_d268:
end module     p12_sbars_hepemg_d268h3l1d
