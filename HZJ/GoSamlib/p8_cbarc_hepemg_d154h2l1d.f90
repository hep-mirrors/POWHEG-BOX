module     p8_cbarc_hepemg_d154h2l1d
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p8_cbarc_hepemg/helicity2d154h2l1d.f90
   ! generator: buildfortran_d.py
   use p8_cbarc_hepemg_config, only: ki
   use p8_cbarc_hepemg_util, only: cond, d => metric_tensor
   implicit none
   private
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   integer, private :: iv0
   integer, private :: iv1
   integer, private :: iv2
   real(ki), dimension(4), private :: qshift
   public :: derivative , reconstruct_d154
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd154h2
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(28) :: acd154
      complex(ki) :: brack
      acd154(1)=dotproduct(k1,qshift)
      acd154(2)=abb154(17)
      acd154(3)=dotproduct(k2,qshift)
      acd154(4)=dotproduct(qshift,spvak4k5)
      acd154(5)=abb154(13)
      acd154(6)=abb154(15)
      acd154(7)=dotproduct(k6,qshift)
      acd154(8)=dotproduct(e6,qshift)
      acd154(9)=abb154(7)
      acd154(10)=abb154(6)
      acd154(11)=dotproduct(qshift,qshift)
      acd154(12)=abb154(18)
      acd154(13)=dotproduct(qshift,spvak1k6)
      acd154(14)=abb154(11)
      acd154(15)=dotproduct(qshift,spvae6k2)
      acd154(16)=abb154(16)
      acd154(17)=abb154(14)
      acd154(18)=dotproduct(qshift,spvak1k5)
      acd154(19)=abb154(8)
      acd154(20)=abb154(10)
      acd154(21)=abb154(5)
      acd154(22)=acd154(3)-acd154(7)
      acd154(23)=-acd154(5)*acd154(22)
      acd154(24)=acd154(13)*acd154(14)
      acd154(25)=acd154(15)*acd154(16)
      acd154(26)=acd154(8)*acd154(9)
      acd154(23)=acd154(26)+acd154(25)-acd154(17)+acd154(24)+acd154(23)
      acd154(23)=acd154(4)*acd154(23)
      acd154(22)=-acd154(6)*acd154(22)
      acd154(24)=-acd154(18)*acd154(19)
      acd154(25)=acd154(11)*acd154(12)
      acd154(26)=-acd154(1)*acd154(2)
      acd154(27)=-acd154(15)*acd154(20)
      acd154(28)=-acd154(8)*acd154(10)
      brack=acd154(21)+acd154(22)+acd154(23)+acd154(24)+acd154(25)+acd154(26)+a&
      &cd154(27)+acd154(28)
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd154h2
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(34) :: acd154
      complex(ki) :: brack
      acd154(1)=k1(iv1)
      acd154(2)=abb154(17)
      acd154(3)=k2(iv1)
      acd154(4)=dotproduct(qshift,spvak4k5)
      acd154(5)=abb154(13)
      acd154(6)=abb154(15)
      acd154(7)=k6(iv1)
      acd154(8)=e6(iv1)
      acd154(9)=abb154(7)
      acd154(10)=abb154(6)
      acd154(11)=qshift(iv1)
      acd154(12)=abb154(18)
      acd154(13)=spvak4k5(iv1)
      acd154(14)=dotproduct(k2,qshift)
      acd154(15)=dotproduct(k6,qshift)
      acd154(16)=dotproduct(e6,qshift)
      acd154(17)=dotproduct(qshift,spvak1k6)
      acd154(18)=abb154(11)
      acd154(19)=dotproduct(qshift,spvae6k2)
      acd154(20)=abb154(16)
      acd154(21)=abb154(14)
      acd154(22)=spvak1k5(iv1)
      acd154(23)=abb154(8)
      acd154(24)=spvak1k6(iv1)
      acd154(25)=spvae6k2(iv1)
      acd154(26)=abb154(10)
      acd154(27)=-acd154(20)*acd154(25)
      acd154(28)=-acd154(18)*acd154(24)
      acd154(29)=-acd154(8)*acd154(9)
      acd154(30)=acd154(3)-acd154(7)
      acd154(31)=acd154(5)*acd154(30)
      acd154(27)=acd154(31)+acd154(29)+acd154(27)+acd154(28)
      acd154(27)=acd154(4)*acd154(27)
      acd154(28)=-acd154(20)*acd154(19)
      acd154(29)=-acd154(18)*acd154(17)
      acd154(31)=-acd154(9)*acd154(16)
      acd154(32)=-acd154(15)+acd154(14)
      acd154(32)=acd154(5)*acd154(32)
      acd154(28)=acd154(32)+acd154(31)+acd154(29)+acd154(21)+acd154(28)
      acd154(28)=acd154(13)*acd154(28)
      acd154(29)=acd154(6)*acd154(30)
      acd154(30)=acd154(22)*acd154(23)
      acd154(31)=acd154(11)*acd154(12)
      acd154(32)=acd154(1)*acd154(2)
      acd154(33)=acd154(25)*acd154(26)
      acd154(34)=acd154(8)*acd154(10)
      brack=acd154(27)+acd154(28)+acd154(29)+acd154(30)-2.0_ki*acd154(31)+acd15&
      &4(32)+acd154(33)+acd154(34)
   end function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd154h2
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(23) :: acd154
      complex(ki) :: brack
      acd154(1)=d(iv1,iv2)
      acd154(2)=abb154(18)
      acd154(3)=k2(iv1)
      acd154(4)=spvak4k5(iv2)
      acd154(5)=abb154(13)
      acd154(6)=k2(iv2)
      acd154(7)=spvak4k5(iv1)
      acd154(8)=k6(iv1)
      acd154(9)=k6(iv2)
      acd154(10)=e6(iv1)
      acd154(11)=abb154(7)
      acd154(12)=e6(iv2)
      acd154(13)=spvak1k6(iv2)
      acd154(14)=abb154(11)
      acd154(15)=spvae6k2(iv2)
      acd154(16)=abb154(16)
      acd154(17)=spvak1k6(iv1)
      acd154(18)=spvae6k2(iv1)
      acd154(19)=acd154(16)*acd154(15)
      acd154(20)=acd154(14)*acd154(13)
      acd154(21)=acd154(11)*acd154(12)
      acd154(22)=acd154(9)-acd154(6)
      acd154(22)=acd154(5)*acd154(22)
      acd154(19)=acd154(22)+acd154(21)+acd154(19)+acd154(20)
      acd154(19)=acd154(7)*acd154(19)
      acd154(20)=acd154(16)*acd154(18)
      acd154(21)=acd154(14)*acd154(17)
      acd154(22)=acd154(11)*acd154(10)
      acd154(23)=acd154(8)-acd154(3)
      acd154(23)=acd154(5)*acd154(23)
      acd154(20)=acd154(23)+acd154(22)+acd154(20)+acd154(21)
      acd154(20)=acd154(4)*acd154(20)
      acd154(21)=acd154(1)*acd154(2)
      brack=acd154(19)+acd154(20)+2.0_ki*acd154(21)
   end function brack_3
!---#] function brack_3:
!---#[ function derivative:
   function derivative(mu2,i1,i2) result(numerator)
      use p8_cbarc_hepemg_globalsl1, only: epspow
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_abbrevd154h2
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
      qshift = k2
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
!---#[ subroutine reconstruct_d154:
   subroutine     reconstruct_d154(coeffs)
      use p8_cbarc_hepemg_groups, only: tensrec_info_group1
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group1), intent(out) :: coeffs
      ! rank 2 case :
      !---[# reconstruct coeffs%coeffs_154:
      coeffs%coeffs_154%c0 = derivative(czip)
      coeffs%coeffs_154%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_154%c1(1,2) = derivative(czip,1,1)/ 2.0_ki
      coeffs%coeffs_154%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_154%c1(2,2) = derivative(czip,2,2)/ 2.0_ki
      coeffs%coeffs_154%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_154%c1(3,2) = derivative(czip,3,3)/ 2.0_ki
      coeffs%coeffs_154%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_154%c1(4,2) = derivative(czip,4,4)/ 2.0_ki
      coeffs%coeffs_154%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_154%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_154%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_154%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_154%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_154%c2(6,1) = derivative(czip,3,4)
      !---#] reconstruct coeffs%coeffs_154:
   end subroutine reconstruct_d154
!---#] subroutine reconstruct_d154:
end module     p8_cbarc_hepemg_d154h2l1d
