module     p12_sbars_hepemg_d235h0l1d
   ! file: /home/oleari/fortran/POWHEG-BOX-trunk/HZJ/GoSam_POWHEG/Virtual/p12_s &
   ! &bars_hepemg/helicity0d235h0l1d.f90
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
   public :: derivative , reconstruct_d235
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd235h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(28) :: acd235
      complex(ki) :: brack
      acd235(1)=dotproduct(k1,qshift)
      acd235(2)=dotproduct(e6,qshift)
      acd235(3)=abb235(8)
      acd235(4)=abb235(5)
      acd235(5)=dotproduct(k6,qshift)
      acd235(6)=abb235(20)
      acd235(7)=abb235(9)
      acd235(8)=dotproduct(qshift,spvak1k6)
      acd235(9)=abb235(17)
      acd235(10)=dotproduct(qshift,spvak5k2)
      acd235(11)=abb235(11)
      acd235(12)=dotproduct(qshift,spvak5k4)
      acd235(13)=abb235(15)
      acd235(14)=abb235(10)
      acd235(15)=dotproduct(qshift,qshift)
      acd235(16)=abb235(7)
      acd235(17)=abb235(6)
      acd235(18)=abb235(16)
      acd235(19)=dotproduct(qshift,spvak1e6)
      acd235(20)=abb235(12)
      acd235(21)=abb235(14)
      acd235(22)=acd235(8)*acd235(9)
      acd235(23)=acd235(12)*acd235(13)
      acd235(24)=acd235(10)*acd235(11)
      acd235(25)=acd235(5)*acd235(6)
      acd235(26)=acd235(1)*acd235(3)
      acd235(22)=acd235(26)+acd235(25)+acd235(24)+acd235(23)-acd235(14)+acd235(&
      &22)
      acd235(22)=acd235(2)*acd235(22)
      acd235(23)=-acd235(19)*acd235(20)
      acd235(24)=acd235(15)*acd235(16)
      acd235(25)=-acd235(12)*acd235(18)
      acd235(26)=-acd235(10)*acd235(17)
      acd235(27)=-acd235(5)*acd235(7)
      acd235(28)=-acd235(1)*acd235(4)
      brack=acd235(21)+acd235(22)+acd235(23)+acd235(24)+acd235(25)+acd235(26)+a&
      &cd235(27)+acd235(28)
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd235h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(34) :: acd235
      complex(ki) :: brack
      acd235(1)=k1(iv1)
      acd235(2)=dotproduct(e6,qshift)
      acd235(3)=abb235(8)
      acd235(4)=abb235(5)
      acd235(5)=k6(iv1)
      acd235(6)=abb235(20)
      acd235(7)=abb235(9)
      acd235(8)=e6(iv1)
      acd235(9)=dotproduct(k1,qshift)
      acd235(10)=dotproduct(k6,qshift)
      acd235(11)=dotproduct(qshift,spvak1k6)
      acd235(12)=abb235(17)
      acd235(13)=dotproduct(qshift,spvak5k2)
      acd235(14)=abb235(11)
      acd235(15)=dotproduct(qshift,spvak5k4)
      acd235(16)=abb235(15)
      acd235(17)=abb235(10)
      acd235(18)=qshift(iv1)
      acd235(19)=abb235(7)
      acd235(20)=spvak1k6(iv1)
      acd235(21)=spvak5k2(iv1)
      acd235(22)=abb235(6)
      acd235(23)=spvak5k4(iv1)
      acd235(24)=abb235(16)
      acd235(25)=spvak1e6(iv1)
      acd235(26)=abb235(12)
      acd235(27)=acd235(3)*acd235(1)
      acd235(28)=acd235(6)*acd235(5)
      acd235(29)=acd235(21)*acd235(14)
      acd235(30)=acd235(23)*acd235(16)
      acd235(31)=acd235(20)*acd235(12)
      acd235(27)=acd235(31)+acd235(30)+acd235(29)+acd235(27)+acd235(28)
      acd235(27)=acd235(2)*acd235(27)
      acd235(28)=acd235(9)*acd235(3)
      acd235(29)=acd235(10)*acd235(6)
      acd235(30)=acd235(11)*acd235(12)
      acd235(31)=acd235(13)*acd235(14)
      acd235(32)=acd235(15)*acd235(16)
      acd235(28)=-acd235(17)+acd235(32)+acd235(31)+acd235(30)+acd235(29)+acd235&
      &(28)
      acd235(28)=acd235(8)*acd235(28)
      acd235(29)=-acd235(4)*acd235(1)
      acd235(30)=-acd235(7)*acd235(5)
      acd235(31)=acd235(19)*acd235(18)
      acd235(32)=-acd235(22)*acd235(21)
      acd235(33)=-acd235(24)*acd235(23)
      acd235(34)=-acd235(26)*acd235(25)
      brack=acd235(27)+acd235(28)+acd235(29)+acd235(30)+2.0_ki*acd235(31)+acd23&
      &5(32)+acd235(33)+acd235(34)
   end function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd235h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(25) :: acd235
      complex(ki) :: brack
      acd235(1)=d(iv1,iv2)
      acd235(2)=abb235(7)
      acd235(3)=k1(iv1)
      acd235(4)=e6(iv2)
      acd235(5)=abb235(8)
      acd235(6)=k1(iv2)
      acd235(7)=e6(iv1)
      acd235(8)=k6(iv1)
      acd235(9)=abb235(20)
      acd235(10)=k6(iv2)
      acd235(11)=spvak1k6(iv2)
      acd235(12)=abb235(17)
      acd235(13)=spvak5k2(iv2)
      acd235(14)=abb235(11)
      acd235(15)=spvak5k4(iv2)
      acd235(16)=abb235(15)
      acd235(17)=spvak1k6(iv1)
      acd235(18)=spvak5k2(iv1)
      acd235(19)=spvak5k4(iv1)
      acd235(20)=acd235(3)*acd235(5)
      acd235(21)=acd235(8)*acd235(9)
      acd235(22)=acd235(17)*acd235(12)
      acd235(23)=acd235(18)*acd235(14)
      acd235(24)=acd235(19)*acd235(16)
      acd235(20)=acd235(24)+acd235(23)+acd235(22)+acd235(21)+acd235(20)
      acd235(20)=acd235(4)*acd235(20)
      acd235(21)=acd235(6)*acd235(5)
      acd235(22)=acd235(10)*acd235(9)
      acd235(23)=acd235(11)*acd235(12)
      acd235(24)=acd235(13)*acd235(14)
      acd235(25)=acd235(15)*acd235(16)
      acd235(21)=acd235(25)+acd235(24)+acd235(23)+acd235(22)+acd235(21)
      acd235(21)=acd235(7)*acd235(21)
      acd235(22)=acd235(2)*acd235(1)
      brack=acd235(20)+acd235(21)+2.0_ki*acd235(22)
   end function brack_3
!---#] function brack_3:
!---#[ function derivative:
   function derivative(mu2,i1,i2) result(numerator)
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd235h0
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
!---#[ subroutine reconstruct_d235:
   subroutine     reconstruct_d235(coeffs)
      use p12_sbars_hepemg_groups, only: tensrec_info_group2
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group2), intent(out) :: coeffs
      ! rank 2 case :
      !---[# reconstruct coeffs%coeffs_235:
      coeffs%coeffs_235%c0 = derivative(czip)
      coeffs%coeffs_235%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_235%c1(1,2) = derivative(czip,1,1)/ 2.0_ki
      coeffs%coeffs_235%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_235%c1(2,2) = derivative(czip,2,2)/ 2.0_ki
      coeffs%coeffs_235%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_235%c1(3,2) = derivative(czip,3,3)/ 2.0_ki
      coeffs%coeffs_235%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_235%c1(4,2) = derivative(czip,4,4)/ 2.0_ki
      coeffs%coeffs_235%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_235%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_235%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_235%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_235%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_235%c2(6,1) = derivative(czip,3,4)
      !---#] reconstruct coeffs%coeffs_235:
   end subroutine reconstruct_d235
!---#] subroutine reconstruct_d235:
end module     p12_sbars_hepemg_d235h0l1d
