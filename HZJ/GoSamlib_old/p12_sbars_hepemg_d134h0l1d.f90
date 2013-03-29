module     p12_sbars_hepemg_d134h0l1d
   ! file: /home/oleari/fortran/POWHEG-BOX-trunk/HZJ/GoSam_POWHEG/Virtual/p12_s &
   ! &bars_hepemg/helicity0d134h0l1d.f90
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
   public :: derivative , reconstruct_d134
contains
!---#[ function brack_1:
   pure function brack_1(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd134h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(28) :: acd134
      complex(ki) :: brack
      acd134(1)=dotproduct(k1,qshift)
      acd134(2)=abb134(17)
      acd134(3)=dotproduct(k2,qshift)
      acd134(4)=dotproduct(qshift,spvak5k4)
      acd134(5)=abb134(13)
      acd134(6)=abb134(15)
      acd134(7)=dotproduct(k6,qshift)
      acd134(8)=dotproduct(e6,qshift)
      acd134(9)=abb134(7)
      acd134(10)=abb134(6)
      acd134(11)=dotproduct(qshift,qshift)
      acd134(12)=abb134(18)
      acd134(13)=dotproduct(qshift,spvak1k6)
      acd134(14)=abb134(11)
      acd134(15)=dotproduct(qshift,spvae6k2)
      acd134(16)=abb134(16)
      acd134(17)=abb134(14)
      acd134(18)=dotproduct(qshift,spvak1k4)
      acd134(19)=abb134(8)
      acd134(20)=abb134(10)
      acd134(21)=abb134(5)
      acd134(22)=acd134(3)-acd134(7)
      acd134(23)=-acd134(5)*acd134(22)
      acd134(24)=acd134(13)*acd134(14)
      acd134(25)=acd134(15)*acd134(16)
      acd134(26)=acd134(8)*acd134(9)
      acd134(23)=acd134(26)+acd134(25)-acd134(17)+acd134(24)+acd134(23)
      acd134(23)=acd134(4)*acd134(23)
      acd134(22)=-acd134(6)*acd134(22)
      acd134(24)=-acd134(18)*acd134(19)
      acd134(25)=acd134(11)*acd134(12)
      acd134(26)=-acd134(1)*acd134(2)
      acd134(27)=-acd134(15)*acd134(20)
      acd134(28)=-acd134(8)*acd134(10)
      brack=acd134(21)+acd134(22)+acd134(23)+acd134(24)+acd134(25)+acd134(26)+a&
      &cd134(27)+acd134(28)
   end function brack_1
!---#] function brack_1:
!---#[ function brack_2:
   pure function brack_2(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd134h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(34) :: acd134
      complex(ki) :: brack
      acd134(1)=k1(iv1)
      acd134(2)=abb134(17)
      acd134(3)=k2(iv1)
      acd134(4)=dotproduct(qshift,spvak5k4)
      acd134(5)=abb134(13)
      acd134(6)=abb134(15)
      acd134(7)=k6(iv1)
      acd134(8)=e6(iv1)
      acd134(9)=abb134(7)
      acd134(10)=abb134(6)
      acd134(11)=qshift(iv1)
      acd134(12)=abb134(18)
      acd134(13)=spvak5k4(iv1)
      acd134(14)=dotproduct(k2,qshift)
      acd134(15)=dotproduct(k6,qshift)
      acd134(16)=dotproduct(e6,qshift)
      acd134(17)=dotproduct(qshift,spvak1k6)
      acd134(18)=abb134(11)
      acd134(19)=dotproduct(qshift,spvae6k2)
      acd134(20)=abb134(16)
      acd134(21)=abb134(14)
      acd134(22)=spvak1k4(iv1)
      acd134(23)=abb134(8)
      acd134(24)=spvak1k6(iv1)
      acd134(25)=spvae6k2(iv1)
      acd134(26)=abb134(10)
      acd134(27)=-acd134(15)+acd134(14)
      acd134(27)=acd134(27)*acd134(5)
      acd134(28)=-acd134(16)*acd134(9)
      acd134(29)=-acd134(17)*acd134(18)
      acd134(30)=-acd134(19)*acd134(20)
      acd134(27)=acd134(27)+acd134(21)+acd134(30)+acd134(29)+acd134(28)
      acd134(27)=acd134(13)*acd134(27)
      acd134(28)=-acd134(9)*acd134(8)
      acd134(29)=-acd134(25)*acd134(20)
      acd134(30)=-acd134(24)*acd134(18)
      acd134(28)=acd134(30)+acd134(29)+acd134(28)
      acd134(28)=acd134(4)*acd134(28)
      acd134(29)=-acd134(7)+acd134(3)
      acd134(30)=acd134(5)*acd134(4)
      acd134(30)=acd134(30)+acd134(6)
      acd134(29)=acd134(30)*acd134(29)
      acd134(30)=acd134(2)*acd134(1)
      acd134(31)=acd134(10)*acd134(8)
      acd134(32)=acd134(12)*acd134(11)
      acd134(33)=acd134(23)*acd134(22)
      acd134(34)=acd134(26)*acd134(25)
      brack=acd134(27)+acd134(28)+acd134(29)+acd134(30)+acd134(31)-2.0_ki*acd13&
      &4(32)+acd134(33)+acd134(34)
   end function brack_2
!---#] function brack_2:
!---#[ function brack_3:
   pure function brack_3(Q, mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd134h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki), dimension(23) :: acd134
      complex(ki) :: brack
      acd134(1)=d(iv1,iv2)
      acd134(2)=abb134(18)
      acd134(3)=k2(iv1)
      acd134(4)=spvak5k4(iv2)
      acd134(5)=abb134(13)
      acd134(6)=k2(iv2)
      acd134(7)=spvak5k4(iv1)
      acd134(8)=k6(iv1)
      acd134(9)=k6(iv2)
      acd134(10)=e6(iv1)
      acd134(11)=abb134(7)
      acd134(12)=e6(iv2)
      acd134(13)=spvak1k6(iv2)
      acd134(14)=abb134(11)
      acd134(15)=spvae6k2(iv2)
      acd134(16)=abb134(16)
      acd134(17)=spvak1k6(iv1)
      acd134(18)=spvae6k2(iv1)
      acd134(19)=acd134(16)*acd134(15)
      acd134(20)=acd134(14)*acd134(13)
      acd134(21)=acd134(11)*acd134(12)
      acd134(22)=acd134(9)-acd134(6)
      acd134(22)=acd134(5)*acd134(22)
      acd134(19)=acd134(22)+acd134(21)+acd134(19)+acd134(20)
      acd134(19)=acd134(7)*acd134(19)
      acd134(20)=acd134(16)*acd134(18)
      acd134(21)=acd134(14)*acd134(17)
      acd134(22)=acd134(11)*acd134(10)
      acd134(23)=acd134(8)-acd134(3)
      acd134(23)=acd134(5)*acd134(23)
      acd134(20)=acd134(23)+acd134(22)+acd134(20)+acd134(21)
      acd134(20)=acd134(4)*acd134(20)
      acd134(21)=acd134(1)*acd134(2)
      brack=acd134(19)+acd134(20)+2.0_ki*acd134(21)
   end function brack_3
!---#] function brack_3:
!---#[ function derivative:
   function derivative(mu2,i1,i2) result(numerator)
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd134h0
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
!---#[ subroutine reconstruct_d134:
   subroutine     reconstruct_d134(coeffs)
      use p12_sbars_hepemg_groups, only: tensrec_info_group1
      implicit none
      complex(ki), parameter :: czip = (0.0_ki, 0.0_ki)
      complex(ki), parameter :: cone = (1.0_ki, 0.0_ki)
      type(tensrec_info_group1), intent(out) :: coeffs
      ! rank 2 case :
      !---[# reconstruct coeffs%coeffs_134:
      coeffs%coeffs_134%c0 = derivative(czip)
      coeffs%coeffs_134%c1(1,1) = derivative(czip,1)
      coeffs%coeffs_134%c1(1,2) = derivative(czip,1,1)/ 2.0_ki
      coeffs%coeffs_134%c1(2,1) = -derivative(czip,2)
      coeffs%coeffs_134%c1(2,2) = derivative(czip,2,2)/ 2.0_ki
      coeffs%coeffs_134%c1(3,1) = -derivative(czip,3)
      coeffs%coeffs_134%c1(3,2) = derivative(czip,3,3)/ 2.0_ki
      coeffs%coeffs_134%c1(4,1) = -derivative(czip,4)
      coeffs%coeffs_134%c1(4,2) = derivative(czip,4,4)/ 2.0_ki
      coeffs%coeffs_134%c2(1,1) = -derivative(czip,1,2)
      coeffs%coeffs_134%c2(2,1) = -derivative(czip,1,3)
      coeffs%coeffs_134%c2(3,1) = -derivative(czip,1,4)
      coeffs%coeffs_134%c2(4,1) = derivative(czip,2,3)
      coeffs%coeffs_134%c2(5,1) = derivative(czip,2,4)
      coeffs%coeffs_134%c2(6,1) = derivative(czip,3,4)
      !---#] reconstruct coeffs%coeffs_134:
   end subroutine reconstruct_d134
!---#] subroutine reconstruct_d134:
end module     p12_sbars_hepemg_d134h0l1d
