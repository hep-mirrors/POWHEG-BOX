module     p16_bbarb_hepemg_d43h0l1
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p16_bbarb_hepemg/helicity0d43h0l1.f90
   ! generator: buildfortran.py
   use p16_bbarb_hepemg_config, only: ki
   use p16_bbarb_hepemg_util, only: cond
   implicit none
   private
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   public :: numerator_samurai
   public :: numerator_golem95
contains
!---#[ function brack_1:
   pure function brack_1(Q,mu2) result(brack)
      use p16_bbarb_hepemg_model
      use p16_bbarb_hepemg_kinematics
      use p16_bbarb_hepemg_color
      use p16_bbarb_hepemg_abbrevd43h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc43(92)
      complex(ki) :: Qspk6
      complex(ki) :: Qspe6
      complex(ki) :: Qspvak5l3
      complex(ki) :: Qspvae6k6
      complex(ki) :: Qspvak5k2
      complex(ki) :: Qspvak5e6
      complex(ki) :: Qspvae6k4
      complex(ki) :: Qspval3k4
      complex(ki) :: Qspvak6e6
      complex(ki) :: Qspval3e6
      complex(ki) :: Qspvae6k2
      complex(ki) :: Qspvak1e6
      complex(ki) :: Qspvak6k4
      complex(ki) :: Qspvak5k6
      complex(ki) :: Qspvae6l3
      complex(ki) :: Qspvak2e6
      complex(ki) :: Qspvae6k1
      complex(ki) :: Qspvak2k4
      complex(ki) :: Qspvak1k2
      complex(ki) :: Qspvak1k4
      complex(ki) :: Qspvak5k4
      complex(ki) :: Qspk2
      complex(ki) :: Qspvak1l3
      complex(ki) :: Qspval3k2
      complex(ki) :: Qspvak6k2
      complex(ki) :: Qspvak1k6
      complex(ki) :: Qspk1
      complex(ki) :: Qspl3
      complex(ki) :: QspQ
      Qspk6 = dotproduct(Q,k6)
      Qspe6 = dotproduct(Q,e6)
      Qspvak5l3 = dotproduct(Q,spvak5l3)
      Qspvae6k6 = dotproduct(Q,spvae6k6)
      Qspvak5k2 = dotproduct(Q,spvak5k2)
      Qspvak5e6 = dotproduct(Q,spvak5e6)
      Qspvae6k4 = dotproduct(Q,spvae6k4)
      Qspval3k4 = dotproduct(Q,spval3k4)
      Qspvak6e6 = dotproduct(Q,spvak6e6)
      Qspval3e6 = dotproduct(Q,spval3e6)
      Qspvae6k2 = dotproduct(Q,spvae6k2)
      Qspvak1e6 = dotproduct(Q,spvak1e6)
      Qspvak6k4 = dotproduct(Q,spvak6k4)
      Qspvak5k6 = dotproduct(Q,spvak5k6)
      Qspvae6l3 = dotproduct(Q,spvae6l3)
      Qspvak2e6 = dotproduct(Q,spvak2e6)
      Qspvae6k1 = dotproduct(Q,spvae6k1)
      Qspvak2k4 = dotproduct(Q,spvak2k4)
      Qspvak1k2 = dotproduct(Q,spvak1k2)
      Qspvak1k4 = dotproduct(Q,spvak1k4)
      Qspvak5k4 = dotproduct(Q,spvak5k4)
      Qspk2 = dotproduct(Q,k2)
      Qspvak1l3 = dotproduct(Q,spvak1l3)
      Qspval3k2 = dotproduct(Q,spval3k2)
      Qspvak6k2 = dotproduct(Q,spvak6k2)
      Qspvak1k6 = dotproduct(Q,spvak1k6)
      Qspk1 = dotproduct(Q,k1)
      Qspl3 = dotproduct(Q,l3)
      QspQ = dotproduct(Q,Q)
      acc43(1)=abb43(6)
      acc43(2)=abb43(7)
      acc43(3)=abb43(8)
      acc43(4)=abb43(9)
      acc43(5)=abb43(10)
      acc43(6)=abb43(11)
      acc43(7)=abb43(12)
      acc43(8)=abb43(13)
      acc43(9)=abb43(14)
      acc43(10)=abb43(15)
      acc43(11)=abb43(16)
      acc43(12)=abb43(17)
      acc43(13)=abb43(18)
      acc43(14)=abb43(19)
      acc43(15)=abb43(20)
      acc43(16)=abb43(21)
      acc43(17)=abb43(22)
      acc43(18)=abb43(23)
      acc43(19)=abb43(24)
      acc43(20)=abb43(25)
      acc43(21)=abb43(26)
      acc43(22)=abb43(27)
      acc43(23)=abb43(28)
      acc43(24)=abb43(29)
      acc43(25)=abb43(30)
      acc43(26)=abb43(31)
      acc43(27)=abb43(32)
      acc43(28)=abb43(33)
      acc43(29)=abb43(34)
      acc43(30)=abb43(35)
      acc43(31)=abb43(36)
      acc43(32)=abb43(37)
      acc43(33)=abb43(38)
      acc43(34)=abb43(39)
      acc43(35)=abb43(40)
      acc43(36)=abb43(41)
      acc43(37)=abb43(42)
      acc43(38)=abb43(43)
      acc43(39)=abb43(44)
      acc43(40)=abb43(45)
      acc43(41)=abb43(47)
      acc43(42)=abb43(48)
      acc43(43)=abb43(53)
      acc43(44)=abb43(57)
      acc43(45)=abb43(60)
      acc43(46)=abb43(61)
      acc43(47)=abb43(62)
      acc43(48)=abb43(63)
      acc43(49)=abb43(65)
      acc43(50)=abb43(67)
      acc43(51)=abb43(68)
      acc43(52)=abb43(69)
      acc43(53)=abb43(70)
      acc43(54)=abb43(71)
      acc43(55)=abb43(72)
      acc43(56)=abb43(77)
      acc43(57)=abb43(78)
      acc43(58)=abb43(79)
      acc43(59)=abb43(80)
      acc43(60)=abb43(81)
      acc43(61)=abb43(84)
      acc43(62)=abb43(85)
      acc43(63)=abb43(87)
      acc43(64)=abb43(88)
      acc43(65)=abb43(89)
      acc43(66)=abb43(90)
      acc43(67)=acc43(6)*Qspk6
      acc43(68)=acc43(10)*Qspe6
      acc43(69)=acc43(12)*Qspvak5l3
      acc43(70)=acc43(13)*Qspvae6k6
      acc43(71)=acc43(14)*Qspvak5k2
      acc43(72)=acc43(15)*Qspvak5e6
      acc43(73)=acc43(17)*Qspvae6k4
      acc43(74)=acc43(18)*Qspval3k4
      acc43(75)=acc43(19)*Qspvak6e6
      acc43(76)=acc43(20)*Qspval3e6
      acc43(77)=acc43(21)*Qspvae6k2
      acc43(78)=acc43(28)*Qspvak1e6
      acc43(79)=acc43(33)*Qspvak6k4
      acc43(80)=acc43(35)*Qspvak5k6
      acc43(81)=acc43(41)*Qspvae6l3
      acc43(82)=Qspvak2e6*acc43(24)
      acc43(83)=Qspvae6k1*acc43(5)
      acc43(84)=Qspvak2k4*acc43(39)
      acc43(67)=acc43(84)+acc43(83)+acc43(82)+acc43(81)+acc43(80)+acc43(79)+acc&
      &43(78)+acc43(77)+acc43(76)+acc43(75)+acc43(74)+acc43(73)+acc43(72)+acc43&
      &(71)+acc43(70)+acc43(69)+acc43(68)+acc43(67)+acc43(1)
      acc43(67)=Qspvak1k2*acc43(67)
      acc43(68)=acc43(32)*Qspvak5k2
      acc43(69)=acc43(53)*Qspe6
      acc43(70)=acc43(54)*Qspvak1e6
      acc43(71)=-acc43(57)*Qspvak1k4
      acc43(72)=acc43(58)*Qspvak5e6
      acc43(73)=acc43(60)*Qspvae6k4
      acc43(74)=acc43(62)*Qspvae6k2
      acc43(75)=acc43(65)*Qspvak5k4
      acc43(68)=acc43(75)+acc43(74)+acc43(73)+acc43(72)+acc43(71)+acc43(70)+acc&
      &43(69)+acc43(51)+acc43(68)
      acc43(68)=Qspk2*acc43(68)
      acc43(69)=acc43(31)*Qspvak1l3
      acc43(70)=acc43(36)*Qspvae6k2
      acc43(71)=acc43(43)*Qspval3k2
      acc43(72)=acc43(66)*Qspvak1e6
      acc43(73)=Qspvak6k2*acc43(63)
      acc43(74)=Qspvak1k6*acc43(55)
      acc43(69)=acc43(74)+acc43(73)+acc43(72)+acc43(64)+acc43(71)+acc43(70)+acc&
      &43(69)
      acc43(69)=Qspvak5k4*acc43(69)
      acc43(70)=acc43(42)*Qspvae6k4
      acc43(71)=-acc43(45)*Qspvak5e6
      acc43(72)=-acc43(46)*Qspvak1e6
      acc43(73)=acc43(52)*Qspvae6k2
      acc43(70)=acc43(73)+acc43(72)+acc43(71)+acc43(70)
      acc43(71)=Qspk1-Qspl3
      acc43(70)=acc43(71)*acc43(70)
      acc43(71)=acc43(9)*Qspvak1k4
      acc43(72)=acc43(23)*Qspvak5k2
      acc43(73)=acc43(40)*Qspk1
      acc43(71)=acc43(73)+acc43(72)+acc43(16)+acc43(71)
      acc43(71)=Qspe6*acc43(71)
      acc43(72)=acc43(25)*Qspvak5k2
      acc43(73)=-acc43(50)*Qspvak1k4
      acc43(72)=acc43(73)+acc43(72)
      acc43(73)=Qspk6+Qspl3
      acc43(72)=acc43(73)*acc43(72)
      acc43(73)=acc43(2)*Qspvak1e6
      acc43(74)=acc43(3)*Qspk6
      acc43(75)=acc43(4)*Qspvak1k4
      acc43(76)=acc43(8)*Qspvae6k2
      acc43(77)=acc43(11)*Qspvak6k4
      acc43(78)=acc43(22)*Qspvak5k2
      acc43(79)=acc43(26)*Qspvak1l3
      acc43(80)=acc43(27)*Qspvak5e6
      acc43(81)=acc43(29)*Qspval3k4
      acc43(82)=acc43(30)*Qspval3k2
      acc43(83)=acc43(34)*Qspvak5l3
      acc43(84)=acc43(37)*Qspl3
      acc43(85)=acc43(38)*Qspvak5k6
      acc43(86)=acc43(47)*Qspk1
      acc43(87)=acc43(48)*Qspvae6k6
      acc43(88)=acc43(49)*Qspvae6l3
      acc43(89)=acc43(56)*Qspvak6e6
      acc43(90)=acc43(59)*Qspvae6k4
      acc43(91)=acc43(61)*Qspval3e6
      acc43(92)=QspQ*acc43(44)
      brack=acc43(7)+acc43(67)+acc43(68)+acc43(69)+acc43(70)+acc43(71)+acc43(72&
      &)+acc43(73)+acc43(74)+acc43(75)+acc43(76)+acc43(77)+acc43(78)+acc43(79)+&
      &acc43(80)+acc43(81)+acc43(82)+acc43(83)+acc43(84)+acc43(85)+acc43(86)+ac&
      &c43(87)+acc43(88)+acc43(89)+acc43(90)+acc43(91)+acc43(92)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p16_bbarb_hepemg_groups, only: &
!           & sign => diagram43_sign, shift => diagram43_shift
      use p16_bbarb_hepemg_globalsl1, only: epspow
      use p16_bbarb_hepemg_kinematics
      use p16_bbarb_hepemg_abbrevd43h0
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d43
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      Q(1)  =cmplx(real(-Q_ext(4),  ki_sam),aimag(-Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(-Q_ext(1:3),ki_sam),aimag(-Q_ext(1:3)),ki)
      d43 = 0.0_ki
      d43 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d43, ki), aimag(d43), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p16_bbarb_hepemg_globalsl1, only: epspow
      use p16_bbarb_hepemg_kinematics
      use p16_bbarb_hepemg_abbrevd43h0
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d43
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      Q(:)  =cmplx(real(-Q_ext(:),  ki_gol), 0.0_ki_gol, ki)
      d43 = 0.0_ki
      d43 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d43, ki), aimag(d43), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p16_bbarb_hepemg_d43h0l1
