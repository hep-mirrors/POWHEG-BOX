module     p16_bbarb_hepemg_d27h0l1
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p16_bbarb_hepemg/helicity0d27h0l1.f90
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
      use p16_bbarb_hepemg_abbrevd27h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc27(70)
      complex(ki) :: Qspvak1k2
      complex(ki) :: Qspvak1k4
      complex(ki) :: Qspe6
      complex(ki) :: Qspvae6k4
      complex(ki) :: Qspvak5e6
      complex(ki) :: Qspvae6k2
      complex(ki) :: Qspvak1e6
      complex(ki) :: Qspvak5k2
      complex(ki) :: QspQ
      complex(ki) :: Qspk2
      complex(ki) :: Qspk1
      complex(ki) :: Qspvak6k2
      complex(ki) :: Qspvak1k6
      complex(ki) :: Qspvak6k4
      complex(ki) :: Qspvak5k6
      complex(ki) :: Qspvak5k4
      complex(ki) :: Qspk6
      complex(ki) :: Qspvak5k1
      complex(ki) :: Qspvak2k4
      complex(ki) :: Qspl3
      Qspvak1k2 = dotproduct(Q,spvak1k2)
      Qspvak1k4 = dotproduct(Q,spvak1k4)
      Qspe6 = dotproduct(Q,e6)
      Qspvae6k4 = dotproduct(Q,spvae6k4)
      Qspvak5e6 = dotproduct(Q,spvak5e6)
      Qspvae6k2 = dotproduct(Q,spvae6k2)
      Qspvak1e6 = dotproduct(Q,spvak1e6)
      Qspvak5k2 = dotproduct(Q,spvak5k2)
      QspQ = dotproduct(Q,Q)
      Qspk2 = dotproduct(Q,k2)
      Qspk1 = dotproduct(Q,k1)
      Qspvak6k2 = dotproduct(Q,spvak6k2)
      Qspvak1k6 = dotproduct(Q,spvak1k6)
      Qspvak6k4 = dotproduct(Q,spvak6k4)
      Qspvak5k6 = dotproduct(Q,spvak5k6)
      Qspvak5k4 = dotproduct(Q,spvak5k4)
      Qspk6 = dotproduct(Q,k6)
      Qspvak5k1 = dotproduct(Q,spvak5k1)
      Qspvak2k4 = dotproduct(Q,spvak2k4)
      Qspl3 = dotproduct(Q,l3)
      acc27(1)=abb27(6)
      acc27(2)=abb27(7)
      acc27(3)=abb27(8)
      acc27(4)=abb27(9)
      acc27(5)=abb27(10)
      acc27(6)=abb27(11)
      acc27(7)=abb27(12)
      acc27(8)=abb27(13)
      acc27(9)=abb27(14)
      acc27(10)=abb27(15)
      acc27(11)=abb27(16)
      acc27(12)=abb27(17)
      acc27(13)=abb27(18)
      acc27(14)=abb27(19)
      acc27(15)=abb27(20)
      acc27(16)=abb27(23)
      acc27(17)=abb27(24)
      acc27(18)=abb27(25)
      acc27(19)=abb27(26)
      acc27(20)=abb27(28)
      acc27(21)=abb27(30)
      acc27(22)=abb27(32)
      acc27(23)=abb27(34)
      acc27(24)=abb27(35)
      acc27(25)=abb27(37)
      acc27(26)=abb27(38)
      acc27(27)=abb27(39)
      acc27(28)=abb27(40)
      acc27(29)=abb27(41)
      acc27(30)=abb27(43)
      acc27(31)=abb27(44)
      acc27(32)=abb27(48)
      acc27(33)=abb27(50)
      acc27(34)=abb27(51)
      acc27(35)=abb27(52)
      acc27(36)=abb27(55)
      acc27(37)=abb27(59)
      acc27(38)=abb27(63)
      acc27(39)=abb27(64)
      acc27(40)=abb27(65)
      acc27(41)=abb27(66)
      acc27(42)=abb27(67)
      acc27(43)=abb27(69)
      acc27(44)=abb27(71)
      acc27(45)=abb27(72)
      acc27(46)=abb27(74)
      acc27(47)=abb27(76)
      acc27(48)=abb27(77)
      acc27(49)=abb27(78)
      acc27(50)=abb27(79)
      acc27(51)=acc27(6)*Qspvak1k2
      acc27(52)=acc27(8)*Qspvak1k4
      acc27(53)=acc27(27)*Qspe6
      acc27(54)=acc27(37)*Qspvae6k4
      acc27(55)=acc27(38)*Qspvak5e6
      acc27(56)=acc27(39)*Qspvae6k2
      acc27(57)=acc27(41)*Qspvak1e6
      acc27(58)=acc27(48)*Qspvak5k2
      acc27(51)=acc27(58)+acc27(57)+acc27(56)+acc27(55)+acc27(54)+acc27(53)+acc&
      &27(22)+acc27(52)+acc27(51)
      acc27(51)=QspQ*acc27(51)
      acc27(52)=Qspk2-Qspk1
      acc27(52)=acc27(24)*acc27(52)
      acc27(53)=acc27(3)*Qspvak6k2
      acc27(54)=acc27(4)*Qspvak1k2
      acc27(55)=acc27(20)*Qspvak1k4
      acc27(56)=acc27(31)*Qspvak5k2
      acc27(57)=acc27(50)*Qspvak1k6
      acc27(52)=acc27(52)+acc27(57)+acc27(56)+acc27(55)+acc27(54)+acc27(53)+acc&
      &27(1)
      acc27(52)=Qspe6*acc27(52)
      acc27(53)=-acc27(29)*Qspvak1k4
      acc27(54)=acc27(30)*Qspvak5e6
      acc27(55)=acc27(33)*Qspvae6k4
      acc27(56)=-acc27(49)*Qspvak5k2
      acc27(53)=acc27(56)+acc27(55)+acc27(54)+acc27(53)
      acc27(54)=Qspk1+Qspk2
      acc27(53)=acc27(54)*acc27(53)
      acc27(55)=acc27(10)*Qspvak5e6
      acc27(56)=acc27(11)*Qspvae6k4
      acc27(57)=acc27(12)*Qspvak6k4
      acc27(58)=acc27(15)*Qspvak5k6
      acc27(55)=acc27(58)+acc27(57)+acc27(56)+acc27(55)+acc27(2)
      acc27(55)=Qspvak1k2*acc27(55)
      acc27(56)=acc27(13)*Qspvak6k2
      acc27(57)=acc27(19)*Qspvak1k6
      acc27(58)=acc27(45)*Qspvae6k2
      acc27(59)=acc27(46)*Qspvak1e6
      acc27(56)=acc27(59)+acc27(58)+acc27(57)+acc27(14)+acc27(56)
      acc27(56)=Qspvak5k4*acc27(56)
      acc27(57)=acc27(9)*Qspvak1k2
      acc27(58)=acc27(40)*Qspvae6k2
      acc27(59)=-acc27(42)*Qspvak1e6
      acc27(57)=acc27(59)+acc27(57)+acc27(58)
      acc27(54)=acc27(54)-Qspk6
      acc27(54)=acc27(54)*acc27(57)
      acc27(57)=acc27(25)*Qspvak1k4
      acc27(58)=-acc27(36)*Qspvak5k2
      acc27(57)=acc27(58)+acc27(32)+acc27(57)
      acc27(57)=Qspk6*acc27(57)
      acc27(58)=acc27(7)*Qspvak1e6
      acc27(59)=acc27(17)*Qspvak1k4
      acc27(60)=acc27(18)*Qspvae6k4
      acc27(61)=acc27(21)*Qspk1
      acc27(62)=acc27(23)*Qspk2
      acc27(63)=acc27(28)*Qspvak5e6
      acc27(64)=acc27(35)*Qspvae6k2
      acc27(65)=acc27(43)*Qspvak6k4
      acc27(66)=acc27(44)*Qspvak5k6
      acc27(67)=acc27(47)*Qspvak5k2
      acc27(68)=Qspvak5k1*acc27(26)
      acc27(69)=Qspvak2k4*acc27(16)
      acc27(70)=Qspl3*acc27(34)
      brack=acc27(5)+acc27(51)+acc27(52)+acc27(53)+acc27(54)+acc27(55)+acc27(56&
      &)+acc27(57)+acc27(58)+acc27(59)+acc27(60)+acc27(61)+acc27(62)+acc27(63)+&
      &acc27(64)+acc27(65)+acc27(66)+acc27(67)+acc27(68)+acc27(69)+acc27(70)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p16_bbarb_hepemg_groups, only: &
!           & sign => diagram27_sign, shift => diagram27_shift
      use p16_bbarb_hepemg_globalsl1, only: epspow
      use p16_bbarb_hepemg_kinematics
      use p16_bbarb_hepemg_abbrevd27h0
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d27
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(0:3) :: qshift
      qshift = k3
      Q(1)  =cmplx(real(+Q_ext(4)  -qshift(0),  ki_sam),aimag(+Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(+Q_ext(1:3)-qshift(1:3),ki_sam),aimag(+Q_ext(1:3)),ki)
      d27 = 0.0_ki
      d27 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d27, ki), aimag(d27), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p16_bbarb_hepemg_globalsl1, only: epspow
      use p16_bbarb_hepemg_kinematics
      use p16_bbarb_hepemg_abbrevd27h0
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d27
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(4) :: qshift
      qshift = k3
      Q(:)  =cmplx(real(+Q_ext(:)  -qshift(:),  ki_gol), 0.0_ki_gol, ki)
      d27 = 0.0_ki
      d27 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d27, ki), aimag(d27), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p16_bbarb_hepemg_d27h0l1
