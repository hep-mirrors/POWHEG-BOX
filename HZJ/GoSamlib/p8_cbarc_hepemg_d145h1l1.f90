module     p8_cbarc_hepemg_d145h1l1
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p8_cbarc_hepemg/helicity1d145h1l1.f90
   ! generator: buildfortran.py
   use p8_cbarc_hepemg_config, only: ki
   use p8_cbarc_hepemg_util, only: cond
   implicit none
   private
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   public :: numerator_samurai
   public :: numerator_golem95
contains
!---#[ function brack_1:
   pure function brack_1(Q,mu2) result(brack)
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_color
      use p8_cbarc_hepemg_abbrevd145h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc145(58)
      complex(ki) :: Qspvak2e6
      complex(ki) :: Qspvak6k4
      complex(ki) :: Qspvae6k4
      complex(ki) :: Qspe6
      complex(ki) :: Qspvae6k1
      complex(ki) :: Qspk6
      complex(ki) :: QspQ
      complex(ki) :: Qspvak5e6
      complex(ki) :: Qspvak5k6
      complex(ki) :: Qspvae6k2
      complex(ki) :: Qspvak1e6
      complex(ki) :: Qspvak2k1
      complex(ki) :: Qspvak2k4
      complex(ki) :: Qspvak5k1
      complex(ki) :: Qspvak6k1
      complex(ki) :: Qspvak2k6
      complex(ki) :: Qspvak5k4
      complex(ki) :: Qspk1
      complex(ki) :: Qspk2
      Qspvak2e6 = dotproduct(Q,spvak2e6)
      Qspvak6k4 = dotproduct(Q,spvak6k4)
      Qspvae6k4 = dotproduct(Q,spvae6k4)
      Qspe6 = dotproduct(Q,e6)
      Qspvae6k1 = dotproduct(Q,spvae6k1)
      Qspk6 = dotproduct(Q,k6)
      QspQ = dotproduct(Q,Q)
      Qspvak5e6 = dotproduct(Q,spvak5e6)
      Qspvak5k6 = dotproduct(Q,spvak5k6)
      Qspvae6k2 = dotproduct(Q,spvae6k2)
      Qspvak1e6 = dotproduct(Q,spvak1e6)
      Qspvak2k1 = dotproduct(Q,spvak2k1)
      Qspvak2k4 = dotproduct(Q,spvak2k4)
      Qspvak5k1 = dotproduct(Q,spvak5k1)
      Qspvak6k1 = dotproduct(Q,spvak6k1)
      Qspvak2k6 = dotproduct(Q,spvak2k6)
      Qspvak5k4 = dotproduct(Q,spvak5k4)
      Qspk1 = dotproduct(Q,k1)
      Qspk2 = dotproduct(Q,k2)
      acc145(1)=abb145(12)
      acc145(2)=abb145(14)
      acc145(3)=abb145(15)
      acc145(4)=abb145(16)
      acc145(5)=abb145(17)
      acc145(6)=abb145(18)
      acc145(7)=abb145(19)
      acc145(8)=abb145(20)
      acc145(9)=abb145(21)
      acc145(10)=abb145(22)
      acc145(11)=abb145(23)
      acc145(12)=abb145(24)
      acc145(13)=abb145(25)
      acc145(14)=abb145(26)
      acc145(15)=abb145(27)
      acc145(16)=abb145(28)
      acc145(17)=abb145(29)
      acc145(18)=abb145(30)
      acc145(19)=abb145(31)
      acc145(20)=abb145(32)
      acc145(21)=abb145(33)
      acc145(22)=abb145(34)
      acc145(23)=abb145(35)
      acc145(24)=abb145(36)
      acc145(25)=abb145(37)
      acc145(26)=abb145(38)
      acc145(27)=abb145(39)
      acc145(28)=abb145(42)
      acc145(29)=abb145(43)
      acc145(30)=abb145(44)
      acc145(31)=abb145(45)
      acc145(32)=abb145(46)
      acc145(33)=abb145(48)
      acc145(34)=abb145(49)
      acc145(35)=abb145(50)
      acc145(36)=abb145(52)
      acc145(37)=abb145(53)
      acc145(38)=abb145(54)
      acc145(39)=abb145(55)
      acc145(40)=abb145(56)
      acc145(41)=abb145(59)
      acc145(42)=abb145(60)
      acc145(43)=abb145(61)
      acc145(44)=acc145(1)*Qspvak2e6
      acc145(45)=acc145(5)*Qspvak6k4
      acc145(46)=acc145(6)*Qspvae6k4
      acc145(47)=acc145(15)*Qspe6
      acc145(48)=acc145(17)*Qspvae6k1
      acc145(49)=acc145(20)*Qspk6
      acc145(50)=acc145(21)*QspQ
      acc145(51)=acc145(23)*Qspvak5e6
      acc145(52)=acc145(26)*Qspvak5k6
      acc145(53)=Qspvae6k2*acc145(4)
      acc145(54)=Qspvak1e6*acc145(7)
      acc145(44)=acc145(54)+acc145(53)+acc145(52)+acc145(51)+acc145(50)+acc145(&
      &49)+acc145(48)+acc145(47)+acc145(46)+acc145(45)+acc145(3)+acc145(44)
      acc145(44)=Qspvak2k1*acc145(44)
      acc145(45)=acc145(8)*Qspvak2k4
      acc145(46)=acc145(10)*Qspvak5k1
      acc145(47)=acc145(24)*QspQ
      acc145(48)=acc145(36)*Qspvak6k1
      acc145(49)=acc145(41)*Qspvak2k6
      acc145(45)=acc145(49)+acc145(48)+acc145(47)+acc145(46)+acc145(9)+acc145(4&
      &5)
      acc145(45)=Qspe6*acc145(45)
      acc145(46)=acc145(2)*Qspvae6k4
      acc145(47)=acc145(11)*Qspvak5k1
      acc145(48)=acc145(30)*Qspvak5e6
      acc145(49)=acc145(43)*Qspvak2k4
      acc145(46)=acc145(49)+acc145(48)+acc145(47)+acc145(46)
      acc145(46)=QspQ*acc145(46)
      acc145(47)=acc145(14)*Qspvak2e6
      acc145(48)=acc145(34)*Qspvak6k1
      acc145(49)=acc145(39)*Qspvae6k1
      acc145(50)=acc145(40)*Qspvak2k6
      acc145(47)=acc145(50)+acc145(49)+acc145(38)+acc145(48)+acc145(47)
      acc145(47)=Qspvak5k4*acc145(47)
      acc145(48)=-acc145(13)*Qspvak5e6
      acc145(49)=-acc145(28)*Qspe6
      acc145(50)=acc145(31)*Qspvae6k4
      acc145(48)=acc145(49)+acc145(50)+acc145(48)
      acc145(49)=Qspk1+Qspk2
      acc145(48)=acc145(49)*acc145(48)
      acc145(50)=acc145(18)*Qspvae6k1
      acc145(51)=-acc145(22)*Qspvak2e6
      acc145(50)=acc145(50)+acc145(51)
      acc145(49)=acc145(49)+Qspk6+QspQ
      acc145(49)=acc145(49)*acc145(50)
      acc145(50)=acc145(37)*Qspvak5k1
      acc145(51)=acc145(42)*Qspvak2k4
      acc145(50)=acc145(51)+acc145(50)+acc145(19)
      acc145(50)=Qspk6*acc145(50)
      acc145(51)=acc145(12)*Qspvae6k1
      acc145(52)=acc145(16)*Qspk1
      acc145(53)=acc145(25)*Qspvak5k1
      acc145(54)=acc145(27)*Qspk2
      acc145(55)=acc145(29)*Qspvak5k6
      acc145(56)=acc145(32)*Qspvak2k4
      acc145(57)=acc145(33)*Qspvak2e6
      acc145(58)=acc145(35)*Qspvak6k4
      brack=acc145(44)+acc145(45)+acc145(46)+acc145(47)+acc145(48)+acc145(49)+a&
      &cc145(50)+acc145(51)+acc145(52)+acc145(53)+acc145(54)+acc145(55)+acc145(&
      &56)+acc145(57)+acc145(58)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p8_cbarc_hepemg_groups, only: &
!           & sign => diagram145_sign, shift => diagram145_shift
      use p8_cbarc_hepemg_globalsl1, only: epspow
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_abbrevd145h1
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d145
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      Q(1)  =cmplx(real(-Q_ext(4),  ki_sam),aimag(-Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(-Q_ext(1:3),ki_sam),aimag(-Q_ext(1:3)),ki)
      d145 = 0.0_ki
      d145 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d145, ki), aimag(d145), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p8_cbarc_hepemg_globalsl1, only: epspow
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_abbrevd145h1
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d145
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      Q(:)  =cmplx(real(-Q_ext(:),  ki_gol), 0.0_ki_gol, ki)
      d145 = 0.0_ki
      d145 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d145, ki), aimag(d145), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p8_cbarc_hepemg_d145h1l1
