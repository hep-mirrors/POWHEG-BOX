module     p8_cbarc_hepemg_d143h2l1
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p8_cbarc_hepemg/helicity2d143h2l1.f90
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
      use p8_cbarc_hepemg_abbrevd143h2
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc143(63)
      complex(ki) :: Qspvak1e6
      complex(ki) :: Qspvae6k2
      complex(ki) :: Qspe6
      complex(ki) :: Qspvak4k6
      complex(ki) :: Qspvak6k5
      complex(ki) :: Qspvak4e6
      complex(ki) :: Qspk6
      complex(ki) :: QspQ
      complex(ki) :: Qspvae6k5
      complex(ki) :: Qspvak2e6
      complex(ki) :: Qspvae6k1
      complex(ki) :: Qspvak1k2
      complex(ki) :: Qspvak1k5
      complex(ki) :: Qspvak4k2
      complex(ki) :: Qspvak6k2
      complex(ki) :: Qspvak1k6
      complex(ki) :: Qspvak4k5
      complex(ki) :: Qspk1
      complex(ki) :: Qspk2
      Qspvak1e6 = dotproduct(Q,spvak1e6)
      Qspvae6k2 = dotproduct(Q,spvae6k2)
      Qspe6 = dotproduct(Q,e6)
      Qspvak4k6 = dotproduct(Q,spvak4k6)
      Qspvak6k5 = dotproduct(Q,spvak6k5)
      Qspvak4e6 = dotproduct(Q,spvak4e6)
      Qspk6 = dotproduct(Q,k6)
      QspQ = dotproduct(Q,Q)
      Qspvae6k5 = dotproduct(Q,spvae6k5)
      Qspvak2e6 = dotproduct(Q,spvak2e6)
      Qspvae6k1 = dotproduct(Q,spvae6k1)
      Qspvak1k2 = dotproduct(Q,spvak1k2)
      Qspvak1k5 = dotproduct(Q,spvak1k5)
      Qspvak4k2 = dotproduct(Q,spvak4k2)
      Qspvak6k2 = dotproduct(Q,spvak6k2)
      Qspvak1k6 = dotproduct(Q,spvak1k6)
      Qspvak4k5 = dotproduct(Q,spvak4k5)
      Qspk1 = dotproduct(Q,k1)
      Qspk2 = dotproduct(Q,k2)
      acc143(1)=abb143(13)
      acc143(2)=abb143(15)
      acc143(3)=abb143(16)
      acc143(4)=abb143(17)
      acc143(5)=abb143(18)
      acc143(6)=abb143(19)
      acc143(7)=abb143(20)
      acc143(8)=abb143(21)
      acc143(9)=abb143(22)
      acc143(10)=abb143(23)
      acc143(11)=abb143(24)
      acc143(12)=abb143(25)
      acc143(13)=abb143(26)
      acc143(14)=abb143(27)
      acc143(15)=abb143(28)
      acc143(16)=abb143(29)
      acc143(17)=abb143(30)
      acc143(18)=abb143(31)
      acc143(19)=abb143(32)
      acc143(20)=abb143(33)
      acc143(21)=abb143(34)
      acc143(22)=abb143(35)
      acc143(23)=abb143(36)
      acc143(24)=abb143(37)
      acc143(25)=abb143(38)
      acc143(26)=abb143(39)
      acc143(27)=abb143(40)
      acc143(28)=abb143(42)
      acc143(29)=abb143(43)
      acc143(30)=abb143(44)
      acc143(31)=abb143(45)
      acc143(32)=abb143(46)
      acc143(33)=abb143(47)
      acc143(34)=abb143(48)
      acc143(35)=abb143(49)
      acc143(36)=abb143(50)
      acc143(37)=abb143(51)
      acc143(38)=abb143(52)
      acc143(39)=abb143(53)
      acc143(40)=abb143(54)
      acc143(41)=abb143(55)
      acc143(42)=abb143(57)
      acc143(43)=abb143(58)
      acc143(44)=abb143(59)
      acc143(45)=abb143(60)
      acc143(46)=abb143(62)
      acc143(47)=acc143(1)*Qspvak1e6
      acc143(48)=acc143(5)*Qspvae6k2
      acc143(49)=acc143(17)*Qspe6
      acc143(50)=acc143(18)*Qspvak4k6
      acc143(51)=acc143(19)*Qspvak6k5
      acc143(52)=acc143(21)*Qspvak4e6
      acc143(53)=acc143(22)*Qspk6
      acc143(54)=acc143(23)*QspQ
      acc143(55)=acc143(25)*Qspvae6k5
      acc143(56)=Qspvak2e6*acc143(29)
      acc143(57)=Qspvae6k1*acc143(31)
      acc143(47)=acc143(57)+acc143(56)+acc143(55)+acc143(54)+acc143(53)+acc143(&
      &52)+acc143(51)+acc143(50)+acc143(49)+acc143(13)+acc143(48)+acc143(47)
      acc143(47)=Qspvak1k2*acc143(47)
      acc143(48)=acc143(10)*Qspvak1k5
      acc143(49)=acc143(16)*Qspvak4k2
      acc143(50)=acc143(26)*QspQ
      acc143(51)=acc143(42)*Qspvak6k2
      acc143(52)=acc143(46)*Qspvak1k6
      acc143(48)=acc143(52)+acc143(51)+acc143(50)+acc143(49)+acc143(12)+acc143(&
      &48)
      acc143(48)=Qspe6*acc143(48)
      acc143(49)=acc143(2)*Qspvae6k5
      acc143(50)=acc143(11)*Qspvak4e6
      acc143(51)=acc143(27)*Qspvak4k2
      acc143(52)=acc143(28)*Qspvak1k5
      acc143(49)=acc143(52)+acc143(51)+acc143(50)+acc143(49)
      acc143(49)=QspQ*acc143(49)
      acc143(50)=acc143(38)*Qspvak1k6
      acc143(51)=acc143(43)*Qspvae6k2
      acc143(52)=acc143(44)*Qspvak6k2
      acc143(53)=acc143(45)*Qspvak1e6
      acc143(50)=acc143(53)+acc143(52)+acc143(51)+acc143(41)+acc143(50)
      acc143(50)=Qspvak4k5*acc143(50)
      acc143(51)=-acc143(30)*Qspe6
      acc143(52)=acc143(33)*Qspvae6k5
      acc143(53)=-acc143(36)*Qspvak4e6
      acc143(51)=acc143(51)+acc143(53)+acc143(52)
      acc143(52)=Qspk1+Qspk2
      acc143(51)=acc143(52)*acc143(51)
      acc143(53)=-acc143(20)*Qspvak1e6
      acc143(54)=acc143(24)*Qspvae6k2
      acc143(53)=acc143(53)+acc143(54)
      acc143(52)=acc143(52)+Qspk6+QspQ
      acc143(52)=acc143(52)*acc143(53)
      acc143(53)=acc143(8)*Qspvak1k5
      acc143(54)=acc143(14)*Qspvak4k2
      acc143(53)=acc143(54)+acc143(53)+acc143(4)
      acc143(53)=Qspk6*acc143(53)
      acc143(54)=acc143(3)*Qspvak4k2
      acc143(55)=acc143(6)*Qspk1
      acc143(56)=acc143(7)*Qspvak1k5
      acc143(57)=acc143(15)*Qspk2
      acc143(58)=acc143(32)*Qspvae6k5
      acc143(59)=acc143(34)*Qspvak6k5
      acc143(60)=acc143(35)*Qspvak4e6
      acc143(61)=acc143(37)*Qspvak4k6
      acc143(62)=acc143(39)*Qspvae6k2
      acc143(63)=acc143(40)*Qspvak1e6
      brack=acc143(9)+acc143(47)+acc143(48)+acc143(49)+acc143(50)+acc143(51)+ac&
      &c143(52)+acc143(53)+acc143(54)+acc143(55)+acc143(56)+acc143(57)+acc143(5&
      &8)+acc143(59)+acc143(60)+acc143(61)+acc143(62)+acc143(63)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p8_cbarc_hepemg_groups, only: &
!           & sign => diagram143_sign, shift => diagram143_shift
      use p8_cbarc_hepemg_globalsl1, only: epspow
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_abbrevd143h2
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d143
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(0:3) :: qshift
      qshift = k6+k5+k4
      Q(1)  =cmplx(real(-Q_ext(4)  -qshift(0),  ki_sam),aimag(-Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(-Q_ext(1:3)-qshift(1:3),ki_sam),aimag(-Q_ext(1:3)),ki)
      d143 = 0.0_ki
      d143 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d143, ki), aimag(d143), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p8_cbarc_hepemg_globalsl1, only: epspow
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_abbrevd143h2
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d143
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(4) :: qshift
      qshift = k6+k5+k4
      Q(:)  =cmplx(real(-Q_ext(:)  -qshift(:),  ki_gol), 0.0_ki_gol, ki)
      d143 = 0.0_ki
      d143 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d143, ki), aimag(d143), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p8_cbarc_hepemg_d143h2l1
