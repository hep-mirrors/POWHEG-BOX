module     p12_sbars_hepemg_d55h0l1
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p12_sbars_hepemg/helicity0d55h0l1.f90
   ! generator: buildfortran.py
   use p12_sbars_hepemg_config, only: ki
   use p12_sbars_hepemg_util, only: cond
   implicit none
   private
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   public :: numerator_samurai
   public :: numerator_golem95
contains
!---#[ function brack_1:
   pure function brack_1(Q,mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd55h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc55(65)
      complex(ki) :: Qspk1
      complex(ki) :: Qspk2
      complex(ki) :: Qspvak1k6
      complex(ki) :: Qspvak6k2
      complex(ki) :: Qspvak1k4
      complex(ki) :: Qspvak5k2
      complex(ki) :: Qspk6
      complex(ki) :: Qspvak1k2
      complex(ki) :: QspQ
      complex(ki) :: Qspe6
      complex(ki) :: Qspvak1e6
      complex(ki) :: Qspvae6k2
      complex(ki) :: Qspvae6k4
      complex(ki) :: Qspvak5e6
      complex(ki) :: Qspvak5k4
      complex(ki) :: Qspvak5k6
      complex(ki) :: Qspvak6k4
      complex(ki) :: Qspl3
      Qspk1 = dotproduct(Q,k1)
      Qspk2 = dotproduct(Q,k2)
      Qspvak1k6 = dotproduct(Q,spvak1k6)
      Qspvak6k2 = dotproduct(Q,spvak6k2)
      Qspvak1k4 = dotproduct(Q,spvak1k4)
      Qspvak5k2 = dotproduct(Q,spvak5k2)
      Qspk6 = dotproduct(Q,k6)
      Qspvak1k2 = dotproduct(Q,spvak1k2)
      QspQ = dotproduct(Q,Q)
      Qspe6 = dotproduct(Q,e6)
      Qspvak1e6 = dotproduct(Q,spvak1e6)
      Qspvae6k2 = dotproduct(Q,spvae6k2)
      Qspvae6k4 = dotproduct(Q,spvae6k4)
      Qspvak5e6 = dotproduct(Q,spvak5e6)
      Qspvak5k4 = dotproduct(Q,spvak5k4)
      Qspvak5k6 = dotproduct(Q,spvak5k6)
      Qspvak6k4 = dotproduct(Q,spvak6k4)
      Qspl3 = dotproduct(Q,l3)
      acc55(1)=abb55(6)
      acc55(2)=abb55(7)
      acc55(3)=abb55(8)
      acc55(4)=abb55(9)
      acc55(5)=abb55(10)
      acc55(6)=abb55(11)
      acc55(7)=abb55(12)
      acc55(8)=abb55(13)
      acc55(9)=abb55(14)
      acc55(10)=abb55(15)
      acc55(11)=abb55(16)
      acc55(12)=abb55(17)
      acc55(13)=abb55(18)
      acc55(14)=abb55(19)
      acc55(15)=abb55(20)
      acc55(16)=abb55(21)
      acc55(17)=abb55(22)
      acc55(18)=abb55(23)
      acc55(19)=abb55(24)
      acc55(20)=abb55(25)
      acc55(21)=abb55(26)
      acc55(22)=abb55(27)
      acc55(23)=abb55(28)
      acc55(24)=abb55(29)
      acc55(25)=abb55(30)
      acc55(26)=abb55(31)
      acc55(27)=abb55(32)
      acc55(28)=abb55(33)
      acc55(29)=abb55(35)
      acc55(30)=abb55(36)
      acc55(31)=abb55(37)
      acc55(32)=abb55(39)
      acc55(33)=abb55(40)
      acc55(34)=abb55(42)
      acc55(35)=abb55(45)
      acc55(36)=abb55(46)
      acc55(37)=abb55(47)
      acc55(38)=abb55(50)
      acc55(39)=abb55(53)
      acc55(40)=abb55(58)
      acc55(41)=abb55(60)
      acc55(42)=abb55(61)
      acc55(43)=abb55(62)
      acc55(44)=abb55(63)
      acc55(45)=abb55(65)
      acc55(46)=abb55(66)
      acc55(47)=abb55(67)
      acc55(48)=Qspk1*acc55(39)
      acc55(49)=Qspk2*acc55(40)
      acc55(50)=Qspvak1k6*acc55(33)
      acc55(51)=Qspvak6k2*acc55(46)
      acc55(52)=Qspvak1k4*acc55(2)
      acc55(53)=Qspvak5k2*acc55(30)
      acc55(54)=Qspk6*acc55(35)
      acc55(55)=Qspvak1k2*acc55(9)
      acc55(56)=QspQ*acc55(20)
      acc55(48)=acc55(56)+acc55(55)+acc55(54)+acc55(53)+acc55(52)+acc55(51)+acc&
      &55(50)+acc55(49)+acc55(18)+acc55(48)
      acc55(48)=Qspe6*acc55(48)
      acc55(49)=Qspvak1e6*acc55(43)
      acc55(50)=Qspvae6k2*acc55(42)
      acc55(51)=Qspvak1k4*acc55(17)
      acc55(52)=Qspvak5k2*acc55(36)
      acc55(53)=Qspvae6k4*acc55(28)
      acc55(54)=Qspvak5e6*acc55(29)
      acc55(55)=Qspvak1k2*acc55(11)
      acc55(49)=acc55(55)+acc55(54)+acc55(53)+acc55(52)+acc55(51)+acc55(50)+acc&
      &55(3)+acc55(49)
      acc55(49)=QspQ*acc55(49)
      acc55(50)=Qspvak1k6*acc55(27)
      acc55(51)=Qspvak6k2*acc55(45)
      acc55(52)=Qspvak1e6*acc55(31)
      acc55(53)=-Qspvae6k2*acc55(26)
      acc55(50)=acc55(53)+acc55(52)+acc55(51)+acc55(32)+acc55(50)
      acc55(50)=Qspvak5k4*acc55(50)
      acc55(51)=Qspvak1k4*acc55(6)
      acc55(52)=Qspvak5k2*acc55(1)
      acc55(53)=Qspvae6k4*acc55(37)
      acc55(54)=Qspvak5e6*acc55(34)
      acc55(51)=acc55(54)+acc55(53)+acc55(52)+acc55(8)+acc55(51)
      acc55(51)=Qspk6*acc55(51)
      acc55(52)=Qspvak5k6*acc55(24)
      acc55(53)=Qspvak6k4*acc55(22)
      acc55(54)=Qspvae6k4*acc55(25)
      acc55(55)=Qspvak5e6*acc55(13)
      acc55(52)=acc55(55)+acc55(54)+acc55(53)+acc55(4)+acc55(52)
      acc55(52)=Qspvak1k2*acc55(52)
      acc55(53)=acc55(38)*Qspl3
      acc55(54)=Qspk1*acc55(14)
      acc55(55)=Qspk2*acc55(12)
      acc55(56)=Qspvak5k6*acc55(47)
      acc55(57)=Qspvak6k4*acc55(44)
      acc55(58)=Qspvak1k6*acc55(5)
      acc55(59)=Qspvak6k2*acc55(10)
      acc55(60)=Qspvak1e6*acc55(21)
      acc55(61)=Qspvae6k2*acc55(41)
      acc55(62)=Qspvak1k4*acc55(16)
      acc55(63)=Qspvak5k2*acc55(19)
      acc55(64)=Qspvae6k4*acc55(23)
      acc55(65)=Qspvak5e6*acc55(7)
      brack=acc55(15)+acc55(48)+acc55(49)+acc55(50)+acc55(51)+acc55(52)+acc55(5&
      &3)+acc55(54)+acc55(55)+acc55(56)+acc55(57)+acc55(58)+acc55(59)+acc55(60)&
      &+acc55(61)+acc55(62)+acc55(63)+acc55(64)+acc55(65)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p12_sbars_hepemg_groups, only: &
!           & sign => diagram55_sign, shift => diagram55_shift
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd55h0
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d55
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      Q(1)  =cmplx(real(+Q_ext(4),  ki_sam),aimag(+Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(+Q_ext(1:3),ki_sam),aimag(+Q_ext(1:3)),ki)
      d55 = 0.0_ki
      d55 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d55, ki), aimag(d55), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd55h0
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d55
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      Q(:)  =cmplx(real(+Q_ext(:),  ki_gol), 0.0_ki_gol, ki)
      d55 = 0.0_ki
      d55 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d55, ki), aimag(d55), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p12_sbars_hepemg_d55h0l1
