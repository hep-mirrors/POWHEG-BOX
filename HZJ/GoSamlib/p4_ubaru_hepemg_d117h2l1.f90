module     p4_ubaru_hepemg_d117h2l1
   ! file: /home/oleari/fortran/POWHEG-BOX-trunk/HZJ/GoSam_POWHEG/Virtual/p4_ub &
   ! &aru_hepemg/helicity2d117h2l1.f90
   ! generator: buildfortran.py
   use p4_ubaru_hepemg_config, only: ki
   use p4_ubaru_hepemg_util, only: cond
   implicit none
   private
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   public :: numerator_samurai
   public :: numerator_golem95
contains
!---#[ function brack_1:
   pure function brack_1(Q,mu2) result(brack)
      use p4_ubaru_hepemg_model
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_color
      use p4_ubaru_hepemg_abbrevd117h2
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc117(58)
      complex(ki) :: QspQ
      complex(ki) :: Qspvae6k5
      complex(ki) :: Qspvae6k2
      complex(ki) :: Qspvak4e6
      complex(ki) :: Qspk6
      complex(ki) :: Qspvak6k5
      complex(ki) :: Qspe6
      complex(ki) :: Qspvak4k6
      complex(ki) :: Qspvak1e6
      complex(ki) :: Qspvak2e6
      complex(ki) :: Qspvae6k1
      complex(ki) :: Qspvak1k2
      complex(ki) :: Qspvak4k2
      complex(ki) :: Qspvak1k5
      complex(ki) :: Qspvak1k6
      complex(ki) :: Qspvak6k2
      complex(ki) :: Qspvak4k5
      complex(ki) :: Qspk1
      complex(ki) :: Qspk2
      QspQ = dotproduct(Q,Q)
      Qspvae6k5 = dotproduct(Q,spvae6k5)
      Qspvae6k2 = dotproduct(Q,spvae6k2)
      Qspvak4e6 = dotproduct(Q,spvak4e6)
      Qspk6 = dotproduct(Q,k6)
      Qspvak6k5 = dotproduct(Q,spvak6k5)
      Qspe6 = dotproduct(Q,e6)
      Qspvak4k6 = dotproduct(Q,spvak4k6)
      Qspvak1e6 = dotproduct(Q,spvak1e6)
      Qspvak2e6 = dotproduct(Q,spvak2e6)
      Qspvae6k1 = dotproduct(Q,spvae6k1)
      Qspvak1k2 = dotproduct(Q,spvak1k2)
      Qspvak4k2 = dotproduct(Q,spvak4k2)
      Qspvak1k5 = dotproduct(Q,spvak1k5)
      Qspvak1k6 = dotproduct(Q,spvak1k6)
      Qspvak6k2 = dotproduct(Q,spvak6k2)
      Qspvak4k5 = dotproduct(Q,spvak4k5)
      Qspk1 = dotproduct(Q,k1)
      Qspk2 = dotproduct(Q,k2)
      acc117(1)=abb117(12)
      acc117(2)=abb117(13)
      acc117(3)=abb117(14)
      acc117(4)=abb117(16)
      acc117(5)=abb117(17)
      acc117(6)=abb117(18)
      acc117(7)=abb117(19)
      acc117(8)=abb117(20)
      acc117(9)=abb117(21)
      acc117(10)=abb117(22)
      acc117(11)=abb117(23)
      acc117(12)=abb117(24)
      acc117(13)=abb117(25)
      acc117(14)=abb117(26)
      acc117(15)=abb117(27)
      acc117(16)=abb117(28)
      acc117(17)=abb117(29)
      acc117(18)=abb117(30)
      acc117(19)=abb117(31)
      acc117(20)=abb117(32)
      acc117(21)=abb117(33)
      acc117(22)=abb117(34)
      acc117(23)=abb117(35)
      acc117(24)=abb117(36)
      acc117(25)=abb117(37)
      acc117(26)=abb117(38)
      acc117(27)=abb117(39)
      acc117(28)=abb117(40)
      acc117(29)=abb117(41)
      acc117(30)=abb117(42)
      acc117(31)=abb117(43)
      acc117(32)=abb117(44)
      acc117(33)=abb117(46)
      acc117(34)=abb117(48)
      acc117(35)=abb117(49)
      acc117(36)=abb117(50)
      acc117(37)=abb117(51)
      acc117(38)=abb117(52)
      acc117(39)=abb117(54)
      acc117(40)=abb117(55)
      acc117(41)=abb117(57)
      acc117(42)=abb117(61)
      acc117(43)=abb117(63)
      acc117(44)=acc117(3)*QspQ
      acc117(45)=acc117(4)*Qspvae6k5
      acc117(46)=acc117(6)*Qspvae6k2
      acc117(47)=acc117(8)*Qspvak4e6
      acc117(48)=acc117(18)*Qspk6
      acc117(49)=acc117(22)*Qspvak6k5
      acc117(50)=acc117(23)*Qspe6
      acc117(51)=acc117(27)*Qspvak4k6
      acc117(52)=acc117(29)*Qspvak1e6
      acc117(53)=Qspvak2e6*acc117(11)
      acc117(54)=Qspvae6k1*acc117(30)
      acc117(44)=acc117(54)+acc117(53)+acc117(52)+acc117(51)+acc117(50)+acc117(&
      &49)+acc117(20)+acc117(48)+acc117(47)+acc117(46)+acc117(45)+acc117(44)
      acc117(44)=Qspvak1k2*acc117(44)
      acc117(45)=acc117(2)*Qspvak4k2
      acc117(46)=acc117(14)*Qspvak1k5
      acc117(47)=acc117(16)*QspQ
      acc117(48)=acc117(25)*Qspvak1k6
      acc117(49)=acc117(31)*Qspvak6k2
      acc117(45)=acc117(49)+acc117(48)+acc117(47)+acc117(46)+acc117(9)+acc117(4&
      &5)
      acc117(45)=Qspe6*acc117(45)
      acc117(46)=acc117(12)*Qspvak1k5
      acc117(47)=acc117(33)*Qspvak4k2
      acc117(48)=acc117(34)*Qspvae6k5
      acc117(49)=acc117(38)*Qspvak4e6
      acc117(46)=acc117(49)+acc117(48)+acc117(47)+acc117(46)
      acc117(46)=QspQ*acc117(46)
      acc117(47)=acc117(13)*Qspvae6k2
      acc117(48)=acc117(15)*Qspvak1e6
      acc117(49)=acc117(17)*Qspvak6k2
      acc117(50)=acc117(37)*Qspvak1k6
      acc117(47)=acc117(50)+acc117(49)+acc117(48)+acc117(47)+acc117(7)
      acc117(47)=Qspvak4k5*acc117(47)
      acc117(48)=-acc117(26)*Qspe6
      acc117(49)=acc117(32)*Qspvae6k5
      acc117(50)=-acc117(35)*Qspvak4e6
      acc117(48)=acc117(48)+acc117(50)+acc117(49)
      acc117(49)=Qspk1+Qspk2
      acc117(48)=acc117(49)*acc117(48)
      acc117(50)=acc117(40)*Qspvae6k2
      acc117(51)=-acc117(41)*Qspvak1e6
      acc117(50)=acc117(50)+acc117(51)
      acc117(49)=acc117(49)+Qspk6+QspQ
      acc117(49)=acc117(49)*acc117(50)
      acc117(50)=acc117(10)*Qspvak1k5
      acc117(51)=acc117(19)*Qspvak4k2
      acc117(50)=acc117(51)+acc117(50)+acc117(5)
      acc117(50)=Qspk6*acc117(50)
      acc117(51)=acc117(1)*Qspvak4k2
      acc117(52)=acc117(21)*Qspk2
      acc117(53)=acc117(24)*Qspk1
      acc117(54)=acc117(28)*Qspvak1e6
      acc117(55)=acc117(36)*Qspvak1k5
      acc117(56)=acc117(39)*Qspvae6k2
      acc117(57)=acc117(42)*Qspvak6k5
      acc117(58)=acc117(43)*Qspvak4k6
      brack=acc117(44)+acc117(45)+acc117(46)+acc117(47)+acc117(48)+acc117(49)+a&
      &cc117(50)+acc117(51)+acc117(52)+acc117(53)+acc117(54)+acc117(55)+acc117(&
      &56)+acc117(57)+acc117(58)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p4_ubaru_hepemg_groups, only: &
!           & sign => diagram117_sign, shift => diagram117_shift
      use p4_ubaru_hepemg_globalsl1, only: epspow
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_abbrevd117h2
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d117
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      Q(1)  =cmplx(real(-Q_ext(4),  ki_sam),aimag(-Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(-Q_ext(1:3),ki_sam),aimag(-Q_ext(1:3)),ki)
      d117 = 0.0_ki
      d117 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d117, ki), aimag(d117), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p4_ubaru_hepemg_globalsl1, only: epspow
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_abbrevd117h2
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d117
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      Q(:)  =cmplx(real(-Q_ext(:),  ki_gol), 0.0_ki_gol, ki)
      d117 = 0.0_ki
      d117 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d117, ki), aimag(d117), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p4_ubaru_hepemg_d117h2l1
