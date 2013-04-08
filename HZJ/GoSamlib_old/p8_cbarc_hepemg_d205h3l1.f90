module     p8_cbarc_hepemg_d205h3l1
   ! file: /home/oleari/fortran/POWHEG-BOX-trunk/HZJ/GoSam_POWHEG/Virtual/p8_cb &
   ! &arc_hepemg/helicity3d205h3l1.f90
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
      use p8_cbarc_hepemg_abbrevd205h3
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc205(34)
      complex(ki) :: Qspvak6k1
      complex(ki) :: Qspvak2k6
      complex(ki) :: QspQ
      complex(ki) :: Qspvak2e6
      complex(ki) :: Qspk2
      complex(ki) :: Qspvak4k5
      complex(ki) :: Qspvak2k1
      complex(ki) :: Qspk1
      complex(ki) :: Qspvak2k5
      complex(ki) :: Qspvak4k1
      complex(ki) :: Qspe6
      complex(ki) :: Qspk6
      Qspvak6k1 = dotproduct(Q,spvak6k1)
      Qspvak2k6 = dotproduct(Q,spvak2k6)
      QspQ = dotproduct(Q,Q)
      Qspvak2e6 = dotproduct(Q,spvak2e6)
      Qspk2 = dotproduct(Q,k2)
      Qspvak4k5 = dotproduct(Q,spvak4k5)
      Qspvak2k1 = dotproduct(Q,spvak2k1)
      Qspk1 = dotproduct(Q,k1)
      Qspvak2k5 = dotproduct(Q,spvak2k5)
      Qspvak4k1 = dotproduct(Q,spvak4k1)
      Qspe6 = dotproduct(Q,e6)
      Qspk6 = dotproduct(Q,k6)
      acc205(1)=abb205(4)
      acc205(2)=abb205(5)
      acc205(3)=abb205(6)
      acc205(4)=abb205(7)
      acc205(5)=abb205(8)
      acc205(6)=abb205(9)
      acc205(7)=abb205(10)
      acc205(8)=abb205(11)
      acc205(9)=abb205(12)
      acc205(10)=abb205(13)
      acc205(11)=abb205(14)
      acc205(12)=abb205(15)
      acc205(13)=abb205(16)
      acc205(14)=abb205(17)
      acc205(15)=abb205(18)
      acc205(16)=abb205(19)
      acc205(17)=abb205(20)
      acc205(18)=abb205(21)
      acc205(19)=abb205(22)
      acc205(20)=abb205(23)
      acc205(21)=abb205(24)
      acc205(22)=abb205(25)
      acc205(23)=abb205(26)
      acc205(24)=abb205(27)
      acc205(25)=abb205(28)
      acc205(26)=abb205(29)
      acc205(27)=-acc205(4)*Qspvak6k1
      acc205(28)=Qspvak2k6*acc205(12)
      acc205(29)=QspQ*acc205(24)
      acc205(30)=Qspvak2e6*acc205(25)
      acc205(31)=Qspk2*acc205(22)
      acc205(27)=acc205(31)+acc205(30)+acc205(29)+acc205(28)+acc205(20)+acc205(&
      &27)
      acc205(27)=Qspvak4k5*acc205(27)
      acc205(28)=-Qspvak4k5*acc205(11)
      acc205(28)=acc205(28)+acc205(10)
      acc205(28)=Qspvak2k1*acc205(28)
      acc205(29)=Qspk2+Qspk1
      acc205(29)=acc205(2)*acc205(29)
      acc205(30)=Qspvak2k6*acc205(16)
      acc205(31)=Qspvak2k5*acc205(19)
      acc205(32)=Qspvak4k1*acc205(26)
      acc205(28)=acc205(32)+acc205(31)+acc205(30)+acc205(9)+acc205(29)+acc205(2&
      &8)
      acc205(28)=Qspe6*acc205(28)
      acc205(29)=Qspvak2k5*acc205(21)
      acc205(30)=Qspvak4k1*acc205(13)
      acc205(29)=acc205(29)-acc205(30)
      acc205(30)=acc205(14)+acc205(29)
      acc205(30)=Qspk6*acc205(30)
      acc205(31)=Qspvak2k5*acc205(23)
      acc205(32)=Qspvak4k1*acc205(3)
      acc205(31)=acc205(32)+acc205(17)+acc205(31)
      acc205(31)=QspQ*acc205(31)
      acc205(32)=-Qspk6*acc205(8)
      acc205(33)=QspQ*acc205(18)
      acc205(32)=acc205(33)+acc205(7)+acc205(32)
      acc205(32)=Qspvak2e6*acc205(32)
      acc205(33)=Qspvak2e6*acc205(8)
      acc205(29)=acc205(33)+acc205(15)-acc205(29)
      acc205(29)=Qspk2*acc205(29)
      acc205(33)=Qspk1*acc205(1)
      acc205(34)=Qspvak2k6*acc205(5)
      brack=acc205(6)+acc205(27)+acc205(28)+acc205(29)+acc205(30)+acc205(31)+ac&
      &c205(32)+acc205(33)+acc205(34)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p8_cbarc_hepemg_groups, only: &
!           & sign => diagram205_sign, shift => diagram205_shift
      use p8_cbarc_hepemg_globalsl1, only: epspow
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_abbrevd205h3
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d205
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(0:3) :: qshift
      qshift = -k2
      Q(1)  =cmplx(real(-Q_ext(4)  -qshift(0),  ki_sam),aimag(-Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(-Q_ext(1:3)-qshift(1:3),ki_sam),aimag(-Q_ext(1:3)),ki)
      d205 = 0.0_ki
      d205 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d205, ki), aimag(d205), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p8_cbarc_hepemg_globalsl1, only: epspow
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_abbrevd205h3
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d205
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(4) :: qshift
      qshift = -k2
      Q(:)  =cmplx(real(-Q_ext(:)  -qshift(:),  ki_gol), 0.0_ki_gol, ki)
      d205 = 0.0_ki
      d205 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d205, ki), aimag(d205), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p8_cbarc_hepemg_d205h3l1