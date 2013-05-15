module     p12_sbars_hepemg_d234h3l1
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p12_sbars_hepemg/helicity3d234h3l1.f90
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
      use p12_sbars_hepemg_abbrevd234h3
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc234(34)
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
      acc234(1)=abb234(4)
      acc234(2)=abb234(5)
      acc234(3)=abb234(6)
      acc234(4)=abb234(7)
      acc234(5)=abb234(8)
      acc234(6)=abb234(9)
      acc234(7)=abb234(10)
      acc234(8)=abb234(11)
      acc234(9)=abb234(12)
      acc234(10)=abb234(13)
      acc234(11)=abb234(14)
      acc234(12)=abb234(15)
      acc234(13)=abb234(16)
      acc234(14)=abb234(17)
      acc234(15)=abb234(18)
      acc234(16)=abb234(19)
      acc234(17)=abb234(20)
      acc234(18)=abb234(21)
      acc234(19)=abb234(22)
      acc234(20)=abb234(23)
      acc234(21)=abb234(24)
      acc234(22)=abb234(25)
      acc234(23)=abb234(26)
      acc234(24)=abb234(27)
      acc234(25)=abb234(28)
      acc234(26)=abb234(29)
      acc234(27)=-acc234(4)*Qspvak6k1
      acc234(28)=Qspvak2k6*acc234(12)
      acc234(29)=QspQ*acc234(24)
      acc234(30)=Qspvak2e6*acc234(25)
      acc234(31)=Qspk2*acc234(22)
      acc234(27)=acc234(31)+acc234(30)+acc234(29)+acc234(28)+acc234(20)+acc234(&
      &27)
      acc234(27)=Qspvak4k5*acc234(27)
      acc234(28)=-Qspvak4k5*acc234(11)
      acc234(28)=acc234(28)+acc234(10)
      acc234(28)=Qspvak2k1*acc234(28)
      acc234(29)=Qspk2+Qspk1
      acc234(29)=acc234(2)*acc234(29)
      acc234(30)=Qspvak2k6*acc234(16)
      acc234(31)=Qspvak2k5*acc234(19)
      acc234(32)=Qspvak4k1*acc234(26)
      acc234(28)=acc234(32)+acc234(31)+acc234(30)+acc234(9)+acc234(29)+acc234(2&
      &8)
      acc234(28)=Qspe6*acc234(28)
      acc234(29)=Qspvak2k5*acc234(21)
      acc234(30)=Qspvak4k1*acc234(13)
      acc234(29)=acc234(29)-acc234(30)
      acc234(30)=acc234(14)+acc234(29)
      acc234(30)=Qspk6*acc234(30)
      acc234(31)=Qspvak2k5*acc234(23)
      acc234(32)=Qspvak4k1*acc234(3)
      acc234(31)=acc234(32)+acc234(17)+acc234(31)
      acc234(31)=QspQ*acc234(31)
      acc234(32)=-Qspk6*acc234(8)
      acc234(33)=QspQ*acc234(18)
      acc234(32)=acc234(33)+acc234(7)+acc234(32)
      acc234(32)=Qspvak2e6*acc234(32)
      acc234(33)=Qspvak2e6*acc234(8)
      acc234(29)=acc234(33)+acc234(15)-acc234(29)
      acc234(29)=Qspk2*acc234(29)
      acc234(33)=Qspk1*acc234(1)
      acc234(34)=Qspvak2k6*acc234(5)
      brack=acc234(6)+acc234(27)+acc234(28)+acc234(29)+acc234(30)+acc234(31)+ac&
      &c234(32)+acc234(33)+acc234(34)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p12_sbars_hepemg_groups, only: &
!           & sign => diagram234_sign, shift => diagram234_shift
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd234h3
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d234
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(0:3) :: qshift
      qshift = -k2
      Q(1)  =cmplx(real(-Q_ext(4)  -qshift(0),  ki_sam),aimag(-Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(-Q_ext(1:3)-qshift(1:3),ki_sam),aimag(-Q_ext(1:3)),ki)
      d234 = 0.0_ki
      d234 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d234, ki), aimag(d234), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd234h3
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d234
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(4) :: qshift
      qshift = -k2
      Q(:)  =cmplx(real(-Q_ext(:)  -qshift(:),  ki_gol), 0.0_ki_gol, ki)
      d234 = 0.0_ki
      d234 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d234, ki), aimag(d234), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p12_sbars_hepemg_d234h3l1
