module     p12_sbars_hepemg_d233h3l1
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p12_sbars_hepemg/helicity3d233h3l1.f90
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
      use p12_sbars_hepemg_abbrevd233h3
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc233(35)
      complex(ki) :: Qspvak4k5
      complex(ki) :: Qspvak2k1
      complex(ki) :: Qspvak6k1
      complex(ki) :: Qspvak2k6
      complex(ki) :: QspQ
      complex(ki) :: Qspvak2k5
      complex(ki) :: Qspk1
      complex(ki) :: Qspe6
      complex(ki) :: Qspvae6k1
      complex(ki) :: Qspvak2e6
      complex(ki) :: Qspk6
      Qspvak4k5 = dotproduct(Q,spvak4k5)
      Qspvak2k1 = dotproduct(Q,spvak2k1)
      Qspvak6k1 = dotproduct(Q,spvak6k1)
      Qspvak2k6 = dotproduct(Q,spvak2k6)
      QspQ = dotproduct(Q,Q)
      Qspvak2k5 = dotproduct(Q,spvak2k5)
      Qspk1 = dotproduct(Q,k1)
      Qspe6 = dotproduct(Q,e6)
      Qspvae6k1 = dotproduct(Q,spvae6k1)
      Qspvak2e6 = dotproduct(Q,spvak2e6)
      Qspk6 = dotproduct(Q,k6)
      acc233(1)=abb233(4)
      acc233(2)=abb233(5)
      acc233(3)=abb233(6)
      acc233(4)=abb233(7)
      acc233(5)=abb233(8)
      acc233(6)=abb233(9)
      acc233(7)=abb233(10)
      acc233(8)=abb233(11)
      acc233(9)=abb233(12)
      acc233(10)=abb233(13)
      acc233(11)=abb233(14)
      acc233(12)=abb233(15)
      acc233(13)=abb233(16)
      acc233(14)=abb233(17)
      acc233(15)=abb233(18)
      acc233(16)=abb233(19)
      acc233(17)=abb233(20)
      acc233(18)=abb233(21)
      acc233(19)=abb233(22)
      acc233(20)=abb233(23)
      acc233(21)=abb233(25)
      acc233(22)=abb233(26)
      acc233(23)=abb233(27)
      acc233(24)=abb233(28)
      acc233(25)=abb233(29)
      acc233(26)=abb233(30)
      acc233(27)=abb233(31)
      acc233(28)=Qspvak4k5*acc233(8)
      acc233(28)=acc233(28)+acc233(5)
      acc233(28)=Qspvak2k1*acc233(28)
      acc233(29)=acc233(9)*Qspvak6k1
      acc233(30)=Qspvak2k6*acc233(26)
      acc233(31)=QspQ*acc233(15)
      acc233(32)=Qspvak2k5*acc233(10)
      acc233(33)=Qspk1*acc233(3)
      acc233(28)=acc233(33)+acc233(32)+acc233(31)+acc233(30)+acc233(29)+acc233(&
      &7)+acc233(28)
      acc233(28)=Qspe6*acc233(28)
      acc233(29)=-Qspvak2k6*acc233(27)
      acc233(30)=Qspvak2k1*acc233(14)
      acc233(31)=Qspvae6k1*acc233(23)
      acc233(32)=Qspvak2e6*acc233(22)
      acc233(33)=Qspk1*acc233(21)
      acc233(29)=acc233(33)+acc233(32)+acc233(31)+acc233(30)+acc233(20)+acc233(&
      &29)
      acc233(29)=Qspvak4k5*acc233(29)
      acc233(30)=Qspvae6k1*acc233(18)
      acc233(31)=Qspvak2e6*acc233(16)
      acc233(30)=acc233(30)-acc233(31)
      acc233(31)=acc233(13)-acc233(30)
      acc233(31)=Qspk6*acc233(31)
      acc233(32)=Qspvae6k1*acc233(11)
      acc233(33)=Qspvak2e6*acc233(17)
      acc233(32)=acc233(33)+acc233(12)+acc233(32)
      acc233(32)=QspQ*acc233(32)
      acc233(33)=-Qspk6*acc233(19)
      acc233(34)=QspQ*acc233(24)
      acc233(33)=acc233(34)+acc233(4)+acc233(33)
      acc233(33)=Qspvak2k5*acc233(33)
      acc233(34)=Qspvak2k5*acc233(19)
      acc233(30)=acc233(34)+acc233(1)+acc233(30)
      acc233(30)=Qspk1*acc233(30)
      acc233(34)=Qspvak2k6*acc233(25)
      acc233(35)=Qspvak2k1*acc233(2)
      brack=acc233(6)+acc233(28)+acc233(29)+acc233(30)+acc233(31)+acc233(32)+ac&
      &c233(33)+acc233(34)+acc233(35)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p12_sbars_hepemg_groups, only: &
!           & sign => diagram233_sign, shift => diagram233_shift
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd233h3
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d233
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(0:3) :: qshift
      qshift = k2
      Q(1)  =cmplx(real(+Q_ext(4)  -qshift(0),  ki_sam),aimag(+Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(+Q_ext(1:3)-qshift(1:3),ki_sam),aimag(+Q_ext(1:3)),ki)
      d233 = 0.0_ki
      d233 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d233, ki), aimag(d233), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd233h3
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d233
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(4) :: qshift
      qshift = k2
      Q(:)  =cmplx(real(+Q_ext(:)  -qshift(:),  ki_gol), 0.0_ki_gol, ki)
      d233 = 0.0_ki
      d233 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d233, ki), aimag(d233), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p12_sbars_hepemg_d233h3l1
