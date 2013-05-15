module     p12_sbars_hepemg_d244h1l1
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p12_sbars_hepemg/helicity1d244h1l1.f90
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
      use p12_sbars_hepemg_abbrevd244h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc244(36)
      complex(ki) :: Qspvak5k1
      complex(ki) :: Qspvak2k1
      complex(ki) :: Qspvak2k4
      complex(ki) :: Qspe6
      complex(ki) :: Qspvak6e6
      complex(ki) :: Qspvak5e6
      complex(ki) :: Qspvae6k6
      complex(ki) :: Qspval3e6
      complex(ki) :: Qspvae6l3
      complex(ki) :: Qspvae6k4
      complex(ki) :: Qspvak5k6
      complex(ki) :: Qspvak6k4
      complex(ki) :: Qspk6
      complex(ki) :: Qspvak2k6
      complex(ki) :: QspQ
      complex(ki) :: Qspvak6k1
      complex(ki) :: Qspvae6k1
      complex(ki) :: Qspvae6k2
      complex(ki) :: Qspvak2e6
      Qspvak5k1 = dotproduct(Q,spvak5k1)
      Qspvak2k1 = dotproduct(Q,spvak2k1)
      Qspvak2k4 = dotproduct(Q,spvak2k4)
      Qspe6 = dotproduct(Q,e6)
      Qspvak6e6 = dotproduct(Q,spvak6e6)
      Qspvak5e6 = dotproduct(Q,spvak5e6)
      Qspvae6k6 = dotproduct(Q,spvae6k6)
      Qspval3e6 = dotproduct(Q,spval3e6)
      Qspvae6l3 = dotproduct(Q,spvae6l3)
      Qspvae6k4 = dotproduct(Q,spvae6k4)
      Qspvak5k6 = dotproduct(Q,spvak5k6)
      Qspvak6k4 = dotproduct(Q,spvak6k4)
      Qspk6 = dotproduct(Q,k6)
      Qspvak2k6 = dotproduct(Q,spvak2k6)
      QspQ = dotproduct(Q,Q)
      Qspvak6k1 = dotproduct(Q,spvak6k1)
      Qspvae6k1 = dotproduct(Q,spvae6k1)
      Qspvae6k2 = dotproduct(Q,spvae6k2)
      Qspvak2e6 = dotproduct(Q,spvak2e6)
      acc244(1)=abb244(8)
      acc244(2)=abb244(9)
      acc244(3)=abb244(10)
      acc244(4)=abb244(11)
      acc244(5)=abb244(12)
      acc244(6)=abb244(13)
      acc244(7)=abb244(14)
      acc244(8)=abb244(15)
      acc244(9)=abb244(16)
      acc244(10)=abb244(17)
      acc244(11)=abb244(18)
      acc244(12)=abb244(20)
      acc244(13)=abb244(21)
      acc244(14)=abb244(22)
      acc244(15)=abb244(23)
      acc244(16)=abb244(25)
      acc244(17)=abb244(27)
      acc244(18)=abb244(28)
      acc244(19)=abb244(29)
      acc244(20)=abb244(31)
      acc244(21)=acc244(6)*Qspvak5k1
      acc244(22)=acc244(5)*Qspvak2k1
      acc244(23)=acc244(4)*Qspvak2k4
      acc244(21)=acc244(23)+acc244(22)+acc244(7)+acc244(21)
      acc244(21)=Qspe6*acc244(21)
      acc244(22)=acc244(20)*Qspvak6e6
      acc244(23)=acc244(19)*Qspvak5e6
      acc244(24)=acc244(18)*Qspvae6k6
      acc244(25)=acc244(17)*Qspval3e6
      acc244(26)=acc244(16)*Qspvae6l3
      acc244(27)=acc244(15)*Qspvae6k4
      acc244(28)=acc244(14)*Qspvak5k6
      acc244(29)=acc244(13)*Qspvak6k4
      acc244(30)=acc244(12)*Qspk6
      acc244(31)=acc244(11)*Qspvak2k6
      acc244(32)=acc244(10)*QspQ
      acc244(33)=acc244(9)*Qspvak6k1
      acc244(34)=acc244(8)*Qspvae6k1
      acc244(35)=acc244(3)*Qspvae6k2
      acc244(36)=acc244(2)*Qspvak2e6
      brack=acc244(1)+acc244(21)+acc244(22)+acc244(23)+acc244(24)+acc244(25)+ac&
      &c244(26)+acc244(27)+acc244(28)+acc244(29)+acc244(30)+acc244(31)+acc244(3&
      &2)+acc244(33)+acc244(34)+acc244(35)+acc244(36)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p12_sbars_hepemg_groups, only: &
!           & sign => diagram244_sign, shift => diagram244_shift
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd244h1
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d244
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      Q(1)  =cmplx(real(-Q_ext(4),  ki_sam),aimag(-Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(-Q_ext(1:3),ki_sam),aimag(-Q_ext(1:3)),ki)
      d244 = 0.0_ki
      d244 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d244, ki), aimag(d244), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd244h1
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d244
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      Q(:)  =cmplx(real(-Q_ext(:),  ki_gol), 0.0_ki_gol, ki)
      d244 = 0.0_ki
      d244 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d244, ki), aimag(d244), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p12_sbars_hepemg_d244h1l1
