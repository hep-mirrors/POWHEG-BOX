module     p0_dbard_hepemg_d268h1l1
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p0_dbard_hepemg/helicity1d268h1l1.f90
   ! generator: buildfortran.py
   use p0_dbard_hepemg_config, only: ki
   use p0_dbard_hepemg_util, only: cond
   implicit none
   private
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   public :: numerator_samurai
   public :: numerator_golem95
contains
!---#[ function brack_1:
   pure function brack_1(Q,mu2) result(brack)
      use p0_dbard_hepemg_model
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_color
      use p0_dbard_hepemg_abbrevd268h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc268(20)
      complex(ki) :: Qspvak6k1
      complex(ki) :: Qspk1
      complex(ki) :: Qspk6
      complex(ki) :: Qspvak2k4
      complex(ki) :: Qspvak5k4
      complex(ki) :: Qspe6
      complex(ki) :: Qspvae6k1
      complex(ki) :: QspQ
      Qspvak6k1 = dotproduct(Q,spvak6k1)
      Qspk1 = dotproduct(Q,k1)
      Qspk6 = dotproduct(Q,k6)
      Qspvak2k4 = dotproduct(Q,spvak2k4)
      Qspvak5k4 = dotproduct(Q,spvak5k4)
      Qspe6 = dotproduct(Q,e6)
      Qspvae6k1 = dotproduct(Q,spvae6k1)
      QspQ = dotproduct(Q,Q)
      acc268(1)=abb268(5)
      acc268(2)=abb268(6)
      acc268(3)=abb268(7)
      acc268(4)=abb268(8)
      acc268(5)=abb268(9)
      acc268(6)=abb268(10)
      acc268(7)=abb268(11)
      acc268(8)=abb268(12)
      acc268(9)=abb268(14)
      acc268(10)=abb268(15)
      acc268(11)=abb268(16)
      acc268(12)=abb268(19)
      acc268(13)=abb268(20)
      acc268(14)=acc268(11)*Qspvak6k1
      acc268(15)=Qspk1*acc268(4)
      acc268(16)=Qspk6*acc268(13)
      acc268(17)=Qspvak2k4*acc268(7)
      acc268(18)=Qspvak5k4*acc268(10)
      acc268(14)=acc268(18)+acc268(17)+acc268(16)+acc268(15)+acc268(14)+acc268(&
      &6)
      acc268(14)=Qspe6*acc268(14)
      acc268(15)=acc268(8)*Qspvae6k1
      acc268(16)=acc268(3)*QspQ
      acc268(17)=Qspk1*acc268(1)
      acc268(18)=Qspk6*acc268(5)
      acc268(19)=Qspvak2k4*acc268(2)
      acc268(20)=Qspvak5k4*acc268(12)
      brack=acc268(9)+acc268(14)+acc268(15)+acc268(16)+acc268(17)+acc268(18)+ac&
      &c268(19)+acc268(20)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p0_dbard_hepemg_groups, only: &
!           & sign => diagram268_sign, shift => diagram268_shift
      use p0_dbard_hepemg_globalsl1, only: epspow
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_abbrevd268h1
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d268
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(0:3) :: qshift
      qshift = -k3-k6-k5-k4
      Q(1)  =cmplx(real(-Q_ext(4)  -qshift(0),  ki_sam),aimag(-Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(-Q_ext(1:3)-qshift(1:3),ki_sam),aimag(-Q_ext(1:3)),ki)
      d268 = 0.0_ki
      d268 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d268, ki), aimag(d268), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p0_dbard_hepemg_globalsl1, only: epspow
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_abbrevd268h1
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d268
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(4) :: qshift
      qshift = -k3-k6-k5-k4
      Q(:)  =cmplx(real(-Q_ext(:)  -qshift(:),  ki_gol), 0.0_ki_gol, ki)
      d268 = 0.0_ki
      d268 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d268, ki), aimag(d268), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p0_dbard_hepemg_d268h1l1
