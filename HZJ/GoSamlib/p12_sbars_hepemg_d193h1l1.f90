module     p12_sbars_hepemg_d193h1l1
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p12_sbars_hepemg/helicity1d193h1l1.f90
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
      use p12_sbars_hepemg_abbrevd193h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc193(13)
      complex(ki) :: Qspvak6k1
      complex(ki) :: Qspvak5k4
      complex(ki) :: Qspvak2k4
      complex(ki) :: Qspvak1k6
      complex(ki) :: Qspk6
      complex(ki) :: Qspk1
      Qspvak6k1 = dotproduct(Q,spvak6k1)
      Qspvak5k4 = dotproduct(Q,spvak5k4)
      Qspvak2k4 = dotproduct(Q,spvak2k4)
      Qspvak1k6 = dotproduct(Q,spvak1k6)
      Qspk6 = dotproduct(Q,k6)
      Qspk1 = dotproduct(Q,k1)
      acc193(1)=abb193(5)
      acc193(2)=abb193(6)
      acc193(3)=abb193(7)
      acc193(4)=abb193(8)
      acc193(5)=abb193(9)
      acc193(6)=abb193(12)
      acc193(7)=abb193(15)
      acc193(8)=Qspvak6k1*acc193(1)
      acc193(9)=Qspvak5k4*acc193(4)
      acc193(10)=Qspvak2k4*acc193(2)
      acc193(11)=Qspvak1k6*acc193(5)
      acc193(12)=Qspk6*acc193(6)
      acc193(13)=Qspk1*acc193(7)
      brack=acc193(3)+acc193(8)+acc193(9)+acc193(10)+acc193(11)+acc193(12)+acc1&
      &93(13)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p12_sbars_hepemg_groups, only: &
!           & sign => diagram193_sign, shift => diagram193_shift
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd193h1
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d193
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(0:3) :: qshift
      qshift = -k3-k5-k4
      Q(1)  =cmplx(real(-Q_ext(4)  -qshift(0),  ki_sam),aimag(-Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(-Q_ext(1:3)-qshift(1:3),ki_sam),aimag(-Q_ext(1:3)),ki)
      d193 = 0.0_ki
      d193 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d193, ki), aimag(d193), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd193h1
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d193
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(4) :: qshift
      qshift = -k3-k5-k4
      Q(:)  =cmplx(real(-Q_ext(:)  -qshift(:),  ki_gol), 0.0_ki_gol, ki)
      d193 = 0.0_ki
      d193 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d193, ki), aimag(d193), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p12_sbars_hepemg_d193h1l1
