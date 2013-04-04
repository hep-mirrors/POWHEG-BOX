module     p16_bbarb_hepemg_d150h1l1
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p16_bbarb_hepemg/helicity1d150h1l1.f90
   ! generator: buildfortran.py
   use p16_bbarb_hepemg_config, only: ki
   use p16_bbarb_hepemg_util, only: cond
   implicit none
   private
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   public :: numerator_samurai
   public :: numerator_golem95
contains
!---#[ function brack_1:
   pure function brack_1(Q,mu2) result(brack)
      use p16_bbarb_hepemg_model
      use p16_bbarb_hepemg_kinematics
      use p16_bbarb_hepemg_color
      use p16_bbarb_hepemg_abbrevd150h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc150(19)
      complex(ki) :: Qspvak5k4
      complex(ki) :: Qspvak2k6
      complex(ki) :: Qspvak2k1
      complex(ki) :: Qspvak6k1
      complex(ki) :: Qspvak2k4
      complex(ki) :: Qspvak1k6
      complex(ki) :: Qspk6
      complex(ki) :: Qspk1
      complex(ki) :: QspQ
      Qspvak5k4 = dotproduct(Q,spvak5k4)
      Qspvak2k6 = dotproduct(Q,spvak2k6)
      Qspvak2k1 = dotproduct(Q,spvak2k1)
      Qspvak6k1 = dotproduct(Q,spvak6k1)
      Qspvak2k4 = dotproduct(Q,spvak2k4)
      Qspvak1k6 = dotproduct(Q,spvak1k6)
      Qspk6 = dotproduct(Q,k6)
      Qspk1 = dotproduct(Q,k1)
      QspQ = dotproduct(Q,Q)
      acc150(1)=abb150(5)
      acc150(2)=abb150(6)
      acc150(3)=abb150(7)
      acc150(4)=abb150(8)
      acc150(5)=abb150(9)
      acc150(6)=abb150(10)
      acc150(7)=abb150(14)
      acc150(8)=abb150(15)
      acc150(9)=abb150(16)
      acc150(10)=abb150(17)
      acc150(11)=abb150(18)
      acc150(12)=acc150(10)*Qspvak5k4
      acc150(12)=acc150(12)+acc150(9)
      acc150(12)=Qspvak2k6*acc150(12)
      acc150(13)=acc150(3)*Qspvak5k4
      acc150(13)=acc150(13)+acc150(5)
      acc150(13)=Qspvak2k1*acc150(13)
      acc150(14)=Qspvak6k1*acc150(8)
      acc150(15)=Qspvak2k4*acc150(11)
      acc150(16)=Qspvak1k6*acc150(1)
      acc150(17)=Qspk6*acc150(2)
      acc150(18)=Qspk1*acc150(4)
      acc150(19)=QspQ*acc150(7)
      brack=acc150(6)+acc150(12)+acc150(13)+acc150(14)+acc150(15)+acc150(16)+ac&
      &c150(17)+acc150(18)+acc150(19)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p16_bbarb_hepemg_groups, only: &
!           & sign => diagram150_sign, shift => diagram150_shift
      use p16_bbarb_hepemg_globalsl1, only: epspow
      use p16_bbarb_hepemg_kinematics
      use p16_bbarb_hepemg_abbrevd150h1
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d150
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(0:3) :: qshift
      qshift = -k2
      Q(1)  =cmplx(real(-Q_ext(4)  -qshift(0),  ki_sam),aimag(-Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(-Q_ext(1:3)-qshift(1:3),ki_sam),aimag(-Q_ext(1:3)),ki)
      d150 = 0.0_ki
      d150 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d150, ki), aimag(d150), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p16_bbarb_hepemg_globalsl1, only: epspow
      use p16_bbarb_hepemg_kinematics
      use p16_bbarb_hepemg_abbrevd150h1
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d150
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(4) :: qshift
      qshift = -k2
      Q(:)  =cmplx(real(-Q_ext(:)  -qshift(:),  ki_gol), 0.0_ki_gol, ki)
      d150 = 0.0_ki
      d150 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d150, ki), aimag(d150), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p16_bbarb_hepemg_d150h1l1
