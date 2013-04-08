module     p4_ubaru_hepemg_d167h1l1
   ! file: /home/oleari/fortran/POWHEG-BOX-trunk/HZJ/GoSam_POWHEG/Virtual/p4_ub &
   ! &aru_hepemg/helicity1d167h1l1.f90
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
      use p4_ubaru_hepemg_abbrevd167h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc167(13)
      complex(ki) :: Qspk1
      complex(ki) :: Qspk6
      complex(ki) :: Qspvak1k6
      complex(ki) :: Qspvak5k4
      complex(ki) :: Qspvak2k4
      complex(ki) :: Qspvak6k1
      Qspk1 = dotproduct(Q,k1)
      Qspk6 = dotproduct(Q,k6)
      Qspvak1k6 = dotproduct(Q,spvak1k6)
      Qspvak5k4 = dotproduct(Q,spvak5k4)
      Qspvak2k4 = dotproduct(Q,spvak2k4)
      Qspvak6k1 = dotproduct(Q,spvak6k1)
      acc167(1)=abb167(5)
      acc167(2)=abb167(6)
      acc167(3)=abb167(7)
      acc167(4)=abb167(8)
      acc167(5)=abb167(9)
      acc167(6)=abb167(12)
      acc167(7)=abb167(15)
      acc167(8)=acc167(7)*Qspk1
      acc167(9)=acc167(6)*Qspk6
      acc167(10)=acc167(5)*Qspvak1k6
      acc167(11)=acc167(4)*Qspvak5k4
      acc167(12)=acc167(2)*Qspvak2k4
      acc167(13)=acc167(1)*Qspvak6k1
      brack=acc167(3)+acc167(8)+acc167(9)+acc167(10)+acc167(11)+acc167(12)+acc1&
      &67(13)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p4_ubaru_hepemg_groups, only: &
!           & sign => diagram167_sign, shift => diagram167_shift
      use p4_ubaru_hepemg_globalsl1, only: epspow
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_abbrevd167h1
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d167
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(0:3) :: qshift
      qshift = -k3-k5-k4
      Q(1)  =cmplx(real(-Q_ext(4)  -qshift(0),  ki_sam),aimag(-Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(-Q_ext(1:3)-qshift(1:3),ki_sam),aimag(-Q_ext(1:3)),ki)
      d167 = 0.0_ki
      d167 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d167, ki), aimag(d167), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p4_ubaru_hepemg_globalsl1, only: epspow
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_abbrevd167h1
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d167
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(4) :: qshift
      qshift = -k3-k5-k4
      Q(:)  =cmplx(real(-Q_ext(:)  -qshift(:),  ki_gol), 0.0_ki_gol, ki)
      d167 = 0.0_ki
      d167 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d167, ki), aimag(d167), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p4_ubaru_hepemg_d167h1l1