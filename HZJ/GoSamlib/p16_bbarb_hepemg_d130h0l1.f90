module     p16_bbarb_hepemg_d130h0l1
   ! file: /home/oleari/fortran/POWHEG-BOX-trunk/HZJ/GoSam_POWHEG/Virtual/p16_b &
   ! &barb_hepemg/helicity0d130h0l1.f90
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
      use p16_bbarb_hepemg_abbrevd130h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc130(19)
      complex(ki) :: Qspvak5k4
      complex(ki) :: Qspvak6k2
      complex(ki) :: Qspvak1k2
      complex(ki) :: Qspvak6k1
      complex(ki) :: Qspvak5k2
      complex(ki) :: Qspvak1k6
      complex(ki) :: Qspk6
      complex(ki) :: Qspk1
      complex(ki) :: QspQ
      Qspvak5k4 = dotproduct(Q,spvak5k4)
      Qspvak6k2 = dotproduct(Q,spvak6k2)
      Qspvak1k2 = dotproduct(Q,spvak1k2)
      Qspvak6k1 = dotproduct(Q,spvak6k1)
      Qspvak5k2 = dotproduct(Q,spvak5k2)
      Qspvak1k6 = dotproduct(Q,spvak1k6)
      Qspk6 = dotproduct(Q,k6)
      Qspk1 = dotproduct(Q,k1)
      QspQ = dotproduct(Q,Q)
      acc130(1)=abb130(5)
      acc130(2)=abb130(6)
      acc130(3)=abb130(7)
      acc130(4)=abb130(8)
      acc130(5)=abb130(9)
      acc130(6)=abb130(10)
      acc130(7)=abb130(12)
      acc130(8)=abb130(14)
      acc130(9)=abb130(15)
      acc130(10)=abb130(16)
      acc130(11)=abb130(17)
      acc130(12)=acc130(11)*Qspvak5k4
      acc130(12)=acc130(12)+acc130(9)
      acc130(12)=Qspvak6k2*acc130(12)
      acc130(13)=acc130(3)*Qspvak5k4
      acc130(13)=acc130(13)+acc130(5)
      acc130(13)=Qspvak1k2*acc130(13)
      acc130(14)=Qspvak6k1*acc130(10)
      acc130(15)=Qspvak5k2*acc130(1)
      acc130(16)=Qspvak1k6*acc130(7)
      acc130(17)=Qspk6*acc130(2)
      acc130(18)=Qspk1*acc130(4)
      acc130(19)=QspQ*acc130(8)
      brack=acc130(6)+acc130(12)+acc130(13)+acc130(14)+acc130(15)+acc130(16)+ac&
      &c130(17)+acc130(18)+acc130(19)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p16_bbarb_hepemg_groups, only: &
!           & sign => diagram130_sign, shift => diagram130_shift
      use p16_bbarb_hepemg_globalsl1, only: epspow
      use p16_bbarb_hepemg_kinematics
      use p16_bbarb_hepemg_abbrevd130h0
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d130
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(0:3) :: qshift
      qshift = -k2
      Q(1)  =cmplx(real(-Q_ext(4)  -qshift(0),  ki_sam),aimag(-Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(-Q_ext(1:3)-qshift(1:3),ki_sam),aimag(-Q_ext(1:3)),ki)
      d130 = 0.0_ki
      d130 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d130, ki), aimag(d130), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p16_bbarb_hepemg_globalsl1, only: epspow
      use p16_bbarb_hepemg_kinematics
      use p16_bbarb_hepemg_abbrevd130h0
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d130
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(4) :: qshift
      qshift = -k2
      Q(:)  =cmplx(real(-Q_ext(:)  -qshift(:),  ki_gol), 0.0_ki_gol, ki)
      d130 = 0.0_ki
      d130 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d130, ki), aimag(d130), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p16_bbarb_hepemg_d130h0l1
