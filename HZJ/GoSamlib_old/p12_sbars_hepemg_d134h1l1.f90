module     p12_sbars_hepemg_d134h1l1
   ! file: /home/oleari/fortran/POWHEG-BOX-trunk/HZJ/GoSam_POWHEG/Virtual/p12_s &
   ! &bars_hepemg/helicity1d134h1l1.f90
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
      use p12_sbars_hepemg_abbrevd134h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc134(19)
      complex(ki) :: Qspvak6k1
      complex(ki) :: Qspe6
      complex(ki) :: Qspvak2e6
      complex(ki) :: Qspk6
      complex(ki) :: Qspk2
      complex(ki) :: Qspvak5k4
      complex(ki) :: QspQ
      complex(ki) :: Qspk1
      complex(ki) :: Qspvak5k1
      Qspvak6k1 = dotproduct(Q,spvak6k1)
      Qspe6 = dotproduct(Q,e6)
      Qspvak2e6 = dotproduct(Q,spvak2e6)
      Qspk6 = dotproduct(Q,k6)
      Qspk2 = dotproduct(Q,k2)
      Qspvak5k4 = dotproduct(Q,spvak5k4)
      QspQ = dotproduct(Q,Q)
      Qspk1 = dotproduct(Q,k1)
      Qspvak5k1 = dotproduct(Q,spvak5k1)
      acc134(1)=abb134(5)
      acc134(2)=abb134(6)
      acc134(3)=abb134(7)
      acc134(4)=abb134(8)
      acc134(5)=abb134(10)
      acc134(6)=abb134(11)
      acc134(7)=abb134(13)
      acc134(8)=abb134(14)
      acc134(9)=abb134(15)
      acc134(10)=abb134(16)
      acc134(11)=abb134(17)
      acc134(12)=abb134(18)
      acc134(13)=acc134(6)*Qspvak6k1
      acc134(14)=Qspe6*acc134(3)
      acc134(15)=Qspvak2e6*acc134(10)
      acc134(16)=Qspk6-Qspk2
      acc134(17)=acc134(7)*acc134(16)
      acc134(13)=acc134(17)+acc134(15)+acc134(14)+acc134(8)+acc134(13)
      acc134(13)=Qspvak5k4*acc134(13)
      acc134(14)=acc134(12)*QspQ
      acc134(15)=acc134(11)*Qspk1
      acc134(17)=acc134(4)*Qspvak5k1
      acc134(18)=Qspe6*acc134(2)
      acc134(19)=Qspvak2e6*acc134(5)
      acc134(16)=-acc134(9)*acc134(16)
      brack=acc134(1)+acc134(13)+acc134(14)+acc134(15)+acc134(16)+acc134(17)+ac&
      &c134(18)+acc134(19)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p12_sbars_hepemg_groups, only: &
!           & sign => diagram134_sign, shift => diagram134_shift
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd134h1
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d134
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(0:3) :: qshift
      qshift = k2
      Q(1)  =cmplx(real(+Q_ext(4)  -qshift(0),  ki_sam),aimag(+Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(+Q_ext(1:3)-qshift(1:3),ki_sam),aimag(+Q_ext(1:3)),ki)
      d134 = 0.0_ki
      d134 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d134, ki), aimag(d134), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd134h1
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d134
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(4) :: qshift
      qshift = k2
      Q(:)  =cmplx(real(+Q_ext(:)  -qshift(:),  ki_gol), 0.0_ki_gol, ki)
      d134 = 0.0_ki
      d134 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d134, ki), aimag(d134), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p12_sbars_hepemg_d134h1l1
