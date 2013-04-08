module     p8_cbarc_hepemg_d204h2l1
   ! file: /home/oleari/fortran/POWHEG-BOX-trunk/HZJ/GoSam_POWHEG/Virtual/p8_cb &
   ! &arc_hepemg/helicity2d204h2l1.f90
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
      use p8_cbarc_hepemg_abbrevd204h2
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc204(35)
      complex(ki) :: Qspvak4k5
      complex(ki) :: Qspvak1k2
      complex(ki) :: Qspvak1k6
      complex(ki) :: Qspvak6k2
      complex(ki) :: QspQ
      complex(ki) :: Qspvak4k2
      complex(ki) :: Qspk1
      complex(ki) :: Qspe6
      complex(ki) :: Qspvak1e6
      complex(ki) :: Qspvae6k2
      complex(ki) :: Qspk6
      Qspvak4k5 = dotproduct(Q,spvak4k5)
      Qspvak1k2 = dotproduct(Q,spvak1k2)
      Qspvak1k6 = dotproduct(Q,spvak1k6)
      Qspvak6k2 = dotproduct(Q,spvak6k2)
      QspQ = dotproduct(Q,Q)
      Qspvak4k2 = dotproduct(Q,spvak4k2)
      Qspk1 = dotproduct(Q,k1)
      Qspe6 = dotproduct(Q,e6)
      Qspvak1e6 = dotproduct(Q,spvak1e6)
      Qspvae6k2 = dotproduct(Q,spvae6k2)
      Qspk6 = dotproduct(Q,k6)
      acc204(1)=abb204(4)
      acc204(2)=abb204(5)
      acc204(3)=abb204(6)
      acc204(4)=abb204(7)
      acc204(5)=abb204(8)
      acc204(6)=abb204(9)
      acc204(7)=abb204(10)
      acc204(8)=abb204(11)
      acc204(9)=abb204(12)
      acc204(10)=abb204(13)
      acc204(11)=abb204(14)
      acc204(12)=abb204(15)
      acc204(13)=abb204(16)
      acc204(14)=abb204(17)
      acc204(15)=abb204(18)
      acc204(16)=abb204(19)
      acc204(17)=abb204(20)
      acc204(18)=abb204(21)
      acc204(19)=abb204(22)
      acc204(20)=abb204(23)
      acc204(21)=abb204(24)
      acc204(22)=abb204(25)
      acc204(23)=abb204(26)
      acc204(24)=abb204(28)
      acc204(25)=abb204(29)
      acc204(26)=abb204(30)
      acc204(27)=abb204(31)
      acc204(28)=Qspvak4k5*acc204(8)
      acc204(28)=acc204(28)+acc204(5)
      acc204(28)=Qspvak1k2*acc204(28)
      acc204(29)=acc204(9)*Qspvak1k6
      acc204(30)=Qspvak6k2*acc204(21)
      acc204(31)=QspQ*acc204(15)
      acc204(32)=Qspvak4k2*acc204(10)
      acc204(33)=Qspk1*acc204(3)
      acc204(28)=acc204(33)+acc204(32)+acc204(31)+acc204(30)+acc204(29)+acc204(&
      &7)+acc204(28)
      acc204(28)=Qspe6*acc204(28)
      acc204(29)=-Qspvak6k2*acc204(27)
      acc204(30)=Qspvak1k2*acc204(14)
      acc204(31)=Qspvak1e6*acc204(26)
      acc204(32)=Qspvae6k2*acc204(25)
      acc204(33)=Qspk1*acc204(24)
      acc204(29)=acc204(33)+acc204(32)+acc204(31)+acc204(30)+acc204(23)+acc204(&
      &29)
      acc204(29)=Qspvak4k5*acc204(29)
      acc204(30)=Qspvak1e6*acc204(18)
      acc204(31)=Qspvae6k2*acc204(16)
      acc204(30)=acc204(30)-acc204(31)
      acc204(31)=acc204(13)-acc204(30)
      acc204(31)=Qspk6*acc204(31)
      acc204(32)=Qspvak1e6*acc204(11)
      acc204(33)=Qspvae6k2*acc204(17)
      acc204(32)=acc204(33)+acc204(12)+acc204(32)
      acc204(32)=QspQ*acc204(32)
      acc204(33)=-Qspk6*acc204(19)
      acc204(34)=QspQ*acc204(22)
      acc204(33)=acc204(34)+acc204(4)+acc204(33)
      acc204(33)=Qspvak4k2*acc204(33)
      acc204(34)=Qspvak4k2*acc204(19)
      acc204(30)=acc204(34)+acc204(1)+acc204(30)
      acc204(30)=Qspk1*acc204(30)
      acc204(34)=Qspvak6k2*acc204(20)
      acc204(35)=Qspvak1k2*acc204(2)
      brack=acc204(6)+acc204(28)+acc204(29)+acc204(30)+acc204(31)+acc204(32)+ac&
      &c204(33)+acc204(34)+acc204(35)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p8_cbarc_hepemg_groups, only: &
!           & sign => diagram204_sign, shift => diagram204_shift
      use p8_cbarc_hepemg_globalsl1, only: epspow
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_abbrevd204h2
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d204
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(0:3) :: qshift
      qshift = k2
      Q(1)  =cmplx(real(+Q_ext(4)  -qshift(0),  ki_sam),aimag(+Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(+Q_ext(1:3)-qshift(1:3),ki_sam),aimag(+Q_ext(1:3)),ki)
      d204 = 0.0_ki
      d204 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d204, ki), aimag(d204), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p8_cbarc_hepemg_globalsl1, only: epspow
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_abbrevd204h2
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d204
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(4) :: qshift
      qshift = k2
      Q(:)  =cmplx(real(+Q_ext(:)  -qshift(:),  ki_gol), 0.0_ki_gol, ki)
      d204 = 0.0_ki
      d204 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d204, ki), aimag(d204), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p8_cbarc_hepemg_d204h2l1