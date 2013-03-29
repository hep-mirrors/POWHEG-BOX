module     p4_ubaru_hepemg_d206h0l1
   ! file: /home/oleari/fortran/POWHEG-BOX-trunk/HZJ/GoSam_POWHEG/Virtual/p4_ub &
   ! &aru_hepemg/helicity0d206h0l1.f90
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
      use p4_ubaru_hepemg_abbrevd206h0
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc206(35)
      complex(ki) :: Qspvak1k4
      complex(ki) :: Qspvak1k2
      complex(ki) :: Qspk1
      complex(ki) :: Qspvak6k2
      complex(ki) :: Qspk6
      complex(ki) :: Qspvak5k2
      complex(ki) :: QspQ
      complex(ki) :: Qspvak5k4
      complex(ki) :: Qspe6
      complex(ki) :: Qspk2
      complex(ki) :: Qspvae6k2
      Qspvak1k4 = dotproduct(Q,spvak1k4)
      Qspvak1k2 = dotproduct(Q,spvak1k2)
      Qspk1 = dotproduct(Q,k1)
      Qspvak6k2 = dotproduct(Q,spvak6k2)
      Qspk6 = dotproduct(Q,k6)
      Qspvak5k2 = dotproduct(Q,spvak5k2)
      QspQ = dotproduct(Q,Q)
      Qspvak5k4 = dotproduct(Q,spvak5k4)
      Qspe6 = dotproduct(Q,e6)
      Qspk2 = dotproduct(Q,k2)
      Qspvae6k2 = dotproduct(Q,spvae6k2)
      acc206(1)=abb206(3)
      acc206(2)=abb206(4)
      acc206(3)=abb206(5)
      acc206(4)=abb206(6)
      acc206(5)=abb206(7)
      acc206(6)=abb206(8)
      acc206(7)=abb206(9)
      acc206(8)=abb206(10)
      acc206(9)=abb206(11)
      acc206(10)=abb206(12)
      acc206(11)=abb206(13)
      acc206(12)=abb206(14)
      acc206(13)=abb206(15)
      acc206(14)=abb206(16)
      acc206(15)=abb206(17)
      acc206(16)=abb206(18)
      acc206(17)=abb206(19)
      acc206(18)=abb206(20)
      acc206(19)=abb206(21)
      acc206(20)=abb206(22)
      acc206(21)=abb206(23)
      acc206(22)=abb206(24)
      acc206(23)=abb206(25)
      acc206(24)=abb206(26)
      acc206(25)=abb206(27)
      acc206(26)=abb206(28)
      acc206(27)=acc206(12)*Qspvak1k4
      acc206(28)=Qspvak1k2*acc206(3)
      acc206(29)=Qspk1*acc206(21)
      acc206(30)=Qspvak6k2*acc206(8)
      acc206(31)=Qspk6*acc206(18)
      acc206(32)=Qspvak5k2*acc206(14)
      acc206(33)=QspQ*acc206(4)
      acc206(34)=-Qspvak1k2*acc206(2)
      acc206(34)=acc206(13)+acc206(34)
      acc206(34)=Qspvak5k4*acc206(34)
      acc206(27)=acc206(34)+acc206(33)+acc206(32)+acc206(31)+acc206(30)+acc206(&
      &29)+acc206(28)+acc206(27)+acc206(11)
      acc206(27)=Qspe6*acc206(27)
      acc206(28)=Qspk6+Qspk2
      acc206(28)=acc206(26)*acc206(28)
      acc206(29)=Qspvak6k2*acc206(16)
      acc206(30)=Qspvae6k2*acc206(15)
      acc206(31)=-QspQ*acc206(25)
      acc206(28)=acc206(31)+acc206(30)+acc206(29)+acc206(6)+acc206(28)
      acc206(28)=Qspvak5k4*acc206(28)
      acc206(29)=Qspvak5k2*acc206(19)
      acc206(30)=-Qspvae6k2*acc206(23)
      acc206(29)=acc206(30)+acc206(5)+acc206(29)
      acc206(29)=QspQ*acc206(29)
      acc206(30)=-Qspk1*acc206(10)
      acc206(31)=Qspk2*acc206(20)
      acc206(32)=Qspvak6k2*acc206(1)
      acc206(33)=Qspk6*acc206(17)
      acc206(34)=-Qspk2+Qspk6
      acc206(34)=Qspvak5k2*acc206(22)*acc206(34)
      acc206(35)=Qspk1*acc206(24)
      acc206(35)=acc206(9)+acc206(35)
      acc206(35)=Qspvae6k2*acc206(35)
      brack=acc206(7)+acc206(27)+acc206(28)+acc206(29)+acc206(30)+acc206(31)+ac&
      &c206(32)+acc206(33)+acc206(34)+acc206(35)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p4_ubaru_hepemg_groups, only: &
!           & sign => diagram206_sign, shift => diagram206_shift
      use p4_ubaru_hepemg_globalsl1, only: epspow
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_abbrevd206h0
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d206
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(0:3) :: qshift
      qshift = k6
      Q(1)  =cmplx(real(+Q_ext(4)  -qshift(0),  ki_sam),aimag(+Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(+Q_ext(1:3)-qshift(1:3),ki_sam),aimag(+Q_ext(1:3)),ki)
      d206 = 0.0_ki
      d206 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d206, ki), aimag(d206), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p4_ubaru_hepemg_globalsl1, only: epspow
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_abbrevd206h0
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d206
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(4) :: qshift
      qshift = k6
      Q(:)  =cmplx(real(+Q_ext(:)  -qshift(:),  ki_gol), 0.0_ki_gol, ki)
      d206 = 0.0_ki
      d206 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d206, ki), aimag(d206), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p4_ubaru_hepemg_d206h0l1
