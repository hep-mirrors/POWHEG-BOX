module     p0_dbard_hepemg_d235h2l1
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p0_dbard_hepemg/helicity2d235h2l1.f90
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
      use p0_dbard_hepemg_abbrevd235h2
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc235(35)
      complex(ki) :: Qspvak1k5
      complex(ki) :: Qspvak1k2
      complex(ki) :: Qspk1
      complex(ki) :: Qspvak6k2
      complex(ki) :: Qspk6
      complex(ki) :: Qspvak4k2
      complex(ki) :: QspQ
      complex(ki) :: Qspvak4k5
      complex(ki) :: Qspe6
      complex(ki) :: Qspk2
      complex(ki) :: Qspvae6k2
      Qspvak1k5 = dotproduct(Q,spvak1k5)
      Qspvak1k2 = dotproduct(Q,spvak1k2)
      Qspk1 = dotproduct(Q,k1)
      Qspvak6k2 = dotproduct(Q,spvak6k2)
      Qspk6 = dotproduct(Q,k6)
      Qspvak4k2 = dotproduct(Q,spvak4k2)
      QspQ = dotproduct(Q,Q)
      Qspvak4k5 = dotproduct(Q,spvak4k5)
      Qspe6 = dotproduct(Q,e6)
      Qspk2 = dotproduct(Q,k2)
      Qspvae6k2 = dotproduct(Q,spvae6k2)
      acc235(1)=abb235(3)
      acc235(2)=abb235(4)
      acc235(3)=abb235(5)
      acc235(4)=abb235(6)
      acc235(5)=abb235(7)
      acc235(6)=abb235(8)
      acc235(7)=abb235(9)
      acc235(8)=abb235(10)
      acc235(9)=abb235(11)
      acc235(10)=abb235(12)
      acc235(11)=abb235(13)
      acc235(12)=abb235(14)
      acc235(13)=abb235(15)
      acc235(14)=abb235(16)
      acc235(15)=abb235(17)
      acc235(16)=abb235(18)
      acc235(17)=abb235(19)
      acc235(18)=abb235(20)
      acc235(19)=abb235(21)
      acc235(20)=abb235(22)
      acc235(21)=abb235(23)
      acc235(22)=abb235(24)
      acc235(23)=abb235(25)
      acc235(24)=abb235(26)
      acc235(25)=abb235(27)
      acc235(26)=abb235(28)
      acc235(27)=acc235(12)*Qspvak1k5
      acc235(28)=Qspvak1k2*acc235(3)
      acc235(29)=Qspk1*acc235(21)
      acc235(30)=Qspvak6k2*acc235(8)
      acc235(31)=Qspk6*acc235(18)
      acc235(32)=Qspvak4k2*acc235(14)
      acc235(33)=QspQ*acc235(4)
      acc235(34)=-Qspvak1k2*acc235(2)
      acc235(34)=acc235(13)+acc235(34)
      acc235(34)=Qspvak4k5*acc235(34)
      acc235(27)=acc235(34)+acc235(33)+acc235(32)+acc235(31)+acc235(30)+acc235(&
      &29)+acc235(28)+acc235(27)+acc235(11)
      acc235(27)=Qspe6*acc235(27)
      acc235(28)=Qspk6+Qspk2
      acc235(28)=acc235(26)*acc235(28)
      acc235(29)=Qspvak6k2*acc235(16)
      acc235(30)=Qspvae6k2*acc235(15)
      acc235(31)=-QspQ*acc235(25)
      acc235(28)=acc235(31)+acc235(30)+acc235(29)+acc235(6)+acc235(28)
      acc235(28)=Qspvak4k5*acc235(28)
      acc235(29)=Qspvak4k2*acc235(19)
      acc235(30)=-Qspvae6k2*acc235(23)
      acc235(29)=acc235(30)+acc235(5)+acc235(29)
      acc235(29)=QspQ*acc235(29)
      acc235(30)=-Qspk1*acc235(10)
      acc235(31)=Qspk2*acc235(20)
      acc235(32)=Qspvak6k2*acc235(1)
      acc235(33)=Qspk6*acc235(17)
      acc235(34)=-Qspk2+Qspk6
      acc235(34)=Qspvak4k2*acc235(22)*acc235(34)
      acc235(35)=Qspk1*acc235(24)
      acc235(35)=acc235(9)+acc235(35)
      acc235(35)=Qspvae6k2*acc235(35)
      brack=acc235(7)+acc235(27)+acc235(28)+acc235(29)+acc235(30)+acc235(31)+ac&
      &c235(32)+acc235(33)+acc235(34)+acc235(35)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p0_dbard_hepemg_groups, only: &
!           & sign => diagram235_sign, shift => diagram235_shift
      use p0_dbard_hepemg_globalsl1, only: epspow
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_abbrevd235h2
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d235
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(0:3) :: qshift
      qshift = k6
      Q(1)  =cmplx(real(+Q_ext(4)  -qshift(0),  ki_sam),aimag(+Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(+Q_ext(1:3)-qshift(1:3),ki_sam),aimag(+Q_ext(1:3)),ki)
      d235 = 0.0_ki
      d235 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d235, ki), aimag(d235), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p0_dbard_hepemg_globalsl1, only: epspow
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_abbrevd235h2
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d235
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(4) :: qshift
      qshift = k6
      Q(:)  =cmplx(real(+Q_ext(:)  -qshift(:),  ki_gol), 0.0_ki_gol, ki)
      d235 = 0.0_ki
      d235 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d235, ki), aimag(d235), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p0_dbard_hepemg_d235h2l1
