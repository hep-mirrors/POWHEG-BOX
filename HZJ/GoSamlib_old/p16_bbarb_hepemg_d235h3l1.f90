module     p16_bbarb_hepemg_d235h3l1
   ! file: /home/oleari/fortran/POWHEG-BOX-trunk/HZJ/GoSam_POWHEG/Virtual/p16_b &
   ! &barb_hepemg/helicity3d235h3l1.f90
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
      use p16_bbarb_hepemg_abbrevd235h3
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc235(20)
      complex(ki) :: Qspk1
      complex(ki) :: Qspvak2k5
      complex(ki) :: Qspvak4k5
      complex(ki) :: Qspk6
      complex(ki) :: Qspvak6k1
      complex(ki) :: Qspe6
      complex(ki) :: Qspvae6k1
      complex(ki) :: QspQ
      Qspk1 = dotproduct(Q,k1)
      Qspvak2k5 = dotproduct(Q,spvak2k5)
      Qspvak4k5 = dotproduct(Q,spvak4k5)
      Qspk6 = dotproduct(Q,k6)
      Qspvak6k1 = dotproduct(Q,spvak6k1)
      Qspe6 = dotproduct(Q,e6)
      Qspvae6k1 = dotproduct(Q,spvae6k1)
      QspQ = dotproduct(Q,Q)
      acc235(1)=abb235(5)
      acc235(2)=abb235(6)
      acc235(3)=abb235(7)
      acc235(4)=abb235(8)
      acc235(5)=abb235(9)
      acc235(6)=abb235(10)
      acc235(7)=abb235(11)
      acc235(8)=abb235(12)
      acc235(9)=abb235(14)
      acc235(10)=abb235(15)
      acc235(11)=abb235(16)
      acc235(12)=abb235(19)
      acc235(13)=abb235(20)
      acc235(14)=acc235(4)*Qspk1
      acc235(15)=acc235(7)*Qspvak2k5
      acc235(16)=acc235(10)*Qspvak4k5
      acc235(17)=acc235(13)*Qspk6
      acc235(18)=Qspvak6k1*acc235(11)
      acc235(14)=acc235(18)+acc235(17)+acc235(16)+acc235(15)+acc235(6)+acc235(1&
      &4)
      acc235(14)=Qspe6*acc235(14)
      acc235(15)=acc235(1)*Qspk1
      acc235(16)=acc235(2)*Qspvak2k5
      acc235(17)=acc235(5)*Qspk6
      acc235(18)=acc235(12)*Qspvak4k5
      acc235(19)=Qspvae6k1*acc235(8)
      acc235(20)=QspQ*acc235(3)
      brack=acc235(9)+acc235(14)+acc235(15)+acc235(16)+acc235(17)+acc235(18)+ac&
      &c235(19)+acc235(20)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p16_bbarb_hepemg_groups, only: &
!           & sign => diagram235_sign, shift => diagram235_shift
      use p16_bbarb_hepemg_globalsl1, only: epspow
      use p16_bbarb_hepemg_kinematics
      use p16_bbarb_hepemg_abbrevd235h3
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
      qshift = -k3-k6-k5-k4
      Q(1)  =cmplx(real(-Q_ext(4)  -qshift(0),  ki_sam),aimag(-Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(-Q_ext(1:3)-qshift(1:3),ki_sam),aimag(-Q_ext(1:3)),ki)
      d235 = 0.0_ki
      d235 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d235, ki), aimag(d235), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p16_bbarb_hepemg_globalsl1, only: epspow
      use p16_bbarb_hepemg_kinematics
      use p16_bbarb_hepemg_abbrevd235h3
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d235
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(4) :: qshift
      qshift = -k3-k6-k5-k4
      Q(:)  =cmplx(real(-Q_ext(:)  -qshift(:),  ki_gol), 0.0_ki_gol, ki)
      d235 = 0.0_ki
      d235 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d235, ki), aimag(d235), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p16_bbarb_hepemg_d235h3l1
