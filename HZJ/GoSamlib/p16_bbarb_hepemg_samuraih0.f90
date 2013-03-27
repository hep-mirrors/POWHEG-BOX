module     p16_bbarb_hepemg_samuraih0
   ! This file has been generated for samurai version 2.1.1
   ! Please, not the interface changes:
   ! 2.0 -> 2.1   : mu2 has changed from real to complex.
   ! 2.1 -> 2.1.1 : samurai_cm and samurai_rm have been made public
   !                we call them directly instead of the generic routine
   !                in order to avoid problems with some older versions of
   !                gfortran.
   !              + passing of invariants has been added.
   use precision, only: ki_sam => ki
   use p16_bbarb_hepemg_config, only: ki
   use p16_bbarb_hepemg_scalar_cache
   implicit none
   private
   public :: reduce_group0
   public :: reduce_group1
   public :: reduce_group2
contains
!---#[ grouped numerators for samurai:
!-----#[ function numeval_group0:
function     numeval_group0(icut, Q, mu2) result(num)
   use p16_bbarb_hepemg_kinematics, only: k1, k2, k3, k4, k5, k6
   use p16_bbarb_hepemg_model
   use p16_bbarb_hepemg_d206h0l1, only: numerator_d206 => numerator_samurai
   implicit none
   integer, intent(in) :: icut
   complex(ki_sam), dimension(4), intent(in) :: Q
   complex(ki_sam), intent(in) :: mu2
   complex(ki_sam) :: num

   logical, dimension(0:1-1) :: nonzero
   real(ki_sam), dimension(0:3) :: R
   complex(ki_sam) :: Q2
   complex(ki_sam) ::denom1,denom2,denom3,denom4

   nonzero(:) = .true.
   Q2 = Q(4)*Q(4) - Q(1)*Q(1) - Q(2)*Q(2) - Q(3)*Q(3) - mu2

   select case(icut)
   case default
      R = real(-k2, ki_sam)
      denom1 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom2 = Q2
      R = real(-k6, ki_sam)
      denom3 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      R = real(k3-k2+k5+k4, ki_sam)
      denom4 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
   end select

   num = (0.0_ki_sam, 0.0_ki_sam)
   !-------#[ Diagram 206:
   if(nonzero(0)) then
      num = num + numerator_d206(icut, Q, mu2)
   end if
   !-------#] Diagram 206:
end function numeval_group0
!-----#] function numeval_group0:
!-----#[ function numeval_group1:
function     numeval_group1(icut, Q, mu2) result(num)
   use p16_bbarb_hepemg_kinematics, only: k1, k2, k3, k4, k5, k6
   use p16_bbarb_hepemg_model
   use p16_bbarb_hepemg_d117h0l1, only: numerator_d117 => numerator_samurai
   use p16_bbarb_hepemg_d134h0l1, only: numerator_d134 => numerator_samurai
   use p16_bbarb_hepemg_d170h0l1, only: numerator_d170 => numerator_samurai
   use p16_bbarb_hepemg_d205h0l1, only: numerator_d205 => numerator_samurai
   use p16_bbarb_hepemg_d233h0l1, only: numerator_d233 => numerator_samurai
   implicit none
   integer, intent(in) :: icut
   complex(ki_sam), dimension(4), intent(in) :: Q
   complex(ki_sam), intent(in) :: mu2
   complex(ki_sam) :: num

   logical, dimension(0:5-1) :: nonzero
   real(ki_sam), dimension(0:3) :: R
   complex(ki_sam) :: Q2
   complex(ki_sam) ::denom1,denom2,denom3,denom4

   nonzero(:) = .true.
   Q2 = Q(4)*Q(4) - Q(1)*Q(1) - Q(2)*Q(2) - Q(3)*Q(3) - mu2

   select case(icut)
   case(1)
      nonzero(1) = .false.
      nonzero(2) = .false.
      R = real(-k6, ki_sam)
      denom1 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom2 = 0.0_ki
      R = real(-k2, ki_sam)
      denom3 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      R = real(-k3-k6-k5-k4, ki_sam)
      denom4 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
   case(10)
      nonzero(1) = .false.
      nonzero(2) = .false.
      denom1 = 0.0_ki
      denom2 = 0.0_ki
      R = real(-k2, ki_sam)
      denom3 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      R = real(-k3-k6-k5-k4, ki_sam)
      denom4 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
   case(2)
      nonzero(0) = .false.
      R = real(-k6, ki_sam)
      denom1 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom2 = Q2
      denom3 = 0.0_ki
      R = real(-k3-k6-k5-k4, ki_sam)
      denom4 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
   case(20)
      nonzero(0) = .false.
      denom1 = 0.0_ki
      denom2 = Q2
      denom3 = 0.0_ki
      R = real(-k3-k6-k5-k4, ki_sam)
      denom4 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
   case(21)
      nonzero(0) = .false.
      nonzero(1) = .false.
      nonzero(2) = .false.
      R = real(-k6, ki_sam)
      denom1 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom2 = 0.0_ki
      denom3 = 0.0_ki
      R = real(-k3-k6-k5-k4, ki_sam)
      denom4 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
   case(210)
      nonzero(0) = .false.
      nonzero(1) = .false.
      nonzero(2) = .false.
      denom1 = 0.0_ki
      denom2 = 0.0_ki
      denom3 = 0.0_ki
      R = real(-k3-k6-k5-k4, ki_sam)
      denom4 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
   case(3)
      nonzero(2) = .false.
      nonzero(4) = .false.
      R = real(-k6, ki_sam)
      denom1 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom2 = Q2
      R = real(-k2, ki_sam)
      denom3 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom4 = 0.0_ki
   case(30)
      nonzero(2) = .false.
      nonzero(4) = .false.
      denom1 = 0.0_ki
      denom2 = Q2
      R = real(-k2, ki_sam)
      denom3 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom4 = 0.0_ki
   case(31)
      nonzero(1) = .false.
      nonzero(2) = .false.
      nonzero(4) = .false.
      R = real(-k6, ki_sam)
      denom1 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom2 = 0.0_ki
      R = real(-k2, ki_sam)
      denom3 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom4 = 0.0_ki
   case(310)
      nonzero(1) = .false.
      nonzero(2) = .false.
      nonzero(4) = .false.
      denom1 = 0.0_ki
      denom2 = 0.0_ki
      R = real(-k2, ki_sam)
      denom3 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom4 = 0.0_ki
   case(32)
      nonzero(0) = .false.
      nonzero(2) = .false.
      nonzero(4) = .false.
      R = real(-k6, ki_sam)
      denom1 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom2 = Q2
      denom3 = 0.0_ki
      denom4 = 0.0_ki
   case(320)
      nonzero(0) = .false.
      nonzero(2) = .false.
      nonzero(4) = .false.
      denom1 = 0.0_ki
      denom2 = Q2
      denom3 = 0.0_ki
      denom4 = 0.0_ki
   case(321)
      nonzero(0) = .false.
      nonzero(1) = .false.
      nonzero(2) = .false.
      nonzero(4) = .false.
      R = real(-k6, ki_sam)
      denom1 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom2 = 0.0_ki
      denom3 = 0.0_ki
      denom4 = 0.0_ki
   case(3210)
      nonzero(0) = .false.
      nonzero(1) = .false.
      nonzero(2) = .false.
      nonzero(4) = .false.
      denom1 = 0.0_ki
      denom2 = 0.0_ki
      denom3 = 0.0_ki
      denom4 = 0.0_ki
   case default
      R = real(-k6, ki_sam)
      denom1 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom2 = Q2
      R = real(-k2, ki_sam)
      denom3 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      R = real(-k3-k6-k5-k4, ki_sam)
      denom4 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
   end select

   num = (0.0_ki_sam, 0.0_ki_sam)
   !-------#[ Diagram 117:
   if(nonzero(0)) then
      num = num + numerator_d117(icut, Q, mu2) * denom3
   end if
   !-------#] Diagram 117:
   !-------#[ Diagram 134:
   if(nonzero(1)) then
      num = num + numerator_d134(icut, Q, mu2) * denom2
   end if
   !-------#] Diagram 134:
   !-------#[ Diagram 170:
   if(nonzero(2)) then
      num = num + numerator_d170(icut, Q, mu2) * denom2 * denom4
   end if
   !-------#] Diagram 170:
   !-------#[ Diagram 205:
   if(nonzero(3)) then
      num = num + numerator_d205(icut, Q, mu2)
   end if
   !-------#] Diagram 205:
   !-------#[ Diagram 233:
   if(nonzero(4)) then
      num = num + numerator_d233(icut, Q, mu2) * denom4
   end if
   !-------#] Diagram 233:
end function numeval_group1
!-----#] function numeval_group1:
!-----#[ function numeval_group2:
function     numeval_group2(icut, Q, mu2) result(num)
   use p16_bbarb_hepemg_kinematics, only: k1, k2, k3, k4, k5, k6
   use p16_bbarb_hepemg_model
   use p16_bbarb_hepemg_d130h0l1, only: numerator_d130 => numerator_samurai
   use p16_bbarb_hepemg_d167h0l1, only: numerator_d167 => numerator_samurai
   use p16_bbarb_hepemg_d204h0l1, only: numerator_d204 => numerator_samurai
   use p16_bbarb_hepemg_d235h0l1, only: numerator_d235 => numerator_samurai
   implicit none
   integer, intent(in) :: icut
   complex(ki_sam), dimension(4), intent(in) :: Q
   complex(ki_sam), intent(in) :: mu2
   complex(ki_sam) :: num

   logical, dimension(0:4-1) :: nonzero
   real(ki_sam), dimension(0:3) :: R
   complex(ki_sam) :: Q2
   complex(ki_sam) ::denom1,denom2,denom3,denom4

   nonzero(:) = .true.
   Q2 = Q(4)*Q(4) - Q(1)*Q(1) - Q(2)*Q(2) - Q(3)*Q(3) - mu2

   select case(icut)
   case(1)
      nonzero(0) = .false.
      nonzero(1) = .false.
      R = real(-k3-k5-k4, ki_sam)
      denom1 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom2 = 0.0_ki
      R = real(-k2, ki_sam)
      denom3 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom4 = Q2
   case(10)
      nonzero(0) = .false.
      nonzero(1) = .false.
      denom1 = 0.0_ki
      denom2 = 0.0_ki
      R = real(-k2, ki_sam)
      denom3 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom4 = Q2
   case(21)
      nonzero(0) = .false.
      nonzero(1) = .false.
      R = real(-k3-k5-k4, ki_sam)
      denom1 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom2 = 0.0_ki
      denom3 = 0.0_ki
      denom4 = Q2
   case(210)
      nonzero(0) = .false.
      nonzero(1) = .false.
      denom1 = 0.0_ki
      denom2 = 0.0_ki
      denom3 = 0.0_ki
      denom4 = Q2
   case(3)
      nonzero(1) = .false.
      nonzero(3) = .false.
      R = real(-k3-k5-k4, ki_sam)
      denom1 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      R = real(-k3-k6-k5-k4, ki_sam)
      denom2 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      R = real(-k2, ki_sam)
      denom3 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom4 = 0.0_ki
   case(30)
      nonzero(1) = .false.
      nonzero(3) = .false.
      denom1 = 0.0_ki
      R = real(-k3-k6-k5-k4, ki_sam)
      denom2 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      R = real(-k2, ki_sam)
      denom3 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom4 = 0.0_ki
   case(31)
      nonzero(0) = .false.
      nonzero(1) = .false.
      nonzero(3) = .false.
      R = real(-k3-k5-k4, ki_sam)
      denom1 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom2 = 0.0_ki
      R = real(-k2, ki_sam)
      denom3 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom4 = 0.0_ki
   case(310)
      nonzero(0) = .false.
      nonzero(1) = .false.
      nonzero(3) = .false.
      denom1 = 0.0_ki
      denom2 = 0.0_ki
      R = real(-k2, ki_sam)
      denom3 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom4 = 0.0_ki
   case(32)
      nonzero(1) = .false.
      nonzero(3) = .false.
      R = real(-k3-k5-k4, ki_sam)
      denom1 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      R = real(-k3-k6-k5-k4, ki_sam)
      denom2 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom3 = 0.0_ki
      denom4 = 0.0_ki
   case(320)
      nonzero(1) = .false.
      nonzero(3) = .false.
      denom1 = 0.0_ki
      R = real(-k3-k6-k5-k4, ki_sam)
      denom2 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom3 = 0.0_ki
      denom4 = 0.0_ki
   case(321)
      nonzero(0) = .false.
      nonzero(1) = .false.
      nonzero(3) = .false.
      R = real(-k3-k5-k4, ki_sam)
      denom1 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom2 = 0.0_ki
      denom3 = 0.0_ki
      denom4 = 0.0_ki
   case(3210)
      nonzero(0) = .false.
      nonzero(1) = .false.
      nonzero(3) = .false.
      denom1 = 0.0_ki
      denom2 = 0.0_ki
      denom3 = 0.0_ki
      denom4 = 0.0_ki
   case default
      R = real(-k3-k5-k4, ki_sam)
      denom1 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      R = real(-k3-k6-k5-k4, ki_sam)
      denom2 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      R = real(-k2, ki_sam)
      denom3 = Q2 + (Q(4) + Q(4) + R(0))*R(0) &
                 &    - (Q(1) + Q(1) + R(1))*R(1) &
                 &    - (Q(2) + Q(2) + R(2))*R(2) &
                 &    - (Q(3) + Q(3) + R(3))*R(3)
      denom4 = Q2
   end select

   num = (0.0_ki_sam, 0.0_ki_sam)
   !-------#[ Diagram 130:
   if(nonzero(0)) then
      num = num + numerator_d130(icut, Q, mu2) * denom2
   end if
   !-------#] Diagram 130:
   !-------#[ Diagram 167:
   if(nonzero(1)) then
      num = num + numerator_d167(icut, Q, mu2) * denom2 * denom4
   end if
   !-------#] Diagram 167:
   !-------#[ Diagram 204:
   if(nonzero(2)) then
      num = num + numerator_d204(icut, Q, mu2)
   end if
   !-------#] Diagram 204:
   !-------#[ Diagram 235:
   if(nonzero(3)) then
      num = num + numerator_d235(icut, Q, mu2) * denom4
   end if
   !-------#] Diagram 235:
end function numeval_group2
!-----#] function numeval_group2:
!---#] grouped numerators for samurai:
!---#[ reduce groups with samurai:
!-----#[ subroutine reduce_group0:
subroutine     reduce_group0(scale2,tot,totr,ok)
   use msamurai, only: samurai, samurai_rm, samurai_cm
   use options, only: samurai_out => iout
   use madds, only: s_mat
   use p16_bbarb_hepemg_config, only: samurai_group_numerators, &
      & samurai_verbosity, samurai_istop, samurai_test, &
      & debug_nlo_diagrams, logfile
   use p16_bbarb_hepemg_kinematics
   use p16_bbarb_hepemg_model
   use p16_bbarb_hepemg_d206h0l1, only: numerator_diagram206 => numerator_samur&
   &ai
   use p16_bbarb_hepemg_globalsl1, only: epspow

   implicit none
   real(ki_sam), intent(in) :: scale2
   complex(ki_sam), dimension(-2:0), intent(out) :: tot
   complex(ki_sam), intent(out) :: totr
   logical, intent(out) :: ok

   complex(ki_sam), dimension(-2:0) :: acc
   complex(ki_sam) :: accr
   logical :: acc_ok

   integer :: istopm, istop0

   integer, parameter :: effective_group_rank = 3
   !-----------#[ invariants for samurai:
   complex(ki_sam), dimension(4, 4) :: g_mat
   !-----------#] initialize invariants:
   real(ki_sam), dimension(4) :: msq
   real(ki_sam), dimension(4,4) :: Vi

   if(samurai_test.eq.1 .or. samurai_test.eq.3) then
      istopm = 1
      istop0 = 1
   else
      istopm = samurai_istop
      istop0 = max(2,samurai_istop)
   end if
   msq(1) = 0.0_ki_sam
   Vi(1,:) = real(-k2((/2,3,4,1/)), ki_sam)
   msq(2) = 0.0_ki_sam
   Vi(2,:) = real(0, ki_sam)
   msq(3) = 0.0_ki_sam
   Vi(3,:) = real(-k6((/2,3,4,1/)), ki_sam)
   msq(4) = 0.0_ki_sam
   Vi(4,:) = real(k3((/2,3,4,1/))-k2((/2,3,4,1/))+k5((/2,3,4,1/))+k4((/2,3,4,1/&
   &)), ki_sam)
   !-----------#[ initialize invariants:
   g_mat(1, 1) = real(0.0_ki, ki_sam)
   g_mat(1, 2) = real(0.0_ki, ki_sam)
   g_mat(2, 1) = g_mat(1, 2)
   g_mat(1, 3) = real(-es12-es61+es345, ki_sam)
   g_mat(3, 1) = g_mat(1, 3)
   g_mat(1, 4) = real(es345, ki_sam)
   g_mat(4, 1) = g_mat(1, 4)
   g_mat(2, 2) = real(0.0_ki, ki_sam)
   g_mat(2, 3) = real(0.0_ki, ki_sam)
   g_mat(3, 2) = g_mat(2, 3)
   g_mat(2, 4) = real(es61, ki_sam)
   g_mat(4, 2) = g_mat(2, 4)
   g_mat(3, 3) = real(0.0_ki, ki_sam)
   g_mat(3, 4) = real(0.0_ki, ki_sam)
   g_mat(4, 3) = g_mat(3, 4)
   g_mat(4, 4) = real(0.0_ki, ki_sam)
   !-----------#] initialize invariants:

   if(samurai_group_numerators) then
      !------#[ reduce numerator numeval_group0:
      if(samurai_verbosity > 0) then
         write(samurai_out,*) "[golem-2.0] numeval_group0"
         write(samurai_out,*) "[golem-2.0] epspow=", epspow
      end if
      !-----------#[ initialize invariants:
      allocate(s_mat(4, 4))
      s_mat(:,:) = g_mat(:,:)
      !-----------#] initialize invariants:
      call samurai_rm(numeval_group0, tot, totr, Vi, msq, 4, &
         & effective_group_rank, istop0, scale2, ok, &
         & samurai_cache_flag_g0, samurai_cache_g0)
      !-----------#[ deallocate invariants:
      deallocate(s_mat)
      !-----------#] deallocate invariants:

      !------#] reduce numerator numeval_group0:
   else
      !------#[ sum over reduction of single diagrams:
         if(debug_nlo_diagrams) then
            write(logfile,*) "<diagram index='206'>"
         end if
         if(samurai_verbosity > 0) then
            write(samurai_out,*) "[golem-2.0] numerator_diagram206"
            write(samurai_out,*) "[golem-2.0] epspow=", epspow
         end if
         !-----------#[ initialize invariants:
         allocate(s_mat(4, 4))
         s_mat(:,:) = g_mat( (/1,2,3,4/), (/1,2,3,4/) )
         !-----------#] initialize invariants:
         call samurai_rm(numerator_diagram206, acc, accr, &
            & Vi((/1,2,3,4/),:), msq((/1,2,3,4/)), 4, &
            & 3, istop0, scale2, ok, &
            & samurai_cache_flag_d206, samurai_cache_d206)
         !-----------#[ deallocate invariants:
         deallocate(s_mat)
         !-----------#] deallocate invariants:
         if(debug_nlo_diagrams) then
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-finite' re='", +real(acc(0), ki), &
               & "' im='", aimag(acc(0)), "'/>"
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-single' re='", +real(acc(-1), ki), &
               & "' im='", aimag(acc(-1)), "'/>"
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-double' re='", +real(acc(-2), ki), &
               & "' im='", aimag(acc(-2)), "'/>"
            write(logfile,'(A32,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-rational' re='", +real(accr, ki), &
               & "' im='", aimag(accr), "'/>"
            if(ok) then
               write(logfile,'(A30)') "<flag name='ok' status='yes'/>"
            else
               write(logfile,'(A29)') "<flag name='ok' status='no'/>"
            end if
            write(logfile,*) "</diagram>"
         end if

         tot =  + acc
         totr =  + accr
      !------#] sum over reduction of single diagrams:
   end if
end subroutine reduce_group0
!-----#] subroutine reduce_group0:
!-----#[ subroutine reduce_group1:
subroutine     reduce_group1(scale2,tot,totr,ok)
   use msamurai, only: samurai, samurai_rm, samurai_cm
   use options, only: samurai_out => iout
   use madds, only: s_mat
   use p16_bbarb_hepemg_config, only: samurai_group_numerators, &
      & samurai_verbosity, samurai_istop, samurai_test, &
      & debug_nlo_diagrams, logfile
   use p16_bbarb_hepemg_kinematics
   use p16_bbarb_hepemg_model
   use p16_bbarb_hepemg_d117h0l1, only: numerator_diagram117 => numerator_samur&
   &ai
   use p16_bbarb_hepemg_d134h0l1, only: numerator_diagram134 => numerator_samur&
   &ai
   use p16_bbarb_hepemg_d170h0l1, only: numerator_diagram170 => numerator_samur&
   &ai
   use p16_bbarb_hepemg_d205h0l1, only: numerator_diagram205 => numerator_samur&
   &ai
   use p16_bbarb_hepemg_d233h0l1, only: numerator_diagram233 => numerator_samur&
   &ai
   use p16_bbarb_hepemg_globalsl1, only: epspow

   implicit none
   real(ki_sam), intent(in) :: scale2
   complex(ki_sam), dimension(-2:0), intent(out) :: tot
   complex(ki_sam), intent(out) :: totr
   logical, intent(out) :: ok

   complex(ki_sam), dimension(-2:0) :: acc
   complex(ki_sam) :: accr
   logical :: acc_ok

   integer :: istopm, istop0

   integer, parameter :: effective_group_rank = 4
   !-----------#[ invariants for samurai:
   complex(ki_sam), dimension(4, 4) :: g_mat
   !-----------#] initialize invariants:
   real(ki_sam), dimension(4) :: msq
   real(ki_sam), dimension(4,4) :: Vi

   if(samurai_test.eq.1 .or. samurai_test.eq.3) then
      istopm = 1
      istop0 = 1
   else
      istopm = samurai_istop
      istop0 = max(2,samurai_istop)
   end if
   msq(1) = 0.0_ki_sam
   Vi(1,:) = real(-k6((/2,3,4,1/)), ki_sam)
   msq(2) = 0.0_ki_sam
   Vi(2,:) = real(0, ki_sam)
   msq(3) = 0.0_ki_sam
   Vi(3,:) = real(-k2((/2,3,4,1/)), ki_sam)
   msq(4) = 0.0_ki_sam
   Vi(4,:) = real(-k3((/2,3,4,1/))-k6((/2,3,4,1/))-k5((/2,3,4,1/))-k4((/2,3,4,1&
   &/)), ki_sam)
   !-----------#[ initialize invariants:
   g_mat(1, 1) = real(0.0_ki, ki_sam)
   g_mat(1, 2) = real(0.0_ki, ki_sam)
   g_mat(2, 1) = g_mat(1, 2)
   g_mat(1, 3) = real(-es12-es61+es345, ki_sam)
   g_mat(3, 1) = g_mat(1, 3)
   g_mat(1, 4) = real(es345, ki_sam)
   g_mat(4, 1) = g_mat(1, 4)
   g_mat(2, 2) = real(0.0_ki, ki_sam)
   g_mat(2, 3) = real(0.0_ki, ki_sam)
   g_mat(3, 2) = g_mat(2, 3)
   g_mat(2, 4) = real(es12, ki_sam)
   g_mat(4, 2) = g_mat(2, 4)
   g_mat(3, 3) = real(0.0_ki, ki_sam)
   g_mat(3, 4) = real(0.0_ki, ki_sam)
   g_mat(4, 3) = g_mat(3, 4)
   g_mat(4, 4) = real(0.0_ki, ki_sam)
   !-----------#] initialize invariants:

   if(samurai_group_numerators) then
      !------#[ reduce numerator numeval_group1:
      if(samurai_verbosity > 0) then
         write(samurai_out,*) "[golem-2.0] numeval_group1"
         write(samurai_out,*) "[golem-2.0] epspow=", epspow
      end if
      !-----------#[ initialize invariants:
      allocate(s_mat(4, 4))
      s_mat(:,:) = g_mat(:,:)
      !-----------#] initialize invariants:
      call samurai_rm(numeval_group1, tot, totr, Vi, msq, 4, &
         & effective_group_rank, istop0, scale2, ok, &
         & samurai_cache_flag_g1, samurai_cache_g1)
      !-----------#[ deallocate invariants:
      deallocate(s_mat)
      !-----------#] deallocate invariants:

      !------#] reduce numerator numeval_group1:
   else
      !------#[ sum over reduction of single diagrams:
         if(debug_nlo_diagrams) then
            write(logfile,*) "<diagram index='117'>"
         end if
         if(samurai_verbosity > 0) then
            write(samurai_out,*) "[golem-2.0] numerator_diagram117"
            write(samurai_out,*) "[golem-2.0] epspow=", epspow
         end if
         !-----------#[ initialize invariants:
         allocate(s_mat(3, 3))
         s_mat(:,:) = g_mat( (/1,2,4/), (/1,2,4/) )
         !-----------#] initialize invariants:
         call samurai_rm(numerator_diagram117, acc, accr, &
            & Vi((/1,2,4/),:), msq((/1,2,4/)), 3, &
            & 3, istop0, scale2, ok, &
            & samurai_cache_flag_d117, samurai_cache_d117)
         !-----------#[ deallocate invariants:
         deallocate(s_mat)
         !-----------#] deallocate invariants:
         if(debug_nlo_diagrams) then
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-finite' re='", -real(acc(0), ki), &
               & "' im='", aimag(acc(0)), "'/>"
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-single' re='", -real(acc(-1), ki), &
               & "' im='", aimag(acc(-1)), "'/>"
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-double' re='", -real(acc(-2), ki), &
               & "' im='", aimag(acc(-2)), "'/>"
            write(logfile,'(A32,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-rational' re='", -real(accr, ki), &
               & "' im='", aimag(accr), "'/>"
            if(ok) then
               write(logfile,'(A30)') "<flag name='ok' status='yes'/>"
            else
               write(logfile,'(A29)') "<flag name='ok' status='no'/>"
            end if
            write(logfile,*) "</diagram>"
         end if

         tot =  +  acc
         totr =  +  accr
         if(debug_nlo_diagrams) then
            write(logfile,*) "<diagram index='134'>"
         end if
         if(samurai_verbosity > 0) then
            write(samurai_out,*) "[golem-2.0] numerator_diagram134"
            write(samurai_out,*) "[golem-2.0] epspow=", epspow
         end if
         !-----------#[ initialize invariants:
         allocate(s_mat(3, 3))
         s_mat(:,:) = g_mat( (/1,3,4/), (/1,3,4/) )
         !-----------#] initialize invariants:
         call samurai_rm(numerator_diagram134, acc, accr, &
            & Vi((/1,3,4/),:), msq((/1,3,4/)), 3, &
            & 2, istop0, scale2, acc_ok, &
            & samurai_cache_flag_d134, samurai_cache_d134)
         !-----------#[ deallocate invariants:
         deallocate(s_mat)
         !-----------#] deallocate invariants:
         if(debug_nlo_diagrams) then
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-finite' re='", +real(acc(0), ki), &
               & "' im='", aimag(acc(0)), "'/>"
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-single' re='", +real(acc(-1), ki), &
               & "' im='", aimag(acc(-1)), "'/>"
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-double' re='", +real(acc(-2), ki), &
               & "' im='", aimag(acc(-2)), "'/>"
            write(logfile,'(A32,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-rational' re='", +real(accr, ki), &
               & "' im='", aimag(accr), "'/>"
            if(ok) then
               write(logfile,'(A30)') "<flag name='ok' status='yes'/>"
            else
               write(logfile,'(A29)') "<flag name='ok' status='no'/>"
            end if
            write(logfile,*) "</diagram>"
         end if

         tot = tot  + acc
         totr = totr  + accr
         ok = ok .and. acc_ok
         if(debug_nlo_diagrams) then
            write(logfile,*) "<diagram index='170'>"
         end if
         if(samurai_verbosity > 0) then
            write(samurai_out,*) "[golem-2.0] numerator_diagram170"
            write(samurai_out,*) "[golem-2.0] epspow=", epspow
         end if
         !-----------#[ initialize invariants:
         allocate(s_mat(2, 2))
         s_mat(:,:) = g_mat( (/1,3/), (/1,3/) )
         !-----------#] initialize invariants:
         call samurai_rm(numerator_diagram170, acc, accr, &
            & Vi((/1,3/),:), msq((/1,3/)), 2, &
            & 1, istop0, scale2, acc_ok, &
            & samurai_cache_flag_d170, samurai_cache_d170)
         !-----------#[ deallocate invariants:
         deallocate(s_mat)
         !-----------#] deallocate invariants:
         if(debug_nlo_diagrams) then
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-finite' re='", +real(acc(0), ki), &
               & "' im='", aimag(acc(0)), "'/>"
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-single' re='", +real(acc(-1), ki), &
               & "' im='", aimag(acc(-1)), "'/>"
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-double' re='", +real(acc(-2), ki), &
               & "' im='", aimag(acc(-2)), "'/>"
            write(logfile,'(A32,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-rational' re='", +real(accr, ki), &
               & "' im='", aimag(accr), "'/>"
            if(ok) then
               write(logfile,'(A30)') "<flag name='ok' status='yes'/>"
            else
               write(logfile,'(A29)') "<flag name='ok' status='no'/>"
            end if
            write(logfile,*) "</diagram>"
         end if

         tot = tot  + acc
         totr = totr  + accr
         ok = ok .and. acc_ok
         if(debug_nlo_diagrams) then
            write(logfile,*) "<diagram index='205'>"
         end if
         if(samurai_verbosity > 0) then
            write(samurai_out,*) "[golem-2.0] numerator_diagram205"
            write(samurai_out,*) "[golem-2.0] epspow=", epspow
         end if
         !-----------#[ initialize invariants:
         allocate(s_mat(4, 4))
         s_mat(:,:) = g_mat( (/1,2,3,4/), (/1,2,3,4/) )
         !-----------#] initialize invariants:
         call samurai_rm(numerator_diagram205, acc, accr, &
            & Vi((/1,2,3,4/),:), msq((/1,2,3,4/)), 4, &
            & 3, istop0, scale2, acc_ok, &
            & samurai_cache_flag_d205, samurai_cache_d205)
         !-----------#[ deallocate invariants:
         deallocate(s_mat)
         !-----------#] deallocate invariants:
         if(debug_nlo_diagrams) then
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-finite' re='", +real(acc(0), ki), &
               & "' im='", aimag(acc(0)), "'/>"
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-single' re='", +real(acc(-1), ki), &
               & "' im='", aimag(acc(-1)), "'/>"
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-double' re='", +real(acc(-2), ki), &
               & "' im='", aimag(acc(-2)), "'/>"
            write(logfile,'(A32,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-rational' re='", +real(accr, ki), &
               & "' im='", aimag(accr), "'/>"
            if(ok) then
               write(logfile,'(A30)') "<flag name='ok' status='yes'/>"
            else
               write(logfile,'(A29)') "<flag name='ok' status='no'/>"
            end if
            write(logfile,*) "</diagram>"
         end if

         tot = tot  + acc
         totr = totr  + accr
         ok = ok .and. acc_ok
         if(debug_nlo_diagrams) then
            write(logfile,*) "<diagram index='233'>"
         end if
         if(samurai_verbosity > 0) then
            write(samurai_out,*) "[golem-2.0] numerator_diagram233"
            write(samurai_out,*) "[golem-2.0] epspow=", epspow
         end if
         !-----------#[ initialize invariants:
         allocate(s_mat(3, 3))
         s_mat(:,:) = g_mat( (/1,2,3/), (/1,2,3/) )
         !-----------#] initialize invariants:
         call samurai_rm(numerator_diagram233, acc, accr, &
            & Vi((/1,2,3/),:), msq((/1,2,3/)), 3, &
            & 2, istop0, scale2, acc_ok, &
            & samurai_cache_flag_d233, samurai_cache_d233)
         !-----------#[ deallocate invariants:
         deallocate(s_mat)
         !-----------#] deallocate invariants:
         if(debug_nlo_diagrams) then
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-finite' re='", +real(acc(0), ki), &
               & "' im='", aimag(acc(0)), "'/>"
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-single' re='", +real(acc(-1), ki), &
               & "' im='", aimag(acc(-1)), "'/>"
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-double' re='", +real(acc(-2), ki), &
               & "' im='", aimag(acc(-2)), "'/>"
            write(logfile,'(A32,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-rational' re='", +real(accr, ki), &
               & "' im='", aimag(accr), "'/>"
            if(ok) then
               write(logfile,'(A30)') "<flag name='ok' status='yes'/>"
            else
               write(logfile,'(A29)') "<flag name='ok' status='no'/>"
            end if
            write(logfile,*) "</diagram>"
         end if

         tot = tot  + acc
         totr = totr  + accr
         ok = ok .and. acc_ok
      !------#] sum over reduction of single diagrams:
   end if
end subroutine reduce_group1
!-----#] subroutine reduce_group1:
!-----#[ subroutine reduce_group2:
subroutine     reduce_group2(scale2,tot,totr,ok)
   use msamurai, only: samurai, samurai_rm, samurai_cm
   use options, only: samurai_out => iout
   use madds, only: s_mat
   use p16_bbarb_hepemg_config, only: samurai_group_numerators, &
      & samurai_verbosity, samurai_istop, samurai_test, &
      & debug_nlo_diagrams, logfile
   use p16_bbarb_hepemg_kinematics
   use p16_bbarb_hepemg_model
   use p16_bbarb_hepemg_d130h0l1, only: numerator_diagram130 => numerator_samur&
   &ai
   use p16_bbarb_hepemg_d167h0l1, only: numerator_diagram167 => numerator_samur&
   &ai
   use p16_bbarb_hepemg_d204h0l1, only: numerator_diagram204 => numerator_samur&
   &ai
   use p16_bbarb_hepemg_d235h0l1, only: numerator_diagram235 => numerator_samur&
   &ai
   use p16_bbarb_hepemg_globalsl1, only: epspow

   implicit none
   real(ki_sam), intent(in) :: scale2
   complex(ki_sam), dimension(-2:0), intent(out) :: tot
   complex(ki_sam), intent(out) :: totr
   logical, intent(out) :: ok

   complex(ki_sam), dimension(-2:0) :: acc
   complex(ki_sam) :: accr
   logical :: acc_ok

   integer :: istopm, istop0

   integer, parameter :: effective_group_rank = 3
   !-----------#[ invariants for samurai:
   complex(ki_sam), dimension(4, 4) :: g_mat
   !-----------#] initialize invariants:
   real(ki_sam), dimension(4) :: msq
   real(ki_sam), dimension(4,4) :: Vi

   if(samurai_test.eq.1 .or. samurai_test.eq.3) then
      istopm = 1
      istop0 = 1
   else
      istopm = samurai_istop
      istop0 = max(2,samurai_istop)
   end if
   msq(1) = 0.0_ki_sam
   Vi(1,:) = real(-k3((/2,3,4,1/))-k5((/2,3,4,1/))-k4((/2,3,4,1/)), ki_sam)
   msq(2) = 0.0_ki_sam
   Vi(2,:) = real(-k3((/2,3,4,1/))-k6((/2,3,4,1/))-k5((/2,3,4,1/))-k4((/2,3,4,1&
   &/)), ki_sam)
   msq(3) = 0.0_ki_sam
   Vi(3,:) = real(-k2((/2,3,4,1/)), ki_sam)
   msq(4) = 0.0_ki_sam
   Vi(4,:) = real(0, ki_sam)
   !-----------#[ initialize invariants:
   g_mat(1, 1) = real(0.0_ki, ki_sam)
   g_mat(1, 2) = real(0.0_ki, ki_sam)
   g_mat(2, 1) = g_mat(1, 2)
   g_mat(1, 3) = real(es61, ki_sam)
   g_mat(3, 1) = g_mat(1, 3)
   g_mat(1, 4) = real(es345, ki_sam)
   g_mat(4, 1) = g_mat(1, 4)
   g_mat(2, 2) = real(0.0_ki, ki_sam)
   g_mat(2, 3) = real(0.0_ki, ki_sam)
   g_mat(3, 2) = g_mat(2, 3)
   g_mat(2, 4) = real(es12, ki_sam)
   g_mat(4, 2) = g_mat(2, 4)
   g_mat(3, 3) = real(0.0_ki, ki_sam)
   g_mat(3, 4) = real(0.0_ki, ki_sam)
   g_mat(4, 3) = g_mat(3, 4)
   g_mat(4, 4) = real(0.0_ki, ki_sam)
   !-----------#] initialize invariants:

   if(samurai_group_numerators) then
      !------#[ reduce numerator numeval_group2:
      if(samurai_verbosity > 0) then
         write(samurai_out,*) "[golem-2.0] numeval_group2"
         write(samurai_out,*) "[golem-2.0] epspow=", epspow
      end if
      !-----------#[ initialize invariants:
      allocate(s_mat(4, 4))
      s_mat(:,:) = g_mat(:,:)
      !-----------#] initialize invariants:
      call samurai_rm(numeval_group2, tot, totr, Vi, msq, 4, &
         & effective_group_rank, istop0, scale2, ok, &
         & samurai_cache_flag_g2, samurai_cache_g2)
      !-----------#[ deallocate invariants:
      deallocate(s_mat)
      !-----------#] deallocate invariants:

      !------#] reduce numerator numeval_group2:
   else
      !------#[ sum over reduction of single diagrams:
         if(debug_nlo_diagrams) then
            write(logfile,*) "<diagram index='130'>"
         end if
         if(samurai_verbosity > 0) then
            write(samurai_out,*) "[golem-2.0] numerator_diagram130"
            write(samurai_out,*) "[golem-2.0] epspow=", epspow
         end if
         !-----------#[ initialize invariants:
         allocate(s_mat(3, 3))
         s_mat(:,:) = g_mat( (/1,3,4/), (/1,3,4/) )
         !-----------#] initialize invariants:
         call samurai_rm(numerator_diagram130, acc, accr, &
            & Vi((/1,3,4/),:), msq((/1,3,4/)), 3, &
            & 2, istop0, scale2, ok, &
            & samurai_cache_flag_d130, samurai_cache_d130)
         !-----------#[ deallocate invariants:
         deallocate(s_mat)
         !-----------#] deallocate invariants:
         if(debug_nlo_diagrams) then
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-finite' re='", +real(acc(0), ki), &
               & "' im='", aimag(acc(0)), "'/>"
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-single' re='", +real(acc(-1), ki), &
               & "' im='", aimag(acc(-1)), "'/>"
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-double' re='", +real(acc(-2), ki), &
               & "' im='", aimag(acc(-2)), "'/>"
            write(logfile,'(A32,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-rational' re='", +real(accr, ki), &
               & "' im='", aimag(accr), "'/>"
            if(ok) then
               write(logfile,'(A30)') "<flag name='ok' status='yes'/>"
            else
               write(logfile,'(A29)') "<flag name='ok' status='no'/>"
            end if
            write(logfile,*) "</diagram>"
         end if

         tot =  + acc
         totr =  + accr
         if(debug_nlo_diagrams) then
            write(logfile,*) "<diagram index='167'>"
         end if
         if(samurai_verbosity > 0) then
            write(samurai_out,*) "[golem-2.0] numerator_diagram167"
            write(samurai_out,*) "[golem-2.0] epspow=", epspow
         end if
         !-----------#[ initialize invariants:
         allocate(s_mat(2, 2))
         s_mat(:,:) = g_mat( (/1,3/), (/1,3/) )
         !-----------#] initialize invariants:
         call samurai_rm(numerator_diagram167, acc, accr, &
            & Vi((/1,3/),:), msq((/1,3/)), 2, &
            & 1, istop0, scale2, acc_ok, &
            & samurai_cache_flag_d167, samurai_cache_d167)
         !-----------#[ deallocate invariants:
         deallocate(s_mat)
         !-----------#] deallocate invariants:
         if(debug_nlo_diagrams) then
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-finite' re='", +real(acc(0), ki), &
               & "' im='", aimag(acc(0)), "'/>"
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-single' re='", +real(acc(-1), ki), &
               & "' im='", aimag(acc(-1)), "'/>"
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-double' re='", +real(acc(-2), ki), &
               & "' im='", aimag(acc(-2)), "'/>"
            write(logfile,'(A32,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-rational' re='", +real(accr, ki), &
               & "' im='", aimag(accr), "'/>"
            if(ok) then
               write(logfile,'(A30)') "<flag name='ok' status='yes'/>"
            else
               write(logfile,'(A29)') "<flag name='ok' status='no'/>"
            end if
            write(logfile,*) "</diagram>"
         end if

         tot = tot  + acc
         totr = totr  + accr
         ok = ok .and. acc_ok
         if(debug_nlo_diagrams) then
            write(logfile,*) "<diagram index='204'>"
         end if
         if(samurai_verbosity > 0) then
            write(samurai_out,*) "[golem-2.0] numerator_diagram204"
            write(samurai_out,*) "[golem-2.0] epspow=", epspow
         end if
         !-----------#[ initialize invariants:
         allocate(s_mat(4, 4))
         s_mat(:,:) = g_mat( (/1,2,3,4/), (/1,2,3,4/) )
         !-----------#] initialize invariants:
         call samurai_rm(numerator_diagram204, acc, accr, &
            & Vi((/1,2,3,4/),:), msq((/1,2,3,4/)), 4, &
            & 3, istop0, scale2, acc_ok, &
            & samurai_cache_flag_d204, samurai_cache_d204)
         !-----------#[ deallocate invariants:
         deallocate(s_mat)
         !-----------#] deallocate invariants:
         if(debug_nlo_diagrams) then
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-finite' re='", +real(acc(0), ki), &
               & "' im='", aimag(acc(0)), "'/>"
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-single' re='", +real(acc(-1), ki), &
               & "' im='", aimag(acc(-1)), "'/>"
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-double' re='", +real(acc(-2), ki), &
               & "' im='", aimag(acc(-2)), "'/>"
            write(logfile,'(A32,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-rational' re='", +real(accr, ki), &
               & "' im='", aimag(accr), "'/>"
            if(ok) then
               write(logfile,'(A30)') "<flag name='ok' status='yes'/>"
            else
               write(logfile,'(A29)') "<flag name='ok' status='no'/>"
            end if
            write(logfile,*) "</diagram>"
         end if

         tot = tot  + acc
         totr = totr  + accr
         ok = ok .and. acc_ok
         if(debug_nlo_diagrams) then
            write(logfile,*) "<diagram index='235'>"
         end if
         if(samurai_verbosity > 0) then
            write(samurai_out,*) "[golem-2.0] numerator_diagram235"
            write(samurai_out,*) "[golem-2.0] epspow=", epspow
         end if
         !-----------#[ initialize invariants:
         allocate(s_mat(3, 3))
         s_mat(:,:) = g_mat( (/1,2,3/), (/1,2,3/) )
         !-----------#] initialize invariants:
         call samurai_rm(numerator_diagram235, acc, accr, &
            & Vi((/1,2,3/),:), msq((/1,2,3/)), 3, &
            & 2, istop0, scale2, acc_ok, &
            & samurai_cache_flag_d235, samurai_cache_d235)
         !-----------#[ deallocate invariants:
         deallocate(s_mat)
         !-----------#] deallocate invariants:
         if(debug_nlo_diagrams) then
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-finite' re='", +real(acc(0), ki), &
               & "' im='", aimag(acc(0)), "'/>"
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-single' re='", +real(acc(-1), ki), &
               & "' im='", aimag(acc(-1)), "'/>"
            write(logfile,'(A30,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-double' re='", +real(acc(-2), ki), &
               & "' im='", aimag(acc(-2)), "'/>"
            write(logfile,'(A32,E24.16,A6,E24.16,A3)') &
               & "<result kind='nlo-rational' re='", +real(accr, ki), &
               & "' im='", aimag(accr), "'/>"
            if(ok) then
               write(logfile,'(A30)') "<flag name='ok' status='yes'/>"
            else
               write(logfile,'(A29)') "<flag name='ok' status='no'/>"
            end if
            write(logfile,*) "</diagram>"
         end if

         tot = tot  + acc
         totr = totr  + accr
         ok = ok .and. acc_ok
      !------#] sum over reduction of single diagrams:
   end if
end subroutine reduce_group2
!-----#] subroutine reduce_group2:
!---#] reduce groups with samurai:
end module p16_bbarb_hepemg_samuraih0
