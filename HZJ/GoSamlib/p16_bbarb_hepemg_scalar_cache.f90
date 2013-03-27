module     p16_bbarb_hepemg_scalar_cache
   use precision, only: ki_sam => ki
   use madds
   implicit none
   save

   private
!---#[ scalar integral cache for samurai:
    logical, public  :: samurai_cache_flag_g0
    complex(ki_sam), dimension(-2:0,cachedim4(1)), public :: samurai_cache_g0
    logical, public :: samurai_cache_flag_d206
    complex(ki_sam), dimension(-2:0,cachedim4(1)), public :: samurai_cache_d206
    logical, public  :: samurai_cache_flag_g1
    complex(ki_sam), dimension(-2:0,cachedim4(1)), public :: samurai_cache_g1
    logical, public :: samurai_cache_flag_d117
    complex(ki_sam), dimension(-2:0,cachedim3(1)), public :: samurai_cache_d117
    logical, public :: samurai_cache_flag_d134
    complex(ki_sam), dimension(-2:0,cachedim3(1)), public :: samurai_cache_d134
    logical, public :: samurai_cache_flag_d170
    complex(ki_sam), dimension(-2:0,cachedim2(1)), public :: samurai_cache_d170
    logical, public :: samurai_cache_flag_d205
    complex(ki_sam), dimension(-2:0,cachedim4(1)), public :: samurai_cache_d205
    logical, public :: samurai_cache_flag_d233
    complex(ki_sam), dimension(-2:0,cachedim3(1)), public :: samurai_cache_d233
    logical, public  :: samurai_cache_flag_g2
    complex(ki_sam), dimension(-2:0,cachedim4(1)), public :: samurai_cache_g2
    logical, public :: samurai_cache_flag_d130
    complex(ki_sam), dimension(-2:0,cachedim3(1)), public :: samurai_cache_d130
    logical, public :: samurai_cache_flag_d167
    complex(ki_sam), dimension(-2:0,cachedim2(1)), public :: samurai_cache_d167
    logical, public :: samurai_cache_flag_d204
    complex(ki_sam), dimension(-2:0,cachedim4(1)), public :: samurai_cache_d204
    logical, public :: samurai_cache_flag_d235
    complex(ki_sam), dimension(-2:0,cachedim3(1)), public :: samurai_cache_d235
!---#] scalar integral cache for samurai:

   public :: invalidate_cache
contains
   subroutine invalidate_cache()
      implicit none
      samurai_cache_flag_g0 = .false.
      samurai_cache_flag_d206 = .false.
      samurai_cache_flag_g1 = .false.
      samurai_cache_flag_d117 = .false.
      samurai_cache_flag_d134 = .false.
      samurai_cache_flag_d170 = .false.
      samurai_cache_flag_d205 = .false.
      samurai_cache_flag_d233 = .false.
      samurai_cache_flag_g2 = .false.
      samurai_cache_flag_d130 = .false.
      samurai_cache_flag_d167 = .false.
      samurai_cache_flag_d204 = .false.
      samurai_cache_flag_d235 = .false.
   end subroutine invalidate_cache
end module p16_bbarb_hepemg_scalar_cache