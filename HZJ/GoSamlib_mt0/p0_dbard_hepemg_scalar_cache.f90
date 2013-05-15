module     p0_dbard_hepemg_scalar_cache
   use precision, only: ki_sam => ki
   use madds
   implicit none
   save

   private
!---#[ scalar integral cache for samurai:
    logical, public  :: samurai_cache_flag_g0
    complex(ki_sam), dimension(-2:0,cachedim4(1)), public :: samurai_cache_g0
    logical, public :: samurai_cache_flag_d235
    complex(ki_sam), dimension(-2:0,cachedim4(1)), public :: samurai_cache_d235
    logical, public  :: samurai_cache_flag_g1
    complex(ki_sam), dimension(-2:0,cachedim4(1)), public :: samurai_cache_g1
    logical, public :: samurai_cache_flag_d145
    complex(ki_sam), dimension(-2:0,cachedim3(1)), public :: samurai_cache_d145
    logical, public :: samurai_cache_flag_d154
    complex(ki_sam), dimension(-2:0,cachedim3(1)), public :: samurai_cache_d154
    logical, public :: samurai_cache_flag_d196
    complex(ki_sam), dimension(-2:0,cachedim2(1)), public :: samurai_cache_d196
    logical, public :: samurai_cache_flag_d234
    complex(ki_sam), dimension(-2:0,cachedim4(1)), public :: samurai_cache_d234
    logical, public :: samurai_cache_flag_d266
    complex(ki_sam), dimension(-2:0,cachedim3(1)), public :: samurai_cache_d266
    logical, public  :: samurai_cache_flag_g2
    complex(ki_sam), dimension(-2:0,cachedim4(1)), public :: samurai_cache_g2
    logical, public :: samurai_cache_flag_d150
    complex(ki_sam), dimension(-2:0,cachedim3(1)), public :: samurai_cache_d150
    logical, public :: samurai_cache_flag_d193
    complex(ki_sam), dimension(-2:0,cachedim2(1)), public :: samurai_cache_d193
    logical, public :: samurai_cache_flag_d233
    complex(ki_sam), dimension(-2:0,cachedim4(1)), public :: samurai_cache_d233
    logical, public :: samurai_cache_flag_d268
    complex(ki_sam), dimension(-2:0,cachedim3(1)), public :: samurai_cache_d268
    logical, public  :: samurai_cache_flag_g3
    complex(ki_sam), dimension(-2:0,cachedim4(1)), public :: samurai_cache_g3
    logical, public :: samurai_cache_flag_d55
    complex(ki_sam), dimension(-2:0,cachedim4(1)), public :: samurai_cache_d55
    logical, public :: samurai_cache_flag_d244
    complex(ki_sam), dimension(-2:0,cachedim3(1)), public :: samurai_cache_d244
    logical, public  :: samurai_cache_flag_g4
    complex(ki_sam), dimension(-2:0,cachedim4(1)), public :: samurai_cache_g4
    logical, public :: samurai_cache_flag_d43
    complex(ki_sam), dimension(-2:0,cachedim4(1)), public :: samurai_cache_d43
    logical, public  :: samurai_cache_flag_g5
    complex(ki_sam), dimension(-2:0,cachedim4(1)), public :: samurai_cache_g5
    logical, public :: samurai_cache_flag_d27
    complex(ki_sam), dimension(-2:0,cachedim4(1)), public :: samurai_cache_d27
    logical, public :: samurai_cache_flag_d143
    complex(ki_sam), dimension(-2:0,cachedim3(1)), public :: samurai_cache_d143
!---#] scalar integral cache for samurai:

   public :: invalidate_cache
contains
   subroutine invalidate_cache()
      implicit none
      samurai_cache_flag_g0 = .false.
      samurai_cache_flag_d235 = .false.
      samurai_cache_flag_g1 = .false.
      samurai_cache_flag_d145 = .false.
      samurai_cache_flag_d154 = .false.
      samurai_cache_flag_d196 = .false.
      samurai_cache_flag_d234 = .false.
      samurai_cache_flag_d266 = .false.
      samurai_cache_flag_g2 = .false.
      samurai_cache_flag_d150 = .false.
      samurai_cache_flag_d193 = .false.
      samurai_cache_flag_d233 = .false.
      samurai_cache_flag_d268 = .false.
      samurai_cache_flag_g3 = .false.
      samurai_cache_flag_d55 = .false.
      samurai_cache_flag_d244 = .false.
      samurai_cache_flag_g4 = .false.
      samurai_cache_flag_d43 = .false.
      samurai_cache_flag_g5 = .false.
      samurai_cache_flag_d27 = .false.
      samurai_cache_flag_d143 = .false.
   end subroutine invalidate_cache
end module p0_dbard_hepemg_scalar_cache