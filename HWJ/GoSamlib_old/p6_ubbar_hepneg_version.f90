module     p6_ubbar_hepneg_version
   implicit none
   ! The version of Form used for code generation
   integer, parameter, dimension(2) :: formversion = (/3, 3/)
   ! The version of haggies used for code generation
   integer, parameter, dimension(2) :: haggiesversion = (/1, 1/)
   ! The version of GoSam used for code generation
   integer, parameter, dimension(2) :: gosamversion = (/1, 0/)
   ! The SVN revision of GoSam used for code generation
   integer, parameter :: gosamrevision = 139
end module p6_ubbar_hepneg_version
