module     p4_ubaru_hepemg_abbrevd167h2
   use p4_ubaru_hepemg_config, only: ki
   use p4_ubaru_hepemg_globalsh2
   implicit none
   private
   complex(ki), dimension(16), public :: abb167
   complex(ki), public :: R2d167
   public :: init_abbrev
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
contains
   subroutine     init_abbrev()
      use p4_ubaru_hepemg_config, only: deltaOS, &
     &    logfile, debug_nlo_diagrams
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_model
      use p4_ubaru_hepemg_color, only: TR
      use p4_ubaru_hepemg_globalsl1, only: epspow
      implicit none
      abb167(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb167(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb167(3)=NC**(-1)
      abb167(4)=es61**(-1)
      abb167(5)=gHZZ*spak1e6*c1*i_*TR*gUl*ger*abb167(2)*abb167(1)
      abb167(6)=abb167(5)*spbk6e6
      abb167(7)=abb167(6)*abb167(3)
      abb167(8)=abb167(5)*NC
      abb167(9)=-spbk6e6*abb167(8)
      abb167(7)=abb167(7)+abb167(9)
      abb167(7)=spak4k6*abb167(7)
      abb167(9)=abb167(8)*spbe6k1
      abb167(10)=abb167(5)*spbe6k1
      abb167(11)=abb167(10)*abb167(3)
      abb167(9)=-abb167(11)+abb167(9)
      abb167(9)=spak1k4*abb167(9)
      abb167(7)=abb167(7)+abb167(9)
      abb167(7)=2.0_ki*abb167(7)*spbk5k2*abb167(4)
      abb167(9)=abb167(3)-NC
      abb167(11)=abb167(4)**2
      abb167(5)=abb167(9)*abb167(5)*abb167(11)*spbk5k2
      abb167(9)=-spbk6e6*abb167(5)
      abb167(12)=-spak4k6*abb167(9)
      abb167(5)=-spbe6k1*abb167(5)
      abb167(13)=2.0_ki*spak1k4
      abb167(14)=abb167(5)*abb167(13)
      abb167(12)=abb167(12)+abb167(14)
      abb167(12)=4.0_ki*abb167(12)
      abb167(14)=2.0_ki*spak4k6
      abb167(15)=abb167(9)*abb167(14)
      abb167(16)=-spak1k4*abb167(5)
      abb167(15)=abb167(15)+abb167(16)
      abb167(15)=4.0_ki*abb167(15)
      abb167(9)=abb167(9)*abb167(13)
      abb167(8)=abb167(8)*abb167(11)
      abb167(13)=abb167(8)*spbe6k1
      abb167(11)=abb167(11)*abb167(3)
      abb167(10)=abb167(11)*abb167(10)
      abb167(10)=abb167(13)-abb167(10)
      abb167(13)=-spbk6k2*abb167(10)
      abb167(8)=abb167(8)*spbk6e6
      abb167(6)=abb167(11)*abb167(6)
      abb167(6)=abb167(8)-abb167(6)
      abb167(8)=spbk2k1*abb167(6)
      abb167(8)=abb167(13)+abb167(8)
      abb167(11)=4.0_ki*spak1k6
      abb167(8)=abb167(8)*abb167(11)
      abb167(10)=spbk6k5*abb167(10)
      abb167(6)=-spbk5k1*abb167(6)
      abb167(6)=abb167(10)+abb167(6)
      abb167(6)=abb167(6)*abb167(11)
      abb167(5)=abb167(5)*abb167(14)
      R2d167=0.0_ki
      rat2 = rat2 + R2d167
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='167' value='", &
          & R2d167, "'/>"
      end if
   end subroutine
end module p4_ubaru_hepemg_abbrevd167h2
