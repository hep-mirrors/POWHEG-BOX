module     p4_ubaru_hepemg_abbrevd413h0
   use p4_ubaru_hepemg_config, only: ki
   use p4_ubaru_hepemg_globalsh0
   implicit none
   private
   complex(ki), dimension(16), public :: abb413
   complex(ki), public :: R2d413
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
      abb413(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb413(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb413(3)=NC**(-1)
      abb413(4)=es61**(-1)
      abb413(5)=gHZZ*spak1e6*c1*i_*TR*gUl*gel*abb413(2)*abb413(1)
      abb413(6)=abb413(5)*spbk6e6
      abb413(7)=abb413(6)*abb413(3)
      abb413(8)=abb413(5)*NC
      abb413(9)=-spbk6e6*abb413(8)
      abb413(7)=abb413(7)+abb413(9)
      abb413(7)=spak5k6*abb413(7)
      abb413(9)=abb413(8)*spbe6k1
      abb413(10)=abb413(5)*spbe6k1
      abb413(11)=abb413(10)*abb413(3)
      abb413(9)=-abb413(11)+abb413(9)
      abb413(9)=spak1k5*abb413(9)
      abb413(7)=abb413(7)+abb413(9)
      abb413(7)=2.0_ki*abb413(7)*spbk4k2*abb413(4)
      abb413(9)=abb413(3)-NC
      abb413(11)=abb413(4)**2
      abb413(5)=abb413(9)*abb413(5)*abb413(11)*spbk4k2
      abb413(9)=-spbk6e6*abb413(5)
      abb413(12)=-spak5k6*abb413(9)
      abb413(5)=-spbe6k1*abb413(5)
      abb413(13)=2.0_ki*spak1k5
      abb413(14)=abb413(5)*abb413(13)
      abb413(12)=abb413(12)+abb413(14)
      abb413(12)=4.0_ki*abb413(12)
      abb413(14)=2.0_ki*spak5k6
      abb413(15)=abb413(9)*abb413(14)
      abb413(16)=-spak1k5*abb413(5)
      abb413(15)=abb413(15)+abb413(16)
      abb413(15)=4.0_ki*abb413(15)
      abb413(9)=abb413(9)*abb413(13)
      abb413(8)=abb413(8)*abb413(11)
      abb413(13)=abb413(8)*spbe6k1
      abb413(11)=abb413(11)*abb413(3)
      abb413(10)=abb413(11)*abb413(10)
      abb413(10)=abb413(13)-abb413(10)
      abb413(13)=-spbk6k2*abb413(10)
      abb413(8)=abb413(8)*spbk6e6
      abb413(6)=abb413(11)*abb413(6)
      abb413(6)=abb413(8)-abb413(6)
      abb413(8)=spbk2k1*abb413(6)
      abb413(8)=abb413(13)+abb413(8)
      abb413(11)=4.0_ki*spak1k6
      abb413(8)=abb413(8)*abb413(11)
      abb413(10)=spbk6k4*abb413(10)
      abb413(6)=-spbk4k1*abb413(6)
      abb413(6)=abb413(10)+abb413(6)
      abb413(6)=abb413(6)*abb413(11)
      abb413(5)=abb413(5)*abb413(14)
      R2d413=0.0_ki
      rat2 = rat2 + R2d413
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='413' value='", &
          & R2d413, "'/>"
      end if
   end subroutine
end module p4_ubaru_hepemg_abbrevd413h0
