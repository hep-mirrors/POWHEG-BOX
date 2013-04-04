module     p0_dbard_hepemg_abbrevd150h0
   use p0_dbard_hepemg_config, only: ki
   use p0_dbard_hepemg_globalsh0
   implicit none
   private
   complex(ki), dimension(19), public :: abb150
   complex(ki), public :: R2d150
   public :: init_abbrev
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
contains
   subroutine     init_abbrev()
      use p0_dbard_hepemg_config, only: deltaOS, &
     &    logfile, debug_nlo_diagrams
      use p0_dbard_hepemg_kinematics
      use p0_dbard_hepemg_model
      use p0_dbard_hepemg_color, only: TR
      use p0_dbard_hepemg_globalsl1, only: epspow
      implicit none
      abb150(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb150(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb150(3)=NC**(-1)
      abb150(4)=es61**(-1)
      abb150(5)=abb150(3)-NC
      abb150(6)=-spbk6e6*spak5k6*abb150(5)
      abb150(7)=-spbe6k1*abb150(5)
      abb150(8)=-spak1k5*abb150(7)
      abb150(9)=abb150(6)+abb150(8)
      abb150(10)=abb150(4)*spbk4k2
      abb150(11)=gHZZ*gDl*gel*abb150(1)*abb150(2)*spak1e6*c1*TR*i_
      abb150(12)=abb150(10)*abb150(11)
      abb150(13)=-abb150(9)*abb150(12)
      abb150(14)=spak1k5*es12
      abb150(15)=spbk6k2*spak5k6*spak1k2
      abb150(14)=abb150(14)-abb150(15)
      abb150(14)=abb150(14)*abb150(7)
      abb150(15)=es12-es345
      abb150(15)=abb150(15)*spak5k6
      abb150(16)=spbk2k1*spak1k5*spak2k6
      abb150(15)=abb150(15)-abb150(16)
      abb150(5)=abb150(5)*spbk6e6
      abb150(15)=-abb150(15)*abb150(5)
      abb150(14)=abb150(15)+abb150(14)
      abb150(10)=abb150(14)*abb150(10)
      abb150(14)=spbk4k2*abb150(6)
      abb150(10)=abb150(14)+abb150(10)
      abb150(10)=2.0_ki*abb150(10)*abb150(11)
      abb150(12)=4.0_ki*abb150(12)
      abb150(6)=-abb150(6)*abb150(12)
      abb150(8)=abb150(8)*abb150(12)
      abb150(12)=2.0_ki*abb150(4)
      abb150(11)=abb150(12)*abb150(11)
      abb150(12)=abb150(11)*spbk4k2
      abb150(14)=abb150(9)*abb150(12)
      abb150(15)=-abb150(11)*spbk6k4*abb150(9)
      abb150(16)=abb150(12)*spak1k5*abb150(5)
      abb150(17)=-abb150(11)*abb150(5)
      abb150(18)=-spbk6k4*abb150(7)
      abb150(19)=-spbk4k1*abb150(5)
      abb150(18)=abb150(19)+abb150(18)
      abb150(18)=spak1k6*abb150(18)
      abb150(19)=spak1k2*abb150(7)
      abb150(5)=spak2k6*abb150(5)
      abb150(5)=abb150(5)+abb150(19)
      abb150(5)=spbk4k2*abb150(5)
      abb150(5)=abb150(5)+abb150(18)
      abb150(5)=abb150(5)*abb150(11)
      abb150(12)=-abb150(12)*spak5k6*abb150(7)
      abb150(9)=-abb150(11)*spbk4k1*abb150(9)
      abb150(7)=abb150(7)*abb150(11)
      R2d150=abb150(13)
      rat2 = rat2 + R2d150
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='150' value='", &
          & R2d150, "'/>"
      end if
   end subroutine
end module p0_dbard_hepemg_abbrevd150h0
