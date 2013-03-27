module     p16_bbarb_hepemg_abbrevd130h1
   use p16_bbarb_hepemg_config, only: ki
   use p16_bbarb_hepemg_globalsh1
   implicit none
   private
   complex(ki), dimension(20), public :: abb130
   complex(ki), public :: R2d130
   public :: init_abbrev
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
contains
   subroutine     init_abbrev()
      use p16_bbarb_hepemg_config, only: deltaOS, &
     &    logfile, debug_nlo_diagrams
      use p16_bbarb_hepemg_kinematics
      use p16_bbarb_hepemg_model
      use p16_bbarb_hepemg_color, only: TR
      use p16_bbarb_hepemg_globalsl1, only: epspow
      implicit none
      abb130(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb130(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb130(3)=NC**(-1)
      abb130(4)=es61**(-1)
      abb130(5)=abb130(3)-NC
      abb130(6)=-spae6k6*spbk6k4*abb130(5)
      abb130(7)=-spak1e6*abb130(5)
      abb130(8)=-spbk4k1*abb130(7)
      abb130(9)=abb130(6)+abb130(8)
      abb130(10)=abb130(4)*spak2k5
      abb130(11)=gHZZ*gBr*gel*abb130(1)*abb130(2)*spbe6k1*c1*TR*i_
      abb130(12)=abb130(10)*abb130(11)
      abb130(13)=-abb130(9)*abb130(12)
      abb130(14)=spbk4k1*es12
      abb130(15)=spak2k6*spbk6k4*spbk2k1
      abb130(14)=abb130(14)-abb130(15)
      abb130(14)=abb130(14)*abb130(7)
      abb130(15)=es12-es345
      abb130(15)=abb130(15)*spbk6k4
      abb130(16)=spak1k2*spbk4k1*spbk6k2
      abb130(15)=abb130(15)-abb130(16)
      abb130(5)=abb130(5)*spae6k6
      abb130(15)=-abb130(15)*abb130(5)
      abb130(14)=abb130(15)+abb130(14)
      abb130(10)=abb130(14)*abb130(10)
      abb130(14)=spak2k5*abb130(6)
      abb130(10)=abb130(14)+abb130(10)
      abb130(10)=2.0_ki*abb130(10)*abb130(11)
      abb130(12)=4.0_ki*abb130(12)
      abb130(6)=-abb130(6)*abb130(12)
      abb130(8)=abb130(8)*abb130(12)
      abb130(12)=2.0_ki*abb130(4)
      abb130(11)=abb130(12)*abb130(11)
      abb130(12)=abb130(11)*spak2k5
      abb130(14)=abb130(9)*abb130(12)
      abb130(15)=-abb130(12)*spbk6k4*abb130(7)
      abb130(16)=-abb130(11)*spak5k6*abb130(9)
      abb130(17)=-abb130(11)*abb130(5)
      abb130(18)=-spak5k6*abb130(7)
      abb130(19)=-spak1k5*abb130(5)
      abb130(18)=abb130(19)+abb130(18)
      abb130(18)=spbk6k1*abb130(18)
      abb130(19)=spbk2k1*abb130(7)
      abb130(20)=spbk6k2*abb130(5)
      abb130(19)=abb130(20)+abb130(19)
      abb130(19)=spak2k5*abb130(19)
      abb130(18)=abb130(19)+abb130(18)
      abb130(18)=abb130(18)*abb130(11)
      abb130(9)=-abb130(11)*spak1k5*abb130(9)
      abb130(7)=abb130(7)*abb130(11)
      abb130(5)=abb130(12)*spbk4k1*abb130(5)
      R2d130=abb130(13)
      rat2 = rat2 + R2d130
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='130' value='", &
          & R2d130, "'/>"
      end if
   end subroutine
end module p16_bbarb_hepemg_abbrevd130h1
