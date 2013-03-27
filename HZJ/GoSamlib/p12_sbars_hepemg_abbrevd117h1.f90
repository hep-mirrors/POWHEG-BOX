module     p12_sbars_hepemg_abbrevd117h1
   use p12_sbars_hepemg_config, only: ki
   use p12_sbars_hepemg_globalsh1
   implicit none
   private
   complex(ki), dimension(63), public :: abb117
   complex(ki), public :: R2d117
   public :: init_abbrev
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
contains
   subroutine     init_abbrev()
      use p12_sbars_hepemg_config, only: deltaOS, &
     &    logfile, debug_nlo_diagrams
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_color, only: TR
      use p12_sbars_hepemg_globalsl1, only: epspow
      implicit none
      abb117(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb117(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb117(3)=es12**(-1)
      abb117(4)=dotproduct(k3,e6)
      abb117(5)=dotproduct(k3,spvae6k1)
      abb117(6)=dotproduct(k3,spvak2e6)
      abb117(7)=dotproduct(k3,spvae6k4)
      abb117(8)=dotproduct(k3,spvak5e6)
      abb117(9)=dotproduct(k3,spvak2k1)
      abb117(10)=dotproduct(k3,spvak2k4)
      abb117(11)=dotproduct(k3,spvak5k1)
      abb117(12)=gUr+gDr+gCr+gBr+gSr
      abb117(13)=gUl+gDl+gCl+gBl+gSl
      abb117(14)=abb117(12)+abb117(13)
      abb117(15)=-abb117(4)*abb117(14)
      abb117(16)=abb117(12)+1.0_ki/3.0_ki*abb117(13)
      abb117(17)=1.0_ki/2.0_ki*spbe6k4
      abb117(17)=abb117(17)*abb117(16)
      abb117(18)=-spak4e6*abb117(17)
      abb117(19)=abb117(13)+1.0_ki/3.0_ki*abb117(12)
      abb117(20)=1.0_ki/2.0_ki*spak5e6
      abb117(21)=-abb117(20)*spbe6k5*abb117(19)
      abb117(15)=abb117(21)+2.0_ki/3.0_ki*abb117(15)+abb117(18)
      abb117(15)=spbk4k1*abb117(15)
      abb117(18)=spae6k6*spbk6k4
      abb117(21)=1.0_ki/2.0_ki*abb117(7)+abb117(18)
      abb117(22)=1.0_ki/2.0_ki*spbe6k1
      abb117(21)=abb117(22)*abb117(16)*abb117(21)
      abb117(23)=abb117(12)-abb117(13)
      abb117(24)=abb117(23)*spbk6k1
      abb117(25)=abb117(24)*spae6k6
      abb117(26)=abb117(5)*abb117(23)
      abb117(26)=1.0_ki/2.0_ki*abb117(26)+abb117(25)
      abb117(27)=1.0_ki/3.0_ki*spbe6k4
      abb117(26)=abb117(26)*abb117(27)
      abb117(28)=-spbe6k1*spbk5k4*abb117(16)
      abb117(29)=spbe6k4*spbk5k1*abb117(19)
      abb117(28)=abb117(28)+abb117(29)
      abb117(20)=abb117(28)*abb117(20)
      abb117(15)=abb117(15)+abb117(20)+abb117(21)+abb117(26)
      abb117(15)=spak2k5*abb117(15)
      abb117(20)=-spbk6k4*spak2k6
      abb117(20)=-1.0_ki/2.0_ki*abb117(10)+abb117(20)
      abb117(16)=abb117(22)*abb117(16)*abb117(20)
      abb117(20)=spak2k6*spbk6k1
      abb117(20)=abb117(9)+2.0_ki*abb117(20)
      abb117(20)=abb117(27)*abb117(20)*abb117(14)
      abb117(16)=abb117(16)+abb117(20)
      abb117(16)=spak5e6*abb117(16)
      abb117(20)=spbk6e6*spak5k6
      abb117(21)=1.0_ki/2.0_ki*abb117(8)+abb117(20)
      abb117(21)=spak2e6*abb117(19)*abb117(21)
      abb117(22)=spbe6k4*spak2e6
      abb117(26)=abb117(22)*spak4k5*abb117(19)
      abb117(21)=abb117(21)+abb117(26)
      abb117(26)=abb117(23)*spak2k6
      abb117(27)=abb117(26)*spbk6e6
      abb117(28)=-abb117(6)*abb117(23)
      abb117(28)=1.0_ki/2.0_ki*abb117(28)-abb117(27)
      abb117(17)=spak2k4*abb117(17)
      abb117(17)=1.0_ki/3.0_ki*abb117(28)+abb117(17)
      abb117(17)=spak5e6*abb117(17)
      abb117(17)=1.0_ki/2.0_ki*abb117(21)+abb117(17)
      abb117(17)=spbk4k1*abb117(17)
      abb117(21)=-spak5k6*spbk6k1
      abb117(21)=-1.0_ki/2.0_ki*abb117(11)+abb117(21)
      abb117(19)=abb117(22)*abb117(19)*abb117(21)
      abb117(15)=abb117(15)+abb117(17)+1.0_ki/2.0_ki*abb117(19)+abb117(16)
      abb117(16)=i_*abb117(1)*abb117(2)*abb117(3)*gel*gHZZ*Nfrat*c1*TR
      abb117(15)=abb117(15)*abb117(16)
      abb117(17)=abb117(13)*spbk4k1
      abb117(19)=spak1k2*spbk6k1
      abb117(21)=-spak5k6*abb117(19)*abb117(17)
      abb117(28)=abb117(12)*spak2k5
      abb117(29)=spbk2k1*spak2k6
      abb117(30)=spbk6k4*abb117(29)*abb117(28)
      abb117(21)=abb117(21)+abb117(30)
      abb117(30)=4.0_ki*abb117(16)
      abb117(21)=abb117(21)*abb117(30)
      abb117(31)=spak5e6*spbe6k1
      abb117(17)=abb117(31)*spak1k2*abb117(17)
      abb117(28)=-abb117(22)*spbk2k1*abb117(28)
      abb117(17)=abb117(17)+abb117(28)
      abb117(17)=abb117(17)*abb117(30)
      abb117(28)=abb117(14)*spbk4k1
      abb117(32)=abb117(20)*spak2e6
      abb117(33)=abb117(28)*abb117(32)
      abb117(34)=abb117(12)+3.0_ki*abb117(13)
      abb117(35)=abb117(18)*spak2k5
      abb117(36)=abb117(35)*spbe6k1
      abb117(37)=-abb117(34)*abb117(36)
      abb117(33)=-2.0_ki*abb117(33)+abb117(37)
      abb117(33)=abb117(33)*abb117(16)
      abb117(37)=abb117(16)*spak2k5
      abb117(28)=abb117(28)*abb117(37)
      abb117(38)=8.0_ki*abb117(28)
      abb117(39)=abb117(13)+3.0_ki*abb117(12)
      abb117(40)=abb117(20)*spbk4k1
      abb117(41)=abb117(40)*abb117(39)
      abb117(42)=-spak2e6*abb117(41)
      abb117(36)=abb117(14)*abb117(36)
      abb117(36)=abb117(42)-2.0_ki*abb117(36)
      abb117(36)=abb117(36)*abb117(16)
      abb117(28)=-4.0_ki*abb117(28)
      abb117(42)=abb117(23)*spbk4k1
      abb117(43)=abb117(16)*abb117(42)
      abb117(44)=abb117(43)*spak2e6
      abb117(45)=1.0_ki/2.0_ki*abb117(16)
      abb117(46)=abb117(45)*spak2e6
      abb117(47)=abb117(34)*abb117(46)
      abb117(48)=spbk4k1*abb117(47)
      abb117(37)=abb117(23)*abb117(37)
      abb117(49)=abb117(37)*spbe6k1
      abb117(50)=abb117(45)*spbe6k1
      abb117(51)=abb117(39)*abb117(50)
      abb117(52)=spak2k5*abb117(51)
      abb117(53)=abb117(23)*spbk2k1
      abb117(54)=abb117(53)*abb117(45)*abb117(35)
      abb117(55)=abb117(43)*spak5e6
      abb117(56)=abb117(23)*spak1k2
      abb117(40)=abb117(56)*abb117(45)*abb117(40)
      abb117(57)=abb117(37)*spbe6k4
      abb117(58)=3.0_ki*spbk2k1
      abb117(58)=abb117(12)*abb117(58)
      abb117(59)=abb117(13)*spbk2k1
      abb117(58)=abb117(59)+abb117(58)
      abb117(59)=abb117(58)*spak2k5
      abb117(60)=abb117(46)*spbk6e6
      abb117(61)=-abb117(60)*abb117(59)
      abb117(37)=-spbk6k4*abb117(37)
      abb117(62)=3.0_ki*spak1k2
      abb117(13)=abb117(13)*abb117(62)
      abb117(12)=abb117(12)*spak1k2
      abb117(12)=abb117(13)+abb117(12)
      abb117(13)=abb117(12)*spbk4k1
      abb117(62)=abb117(50)*spae6k6
      abb117(63)=abb117(62)*abb117(13)
      abb117(19)=-spbe6k1*spae6k6*abb117(19)
      abb117(29)=-spak2e6*spbk6e6*abb117(29)
      abb117(19)=abb117(19)+abb117(29)
      abb117(19)=abb117(45)*abb117(23)*abb117(19)
      abb117(29)=spak2e6*abb117(53)
      abb117(25)=-1.0_ki/2.0_ki*abb117(25)+abb117(29)
      abb117(25)=abb117(25)*abb117(16)
      abb117(29)=spbe6k1*abb117(56)
      abb117(27)=1.0_ki/2.0_ki*abb117(27)+abb117(29)
      abb117(27)=abb117(27)*abb117(16)
      abb117(29)=abb117(23)*abb117(60)
      abb117(12)=-abb117(12)*abb117(18)*abb117(50)
      abb117(26)=-spbk6k4*abb117(26)
      abb117(13)=abb117(26)-2.0_ki*abb117(13)
      abb117(13)=abb117(13)*abb117(16)
      abb117(26)=abb117(23)*abb117(16)
      abb117(22)=abb117(22)*abb117(26)
      abb117(47)=-spbe6k4*abb117(47)
      abb117(43)=spak5k6*abb117(43)
      abb117(50)=-abb117(23)*abb117(62)
      abb117(46)=abb117(58)*abb117(20)*abb117(46)
      abb117(24)=spak5k6*abb117(24)
      abb117(24)=abb117(24)+2.0_ki*abb117(59)
      abb117(24)=abb117(24)*abb117(16)
      abb117(26)=-abb117(26)*abb117(31)
      abb117(31)=-spak5e6*abb117(51)
      abb117(41)=spak1e6*abb117(41)
      abb117(35)=abb117(35)*spbe6k2*abb117(34)
      abb117(35)=abb117(41)+abb117(35)
      abb117(41)=abb117(14)*spak1k5
      abb117(51)=abb117(41)*abb117(18)*spbe6k1
      abb117(53)=abb117(14)*spbk4k2
      abb117(32)=abb117(53)*abb117(32)
      abb117(32)=abb117(51)+abb117(32)+1.0_ki/2.0_ki*abb117(35)
      abb117(32)=abb117(32)*abb117(16)
      abb117(35)=-spak5k6*spbk6k4*abb117(14)
      abb117(41)=spbk4k1*abb117(41)
      abb117(51)=spak2k5*abb117(53)
      abb117(35)=abb117(51)+abb117(35)+abb117(41)
      abb117(35)=abb117(35)*abb117(30)
      abb117(14)=spbe6k4*abb117(14)*spak5e6
      abb117(30)=abb117(30)*abb117(14)
      abb117(14)=2.0_ki*abb117(16)*abb117(14)
      abb117(16)=abb117(34)*abb117(18)
      abb117(18)=abb117(23)*spbk4k2
      abb117(41)=spak2e6*abb117(18)
      abb117(51)=spak1e6*abb117(42)
      abb117(16)=abb117(51)+abb117(16)+abb117(41)
      abb117(16)=abb117(16)*abb117(45)
      abb117(20)=abb117(39)*abb117(20)
      abb117(41)=abb117(23)*spak1k5
      abb117(51)=-spbe6k1*abb117(41)
      abb117(23)=abb117(23)*spak2k5
      abb117(53)=-spbe6k2*abb117(23)
      abb117(20)=abb117(53)+abb117(20)+abb117(51)
      abb117(20)=abb117(20)*abb117(45)
      abb117(51)=abb117(45)*spbe6k4
      abb117(23)=-abb117(51)*abb117(23)
      abb117(45)=abb117(45)*spak5e6
      abb117(18)=abb117(18)*abb117(45)
      abb117(41)=-abb117(41)*abb117(51)
      abb117(42)=abb117(45)*abb117(42)
      abb117(39)=spbk6e6*abb117(39)*abb117(45)
      abb117(34)=spae6k6*abb117(34)*abb117(51)
      R2d117=abb117(15)
      rat2 = rat2 + R2d117
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='117' value='", &
          & R2d117, "'/>"
      end if
   end subroutine
end module p12_sbars_hepemg_abbrevd117h1
