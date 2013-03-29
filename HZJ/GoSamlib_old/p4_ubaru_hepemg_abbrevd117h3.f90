module     p4_ubaru_hepemg_abbrevd117h3
   use p4_ubaru_hepemg_config, only: ki
   use p4_ubaru_hepemg_globalsh3
   implicit none
   private
   complex(ki), dimension(63), public :: abb117
   complex(ki), public :: R2d117
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
      abb117(1)=1.0_ki/(-mZ**2+es345+i_*mZ*wZ)
      abb117(2)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb117(3)=es12**(-1)
      abb117(4)=dotproduct(k3,e6)
      abb117(5)=dotproduct(k3,spvae6k1)
      abb117(6)=dotproduct(k3,spvak2e6)
      abb117(7)=dotproduct(k3,spvak4e6)
      abb117(8)=dotproduct(k3,spvae6k5)
      abb117(9)=dotproduct(k3,spvak2k1)
      abb117(10)=dotproduct(k3,spvak2k5)
      abb117(11)=dotproduct(k3,spvak4k1)
      abb117(12)=gUr+gDr+gCr+gBr+gSr
      abb117(13)=gUl+gDl+gCl+gBl+gSl
      abb117(14)=abb117(13)+1.0_ki/3.0_ki*abb117(12)
      abb117(15)=spbk6e6*spak4k6
      abb117(16)=1.0_ki/2.0_ki*abb117(7)+abb117(15)
      abb117(17)=1.0_ki/2.0_ki*spak2e6
      abb117(16)=abb117(17)*abb117(14)*abb117(16)
      abb117(18)=abb117(12)-abb117(13)
      abb117(19)=abb117(18)*spak2k6
      abb117(20)=abb117(19)*spbk6e6
      abb117(21)=-abb117(6)*abb117(18)
      abb117(21)=1.0_ki/2.0_ki*abb117(21)-abb117(20)
      abb117(22)=1.0_ki/3.0_ki*spak4e6
      abb117(21)=abb117(21)*abb117(22)
      abb117(23)=-spak2e6*spak4k5*abb117(14)
      abb117(24)=abb117(12)+1.0_ki/3.0_ki*abb117(13)
      abb117(25)=spak4e6*spak2k5*abb117(24)
      abb117(23)=abb117(23)+abb117(25)
      abb117(25)=1.0_ki/2.0_ki*spbe6k5
      abb117(23)=abb117(23)*abb117(25)
      abb117(16)=abb117(23)+abb117(16)+abb117(21)
      abb117(16)=spbk5k1*abb117(16)
      abb117(21)=abb117(12)+abb117(13)
      abb117(23)=-abb117(4)*abb117(21)
      abb117(26)=1.0_ki/2.0_ki*spak4e6
      abb117(26)=abb117(26)*abb117(14)
      abb117(27)=-spbe6k4*abb117(26)
      abb117(25)=-abb117(25)*spak5e6*abb117(24)
      abb117(23)=abb117(25)+2.0_ki/3.0_ki*abb117(23)+abb117(27)
      abb117(23)=spbk5k1*abb117(23)
      abb117(25)=spae6k6*spbk6k5
      abb117(27)=1.0_ki/2.0_ki*abb117(8)+abb117(25)
      abb117(27)=spbe6k1*abb117(24)*abb117(27)
      abb117(28)=spak4e6*spbe6k1
      abb117(29)=abb117(28)*spbk5k4*abb117(24)
      abb117(27)=abb117(27)+abb117(29)
      abb117(29)=abb117(18)*spbk6k1
      abb117(30)=abb117(29)*spae6k6
      abb117(31)=abb117(5)*abb117(18)
      abb117(31)=1.0_ki/2.0_ki*abb117(31)+abb117(30)
      abb117(26)=spbk4k1*abb117(26)
      abb117(26)=1.0_ki/3.0_ki*abb117(31)+abb117(26)
      abb117(26)=spbe6k5*abb117(26)
      abb117(23)=abb117(23)+1.0_ki/2.0_ki*abb117(27)+abb117(26)
      abb117(23)=spak2k4*abb117(23)
      abb117(26)=-spak4k6*spbk6k1
      abb117(26)=-1.0_ki/2.0_ki*abb117(11)+abb117(26)
      abb117(14)=abb117(17)*abb117(14)*abb117(26)
      abb117(17)=spak2k6*spbk6k1
      abb117(17)=abb117(9)+2.0_ki*abb117(17)
      abb117(17)=abb117(22)*abb117(17)*abb117(21)
      abb117(14)=abb117(14)+abb117(17)
      abb117(14)=spbe6k5*abb117(14)
      abb117(17)=-spbk6k5*spak2k6
      abb117(17)=-1.0_ki/2.0_ki*abb117(10)+abb117(17)
      abb117(17)=abb117(28)*abb117(24)*abb117(17)
      abb117(14)=abb117(23)+abb117(16)+1.0_ki/2.0_ki*abb117(17)+abb117(14)
      abb117(16)=i_*abb117(1)*abb117(2)*abb117(3)*ger*gHZZ*Nfrat*c1*TR
      abb117(14)=abb117(14)*abb117(16)
      abb117(17)=abb117(13)*spbk5k1
      abb117(22)=spak1k2*spbk6k1
      abb117(23)=-spak4k6*abb117(22)*abb117(17)
      abb117(24)=abb117(12)*spak2k4
      abb117(26)=spbk2k1*spak2k6
      abb117(27)=spbk6k5*abb117(26)*abb117(24)
      abb117(23)=abb117(23)+abb117(27)
      abb117(27)=4.0_ki*abb117(16)
      abb117(23)=abb117(23)*abb117(27)
      abb117(17)=abb117(28)*spak1k2*abb117(17)
      abb117(31)=spbe6k5*spak2e6
      abb117(24)=-abb117(31)*spbk2k1*abb117(24)
      abb117(17)=abb117(17)+abb117(24)
      abb117(17)=abb117(17)*abb117(27)
      abb117(24)=abb117(21)*spbk5k1
      abb117(32)=abb117(15)*spak2e6
      abb117(33)=abb117(24)*abb117(32)
      abb117(34)=abb117(12)+3.0_ki*abb117(13)
      abb117(35)=abb117(25)*spak2k4
      abb117(36)=abb117(35)*spbe6k1
      abb117(37)=-abb117(34)*abb117(36)
      abb117(33)=-2.0_ki*abb117(33)+abb117(37)
      abb117(33)=abb117(33)*abb117(16)
      abb117(37)=abb117(16)*spak2k4
      abb117(24)=abb117(24)*abb117(37)
      abb117(38)=8.0_ki*abb117(24)
      abb117(39)=abb117(13)+3.0_ki*abb117(12)
      abb117(40)=abb117(15)*spbk5k1
      abb117(41)=abb117(40)*abb117(39)
      abb117(42)=-spak2e6*abb117(41)
      abb117(36)=abb117(21)*abb117(36)
      abb117(36)=abb117(42)-2.0_ki*abb117(36)
      abb117(36)=abb117(36)*abb117(16)
      abb117(24)=-4.0_ki*abb117(24)
      abb117(37)=abb117(18)*abb117(37)
      abb117(42)=abb117(37)*spbe6k1
      abb117(43)=1.0_ki/2.0_ki*abb117(16)
      abb117(44)=abb117(43)*spbe6k1
      abb117(45)=abb117(39)*abb117(44)
      abb117(46)=spak2k4*abb117(45)
      abb117(47)=abb117(18)*spbk5k1
      abb117(48)=abb117(16)*abb117(47)
      abb117(49)=abb117(48)*spak2e6
      abb117(50)=abb117(43)*spak2e6
      abb117(51)=abb117(34)*abb117(50)
      abb117(52)=spbk5k1*abb117(51)
      abb117(53)=abb117(18)*spbk2k1
      abb117(54)=abb117(53)*abb117(43)*abb117(35)
      abb117(55)=abb117(48)*spak4e6
      abb117(56)=abb117(18)*spak1k2
      abb117(40)=abb117(56)*abb117(43)*abb117(40)
      abb117(57)=abb117(37)*spbe6k5
      abb117(58)=3.0_ki*spbk2k1
      abb117(58)=abb117(12)*abb117(58)
      abb117(59)=abb117(13)*spbk2k1
      abb117(58)=abb117(59)+abb117(58)
      abb117(59)=abb117(58)*spak2k4
      abb117(60)=abb117(50)*spbk6e6
      abb117(61)=-abb117(60)*abb117(59)
      abb117(37)=-spbk6k5*abb117(37)
      abb117(62)=3.0_ki*spak1k2
      abb117(13)=abb117(13)*abb117(62)
      abb117(12)=abb117(12)*spak1k2
      abb117(12)=abb117(13)+abb117(12)
      abb117(13)=abb117(12)*spbk5k1
      abb117(62)=abb117(44)*spae6k6
      abb117(63)=abb117(62)*abb117(13)
      abb117(22)=-spbe6k1*spae6k6*abb117(22)
      abb117(26)=-spak2e6*spbk6e6*abb117(26)
      abb117(22)=abb117(22)+abb117(26)
      abb117(22)=abb117(43)*abb117(18)*abb117(22)
      abb117(26)=spak2e6*abb117(53)
      abb117(26)=-1.0_ki/2.0_ki*abb117(30)+abb117(26)
      abb117(26)=abb117(26)*abb117(16)
      abb117(30)=spbe6k1*abb117(56)
      abb117(20)=1.0_ki/2.0_ki*abb117(20)+abb117(30)
      abb117(20)=abb117(20)*abb117(16)
      abb117(30)=abb117(18)*abb117(60)
      abb117(12)=-abb117(12)*abb117(25)*abb117(44)
      abb117(19)=-spbk6k5*abb117(19)
      abb117(13)=abb117(19)-2.0_ki*abb117(13)
      abb117(13)=abb117(13)*abb117(16)
      abb117(19)=abb117(18)*abb117(16)
      abb117(31)=abb117(19)*abb117(31)
      abb117(44)=-spbe6k5*abb117(51)
      abb117(48)=spak4k6*abb117(48)
      abb117(51)=-abb117(18)*abb117(62)
      abb117(50)=abb117(58)*abb117(15)*abb117(50)
      abb117(29)=spak4k6*abb117(29)
      abb117(29)=abb117(29)+2.0_ki*abb117(59)
      abb117(29)=abb117(29)*abb117(16)
      abb117(19)=-abb117(28)*abb117(19)
      abb117(28)=-spak4e6*abb117(45)
      abb117(41)=spak1e6*abb117(41)
      abb117(35)=abb117(35)*spbe6k2*abb117(34)
      abb117(35)=abb117(41)+abb117(35)
      abb117(41)=abb117(21)*spak1k4
      abb117(45)=abb117(41)*abb117(25)*spbe6k1
      abb117(53)=abb117(21)*spbk5k2
      abb117(32)=abb117(53)*abb117(32)
      abb117(32)=abb117(45)+abb117(32)+1.0_ki/2.0_ki*abb117(35)
      abb117(32)=abb117(32)*abb117(16)
      abb117(35)=-spbk6k5*spak4k6*abb117(21)
      abb117(41)=spbk5k1*abb117(41)
      abb117(45)=spak2k4*abb117(53)
      abb117(35)=abb117(45)+abb117(35)+abb117(41)
      abb117(35)=abb117(35)*abb117(27)
      abb117(21)=spak4e6*abb117(21)*spbe6k5
      abb117(27)=abb117(27)*abb117(21)
      abb117(16)=2.0_ki*abb117(16)*abb117(21)
      abb117(15)=abb117(39)*abb117(15)
      abb117(21)=abb117(18)*spak1k4
      abb117(41)=-spbe6k1*abb117(21)
      abb117(45)=abb117(18)*spak2k4
      abb117(53)=-spbe6k2*abb117(45)
      abb117(15)=abb117(53)+abb117(15)+abb117(41)
      abb117(15)=abb117(15)*abb117(43)
      abb117(25)=abb117(34)*abb117(25)
      abb117(18)=abb117(18)*spbk5k2
      abb117(41)=spak2e6*abb117(18)
      abb117(53)=spak1e6*abb117(47)
      abb117(25)=abb117(53)+abb117(25)+abb117(41)
      abb117(25)=abb117(25)*abb117(43)
      abb117(41)=abb117(43)*spbe6k5
      abb117(45)=-abb117(41)*abb117(45)
      abb117(43)=abb117(43)*spak4e6
      abb117(18)=abb117(18)*abb117(43)
      abb117(21)=-abb117(21)*abb117(41)
      abb117(47)=abb117(43)*abb117(47)
      abb117(39)=spbk6e6*abb117(39)*abb117(43)
      abb117(34)=spae6k6*abb117(34)*abb117(41)
      R2d117=abb117(14)
      rat2 = rat2 + R2d117
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='117' value='", &
          & R2d117, "'/>"
      end if
   end subroutine
end module p4_ubaru_hepemg_abbrevd117h3
