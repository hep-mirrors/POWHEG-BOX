module     p4_ubaru_hepemg_abbrevd117h2
   use p4_ubaru_hepemg_config, only: ki
   use p4_ubaru_hepemg_globalsh2
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
      abb117(5)=dotproduct(k3,spvak1e6)
      abb117(6)=dotproduct(k3,spvae6k2)
      abb117(7)=dotproduct(k3,spvak4e6)
      abb117(8)=dotproduct(k3,spvae6k5)
      abb117(9)=dotproduct(k3,spvak1k2)
      abb117(10)=dotproduct(k3,spvak1k5)
      abb117(11)=dotproduct(k3,spvak4k2)
      abb117(12)=gUr+gDr+gCr+gBr+gSr
      abb117(13)=gUl+gDl+gCl+gBl+gSl
      abb117(14)=abb117(12)+abb117(13)
      abb117(15)=-abb117(4)*abb117(14)
      abb117(16)=abb117(13)+1.0_ki/3.0_ki*abb117(12)
      abb117(17)=1.0_ki/2.0_ki*spak4e6
      abb117(17)=abb117(17)*abb117(16)
      abb117(18)=-spbe6k4*abb117(17)
      abb117(19)=abb117(12)+1.0_ki/3.0_ki*abb117(13)
      abb117(20)=1.0_ki/2.0_ki*spbe6k5
      abb117(21)=-abb117(20)*spak5e6*abb117(19)
      abb117(15)=abb117(21)+2.0_ki/3.0_ki*abb117(15)+abb117(18)
      abb117(15)=spak1k4*abb117(15)
      abb117(18)=spbk6e6*spak4k6
      abb117(21)=1.0_ki/2.0_ki*abb117(7)+abb117(18)
      abb117(22)=1.0_ki/2.0_ki*spak1e6
      abb117(21)=abb117(22)*abb117(16)*abb117(21)
      abb117(23)=abb117(12)-abb117(13)
      abb117(24)=abb117(23)*spak1k6
      abb117(25)=abb117(24)*spbk6e6
      abb117(26)=-abb117(5)*abb117(23)
      abb117(26)=1.0_ki/2.0_ki*abb117(26)-abb117(25)
      abb117(27)=1.0_ki/3.0_ki*spak4e6
      abb117(26)=abb117(26)*abb117(27)
      abb117(28)=-spak1e6*spak4k5*abb117(16)
      abb117(29)=spak4e6*spak1k5*abb117(19)
      abb117(28)=abb117(28)+abb117(29)
      abb117(20)=abb117(28)*abb117(20)
      abb117(15)=abb117(15)+abb117(20)+abb117(21)+abb117(26)
      abb117(15)=spbk5k2*abb117(15)
      abb117(20)=-spak4k6*spbk6k2
      abb117(20)=-1.0_ki/2.0_ki*abb117(11)+abb117(20)
      abb117(16)=abb117(22)*abb117(16)*abb117(20)
      abb117(20)=spbk6k2*spak1k6
      abb117(20)=abb117(9)+2.0_ki*abb117(20)
      abb117(20)=abb117(27)*abb117(20)*abb117(14)
      abb117(16)=abb117(16)+abb117(20)
      abb117(16)=spbe6k5*abb117(16)
      abb117(20)=spae6k6*spbk6k5
      abb117(21)=1.0_ki/2.0_ki*abb117(8)+abb117(20)
      abb117(21)=spbe6k2*abb117(19)*abb117(21)
      abb117(22)=spak4e6*spbe6k2
      abb117(26)=abb117(22)*spbk5k4*abb117(19)
      abb117(21)=abb117(21)+abb117(26)
      abb117(26)=abb117(23)*spbk6k2
      abb117(27)=abb117(26)*spae6k6
      abb117(28)=abb117(6)*abb117(23)
      abb117(28)=1.0_ki/2.0_ki*abb117(28)+abb117(27)
      abb117(17)=spbk4k2*abb117(17)
      abb117(17)=1.0_ki/3.0_ki*abb117(28)+abb117(17)
      abb117(17)=spbe6k5*abb117(17)
      abb117(17)=1.0_ki/2.0_ki*abb117(21)+abb117(17)
      abb117(17)=spak1k4*abb117(17)
      abb117(21)=-spbk6k5*spak1k6
      abb117(21)=-1.0_ki/2.0_ki*abb117(10)+abb117(21)
      abb117(19)=abb117(22)*abb117(19)*abb117(21)
      abb117(15)=abb117(15)+abb117(17)+1.0_ki/2.0_ki*abb117(19)+abb117(16)
      abb117(16)=i_*abb117(1)*abb117(2)*abb117(3)*ger*gHZZ*Nfrat*c1*TR
      abb117(15)=abb117(15)*abb117(16)
      abb117(17)=abb117(12)*spak1k4
      abb117(19)=spbk2k1*spak1k6
      abb117(21)=-spbk6k5*abb117(19)*abb117(17)
      abb117(28)=abb117(13)*spbk5k2
      abb117(29)=spak1k2*spbk6k2
      abb117(30)=spak4k6*abb117(29)*abb117(28)
      abb117(21)=abb117(21)+abb117(30)
      abb117(30)=4.0_ki*abb117(16)
      abb117(21)=abb117(21)*abb117(30)
      abb117(31)=spbe6k5*spak1e6
      abb117(17)=abb117(31)*spbk2k1*abb117(17)
      abb117(28)=-abb117(22)*spak1k2*abb117(28)
      abb117(17)=abb117(17)+abb117(28)
      abb117(17)=abb117(17)*abb117(30)
      abb117(28)=abb117(14)*spak1k4
      abb117(32)=abb117(20)*spbe6k2
      abb117(33)=abb117(28)*abb117(32)
      abb117(34)=abb117(13)+3.0_ki*abb117(12)
      abb117(35)=abb117(18)*spbk5k2
      abb117(36)=abb117(35)*spak1e6
      abb117(37)=-abb117(34)*abb117(36)
      abb117(33)=-2.0_ki*abb117(33)+abb117(37)
      abb117(33)=abb117(33)*abb117(16)
      abb117(37)=abb117(16)*spbk5k2
      abb117(28)=abb117(28)*abb117(37)
      abb117(38)=8.0_ki*abb117(28)
      abb117(39)=abb117(12)+3.0_ki*abb117(13)
      abb117(40)=abb117(20)*spak1k4
      abb117(41)=abb117(40)*abb117(39)
      abb117(42)=-spbe6k2*abb117(41)
      abb117(36)=abb117(14)*abb117(36)
      abb117(36)=abb117(42)-2.0_ki*abb117(36)
      abb117(36)=abb117(36)*abb117(16)
      abb117(28)=-4.0_ki*abb117(28)
      abb117(42)=abb117(23)*spak1k4
      abb117(43)=abb117(16)*abb117(42)
      abb117(44)=abb117(43)*spbe6k2
      abb117(45)=1.0_ki/2.0_ki*abb117(16)
      abb117(46)=abb117(45)*spbe6k2
      abb117(47)=abb117(34)*abb117(46)
      abb117(48)=spak1k4*abb117(47)
      abb117(37)=abb117(23)*abb117(37)
      abb117(49)=abb117(37)*spak1e6
      abb117(50)=abb117(45)*spak1e6
      abb117(51)=abb117(39)*abb117(50)
      abb117(52)=spbk5k2*abb117(51)
      abb117(53)=abb117(23)*spak1k2
      abb117(54)=-abb117(53)*abb117(45)*abb117(35)
      abb117(55)=abb117(43)*spbe6k5
      abb117(56)=abb117(23)*spbk2k1
      abb117(40)=-abb117(56)*abb117(45)*abb117(40)
      abb117(57)=abb117(37)*spak4e6
      abb117(58)=3.0_ki*spbk2k1
      abb117(58)=abb117(12)*abb117(58)
      abb117(59)=abb117(13)*spbk2k1
      abb117(58)=abb117(59)+abb117(58)
      abb117(59)=abb117(58)*spak1k4
      abb117(60)=abb117(50)*spbk6e6
      abb117(61)=abb117(60)*abb117(59)
      abb117(43)=-spbk6k5*abb117(43)
      abb117(62)=3.0_ki*spak1k2
      abb117(13)=abb117(13)*abb117(62)
      abb117(12)=abb117(12)*spak1k2
      abb117(12)=abb117(13)+abb117(12)
      abb117(13)=abb117(12)*spbk5k2
      abb117(62)=abb117(46)*spae6k6
      abb117(63)=-abb117(62)*abb117(13)
      abb117(19)=spak1e6*spbk6e6*abb117(19)
      abb117(29)=spbe6k2*spae6k6*abb117(29)
      abb117(19)=abb117(19)+abb117(29)
      abb117(19)=abb117(45)*abb117(23)*abb117(19)
      abb117(29)=-spbe6k2*abb117(53)
      abb117(25)=1.0_ki/2.0_ki*abb117(25)+abb117(29)
      abb117(25)=abb117(25)*abb117(16)
      abb117(29)=-spak1e6*abb117(56)
      abb117(27)=-1.0_ki/2.0_ki*abb117(27)+abb117(29)
      abb117(27)=abb117(27)*abb117(16)
      abb117(29)=abb117(23)*abb117(60)
      abb117(12)=abb117(12)*abb117(20)*abb117(46)
      abb117(24)=-spbk6k5*abb117(24)
      abb117(13)=abb117(24)+2.0_ki*abb117(13)
      abb117(13)=abb117(13)*abb117(16)
      abb117(24)=abb117(23)*abb117(16)
      abb117(31)=abb117(24)*abb117(31)
      abb117(46)=-spbe6k5*abb117(51)
      abb117(37)=spak4k6*abb117(37)
      abb117(51)=-abb117(23)*abb117(62)
      abb117(50)=-abb117(58)*abb117(18)*abb117(50)
      abb117(26)=spak4k6*abb117(26)
      abb117(26)=abb117(26)-2.0_ki*abb117(59)
      abb117(26)=abb117(26)*abb117(16)
      abb117(22)=-abb117(22)*abb117(24)
      abb117(24)=-spak4e6*abb117(47)
      abb117(41)=spbe6k1*abb117(41)
      abb117(35)=abb117(35)*spak2e6*abb117(34)
      abb117(35)=abb117(41)+abb117(35)
      abb117(41)=abb117(14)*spbk5k1
      abb117(47)=abb117(41)*abb117(18)*spak1e6
      abb117(53)=abb117(14)*spak2k4
      abb117(32)=abb117(53)*abb117(32)
      abb117(32)=abb117(47)+abb117(32)+1.0_ki/2.0_ki*abb117(35)
      abb117(32)=abb117(32)*abb117(16)
      abb117(35)=-spbk6k5*spak4k6*abb117(14)
      abb117(41)=spak1k4*abb117(41)
      abb117(47)=spbk5k2*abb117(53)
      abb117(35)=abb117(47)+abb117(35)+abb117(41)
      abb117(35)=abb117(35)*abb117(30)
      abb117(14)=spak4e6*abb117(14)*spbe6k5
      abb117(30)=abb117(30)*abb117(14)
      abb117(14)=2.0_ki*abb117(16)*abb117(14)
      abb117(16)=abb117(34)*abb117(18)
      abb117(18)=abb117(23)*spak2k4
      abb117(41)=-spbe6k2*abb117(18)
      abb117(47)=-spbe6k1*abb117(42)
      abb117(16)=abb117(47)+abb117(16)+abb117(41)
      abb117(16)=abb117(16)*abb117(45)
      abb117(20)=abb117(39)*abb117(20)
      abb117(41)=abb117(23)*spbk5k1
      abb117(47)=spak1e6*abb117(41)
      abb117(23)=abb117(23)*spbk5k2
      abb117(53)=spak2e6*abb117(23)
      abb117(20)=abb117(53)+abb117(20)+abb117(47)
      abb117(20)=abb117(20)*abb117(45)
      abb117(47)=abb117(45)*spbe6k5
      abb117(18)=-abb117(18)*abb117(47)
      abb117(45)=abb117(45)*spak4e6
      abb117(23)=abb117(45)*abb117(23)
      abb117(42)=-abb117(47)*abb117(42)
      abb117(41)=abb117(41)*abb117(45)
      abb117(34)=spbk6e6*abb117(34)*abb117(45)
      abb117(39)=spae6k6*abb117(39)*abb117(47)
      R2d117=abb117(15)
      rat2 = rat2 + R2d117
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='117' value='", &
          & R2d117, "'/>"
      end if
   end subroutine
end module p4_ubaru_hepemg_abbrevd117h2
