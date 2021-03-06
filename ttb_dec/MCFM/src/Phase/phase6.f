      subroutine phase6(r,p1,p2,p3,p4,p5,p6,p7,p8,wt,*)
      implicit none
      include 'constants.f'
      include 'masses.f'
      include 'mxdim.f'
      include 'zerowidth.f'
      include 'process.f'
c******* generate phase space for 2-->4 process
c******* r(mxdim),p1(4),p2(4) are inputs reversed in sign from physical values 
c---- phase space for -p1-p2 --> p5+p6+p3+p4+p7+p8
c---- with all 2 pi's (ie 1/(2*pi)^14)
      logical oldzerowidth
      double precision r(mxdim)
      double precision p1(4),p2(4),p5(4),p6(4),p3(4),p4(4),p7(4),p8(4)
      double precision p12(4),p345(4),p678(4),p78(4),p34(4),smin,
     . p348(4),p567(4),p56(4),p3456(4),p5678(4)
      double precision wt,wt0,wt12,wt678,wt345,wt34,wt78,wt3456,wt56,
     . wt5678
      integer n2,n3
      double precision mass2,width2,mass3,width3
      common/breit/n2,n3,mass2,width2,mass3,width3 
      integer j
      parameter(wt0=1d0/twopi**4)
      oldzerowidth=zerowidth

      wt=0d0
      do j=1,4
      p12(j)=-p1(j)-p2(j)
      enddo
      smin=mb**2

c---- calculate momenta of top and bbbar      
      n2=1
      n3=1
      if (
     .        (case .eq. 'tt_bbh')
     .   .or. (case .eq. 'tt_bbl')
     .   .or. (case .eq. 'tt_ldk')
     .   .or. (case .eq. 'tt_hdk')
     .   .or. (case .eq. 'tthWdk')
     .   .or. (case .eq. 'ttZbbl')
     .   .or. (case .eq. 'tt_bbu')
     .   .or. (case .eq. 'ttbdkl')
     .   .or. (case .eq. 'ttbdkh')
     .   .or. (case .eq. 'vlchk6')) then
        mass2=mt
        width2=twidth
        mass3=mt
        width3=twidth
	zerowidth=.true.
        call phi1_2(r(1),r(2),r(3),r(4),p12,p345,p678,wt12,*99)
	zerowidth=oldzerowidth
      elseif ((case .eq. 'Wtbwdk') .or. (case .eq. 'W_twdk')) then
        mass2=mt
        width2=twidth
        mass3=mt
        width3=twidth
        call phi1_2(r(1),r(2),r(3),r(4),p12,p348,p567,wt12,*99)
      elseif (case .eq. 'tautau') then
        mass2=mtau
        width2=tauwidth
        mass3=mtau
        width3=tauwidth
        zerowidth=.true.
        call phi1_2(r(1),r(2),r(3),r(4),p12,p345,p678,wt12,*99)
        zerowidth=oldzerowidth
      elseif (case .eq. 'WpWp2j') then
        n2=0
	n3=0
        call phi1_2(r(1),r(2),r(3),r(4),p12,p3456,p78,wt12,*99)
	n2=1
	mass2=wmass
	width2=wwidth
	n3=1
	mass3=wmass
	width3=wwidth
        call phi1_2(r(5),r(6),r(7),r(8),p3456,p56,p34,wt3456,*99)
        call phi3m0(r(13),r(14),p34,p3,p4,wt34,*99)
        call phi3m0(r(11),r(12),p56,p5,p6,wt56,*99)
        call phi3m0(r(15),r(16),p78,p7,p8,wt78,*99)
        wt=wt0*wt12*wt3456*wt34*wt56*wt78
	return
      elseif ((case .eq. 'HWWjet') 
     . .or. (case .eq. 'HWW2jt')
     . .or. (case .eq. 'qq_HWW')) then
c--- In the case of HWWjet, we should generate s3456 according to
c--- a Breit-Wigner at mH
        n2=1
        mass2=hmass
        width2=hwidth
        n3=0
        call phi1_2(r(1),r(2),r(3),r(4),p12,p3456,p78,wt12,*99)
        n2=1
        mass2=wmass
        width2=wwidth
        n3=1
        mass3=wmass
        width3=wwidth
        call phi1_2(r(5),r(6),r(7),r(8),p3456,p56,p34,wt3456,*99)
        call phi3m0(r(13),r(14),p34,p3,p4,wt34,*99)
        call phi3m0(r(11),r(12),p56,p5,p6,wt56,*99)
        call phi3m0(r(15),r(16),p78,p7,p8,wt78,*99)
        wt=wt0*wt12*wt3456*wt34*wt56*wt78
        return
      elseif ((case .eq. 'HZZjet')
     . .or. (case .eq. 'HZZ2jt')
     . .or. (case .eq. 'qq_HZZ')) then
c--- In the case of HZZjet, we should generate s3456 according to
c--- a Breit-Wigner at mH
        n2=1
        mass2=hmass
        width2=hwidth
        n3=0
        call phi1_2(r(1),r(2),r(3),r(4),p12,p3456,p78,wt12,*99)
        n2=1
        mass2=zmass
        width2=zwidth
        n3=1
        mass3=zmass
        width3=zwidth
        call phi1_2(r(5),r(6),r(7),r(8),p3456,p56,p34,wt3456,*99)
        call phi3m0(r(13),r(14),p34,p3,p4,wt34,*99)
        call phi3m0(r(11),r(12),p56,p5,p6,wt56,*99)
        call phi3m0(r(15),r(16),p78,p7,p8,wt78,*99)
        wt=wt0*wt12*wt3456*wt34*wt56*wt78
        return
      elseif ((case .eq. 'WH__WW') .or. (case .eq. 'ZH__WW')) then
c--- In the case of W+H(->WW), we should generate s5678 according to
c--- a Breit-Wigner at mH
        n2=1
	if (case .eq. 'WH__WW') then
	  mass2=wmass
	  width2=wwidth
	else
	  mass2=zmass
	  width2=zwidth
	endif
        n3=1
        mass3=hmass
        width3=hwidth
        call phi1_2(r(1),r(2),r(3),r(4),p12,p34,p5678,wt12,*99)
        n2=1
        mass2=wmass
        width2=wwidth
        n3=1
        mass3=wmass
        width3=wwidth
        call phi1_2(r(5),r(6),r(7),r(8),p5678,p56,p78,wt5678,*99)
        call phi3m0(r(13),r(14),p34,p3,p4,wt34,*99)
        call phi3m0(r(11),r(12),p56,p5,p6,wt56,*99)
        call phi3m0(r(15),r(16),p78,p7,p8,wt78,*99)
        wt=wt0*wt12*wt5678*wt34*wt56*wt78
        return
      elseif ((case .eq. 'WH__ZZ') .or. (case .eq. 'ZH__ZZ')) then
c--- In the case of W+H(->WW), we should generate s5678 according to
c--- a Breit-Wigner at mH
        n2=1
	if (case .eq. 'WH__ZZ') then
	  mass2=wmass
	  width2=wwidth
	else
	  mass2=zmass
	  width2=zwidth
	endif
        n3=1
        mass3=hmass
        width3=hwidth
        call phi1_2(r(1),r(2),r(3),r(4),p12,p34,p5678,wt12,*99)
        n2=1
        mass2=zmass
        width2=zwidth
        n3=1
        mass3=zmass
        width3=zwidth
        call phi1_2(r(5),r(6),r(7),r(8),p5678,p56,p78,wt5678,*99)
        call phi3m0(r(13),r(14),p34,p3,p4,wt34,*99)
        call phi3m0(r(11),r(12),p56,p5,p6,wt56,*99)
        call phi3m0(r(15),r(16),p78,p7,p8,wt78,*99)
        wt=wt0*wt12*wt5678*wt34*wt56*wt78
        return
      else
        write(*,*) 'Case not supported in phase6.f'
        stop
      endif

c      write(6,*) '345',(p345(4)**2-p345(3)**2-p345(2)**2-p345(1)**2)
c      write(6,*) '678',(p678(4)**2-p678(3)**2-p678(2)**2-p678(1)**2)

      mass3=wmass
      width3=wwidth
      if (   (case .eq. 'tt_bbh')  
     .  .or. (case .eq. 'tt_bbl')
     .  .or. (case .eq. 'tt_ldk')
     .  .or. (case .eq. 'tt_hdk')
     .  .or. (case .eq. 'tthWdk')
     .  .or. (case .eq. 'ttZbbl')
     .  .or. (case .eq. 'tt_bbu')
     .  .or. (case .eq. 'ttbdkl')
     .  .or. (case .eq. 'ttbdkh')
     .  .or. (case .eq. 'vlchk6')) then
        n3=1
        call phi1_2m(mb,r(5),r(6),r(7),smin,p345,p5,p34,wt345,*99)
        call phi1_2m(mb,r(8),r(11),r(12),smin,p678,p6,p78,wt678,*99)
      elseif ((case .eq. 'Wtbwdk') .or. (case .eq. 'W_twdk')) then
        n3=1
        call phi1_2m(mb,r(5),r(6),r(7),smin,p348,p8,p34,wt345,*99)
        call phi1_2m(zip,r(8),r(11),r(12),smin,p567,p7,p56,wt678,*99)
      elseif (case .eq. 'tautau') then
        n3=0
        smin=0d0
        call phi1_2m(smin,r(5),r(6),r(7),smin,p345,p5,p34,wt345,*99)
        call phi1_2m(smin,r(8),r(11),r(12),smin,p678,p6,p78,wt678,*99)
      endif

c      write(6,*) '34',(p34(4)**2-p34(3)**2-p34(2)**2-p34(1)**2)
c      write(6,*) '78',(p78(4)**2-p78(3)**2-p78(2)**2-p78(1)**2)

      call phi3m0(r(13),r(14),p34,p3,p4,wt34,*99)
      if   ((case .eq. 'Wtbwdk') 
     . .or. (case .eq. 'W_twdk')
     . ) then
      call phi3m0(r(15),r(16),p56,p5,p6,wt78,*99)
      else
      call phi3m0(r(15),r(16),p78,p7,p8,wt78,*99)
      endif
      
      wt=wt0*wt12*wt345*wt678*wt34*wt78
      
      return
 99   wt=0d0
      zerowidth=oldzerowidth
      return 1
      end

