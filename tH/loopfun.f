*************************************************************
	subroutine myAget(m02)
*************************************************************

      include 'loopints.h'

	double precision m02
* functions
	double precision A01

	xa0 = A01(dsqrt(m02))

	end

*************************************************************
	subroutine myBget(p1,m02,m12)
*************************************************************

      include 'loopints.h'

	double precision m02,m12
	double precision p1,m0,m1
	double precision f1D
* functions
	complex*16 B02
	double precision A01

	xb0 = B02(p1,dsqrt(m02),dsqrt(m12))

*********************************
* B^{mu} coefficients 
*
	f1D = p1 - m12 + m02

**** cases for special arguments ----- NO IR parts yet!!!

c b1(p1,m0,m1)
	if (p1.ne.0d0) then
	  xb1 = (1d0/(2d0*p1))*(A01(dsqrt(m02))-A01(dsqrt(m12))-f1D*xb0) 	
c b1(0,m0,m1)
	elseif ((p1.eq.0d0).and.(m02.ne.m12)) then
	  xb1 = A01(dsqrt(m12))/(m02 - m12) + (-m02**2/4d0 + m12**2/4d0 - (m02*A01(dsqrt(m02)))/2d0 + (m12*A01(dsqrt(m12)))/2d0)/(m02 - m12)**2
c b1(0,m0,m0)
	elseif ((p1.eq.0d0).and.(m02.eq.m12).and.(m02.ne.0d0)) then
	  xb1 = -1/2d0*B02(p1,dsqrt(m02),dsqrt(m12))
c b1(0,0,0)
	elseif ((p1.eq.0d0).and.(m02.eq.m12).and.(m02.eq.0d0)) then
	  xb1 = -0.50d0*(UVdiv - IRdiv)

	endif

*********************************
* B^{mu nu} coefficients 
*

	xb00 = (m02 + 2*m02*xb0 + m12 - p1/3d0 + xb1*(m02 - m12 + p1) + A01(dsqrt(m12)))/6d0

**** cases for special arguments ----- NO IR parts yet!!!

c b11(p1,m0,m1)
	if (p1.ne.0d0) then
	  xb11 = (-m02 - 2d0*m02*xb0 - m12 + p1/3d0 - 4*xb1*(m02 - m12 + p1) + 2*A01(dsqrt(m12)))/(6d0*p1)
c b11(0,m0,m1)
	elseif ((p1.eq.0d0).and.(m02.ne.m12)) then
	  xb11 = -(A01(dsqrt(m12))/(m02 - m12)) - (2*m02*
     -     (-m02**2/4d0 + m12**2/4d0 - (m02*A01(dsqrt(m02)))/2d0 + (m12*A01(dsqrt(m12)))/2d0))/(m02 - m12)**3 + 
     -  (2*(-m02**3/9d0 + m12**3/9d0 - (m02**2*A01(dsqrt(m02)))/3d0 + (m12**2*A01(dsqrt(m12)))/3d0))/(m02 - m12)**3
c b11(0,m0,m0)
	elseif ((p1.eq.0d0).and.(m02.eq.m12).and.(m02.ne.0d0)) then
	  xb11 = 1/3d0*B02(p1,dsqrt(m02),dsqrt(m12))
c b11(0,0,0)
	elseif ((p1.eq.0d0).and.(m02.eq.m12).and.(m02.eq.0d0)) then
	  xb11 = (1.0d0/3.0d0)*(UVdiv - IRdiv)

	endif


	end

*************************************************************
	subroutine myBPget(p1,m02,m12)
*************************************************************

	include 'loopints.h'

	double precision m02,m12
	double precision p1,m0,m1
	double precision f1D
* swap
	complex*16 swb0,swb1,swb00,swb11
	complex*16 pb0,pb1,pb00,pb11
* functions
	complex*16 B02,B02p
	double precision A01

	xbp0 = B02p(p1,dsqrt(m02),dsqrt(m12))

*********************************
* Bp^{mu} coefficients 
*

	swb0  = xb0
	swb1  = xb1
	swb00 = xb00
	swb11 = xb11 

	call myBget(p1,m02,m12)
	pb0  = xb0
	pb1  = xb1
	pb00 = xb00
	pb11 = xb11

	xb0  = swb0
	xb1  = swb1
	xb00 = swb00
	xb11 = swb11
*
**** cases for special arguments ----- NO IR parts yet!!!

	f1D = p1 - m12 + m02

c b1'(p1,m0,m1)
	if (p1.ne.0d0) then
	  xbp1 = -(1d0/p1)*(pb1 + 1/2d0*pb0 + 1/2d0*f1D*xbp0) 	
c b1'(0,m0,m1)
	elseif ((p1.eq.0d0).and.(m02.ne.m12).and.(m02.ne.0d0).and.(m12.ne.0d0)) then
	  xbp1 = -1/(6.*(-m02 + m12)) + (m02*(m02 + m12))/(2.*(-m02 + m12)**3) - (m02**2*m12*dlog(m12/m02))/(-m02 + m12)**4
c b1'(0,m0,m0)
	elseif ((p1.eq.0d0).and.(m02.eq.m12).and.(m02.ne.0d0)) then
	  xbp1 = -1/(12d0*m02)
c b1'(0,m0,0)
	elseif ((p1.eq.0d0).and.(m02.ne.0d0).and.(m12.eq.0d0)) then
	  xbp1 = -1/(3d0*m02)
c b1'(0,0,m1)
	elseif ((p1.eq.0d0).and.(m12.ne.0d0).and.(m02.eq.0d0)) then
	  xbp1 = -1/(6d0*m12)
c b1'(0,0,0)
	elseif ((p1.eq.0d0).and.(m02.eq.m12).and.(m02.eq.0d0)) then
	  xbp1 = 0d0

	endif

*********************************
* Bp^{mu nu} coefficients 
*

	xbp00 = (- 1/3d0 + 2d0*m02*xbp0 + xb1 + xbp1*(m02 - m12 + p1))/6d0

c b11'(p1,m0,m1)
	if (p1.ne.0d0) then
	  xbp11 = (-6d0*pb11 - 2d0*m02*xbp0 + 1/3d0 - 4*xbp1*(m02 - m12 + p1) - 4*xb1)/(6d0*p1)
c b11'(0,m0,m1)
	elseif ((p1.eq.0d0).and.(m02.ne.m12).and.(m02.ne.0d0).and.(m12.ne.0d0)) then
	  xbp11 = ((m02 - m12)*(3*m02**3 + 13*m02**2*m12 - 5*m02*m12**2 + m12**3) + 12*m02**3*m12*dlog(m12/m02))/
     -  (12.*(m02 - m12)**5)
c b11'(0,m0,m0)
	elseif ((p1.eq.0d0).and.(m02.eq.m12).and.(m02.ne.0d0)) then
	  xbp11 = 1/(20d0*m02)
c b11'(0,m0,0)
	elseif ((p1.eq.0d0).and.(m02.ne.0d0).and.(m12.eq.0d0)) then
	  xbp11 = 1/(4d0*m02)
c b11'(0,0,m1)
	elseif ((p1.eq.0d0).and.(m12.ne.0d0).and.(m02.eq.0d0)) then
	  xbp11 = 1/(12d0*m12)
c b11'(0,0,0)
	elseif ((p1.eq.0d0).and.(m02.eq.m12).and.(m02.eq.0d0)) then
	  xbp11 = 0d0

	endif

	end

*************************************************************
	subroutine myCget(p1,p21,p2,m02,m12,m22)
*************************************************************

      include 'loopints.h'

	double precision m02,m12,m22
	double precision p1,p2,p21,m0,m1,m2
	double precision X2(2,2), detX2
	double precision f1D, f2D, c0coef
	complex*16 R1D, R2D, R3D, R4D, R5D, R6D, sC03, x0
* swap
	complex*16 swb0,swb1,swb00,swb11
	complex*16 cb0_1,cb1_1,cb0_2,cb1_2,cb0_3,cb1_3
* functions
	complex*16 B02,C03f
* subroutines
	external myBget

	xc0 = C03f(p1,p21,p2,dsqrt(m02),dsqrt(m12),dsqrt(m22))

*********************************
* C^{mu} coefficients 
*

	detX2 = (-p1**2 - (p2 - p21)**2 + 2*p1*(p2 + p21))/4d0

	X2(1,1) = p2
	X2(2,2) = p1
	X2(1,2) = (-p1 - p2 + p21)/2d0
	X2(2,1) = X2(1,2)

	f1D = p1 - m12 + m02
	f2D = p2 - m22 + m02

	R1D = 0.5d0*(B02(p2,dsqrt(m02),dsqrt(m22)) - B02(p21,dsqrt(m12),dsqrt(m22)) - f1D*xc0)
	R2D = 0.5d0*(B02(p1,dsqrt(m02),dsqrt(m12)) - B02(p21,dsqrt(m12),dsqrt(m22)) - f2D*xc0)

****
	xc1 = 1d0/detX2*(X2(1,1)*R1D + X2(1,2)*R2D) 	
	xc2 = 1d0/detX2*(X2(2,1)*R1D + X2(2,2)*R2D) 	
*	write(*,*)'c1= ',xc1
*	write(*,*)'c2= ',xc2

*********************************
* C^{mu} coefficients 
*
	
******** B coefficients

	swb0  = xb0
	swb1  = xb1
	swb00 = xb00
	swb11 = xb11

	call myBget(p21,m12,m22)
	cb0_1  = xb0
	cb1_1  = xb1
	call myBget(p2,m02,m22)
	cb0_2  = xb0
	cb1_2  = xb1
	call myBget(p1,m02,m12)
	cb0_3  = xb0
	cb1_3  = xb1

	xb0  = swb0
	xb1  = swb1
	xb00 = swb00
	xb11 = swb11
*********

	c0coef = -1d0/(2d0*detX2)*(-4d0*m02*detX2 + (f1D**2 - f1D*f2D)*p2 + (f2D**2 - f1D*f2D)*p1 + p21*f1D*f2D)
*	write(*,*)'c0coef ',c0coef
*** different cases for finite parts of C_00

c c00(0,s,0;0,0,0) or c00(0,0,s;0,0,0)

       if(    (p1.eq.0.d0).and.(p2.eq.0.d0).and.(p21.ne.0.d0).and.
     -       (m02 .eq.0.d0).and.(m12 .eq.0.d0).and.(m22 .eq.0.d0)) then
*	write(*,*)'c00(0,s,0,0,0,0)'
	xc00 = 1/4d0*(cb0_1 + 1 + 2*m02*xc0 + f1D*xc1 + f2D*xc2 + xiIR*c0coef/p21*( - IRdiv + 1d0 - LEg + Ln4pi - dlog(p21/muSc**2) ))	

	elseif(    (p1.eq.0.d0).and.(p2.ne.0.d0).and.(p21.eq.0.d0).and.
     -       (m02 .eq.0.d0).and.(m12 .eq.0.d0).and.(m22 .eq.0.d0)) then
*	write(*,*)'c00(0,0,t,0,0,0)'
*	write(*,*)'p2= ',p2
*	write(*,*)'b0= ',cb0_1
*	write(*,*)'f1',f1D
*	write(*,*)'f2',f2D
*	write(*,*)'xiIR=',xiIR
*	write(*,*)'ir= ',xiIR*c0coef/p2*( - IRdiv + 1d0 - LEg + Ln4pi - dlog(p2/muSc**2) )
	xc00 = 1/4d0*(cb0_1 + 1 + 2*m02*xc0 + f1D*xc1 + f2D*xc2 + xiIR*c0coef/p2*( - IRdiv + 1d0 - LEg + Ln4pi - dlog(p2/muSc**2) ))	


	
c c00(0,s,k2^2;0,0,q3) or c00(0,k2^2,s;0,0,q3)

      elseif((p1.eq.0.d0).and.(p2.ne.0.d0).and.(p21 .ne.0.d0).and.
     -       (m02 .eq.0.d0).and.(m12 .eq.0.d0).and.(m22 .ne.0.d0).and.
     -       (p2.ne.p21   ).and.(dabs((p2-m22)/p2).gt.1.d-4))then

	xc00 = 1/4d0*(cb0_1 + 1 + 2*m02*xc0 + f1D*xc1 + f2D*xc2 - xiIR*c0coef/(p21-p2)*cdlog(dcmplx((-p21 + m22)/(m22 - p2))) )	
	

c c00(q2^2,s,q3^2;0,q2,q3)
      elseif((dabs((p1-m12)/p1).lt.1.d-4).and.(dabs((p2-m22)/p2).lt.1.d-4).and.(p21.ne.0d0).and.
     -            (m02 .eq.0d0).and.(m12.ne.0.d0).and.(m22.ne.0.d0)  )then

	x0 = (cdsqrt(dcmplx(1d0-4d0*dsqrt(m12)*dsqrt(m22)/(p21-(dsqrt(m12)-dsqrt(m22))**2)))-1d0)/
     -       (cdsqrt(dcmplx(1d0-4d0*dsqrt(m12)*dsqrt(m22)/(p21-(dsqrt(m12)-dsqrt(m22))**2)))+1d0)	

	xc00 = 1/4d0*(cb0_1 + 1 + 2*m02*xc0 + f1D*xc1 + f2D*xc2 - xiIR*c0coef*x0/(dsqrt(m12)*dsqrt(m22)*(1d0-x0**2))*cdlog(x0) )	

      else 

	xc00 = 1/4d0*(cb0_1 + 1 + 2*m02*xc0 + f1D*xc1 + f2D*xc2)	

      endif

	R3D = 0.5d0*(cb0_1 + cb1_1 - f1D*xc1) - xc00
	R4D = 0.5d0*(cb0_1 + cb1_1 + cb1_3 - f2D*xc1)
	R5D = 0.5d0*(cb1_2 - cb1_1 - f1D*xc2)
	R6D = -0.5d0*(cb1_1 + f2D*xc2) - xc00

****
	xc11 = 1d0/detX2*(X2(1,1)*R3D + X2(1,2)*R4D) 	
	xc12 = 1d0/detX2*(X2(2,1)*R3D + X2(2,2)*R4D) 	
	xc22 = 1d0/detX2*(X2(2,1)*R5D + X2(2,2)*R6D) 	

	end


*************************************************************
	subroutine myDget(p1,p21,p32,p3,p2,p13,m02,m12,m22,m32)
*************************************************************

      include 'loopints.h'
	
	double precision m02,m12,m22,m32
	double precision p1,p2,p3,p21,p32,p13,m0,m1,m2,m3 
	double precision k12,k22,k32,k42,s,t,u,q1,q2,q3,q4
	double precision X3(3,3), detX3
	double precision f1D, f2D, f3D, d0coef, c0coef
	complex*16 R1D, R2D, R3D, R4D, R5D, R6D, R7D, R8D, R9D, R10D, sD04, x0, x1
* swap
	complex*16 swc0,swc1,swc2,swc00,swc11,swc12,swc22
	complex*16 dc0_1,dc1_1,dc2_1,dc0_2,dc1_2,dc2_2,dc0_3,dc1_3,dc2_3,dc0_4,dc1_4,dc2_4

	complex*16 test

* functions
	complex*16 C03f,D04f
* subroutines
	external myCget

	sD04 = D04f(p1,p21,p32,p3,p2,p13,dsqrt(m02),dsqrt(m12),dsqrt(m22),dsqrt(m32))

	xd0 = D04f(p1,p21,p32,p3,p2,p13,dsqrt(m02),dsqrt(m12),dsqrt(m22),dsqrt(m32))

*********************************
* D^{mu} coefficients 
*
	detX3 = (-(p13**2*p2) - p1**2*p32 + p1*((p13 - p21)*(p2 - p3) + (p13 + p2 + p21 + p3)*p32 - p32**2) - p3*(p21*(p21 + p3 - p32) + 
     -    p2*(-p21 + p32)) + p13*(-p2**2 + p21*(p3 - p32) + p2*(p21 + p3 + p32)))/4d0

	X3(1,1) = (-p2**2 - (p3 - p32)**2 + 2*p2*(p3 + p32))/4.
	X3(2,2) = (-p1**2 - (p13 - p3)**2 + 2*p1*(p13 + p3))/4.
	X3(3,3) = (-p1**2 - (p2 - p21)**2 + 2*p1*(p2 + p21))/4.

	X3(1,2) = (p1*(p2 - p3 - p32) - p13*(p2 + p3 - p32) + p3*(-p2 + 2*p21 + p3 - p32))/4.
	X3(1,3) = (2*p13*p2 + p2**2 - p2*p21 - p2*p3 - p21*p3 - p2*p32 + p21*p32 - p1*(p2 - p3 + p32))/4.
	X3(2,3) = (p1**2 - (p2 - p21)*(p13 - p3) - p1*(p13 + p2 + p21 + p3 - 2*p32))/4.

	X3(2,1) = X3(1,2)  
	X3(3,1) = X3(1,3)  
	X3(3,2) = X3(2,3)  

	f1D = p1 - m12 + m02
	f2D = p2 - m22 + m02
	f3D = p3 - m32 + m02
*	write(*,*)'p2: ', p2, ' p32: ', p32,' p3: ', p3, 'm02: ', m02, 'm22: ', m22 , ' m32: ', m32
*	R1D = 0.5d0*(C03f(p2,p32,p3,dsqrt(m02),dsqrt(m22),dsqrt(m32)) - C03f(p21,p32,p13,dsqrt(m12),dsqrt(m22),dsqrt(m32)) - f1D*sD04)
	R1D = 0.5d0*(C03f(p2,p32,p3,dsqrt(m02),dsqrt(m22),dsqrt(m32)))
	R2D = 0.5d0*(C03f(p1,p13,p3,dsqrt(m02),dsqrt(m12),dsqrt(m32)) - C03f(p21,p32,p13,dsqrt(m12),dsqrt(m22),dsqrt(m32)) - f2D*sD04)
	R3D = 0.5d0*(C03f(p1,p21,p2,dsqrt(m02),dsqrt(m12),dsqrt(m22)) - C03f(p21,p32,p13,dsqrt(m12),dsqrt(m22),dsqrt(m32)) - f3D*sD04)

****
	xd1 = 1d0/detX3*(X3(1,1)*R1D + X3(1,2)*R2D + X3(1,3)*R3D) 	
	xd2 = 1d0/detX3*(X3(2,1)*R1D + X3(2,2)*R2D + X3(2,3)*R3D) 	
	xd3 = 1d0/detX3*(X3(3,1)*R1D + X3(3,2)*R2D + X3(3,3)*R3D) 	

	
	
*	write(*,*)'co3f 1= ', C03f(p2,p32,p3,dsqrt(m02),dsqrt(m22),dsqrt(m32)) 
*	write(*,*)'co3f 2= ',C03f(p21,p32,p13,dsqrt(m12),dsqrt(m22),dsqrt(m32)) 
*	write(*,*)'dsdo4 = ', sD04
*	write(*,*)'detX3 = ', detX3
*	write(*,*)'R1D = ', R1D
*	write(*,*)'R2D = ', R2D
*	write(*,*)'R3D = ', R3D
*	write(*,*)'X3(1,1)=', X3(1,1)
*	write(*,*)'X3(2,1)=', X3(2,1)
*	write(*,*)'X3(3,1)=', X3(3,1)
*	write(*,*)'C03f(p2,p32,p3,dsqrt(m02),dsqrt(m22),dsqrt(m32))=',C03f(p2,p32,p3,dsqrt(m02),dsqrt(m22),dsqrt(m32))
*	write(*,*)'C03f(p21,p32,p13,dsqrt(m12),dsqrt(m22),dsqrt(m32))=',C03f(p21,p32,p13,dsqrt(m12),dsqrt(m22),dsqrt(m32))
*	write(*,*)'f1D=',f1D
*	write(*,*)'sD04=',sD04
*	write(*,*)'xd1 = ', xd1
*	write(*,*)'xd2 = ', xd2
	
	
*********************************
* D^{mu nu} coefficients 
*

******** C coefficients

	swc0  = xc0
	swc1  = xc1
	swc2  = xc2
	swc00 = xc00
	swc11 = xc11
	swc12 = xc12
	swc22 = xc22

	call myCget(p21,p32,p13,m12,m22,m32)
	dc0_1  = xc0
	dc1_1  = xc1
	dc2_1  = xc2
	call myCget(p2,p32,p3,m02,m22,m32)
	dc0_2  = xc0
	dc1_2  = xc1
	dc2_2  = xc2
	call myCget(p1,p13,p3,m02,m12,m32)
	dc0_3  = xc0
	dc1_3  = xc1
	dc2_3  = xc2
	call myCget(p1,p21,p2,m02,m12,m22)
	dc0_4  = xc0
	dc1_4  = xc1
	dc2_4  = xc2

	xc0  = swc0
	xc1  = swc1
	xc2  = swc2
	xc00 = swc00
	xc11 = swc11
	xc12 = swc12
	xc22 = swc22
********

	d0coef = 2*m02 - f1D*1d0/(2d0*detX3)*(X3(1,1)*f1D + X3(1,2)*f2D + X3(1,3)*f3D) -
     -                f2D*1d0/(2d0*detX3)*(X3(2,1)*f1D + X3(2,2)*f2D + X3(2,3)*f3D) -
     -                f3D*1d0/(2d0*detX3)*(X3(3,1)*f1D + X3(3,2)*f2D + X3(3,3)*f3D)

	d0coef = 2*m02 - 1d0/(2d0*detX3)*(X3(1,1)*f1D**2 + 2d0*X3(1,2)*f1D*f2D + 2d0*X3(1,3)*f1D*f3D +
     -                                  X3(2,2)*f2D**2 + 2d0*X3(2,3)*f3D*f2D + X3(3,3)*f3D**2)

	c0coef = 1d0/(2d0*detX3)*(X3(1,3)*f1D + X3(2,3)*f2D + X3(3,3)*f3D)
	

	
	if( (m32.eq.0d0).and.(m22.ne.0d0) )then
	k12 = p1
	k22 = p3
	k32 = p21
	k42 = p32
	s = p13
	t = p2
	q1 = sqrt(m02)
	q2 = sqrt(m12)
	q3 = sqrt(m32)
	q4 = sqrt(m22)
c	write(*,*)'switch: ', k12, ' ', k22, ' ',k32, ' ',k42, 'masses: ',q1,' ',q2,' ',q3,' ',q4
	
	endif
	
c d00(0,0,k2^2,k1^2,s,t;0,0,0,q2)
      	if(    (p3.ne.0d0).and.(p32.ne.0d0).and.(p1.eq.0d0).and.
     -       (p21.eq.0d0).and.(p2 .ne.0d0).and.(p13 .ne.0d0).and.
     -       (m02 .eq.0d0).and.(m12 .eq.0d0).and.(m22 .eq.0d0).and.(m32 .ne.0d0).and.(m32.ne.p3)) then

	xd00 = 1/2d0*(dc0_1 + 2*m02*xd0 + f1D*xd1 + f2D*xd2 + f3D*xd3 + 
     -       xiIR*(d0coef*1d0/(p2*(p13-m32))*(4d0 - 2d0*IRdiv + 2d0*(Ln4pi - LEg + dlog(muSc**2/p2) - 2d0*dlog((m32-p13)/(m32-p32)) )) 
     -             -2d0/(2d0*detX3)*(-X3(1,3)*f1D - X3(3,3)*f3D)*1d0/(p32-p13)*cdlog(dcmplx((-p32+m32)/(m32-p13))) 
     -             + 4d0 - 2d0*IRdiv - 2d0*(LEg - Ln4pi + dlog(p2/muSc**2)) ))	

c d00(0,0,m22,m12,s,t;0,0,0,m1)
     	elseif(    (k12.eq.0.d0).and.(k22.eq.0.d0).and.(k32.ne.0.d0).and.
     -       (k42.ne.0.d0).and.(s  .ne.0.d0).and.(t  .ne.0.d0).and.
     -       (q1 .eq.0.d0).and.(q2 .eq.0.d0).and.(q3 .eq.0.d0).and.
     -       (q4 .ne.0.d0).and.(k42.eq.q4**2))then
     
	xd00 = 1/2d0*(dc0_1 + 2*m02*xd0 + f1D*xd1 + f2D*xd2 + f3D*xd3)
c    	write(*,*)'test d00: ', xd00
     
     	
c d00(0,m1^2,0,m2^2,t,u;0,0,m1,m1)
c IR part wrong, put xiIR equal to 0
c      elseif(    (p3.ne.0d0).and.(p32.eq.0d0).and.(p1.eq.0d0).and.
c     -       (p21.ne.0d0).and.(p2 .ne.0d0).and.(p13 .ne.0d0).and.
c     -       (m02 .eq.0d0).and.(m12 .eq.0d0).and.(m22 .ne.0d0).and.(m32 .ne.0d0)) then

c	xd00 = 1/2d0*(dc0_1 + 2*m02*xd0 + f1D*xd1 + f2D*xd2 + f3D*xd3 + 
c     -       xiIR*(1.0d0)	)

c d00(m^2,m^2,k1^2,k2^2,s,t;m,0,m,M)
      elseif((p32.ne.0d0).and.(p3.ne.0d0).and.(dabs((p1-m02)/p1).lt.1.d-4).and.
     -       (dabs((p21-m22)/p21).lt.1.d-4).and.(p2 .ne.0d0).and.(p13 .ne.0d0).and.
     -       (m02 .ne.0d0).and.(m22 .ne.0d0).and.(m32 .ne.0d0  ).and.
     -       (m12 .eq.0d0)) then

	x0 = -(1d0-cdsqrt(dcmplx(1d0-4d0*m02/p2)))/
     -		  (1d0+cdsqrt(dcmplx(1d0-4d0*m02/p2)))
	x1 = (cdsqrt(dcmplx(1d0-4d0*m02/p21))-1d0)/(cdsqrt(dcmplx(1d0-4d0*m02/p21))+1d0)	

	xd00 = 1/2d0*(dc0_1 + 2*m02*xd0 + f1D*xd1 + f2D*xd2 + f3D*xd3 + 
     -       xiIR*(d0coef*(-1d0)*x0/(m02*(p13 - m32)*(1d0 - x0**2))*cdlog(dcmplx(x0))
     -          +  c0coef*(-1d0)*x0/(m02*            (1d0 - x0**2))*cdlog(dcmplx(x0)) ))	

c     -       xiIR*(d0coef*(-2d0)*x0/(m02*(p13 - m32)*(1d0 - x0**2))*cdlog(dcmplx(x0))
c     -            - 2d0/(2d0*detX3)*(X3(1,3)*f1D + X3(2,3)*f2D + X3(3,3)*f3D)*x1/(m02*(1d0-x1**2))*cdlog(x1) ))	

	test = d0coef*(-2d0)*x0/(m02*(p13 - m32)*(1d0 - x0**2))*cdlog(dcmplx(x0))

	test = 2d0/(2d0*detX3)*(X3(1,3)*f1D + X3(2,3)*f2D + X3(3,3)*f3D)*x1/(m02*(1d0-x1**2))*cdlog(x1)

	else
	
	xd00 = 1/2d0*(dc0_1 + 2*m02*xd0 + f1D*xd1 + f2D*xd2 + f3D*xd3)	
	
	endif
	
	R4D  = 0.5d0*(dc0_1 + dc1_1 + dc2_1         - f1D*xd1) - xd00
	R5D  = 0.5d0*(dc1_3 + dc0_1 + dc1_1 + dc2_1 - f2D*xd1)
	R6D  = 0.5d0*(dc1_4 + dc0_1 + dc1_1 + dc2_1 - f3D*xd1)
	R7D  = 0.5d0*(dc1_2 - dc1_1                 - f1D*xd2)
	R8D  = 0.5d0*(      - dc1_1                 - f2D*xd2) - xd00
	R9D  = 0.5d0*(dc2_4 - dc1_1                 - f3D*xd2)
	R10D = 0.5d0*(dc2_2 - dc2_1                 - f1D*xd3)
	R11D = 0.5d0*(dc2_3 - dc2_1                 - f2D*xd3)
	R12D = 0.5d0*(      - dc2_1                 - f3D*xd3) - xd00

	xd11 = 1d0/detX3*(X3(1,1)*R4D + X3(1,2)*R5D + X3(1,3)*R6D) 	
	xd12 = 1d0/detX3*(X3(2,1)*R4D + X3(2,2)*R5D + X3(2,3)*R6D) 	
	xd13 = 1d0/detX3*(X3(3,1)*R4D + X3(3,2)*R5D + X3(3,3)*R6D) 	

c	xd12 = 1d0/detX3*(X3(1,1)*R7D + X3(1,2)*R8D + X3(1,3)*R9D) 	
	xd22 = 1d0/detX3*(X3(2,1)*R7D + X3(2,2)*R8D + X3(2,3)*R9D) 	
	xd23 = 1d0/detX3*(X3(3,1)*R7D + X3(3,2)*R8D + X3(3,3)*R9D) 	

c	xd13 = 1d0/detX3*(X3(1,1)*R10D + X3(1,2)*R11D + X3(1,3)*R12D) 	
c	xd23 = 1d0/detX3*(X3(2,1)*R10D + X3(2,2)*R11D + X3(2,3)*R12D) 	
	xd33 = 1d0/detX3*(X3(3,1)*R10D + X3(3,2)*R11D + X3(3,3)*R12D) 	

	end

************************************************************************
      function	A01(q1)
************************************************************************

      implicit	real*8(a-h,k-z)
      implicit	integer(i)
      implicit	character*60(j)

      include 'loopints.h'

* setting the renorm. scale
	q = muSc

c a0(0)
      if(q1.eq.0.d0)then
        A01	= 0.d0
c a0(m)
      else
        A01	= q1**2*UVdiv + q1**2*(1.d0-dlog(q1**2/q**2))
      endif

      return
      end


************************************************************************
        function  B02(k2,q1,q2)
************************************************************************

	implicit  real*8(a-h,k-z)
	implicit  integer(i)
	implicit  character*60(j)
	complex*16 xp,xm
	complex*16 B02

      include 'loopints.h'

* setting the renorm. scale
	q = muSc


c b0(0;0,0)
      if(    (k2.eq.0.d0).and.(q1.eq.0.d0).and.(q2.eq.0.d0))then
        B02   = UVdiv + IRdiv +LEg -Ln4pi
c b0(0;0,m) & b0(0;m,0)
      elseif(((k2.eq.0.d0).and.((q1.eq.0.d0).and.(q2.ne.0.d0))).or.
     -       ((k2.eq.0.d0).and.((q2.eq.0.d0).and.(q1.ne.0.d0))))then
	qq2 = q1**2 + q2**2
        B02   = UVdiv + 1 + (-dlog(qq2/q**2))
c b0(0;m,m)
      elseif((k2.eq.0.d0).and.(q1.ne.0.d0).and.(q2.ne.0.d0).and.
     -       (q1.eq.q2  ))then
        B02   = UVdiv + (-dlog(q1**2/q**2))
c b0(0;q1,q2)
      elseif((k2.eq.0.d0).and.(q1.ne.0.d0).and.(q2.ne.0.d0).and.
     -       (q1.ne.q2  ))then
        B02   = UVdiv + (1.d0-q1**2/(q1**2-q2**2)*dlog(q1**2/q**2)
     -                      -q2**2/(q2**2-q1**2)*dlog(q2**2/q**2))
c b0(k^2;0,0), k^2 > 0
      elseif((k2.ne.0.d0).and.(q1.eq.0.d0).and.(q2.eq.0.d0))then
        B02   = UVdiv + (2.d0-dlog(k2/q**2)) + (0D0, 1D0)*Lpi
c b0(m^2;m,0)
      elseif((k2.ne.0.d0).and.(q1.ne.0.d0).and.(q2.eq.0.d0).and.
     -       (dabs((k2-q1**2)/k2).lt.1.d-4))then
        B02   = UVdiv + (2.d0-dlog(k2/q**2))
c b0(m^2;0,m)
      elseif((k2.ne.0.d0).and.(q1.eq.0.d0).and.(q2.ne.0.d0).and.
     -       (dabs((k2-q2**2)/k2).lt.1.d-4))then
        B02   = UVdiv + (2.d0-dlog(k2/q**2))
c b0(k^2;q1,0)
      elseif((k2.ne.0.d0).and.(q1.ne.0.d0).and.(q2.eq.0.d0).and.
     -       (dabs((k2-q1**2)/k2).gt.1.d-4))then
* complex part
	  if (k2.gt.q1**2) then	
            B02   = UVdiv + (2.d0-(k2-q1**2)/k2*dlog(dabs(q1**2-k2)/q1**2)
     -         - dlog(     q1**2    / q**2)) + (0D0, 1D0)*Lpi*(k2-q1**2)/k2
	  else
            B02   = UVdiv + (2.d0-(k2-q1**2)/k2*dlog(dabs(q1**2-k2)/q1**2)
     -                      -              dlog(     q1**2    / q**2))
	  endif
c b0(k^2;0,q2)
      elseif((k2.ne.0.d0).and.(q1.eq.0.d0).and.(q2.ne.0.d0).and.
     -       (dabs((k2-q2**2)/k2).gt.1.d-4))then
* complex part
	  if (k2.gt.q2**2) then	
            B02   = UVdiv + (2.d0-(k2-q2**2)/k2*dlog(dabs(q2**2-k2)/q2**2)
     -         - dlog(     q2**2    / q**2)) + (0D0, 1D0)*Lpi*(k2-q2**2)/k2
	  else
            B02   = UVdiv + (2.d0-(k2-q2**2)/k2*dlog(dabs(q2**2-k2)/q2**2)
     -                      -              dlog(     q2**2    / q**2))
	  endif
c b0(k^2;q1,q2)
      elseif((k2.ne.0.d0).and.(q1.ne.0.d0).and.(q2.ne.0.d0))then
        xp      = (k2+q2**2-q1**2)/(2.d0*k2)+cdsqrt(dcmplx(
     -            ((k2+q2**2-q1**2)/(2.d0*k2))**2-q2**2/k2))
        xm      = (k2+q2**2-q1**2)/(2.d0*k2)-cdsqrt(dcmplx(
     -            ((k2+q2**2-q1**2)/(2.d0*k2))**2-q2**2/k2))
	kappa = cdsqrt(dcmplx( (k2-q2**2-q1**2)**2-4d0*q1**2*q2**2))
	thmass = (q1+q2)**2
* complex part
 	if (k2.gt.thmass) then
           B02   = UVdiv + (2.d0+dreal(xp*cdlog(1.d0-1.d0/xp))
     -                      +dreal(xm*cdlog(1.d0-1.d0/xm))
     -                      -dlog(q1**2/q**2))
     -           + (0D0, 1D0)*Lpi*kappa/k2
	else
           B02   = UVdiv + (2.d0+dreal(xp*cdlog(1.d0-1.d0/xp))
     -                      +dreal(xm*cdlog(1.d0-1.d0/xm))
     -                      -dlog(q1**2/q**2))
	endif
c Error Message
      else
        write(6,*)'B02 (',sngl(dsqrt(dabs(k2))),',',sngl(q1),',',
     -             sngl(q2),') NOT YET IMPLEMENTED !'
      endif

      return
      end

************************************************************************
        function  B02p(k2,q1,q2)
************************************************************************

	implicit real*8(a-h,k-z)
	implicit integer(i)
	implicit character*60(j)
	complex*16 xp,xm
	complex*16 B02p

      include 'loopints.h'

* setting the renorm. scale
	q = muSc

c b0'(0;0,0)
      if(    (k2.eq.0.d0).and.(q1.eq.0.d0).and.(q2.eq.0.d0))then
        B02p	= 0.d0
c b0'(0;m,m)
      elseif((k2.eq.0.d0).and.(q1.ne.0.d0).and.(q2.ne.0.d0).and.
     -       (q1.eq.q2  ))then
        B02p	= (1.d0/(6.d0*q1**2))
c b0'(0;q1,q2)
      elseif((k2.eq.0.d0).and.(q1.ne.0.d0).and.(q2.ne.0.d0).and.
     -       (q1.ne.q2  ))then
        B02p	= ((q1**2+q2**2)/2.d0-q1**2*q2**2/(q1**2-q2**2)*
     -		  dlog(q1**2/q2**2))/(q1**2-q2**2)**2
c b0'(k^2;0,0), k^2 > 0
      elseif((k2.ne.0.d0).and.(q1.eq.0.d0).and.(q2.eq.0.d0))then
        write(6,*)'b0p(k^2;0,0) not implemented.'
c b0'(m^2;m,0)
      elseif((k2.ne.0.d0).and.(q1.ne.0.d0).and.(q2.eq.0.d0).and.
     -       (dabs((k2-q1**2)/k2).lt.1.d-4))then
        B02p	= 1/(2d0*k2)*xiIR*(IRdiv + LEg - Ln4pi - dlog(q**2)) + 
     -            (-1.d0+1.d0/2.d0*dlog(k2))/k2
c b0'(m^2;0,m)
      elseif((k2.ne.0.d0).and.(q1.eq.0.d0).and.(q2.ne.0.d0).and.
     -       (dabs((k2-q2**2)/k2).lt.1.d-4))then
        B02p	= 1/(2d0*k2)*xiIR*(IRdiv + LEg - Ln4pi - dlog(q**2)) + 
     -            (-1.d0+1.d0/2.d0*dlog(k2))/k2
c b0'(k^2;q1,0)
      elseif((k2.ne.0.d0).and.(q1.ne.0.d0).and.(q2.eq.0.d0).and.
     -       (dabs((k2-q1**2)/k2).gt.1.d-4))then
* complex part
	  if (k2.gt.q1**2) then	
           B02p = (-1.d0-q1**2/k2*dlog(dabs(q1**2-k2)/q1**2))/k2
     -            + (0D0, 1D0)*Lpi*q1**2/k2**2
	  else
           B02p = (-1.d0-q1**2/k2*dlog(dabs(q1**2-k2)/q1**2))/k2
	  endif
c b0'(k^2;0,q2)
      elseif((k2.ne.0.d0).and.(q1.eq.0.d0).and.(q2.ne.0.d0).and.
     -       (dabs((k2-q2**2)/k2).gt.1.d-4))then
* complex part
	  if (k2.gt.q2**2) then	
           B02p = (-1.d0-q2**2/k2*dlog(dabs(q2**2-k2)/q2**2))/k2
     -            + (0D0, 1D0)*Lpi*q2**2/k2**2
	  else
           B02p = (-1.d0-q2**2/k2*dlog(dabs(q2**2-k2)/q2**2))/k2
	  endif
c b0'(k^2;q1,q2)
      elseif((k2.ne.0.d0).and.(q1.ne.0.d0).and.(q2.ne.0.d0))then
        xp	= (k2+q2**2-q1**2)/(2.d0*k2)+cdsqrt(dcmplx(
     -		  ((k2+q2**2-q1**2)/(2.d0*k2))**2-q2**2/k2))
        xm	= (k2+q2**2-q1**2)/(2.d0*k2)-cdsqrt(dcmplx(
     -		  ((k2+q2**2-q1**2)/(2.d0*k2))**2-q2**2/k2))
	kappa = cdsqrt(dcmplx( (k2-q2**2-q1**2)**2-4d0*q1**2*q2**2))
	thmass = (q1+q2)**2
* complex part
 	if (k2.gt.thmass) then
          B02p = (-1.d0/k2+dreal(((xp-xp**2)*cdlog(1.d0-1.d0/xp)
     -		                       -(xm-xm**2)*cdlog(1.d0-1.d0/xm))/
     -		                       (k2*(xp-xm))))
     -         + (0D0, 1D0)*Lpi*(k2*(q1**2+q2**2)-(q1**2-q2**2)**2)/(kappa*k2**2)   
	else
          B02p = (-1.d0/k2+dreal(((xp-xp**2)*cdlog(1.d0-1.d0/xp)
     -		                       -(xm-xm**2)*cdlog(1.d0-1.d0/xm))/
     -		                       (k2*(xp-xm))))
	endif
c Error Message
      else
        write(6,*)'B02p(',sngl(dsqrt(dabs(k2))),',',sngl(q1),',',
     -             sngl(q2),') NOT YET IMPLEMENTED !'
      endif

      return
      end

************************************************************************
      double complex function  C03f(p1,p0,p2,mm0,mm1,mm2)
************************************************************************
c      implicit	real*8(a-h,k-z)
c      implicit	integer(i)
c      implicit	character*60(j)

      double precision p1,p0,p2,mm0,mm1,mm2		
      double precision k12,s,ss,k22,q1,q2,q3,q,q123		
      double precision old1,old2,old3,oldM1,oldM2,oldM3		

      complex*16 x0,x1,x2,x3,f1,f2,C03,cspen,ckappa

      include 'loopints.h'

* setting the renorm. scale
	q = muSc

	k12 = p1
	s   = p0
	k22 = p2
	q1  = mm0
	q2  = mm1
	q3  = mm2
	
***************************
* 1. only real part of C0
* 2. IRdiv = 2/(D-4)
* 3. xiIR is there to set the whole Delta_IR to zero

**** argument rotation so that zero masses start at q1

	q123 = q1*q2*q3

	if (q123.eq.0d0) then

	  old1 = k12
	  old2 = s
	  old3 = k22

	  oldM1 = q1
	  oldM2 = q2
	  oldM3 = q3
	
	 if ((q1.eq.0d0).and.(q2.ne.0d0).and.(q3.eq.0d0)) then

	  k12 = old3
	  k22 = old1
	  q2  = oldM3
	  q3  = oldM2
	
	 elseif( (q1.ne.0d0).and.(q2.ne.0d0).and.(q3.eq.0d0) ) then

	  k12 = old2
	  s = old1
	  q1  = oldM3
	  q3  = oldM1	  
	
	 elseif( (q1.ne.0d0).and.(q2.eq.0d0).and.(q3.ne.0d0) ) then

	  k22 = old2
	  s = old3
	  q1  = oldM2
	  q2  = oldM1	  

	 elseif( (q1.ne.0d0).and.(q2.eq.0d0).and.(q3.eq.0d0) ) then

	  k12 = old2
	  s = old3
	  k22 = old1
	  q1  = oldM2
	  q2  = oldM3	  
	  q3  = oldM1	  
	 endif
	endif

*	write(*,*)'k12= ', k12, ' s= ',s, 'k22= ',k22, ' q1 = ',q1, ' q2 = ', q2, ' q3 = ', q3 	

c c0(0,s,0;0,0,0)  or c0(0,0,s;0,0,0)
       if(    (k12.eq.0.d0).and.(k22.eq.0.d0).and.(s .ne.0.d0).and.
     -       (q1 .eq.0.d0).and.(q2 .eq.0.d0).and.(q3 .eq.0.d0))then
        C03f	= 1d0/s*( IRdiv**2 + IRdiv*(LEg - Ln4pi + dlog(s/q**2)) +
     -           1d0/2d0*(LEg - Ln4pi + dlog(s/q**2) )**2 - 7d0/12d0*Lpi**2 )
     	
     	elseif(    (k12.eq.0.d0).and.(k22.ne.0.d0).and.(s .eq.0.d0).and.
     -       (q1 .eq.0.d0).and.(q2 .eq.0.d0).and.(q3 .eq.0.d0))then
     	s = k22
        C03f	= 1d0/s*( IRdiv**2 + IRdiv*(LEg - Ln4pi + dlog(s/q**2)) +
     -           1d0/2d0*(LEg - Ln4pi + dlog(s/q**2) )**2 - 7d0/12d0*Lpi**2 )

c c0(0,p22,p32:0,0,0) cf hep-ph 0712.1851v2
	elseif( (k12.eq.0.d0).and.(k22.ne.0.d0).and.(s.ne.0.d0).and.
     -       (q1 .eq.0.d0).and.(q2 .eq.0.d0).and.(q3 .eq.0.d0) )then
     	C03f	=  1d0/(s-k22)*( IRdiv*(cdlog(dcmplx(-k22/-s))) + 1d0/2d0*(cdlog(dcmplx(-s/q*q))**2 - cdlog(dcmplx(-k22/q*q))**2  ) )
	
	elseif( (k12.ne.0.d0).and.(k22.eq.0.d0).and.(s.ne.0.d0).and.
     -       (q1 .eq.0.d0).and.(q2 .eq.0.d0).and.(q3 .eq.0.d0) )then
     	k22 = k12
	C03f	=  1d0/(s-k22)*( IRdiv*(cdlog(dcmplx(-k22/-s))) + 1d0/2d0*(cdlog(dcmplx(-s/q*q))**2 - cdlog(dcmplx(-k22/q*q))**2  ) )
*     	write(*,*)'c0=', C03f
	
c c0(0,s,0;q1,q2,q3) 
! complex part wrong
      elseif((k12.eq.0.d0).and.(k22.eq.0.d0).and.(s  .ne.0.d0).and.
     -       (q1 .ne.0.d0).and.(q2 .ne.0.d0).and.(q3 .ne.0.d0).and.
     -       (q1 .ne.q3  )  )then
        x0  = 1d0 - (q2**2-q1**2)/s
        x1  = (s - q2**2 + q3**2 + ckappa(s,q2**2,q3**2))/(2d0*s)
        x2  = (s - q2**2 + q3**2 - ckappa(s,q2**2,q3**2))/(2d0*s)
        x3  = q3**2/(q3**2-q1**2)

        C03f = 1d0/s*( cspen(dcmplx( (x0-1d0)/(x0 - x1) )) - cspen(dcmplx( (x0)/(x0 - x1) )) +
     -                 cspen(dcmplx( (x0-1d0)/(x0 - x2) )) - cspen(dcmplx( (x0)/(x0 - x2) )) -
     -                 cspen(dcmplx( (x0-1d0)/(x0 - x3) )) + cspen(dcmplx( (x0)/(x0 - x3) )) )
c c0(0,s,0;q1,q2,q1)
! complex part wrong
      elseif((k12.eq.0.d0).and.(k22.eq.0.d0).and.(s  .ne.0.d0).and.
     -       (q1 .ne.0.d0).and.(q2 .ne.0.d0).and.(q3 .ne.0.d0).and.
     -       (q1 .eq.q3  )  )then
        x0  = 1d0 - (q2**2-q1**2)/s
        x1  = (s - q2**2 + q3**2 + ckappa(s,q2**2,q3**2))/(2d0*s)
        x2  = (s - q2**2 + q3**2 - ckappa(s,q2**2,q3**2))/(2d0*s)

        C03f = 1d0/s*( cspen(dcmplx( 1d0 - q2**2/q3**2 )) - cspen(dcmplx( 1d0/x1 )) - cspen(dcmplx( 1d0/x2 )) )

c c0(q2^2,s,q3^2;0,q2,q3)
      elseif((dabs((k12-q2**2)/k12).lt.1.d-4).and.(dabs((k22-q3**2)/k22).lt.1.d-4).and.(s.ne.0d0).and.
     -            (q1 .eq.0d0).and.(q2.ne.0.d0).and.(q3.ne.0.d0)  )then

	x0 = (cdsqrt(dcmplx(1d0-4d0*q2*q3/(s-(q2-q3)**2)))-1d0)/(cdsqrt(dcmplx(1d0-4d0*q2*q3/(s-(q2-q3)**2)))+1d0)	

        C03f = x0/(q2*q3*(1d0-x0**2))*(xiIR*(IRdiv + LEg - Ln4pi - dlog(muSc**2))*cdlog(x0) +
     -         cdlog(x0)*dlog(q2*q3) - 0.5d0*cdlog(x0)**2 + 2d0*cdlog(x0)*cdlog(1d0-x0**2) +
     -         0.5d0*dlog(q2/q3)**2 - Lpi**2/6d0 + cspen(x0**2) + cspen(1d0 - x0*q2/q3) + cspen(1d0 - x0*q3/q2))
         
c----------------------------------------------------------------------------------------------------------------------	
c c0(0,s,k2^2;0,0,q3) k2^2.eq.q3(here s=t<0)
	elseif((k12.eq.0.d0).and.(k22.ne.0.d0).and.(s  .ne.0.d0).and.
     -       (q1 .eq.0.d0).and.(q2 .eq.0.d0).and.(q3 .ne.0.d0).and.
     -       (k22.ne.s).and.(k22.eq.q3**2))then	
     	C03f	= IRdiv**2/(2.*(-q3**2 + s)) + (IRdiv*
     -     (-LEg + Ln4pi + 2*Cdlog(Dcmplx(q3**2/(q3**2 - s))) + Cdlog(Dcmplx(scale**2/q3**2)))
     -     )/(2.*(q3**2 - s)) - (6*(LEg - Ln4pi)**2 + Pi**2 + 
     -     12*Cdlog(Dcmplx(q3**2/(q3**2 - s)))**2 + 
     -     24*Cdlog(Dcmplx(q3**2/(q3**2 - s)))*
     -      (-LEg + Ln4pi + Cdlog(Dcmplx(scale**2/q3**2))) + 
     -     6*Cdlog(Dcmplx(scale**2/q3**2))*
     -      (-2*LEg + 2*Ln4pi + Cdlog(Dcmplx(scale**2/q3**2))) - 
     -     24*Cspen(Dcmplx(-(s/(q3**2 - s)))))/(24.*(q3**2 - s))
*	write(*,*)'new code for c0(0,s,k22;0,0,q3) if k22 eq q3 =',C03f

c c0(0,s,k2^2;0,0,q3)  for s eq q3**2
	elseif((k12.eq.0.d0).and.(k22.ne.0.d0).and.(s  .ne.0.d0).and.
     -       (q1 .eq.0.d0).and.(q2 .eq.0.d0).and.(q3 .ne.0.d0).and.(s.eq.q3**2))then

c    twist
	k12 = k22
	k22 = s
	s = k12
	k12 = 0	
     	C03f =	IRdiv**2/(2.*(-q3**2 + s)) + (IRdiv*
     -     (-LEg + Ln4pi + 2*Cdlog(Dcmplx(q3**2/(q3**2 - s))) + Cdlog(Dcmplx(scale**2/q3**2)))
     -     )/(2.*(q3**2 - s)) - (6*(LEg - Ln4pi)**2 + Pi**2 + 
     -     12*Cdlog(Dcmplx(q3**2/(q3**2 - s)))**2 + 
     -     24*Cdlog(Dcmplx(q3**2/(q3**2 - s)))*
     -      (-LEg + Ln4pi + Cdlog(Dcmplx(scale**2/q3**2))) + 
     -     6*Cdlog(Dcmplx(scale**2/q3**2))*
     -      (-2*LEg + 2*Ln4pi + Cdlog(Dcmplx(scale**2/q3**2))) - 
     -     24*Cspen(Dcmplx(-(s/(q3**2 - s)))))/(24.*(q3**2 - s))
*     	write(*,*)'twisted new code for c0(0,k22,s;0,0,q3) if p0 eq q3 =',C03f 
c----------------------------------------------------------------------------------------------------------------------	


c c0(0,s,k2^2;0,0,q3) k2^2.ne.q3

      elseif((k12.eq.0.d0).and.(k22.ne.0.d0).and.(s  .ne.0.d0).and.
     -       (q1 .eq.0.d0).and.(q2 .eq.0.d0).and.(q3 .ne.0.d0).and.
     -       (k22.ne.s   ).and.(dabs((k22-q3**2)/k22).gt.1.d-4).and.(dabs((s-q3**2)/s).gt.1.d-4))then
   
*     	write(*,*)'here: k12 = ', k12, ' k22=', k22, 's= ', s, 'q3= ', q3
*	write(*,*)'dabs((k22-q3**2)/k22)=',dabs((k22-q3**2)/k22)
        C03f	= 1d0/(s-k22)*(xiIR*(IRdiv + LEg - Ln4pi - cdlog(dcmplx(muSc**2)))*cdlog(dcmplx((-s+q3**2)/(q3**2-k22)))
     -                              +cspen(dcmplx(     s  /q3**2))
     -		                    -cspen(dcmplx(     k22/q3**2))
     -		                    +cdlog(dcmplx(1.d0-s  /q3**2))**2
     -		                    -cdlog(dcmplx(1.d0-k22/q3**2))**2
     -		                    +cdlog(dcmplx(   q3**2))*
     -		                     cdlog(dcmplx((-s+q3**2)/
     -		                                  (q3**2-k22))))
*   	write(*,*)'c0=',C03f    
     
c c0(s,m^2,m^2;0,0,q3)
      elseif((dabs((k22-s)/k22).lt.1.d-4).and.(k12 .ne.0.d0).and.
     -       (q1 .eq.0.d0).and.(q2 .eq.0.d0).and.(q3 .ne.0.d0))then
	x0 = (k12 - ckappa(s,k12,k22))/(k12 + ckappa(s,k12,k22))

        C03f	= 1d0/(ckappa(s,k12,k22))*(-2d0*cspen(dcmplx( 1d0 -  (1d0-q3**2/k22)/(1d0 + x0) )) 
     -                                     +2d0*cspen(dcmplx( 1d0 -  (1d0-q3**2/k22)/(1d0 + 1d0/x0) )) 
     -                                     -    cspen(dcmplx( 1d0 +  (q3**2*x0)/k22 )) 
     -                                     +    cspen(dcmplx( 1d0 +  (q3**2)/(k22*x0) )) 
     -                                     +    cdlog(dcmplx( x0 ))**2 
     -                                     -2d0*cdlog(dcmplx( x0 ))*cdlog(dcmplx(1d0 + x0 )) )

c c0(k12,0,k22;0,q2,q3)
      elseif((k12.ne.0d0).and.(k22.ne.0d0).and.(s  .eq.0d0).and.
     -       (q1 .eq.0d0).and.(q2 .ne.0d0).and.(q3 .ne.0d0))then
        C03f = C03(0d0,k12,k22,q3,q2,0d0)

c c0(q2,s,k2^2;0,q2,q3)
      elseif((dabs((k12-q2**2)/k12).lt.1.d-4).and.(k22.ne.0.d0).and.(s  .ne.0.d0).and.
     -       (q1 .eq.0.d0).and.(q2 .ne.0.d0).and.(q3 .ne.0.d0))then
        C03f = C03(k12,s,k22,0d0,q2,q3)

c c0(k1^2,s,k1^2;0,q2,q3)
      elseif((dabs((k12-k22)/k22).lt.1.d-4).and.(k22.ne.0.d0).and.(s  .ne.0.d0).and.
     -       (q1 .eq.0.d0).and.(q2 .ne.0.d0).and.(q3 .ne.0.d0))then
        C03f = C03(k12,s,k22,0d0,q2,q3)

c c0(k1^2,s,k2^2;0,0,q3)
	elseif((k12.ne.0d0).and.(k22.ne.0d0).and.(s .ne.0d0).and.
     -       (q1 .eq.0d0).and.(q2 .eq.0d0).and.(q3 .ne.0d0))then
        C03f = C03(k12,s,k22,0d0,0d0,q3)
*	write(*,*)'c03=',C03f

c c0(k1^2,s,k2^2;q1,q2,q3)
      elseif((k12.ne.0.d0).and.(k22.ne.0.d0).and.(s  .ne.0.d0).and.
     -       (q1 .ne.0.d0).and.(q2 .ne.0.d0).and.(q3 .ne.0.d0))then
        C03f = C03(k12,s,k22,q1,q2,q3)	
	
	
c Error Message
      else
        write(6,*)'C03f (',sngl(dsqrt(dabs(k12))),',',
     .                      sngl(dsqrt(dabs(k22))),',',
     .                      sngl(dsqrt(dabs(s  ))),',',
     .                      sngl(q1              ),',',
     .                      sngl(q2              ),',',
     .                      sngl(q3              ),
     .                    ') NOT YET IMPLEMENTED !'
      endif

      return
      end

************************************************************************
      function  D04f(p1,p21,p32,p3,p2,p13,mm1,mm2,mm3,mm4)
************************************************************************

      double precision p1,p21,p32,p3,p2,p13,mm1,mm2,mm3,mm4
      double precision k12,k22,k32,k42,s,t,u,q1,q2,q3,q4,q		

      complex*16 x0,x1,x2,x3,f1,f2,D04,cspen,D04f,ckappa
      complex*16 test

      include 'loopints.h'

* setting the renorm. scale
	q = muSc

***************************
* 1. only real part of D0
* 2. IRdiv = 2/(D-4)
* 3. xiIR is there to set the whole Delta_IR to zero

	
	if (mm4.ne.0d0) then
	k12 = p1
	k22 = p21	
	k32 = p32
	k42 = p3
	s   = p2
	t   = p13
	q1  = mm1
	q2  = mm2
	q3  = mm3
	q4  = mm4
	
	elseif( (mm4.eq.0d0).and.(mm3.ne.0d0) )then
	k12 = p1
	k22 = p3
	k32 = p21
	k42 = p32
	s = p13
	t = p2
	q1 = mm1
	q2 = mm2
	q3 = mm4
	q4 = mm3
c	write(*,*)'switch: ', k12, ' ', k22, ' ',k32, ' ',k42, 'masses: ',q1,' ',q2,' ',q3,' ',q4
	
	endif
 	
	
c d0(0,0,k2^2,k1^2,s,t;0,0,0,q2)
      if(    (k12.ne.0.d0).and.(k22.ne.0.d0).and.(k32.eq.0.d0).and.
     -       (k42.eq.0.d0).and.(s  .ne.0.d0).and.(t  .ne.0.d0).and.
     -       (q1 .eq.0.d0).and.(q2 .ne.0.d0).and.(q3 .eq.0.d0).and.
     -       (q4 .eq.0.d0).and.(k12.ne.q2**2))then
	write(*,*)'A'
	write(*,*)'q3=',q3
        D04f	= 1d0/s/(t-q2**2)*(
     -    xiIR*(IRdiv**2 - IRdiv*(Ln4pi - LEg + dlog(muSc**2/s) - 2d0*dlog((q2**2-t)/(q2**2-k12)) ) + 
     -    1/2d0*(Ln4pi - LEg + dlog(muSc**2/s) - 2d0*dlog((q2**2-t)/(q2**2-k12)) )**2 - 1/2d0*(dlog(s/k12) + 2d0*dlog((q2**2-t)/(q2**2-k12)) )**2)
     -		   -2.d0*cspen(dcmplx(1.d0+(q2**2-k12)/(t-q2**2)))
     -		   -2.d0*cspen(dcmplx(1.d0+(q2**2-k22)/(t-q2**2)))
     -		   -     cspen(dcmplx(1.d0+(q2**2-k12)*(q2**2-k22)/
     -		                      s/q2**2))
     -		   -pi**2/4.d0
     -		   +     cdlog(dcmplx( s         /q **2))**2/2.d0
     -		   -     cdlog(dcmplx( s         /q2**2))**2/2.d0
     -		   +2.d0*cdlog(dcmplx( s         /q **2))*
     -		         cdlog(dcmplx((t  -q2**2)/q2**2))
     -		   -     cdlog(dcmplx((q2**2-k12)/q **2))*
     -		         cdlog(dcmplx((q2**2-k12)/q2**2))
     -		   -     cdlog(dcmplx((q2**2-k22)/q **2))*
     -		         cdlog(dcmplx((q2**2-k22)/q2**2)))
     

c d0(0,0,m2^2,m1^2,s,t;0,0,0,m1) see Hoepker 6.75
      elseif(    (k12.eq.0.d0).and.(k22.eq.0.d0).and.(k32.ne.0.d0).and.
     -       (k42.ne.0.d0).and.(s  .ne.0.d0).and.(t  .ne.0.d0).and.
     -       (q1 .eq.0.d0).and.(q2 .eq.0.d0).and.(q3 .eq.0.d0).and.
     -       (q4 .ne.0.d0).and.(k42.eq.q4**2))then
     	m12 = q4**2
	m22 = k32
c	write(*,*)'m12 = ', m12, 'm22 = ', m22
        D04f	= 1d0/(s*(t-m12))*(xiIR*(
     e    	3d0*IRdiv**2/2d0 
     e		- IRdiv*( 2d0*cdlog(dcmplx((m12-t)/(q4*q))) + cdlog(dcmplx(s/q*q)) - cdlog(dcmplx((m12-m22)/(q4*q))) )
     e		- 2d0*cspen(dcmplx((t-m22)/(t-m12))) + 2d0*cdlog(dcmplx(s/q*q))*cdlog(dcmplx((m12-t)/(q4*q))) - cdlog(dcmplx((m12-m22)/(q4*q)))**2d0 	
     e		-(13d0/24d0)*pi**2 ) )
c	write(*,*)'you are here and d0 is ', D04f

c tbox correction see Hoepker thesis 6.78
c d0(0,m1^1,0,m2^2,t,u;0,0,m1,m1) switch m1 and m2
	elseif(    (k12.eq.0.d0).and.(k22.ne.0.d0).and.(k32.eq.0.d0).and.
     -       (k42.ne.0.d0).and.(p2  .ne.0.d0).and.(p13 .ne.0.d0).and.
     -       (q1 .eq.0.d0).and.(q2 .eq.0.d0).and.(q3 .ne.0.d0).and.
     -       (q4 .ne.0.d0).and.(q3.eq.q4))then
c   	write(*,*)'you are here'
	u = p13
	t = p2
	m12 = q4**2
	m22 = k22
c	write(*,*)'m12 = ', m12, ' m22 = ', m22, ' q= ', q
	D04f	= 1d0/((u-m12)*(t-m12))*( 
     -		xiIR*( IRdiv**2/2d0 - IRdiv*( cdlog(dcmplx((m12-u)/(m12-m22))) - cdlog(dcmplx((m12-t)/(q4*q))) + 0.5d0*(Ln4pi - LEg) ) )
     -		+ (1d0/4d0)*(LEg-Ln4pi)**2 - (LEg-Ln4pi)*( cdlog(dcmplx((m12-u)/(m12-m22))) + cdlog(dcmplx((m12-t)/(q4*q))) )  
     -		+ 2d0*( cspen(dcmplx((t-m22)/(m12-m22))) - cspen(dcmplx((m22-u)/(m12-u))) )  
     -		+ 2d0*cdlog(dcmplx((m12-t)/(q4*q)))*cdlog(dcmplx((m12-u)/(m12-m22))) + (cdlog(dcmplx((m12-t)/(q4*q))))**2 - pi**2/8d0 )

c  	write(*,*)'d04f = ', D04f
     
c d0(m^2,m^2,k1^2,k2^2,s,t;m,0,m,M)
      elseif((k12.ne.0d0).and.(k22.ne.0d0).and.(dabs((k32-q3**2)/k32).lt.1.d-4).and.
     -       (dabs((k42-q1**2)/k42).lt.1.d-4).and.(s  .ne.0d0).and.(t  .ne.0d0).and.
     -       (q1 .ne.0d0).and.(q2 .ne.0d0).and.(q3 .ne.0d0  ).and.
     -       (q4 .eq.0d0))then
*	write(*,*)'C'
	x0 = -(1d0-cdsqrt(dcmplx(1d0-4d0*q1**2/s)))/
     -		  (1d0+cdsqrt(dcmplx(1d0-4d0*q1**2/s)))
	x2 = -(1d0-cdsqrt(dcmplx(1d0-4d0*q1*q2/(k12-(q1-q2)**2) )))/
     -		  (1d0+cdsqrt(dcmplx(1d0-4d0*q1*q2/(k12-(q1-q2)**2) )))
	x3 = -(1d0-cdsqrt(dcmplx(1d0-4d0*q1*q2/(k22-(q1-q2)**2) )))/
     -		  (1d0+cdsqrt(dcmplx(1d0-4d0*q1*q2/(k22-(q1-q2)**2) )))

        D04f	= x0/(q1**2*(t - q2**2)*(1d0 - x0**2))*(
     -    xiIR*cdlog(dcmplx(x0))*(IRdiv + LEg - Ln4pi - dlog(muSc**2))   
     -          - 2d0*cdlog(dcmplx(x0))*cdlog(dcmplx(q2/(q2**2-t) ))
     -          - cspen(dcmplx(1d0-x0**2)) + cdlog(dcmplx(x2))**2 + cdlog(dcmplx(x3))**2
     -   + cspen(dcmplx(1d0-x0*x2*x3)) + cspen(dcmplx(1d0-x0/(x2*x3))) + cspen(dcmplx(1d0-x0*x3/x2)) + cspen(dcmplx(1d0-x0*x2/x3)) )

c d0(0,0,k1^2,k2^2,s,t;q1,q3,q4,0)
      elseif((k12.ne.0d0).and.(k22.ne.0d0).and.(k32.eq.0d0).and.
     -       (k42.eq.0d0).and.(s  .ne.0d0).and.(t  .ne.0d0).and.
     -       (q1 .ne.0d0).and.(q2 .eq.0d0).and.(q3 .ne.0d0  ).and.
     -       (q4 .ne.0d0))then
     	
        D04f	= D04(k12,k22,0d0,0d0,s,t,q1,0d0,q3,q4)
*	write(*,*)'D'
c d0(p1,p21,p32,p3,p2,p13,q1,q2,q3,q4)
      elseif((k12.ne.0d0).and.(k22.ne.0d0).and.(k32.ne.0d0).and.
     -       (k42.ne.0d0).and.(s  .ne.0d0).and.(t  .ne.0d0).and.
     -       (q1 .ne.0d0).and.(q2 .ne.0d0).and.(q3 .ne.0d0).and.
     -       (q4 .ne.0d0))then
        D04f	= D04(k12,k22,k32,k42,s,t,q1,q2,q3,q4)

	endif


	return
	end

************************************************************************
        FUNCTION C03(P1,P2,P3,M1,M2,M3)
************************************************************************
*  SCALAR 3-POINT FUNCTION                                             *
*  P1,P2,P3 = SQUARED EXTERNAL MOMENTA  			       *
*----------------------------------------------------------------------*
*  5.12.96  M. SPIRA    					       *
* Es muss die Dittmaier-Routine D04 hinzugelinkt werden fuer die eta's *
*                   und die Spence-Funktionen                          *
************************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 M1,M2,M3
      REAL*8 R(0:2)
c ----------------------------------------------
      complex*16 cx,cy
c ----------------------------------------------
      COMPLEX*16 C03,CSPEN,ETAD,IEPS,IM
      COMPLEX*16 ALP(0:2),X(0:2,2),Y0(0:2),Y(0:2,2)
      COMPLEX*16 CDUM
C     REAL*8 KAPPA
      COMPLEX*16 KAPPA
C     KAPPA(A,B,C) = DSQRT(A**2+B**2+C**2-2*(A*B+A*C+B*C))
C     KAPPA(A,B,C) = DSQRT(DABS(A**2+B**2+C**2-2*(A*B+A*C+B*C)))
      KAPPA(A,B,C) = CDSQRT(DCMPLX(A**2+B**2+C**2-2*(A*B+A*C+B*C)))
      EPS = 1.D-8
      IM = DCMPLX(0.D0,1.D0)
      IEPS = DCMPLX(0.D0,1.D-17)
      PI = 4*DATAN(1.D0)
      XX = 0.D0
C     IF(P1.LT.0.D0.OR.P2.LT.0.D0.OR.P3.LT.0.D0) XX=1.D0
      IF(P1.NE.0.D0.OR.XX.NE.0.D0)THEN
       Q10 = P1
      ELSE
       Q10 = EPS
      ENDIF
      IF(P3.NE.0.D0.OR.XX.NE.0.D0)THEN
       Q20 = P3
      ELSE
       Q20 = EPS
      ENDIF
      IF(P2.NE.0.D0.OR.XX.NE.0.D0)THEN
       Q21 = P2
      ELSE
       Q21 = EPS
      ENDIF
      R(0) = P2
      R(1) = P3
      R(2) = P1
      SM0 = M1**2
      SM1 = M2**2
      SM2 = M3**2
c ----------------------------------------------
      alpha = cdabs( kappa(q10,q21,q20) )
c      ALPHA = KAPPA(Q10,Q21,Q20)
c ----------------------------------------------
      ALP(0) = KAPPA(Q21,SM1,SM2)*(1+IEPS*Q21)
      ALP(1) = KAPPA(Q20,SM2,SM0)*(1+IEPS*Q20)
      ALP(2) = KAPPA(Q10,SM0,SM1)*(1+IEPS*Q10)
      X(0,1) = (Q21 - SM1 + SM2 + ALP(0))/2/Q21
      X(0,2) = (Q21 - SM1 + SM2 - ALP(0))/2/Q21
      X(1,1) = (Q20 - SM2 + SM0 + ALP(1))/2/Q20
      X(1,2) = (Q20 - SM2 + SM0 - ALP(1))/2/Q20
      X(2,1) = (Q10 - SM0 + SM1 + ALP(2))/2/Q10
      X(2,2) = (Q10 - SM0 + SM1 - ALP(2))/2/Q10
      Y0(0) = (Q21*(Q21-Q20-Q10+2*SM0-SM1-SM2) - (Q20-Q10)*(SM1-SM2)
     .      + ALPHA*(Q21-SM1+SM2))/2/ALPHA/Q21
      Y0(1) = (Q20*(Q20-Q10-Q21+2*SM1-SM2-SM0) - (Q10-Q21)*(SM2-SM0)
     .      + ALPHA*(Q20-SM2+SM0))/2/ALPHA/Q20
      Y0(2) = (Q10*(Q10-Q21-Q20+2*SM2-SM0-SM1) - (Q21-Q20)*(SM0-SM1)
     .      + ALPHA*(Q10-SM0+SM1))/2/ALPHA/Q10
      Y(0,1) = Y0(0) - X(0,1)
      Y(0,2) = Y0(0) - X(0,2)
      Y(1,1) = Y0(1) - X(1,1)
      Y(1,2) = Y0(1) - X(1,2)
      Y(2,1) = Y0(2) - X(2,1)
      Y(2,2) = Y0(2) - X(2,2)
c ----------------------------------------------
      cdum = dcmplx(0.D0)
c      CDUM=0.D0
c ----------------------------------------------
      DO I=0,2
       DO J=1,2
        CDUM = CDUM + CSPEN((Y0(I)-1)/Y(I,J)) - CSPEN(Y0(I)/Y(I,J))
        CX = ETAD(1-X(I,J),1/Y(I,J))
c ----------------------------------------------
        if (cdabs(cx).ne.0.D0) then 
c        IF(CX.NE.0.D0)THEN
c ----------------------------------------------
         CDUM = CDUM + CX*CDLOG((Y0(I)-1)/Y(I,J))
        ENDIF
        CY = ETAD(-X(I,J),1/Y(I,J))
c ----------------------------------------------
        if (cdabs(cy).ne.0.D0) then 
c        IF(CY.NE.0.D0)THEN 
c ----------------------------------------------
         CDUM = CDUM - CY*CDLOG(Y0(I)/Y(I,J))
        ENDIF
       ENDDO
       CX = ETAD(-X(I,1),-X(I,2))
c ----------------------------------------------
       if (cdabs(cx).ne.0.D0) then 
c       IF(CX.NE.0.D0)THEN
c ----------------------------------------------
        CDUM = CDUM - CX*CDLOG((1-Y0(I))/(-Y0(I)))
       ENDIF
       CY = ETAD(Y(I,1),Y(I,2))
c ----------------------------------------------
       if (cdabs(cy).ne.0.D0) then 
c       IF(CY.NE.0.D0)THEN
c ----------------------------------------------
        CDUM = CDUM + CY*CDLOG((1-Y0(I))/(-Y0(I)))
       ENDIF
       A = -R(I)
       B = -DIMAG(Y(I,1)*Y(I,2))
       IF(A.GT.0.D0.AND.B.GT.0.D0) THEN
        CDUM = CDUM + 2*PI*IM*CDLOG((1-Y0(I))/(-Y0(I)))
       ENDIF
      ENDDO
      C03 = CDUM/ALPHA
      RETURN
      END
c ======================================================================

************************************************************************
        FUNCTION D04(P1,P2,P3,P4,P12,P23,M1,M2,M3,M4)
************************************************************************
*  SCALAR 4-POINT FUNCTION WITH AT LEAST ONE MASS ZERO                 *
*  P1,P2,P3,P4 = SQUARED EXTERNAL MOMENTA			       *
*  P12 = (p1+p2)**2,  P23 = (p2+p3)**2				       *
*----------------------------------------------------------------------*
*  2.1.92  SD	         					       *
************************************************************************
        IMPLICIT REAL*8 (A-Z)
	REAL*8 M(4),P(4,4),K(4,4)
        real*8 pi,eps
        real*8 im1,im2
        real*8 m1,m2,m3,m4
        real*8 m02,m12,m22,m32,m42
        real*8 mm0,mm1,mm2,mm3,mm4 
        real*8 p1,p2,p3,p4,p12,p23
        real*8 q0,q1,q2,q3,q4,q00,q12,q23
	COMPLEX*16 A1,A2,A3,A4,SWAP
	COMPLEX*16 SS(4), XX(2), X(2,4),RS(4,4)
	COMPLEX*16 S0(4),XX0(2),X0(2,4), R(4,4),G(2)
        COMPLEX*16 D04,CSPEN,ETAD,SQE,ETAS
	COMPLEX*16 AA,BB,CC,DD,IEPS,H,HH,L1,L2,L3,L4
	INTEGER I,J

        MM1=M1
        MM2=M2
        MM3=M3
        MM4=M4
        M12=M1*M1
        M22=M2*M2
        M32=M3*M3
        M42=M4*M4
        Q1=P1
        Q2=P2
        Q3=P3
	Q4=P4
        Q12=P12
        Q23=P23

C	IS AT LEAST ONE MASS ZERO ???
	IF (MM1*MM2*MM3*MM4.NE.0D0) GOTO 130

C	PERMUTATE UNTIL MM3=0D0
	GOTO 20
10	CONTINUE
	MM0=MM1
	MM1=MM2
	MM2=MM3
	MM3=MM4
	MM4=MM0
	M02=M12
	M12=M22
	M22=M32
	M32=M42
	M42=M02
	Q00=Q12
	Q12=Q23
	Q23=Q00
	Q0=Q1
	Q1=Q2
	Q2=Q3
	Q3=Q4
	Q4=Q0
20	IF (MM3.NE.0D0) GOTO 10
C	ONLY MM3 IS ZERO
	IF (MM1*MM2*MM4.NE.0D0) GOTO 30
C	ONLY MM3 AND MM4 ARE ZERO ==> 3->2, 4->3...
	IF ((MM1*MM2.NE.0D0).AND.(MM4.EQ.0D0)) GOTO 10
C	ONLY MM2 AND MM3 ARE ZERO
	IF ((MM1*MM4.NE.0D0).AND.(MM2.EQ.0D0)) GOTO 40
	WRITE(*,*)'CASE OF THIS SPECIAL D0-FUNCTION NOT IMPLEMENTED!'
	STOP

C	****** NO MASS EQUAL TO ZERO ******
130	CONTINUE
	EPS=1D-18
	IEPS=DCMPLX(0D0,EPS)

	IF( ABS((MM1**2+MM3**2-Q12)/MM1/MM3).LT.2D0 ) THEN
C	R13 WOULD BE NOT REAL. -> PERMUTATION! -> R(2,4) IS NOT REAL.
	   M(1)=MM2
	   M(2)=MM3
	   M(3)=MM4
	   M(4)=MM1
	   P(1,2)=Q2
	   P(1,3)=Q23
	   P(1,4)=Q1
	   P(2,3)=Q3
	   P(2,4)=Q12
	   P(3,4)=Q4
	ELSE
C	R(1,3) IS REAL.
	   M(1)=MM1
	   M(2)=MM2
	   M(3)=MM3
	   M(4)=MM4
	   P(1,2)=Q1
	   P(1,3)=Q12
	   P(1,4)=Q4
	   P(2,3)=Q2
	   P(2,4)=Q23
	   P(3,4)=Q3
	ENDIF

	DO 11 J=2,4
	DO 11 I=1,J-1
	K(I,J)=(M(I)**2+M(J)**2-P(I,J))/M(I)/M(J)
	R(I,J) =SQE(DCMPLX(1D0,0D0),DCMPLX(-K(I,J),0D0),
     *	            DCMPLX(1D0,0D0))
	IF( DIMAG(R(I,J)).EQ.0D0 ) THEN
	   RS(I,J)=SQE(DCMPLX(1D0,0D0),DCMPLX(-K(I,J),EPS),
     *	               DCMPLX(1D0,0D0))
	ELSE
	   RS(I,J)=R(I,J)
	ENDIF
11	CONTINUE

	SS(1)=RS(1,2)
	SS(2)=RS(2,3)
	SS(3)=RS(3,4)
	SS(4)=RS(1,4)
	S0(1)=R(1,2)
	S0(2)=R(2,3)
	S0(3)=R(3,4)
	S0(4)=R(1,4)
	AA=K(3,4)/R(2,4)+R(1,3)*K(1,2)-K(1,4)*R(1,3)/R(2,4)-K(2,3)
	BB=(R(2,4)-1D0/R(2,4))*(R(1,3)-1D0/R(1,3))
     *		+K(1,2)*K(3,4)-K(1,4)*K(2,3)
	CC=K(1,2)/R(1,3)+R(2,4)*K(3,4)-K(1,4)*R(2,4)/R(1,3)-K(2,3)
	DD=K(2,3)-R(1,3)*K(1,2)-R(2,4)*K(3,4)+R(1,3)*R(2,4)*K(1,4)
	XX(1)=SQE(AA,BB,CC+IEPS*DD)
	XX(2)=(CC+IEPS*DD)/AA/XX(1)
	XX0(1)=SQE(AA,BB,CC)
	XX0(2)=CC/AA/XX0(1)
c	IF (ABS(DREAL(XX0(1)-XX(2))).LT.ABS(DREAL(XX0(1)-XX(1)))) THEN
	IF (ABS(XX0(1)-XX(2)).LT.ABS(XX0(1)-XX(1))) THEN
	  SWAP  =XX0(1)
	  XX0(1)=XX0(2)
	  XX0(2)=SWAP
	ENDIF

	DO 12 I=1,2
	G(I)  =SIGN( 1D0,DREAL(AA*(XX(I)-XX(3-I))) )
	 X(I,1)= XX(I)/R(2,4)
	X0(I,1)=XX0(I)/R(2,4)
	 X(I,2)= XX(I)/R(2,4)*R(1,3)
	X0(I,2)=XX0(I)/R(2,4)*R(1,3)
	 X(I,3)= XX(I)*R(1,3)
	X0(I,3)=XX0(I)*R(1,3)
	 X(I,4)= XX(I)
	X0(I,4)=XX0(I)
12	CONTINUE

	D04 = DCMPLX(0D0,0D0)
	DO 13 I=1,2
	DO 13 J=1,4
	A1 = 1D0+X0(I,J)*S0(J) + ABS(1D0+X0(I,J)*S0(J))*IEPS*
     *				  SIGN(1D0,DIMAG(X(I,J)*SS(J)))
	A2 = 1D0+X0(I,J)/S0(J) + ABS(1D0+X0(I,J)/S0(J))*IEPS*
     *				  SIGN(1D0,DIMAG(X(I,J)/SS(J)))
	D04 = D04 + (-1D0)**(I+J)*(
     *		CSPEN(A1)+ETAD(-X(I,J),SS(J))*LOG(A1)
     *	       +CSPEN(A2)+ETAD(-X(I,J),1D0/SS(J))*LOG(A2)     )
13	CONTINUE

	IF( DIMAG(R(1,3)).EQ.0D0 ) THEN
	DO 14 I=1,2
	   A1 = (K(1,3)-2D0*R(1,3))/XX0(I)
     *		      -R(1,3)*K(1,4)+K(3,4)
     	   A2 = ((K(2,4)-2D0*R(2,4))*R(1,3)*XX0(I)
     *		      -R(2,4)*K(3,4)+K(2,3))/DD
	   A3 = (K(1,3)-2D0*R(1,3))*R(2,4)/XX0(I)
     *		      -R(1,3)*K(1,2)+K(2,3)
	   A4 = ((K(2,4)-2D0*R(2,4))*XX0(I)
     *		      -R(2,4)*K(1,4)+K(1,2))/DD
	   L1 = LOG( A1-ABS(A1)*IEPS )
     	   L2 = LOG( A2+ABS(A2)*IEPS*G(I)*SIGN(1D0,DREAL(R(1,3))
     *				        	  *DIMAG(RS(2,4))) )
	   L3 = LOG( A3-ABS(A3)*IEPS )
	   L4 = LOG( A4+ABS(A4)*IEPS*G(I)*SIGN(1D0,DIMAG(RS(2,4))) )

	   D04 = D04 
     &         + (3D0-2D0*I)*(
     *		       ETAS( -XX(I),R(1,3),RS(1,3) )
     *		          *( LOG(R(1,3)*XX(I)) + L1 + L2 )
     *		     + ETAS( -XX(I),1D0/R(2,4),1D0/RS(2,4) )
     *		          *( LOG(XX(I)/R(2,4)) + L3 + L4 )
     *		     - ( ETAS( -XX(I),R(1,3)/R(2,4),RS(1,3)/RS(2,4) )
     *		       + ETAD( RS(1,3),1D0/RS(2,4) )                  )
     *		        *( LOG(XX(I)*R(1,3)/R(2,4)) + L3 + L2 )
     *	  	     + ETAD( RS(1,3),1D0/RS(2,4) )
     *		       *ETAS(-XX(I),-R(1,3)/R(2,4),-RS(1,3)/RS(2,4))   )
14	CONTINUE
	ELSE
	DO 15 I=1,2
	   L1 = LOG( R(2,4)/XX0(I)+XX0(I)/R(2,4)+K(1,2)
     *		     -XX0(I)/R(2,4)*EPS*BB*G(I) )
	   L2 = LOG( R(1,3)*XX0(I)+1D0/XX0(I)/R(1,3)+K(3,4)
     *		     -XX0(I)*R(1,3)*EPS*BB*G(I) )
	   L3 = LOG( R(1,3)/R(2,4)*XX0(I)+R(2,4)/XX0(I)/R(1,3)+K(2,3)
     *		     -XX0(I)*R(1,3)/R(2,4)*EPS*BB*G(I) )

	   D04 = D04 
     &          + (3D0-2D0*I)*(
     *		     ETAD(-XX(I),1D0/R(2,4))
     *		      *( LOG(XX(I)/R(2,4)) + L1 )
     *		    +ETAD(-XX(I),R(1,3))
     *		      *( LOG(R(1,3)*XX(I)) + L2 )
     *		    -( ETAD(-XX(I),R(1,3)/R(2,4))
     *		      +ETAD(R(1,3),1D0/R(2,4)) )
     *		      *( LOG(XX(I)*R(1,3)/R(2,4)) + L3 )
     *	  	    +ETAD(R(1,3),1D0/R(2,4))
     *		      *ETAD(-XX(I),-R(1,3)/R(2,4))
     *		      *(1D0-G(I)*SIGN(1D0,DREAL(BB)))	    )
15	CONTINUE
	ENDIF

	D04 = D04/M(1)/M(2)/M(3)/M(4)/AA/(XX(1)-XX(2))
	RETURN


C--->	***************** SPEZIELL ( --> T.SACK-PROMOTION )
C	D1=Q12-M12
C	D2=Q2 -M22
C	D3=Q3 -M42
C	IF ((D1*D2.LE.0D0).OR.(D2*D3.LE.0D0)) THEN
C	   WRITE(*,*) 'THE CASE OF DIFFERENT SIGNS OF THE D1,D2,D3'
C	   WRITE(*,*) 'IN D04(...) IS NOT IMPLEMENTED !!!'
C	   STOP
C	ENDIF
C	NM1=ABS(MM1/D1)
C	NM2=ABS(MM2/D2)
C	NM3=ABS(MM4/D3)
C	NP1=Q2/D2**2+Q12/D1**2+(Q1-Q2-Q12)/D1/D2
C	NP2=Q2/D2**2+ Q3/D3**2+(Q23-Q2-Q3)/D2/D3
C	NP3=Q3/D3**2+Q12/D1**2+(Q4-Q3-Q12)/D1/D3
C	D04=C04(NP1,NP2,NP3,NM1,NM2,NM3)/D1/D2/D3

C	*************** ALLGEMEIN


C	****** ONLY MM3 IS ZERO ******
30	CONTINUE
	EPS=1D-17
	IEPS=DCMPLX(0D0,EPS)
	M(1)=MM1
	M(2)=MM2
	M(3)=10D0
	M(4)=MM4
	P(1,2)=Q1
	P(1,3)=Q12
	P(1,4)=Q4
	P(2,3)=Q2
	P(2,4)=Q23
	P(3,4)=Q3
	DO 1 J=2,4
	DO 1 I=1,J-1
	K(I,J)=(M(I)**2+M(J)**2-P(I,J))/M(I)/M(J)
	IF (I.EQ.3) K(I,J)=K(I,J)-M(I)/M(J)
	IF (J.EQ.3) K(I,J)=K(I,J)-M(J)/M(I)
	R(I,J) =SQE(DCMPLX(1D0,0D0),DCMPLX(-K(I,J),0D0),
     *	            DCMPLX(1D0,0D0))
	IF( DIMAG(R(I,J)).EQ.0D0 ) THEN
	   RS(I,J)=SQE(DCMPLX(1D0,0D0),DCMPLX(-K(I,J),EPS),
     *	               DCMPLX(1D0,0D0))
	ELSE
	   RS(I,J)=R(I,J)
	ENDIF
1	CONTINUE
	SS(1)=RS(1,2)
	SS(2)=RS(2,3)
	SS(3)=RS(3,4)
	SS(4)=RS(1,4)
	AA=K(3,4)/R(2,4)-K(2,3)
	BB=K(1,3)*(1D0/R(2,4)-R(2,4))+K(1,2)*K(3,4)-K(1,4)*K(2,3)
	CC=K(1,2)*K(1,3)-K(1,3)*K(1,4)*R(2,4)+R(2,4)*K(3,4)-K(2,3)
	DD=K(2,3)-R(2,4)*K(3,4)
	XX(1)=SQE(AA,BB,CC+IEPS*DD)
	XX(2)=(CC+IEPS*DD)/AA/XX(1)
	DO 2 I=1,2
	X(I,1)=XX(I)/R(2,4)
	X(I,2)=XX(I)/R(2,4)*R(1,3)
	X(I,3)=XX(I)*R(1,3)
	X(I,4)=XX(I)
2	CONTINUE
	D04 = DCMPLX(0D0,0D0)
	DO 3 I=1,2
	D04 = D04 + (2D0*I-3D0)*(
     *		CSPEN(1D0+SS(4)*X(I,4))
     *	       -CSPEN(1D0+SS(1)*X(I,1))
     *	       +CSPEN(1D0+X(I,4)/SS(4))
     *	       -CSPEN(1D0+X(I,1)/SS(1))
     *	       +ETAD(-X(I,4),SS(4))*LOG(1D0+SS(4)*X(I,4))
     *	       -ETAD(-X(I,1),SS(1))*LOG(1D0+SS(1)*X(I,1))
     *	       +ETAD(-X(I,4),1D0/SS(4))*LOG(1D0+X(I,4)/SS(4))
     *	       -ETAD(-X(I,1),1D0/SS(1))*LOG(1D0+X(I,1)/SS(1))
     *	       -CSPEN(1D0+X(I,4)*(K(3,4)-IEPS)/(K(1,3)-IEPS))
     *	       +CSPEN(1D0+X(I,1)*(K(2,3)-IEPS)/(K(1,3)-IEPS))
     *	       -ETAD(-X(I,4),(K(3,4)-IEPS)/(K(1,3)-IEPS))
     *	           *LOG(1D0+X(I,4)*(K(3,4)-IEPS)/(K(1,3)-IEPS))
     *	       +ETAD(-X(I,1),(K(2,3)-IEPS)/(K(1,3)-IEPS))
     *	           *LOG(1D0+X(I,1)*(K(2,3)-IEPS)/(K(1,3)-IEPS))   )
	IF (DIMAG(R(2,4)).NE.0D0) THEN
	   H=ETAD(-1D0/XX(I),R(2,4))
	ELSE
	   H=DCMPLX(0D0,0D0)
	   IF (DREAL(R(2,4)).LT.0D0) THEN
	      HH=-1D0/XX(I)
	      IM1=DIMAG(HH)
	      IM2=DIMAG(RS(2,4))
              pi = 4.D0*datan(1.D0)
	      IF ((IM1.GT.0D0).AND.(IM2.GT.0D0)) THEN
	         H=-DCMPLX(0D0,2D0*PI)
	      ENDIF
	      IF ((IM1.LT.0D0).AND.(IM2.LT.0D0)) THEN
	         H=+DCMPLX(0D0,2D0*PI)
	      ENDIF
	   ENDIF
	ENDIF
	D04 = D04 + (2D0*I-3D0)*
     *	          H*( LOG( (K(1,2)-R(2,4)*K(1,4)
     *			  +XX(I)*(1D0/R(2,4)-R(2,4)))/DD )
     *		     +LOG(K(1,3)-IEPS) )
3	CONTINUE
	D04 = D04/M(1)/M(2)/M(3)/M(4)/AA/(XX(1)-XX(2))
	RETURN

C	****** ONLY MM2 AND MM3 ARE ZERO ******
40	CONTINUE
	EPS=1D-17
	IEPS=DCMPLX(0D0,EPS)

	M(1)=MM1
	M(2)=10D0
	M(3)=10D0
	M(4)=MM4
	P(1,2)=Q1
	P(1,3)=Q12
	P(1,4)=Q4
	P(2,3)=Q2
	P(2,4)=Q23
	P(3,4)=Q3
	DO 4 J=2,4
	DO 4 I=1,J-1
	K(I,J)=(M(I)**2+M(J)**2-P(I,J))/M(I)/M(J)
	IF (I.EQ.2) K(I,J)=K(I,J)-M(I)/M(J)
	IF (J.EQ.2) K(I,J)=K(I,J)-M(J)/M(I)
	IF (I.EQ.3) K(I,J)=K(I,J)-M(I)/M(J)
	IF (J.EQ.3) K(I,J)=K(I,J)-M(J)/M(I)
	R(I,J) =SQE(DCMPLX(1D0,0D0),DCMPLX(-K(I,J),0D0),
     *	            DCMPLX(1D0,0D0))
	IF( DIMAG(R(I,J)).EQ.0D0 ) THEN
	   RS(I,J)=SQE(DCMPLX(1D0,0D0),DCMPLX(-K(I,J),EPS),
     *	               DCMPLX(1D0,0D0))
	ELSE
	   RS(I,J)=R(I,J)
	ENDIF
4	CONTINUE
	SS(1)=RS(1,2)
	SS(2)=RS(2,3)
	SS(3)=RS(3,4)
	SS(4)=RS(1,4)
	AA=K(2,4)*K(3,4)-K(2,3)
	BB=K(1,3)*K(2,4)+K(1,2)*K(3,4)-K(1,4)*K(2,3)
	CC=K(1,2)*K(1,3)-K(2,3)
	DD=K(2,3)
	XX(1)=SQE(AA,BB,CC+IEPS*DD)
	XX(2)=(CC+IEPS*DD)/AA/XX(1)
	DO 5 I=1,2
	X(I,1)=XX(I)/R(2,4)
	X(I,2)=XX(I)/R(2,4)*R(1,3)
	X(I,3)=XX(I)*R(1,3)
	X(I,4)=XX(I)
5	CONTINUE
	D04 = DCMPLX(0D0,0D0)
	DO 6 I=1,2
	D04 = D04 + (2D0*I-3D0)*(
     *		CSPEN(1D0+SS(4)*X(I,4))
     *	       +CSPEN(1D0+X(I,4)/SS(4))
     *	       +ETAD(-X(I,4),SS(4))*LOG(1D0+SS(4)*X(I,4))
     *	       +ETAD(-X(I,4),1D0/SS(4))*LOG(1D0+X(I,4)/SS(4))
     *	       -CSPEN(1D0+XX(I)*(K(3,4)-IEPS)/(K(1,3)-IEPS))
     *	       -CSPEN(1D0+XX(I)*(K(2,4)-IEPS)/(K(1,2)-IEPS))
     *	       -ETAD(-XX(I),(K(3,4)-IEPS)/(K(1,3)-IEPS))
     *	           *LOG(1D0+XX(I)*(K(3,4)-IEPS)/(K(1,3)-IEPS))
     *	       -ETAD(-XX(I),(K(2,4)-IEPS)/(K(1,2)-IEPS))
     *	           *LOG(1D0+XX(I)*(K(2,4)-IEPS)/(K(1,2)-IEPS))
     *	       +LOG(-XX(I))*( LOG(K(1,2)-IEPS)
     *			     +LOG(K(1,3)-IEPS)-LOG(K(2,3)-IEPS) ) )
6	CONTINUE
	D04 = D04/M(1)/M(2)/M(3)/M(4)/AA/(XX(1)-XX(2))

	RETURN

	END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        FUNCTION CSPEN(Z)                                              
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       SPENCE-FUNKTION KOMPLEX, FREI NACH HOLLIK                     C
C---------------------------------------------------------------------C
C       20.07.83    LAST CHANGED 10.05.89        ANSGAR DENNER        C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        integer k
        COMPLEX*16 CSPEN,W,SUM,Z,U                                     
        REAL*8 RZ,AZ,A1                                                
        REAL*8 B(9)
        data B/
     1   0.1666666666666666666666666667D0,                             
     2  -0.0333333333333333333333333333D0,                             
     3   0.0238095238095238095238095238D0,                             
     4  -0.0333333333333333333333333333D0,                             
     5   0.0757575757575757575757575758D0,                             
     6  -0.2531135531135531135531135531D0,                             
     7   1.1666666666666666666666666667D0,                             
     8  -7.09215686274509804D0         ,                               
     9  54.97117794486215539D0         /                               
C     BEACHTE:                 B(N)=B2N                                
C     B(1)=1./6.                                                       
C     B(2)=-1./30.                                                     
C     B(3)=1./42.                                                      
C     B(4)=-1./30.                                                     
C     B(5)=5./66.                                                      
C     B(6)=-691./2730.                                                 
C     B(7)=7./6.                                                       
C     B(8)=-3617./510.                                                 
C     B(9)=43867./798.                                                 
C     B(10)=-174611./330.                                              
C     B(11)=54513./138.                                               
C     PI=3.1415926535897932384                                         
C     PI*PI/6.=1.6449..., PI*PI/3=3.28986...                           
C                                                                      
c      Z =Z*DCMPLX(1D0)                                                 
      RZ=DREAL(Z)                                                      
      AZ=CDABS(Z)                                                      
      A1=CDABS(1D0-Z)                                                  
C     IF((SNGL(RZ) .EQ. 0.0) .AND. (SNGL(DIMAG(Z)) .EQ. 0.0)) THEN     
C ---> CHANGED  10.5.89                                                
      IF(AZ .LT. 1D-20) THEN                                           
        CSPEN=-CDLOG(1D0-Z)                                            
        RETURN                                                         
      END IF                                                           
c      IF((SNGL(RZ) .EQ. 1.0) .AND. (SNGL(DIMAG(Z)) .EQ. 0.0)) THEN     
c ---> changed 5.7.94
       IF( (ABS(RZ-1D0).LT.1D-18) .AND. (ABS(DIMAG(Z)).LT.1D-18) ) THEN     
        CSPEN=1.64493406684822643D0                                    
        RETURN                                                         
      END IF                                                           
      IF(RZ.GT.5D-1) GOTO 20                                           
      IF(AZ.GT.1D0) GOTO 10                                            
      W=-CDLOG(1D0-Z)                                                  
      SUM=W-0.25D0*W*W                                                 
      U=W                                                              
      IF(CDABS(U).LT.1D-10) GOTO 2                                     
      DO 1 K=1,9                                                       
      U=U*W*W/dcmplx(2*K*(2*K+1))                                      
      IF(CDABS(U*B(K)/SUM).LT.1D-20) GOTO 2                            
      SUM=SUM+U*B(K)                                                   
 1    CONTINUE                                                         
 2    CSPEN=SUM                                                        
      RETURN                                                           
10    W=-CDLOG(1D0-1D0/Z)                                              
      SUM=W-0.25D0*W*W                                                 
      U=W                                                              
      IF(CDABS(U).LT.1D-10) GOTO 12                                    
                                                                       
      DO 11 K=1,9                                                      
      U=U*W*W/dcmplx(2*K*(2*K+1))                                      
      IF(CDABS(B(K)*U/SUM).LT.1D-20) GOTO 12                           
      SUM=SUM+U*B(K)                                                   
11    CONTINUE                                                         
12    CSPEN=-SUM-1.64493406684822643D0-.5D0*CDLOG(-Z)**2               
      RETURN                                                           
20    IF(A1.GT.1D0) GOTO 30                                            
      W=-CDLOG(Z)                                                      
      SUM=W-0.25D0*W*W                                                 
      U=W                                                              
      IF(CDABS(U).LT.1D-10) GOTO 22                                    
      DO 21 K=1,9                                                      
      U=U*W*W/dcmplx(2*K*(2*K+1))                                      
      IF(CDABS(U*B(K)/SUM).LT.1D-20) GOTO 22                           
      SUM=SUM+U*B(K)                                                   
21    CONTINUE                                                         
22    CSPEN=-SUM+1.64493406684822643D0-CDLOG(Z)*CDLOG(1D0-Z)           
      RETURN                                                           
30    W=CDLOG(1D0-1D0/Z)                                               
      SUM=W-0.25D0*W*W                                                 
      U=W                                                              
      IF(CDABS(U).LT.1D-10) GOTO 32                                    
      DO 31 K=1,9                                                      
      U=U*W*W/dcmplx(2*K*(2*K+1))                                      
      IF(CDABS(U*B(K)/SUM).LT.1D-20) GOTO 32                           
      SUM=SUM+U*B(K)                                                   
31    CONTINUE                                                         
32    CSPEN=SUM+3.28986813369645287D0                                  
     *               +.5D0*CDLOG(Z-1D0)**2-CDLOG(Z)*CDLOG(1D0-Z)       
50    CONTINUE                                                         
        END                                                            

***********************************************************************
        FUNCTION ETAD(C1,C2)                                            
***********************************************************************
*       COMPLEX ETAD-FUNKTION                                           
*---------------------------------------------------------------------*
*       8.06.90    ANSGAR DENNER                                       
***********************************************************************
        IMPLICIT   LOGICAL(A-Z)                                        
        COMPLEX*16 ETAD,C1,C2                                           
        REAL*8     PI,IM1,IM2,IM12                                     
                                                                       
        PI     = 4D0*DATAN(1D0)                                        
        IM1    = DIMAG(C1)                                             
        IM2    = DIMAG(C2)                                             
        IM12   = DIMAG(C1*C2)                                          
                                                                       
        IF(IM1.LT.0D0.AND.IM2.LT.0D0.AND.IM12.GT.0D0) THEN             
            ETAD = DCMPLX(0D0,2D0*PI)                                   
        ELSE IF (IM1.GT.0D0.AND.IM2.GT.0D0.AND.IM12.LT.0D0) THEN       
            ETAD = DCMPLX(0D0,-2D0*PI)                                  
        ELSE                                                           
            ETAD = DCMPLX(0D0)                                          
        END IF                                                         
        END                                                            

***********************************************************************
        FUNCTION ETAS(Y,R,RS)                                            
***********************************************************************
*       MODIFIED ETAD-FUNKTION                                           
*---------------------------------------------------------------------*
*       18.1.94   SD                                       
***********************************************************************
        IMPLICIT   LOGICAL(A-Z)                                        
        COMPLEX*16 ETAD,ETAS,Y,R,RS
        REAL*8     PI,IMY,IMRS
                                                                       
        PI     = 4D0*DATAN(1D0)                                        

	IF( DIMAG(R).NE.0D0 ) THEN
	    ETAS = ETAD(Y,R)
	ELSE	    
	    IF( DREAL(R).GT.0D0 ) THEN
		ETAS = DCMPLX(0D0,0D0)
	    ELSE
	 	IMY  = DIMAG(Y)
		IMRS = DIMAG(RS)
		ETAS = 2D0*DCMPLX(0D0,PI)*(
     *			(1D0+SIGN(1D0,-IMY))*(1D0+SIGN(1D0,-IMRS))-
     *			(1D0+SIGN(1D0, IMY))*(1D0+SIGN(1D0, IMRS))
     *					  )/4D0
	    ENDIF
	ENDIF
        END                                                            

***********************************************************************
        FUNCTION SQE(A,B,C)                                            
***********************************************************************
*       SOLUTION OF QUADRATIC EQUATION				      *
*---------------------------------------------------------------------*
*       13.1.92  SD						      *
***********************************************************************
        IMPLICIT REAL*8 (A-Z)                                        
        COMPLEX*16 A,B,C,SQE,X1,X2

	X1=(-B+SQRT(B**2-4D0*A*C))/2D0/A
	X2=(-B-SQRT(B**2-4D0*A*C))/2D0/A

	IF (ABS(X1).GT.ABS(X2)) THEN
	   SQE=X1
	ELSE
	   SQE=X2
	ENDIF

        END      
	
***********************************************************************
        double complex function ckappa(Ss,m1,m2)                                            
***********************************************************************

	double precision Ss,m1,m2
 
	ckappa = cdsqrt(dcmplx((Ss-m1-m2)**2-4d0*m1*m2)) 
 
        END      
	
	                                                      
