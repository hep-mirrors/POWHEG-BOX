      subroutine setreal(p,rflav,amp2)
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_flst.h'
      include '../include/pwhg_st.h'
      include '../include/pwhg_math.h'
      real * 8 p(0:3,nlegreal)
      integer rflav(nlegreal),rflavs(nlegreal)
      real * 8 amp2, ampA, ampB
      integer mu
      real * 8 pphy(0:3,nlegreal), sphy(0:3,nlegreal)  
      integer btest, ii(nlegreal),i 


c     assign momenta, switched momenta and access the flavor structure
     
      do i=1,nlegreal
         ii(i)=rflav(i)
         do mu=0,3
            pphy(mu,i)=p(mu,i)
         enddo
      enddo
      
     
      do mu=0,3
         sphy(mu,1)=p(mu,2)
	 sphy(mu,2)=p(mu,1)
	 sphy(mu,3)=p(mu,3)
	 sphy(mu,4)=p(mu,4)
	 sphy(mu,5)=p(mu,5)
      enddo
   
c momenta have to be switched only for gb      
      btest = ii(1)*ii(1)
      amp2 = 0d0
      
c g b  
      if ( (ii(1).eq.5).and.(ii(2).eq.0).and.(ii(5).eq.0) ) then  
      		call bg(pphy,amp2,rflav(3))
c		write(*,*) , 'bg:  ' , ii(1) , ' ' , ii(2) , ' ' , ii(5)   
c b g
      elseif ((ii(1).eq.0).and.(ii(2).eq.5).and.(ii(5).eq.0)) then
      		call bg(sphy,amp2,rflav(3))
c		write(*,*) , 'gb:  ' , ii(1) , ' ' , ii(2) , ' ' , ii(5)   
   

c g g
      elseif ( (ii(1).eq.0).and.(ii(2).eq.0).and.(ii(5).eq.-5) ) then
         call ggproc(pphy,amp2,rflav(3))
      		
c		write(*,*) , 'gg:  ' , ii(1) , ' ' , ii(2) , ' ' , ii(5)   
c b q (q#b,b~)
      elseif ( (ii(1).eq.5).and.(ii(2).eq.ii(5)).and.(abs(ii(5)).ne.5) )
     1        then 
         call bqorqbar(pphy,amp2,rflav(3))
c		write(*,*) , 'bq:  ' , ii(1) , ' ' , ii(2) , ' ' , ii(5)   
c q b (q#b,b~)		
      elseif ( (ii(2).eq.5).and.(ii(1).eq.ii(5)).and.(abs(ii(5)).ne.5) )
     1        then 
         call bqorqbar(sphy,amp2,rflav(3))
c		write(*,*) , 'qb:  ' , ii(1) , ' ' , ii(2) , ' ' , ii(5)   
c b b
      elseif ( (ii(1).eq.5).and.(ii(2).eq.5) )
     1               then
         call bb(sphy,amp2,rflav(3))
cc		call bqorqbar(pphy,ampA)
c		call bqorqbar(sphy,ampB)
c		amp2 = ampA + ampB	
c		write(*,*) , 'bb:  ' , ii(1) , ' ' , ii(2) , ' ' , ii(5)   
		
c q qbar, (q#b,b~), q>0
      elseif ( (btest.ne.25).and.(ii(1).eq.(-ii(2))).and.(ii(5).eq.-5)
     1    .and.(ii(1).gt.0) ) then 
         call qqbar(sphy,amp2,rflav(3))
c		write(*,*) , 'qqbar:  ' , ii(1) , ' ' , ii(2) , ' ' , ii(5)   
c q qbar, (q#b,b~), q<0
      elseif ( (btest.ne.25).and.(ii(1).eq.(-ii(2))).and.(ii(5).eq.-5)
     1   .and.(ii(1).lt.0) ) then 
         call qqbar(pphy,amp2,rflav(3))
c		write(*,*) , 'qbarq:  ' , ii(1) , ' ' , ii(2) , ' ' , ii(5)  
c b b~
      elseif ( (btest.eq.25).and.(ii(2).eq.-5).and.(ii(5).eq.-5) ) then
         call bbbar(pphy,amp2,rflav(3))
c		write(*,*) , 'bbbar:  ' , ii(1) , ' ' , ii(2) , ' ' , ii(5)   
c b~ b
      elseif ( (btest.eq.25).and.(ii(1).eq.-5).and.(ii(5).eq.-5) ) then
         call bbbar(sphy,amp2,rflav(3))
c		write(*,*) , 'bbarb:  ' , ii(1) , ' ' , ii(2) , ' ' , ii(5)    
      else 
      write(*,*)'something is missing'
      	 			
      endif
      
      amp2=amp2*2*pi/st_alpha
      end
      
      
c-------------------------------------------------------------------------------c      
c----------  subroutine for b(p1) g(p2) -> H^{-}(p3) t(p4) g(p5)  --------------c
c-------------------------------------------------------------------------------c       
      
      subroutine bg(pphy,amp2,rflav3)
      implicit none
      real * 8 pphy(0:3,5)          
      real * 8 amp2
      integer rflav3
      real * 8 p1(0:3),p2(0:3),p3(0:3),p4(0:3),p5(0:3)
c      include 'couplings.h'
      real * 8 p12,p13,p14,p15,p23,p24,p25,p34,p35,p45,ss,dentop,dentop2
      real * 8 Mtop, Mch,mt,mh, Mtop2, Mch2, A, B, alphas, GF

      real * 8 dotp
      complex * 16 ccdotp
      integer mu,i
      real * 8 CFamp2, CFNC2amp2
      include 'PhysPars.h'
      include '../include/pwhg_math.h'
      include '../include/pwhg_st.h'      
      include 'coupl.inc'      

      if(rflav3.ne.-37) then
         write(*,*) ' error in bg, rflav3=',rflav3
         call exit(-1)
      endif

      Mch = ph_mCH
      Mtop = ph_mT
      mt=mtop
      mh=mch
      Mch2 = Mch**2
      Mtop2 = Mtop**2
      
      GF=ph_GF
      A=ph_A
      B=ph_B

      ghmq=sqrt(gf*sqrt(2d0)*(a**2+b**2))

      alphas=st_alpha
     
      
      do mu=0,3
         p1(mu) = pphy(mu,1)
         p2(mu) = pphy(mu,2)
         p3(mu) = pphy(mu,3)
         p4(mu) = pphy(mu,4)
         p5(mu) = pphy(mu,5)
      enddo

c    define all the possible scalar products, even if they are not independent      
      p12=dotp(p1,p2)
      p13=dotp(p1,p3)
      p14=dotp(p1,p4)
      p15=dotp(p1,p5)
      p23=dotp(p2,p3)
      p24=dotp(p2,p4)
      p25=dotp(p2,p5)
      p34=dotp(p3,p4)
      p35=dotp(p3,p5)
      p45=dotp(p4,p5)
      
      amp2 =          (GF*((32*alphas**2*(A**2 + B**2)*CF*(Mtop2*(-Mch2 + Mtop2)*p14 + (Mch2 - Mtop2)*p12*(Mtop2 + p24) + 
     -           2*p13*(-(p23*(Mtop2 + p24)) + Mtop2*(2*Mtop2 - 2*p24 + p34)))*Pi**2)/((-Mch2 + Mtop2 + 2*p13)**2*p24**2) - 
     -      (16*alphas**2*(A**2 + B**2)*CF*((Mtop2 + p24)*p25 - Mtop2*p45)*Pi**2)/(p15*p24**2) - 
     -      (32*alphas**2*(A**2 + B**2)*CF*(2*(Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45)*Pi**2)/(p15*(Mch2 + Mtop2 + 2*p34)**2) + 
     -      (32*alphas**2*(A**2 + B**2)*CF*(2*p25*(Mtop2 + p34) + Mtop2*p35 - (Mtop2 + 2*p34)*p45)*Pi**2)/(p15*p24*(Mch2 + Mtop2 + 2*p34)) - 
     -      (16*alphas**2*(A**2 + B**2)*CF*(2*p15*((Mtop2 + p34)*(Mch2 + Mtop2 + 2*p34) + 2*(Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45) + 
     -           (Mch2 + Mtop2 + 2*p34)*(p14*(Mch2 + Mtop2 + 2*p34 + 2*p35) - 2*p13*p45))*Pi**2)/(p12**2*(Mch2 + Mtop2 + 2*p34)**2) - 
     -      (16*alphas**2*(A**2 + B**2)*CF*(2*p15*(p34*(Mch2 + Mtop2 - 2*p45) + Mtop2*(2*Mch2 + p35 - p45)) + 
     -           p13*(-3*mt**4 + Mch2*Mtop2 - 6*Mtop2*p34 - 4*p34**2 - 4*(Mtop2 + p34)*p35 - 8*(Mtop2 + p34)*p45) + 
     -           p14*(-mt**4 + 3*Mch2*Mtop2 + 2*Mch2*p34 - 4*(Mtop2 + p34)*p45))*Pi**2)/(p12**2*(Mch2 + Mtop2 + 2*p34)*p45) - 
     -      (16*alphas**2*(A**2 + B**2)*CF*(Mtop2*p13*(Mtop2 + p45) - p14*(mt**4 + 2*Mtop2*p34 + 2*p35*(Mtop2 - p45) + 3*Mtop2*p45 - 2*p45**2) - 
     -           p15*(mt**4 + 2*Mtop2*p34 + 2*p35*(Mtop2 - p45) + 3*Mtop2*p45 - 2*p45**2) + 2*p12*(mt**4 + Mtop2*p34 + p35*(Mtop2 - p45) + 2*Mtop2*p45 - p45**2))*
     -         Pi**2)/(p12*(p24 + p25 - p45)*p45**2) + (32*alphas**2*(A**2 + B**2)*CF*
     -         (Mtop2*p15*(Mtop2 - p24) + 2*p13*(p12*(Mtop2 + p24) - (Mtop2 + p24)*p25 + Mtop2*(-p14 + p45)))*Pi**2)/((Mch2 - Mtop2 - 2*p13)*p15*p24**2) - 
     -      (16*alphas**2*(A**2 + B**2)*CF*(p15*(mt**4 + (Mtop2 + p24)*p45) - p12*(Mtop2*(Mtop2 + p24 + p25) - p25*p45 + p45**2) + 
     -           p14*(mt**4 + p45*(Mtop2 - p25 + p45)))*Pi**2)/((p24 + p25 - p45)**2*p45**2) - 
     -      (8*alphas**2*(A**2 + B**2)*CF*(p15*((mh - mt)*(mh + mt)*Mtop2 - (Mch2 + 3*Mtop2 + 2*p34)*p45) - 
     -           2*p13*(mt**4 + Mtop2*p34 + p35*(Mtop2 - p45) + 2*Mtop2*p45 - p45**2) + p14*((mh - mt)*(mh + mt)*Mtop2 + 2*p45*(-Mtop2 + p35 + p45)))*Pi**2)/
     -       (p12**2*p45**2) - (32*alphas**2*(A**2 + B**2)*CF*(Mtop2*p15*(Mtop2 + p23 + p24 + p34) - p25*(Mtop2*p14 + p13*(5*Mtop2 + 4*p34)) - 
     -           Mtop2*(2*p13 + p14)*p35 + p13*(2*Mtop2*p13 - 2*p14*(Mtop2 + 2*p34) + (3*Mtop2 + 4*p34)*p45) + p12*(4*p13*p34 + Mtop2*(4*p13 + p35 + p45)))*Pi**2)
     -        /((Mch2 - Mtop2 - 2*p13)*p15*p24*(Mch2 + Mtop2 + 2*p34)) + 
     -      (16*alphas**2*(A**2 + B**2)*CF*(-(Mtop2*p13*(2*(Mtop2 + p34) + p45)) + 
     -           p15*(2*mt**4 - Mch2*Mtop2 + 5*Mtop2*p34 + 4*p34**2 + 4*(Mtop2 + p34)*p35 + 8*(Mtop2 + p34)*p45) + 
     -           2*p12*(-2*p34**2 - 2*p34*(Mtop2 + p35 + 2*p45) - Mtop2*(-Mch2 + Mtop2 + p35 + 3*p45)) + 
     -           p14*(4*p34**2 + 4*p34*(Mtop2 + p35 + 2*p45) + Mtop2*(-Mch2 + Mtop2 + 3*p35 + 6*p45)))*Pi**2)/(p12*(Mch2 + Mtop2 + 2*p34)*(p24 + p25 - p45)*p45) 
     -       + 16*alphas**2*(A**2 + B**2)*CF*((4*((Mch2 - Mtop2)*p14 - 2*p13*(Mtop2 + p34))*(p13 + p14 + p15 - p35 - p45))/(p12*p15*(Mch2 + Mtop2 + 2*p34)**2) - 
     -         (2*p15**2*(Mtop2 + p34) + p14*(Mtop2*(-2*p13 + p35) + 2*p25*(Mtop2 + p34 + p35) + 2*p14*(Mtop2 + 2*p34 + p35)) + 
     -            (p13*(Mtop2 - 2*p25) - 2*p14*(Mtop2 + p13 + 2*p34 + p35))*p45 + 2*p13*p45**2 + 
     -            2*p12*(-(p14*(2*(Mtop2 + p34) + p35)) + (Mtop2 + p13 + p34)*p45) + 
     -            p15*(mt**4 + 4*Mtop2*p14 + Mtop2*p34 + 6*p14*p34 - 2*p12*(Mtop2 + p34) + 2*(Mtop2 + p14)*p35 - 2*p24*(Mtop2 + p34 + p35) + 2*p23*p45 - 
     -               2*p34*p45 - 2*p13*(Mtop2 + p45)))/(p15*(Mch2 + Mtop2 + 2*p34)*(p24 + p25 - p45)*p45) - 
     -         ((p12 - p15)*(2*mt**4 - (Mtop2 + 2*p24 + 2*p25 - 2*p45)*(p24 - p45)) - p14*(2*p25*(p24 + p25 - p45) + Mtop2*(2*Mtop2 - p24 + p45)))/
     -          (p24*(p24 + p25 - p45)**2*p45) + (2*(p14*((Mch2 - Mtop2)*p14 - 2*p13*(Mtop2 + p34) + (Mtop2 - p13 + p34)*p35 + p35**2) + 
     -               (p13*(Mtop2 + p13 + p34 - p35) + p14*(-Mch2 + Mtop2 + p35))*p45 - p13*p45**2) + 
     -            p15*((mh - mt)*(mh + mt)*Mtop2 + 2*Mch2*p14 - 2*Mtop2*p14 - 2*(Mtop2 + p34)*(p13 + p34 + p35 + p45)))/(p12*p15*(Mch2 + Mtop2 + 2*p34)*p45) + 
     -         (2*(p15*(Mch2*p24 - p23*(Mtop2 + 2*p34) + (Mtop2 + p34)*(Mtop2 + 2*p34)) + 
     -              p13*(2*Mtop2*p23 - 2*Mtop2*(Mtop2 + p34) + p25*(3*Mtop2 + 2*p34) - p24*(Mch2 - 3*Mtop2 + 2*p35) - (3*Mtop2 + 2*p34)*p45) + 
     -              p12*(Mtop2*(p34 + p35) + Mch2*(2*Mtop2 + p34 + p45)) - 
     -              p14*((Mch2 - 2*Mtop2)*p25 + 2*p34*(Mch2 + Mtop2 + p35) - p23*(Mch2 + Mtop2 + 2*p35) + Mtop2*(3*Mch2 + Mtop2 - 2*p24 + 3*p35 + 2*p45))))/
     -          (p12*(Mch2 - Mtop2 - 2*p13)*p24*(Mch2 + Mtop2 + 2*p34)) - 
     -         (p25*((-Mch2 + Mtop2)*p14 + 2*p13*(Mtop2 + p34)) + 
     -            p15*(mt**4 - Mch2*Mtop2 + (Mch2 - Mtop2)*p24 - 2*p23*(Mtop2 + p34) + 2*(-(Mtop2*p13) + p34*(Mtop2 + p14 + p34))) + 
     -            p12*(-2*p15*(Mtop2 + p34) - 2*((-Mch2 + Mtop2)*p14 + 2*p13*(Mtop2 + p34)) + 2*(Mtop2 + p14 + p34)*p35 + (-Mch2 + Mtop2 - 2*p13)*p45) + 
     -            2*(-(Mtop2*p13**2) + (Mtop2*p13 - p14*(p14 + p34))*p35 + Mch2*p14*(-p14 + p45) + p13*(2*p14*p34 + (p14 - p34)*p45)))/
     -          (p12*p15*p24*(Mch2 + Mtop2 + 2*p34)) - (p15*((mh - mt)*(mh + mt)*Mtop2 - Mch2*p24 + 2*p23*(Mtop2 + p34) - 2*p34*(2*Mtop2 + p34)) + 
     -            p12*(mt**4 - p34*(Mtop2 + 2*p34 + 2*p35) + (Mch2 + 2*Mtop2 - 2*p34)*p45) + 
     -            p14*((mh - mt)*(mh + mt)*Mtop2 + (Mch2 - 2*Mtop2)*p25 + p23*(Mtop2 + 2*p34 + 2*p45) - 2*(Mtop2*p24 + p34*(Mtop2 - p35) + (Mch2 + Mtop2)*p45)) + 
     -            p13*(-2*p25*(Mtop2 + p34) - p24*(3*Mtop2 + 2*p34 + 2*p45) + 2*(mt**4 - Mtop2*p35 + (Mtop2 + p34)*p45)))/(p12*(Mch2 - Mtop2 - 2*p13)*p24*p45) + 
     -         (p15**2*p24 + p14*((Mtop2 + p24)*p25 + p25**2 + 2*Mtop2*(p14 - p45)) + p12**2*p45 + 
     -            p12*(-(p14*(2*(Mtop2 + p24) + p25)) + (Mtop2 + p24 - p25)*p45) + 
     -            p15*(mt**4 - p24**2 - p24*(Mtop2 + p25 - 2*p45) - p12*(Mtop2 + p24 + p45) - p45*(Mtop2 - 2*p25 + 2*p45) + p14*(-p25 + 2*(Mtop2 + p45))))/
     -          (p15*p24*(p24 + p25 - p45)*p45) - (p14*(Mtop2*(-2*p13 + p35) + 2*p25*(Mtop2 + p34 + p35) + 2*p14*(Mtop2 + 2*p34 + p35)) + 
     -            (4*p14**2 + p13*(Mtop2 - 2*p25) - 2*p14*(Mtop2 + p13 - p25 + 2*p34 + p35))*p45 + 2*(p13 - 2*p14)*p45**2 + 
     -            p15*(mt**4 - 2*p12*(Mtop2 + p34) + p34*(Mtop2 + 2*p14 - 2*p45) + 2*p23*p45 - 2*p24*(Mtop2 + p34 + p35 + p45)) + 
     -            2*p12*(p45*(Mtop2 + p13 + p34 + p45) - p14*(p35 + 2*(Mtop2 + p34 + p45))))/(2.*p12*p15*p24*p45))*Pi**2 + 
     -      (CF*NC**2*((-64*alphas**2*(A**2 + B**2)*(Mtop2*(-Mch2 + Mtop2)*p14 + (Mch2 - Mtop2)*p12*(Mtop2 + p24) + 
     -                2*p13*(-(p23*(Mtop2 + p24)) + Mtop2*(2*Mtop2 - 2*p24 + p34)))*Pi**2)/((-Mch2 + Mtop2 + 2*p13)**2*p24**2) + 
     -           (32*alphas**2*(A**2 + B**2)*((Mtop2 + p24)*p25 - Mtop2*p45)*Pi**2)/(p15*p24**2) + 
     -           (64*alphas**2*(A**2 + B**2)*(2*(Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45)*Pi**2)/(p15*(Mch2 + Mtop2 + 2*p34)**2) - 
     -           (64*alphas**2*(A**2 + B**2)*(2*p25*(Mtop2 + p34) + Mtop2*p35 - (Mtop2 + 2*p34)*p45)*Pi**2)/(p15*p24*(Mch2 + Mtop2 + 2*p34)) + 
     -           (32*alphas**2*(A**2 + B**2)*(p25*((-Mch2 + Mtop2)*p14 + 2*p13*(9*Mtop2 + p34)) + p24*(7*(-Mch2 + Mtop2)*p15 + 2*p13*(-2*p23 + 7*p35)) + 
     -                (Mch2 - Mtop2)*p12*(2*p24 - 7*p45) + 2*(Mch2 - Mtop2)*p15*p45 + 2*p13*(7*p23 - 2*p35)*p45)*Pi**2)/((-Mch2 + Mtop2 + 2*p13)**2*p25**2) - 
     -           (64*alphas**2*(A**2 + B**2)*(2*Mtop2*p13*p15 + 2*Mch2*p15*p24 - 2*Mtop2*p15*p24 + 2*Mtop2*p13*p25 - (Mch2 - Mtop2)*p14*(p15 + p25) + 
     -                2*p13*p15*p34 + 2*p13*p25*p34 - 4*p15*p23*(Mtop2 + p34) - 2*Mtop2*p15*p35 - 2*p15*p34*p35 + Mch2*p15*p45 - Mtop2*p15*p45 + 
     -                p12*(-2*((-Mch2 + Mtop2)*p14 + 2*p13*(Mtop2 + p34)) + 2*(Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45))*Pi**2)/(p15*p25*(Mch2 + Mtop2 + 2*p34)**2)
     -             + (32*alphas**2*(A**2 + B**2)*(2*p15*((Mtop2 + p34)*(Mch2 + Mtop2 + 2*p34) + 2*(Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45) + 
     -                (Mch2 + Mtop2 + 2*p34)*(p14*(Mch2 + Mtop2 + 2*p34 + 2*p35) - 2*p13*p45))*Pi**2)/(p12**2*(Mch2 + Mtop2 + 2*p34)**2) + 
     -           (32*alphas**2*(A**2 + B**2)*(2*p15*(p34*(Mch2 + Mtop2 - 2*p45) + Mtop2*(2*Mch2 + p35 - p45)) + 
     -                p13*(-3*mt**4 + Mch2*Mtop2 - 6*Mtop2*p34 - 4*p34**2 - 4*(Mtop2 + p34)*p35 - 8*(Mtop2 + p34)*p45) + 
     -                p14*(-mt**4 + 3*Mch2*Mtop2 + 2*Mch2*p34 - 4*(Mtop2 + p34)*p45))*Pi**2)/(p12**2*(Mch2 + Mtop2 + 2*p34)*p45) + 
     -           (32*alphas**2*(A**2 + B**2)*(Mtop2*p13*(Mtop2 + p45) - p14*(mt**4 + 2*Mtop2*p34 + 2*p35*(Mtop2 - p45) + 3*Mtop2*p45 - 2*p45**2) - 
     -                p15*(mt**4 + 2*Mtop2*p34 + 2*p35*(Mtop2 - p45) + 3*Mtop2*p45 - 2*p45**2) + 
     -                2*p12*(mt**4 + Mtop2*p34 + p35*(Mtop2 - p45) + 2*Mtop2*p45 - p45**2))*Pi**2)/(p12*(p24 + p25 - p45)*p45**2) - 
     -           (64*alphas**2*(A**2 + B**2)*(Mtop2*p15*(Mtop2 - p24) + 2*p13*(p12*(Mtop2 + p24) - (Mtop2 + p24)*p25 + Mtop2*(-p14 + p45)))*Pi**2)/
     -            ((Mch2 - Mtop2 - 2*p13)*p15*p24**2) - (32*alphas**2*(A**2 + B**2)*
     -              (2*Mtop2*p13*p15 + 2*Mtop2*p14*p15 + 5*Mtop2*p13*p25 - Mch2*p14*p25 + 2*Mtop2*p14*p25 + 4*p13*p25*p34 + 2*p14*p35**2 + 
     -                p24*((Mch2 - 4*Mtop2)*p15 + 2*p13*(-p13 + p35)) + 4*p13**2*p45 + 2*Mch2*p15*p45 - 2*Mtop2*p15*p45 + 
     -                p12*(-2*(2*Mtop2*p14 + p13*(5*Mtop2 + 3*p34)) + (5*Mtop2 + 2*p34)*p35 + (Mch2 + 2*Mtop2)*p45) + 
     -                p23*(-(p15*(7*Mtop2 + 4*p34)) + 2*p14*(p13 + p35) - 4*p13*p45) - 2*p35*(p15*(Mtop2 + p34) + p13*(2*p14 + p45)))*Pi**2)/
     -            ((Mch2 - Mtop2 - 2*p13)*p15*p25*(Mch2 + Mtop2 + 2*p34)) + 
     -           (32*alphas**2*(A**2 + B**2)*(p15*(mt**4 + (Mtop2 + p24)*p45) - p12*(Mtop2*(Mtop2 + p24 + p25) - p25*p45 + p45**2) + 
     -                p14*(mt**4 + p45*(Mtop2 - p25 + p45)))*Pi**2)/((p24 + p25 - p45)**2*p45**2) - 
     -           (64*alphas**2*(A**2 + B**2)*(-(p15*((-Mch2 + Mtop2)*p24 + 2*p23*(Mtop2 + p34) + (Mtop2 + p34)*(Mch2 + Mtop2 + 2*p34) - 2*(Mtop2 + p34)*p35 + 
     -                     (Mch2 - Mtop2)*p45)) + 2*p12*((Mtop2 + p34)*(Mch2 + Mtop2 + 2*p34) + 2*(Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45) + 
     -                p14*(2*(Mch2 - Mtop2)*p24 + (Mch2 - Mtop2)*p25 + p23*(Mch2 - 3*Mtop2 - 2*p34) + (Mch2 + 3*Mtop2 + 4*p34)*p35 + (-Mch2 + Mtop2)*p45) + 
     -                p13*(-(Mtop2*(4*p23 + 3*p24 + 2*p25 - 2*p35)) + Mch2*(p24 - 2*p45) - 2*p34*(2*p23 + p24 + p25 - p35 + p45)))*Pi**2)/
     -            (p12*p25*(Mch2 + Mtop2 + 2*p34)**2) + (16*alphas**2*(A**2 + B**2)*
     -              (p15*((mh - mt)*(mh + mt)*Mtop2 - (Mch2 + 3*Mtop2 + 2*p34)*p45) - 2*p13*(mt**4 + Mtop2*p34 + p35*(Mtop2 - p45) + 2*Mtop2*p45 - p45**2) + 
     -                p14*((mh - mt)*(mh + mt)*Mtop2 + 2*p45*(-Mtop2 + p35 + p45)))*Pi**2)/(p12**2*p45**2) + 
     -           (64*alphas**2*(A**2 + B**2)*(Mtop2*p15*(Mtop2 + p23 + p24 + p34) - p25*(Mtop2*p14 + p13*(5*Mtop2 + 4*p34)) - Mtop2*(2*p13 + p14)*p35 + 
     -                p13*(2*Mtop2*p13 - 2*p14*(Mtop2 + 2*p34) + (3*Mtop2 + 4*p34)*p45) + p12*(4*p13*p34 + Mtop2*(4*p13 + p35 + p45)))*Pi**2)/
     -            ((Mch2 - Mtop2 - 2*p13)*p15*p24*(Mch2 + Mtop2 + 2*p34)) - 
     -           (32*alphas**2*(A**2 + B**2)*(p12*(4*mt**4 + 6*Mch2*Mtop2 + p34*(3*Mch2 + 11*Mtop2 + 4*p34) + (7*Mtop2 + 4*p34)*p35 - (Mch2 - 4*Mtop2)*p45) + 
     -                p14*(-4*Mtop2*p24 + (Mch2 - 2*Mtop2)*p25 + (-Mch2 + 5*Mtop2 + 2*p34)*p35 - 2*p35**2 - p23*(Mch2 + 7*Mtop2 + 4*p34 + 2*p35) + 2*Mtop2*p45) - 
     -                p13*(p25*(5*Mtop2 + 4*p34) + p24*(-3*Mch2 + 7*Mtop2 - 4*p35) + (3*Mch2 + Mtop2 + 6*p34)*p45 - 2*p35*(3*Mtop2 + 2*p34 + p45) + 
     -                   2*p23*(6*Mtop2 + 4*p34 + p45)) - p15*(2*Mtop2*(3*Mch2 + Mtop2) + 2*p34**2 + p34*(7*Mtop2 + 2*p23 - 2*p35) + 
     -                   Mtop2*(5*p23 + 2*p24 - 2*p35 - 2*p45) + Mch2*(p24 + 3*p34 + 2*p45)))*Pi**2)/(p12*(Mch2 - Mtop2 - 2*p13)*p25*(Mch2 + Mtop2 + 2*p34)) - 
     -           (32*alphas**2*(A**2 + B**2)*(p12*(mt**4 + Mtop2*p34 + 3*p25*(Mtop2 + p34) - 2*Mtop2*p35 + p24*(3*Mtop2 + 4*p34 + p35) - 5*Mtop2*p45 - 
     -                   3*p34*p45 - p23*(Mtop2 + p45)) + p14*(p25*(Mtop2 + 2*p34 + p35) - p24*(Mtop2 + 2*p34 + 2*p35) + p35*(Mtop2 - p45) - 
     -                   (Mtop2 + 2*p34)*p45 + p23*(Mtop2 + p45)) + p13*(-(p25*(Mtop2 + p45)) + p45*(p24 + p45)) + 
     -                p15*(-(p24*(2*Mtop2 + 4*p34 + 3*p35)) + p34*(Mtop2 + p45) + Mtop2*(Mtop2 + p35 + 2*p45) + p23*(2*Mtop2 + 3*p45)))*Pi**2)/
     -            (p25*(Mch2 + Mtop2 + 2*p34)*(p24 + p25 - p45)*p45) + 
     -           (32*alphas**2*(A**2 + B**2)*(p25*((Mch2 - 2*Mtop2)*p14 - p13*(5*Mtop2 + 4*p34)) + 
     -                p15*((-Mch2 + 4*Mtop2)*p24 + p23*(Mtop2 - 2*p34) + 2*(mt**4 - Mch2*Mtop2 + p34*(Mtop2 + p34))) + 
     -                2*p14*(-p23**2 - Mtop2*p24 + p23*(Mtop2 + 2*p34 - p35) + (Mtop2 - p34)*p35 + 2*Mtop2*p45) + 
     -                p12*((mh - mt)*(mh + mt)*Mtop2 - 4*p34**2 + 2*p34*(-2*Mtop2 + p23 + 2*p35) + Mtop2*(2*p23 + 2*p24 + p35 - 2*p45) - Mch2*(2*p24 + p45)) + 
     -                2*p13*(2*p24*(-Mtop2 + p35) + p23*(-2*Mtop2 + p24 - p45) + 3*p34*p45 + Mtop2*(p35 + 4*p45)))*Pi**2)/
     -            ((Mch2 - Mtop2 - 2*p13)*p24*p25*(Mch2 + Mtop2 + 2*p34)) - 
     -           (32*alphas**2*(A**2 + B**2)*(-(Mtop2*p13*(2*(Mtop2 + p34) + p45)) + 
     -                p15*(2*mt**4 - Mch2*Mtop2 + 5*Mtop2*p34 + 4*p34**2 + 4*(Mtop2 + p34)*p35 + 8*(Mtop2 + p34)*p45) + 
     -                2*p12*(-2*p34**2 - 2*p34*(Mtop2 + p35 + 2*p45) - Mtop2*(-Mch2 + Mtop2 + p35 + 3*p45)) + 
     -                p14*(4*p34**2 + 4*p34*(Mtop2 + p35 + 2*p45) + Mtop2*(-Mch2 + Mtop2 + 3*p35 + 6*p45)))*Pi**2)/
     -            (p12*(Mch2 + Mtop2 + 2*p34)*(p24 + p25 - p45)*p45) - 
     -           (16*alphas**2*(A**2 + B**2)*(-(p25*((Mch2 + 2*Mtop2)*p14 + p13*(3*Mtop2 + 2*p34))) + 
     -                p12*(2*(mh - mt)*(mh + mt)*Mtop2 - 2*p34*(Mtop2 + p34) - (3*Mtop2 + 2*p34)*p35 - (Mch2 + 8*Mtop2 + 6*p34)*p45) + 
     -                2*p14*(-2*Mtop2*p24 + (2*Mtop2 + p34)*p35 - p35**2 + Mtop2*p45 + p23*(-Mtop2 + p34 + 2*p35 + 3*p45)) - 
     -                2*p13*(p35*(Mtop2 - 3*p45) + p23*(Mtop2 - 2*p45) + p24*(4*Mtop2 + 3*p34 + 2*p35 + 3*p45) - p45*(2*Mtop2 + 3*p34 + 6*p45)) + 
     -                p15*(mt**4 - Mch2*Mtop2 + Mch2*p24 - 2*p34**2 - 2*p34*(Mtop2 + p23 - p35) + Mtop2*(-3*p23 - 2*p24 + 6*p35 + 8*p45)))*Pi**2)/
     -            (p12*(Mch2 - Mtop2 - 2*p13)*p25*p45) - (64*alphas**2*(A**2 + B**2)*
     -              (-2*p14*p23**2 - 10*Mtop2*p13*p25 - 7*Mch2*p14*p25 - Mtop2*p14*p25 - 2*p13*p25*p34 - 2*p14*p35**2 + 7*p24*((Mch2 - Mtop2)*p15 - p13*p35) + 
     -                p23*((2*p12 - 7*p15)*(Mtop2 + p34) + 14*p14*p35 + p13*(2*p24 - 7*p45)) - 2*Mch2*p15*p45 + 2*Mtop2*p15*p45 + 
     -                2*p35*(p15*(Mtop2 + p34) + p13*p45) - p12*(2*(Mch2 - Mtop2)*p24 + 7*((Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45)))*Pi**2)/
     -            ((Mch2 - Mtop2 - 2*p13)*p25**2*(Mch2 + Mtop2 + 2*p34)) - 
     -           (32*alphas**2*(A**2 + B**2)*(7*(Mch2 - Mtop2)*p15*p24 - 2*Mtop2*p13*p25 + Mch2*p14*p25 - Mtop2*p14*p25 - 2*p13*p25*p34 + 
     -                2*(2*p12 - 7*p15)*p23*(Mtop2 + p34) + 4*p15*(Mtop2 + p34)*p35 - 2*Mch2*p15*p45 + 2*Mtop2*p15*p45 - 
     -                p12*(2*(Mch2 - Mtop2)*p24 + 7*(2*(Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45)))*Pi**2)/(p25**2*(Mch2 + Mtop2 + 2*p34)**2) + 
     -           (16*alphas**2*(A**2 + B**2)*(-2*p12**2*(3*Mtop2 + p34) + 
     -                p25*(2*p12*(3*Mtop2 + p34) - 2*p15*(3*Mtop2 + p34) - p14*(Mtop2 + 2*p23 - 2*p35) + p13*(3*Mtop2 + 4*p14 + 6*p24 - 6*p45)) + 
     -                p12*(-2*p13*(2*Mtop2 + 3*p24) + 2*p14*(Mtop2 + p23 - p34) + p15*(6*Mtop2 + 4*p34) + (Mtop2 - 2*p14)*p35 + (-Mtop2 + 2*p13 + 2*p34)*p45) - 
     -                p24*((Mtop2 + 2*p13)*p15 + 6*p13*(-p14 + p45)) - p23*(-2*p14**2 + Mtop2*p15 + 2*p14*(p15 + p45)) + 
     -                2*((-p14 + p45)*(-(p14*p35) + 3*p13*p45) + p15*(Mtop2*p13 - p14*(2*Mtop2 + p34) + (2*Mtop2 + p34)*p45)))*Pi**2)/
     -            ((Mch2 - Mtop2 - 2*p13)*p15*p24*p25) - (32*alphas**2*(A**2 + B**2)*
     -              (p14*(p24*(-Mtop2 + p35) + p25*(Mtop2 - p23 + p35) - p23*(Mtop2 + 2*p24 - 3*p45) - Mtop2*(p35 + p45)) + 
     -                p12*(mt**4 + Mtop2*p34 + p25*(3*Mtop2 + p34) + p24*(3*Mtop2 + 2*p34 + p35) - (5*Mtop2 + 3*p34)*p45 - p35*(Mtop2 + 3*p45)) + 
     -                p15*(mt**4 + (Mtop2 - p24 - p25)*p34 + 2*Mtop2*(-p24 + p45) + p23*(Mtop2 - p24 + 3*p45)) + 
     -                p13*(-((Mtop2 - 2*p24)*p25) + 2*(p24**2 - p24*p45 + p45**2)))*Pi**2)/((Mch2 - Mtop2 - 2*p13)*p25*(p24 + p25 - p45)*p45) + 
     -           (16*alphas**2*(A**2 + B**2)*(-6*p12**2*(Mtop2 + p34) + 
     -                p25*(6*p12*(Mtop2 + p34) - 6*p15*(Mtop2 + p34) - p14*(Mtop2 + 2*p23 + 4*p34 - 2*p35) + p13*(3*Mtop2 + 2*p24 - 2*p45)) + 
     -                p24*(-(p15*(Mtop2 - 2*p34)) + 2*p14*(p13 + p35) - 4*p13*p45) + 
     -                p12*(-2*p13*(2*Mtop2 + p24) + 6*p15*(Mtop2 + p34) + 2*p14*(Mtop2 + p23 + 3*p34) + (Mtop2 + 2*p14)*p35 - (Mtop2 + 2*p13 + 2*p34)*p45) + 
     -                p23*(-3*Mtop2*p15 + 2*p14*(-p14 + p45)) + 2*
     -                 (p13*p45*(p14 + p45) + p15*(Mtop2*p13 - p14*(2*Mtop2 + 3*p34) + (2*Mtop2 + 3*p34)*p45) - p35*(Mtop2*p15 + p14*(p14 + p45))))*Pi**2)/
     -            (p15*p24*p25*(Mch2 + Mtop2 + 2*p34)) + (32*alphas**2*(A**2 + B**2)*
     -              ((Mch2 - Mtop2)*p12*(Mtop2 - 2*p24 + 2*p45) + 
     -                2*((-Mch2 + Mtop2)*p15*(Mtop2 + 2*p24) + (Mch2 - Mtop2)*p14*(p24 + p25 - 2*p45) + 
     -                   p13*(-(p25*(3*Mtop2 + 2*p34)) + 2*Mtop2*p35 + p24*(-3*Mtop2 - 2*p34 + 4*p35) + 6*Mtop2*p45 + 4*p34*p45 - p23*(Mtop2 - 2*p24 + 2*p45))))*
     -              Pi**2)/((-Mch2 + Mtop2 + 2*p13)**2*p24*p25) - 
     -           (16*alphas**2*(A**2 + B**2)*(2*p14*(2*(Mch2 - Mtop2)*p24 + (Mch2 - Mtop2)*p25 - p35**2 - p23*(2*(Mtop2 + p34) + p35) + 
     -                   p35*(Mtop2 + p34 - 3*p45) + (-Mch2 + Mtop2)*p45) + 
     -                2*p12*((mh - mt)*(mh + mt)*Mtop2 - 2*p34*(Mtop2 + p34) - (Mtop2 + 2*p34)*p35 - (Mch2 + 4*Mtop2 + 6*p34)*p45) + 
     -                2*p13*(-(p25*(Mtop2 + p34)) - p24*(2*(Mtop2 + p34) + p35) + p45*(Mtop2 + 2*p23 + p34 + p35 + 3*p45)) + 
     -                p15*(mt**4 - Mch2*Mtop2 + 2*Mch2*p24 + 2*(p34**2 + p34*(Mtop2 - p23 + 3*p35 + 3*p45) - Mtop2*(p23 + p24 - 4*(p35 + p45)))))*Pi**2)/
     -            (p12*p25*(Mch2 + Mtop2 + 2*p34)*p45) - 2*((-32*alphas**2*(A**2 + B**2)*((Mch2 - Mtop2)*p15 - 2*p13*p35)*p45*Pi**2)/
     -               ((-Mch2 + Mtop2 + 2*p13)**2*p25**2) + (32*alphas**2*(A**2 + B**2)*p15*(2*(Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45)*Pi**2)/
     -               (p25**2*(Mch2 + Mtop2 + 2*p34)**2) + (64*alphas**2*(A**2 + B**2)*
     -                 (-(p14*p35**2) + (-Mch2 + Mtop2)*p15*p45 + p35*(p15*(Mtop2 + p34) + p13*p45))*Pi**2)/((Mch2 - Mtop2 - 2*p13)*p25**2*(Mch2 + Mtop2 + 2*p34))
     -              )))/2.))/(Sqrt(2d0)*NC*(-1 + NC**2))     
      
 
      end

c-------------------------------------------------------------------------------c                                    1 2    3     4 5
c-------------------  subroutine for g g -> H^{-} t bbar  ----------------------c
c-------------------------------------------------------------------------------c 
           
      subroutine ggproc(pphy,amp2,rflav3)
      implicit none
      real * 8 pphy(0:3,5)          
      real * 8 amp2
      integer rflav3
      real * 8 p1(0:3),p2(0:3),p3(0:3),p4(0:3),p5(0:3)
c      include 'couplings.h'
      real * 8 p12,p13,p14,p15,p23,p24,p25,p34,p35,p45,ss,dentop,
     1     dentop2,dentopx,dentop2x,dentopxtimesdentop
      real * 8 Mtop, Mch,mt,mh, Mtop2, Mch2, A, B, alphas, GF
      real * 8 amp2dr,subtr
      real * 8 ppp(0:3,5)
      real * 8 dotp
      complex * 16 ccdotp
      integer mu,i
 
      include 'PhysPars.h'
      include '../include/pwhg_math.h'
      include '../include/pwhg_st.h'
      include 'coupl.inc'      
      include 'doublyresonant.h'   

      logical nearresonance
   
      Mch = ph_mCH
      Mtop = ph_mT
      Mch2 = Mch**2
      Mtop2 = Mtop**2
      mt=mtop
      mh=mch
      
      GF=ph_GF
      A=ph_A
      B=ph_B
      alphas=st_alpha
      
      do mu=0,3
         p1(mu) = pphy(mu,1)
         p2(mu) = pphy(mu,2)
         p3(mu) = pphy(mu,3)
         p4(mu) = pphy(mu,4)
         p5(mu) = pphy(mu,5)
      enddo

c    define all the possible scalar products, even if they are not independent      
      p12=dotp(p1,p2)
      p13=dotp(p1,p3)
      p14=dotp(p1,p4)
      p15=dotp(p1,p5)
      p23=dotp(p2,p3)
      p24=dotp(p2,p4)
      p25=dotp(p2,p5)
      p34=dotp(p3,p4)
      p35=dotp(p3,p5)
      p45=dotp(p4,p5)
      
      ghmq=sqrt(gf*sqrt(2d0)*(a**2+b**2))
      ss=2*p35+Mch2
      dentop2=(ss-Mtop2)**2+ph_Twidth**2*Mtop2
      dentop=dentop2/(ss-Mtop2)
      dentopx=dentop/2
      dentop2x=dentop2/4
      dentopxtimesdentop=dentop2/2

CCCcc  should be lt, if not, was for test, and forgot to change back!!!      
      amp2dr = (GF*((64*alphas**2*(A**2 + B**2)*CF**2*NC*(-((Mch2 - Mtop2)*p14*(p12**2 - 2*p12*p25 + 2*p25**2)) + 2*p13*(p12**2 - 2*p12*p25 + 2*p25**2)*(Mtop2 + p34) + 
     -           2*p15*p25*((-Mch2 + Mtop2)*p24 + 2*p23*(Mtop2 + p34)))*Pi**2)/(p12**2*p15*(Mch2 + Mtop2 + 2*p34)**2) + 
     -      (64*alphas**2*(A**2 + B**2)*CF**2*NC*((-Mch2 + Mtop2)*p12**2*p24 + 2*(Mch2 - Mtop2)*p12*p15*p24 + 
     -           2*(p12**2 - 2*p12*p15 + 2*p15**2)*p23*(Mtop2 + p34) + 2*p15*((-Mch2 + Mtop2)*p15*p24 + p25*((-Mch2 + Mtop2)*p14 + 2*p13*(Mtop2 + p34))))*Pi**2)/
     -       (p12**2*p25*(Mch2 + Mtop2 + 2*p34)**2) + (32*alphas**2*(A**2 + B**2)*CF*NC**2*
     -         (p15*((-Mch2 + Mtop2)*p14 + (Mch2 - Mtop2)*p24 + 2*p13*(Mtop2 + p34) - 2*p23*(Mtop2 + p34)) + 
     -           p25*((Mch2 - Mtop2)*p14 + (-Mch2 + Mtop2)*p24 - 2*p13*(Mtop2 + p34) + 2*p23*(Mtop2 + p34)) + p12*(2*(Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45))*
     -         Pi**2)/(p12**2*(Mch2 + Mtop2 + 2*p34)**2) + (16*alphas**2*(A**2 + B**2)*CF*NC**2*
     -         (p25**2*((-Mch2 + Mtop2)*p14 + 2*p13*(Mtop2 + p34)) + 
     -           (-p12 + p15)*(-(p15*((-Mch2 + Mtop2)*p24 + 2*p23*(Mtop2 + p34))) + p12*(2*(Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45)) - 
     -           p25*(2*(2*p12 - 3*p15)*p23*(Mtop2 + p34) + 3*p15*((-Mch2 + Mtop2)*p14 + (Mch2 - Mtop2)*p24 + 2*p13*(Mtop2 + p34)) + 
     -              p12*((Mch2 - Mtop2)*p14 + 2*(-Mch2 + Mtop2)*p24 - 2*p13*(Mtop2 + p34) + 2*(Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45)))*Pi**2)/
     -       (p12**2*p25*(Mch2 + Mtop2 + 2*p34)**2) - (16*alphas**2*(A**2 + B**2)*CF*NC**2*
     -         (p25**2*((-Mch2 + Mtop2)*p14 + 2*p13*(Mtop2 + p34)) - p15**2*((-Mch2 + Mtop2)*p24 + 2*p23*(Mtop2 + p34)) + 
     -           p12*p15*(4*Mtop2*p13 + 2*Mtop2*p14 - Mtop2*p24 + 4*p13*p34 - 2*p23*(Mtop2 + p34) + 2*(Mtop2 + p34)*p35 + Mch2*(-2*p14 + p24 - p45) + 
     -              Mtop2*p45) + p12**2*(2*(Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45) - 
     -           p25*(3*p15*((-Mch2 + Mtop2)*p14 + (Mch2 - Mtop2)*p24 + 2*p13*(Mtop2 + p34) - 2*p23*(Mtop2 + p34)) + 
     -              p12*(Mtop2*p14 + 2*p13*(Mtop2 + p34) + 2*(Mtop2 + p34)*p35 + Mtop2*p45 - Mch2*(p14 + p45))))*Pi**2)/(p12**2*p15*(Mch2 + Mtop2 + 2*p34)**2) - 
     -      (64*alphas**2*(A**2 + B**2)*CF**2*NC*(p15*p24*(-(p14*p23) + p13*p24) + p12**3*(Mtop2 + p34) + 
     -           2*p25**2*(p13*p24 + p12*(Mtop2 + p34) + p14*(Mtop2 - p23 + p34)) + 
     -           p12**2*(p13*(Mtop2 + p24) - p14*(p23 + p34) - Mtop2*p35 + (Mtop2 + 2*p34)*p45) - 
     -           p25*(2*p12**2*(Mtop2 + p34) + p14*p23*(p14 - 2*p45) - p24*(2*p15*(Mtop2 + p34) + p14*(p13 - 4*p35) + 2*p13*p45) + 
     -              p12*(p14*(Mtop2 - 2*p23) + p13*(Mtop2 + 2*p24) - 2*Mtop2*p35 + 2*(Mtop2 + 2*p34)*p45)) + 
     -           p12*(p23*(Mtop2*p15 + p14*(p14 - p45)) - p24*(p15*(Mtop2 + 2*p34) - 2*p14*p35 + p13*(p14 + p45))))*Pi**2)/
     -       (p12**2*p15*p24*(Mch2 + Mtop2 + 2*p34)) + (32*alphas**2*(A**2 + B**2)*CF**2*NC*
     -         (p12**3*(Mtop2 + p14) + 2*p14*(p14*(p15 + p24)*p25 - p15*p24*(p15 + p24 - 2*p45)) + 
     -           p12**2*(-((Mtop2 + 2*p14)*p24) + (Mtop2 + p14)*(-2*p15 + p45)) + 
     -           p12*(2*p14*p24**2 + p24*((Mtop2 + 3*p14)*p15 - 2*p14*p45) - (Mtop2 + p14)*(p14*p25 + 2*p15*(-p15 + p45))))*Pi**2)/(p12**2*p14**2*p25) + 
     -      (32*alphas**2*(A**2 + B**2)*CF**2*NC*(p12*(Mtop2 + p24)*(-(p15*p24) + 2*p25**2 + p12*(p12 - p14 + p45) + p25*(p14 - 2*(p12 + p45))) + 
     -           p24*(2*p15*p24*p25 + p14*(2*p15*p24 - 2*p25**2 - p12*(p12 - 2*p14 + 2*p45) + 2*p25*(p12 - p14 + 2*p45))))*Pi**2)/(p12**2*p15*p24**2) + 
     -      (32*alphas**2*(A**2 + B**2)*CF*((p12 - 4*p15)*p25**2*((-Mch2 + Mtop2)*p14 + 2*p13*(Mtop2 + p34)) + 
     -           p12*(p12 - p15)*(-(p15*((-Mch2 + Mtop2)*p24 + 2*p23*(Mtop2 + p34))) + p12*(2*(Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45)) + 
     -           p25*(-4*p15**2*((-Mch2 + Mtop2)*p24 + 2*p23*(Mtop2 + p34)) - 
     -              p12**2*(Mtop2*p14 + 2*p13*(Mtop2 + p34) + 2*(Mtop2 + p34)*p35 + Mtop2*p45 - Mch2*(p14 + p45)) + 
     -              p12*p15*(2*Mtop2*p13 + Mtop2*p14 + Mtop2*p24 + 2*p13*p34 + 2*p23*(Mtop2 + p34) + 4*(Mtop2 + p34)*p35 + 2*Mtop2*p45 - 
     -                 Mch2*(p14 + p24 + 2*p45))))*Pi**2)/(p12**2*p15*p25*(Mch2 + Mtop2 + 2*p34)**2) - 
     -      (8*alphas**2*(A**2 + B**2)*CF*NC**2*(2*p14*p25**2*(Mtop2 + p34) - p12**2*((Mtop2 + 2*p14)*p35 + (Mtop2 - 2*p13)*p45) - 
     -           2*p15*(p14*p15*p23 + p24**2*(-p13 + p35) + p24*(p15*(Mtop2 - p13 + p34) - p14*p35 + p23*(p14 - p45) + p13*p45)) + 
     -           p25*(-2*p12**2*(Mtop2 + p34) + p12*(Mtop2*p14 + p13*(Mtop2 + 2*p24) + 2*p24*p34 + 2*p15*(Mtop2 + p34) - 2*(Mtop2 + p34)*p45) + 
     -              2*p23*(-(p12*(Mtop2 + p14)) + p14*(p15 + p24 + p45)) + 
     -              2*(-(p13*p24**2) + p24*(p15*(Mtop2 - p13 + p34) - p14*p35) + p14*(-(p15*(Mtop2 + p34)) + p14*p35 - p13*p45))) + 
     -           p12*(p23*((Mtop2 + 2*p14)*p15 - 2*p45**2) + p24*(Mtop2*p15 + 2*p35*(p14 + p45) - 2*p13*(p15 + p45)) + 
     -              2*(p45*(-(p14*p35) + p13*p45) + p15*(p14*p35 + (Mtop2 - p13 + p34)*p45))))*Pi**2)/(p12**2*p14*p25*(Mch2 + Mtop2 + 2*p34)) + 
     -      (8*alphas**2*(A**2 + B**2)*CF*(2*p14**2*p25**3 + 2*p14*p25**2*(p14**2 + p12*(Mtop2 - p12 + p15 + 2*p24 - 2*p45) - p14*(p12 + p15 + p24 + 2*p45)) + 
     -           (-(p15*p24) + p12*p45)*(-2*p12**3 + p12**2*(Mtop2 + 2*p14 + 2*p15 + 2*p24 - 4*p45) - 2*p15*p24*(-p14 + p15 + p24 - 2*p45) + 
     -              2*p12*((p14 - 2*p45)*p45 + p24*(-p14 + p45) + p15*(-Mtop2 - 2*p14 + p24 + p45))) + 
     -           p25*(2*p12**3*(p14 + p45) - 2*p15*p24*(p15*p24 + 4*p14*p45) - 
     -              p12**2*(p14*(Mtop2 + 2*p14 + 2*p24) + 2*(Mtop2 - 3*p14 + 2*p24)*p45 - 2*p45**2 + 2*p15*(2*Mtop2 + p14 + p24 + p45)) + 
     -              2*p12*(p15*p24*(Mtop2 + p15 + 2*p24) + p14**2*(2*p15 + p24 - 2*p45) + p14*(p15*(Mtop2 + 2*p24) + 4*p45**2))))*Pi**2)/(p12**2*p14*p15*p24*p25)
     -        - (16*alphas**2*(A**2 + B**2)*CF*(p12**3*((Mtop2 + 2*p14)*p35 + (Mtop2 - 2*p13)*p45) - 
     -           p12**2*((Mtop2 + 2*p14)*p15*p23 + p35*(Mtop2*p15 + 2*p14*(p15 + p25 - p45)) + p45*((Mtop2 - 2*p13)*p15 + 2*p13*p45) + 
     -              p25*(Mtop2*(p13 + p14) + 2*(Mtop2 - p13 + p34)*p45) + p24*(Mtop2*p15 + 2*p14*p35 - 2*p13*(p15 + p45))) - 
     -           2*(p15**2*p24*(-(p24*p35) + p23*p45) + p14*p25**2*(2*p15*(Mtop2 + p34) + p14*(p23 - p35) + p13*(-p24 + p45)) + 
     -              p15*p25*(-(p13*p24**2) + p14*p23*(2*p15 + p45) + p24*(2*p15*(Mtop2 - p13 + p34) + p14*(p23 - 2*p35) + p13*p45))) + 
     -           p12*(2*p14*p25**2*(Mtop2 + p34) + p15*(p24*(Mtop2*p15 - 2*p13*(p15 + p24 - p45) - 2*p35*p45) + 
     -                 p23*(Mtop2*p15 + 2*p14*(p15 + p24 - p45) + 2*p45**2)) + 
     -              p25*(-2*p15**2*(Mtop2 + p34) + 2*p24*(p15*(Mtop2 - p13 + p34) - p13*p45) + 
     -                 p15*(-(Mtop2*p13) + p14*(Mtop2 + 2*p23 + 2*p34 + 2*p35) + (-2*p13 + 4*(Mtop2 + p34))*p45) + 
     -                 2*(-(p14**2*p35) + p14*(p13 + p23 - p35)*p45 + p13*p45**2))))*Pi**2)/(p12**2*p14*p15*p25*(Mch2 + Mtop2 + 2*p34)) + 
     -      (8*alphas**2*(A**2 + B**2)*CF*NC**2*(p12**2*(2*p15*(Mtop2 + p34) + (Mtop2 + 2*p24)*p35 + (Mtop2 - 2*p23)*p45) - 
     -           p12*(p24*((Mtop2 - 2*p13)*p15 + 2*p35*(p14 - p45)) + 
     -              p25*(Mtop2*p13 + p14*(Mtop2 - 2*p23) + 2*p15*(Mtop2 + p34) + 2*p24*(p13 + p35) + 2*(Mtop2 - p23 + p34)*p45) + 
     -              p23*((Mtop2 + 2*p14)*p15 + 2*p45*(-p14 + p45)) - 2*(p45*(-(p14*p35) + p13*p45) + p15*(Mtop2*p13 - p14*p34 + (Mtop2 + p34)*p45))) + 
     -           2*(p25**2*(p13*p24 + p14*(Mtop2 - p23 + p34)) + 
     -              p25*(p24*(p15*(Mtop2 - p13 + p34) + p14*(p13 - p35)) - p14*(p15*(Mtop2 - p23 + p34) - p14*p35 + p23*(p14 - p45) + p13*p45)) - 
     -              p15*(-(p14**2*p23) + p24**2*p35 + p24*(p15*(Mtop2 + p34) - p14*p35 - p23*p45 + p13*(p14 + p45)))))*Pi**2)/
     -       (p12**2*p15*p24*(Mch2 + Mtop2 + 2*p34)) - (64*alphas**2*(A**2 + B**2)*CF**2*NC*
     -         (-(p13*p15*p24**2) + p12**3*(Mtop2 + p34) + 2*p14*p15*p25*(Mtop2 + p34) - 
     -           p12**2*(2*p15*(Mtop2 + p34) + p24*(p13 + p34) + Mtop2*p35 - (Mtop2 + 2*p34)*p45) + 
     -           p24*(-(p13*p14*p25) + 2*p15**2*(Mtop2 - p13 + p34) + 2*p15*(-2*p14*p35 + p13*p45)) + 
     -           p12*(p13*p24**2 + 2*p15**2*(Mtop2 + p34) + p25*(Mtop2*p13 - p14*(Mtop2 + 2*p34)) + 2*Mtop2*p15*p35 - 2*p15*(Mtop2 + 2*p34)*p45 - 
     -              p24*((Mtop2 - 2*p13)*p15 - 2*p14*p35 + p13*p45)) + 
     -           p23*(p12**2*(Mtop2 + p14) - p12*(Mtop2*p15 + p14*(2*p15 + p24 + p45)) + p14*(p14*p25 + p15*(p24 + 2*(p15 + p45)))))*Pi**2)/
     -       (p12**2*p14*p25*(Mch2 + Mtop2 + 2*p34)) + (16*alphas**2*(A**2 + B**2)*CF*
     -         (-(p12**3*((Mtop2 + 2*p24)*p35 + (Mtop2 - 2*p23)*p45)) + 2*p15**2*p24*(p24*(p13 - p35) + p23*(-p14 + p45)) + 
     -           p25**2*(4*p13*p15*p24 + p12*(-(p14*(Mtop2 - 2*p23)) + p13*(-Mtop2 - 2*p24) + 2*p15*(Mtop2 + p34)) + 
     -              2*p14*(2*p15*(Mtop2 - p23 + p34) - p14*p35 + p13*p45)) - 
     -           2*p12*p15*(-(p24**2*p35) + p23*p45*(-p14 + p45) + p24*(p15*(Mtop2 + p34) + (p13 + p23 - p35)*p45)) + 
     -           p12**2*(p24*(p15*(Mtop2 + 2*p35) + 2*p35*(p14 - p45)) + 2*p15*(Mtop2 + p34)*p45 + p23*(p15*(Mtop2 - 2*p45) + 2*p45*(-p14 + p45))) + 
     -           p25*(p12**2*(p14*(Mtop2 - 2*p23) + p13*(Mtop2 + 2*p24) + (Mtop2 + 2*p24)*p35 + (Mtop2 - 2*p23)*p45) + 
     -              2*p15*(p14*p23*(-p14 + p45) + p24*(2*p15*(Mtop2 + p34) - 2*p14*p35 + p13*(p14 + p45))) + 
     -              p12*(-(p24*(p15*(Mtop2 + 2*p13 + 2*p34 + 2*p35) + 2*p13*(p14 - p45))) + p23*(2*p14*(p14 - p45) + p15*(Mtop2 + 2*p14 + 2*p45)) - 
     -                 2*(p15*(Mtop2 + p34)*(p14 + 2*p45) + p45*(-(p14*p35) + p13*p45)))))*Pi**2)/(p12**2*p15*p24*p25*(Mch2 + Mtop2 + 2*p34))))/
     -  (Sqrt(2d0)*(-1 + NC**2)**2) 

      if(rflav3.eq.-1037.and..not.dr_flag) then 
         amp2 = (GF*((-16*alphas**2*(A**2 + B**2)*CF*(-1 + NC**2)*(Mtop2*p14 - p12*(Mtop2 + p24))*Pi**2)/(p15*p24**2) - 
     -      (32*alphas**2*(A**2 + B**2)*CF*(-1 + NC**2)*(-((-Mch2 + Mtop2)*p14) - 2*p13*(Mtop2 + p34))*Pi**2)/(p15*(Mch2 + Mtop2 + 2*p34)**2) + 
     -      (32*alphas**2*(A**2 + B**2)*CF*(-1 + NC**2)*(-(Mtop2*p13) - 2*p12*(Mtop2 + p34) + p14*(Mtop2 + 2*p34))*Pi**2)/(p15*p24*(Mch2 + Mtop2 + 2*p34)) + 
     -      (32*alphas**2*(A**2 + B**2)*CF*(-1 + NC**2)*(-((Mch2 - Mtop2)*(Mtop2 + p24)*p25) - 2*(-(p23*(Mtop2 + p24)) + Mtop2*(2*Mtop2 - 2*p24 + p34))*p35 - 
     -           Mtop2*(-Mch2 + Mtop2)*p45)*Pi**2)/(p24**2*dentop2) + 
     -      (16*alphas**2*(A**2 + B**2)*CF*NC**2*(2*Mch2*p14*p15 - 2*Mtop2*p14*p15 + 7*(Mch2 - Mtop2)*p15*p24 - 4*p13*p15*(Mtop2 + p34) + 
     -           2*p23*(-7*p15 - 2*p25)*(Mtop2 + p34) + p25*(2*(Mch2 - Mtop2)*p24 + 7*(-((-Mch2 + Mtop2)*p14) - 2*p13*(Mtop2 + p34))) - 2*Mtop2*p12*p35 - 
     -           2*p12*p34*p35 + Mch2*p12*p45 - Mtop2*p12*p45)*Pi**2)/(p12**2*(Mch2 + Mtop2 + 2*p34)**2) - 
     -      (16*alphas**2*(A**2 + B**2)*CF*(-1 + NC**2)*(p15*(mt**4 - p14*(Mtop2 + p24)) + (-(p12*p14) + p14**2 + Mtop2*(Mtop2 - p12 + p24))*p25 - 
     -           (mt**4 - (Mtop2 + p12 - p14)*p14)*p45)*Pi**2)/(p14**2*dentop2x) - 
     -      (8*alphas**2*(A**2 + B**2)*CF*(-1 + NC**2)*(p15*((mh - mt)*(mh + mt)*Mtop2 + p14*(Mch2 + 3*Mtop2 + 2*p34)) + 
     -           2*(mt**4 - 2*Mtop2*p14 - p14**2 - p13*(Mtop2 + p14) + Mtop2*p34)*p35 - ((mh - mt)*(mh + mt)*Mtop2 - 2*(-Mtop2 - p13 - p14)*p14)*p45)*Pi**2)/
     -       (p14**2*p25**2) + (16*alphas**2*(A**2 + B**2)*CF*NC**2*
     -         (p15*(mt**4 + p23*(Mtop2 - 3*p14 - p24) + 2*Mtop2*(-p14 - p24) + (Mtop2 + p12 - p24)*p34) - 
     -           p25*(mt**4 + p13*(Mtop2 - 3*p14) + Mtop2*p34 - p12*(3*Mtop2 + p34) + p24*(3*Mtop2 - p13 + 2*p34) + p14*(5*Mtop2 + 3*p34)) - 
     -           (p12*(Mtop2 - 2*p24) + 2*(p14**2 + p14*p24 + p24**2))*p35 - 
     -           (-(Mtop2*(-p13 - p14)) - p12*(Mtop2 - p13 - p23) + (-Mtop2 - p13)*p24 - p23*(Mtop2 + 3*p14 + 2*p24))*p45)*Pi**2)/
     -       (p12*p14*(-dentopxtimesdentop)) + 
     -      (16*alphas**2*(A**2 + B**2)*CF*(-1 + NC**2)*(-2*p25*(mt**4 - 2*Mtop2*p14 - p14**2 - p13*(Mtop2 + p14) + Mtop2*p34) - 
     -           p15*(mt**4 - 3*Mtop2*p14 - 2*p14**2 - 2*p13*(Mtop2 + p14) + 2*Mtop2*p34) - Mtop2*(Mtop2 - p14)*p35 + 
     -           (mt**4 - 3*Mtop2*p14 - 2*p14**2 - 2*p13*(Mtop2 + p14) + 2*Mtop2*p34)*p45)*Pi**2)/(p14**2*(-dentopx)*p25) + 
     -      (16*alphas**2*(A**2 + B**2)*CF*(-1 + NC**2)*(-2*p25*(-(Mtop2*(-Mch2 + Mtop2 - p13 - 3*p14)) - 2*(Mtop2 - p13 - 2*p14)*p34 - 2*p34**2) + 
     -           p15*(2*mt**4 - Mch2*Mtop2 + 5*Mtop2*p34 + 4*p34**2 - 4*p13*(Mtop2 + p34) - 8*p14*(Mtop2 + p34)) + Mtop2*(-p14 + 2*(Mtop2 + p34))*p35 - 
     -           (Mtop2*(-Mch2 + Mtop2 - 3*p13 - 6*p14) + 4*(Mtop2 - p13 - 2*p14)*p34 + 4*p34**2)*p45)*Pi**2)/(p14*(-dentopx)*p25*(Mch2 + Mtop2 + 2*p34))
     -        + (16*alphas**2*(A**2 + B**2)*CF*(-1 + NC**2)*(2*p15*(Mtop2*(2*Mch2 - p13 + p14) + (Mch2 + Mtop2 + 2*p14)*p34) - 
     -           (-3*mt**4 + Mch2*Mtop2 - 6*Mtop2*p34 - 4*p34**2 + 4*p13*(Mtop2 + p34) + 8*p14*(Mtop2 + p34))*p35 - 
     -           (-mt**4 + 3*Mch2*Mtop2 + 2*Mch2*p34 + 4*p14*(Mtop2 + p34))*p45)*Pi**2)/(p14*p25**2*(Mch2 + Mtop2 + 2*p34)) + 
     -      (16*alphas**2*(A**2 + B**2)*CF*NC**2*(p15*(Mtop2*(Mtop2 - p13 - 2*p14) + (2*Mtop2 - 3*p14)*p23 + (Mtop2 - p14)*p34 - 
     -              p24*(2*Mtop2 - 3*p13 + 4*p34)) - p25*(mt**4 + 2*Mtop2*p13 + 5*Mtop2*p14 - (Mtop2 - p14)*p23 + Mtop2*p34 + 3*p14*p34 - 3*p12*(Mtop2 + p34) + 
     -              p24*(3*Mtop2 - p13 + 4*p34)) - (p12*(Mtop2 - p14) - p14*(-p14 + p24))*p35 - 
     -           (-(p13*(Mtop2 + p14)) + (Mtop2 - p14)*p23 + p14*(Mtop2 + 2*p34) - p24*(Mtop2 - 2*p13 + 2*p34) - p12*(Mtop2 - p13 + 2*p34))*p45)*Pi**2)/
     -       (p12*p14*(-dentopx)*(Mch2 + Mtop2 + 2*p34)) + 
     -      (32*alphas**2*(A**2 + B**2)*CF*NC**2*(-2*p25*(-((-Mch2 + Mtop2)*p14) - 2*p13*(Mtop2 + p34) + (Mtop2 + p34)*(Mch2 + Mtop2 + 2*p34)) - 
     -           p15*(-((Mch2 - Mtop2)*p14) + (-Mch2 + Mtop2)*p24 + 2*p13*(Mtop2 + p34) + 2*p23*(Mtop2 + p34) + (Mtop2 + p34)*(Mch2 + Mtop2 + 2*p34)) - 
     -           (Mch2*(2*p14 + p24) - Mtop2*(-2*p12 + 2*p13 + 4*p23 + 3*p24) - 2*(-p12 + p13 - p14 + 2*p23 + p24)*p34)*p35 - 
     -           (-((Mch2 - Mtop2)*p12) - (-Mch2 + Mtop2)*p14 + 2*(Mch2 - Mtop2)*p24 + p23*(Mch2 - 3*Mtop2 - 2*p34) - p13*(Mch2 + 3*Mtop2 + 4*p34))*p45)*Pi**2)/
     -       (p12*p25*(Mch2 + Mtop2 + 2*p34)**2) + (16*alphas**2*(A**2 + B**2)*CF*NC**2*
     -         (-(p15*(2*Mtop2*(3*Mch2 + Mtop2) + Mtop2*(2*p13 + 2*p14 + 5*p23 + 2*p24) + (7*Mtop2 + 2*p13 + 2*p23)*p34 + 2*p34**2 + 
     -                Mch2*(-2*p14 + p24 + 3*p34))) - p25*(4*mt**4 + 6*Mch2*Mtop2 + (Mch2 - 4*Mtop2)*p14 - p13*(7*Mtop2 + 4*p34) + 
     -              p34*(3*Mch2 + 11*Mtop2 + 4*p34)) + ((-3*Mch2 + 7*Mtop2 + 4*p13)*p24 + 2*p13*(3*Mtop2 - p14 + 2*p34) - p12*(5*Mtop2 + 4*p34) + 
     -              2*p23*(6*Mtop2 - p14 + 4*p34) - p14*(3*Mch2 + Mtop2 + 6*p34))*p35 - 
     -           (-((Mch2 - 2*Mtop2)*p12) - 2*p13**2 - 2*Mtop2*p14 - 4*Mtop2*p24 - p13*(-Mch2 + 5*Mtop2 + 2*p34) - p23*(Mch2 + 7*Mtop2 - 2*p13 + 4*p34))*p45)*
     -         Pi**2)/(p12*p25*(Mch2 + Mtop2 + 2*p34)*dentop) - 
     -      (8*alphas**2*(A**2 + B**2)*CF*NC**2*(-2*p25*((mh - mt)*(mh + mt)*Mtop2 - 2*p34*(Mtop2 + p34) + p13*(Mtop2 + 2*p34) + p14*(Mch2 + 4*Mtop2 + 6*p34)) + 
     -           p15*(mt**4 - Mch2*Mtop2 + 2*Mch2*p24 + 2*(-(Mtop2*(-4*(-p13 - p14) + p23 + p24)) + (Mtop2 - 3*p13 - 3*p14 - p23)*p34 + p34**2)) - 
     -           2*(p12*(Mtop2 + p34) - p14*(Mtop2 - p13 - 3*p14 + 2*p23 + p34) - p24*(-p13 + 2*(Mtop2 + p34)))*p35 - 
     -           2*(-((Mch2 - Mtop2)*p12) - p13**2 - (-Mch2 + Mtop2)*p14 + 2*(Mch2 - Mtop2)*p24 - p13*(Mtop2 + 3*p14 + p34) - p23*(-p13 + 2*(Mtop2 + p34)))*p45)*
     -         Pi**2)/(p12*p14*p25*(Mch2 + Mtop2 + 2*p34)) + (16*alphas**2*(A**2 + B**2)*CF*NC**2*
     -         (-(p25*((mh - mt)*(mh + mt)*Mtop2 - Mch2*(-p14 + 2*p24) + Mtop2*(-p13 + 2*p14 + 2*p23 + 2*p24) + 2*(-2*Mtop2 - 2*p13 + p23)*p34 - 4*p34**2)) + 
     -           p15*((-Mch2 + 4*Mtop2)*p24 + p23*(Mtop2 - 2*p34) + 2*(mt**4 - Mch2*Mtop2 + p34*(Mtop2 + p34))) - 
     -           2*(Mtop2*(-p13 - 4*p14) + 2*(-Mtop2 - p13)*p24 + p23*(-2*Mtop2 + p14 + p24) - 3*p14*p34)*p35 - 
     -           2*(-2*Mtop2*p14 - p23**2 - Mtop2*p24 - p13*(Mtop2 - p34) + p23*(Mtop2 + p13 + 2*p34))*p45 - p12*((5*Mtop2 + 4*p34)*p35 - (Mch2 - 2*Mtop2)*p45))*
     -         Pi**2)/(p12*p24*(Mch2 + Mtop2 + 2*p34)*dentop) - 
     -      (16*alphas**2*(A**2 + B**2)*CF*NC**2*(-2*(Mch2 - Mtop2)*p14*p15 - (Mch2 - Mtop2)*(7*p14 + 2*p24)*p25 + 2*p14*(2*p13 + 7*p23)*p35 + 
     -           p24*(7*(-Mch2 + Mtop2)*p15 - 2*(-7*p13 - 2*p23)*p35) - p12*(-2*(9*Mtop2 + p34)*p35 - (-Mch2 + Mtop2)*p45))*Pi**2)/
     -       (p12**2*dentop2) - (8*alphas**2*(A**2 + B**2)*CF*NC**2*
     -         (p15*(mt**4 - Mch2*Mtop2 + Mtop2*(-6*p13 - 8*p14 - 3*p23 - 2*p24) + Mch2*p24 - 2*(Mtop2 + p13 + p23)*p34 - 2*p34**2) - 
     -           p25*(2*(mh - mt)*(mh + mt)*Mtop2 - 2*p34*(Mtop2 + p34) + p13*(3*Mtop2 + 2*p34) + p14*(Mch2 + 8*Mtop2 + 6*p34)) + 
     -           2*(-(p13*(Mtop2 + 3*p14)) + (Mtop2 + 2*p14)*p23 + p14*(2*Mtop2 - 6*p14 + 3*p34) + p24*(4*Mtop2 - 2*p13 - 3*p14 + 3*p34))*p35 - 
     -           2*(-p13**2 - Mtop2*p14 - 2*Mtop2*p24 - p13*(2*Mtop2 + p34) + p23*(-Mtop2 - 2*p13 - 3*p14 + p34))*p45 + 
     -           p12*(-((3*Mtop2 + 2*p34)*p35) - (Mch2 + 2*Mtop2)*p45))*Pi**2)/(p12*p14*p25*dentop) + 
     -      (32*alphas**2*(A**2 + B**2)*CF*NC**2*(2*Mch2*p14*p15 - 2*Mtop2*p14*p15 + 
     -           p25*(2*(Mch2 - Mtop2)*p24 + 7*(-((-Mch2 + Mtop2)*p14) - p13*(Mtop2 + p34))) - 10*Mtop2*p12*p35 - 2*p12*p34*p35 + 
     -           7*p24*((Mch2 - Mtop2)*p15 - p13*p35) - 2*p13*(p15*(Mtop2 + p34) + p14*p35) - 7*Mch2*p12*p45 - Mtop2*p12*p45 + 2*p13**2*p45 + 2*p23**2*p45 + 
     -           p23*((-7*p15 - 2*p25)*(Mtop2 + p34) - (7*p14 + 2*p24)*p35 + 14*p13*p45))*Pi**2)/(p12**2*(Mch2 + Mtop2 + 2*p34)*dentop) + 
     -      (16*alphas**2*(A**2 + B**2)*CF*NC**2*(-((Mch2 - Mtop2)*(Mtop2 - 2*p14 - 2*p24)*p25) + 
     -           2*((-Mch2 + Mtop2)*p15*(Mtop2 + 2*p24) - (-2*Mtop2*p13 - 6*Mtop2*p14 - p23*(Mtop2 - 2*p14 - 2*p24) + p24*(-3*Mtop2 - 4*p13 - 2*p34) - 
     -                 4*p14*p34 + p12*(3*Mtop2 + 2*p34))*p35 - (Mch2 - Mtop2)*(-p12 + 2*p14 + p24)*p45))*Pi**2)/(p12*p24*dentop2) - 
     -      (32*alphas**2*(A**2 + B**2)*CF*(-1 + NC**2)*(Mtop2*p15*(Mtop2 + p23 + p24 + p34) - p25*(Mtop2*(-p13 - p14 - 4*p35) - 4*p34*p35) + 
     -           Mtop2*p13*(-2*p35 - p45) + p12*(-((5*Mtop2 + 4*p34)*p35) - Mtop2*p45) - p35*(-(p14*(3*Mtop2 + 4*p34)) - 2*Mtop2*p35 + 2*(Mtop2 + 2*p34)*p45))*
     -         Pi**2)/(p15*p24*(Mch2 + Mtop2 + 2*p34)*dentop) - 
     -      (16*alphas**2*(A**2 + B**2)*CF*(-1 + NC**2)*(2*p15*(-((-Mch2 + Mtop2)*p14) - 2*p13*(Mtop2 + p34) + (Mtop2 + p34)*(Mch2 + Mtop2 + 2*p34)) + 
     -           (Mch2 + Mtop2 + 2*p34)*(-2*p14*p35 - (Mch2 + Mtop2 - 2*p13 + 2*p34)*p45))*Pi**2)/(p25**2*(Mch2 + Mtop2 + 2*p34)**2) + 
     -      (32*alphas**2*(A**2 + B**2)*CF*(-1 + NC**2)*(Mtop2*p15*(Mtop2 - p24) - 2*p35*(p12*(Mtop2 + p24) - (Mtop2 + p24)*p25 + Mtop2*(-p14 + p45)))*Pi**2)/
     -       (p15*p24**2*dentop) - (16*alphas**2*(A**2 + B**2)*CF*NC**2*
     -         (-2*Mch2*p14*p15 + 2*Mtop2*p14*p15 + 5*Mtop2*p12*p35 - 2*Mtop2*p15*p35 + 4*p12*p34*p35 - 4*p14*p35**2 + 
     -           p24*((Mch2 - 4*Mtop2)*p15 - 2*p35*(-p13 + p35)) + 2*p13*(p15*(Mtop2 + p34) - p35*(-p14 - 2*p45)) - Mch2*p12*p45 + 2*Mtop2*p12*p45 - 
     -           2*p13**2*p45 - 2*Mtop2*p15*p45 + p23*(-(p15*(7*Mtop2 + 4*p34)) - 4*p14*p35 - 2*(-p13 - p35)*p45) - 
     -           p25*(-((Mch2 + 2*Mtop2)*p14) - p13*(5*Mtop2 + 2*p34) - 2*(-((5*Mtop2 + 3*p34)*p35) - 2*Mtop2*p45)))*Pi**2)/
     -       (p12*p15*(Mch2 + Mtop2 + 2*p34)*dentop) - 
     -      (32*alphas**2*(A**2 + B**2)*CF*NC**2*(2*Mtop2*p13*p15 - Mch2*p14*p15 + Mtop2*p14*p15 + 2*Mch2*p15*p24 - 2*Mtop2*p15*p24 + 2*p13*p15*p34 - 
     -           4*p15*p23*(Mtop2 + p34) + 2*Mtop2*p12*p35 - 2*Mtop2*p15*p35 + 2*p12*p34*p35 - 2*p15*p34*p35 + (Mch2 - Mtop2)*(-p12 + p15)*p45 - 
     -           p25*(-((-Mch2 + Mtop2)*p14) - 2*p13*(Mtop2 + p34) - 2*(-2*(Mtop2 + p34)*p35 - (-Mch2 + Mtop2)*p45)))*Pi**2)/(p12*p15*(Mch2 + Mtop2 + 2*p34)**2) 
     -       + (8*alphas**2*(A**2 + B**2)*CF*NC**2*(-2*p25**2*(3*Mtop2 + p34) - 
     -           p12*(-2*p15*(3*Mtop2 + p34) - 2*p25*(3*Mtop2 + p34) - p35*(3*Mtop2 + 6*p14 + 6*p24 - 4*p45) + (Mtop2 + 2*p13 + 2*p23)*p45) - 
     -           p23*(Mtop2*p15 - 2*(-p14 + p15)*p45 - 2*p45**2) - p24*(p15*(Mtop2 - 2*p35) - 6*p35*(-p14 + p45)) - 
     -           p25*(p15*(6*Mtop2 + 4*p34) - p14*(-Mtop2 + 2*p34 - 2*p35) + 2*(2*Mtop2 + 3*p24)*p35 - 2*(Mtop2 + p23 - p34)*p45 - p13*(Mtop2 + 2*p45)) + 
     -           2*((-p14 + p45)*(3*p14*p35 - p13*p45) + p15*(-(p14*(2*Mtop2 + p34)) - Mtop2*p35 + (2*Mtop2 + p34)*p45)))*Pi**2)/
     -       (p12*p15*p24*dentop) + (8*alphas**2*(A**2 + B**2)*CF*NC**2*
     -         (-6*p25**2*(Mtop2 + p34) - p25*(6*p15*(Mtop2 + p34) + p14*(Mtop2 + 2*p34 - 2*p35) + 2*(2*Mtop2 + p24)*p35 - p13*(Mtop2 - 2*p45) - 
     -              2*(Mtop2 + p23 + 3*p34)*p45) - p12*(-6*p15*(Mtop2 + p34) - 6*p25*(Mtop2 + p34) - (3*Mtop2 + 2*p14 + 2*p24)*p35 + 
     -              (Mtop2 + 2*p13 + 2*p23 + 4*p34)*p45) + p24*(-(p15*(Mtop2 - 2*p34)) - 4*p14*p35 - 2*(-p13 - p35)*p45) + 
     -           p23*(-3*Mtop2*p15 - 2*p45*(-p14 + p45)) + 2*(p14*p35*(-p14 - p45) + p15*(-(p14*(2*Mtop2 + 3*p34)) - Mtop2*p35 + (2*Mtop2 + 3*p34)*p45) + 
     -              p13*(Mtop2*p15 - (-p14 - p45)*p45)))*Pi**2)/(p12*p15*p24*(Mch2 + Mtop2 + 2*p34)) - 
     -      16*alphas**2*(A**2 + B**2)*CF*((-4*(p13 + p14 + p15 - p35 - p45)*(2*(Mtop2 + p34)*p35 - (Mch2 - Mtop2)*p45))/(p15*p25*(Mch2 + Mtop2 + 2*p34)**2) + 
     -         ((2*mt**4 - (p14 + p24)*(Mtop2 - 2*p12 + 2*p14 + 2*p24))*(-p15 - p25) + (Mtop2*(2*Mtop2 - p14 - p24) - 2*p12*(-p12 + p14 + p24))*p45)/
     -          (p14*p24*dentop2x) - (2*(-(p25*(Mtop2*(-p13 + p34) + Mch2*(2*Mtop2 - p14 + p34))) + 
     -              p15*(Mch2*p24 - p23*(Mtop2 + 2*p34) + (Mtop2 + p34)*(Mtop2 + 2*p34)) - 
     -              (2*Mtop2*p23 - (Mch2 - 3*Mtop2 - 2*p13)*p24 - 2*Mtop2*(Mtop2 + p34) - p12*(3*Mtop2 + 2*p34) + p14*(3*Mtop2 + 2*p34))*p35 + 
     -              (-((Mch2 - 2*Mtop2)*p12) - (Mch2 + Mtop2 - 2*p13)*p23 + Mtop2*(3*Mch2 + Mtop2 - 3*p13 - 2*p14 - 2*p24) + 2*(Mch2 + Mtop2 - p13)*p34)*p45))/
     -          (p24*p25*(Mch2 + Mtop2 + 2*p34)*dentop) - 
     -         (p15*((mh - mt)*(mh + mt)*Mtop2 - Mch2*p24 + 2*p23*(Mtop2 + p34) - 2*p34*(2*Mtop2 + p34)) - 
     -            p25*(mt**4 - p14*(Mch2 + 2*Mtop2 - 2*p34) - p34*(Mtop2 - 2*p13 + 2*p34)) - 
     -            (2*p12*(Mtop2 + p34) - p24*(3*Mtop2 - 2*p14 + 2*p34) + 2*(mt**4 + Mtop2*p13 - p14*(Mtop2 + p34)))*p35 - 
     -            ((mh - mt)*(mh + mt)*Mtop2 - (Mch2 - 2*Mtop2)*p12 + p23*(Mtop2 - 2*p14 + 2*p34) - 2*(-((Mch2 + Mtop2)*p14) + Mtop2*p24 + (Mtop2 + p13)*p34))*
     -             p45)/(p14*p24*p25*dentop) - (p15**2*p24 - p14*p25**2 - (p12**2 - p12*(Mtop2 + p24) + 2*Mtop2*(p14 - p45))*p45 + 
     -            p15*(mt**4 + (Mtop2 + 2*p12 - 2*p14)*p14 - (Mtop2 - p12 + 2*p14)*p24 - p24**2 + (Mtop2 - p14 + p24)*p25 - (p12 + 2*(Mtop2 - p14))*p45) - 
     -            p25*(-(p14*(Mtop2 + p12 + p24)) + (-p12 + 2*(Mtop2 + p24))*p45))/(p14*p15*p24*(-dentopx)) + 
     -         (2*p15**2*(Mtop2 + p34) - 2*p14**2*p35 + p15*(mt**4 - 2*p14*p23 + Mtop2*p34 + 2*p14*p34 + 2*p25*(Mtop2 + p34) - 2*p24*(Mtop2 - p13 + p34) + 
     -               2*(Mtop2 - p14)*p35 - 2*p13*(Mtop2 - p45) - 4*Mtop2*p45 - 6*p34*p45) - 
     -            p45*(-2*p12*(Mtop2 - p13 + p34) + Mtop2*(-p13 + 2*p35) - 2*(Mtop2 - p13 + 2*p34)*p45) - 
     -            2*p25*(-(p14*(Mtop2 + p34 - p35)) + (-p13 + 2*(Mtop2 + p34))*p45) - p14*(-((Mtop2 + 2*p12)*p35) + 2*(Mtop2 - p13 + 2*p34 - p35)*p45))/
     -          (p14*p15*(-dentopx)*(Mch2 + Mtop2 + 2*p34)) - 
     -         (p15*(mt**4 - 2*p14*p23 + 2*p25*(Mtop2 + p34) - 2*p24*(Mtop2 - p13 - p14 + p34) + p34*(Mtop2 + 2*p14 - 2*p45)) + 2*p14**2*(-p35 + 2*p45) - 
     -            p45*(-2*p12*(Mtop2 - p13 + p34) + Mtop2*(-p13 + 2*p35) - 2*(Mtop2 - p13 + 2*p34)*p45) - 
     -            2*p25*(-(p14*(Mtop2 - p14 + p34 - p35)) + (-p13 + 2*(Mtop2 - p14 + p34))*p45) - 
     -            p14*(-((Mtop2 + 2*p12)*p35) + 2*(Mtop2 + p12 - p13 + 2*p34 - p35)*p45 + 4*p45**2))/(2.*p14*p15*p24*p25) + 
     -         (p15*((mh - mt)*(mh + mt)*Mtop2 - 2*(Mtop2 + p34)*(-p13 - p14 + p34 - p35) - 2*Mch2*p45 + 2*Mtop2*p45) + 
     -            2*(p14**2*p35 - p45*(p13**2 + 2*(Mtop2 + p34)*p35 - p13*(Mtop2 + p34 + p35) - (Mch2 - Mtop2)*p45) - 
     -               p14*(-((Mtop2 + p13 + p34 - p35)*p35) - (-Mch2 + Mtop2 - p13)*p45)))/(p14*p15*p25*(Mch2 + Mtop2 + 2*p34)) + 
     -         (p15*(mt**4 - Mch2*Mtop2 + (Mch2 - Mtop2)*p24 - 2*p23*(Mtop2 + p34) + 2*(Mtop2*p35 + p34*(Mtop2 + p34 - p45))) - 
     -            p12*(-2*(Mtop2 + p34)*p35 - (-Mch2 + Mtop2)*p45) - 
     -            p25*(-2*p15*(Mtop2 + p34) - p14*(-Mch2 + Mtop2 + 2*p35) - 2*p13*(Mtop2 + p34 - p45) - 2*(-2*(Mtop2 + p34)*p35 - (-Mch2 + Mtop2)*p45)) + 
     -            2*(-(Mtop2*p35**2) - Mch2*p45*(-p14 + p45) - p35*(-(p14*(-p34 - p45)) - 2*p34*p45) - p13*(-(Mtop2*p35) + (p34 - p45)*p45)))/
     -          (p15*p24*p25*(Mch2 + Mtop2 + 2*p34)))*Pi**2 + 2*((16*alphas**2*(A**2 + B**2)*CF*NC**2*p15*(-((-Mch2 + Mtop2)*p14) - 2*p13*(Mtop2 + p34))*Pi**2)/
     -          (p12**2*(Mch2 + Mtop2 + 2*p34)**2) + (16*alphas**2*(A**2 + B**2)*CF*NC**2*p14*((Mch2 - Mtop2)*p15 - 2*p13*p35)*Pi**2)/
     -          (p12**2*dentop2) + (32*alphas**2*(A**2 + B**2)*CF*NC**2*
     -            (-((-Mch2 + Mtop2)*p14*p15) - p13*(p15*(Mtop2 + p34) + p14*p35) + p13**2*p45)*Pi**2)/(p12**2*(Mch2 + Mtop2 + 2*p34)*dentop))))/
     -  (Sqrt(2d0)*(-1 + NC**2)**2)
         amp2=amp2-amp2dr
         if(ds_flag) then
            call madgraphordergg(pphy,ppp)
            call madgraphcouplings
            call  SMATRIXGG(ppp,subtr)
            if(nearresonance(pphy)) write(*,*) 'subtr/amp2',subtr/amp2
            amp2=amp2-subtr
         endif
      else
         amp2=amp2dr
      endif
      
      end


      subroutine madgraphordergg(pphy,ppp)
      implicit none
      include 'nlegborn.h'
      real * 8 pphy(0:3,nlegreal),ppp(0:3,nlegreal)
      integer mu
      do mu=0,3
         ppp(mu,1)=pphy(mu,1)
         ppp(mu,2)=pphy(mu,2)
         ppp(mu,3)=pphy(mu,5)
         ppp(mu,4)=pphy(mu,3)
         ppp(mu,5)=pphy(mu,4)
      enddo
      end
      
      subroutine madgraphorderqq(pphy,ppp)
      implicit none
      include 'nlegborn.h'
      real * 8 pphy(0:3,nlegreal),ppp(0:3,nlegreal)
      integer mu
      do mu=0,3
         ppp(mu,1)=pphy(mu,1)
         ppp(mu,2)=pphy(mu,2)
         ppp(mu,3)=pphy(mu,3)
         ppp(mu,4)=pphy(mu,5)
         ppp(mu,5)=pphy(mu,4)
      enddo
      end
      
c-------------------------------------------------------------------------------c      
c------  subroutine for b(p1) q/qbar(p2)  -> H^{-}(p3) t(p4) q/qbar(p5) --------c
c-------------------------------------------------------------------------------c 
           
      subroutine bqorqbar(pphy,amp2,rflav3)
      implicit none
      real * 8 pphy(0:3,5)          
      real * 8 amp2
      integer rflav3
      real * 8 p1(0:3),p2(0:3),p3(0:3),p4(0:3),p5(0:3)
c      include 'couplings.h'
      real * 8 p12,p13,p14,p15,p23,p24,p25,p34,p35,p45
      real * 8 Mtop, Mch,mt,mh, Mtop2, Mch2, A, B, alphas, GF

      real * 8 dotp
      complex * 16 ccdotp
      integer mu,i
 
      include 'PhysPars.h'
      include '../include/pwhg_math.h'
      include '../include/pwhg_st.h'

      if(rflav3.ne.-37) then
         write(*,*) ' error in bg, rflav3=',rflav3
         call exit(-1)
      endif

      Mch = ph_mCH
      Mtop = ph_mT
      mt=mtop
      mh=mch
      Mch2 = Mch**2
      Mtop2 = Mtop**2

      GF=ph_GF
      A=ph_A
      B=ph_B
      alphas=st_alpha
      
      do mu=0,3
         p1(mu) = pphy(mu,1)
         p2(mu) = pphy(mu,2)
         p3(mu) = pphy(mu,3)
         p4(mu) = pphy(mu,4)
         p5(mu) = pphy(mu,5)
      enddo

c    define all the possible scalar products, even if they are not independent      
      p12=dotp(p1,p2)
      p13=dotp(p1,p3)
      p14=dotp(p1,p4)
      p15=dotp(p1,p5)
      p23=dotp(p2,p3)
      p24=dotp(p2,p4)
      p25=dotp(p2,p5)
      p34=dotp(p3,p4)
      p35=dotp(p3,p5)
      p45=dotp(p4,p5)
      
      
      
      amp2 =         (16*Sqrt(2d0)*alphas**2*(A**2 + B**2)*CF*GF*((p15*((-Mch2 + Mtop2)*p24 + 2*p23*(Mtop2 + p34)) + p12*(2*(Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45))/
     -       (Mch2 + Mtop2 + 2*p34)**2 + (2*((Mtop2*p13 + Mch2*p14)*p25 + p24*((-Mch2 + Mtop2)*p15 + p13*p35) + p12*((Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45) + 
     -           p23*(p15*(Mtop2 + p34) - 2*p14*p35 + p13*p45)))/((Mch2 - Mtop2 - 2*p13)*(Mch2 + Mtop2 + 2*p34)) + 
     -      ((-Mch2 + Mtop2)*(p15*p24 + p12*p45) + 2*p13*(Mtop2*p25 + p24*p35 + p23*p45))/(-Mch2 + Mtop2 + 2*p13)**2)*Pi**2)/(NC*p25**2)
     

      end
c-------------------------------------------------------------------------------c      
c------  subroutine for b(p1) b(p2)  -> H^{-}(p3) t(p4) b(p5) --------c
c-------------------------------------------------------------------------------c 
           
      subroutine bb(pphy,amp2,rflav3)
      implicit none
      real * 8 pphy(0:3,5)          
      real * 8 amp2
      integer rflav3
      real * 8 p1(0:3),p2(0:3),p3(0:3),p4(0:3),p5(0:3)
c      include 'couplings.h'
      real * 8 p12,p13,p14,p15,p23,p24,p25,p34,p35,p45
      real * 8 Mtop, Mch,mt,mh, Mtop2, Mch2, A, B, alphas, GF

      real * 8 dotp
      complex * 16 ccdotp
      integer mu,i
 
      include 'PhysPars.h'
      include '../include/pwhg_math.h'
      include '../include/pwhg_st.h'

      if(rflav3.ne.-37) then
         write(*,*) ' error in bg, rflav3=',rflav3
         call exit(-1)
      endif

      Mch = ph_mCH
      Mtop = ph_mT
      mt=mtop
      mh=mch
      Mch2 = Mch**2
      Mtop2 = Mtop**2

      GF=ph_GF
      A=ph_A
      B=ph_B
      alphas=st_alpha
      
      do mu=0,3
         p1(mu) = pphy(mu,1)
         p2(mu) = pphy(mu,2)
         p3(mu) = pphy(mu,3)
         p4(mu) = pphy(mu,4)
         p5(mu) = pphy(mu,5)
      enddo

c    define all the possible scalar products, even if they are not independent      
      p12=dotp(p1,p2)
      p13=dotp(p1,p3)
      p14=dotp(p1,p4)
      p15=dotp(p1,p5)
      p23=dotp(p2,p3)
      p24=dotp(p2,p4)
      p25=dotp(p2,p5)
      p34=dotp(p3,p4)
      p35=dotp(p3,p5)
      p45=dotp(p4,p5)
      
      
      
      amp2 =   (GF*((32*alphas**2*(A**2 + B**2)*CF*NC*((p25*((-Mch2 + Mtop2)*p14 + 2*p13*(Mtop2 + p34)) + p12*(2*(Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45))/
     -            (Mch2 + Mtop2 + 2*p34)**2 + ((-Mch2 + Mtop2)*(p14*p25 + p12*p45) + 2*p23*(Mtop2*p15 + p14*p35 + p13*p45))/(-Mch2 + Mtop2 + 2*p23)**2 + 
     -           (2*(p15*(Mtop2*p23 + Mch2*p24) + p14*((-Mch2 + Mtop2)*p25 + p23*p35) + p12*((Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45) + 
     -                p13*(p25*(Mtop2 + p34) - 2*p24*p35 + p23*p45)))/((Mch2 - Mtop2 - 2*p23)*(Mch2 + Mtop2 + 2*p34)))*Pi**2)/p15**2 + 
     -      (32*alphas**2*(A**2 + B**2)*CF*NC*((p15*((-Mch2 + Mtop2)*p24 + 2*p23*(Mtop2 + p34)) + p12*(2*(Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45))/
     -            (Mch2 + Mtop2 + 2*p34)**2 + (2*((Mtop2*p13 + Mch2*p14)*p25 + p24*((-Mch2 + Mtop2)*p15 + p13*p35) + 
     -                p12*((Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45) + p23*(p15*(Mtop2 + p34) - 2*p14*p35 + p13*p45)))/
     -            ((Mch2 - Mtop2 - 2*p13)*(Mch2 + Mtop2 + 2*p34)) + 
     -           ((-Mch2 + Mtop2)*(p15*p24 + p12*p45) + 2*p13*(Mtop2*p25 + p24*p35 + p23*p45))/(-Mch2 + Mtop2 + 2*p13)**2)*Pi**2)/p25**2))/(Sqrt(2d0)*NC**2)
     

      end
c-------------------------------------------------------------------------------c      
c--------  subroutine for q(p1) qbar(p2)  -> H^{-}(p3) t(p4) bbar(p5)  ---------c
c-------------------------------------------------------------------------------c 
           
      subroutine qqbar(pphy,amp2,rflav3)
      implicit none
      real * 8 pphy(0:3,5)          
      real * 8 amp2
      integer rflav3
      real * 8 p1(0:3),p2(0:3),p3(0:3),p4(0:3),p5(0:3)
      real * 8 p12,p13,p14,p15,p23,p24,p25,p34,p35,p45,ss,dentop,dentop2
      real * 8 Mtop, Mch,mt,mh, Mtop2, Mch2, A, B, alphas, GF
      real * 8 amp2dr,subtr
      real * 8 ppp(0:3,5)
      real * 8 dotp
      complex * 16 ccdotp
      integer mu,i
 
      include 'PhysPars.h'
      include '../include/pwhg_math.h'
      include '../include/pwhg_st.h'
      include 'coupl.inc'      
      include 'doublyresonant.h'  

      logical nearresonance
    
      Mch = ph_mCH
      Mtop = ph_mT
      Mch2 = Mch**2
      Mtop2 = Mtop**2
      mt=mtop
      mh=mch
      
      A = ph_A
      B = ph_b
      GF = ph_GF
      alphas = st_alpha
      
    
      
      do mu=0,3
         p1(mu) = pphy(mu,1)
         p2(mu) = pphy(mu,2)
         p3(mu) = pphy(mu,3)
         p4(mu) = pphy(mu,4)
         p5(mu) = pphy(mu,5)
      enddo

c    define all the possible scalar products, even if they are not independent      
      p12=dotp(p1,p2)
      p13=dotp(p1,p3)
      p14=dotp(p1,p4)
      p15=dotp(p1,p5)
      p23=dotp(p2,p3)
      p24=dotp(p2,p4)
      p25=dotp(p2,p5)
      p34=dotp(p3,p4)
      p35=dotp(p3,p5)
      p45=dotp(p4,p5)
      
      
      ghmq=sqrt(gf*sqrt(2d0)*(a**2+b**2))
      ss=2*p35+Mch2
      dentop2=(ss-Mtop2)**2+ph_Twidth**2*Mtop2
      dentop=dentop2/(ss-Mtop2)

CCCCcccc changed for testing, shoud be lt, if not, I fogot to change it back      
      amp2dr =      (16*Sqrt(2d0)*alphas**2*(A**2 + B**2)*CF*GF*
     -    ((-Mch2 + Mtop2)*p14*p25 + 2*p13*p25*(Mtop2 + p34) + p15*((-Mch2 + Mtop2)*p24 + 2*p23*(Mtop2 + p34)))*
     -    Pi**2)/(NC*p12**2*(Mch2 + Mtop2 + 2*p34)**2)   

      if(rflav3.eq.-1037.and..not.dr_flag) then
      
         amp2 =  (16*sqrt(2d0)*alphas**2*(A**2 + B**2)*CF*GF*
     -    (((-Mch2 + Mtop2)*p14*p25 + 2*p13*p25*(Mtop2 + p34) + 
     -         p15*((-Mch2 + Mtop2)*p24 + 2*p23*(Mtop2 + p34)))/(Mch2 + Mtop2 + 2*p34)**2
     -        + (2*Mtop2*p12*p35 + p24*((-Mch2 + Mtop2)*p15 + 2*p13*p35) + 
     -         p14*((-Mch2 + Mtop2)*p25 + 2*p23*p35))/dentop2 + 
     -      (2*(p15*((-Mch2 + Mtop2)*p24 + p23*(Mtop2 + p34)) + 
     -           p14*((-Mch2 + Mtop2)*p25 + p23*p35) + p12*(Mtop2*p35 + Mch2*p45) + 
     -           p13*(p25*(Mtop2 + p34) + p24*p35 - 2*p23*p45)))/
     -       ((Mch2 + Mtop2 + 2*p34)*dentop))*Pi**2)/(NC*p12**2)
         amp2=amp2-amp2dr
         if(ds_flag) then
            call madgraphorderqq(pphy,ppp)
            call madgraphcouplings
            call  SMATRIXQQ(ppp,subtr)
            if(nearresonance(pphy)) write(*,*) 'subtr/amp2',subtr/amp2
            amp2=amp2-subtr
         endif         
      else
         amp2=amp2dr
      endif

      end

c-------------------------------------------------------------------------------c      
c--------  subroutine for b(p1) bbar(p2)  -> H^{-}(p3) t(p4) bbar(p5)  ---------c
c-------------------------------------------------------------------------------c 
           
      subroutine bbbar(pphy,amp2,rflav3)
      implicit none
      real * 8 pphy(0:3,5)          
      real * 8 amp2
      integer rflav3
      real * 8 p1(0:3),p2(0:3),p3(0:3),p4(0:3),p5(0:3)
c      include 'couplings.h'
      real * 8 ppp(0:3,5)
      real * 8 p12,p13,p14,p15,p23,p24,p25,p34,p35,p45,ss,dentop,dentop2
      real * 8 Mtop, Mch,mt,mh, Mtop2, Mch2, A, B, alphas, GF
      real * 8 amp2dr
      real * 8 dotp
      complex * 16 ccdotp
      integer mu,i
 
      include 'PhysPars.h'
      include '../include/pwhg_math.h'
      include '../include/pwhg_st.h'
      include 'coupl.inc'      
      include 'doublyresonant.h'   
      real * 8 subtr
      logical nearresonance
      Mch = ph_mCH
      Mtop = ph_mT
      Mch2 = Mch**2
      Mtop2 = Mtop**2
      mt=mtop
      mh=mch
      
      GF=ph_GF
      A=ph_A
      B=ph_B
      alphas=st_alpha
      
      do mu=0,3
         p1(mu) = pphy(mu,1)
         p2(mu) = pphy(mu,2)
         p3(mu) = pphy(mu,3)
         p4(mu) = pphy(mu,4)
         p5(mu) = pphy(mu,5)
      enddo

c    define all the possible scalar products, even if they are not independent      
      p12=dotp(p1,p2)
      p13=dotp(p1,p3)
      p14=dotp(p1,p4)
      p15=dotp(p1,p5)
      p23=dotp(p2,p3)
      p24=dotp(p2,p4)
      p25=dotp(p2,p5)
      p34=dotp(p3,p4)
      p35=dotp(p3,p5)
      p45=dotp(p4,p5)
      
      
      ghmq=sqrt(gf*sqrt(2d0)*(a**2+b**2))
      ss=2*p35+Mch2
      dentop2=(ss-Mtop2)**2+ph_Twidth**2*Mtop2
      dentop=dentop2/(ss-Mtop2)

CCCC should be lt      
      
      amp2dr =         (GF*((32*alphas**2*(A**2 + B**2)*CF*NC*((-Mch2 + Mtop2)*p14*p25 + 2*p13*p25*(Mtop2 + p34) + p15*((-Mch2 + Mtop2)*p24 + 2*p23*(Mtop2 + p34)))*Pi**2)/
     -       (p12**2*(Mch2 + Mtop2 + 2*p34)**2) + (32*alphas**2*(A**2 + B**2)*CF*NC*
     -         ((p15*((-Mch2 + Mtop2)*p24 + 2*p23*(Mtop2 + p34)) + p12*(2*(Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45))/(Mch2 + Mtop2 + 2*p34)**2 + 
     -           (2*((Mtop2*p13 + Mch2*p14)*p25 + p24*((-Mch2 + Mtop2)*p15 + p13*p35) + p12*((Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45) + 
     -                p23*(p15*(Mtop2 + p34) - 2*p14*p35 + p13*p45)))/((Mch2 - Mtop2 - 2*p13)*(Mch2 + Mtop2 + 2*p34)) + 
     -           ((-Mch2 + Mtop2)*(p15*p24 + p12*p45) + 2*p13*(Mtop2*p25 + p24*p35 + p23*p45))/(-Mch2 + Mtop2 + 2*p13)**2)*Pi**2)/p25**2 + 
     -      CF*((64*alphas**2*(A**2 + B**2)*p15*((-Mch2 + Mtop2)*p24 + 2*p23*(Mtop2 + p34))*Pi**2)/(p12*p25*(Mch2 + Mtop2 + 2*p34)**2) - 
     -         (32*alphas**2*(A**2 + B**2)*((Mch2 - 2*Mtop2)*p15*p24 - (Mtop2*p13 + Mch2*p14)*p25 + p12*(Mtop2*p35 + Mch2*p45) - 
     -              p23*(p15*(3*Mtop2 + 2*p34) - 2*p14*p35 + 2*p13*p45))*Pi**2)/(p12*(Mch2 - Mtop2 - 2*p13)*p25*(Mch2 +Mtop2 + 2*p34)))))/(Sqrt(2d0)*NC**2)
      
      
      if(rflav3.eq.-1037) then
      
         amp2 = (GF*((32*alphas**2*(A**2 + B**2)*CF*NC*((-Mch2 + Mtop2)*p14*p25 + 2*p13*p25*(Mtop2 + p34) + p15*((-Mch2 + Mtop2)*p24 + 2*p23*(Mtop2 + p34)))*Pi**2)/
     -       (p12**2*(Mch2 + Mtop2 + 2*p34)**2) + (32*alphas**2*(A**2 + B**2)*CF*NC*
     -         (2*Mtop2*p12*p35 + p24*((-Mch2 + Mtop2)*p15 + 2*p13*p35) + p14*((-Mch2 + Mtop2)*p25 + 2*p23*p35))*Pi**2)/(p12**2*dentop2) + 
     -      (64*alphas**2*(A**2 + B**2)*CF*NC*(p15*((-Mch2 + Mtop2)*p24 + p23*(Mtop2 + p34)) + p14*((-Mch2 + Mtop2)*p25 + p23*p35) + 
     -           p12*(Mtop2*p35 + Mch2*p45) + p13*(p25*(Mtop2 + p34) + p24*p35 - 2*p23*p45))*Pi**2)/(p12**2*(Mch2 + Mtop2 + 2*p34)*dentop) + 
     -      (32*alphas**2*(A**2 + B**2)*CF*NC*((p15*((-Mch2 + Mtop2)*p24 + 2*p23*(Mtop2 + p34)) + p12*(2*(Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45))/
     -            (Mch2 + Mtop2 + 2*p34)**2 + (2*((Mtop2*p13 + Mch2*p14)*p25 + p24*((-Mch2 + Mtop2)*p15 + p13*p35) + 
     -                p12*((Mtop2 + p34)*p35 + (-Mch2 + Mtop2)*p45) + p23*(p15*(Mtop2 + p34) - 2*p14*p35 + p13*p45)))/
     -            ((Mch2 - Mtop2 - 2*p13)*(Mch2 + Mtop2 + 2*p34)) + 
     -           ((-Mch2 + Mtop2)*(p15*p24 + p12*p45) + 2*p13*(Mtop2*p25 + p24*p35 + p23*p45))/(-Mch2 + Mtop2 + 2*p13)**2)*Pi**2)/p25**2 + 
     -      CF*((64*alphas**2*(A**2 + B**2)*p15*((-Mch2 + Mtop2)*p24 + 2*p23*(Mtop2 + p34))*Pi**2)/(p12*p25*(Mch2 + Mtop2 + 2*p34)**2) + 
     -         (32*alphas**2*(A**2 + B**2)*(-((Mch2 - 2*Mtop2)*p15*p24) + Mch2*p14*p25 + Mch2*p12*p45 + p23*(2*p15*(Mtop2 + p34) - 2*p14*p35 - 2*p13*p45))*
     -            Pi**2)/(p12*(Mch2 - Mtop2 - 2*p13)*p25*dentop) + 
     -         (32*alphas**2*(A**2 + B**2)*(-((Mch2 - 2*Mtop2)*p15*p24) - (Mtop2*p13 + Mch2*p14)*p25 + p12*(Mtop2*p35 + Mch2*p45) + 
     -              p23*(p15*(3*Mtop2 + 2*p34) + 2*p14*p35 - 2*p13*p45))*Pi**2)/(p12*p25*(Mch2 + Mtop2 + 2*p34)*dentop) - 
     -         (32*alphas**2*(A**2 + B**2)*((Mch2 - 2*Mtop2)*p15*p24 - (Mtop2*p13 + Mch2*p14)*p25 + p12*(Mtop2*p35 + Mch2*p45) - 
     -              p23*(p15*(3*Mtop2 + 2*p34) - 2*p14*p35 + 2*p13*p45))*Pi**2)/(p12*(Mch2 - Mtop2 - 2*p13)*p25*(Mch2 +Mtop2 + 2*p34)))))/(Sqrt(2d0)*NC**2)
      
         amp2=amp2-amp2dr
         if(ds_flag) then
            call madgraphorderqq(pphy,ppp)
            call madgraphcouplings
            call  SMATRIXQQ(ppp,subtr)
            if(nearresonance(pphy)) write(*,*) 'subtr/amp2',
     1           subtr/amp2
            amp2=amp2-subtr
         endif         
      else
         amp2 = amp2dr
      endif

      end



      function nearresonance(p)
      implicit none
      include 'PhysPars.h'
      logical nearresonance
      real * 8 p(0:3,5),tmp
      integer mu
      tmp=0
      do mu=1,3
         tmp=tmp-(p(mu,3)+p(mu,5))**2
      enddo
      tmp=tmp+(p(0,3)+p(0,5))**2
      
      if(abs(tmp-ph_mT**2).lt.ph_Twidth*2*ph_mt) then
         nearresonance=.true.
      else
         nearresonance=.false.
      endif
      end

      
