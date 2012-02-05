      subroutine twoloopfactors(g2l)
      implicit none
      include 'PhysPars.h'
      include 'Flags.h'
      include 'pwhg_st.h'
      integer i
      double precision cf,cffer,cfsca,ca,m0,m12,y0,y12,mum
      double complex twoloop, twoloopT0

c     amp1loop is the 1-loop amplitude with EXACT mass dependence
      double precision bornqcd
      double complex ampl1loop,ampl,effampl1loop
      common /bornampl/ampl1loop,bornqcd

      double complex a1_full_top,a1_full_bot
      double precision a1_top,a2_top
      double complex a1_bot,a2_bot

      double complex a1_full_top_nosm,a1_full_bot_nosm
      double precision a1_top_nosm,a2_top_nosm
      double complex a1_bot_nosm,a2_bot_nosm

      double precision g2l
      double complex bot2l,top2l

      cf = 4d0/3d0
      cffer = 4d0/3d0
      cfsca = 4d0/3d0
      ca = 3d0

c     MSSM uses Slavich's subroutines
      if (model.eq.2) then
c     WARNING: commented (and implementation removed from the tree) in this SM model only release.
c         call DSgluglu(ih,mh,ph_Zmass,ph_Wmass,ph_topmass,ph_mg,
c     $ ph_t1_2, ph_t2_2,ph_stt,ph_ctt,q,ph_mumssm,ph_tanb,ph_alpha,
c     $ massren,0,a1_full_top_nosm,a1_top_nosm,a2_top_nosm)
c     Bottom/Sbottoms sector contribution
c         call gluglu_bot(ih,mh,ph_Zmass,ph_Wmass,ph_bottommass,
c     $ ph_mbsb,ph_bottommass,ph_bottommass_mh,ph_mg,ph_b1_2,ph_b2_2,
c     $ ph_stb,ph_ctb,q,
c     $ ph_mumssm,ph_tanb,ph_alpha,massren,flg_mssm_q_mh,0,
c     $ a1_full_bot_nosm,
c     $ a1_bot_nosm,a2_bot_nosm)

c     Two loop SM contributions with full mass dependence
         m12=mfer(2)
         y12=m12**2/mh2
         bot2l = lambdafer(2)*twoloop(1d0/y12,trfer(2),
     $        cf,ca,ferlogmratio(2),2)

         m12=mfer(1)
         y12=m12**2/mh2
         top2l = lambdafer(1)*twoloop(1d0/y12,trfer(1),
     $        cf,ca,ferlogmratio(1),1)

         if (flg_noscalars.le.0) then
            ampl = a2_top_nosm + a2_bot_nosm + bot2l + top2l
c$$$            write(*,*) 'a2_stops', a2_top_nosm
c$$$            write(*,*) 'a2_bot_nosm', a2_bot_nosm
c$$$            write(*,*) 'bot2l', bot2l
c$$$            write(*,*) 'top2l', top2l
         else
            ampl = bot2l + top2l
         endif
c     SM and Manohar-Wise models use the ABDV subroutines.
      else
         ampl = dcmplx(0d0)
c     In the Manohar-Wise model the scalars are in the adjoint representation
         if (model.eq.1) cfsca = 3d0
         mum = sqrt(mumass2)

         do i=1,afer
            m12=mfer(i)
            y12=m12**2/mh2
            ampl = ampl + lambdafer(i)*twoloop(1d0/y12,trfer(i),cf,ca,
     $           ferlogmratio(i),i)
         end do

         do i=1,asca
            m0=msca(i)
            y0=m0**2/mh2
            ampl = ampl + (mmaa/m0)**2*
     $           twoloopT0(1d0/y0,trsca(i),cfsca,ca,lambdasca(i),m0,mum)
         end do
      endif

      g2l = ampl/ampl1loop + dconjg(ampl/ampl1loop)
      end


      double complex function twoloop(s,TR,CF,CA,logmassratio,numfer)
      implicit none
      include 'Flags.h'
      double precision s,t,x,y,Hr1,Hr2,Hr3,Hr4,Hi1,Hi2,Hi3,Hi4,
     $                 ReF122la,RiF122la,ReF122lb,RiF122lb,
     $                 RF12OSre,RF12OSim,ReG122l,RiG122l,
     $                 ReG12t2l,RiG12t2l,reg12,rig12,
     $                 ref12a,rif12a,ref12b,rif12b,
     $                 reF1l,imF1l,imlogx2,relogx2,
     $                 CA,TR,CF,logmassratio
      real * 8 pi
      parameter (pi=3.141592653589793238462643383279502884197D0)

      integer numfer
      complex*16 Hc1,Hc2,Hc3,Hc4
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 

c     This is necessary to avoid SIGFPE. However there could be better ways to avoid this.
c     x is assigned a dummy value while the correct value for the case s < 4.0 is indeed calculated in the Re* Ri* functions
      if (s < 4.0d0 ) then
         x = 0.1
      else
         x = 4.d0/(Dsqrt(s-4.d0)+Dsqrt(s))**2
      endif

       call hplog(x,4,Hc1,Hc2,Hc3,Hc4, 
     $                      Hr1,Hr2,Hr3,Hr4,Hi1,Hi2,Hi3,Hi4,-1,1) 

c     Two-loop functions:
       ReF122la  = ref12a(x,s,Hr1,Hr2,Hr3,Hr4)
       RiF122la  = rif12a(x,s,Hr1,Hr2,Hr3,Hr4)
       ReF122lb  = ref12b(x,s,Hr1,Hr2,Hr3,Hr4)
       RiF122lb  = rif12b(x,s,Hr1,Hr2,Hr3,Hr4)

c     MS-DRbar for SM and DRBar for MSSM
       if ((massren.eq.1).or.((massren.eq.2).and.(model.eq.0))) then
          RF12OSre = ReF122la + ReF122lb*logmassratio
          RF12OSim = RiF122la + RiF122lb*logmassratio
c     On-Shell for SM and MSSM
       else if (massren.eq.0) then
c     eq 15
          RF12OSre = ReF122la + 4.d0/3.d0*ReF122lb
          RF12OSim = RiF122la + 4.d0/3.d0*RiF122lb
c     Mixed for MSSM
       else if ((massren.eq.2).and.(model.eq.2)) then
          write(*,*) 'mixed'
          RF12OSre = ReF122la + 4.d0/3.d0*ReF122lb
          RF12OSim = RiF122la + 4.d0/3.d0*RiF122lb
c     NOTE: this only works for the bottom!!! - TODO: if quarks number is arbitray this needs to be modified!
          if(numfer.eq.2) then
             y = 1d0/s
             relogx2 = log(x)**2 - pi**2
             imlogx2 = 2*pi*log(x)
             reF1l = -4*y*(2-(1-4*y)/2d0*relogx2)
             imF1l = 2*y*(1-4*y)*imlogx2

             RF12OSre = RF12OSre - reF1l/4d0*
     $                  (3*(logmassratio+1d0/3d0)-5)
             RF12OSim = RF12OSim - imF1l/4d0*
     $                  (3*(logmassratio+1d0/3d0)-5)
          endif
       end if

       ReG122l  = reg12(x,s,Hr1,Hr2,Hr3,Hr4)
       RiG122l  = rig12(x,s,Hr1,Hr2,Hr3,Hr4)

       ReG12t2l = TR*( CF*RF12OSre + CA*ReG122l )
       RiG12t2l = TR*( CF*RF12OSim + CA*RiG122l )
       twoloop = ReG12t2l + dcmplx(0d0,1d0)*RiG12t2l
      
       return
       end  

      double complex function twoloopT0(s,TR0,CF0,CA0,CouT0,mT0,muR)
      implicit none
      include 'Flags.h'
      double precision s,t,x,y,Hr1,Hr2,Hr3,Hr4,Hi1,Hi2,Hi3,Hi4,
     $                 CF0,CA0,TR0,CouT0,mT0,muR,
     $                 ReF02la,RiF02la,ReF02lb,RiF02lb,
     $                 ReF02lc,RiF02lc,RF0OSre,RF0OSim,ReG02la,RiG02la,
     $                 ReG02l,RiG02l,
     $                 ref0a,rif0a,ref0b,rif0b,ref0c,rif0c,
     $                 reg0,rig0,RF0MSre,RF0MSim

      complex*16 Hc1,Hc2,Hc3,Hc4
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 


c     This is necessary to avoid SIGFPE. However there could be better ways to avoid this.
c     x is assigned a dummy value while the correct value for the case s < 4.0 is indeed calculated in the re* ri* functions
      if (s < 4.0d0 ) then
         x = 0.1
      else
        x = 4.d0/(Dsqrt(s-4.d0)+Dsqrt(s))**2 
      endif

       call hplog(x,4,Hc1,Hc2,Hc3,Hc4, 
     $                      Hr1,Hr2,Hr3,Hr4,Hi1,Hi2,Hi3,Hi4,-1,1) 

c     Two loop functions
       ReF02la    = ref0a(x,s,Hr1,Hr2,Hr3,Hr4)
       RiF02la    = rif0a(x,s,Hr1,Hr2,Hr3,Hr4)

       ReF02lb    = ref0b(x,s,Hr1,Hr2,Hr3,Hr4)
       RiF02lb    = rif0b(x,s,Hr1,Hr2,Hr3,Hr4)

       ReF02lc    = ref0c(x,s,Hr1,Hr2,Hr3,Hr4)
       RiF02lc    = rif0c(x,s,Hr1,Hr2,Hr3,Hr4)

c     On-shell mass scheme
       if (massren.eq.0) then
          RF0OSre = ReF02la + 7.d0/3d0*ReF02lb + ReF02lc*
     $         Dlog(mT0**2/muR**2)
          RF0OSim = RiF02la + 7.d0/3d0*RiF02lb + RiF02lc*
     $         Dlog(mT0**2/muR**2)
c     MS/DRbar mass scheme
       else if (massren.eq.1) then
          RF0MSre = ReF02la + (ReF02lb + ReF02lc)*Dlog(mT0**2/muR**2)
          RF0MSim = RiF02la + (RiF02lb + RiF02lc)*Dlog(mT0**2/muR**2)
       else if (massren.eq.2) then
          write(*,*) 'Mixed renormalization
     $scheme not implemented yet...Aborting.'
          stop
       endif

       ReG02la  = reg0(x,s,Hr1,Hr2,Hr3,Hr4)
       RiG02la  = rig0(x,s,Hr1,Hr2,Hr3,Hr4)

c     MSbar mass renormalization
       if (massren.eq.1) then
          ReG02l = CouT0*TR0*( CF0*RF0MSre + CA0*ReG02la )
          RiG02l = CouT0*TR0*( CF0*RF0MSim + CA0*RiG02la )
       else if (massren.eq.0) then
c     On-shell mass renormalization
          ReG02l = CouT0*TR0*( CF0*RF0OSre + CA0*ReG02la )
          RiG02l = CouT0*TR0*( CF0*RF0OSim + CA0*RiG02la )
       else if (massren.eq.2) then
          write(*,*) 'Sorry, mixed ren scheme not implemented yet
     $...Aborting'
          stop
       endif

       twoloopT0 = ReG02l + dcmplx(0d0,1d0)*RiG02l
       return
       end

c
c     Fermions - Two loop Functions
c

c     F_{1/2}**(2l,a):
      double precision function ref12a(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.3.4d0) then 
      var = - Dlog(1.d0-s/4.d0)
      ref12a = 
     $ + 1.d0 
     $ - 2.8148148148148148148d-1*var 
     $ + 2.3350970017636684303d-2*var**2 
     $ + 4.5026958931720836483d-2*var**3 
     $ - 1.1889873794635699398d-3*var**4 
     $ - 2.4475833065951941785d-3*var**5 
     $ + 6.1657048115034573021d-5*var**6 
     $ + 1.0208266733231549513d-4*var**7 
     $ - 2.8135477669666141225d-6*var**8 
     $ - 3.7361954170466570857d-6*var**9 
     $ + 1.1336251019911585631d-7*var**10 
     $ + 1.2662234883506781727d-7*var**11 
     $ - 4.1637586967361846133d-9*var**12 
     $ - 4.0813823137151174548d-9*var**13 
     $ + 1.4309407250502841616d-10*var**14 
     $ + 1.2700613600707097482d-10*var**15
     $ - 4.6844599447401368541d-12*var**16
     $ - 3.8506235769257602207d-12*var**17
     $ + 1.4786066707857725761d-13*var**18
     $ + 1.1442377015592939720d-13*var**19
     $ - 4.5374855042020696288d-15*var**20
       endif
       if(s.ge.3.4d0.AND.s.lt.4.d0) then  
       var = - Dlog(s/4.d0)
      ref12a = 
     $ + 3.0040290963093044911d0
     $ + 22.134413761170373827d0*var 
     $ + 100.19085282337782347d0*var**2 
     $ + 120.41498403284863764d0*var**3 
     $ + 83.515565363797341150d0*var**4 
     $ + 41.399320670378598273d0*var**5 
     $ + 16.155120666538672973d0*var**6 
     $ + 5.2327545742819423442d0*var**7 
     $ + 1.4535392196489384623d0*var**8 
     $ + 3.5388885516738708230d-1*var**9 
     $ + 7.6690647561326467514d-2*var**10 
     $ - Dsqrt(var)*(
     $ + 18.849555921538759431d0
     $ + 41.785489583964381794d0*var 
     $ + 92.466289110042469781d0*var**2 
     $ + 103.34582073034416237d0*var**3 
     $ + 73.616564026072819935d0*var**4 
     $ + 38.676750958355122410d0*var**5 
     $ + 16.243741072549717212d0*var**6 
     $ + 5.7231820529208336444d0*var**7 
     $ + 1.7454277225947180414d0*var**8 
     $ + 4.7085653615377182139d-1*var**9 
     $ + 1.1414157364277368468d-1*var**10 )
     $ + Dlog(var)*(
     $ - 9.8696044010893586188d0*var 
     $ - 1.6449340668482264365d0*var**2 
     $ + 9.5406175877197133315d0*var**3 
     $ + 11.471456861424893411d0*var**4 
     $ + 7.7272736045036922837d0*var**5 
     $ + 3.7987176872037956469d0*var**6 
     $ + 1.4999967333379824689d0*var**7 
     $ + 4.9997589743396814580d-1*var**8 
     $ + 1.4500554216047137430d-1*var**9 
     $ + 3.7348735861890670383d-2*var**10 )
     $ + Dsqrt(var)*Dlog(var)*(
     $ + 16.755160819145563938d0*var**2 
     $ + 25.411993909037438640d0*var**3 
     $ + 20.260779288151317346d0*var**4 
     $ + 11.275408744300700510d0*var**5 
     $ + 4.9024027304943311775d0*var**6 
     $ + 1.7670681012154063247d0*var**7 
     $ + 5.4738899481570033597d-1*var**8 
     $ + 1.4925412981654110016d-1*var**9 
     $ + 3.6434842589781479026d-2*var**10 )
       endif
       if(s.ge.4.d0) then
      ref12a = 
     $ + 264*z2/(1.d0+x)**4
     $ - 528*z2/(1.d0+x)**3
     $ + 288*z2/(1.d0+x)**2
     $ - 24*z2/(1.d0+x)
     $ - 276.d0/5*z2**2/(1.d0+x)**5
     $ + 138*z2**2/(1.d0+x)**4
     $ - 138*z2**2/(1.d0+x)**3
     $ + 69*z2**2/(1.d0+x)**2
     $ - 69.d0/5*z2**2/(1.d0+x)
     $ + 48*z3/(1.d0+x)**4
     $ - 96*z3/(1.d0+x)**3
     $ + 44*z3/(1.d0+x)**2
     $ + 4*z3/(1.d0+x)
     $ + 36.d0/(1.d0+x)**2
     $ - 36.d0/(1.d0+x)
     $ + 48*Hr1(-1)*z2/(1.d0+x)**4
     $ - 96*Hr1(-1)*z2/(1.d0+x)**3
     $ + 108*Hr1(-1)*z2/(1.d0+x)**2
     $ - 60*Hr1(-1)*z2/(1.d0+x)
     $ + 144*Hr1(0)*z2/(1.d0+x)**5
     $ - 248*Hr1(0)*z2/(1.d0+x)**4
     $ + 100*Hr1(0)*z2/(1.d0+x)**3
     $ - 10*Hr1(0)*z2/(1.d0+x)**2
     $ + 14*Hr1(0)*z2/(1.d0+x)
     $ + 64*Hr1(0)*z3/(1.d0+x)**5
     $ - 160*Hr1(0)*z3/(1.d0+x)**4
     $ + 160*Hr1(0)*z3/(1.d0+x)**3
     $ - 80*Hr1(0)*z3/(1.d0+x)**2
     $ + 16*Hr1(0)*z3/(1.d0+x)
     $ + 8*Hr1(0)/(1.d0+x)**3
     $ - 12*Hr1(0)/(1.d0+x)**2
     $ + 4*Hr1(0)/(1.d0+x)
     $ + 336*Hr2(0,-1)*z2/(1.d0+x)**5
     $ - 840*Hr2(0,-1)*z2/(1.d0+x)**4
     $ + 840*Hr2(0,-1)*z2/(1.d0+x)**3
     $ - 420*Hr2(0,-1)*z2/(1.d0+x)**2
     $ + 84*Hr2(0,-1)*z2/(1.d0+x)
     $ + 8*Hr2(0,0)*z2/(1.d0+x)**5
     $ - 20*Hr2(0,0)*z2/(1.d0+x)**4
     $ + 20*Hr2(0,0)*z2/(1.d0+x)**3
     $ - 10*Hr2(0,0)*z2/(1.d0+x)**2
     $ + 2*Hr2(0,0)*z2/(1.d0+x)
     $ - 88*Hr2(0,0)/(1.d0+x)**4
     $ + 176*Hr2(0,0)/(1.d0+x)**3
     $ - 96*Hr2(0,0)/(1.d0+x)**2
     $ + 8*Hr2(0,0)/(1.d0+x)
     $ - 192*Hr2(0,1)*z2/(1.d0+x)**5
     $ + 480*Hr2(0,1)*z2/(1.d0+x)**4
     $ - 480*Hr2(0,1)*z2/(1.d0+x)**3
     $ + 240*Hr2(0,1)*z2/(1.d0+x)**2
     $ - 48*Hr2(0,1)*z2/(1.d0+x)
     $ - 16*Hr3(-1,0,0)/(1.d0+x)**4
     $ + 32*Hr3(-1,0,0)/(1.d0+x)**3
     $ - 36*Hr3(-1,0,0)/(1.d0+x)**2
     $ + 20*Hr3(-1,0,0)/(1.d0+x)
     $ + 16*Hr3(0,-1,0)/(1.d0+x)**4
     $ - 32*Hr3(0,-1,0)/(1.d0+x)**3
     $ + 20*Hr3(0,-1,0)/(1.d0+x)**2
     $ - 4*Hr3(0,-1,0)/(1.d0+x)
     $ - 48*Hr3(0,0,0)/(1.d0+x)**5
     $ + 88*Hr3(0,0,0)/(1.d0+x)**4
     $ - 44*Hr3(0,0,0)/(1.d0+x)**3
     $ + 10*Hr3(0,0,0)/(1.d0+x)**2
     $ - 6*Hr3(0,0,0)/(1.d0+x)
     $ - 64*Hr3(0,1,0)/(1.d0+x)**4
     $ + 128*Hr3(0,1,0)/(1.d0+x)**3
     $ - 80*Hr3(0,1,0)/(1.d0+x)**2
     $ + 16*Hr3(0,1,0)/(1.d0+x)
     $ - 112*Hr4(0,-1,0,0)/(1.d0+x)**5
     $ + 280*Hr4(0,-1,0,0)/(1.d0+x)**4
     $ - 280*Hr4(0,-1,0,0)/(1.d0+x)**3
     $ + 140*Hr4(0,-1,0,0)/(1.d0+x)**2
     $ - 28*Hr4(0,-1,0,0)/(1.d0+x)
     $ + 32*Hr4(0,0,-1,0)/(1.d0+x)**5
     $ - 80*Hr4(0,0,-1,0)/(1.d0+x)**4
     $ + 80*Hr4(0,0,-1,0)/(1.d0+x)**3
     $ - 40*Hr4(0,0,-1,0)/(1.d0+x)**2
     $ + 8*Hr4(0,0,-1,0)/(1.d0+x)
     $ + 8*Hr4(0,0,0,0)/(1.d0+x)**5
     $ - 20*Hr4(0,0,0,0)/(1.d0+x)**4
     $ + 20*Hr4(0,0,0,0)/(1.d0+x)**3
     $ - 10*Hr4(0,0,0,0)/(1.d0+x)**2
     $ + 2*Hr4(0,0,0,0)/(1.d0+x)
     $ - 128*Hr4(0,0,1,0)/(1.d0+x)**5
     $ + 320*Hr4(0,0,1,0)/(1.d0+x)**4
     $ - 320*Hr4(0,0,1,0)/(1.d0+x)**3
     $ + 160*Hr4(0,0,1,0)/(1.d0+x)**2
     $ - 32*Hr4(0,0,1,0)/(1.d0+x)
     $ + 64*Hr4(0,1,0,0)/(1.d0+x)**5
     $ - 160*Hr4(0,1,0,0)/(1.d0+x)**4
     $ + 160*Hr4(0,1,0,0)/(1.d0+x)**3
     $ - 80*Hr4(0,1,0,0)/(1.d0+x)**2
     $ + 16*Hr4(0,1,0,0)/(1.d0+x)
       endif
      return
      end  

      double precision function ref12at(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.3.4d0) then 
      var = - Dlog(1.d0-s/4.d0)
      ref12at = 
     $ + 1.d0 
     $ - 2.8148148148148148148d-1*var 
     $ + 2.3350970017636684303d-2*var**2 
     $ + 4.5026958931720836483d-2*var**3 
     $ - 1.1889873794635699398d-3*var**4 
     $ - 2.4475833065951941785d-3*var**5 
     $ + 6.1657048115034573021d-5*var**6 
     $ + 1.0208266733231549513d-4*var**7 
     $ - 2.8135477669666141225d-6*var**8 
     $ - 3.7361954170466570857d-6*var**9 
     $ + 1.1336251019911585631d-7*var**10 
     $ + 1.2662234883506781727d-7*var**11 
     $ - 4.1637586967361846133d-9*var**12 
     $ - 4.0813823137151174548d-9*var**13 
     $ + 1.4309407250502841616d-10*var**14 
     $ + 1.2700613600707097482d-10*var**15
     $ - 4.6844599447401368541d-12*var**16
     $ - 3.8506235769257602207d-12*var**17
     $ + 1.4786066707857725761d-13*var**18
     $ + 1.1442377015592939720d-13*var**19
     $ - 4.5374855042020696288d-15*var**20
       endif
       if(s.ge.3.4d0.AND.s.lt.4.d0) then  
       var = - Dlog(s/4.d0)
      ref12at = 
     $ + 3.0040290963093044911d0
     $ + 22.134413761170373827d0*var 
     $ + 100.19085282337782347d0*var**2 
     $ + 120.41498403284863764d0*var**3 
     $ + 83.515565363797341150d0*var**4 
     $ + 41.399320670378598273d0*var**5 
     $ + 16.155120666538672973d0*var**6 
     $ + 5.2327545742819423442d0*var**7 
     $ + 1.4535392196489384623d0*var**8 
     $ + 3.5388885516738708230d-1*var**9 
     $ + 7.6690647561326467514d-2*var**10 
     $ - Dsqrt(var)*(
     $ + 18.849555921538759431d0
     $ + 41.785489583964381794d0*var 
     $ + 92.466289110042469781d0*var**2 
     $ + 103.34582073034416237d0*var**3 
     $ + 73.616564026072819935d0*var**4 
     $ + 38.676750958355122410d0*var**5 
     $ + 16.243741072549717212d0*var**6 
     $ + 5.7231820529208336444d0*var**7 
     $ + 1.7454277225947180414d0*var**8 
     $ + 4.7085653615377182139d-1*var**9 
     $ + 1.1414157364277368468d-1*var**10 )
     $ + Dlog(var)*(
     $ - 9.8696044010893586188d0*var 
     $ - 1.6449340668482264365d0*var**2 
     $ + 9.5406175877197133315d0*var**3 
     $ + 11.471456861424893411d0*var**4 
     $ + 7.7272736045036922837d0*var**5 
     $ + 3.7987176872037956469d0*var**6 
     $ + 1.4999967333379824689d0*var**7 
     $ + 4.9997589743396814580d-1*var**8 
     $ + 1.4500554216047137430d-1*var**9 
     $ + 3.7348735861890670383d-2*var**10 )
     $ + Dsqrt(var)*Dlog(var)*(
     $ + 16.755160819145563938d0*var**2 
     $ + 25.411993909037438640d0*var**3 
     $ + 20.260779288151317346d0*var**4 
     $ + 11.275408744300700510d0*var**5 
     $ + 4.9024027304943311775d0*var**6 
     $ + 1.7670681012154063247d0*var**7 
     $ + 5.4738899481570033597d-1*var**8 
     $ + 1.4925412981654110016d-1*var**9 
     $ + 3.6434842589781479026d-2*var**10 )
       endif
       if(s.ge.4.d0) then
      ref12at = 
     $ + 264*z2/(1.d0+x)**4
     $ - 528*z2/(1.d0+x)**3
     $ + 288*z2/(1.d0+x)**2
     $ - 24*z2/(1.d0+x)
     $ - 276.d0/5*z2**2/(1.d0+x)**5
     $ + 138*z2**2/(1.d0+x)**4
     $ - 138*z2**2/(1.d0+x)**3
     $ + 69*z2**2/(1.d0+x)**2
     $ - 69.d0/5*z2**2/(1.d0+x)
     $ + 48*z3/(1.d0+x)**4
     $ - 96*z3/(1.d0+x)**3
     $ + 44*z3/(1.d0+x)**2
     $ + 4*z3/(1.d0+x)
     $ + 36.d0/(1.d0+x)**2
     $ - 36.d0/(1.d0+x)
     $ + 48*Hr1(-1)*z2/(1.d0+x)**4
     $ - 96*Hr1(-1)*z2/(1.d0+x)**3
     $ + 108*Hr1(-1)*z2/(1.d0+x)**2
     $ - 60*Hr1(-1)*z2/(1.d0+x)
     $ + 144*Hr1(0)*z2/(1.d0+x)**5
     $ - 248*Hr1(0)*z2/(1.d0+x)**4
     $ + 100*Hr1(0)*z2/(1.d0+x)**3
     $ - 10*Hr1(0)*z2/(1.d0+x)**2
     $ + 14*Hr1(0)*z2/(1.d0+x)
     $ + 64*Hr1(0)*z3/(1.d0+x)**5
     $ - 160*Hr1(0)*z3/(1.d0+x)**4
     $ + 160*Hr1(0)*z3/(1.d0+x)**3
     $ - 80*Hr1(0)*z3/(1.d0+x)**2
     $ + 16*Hr1(0)*z3/(1.d0+x)
     $ + 8*Hr1(0)/(1.d0+x)**3
     $ - 12*Hr1(0)/(1.d0+x)**2
     $ + 4*Hr1(0)/(1.d0+x)
     $ + 336*Hr2(0,-1)*z2/(1.d0+x)**5
     $ - 840*Hr2(0,-1)*z2/(1.d0+x)**4
     $ + 840*Hr2(0,-1)*z2/(1.d0+x)**3
     $ - 420*Hr2(0,-1)*z2/(1.d0+x)**2
     $ + 84*Hr2(0,-1)*z2/(1.d0+x)
     $ + 8*Hr2(0,0)*z2/(1.d0+x)**5
     $ - 20*Hr2(0,0)*z2/(1.d0+x)**4
     $ + 20*Hr2(0,0)*z2/(1.d0+x)**3
     $ - 10*Hr2(0,0)*z2/(1.d0+x)**2
     $ + 2*Hr2(0,0)*z2/(1.d0+x)
     $ - 88*Hr2(0,0)/(1.d0+x)**4
     $ + 176*Hr2(0,0)/(1.d0+x)**3
     $ - 96*Hr2(0,0)/(1.d0+x)**2
     $ + 8*Hr2(0,0)/(1.d0+x)
     $ - 192*Hr2(0,1)*z2/(1.d0+x)**5
     $ + 480*Hr2(0,1)*z2/(1.d0+x)**4
     $ - 480*Hr2(0,1)*z2/(1.d0+x)**3
     $ + 240*Hr2(0,1)*z2/(1.d0+x)**2
     $ - 48*Hr2(0,1)*z2/(1.d0+x)
     $ - 16*Hr3(-1,0,0)/(1.d0+x)**4
     $ + 32*Hr3(-1,0,0)/(1.d0+x)**3
     $ - 36*Hr3(-1,0,0)/(1.d0+x)**2
     $ + 20*Hr3(-1,0,0)/(1.d0+x)
     $ + 16*Hr3(0,-1,0)/(1.d0+x)**4
     $ - 32*Hr3(0,-1,0)/(1.d0+x)**3
     $ + 20*Hr3(0,-1,0)/(1.d0+x)**2
     $ - 4*Hr3(0,-1,0)/(1.d0+x)
     $ - 48*Hr3(0,0,0)/(1.d0+x)**5
     $ + 88*Hr3(0,0,0)/(1.d0+x)**4
     $ - 44*Hr3(0,0,0)/(1.d0+x)**3
     $ + 10*Hr3(0,0,0)/(1.d0+x)**2
     $ - 6*Hr3(0,0,0)/(1.d0+x)
     $ - 64*Hr3(0,1,0)/(1.d0+x)**4
     $ + 128*Hr3(0,1,0)/(1.d0+x)**3
     $ - 80*Hr3(0,1,0)/(1.d0+x)**2
     $ + 16*Hr3(0,1,0)/(1.d0+x)
     $ - 112*Hr4(0,-1,0,0)/(1.d0+x)**5
     $ + 280*Hr4(0,-1,0,0)/(1.d0+x)**4
     $ - 280*Hr4(0,-1,0,0)/(1.d0+x)**3
     $ + 140*Hr4(0,-1,0,0)/(1.d0+x)**2
     $ - 28*Hr4(0,-1,0,0)/(1.d0+x)
     $ + 32*Hr4(0,0,-1,0)/(1.d0+x)**5
     $ - 80*Hr4(0,0,-1,0)/(1.d0+x)**4
     $ + 80*Hr4(0,0,-1,0)/(1.d0+x)**3
     $ - 40*Hr4(0,0,-1,0)/(1.d0+x)**2
     $ + 8*Hr4(0,0,-1,0)/(1.d0+x)
     $ + 8*Hr4(0,0,0,0)/(1.d0+x)**5
     $ - 20*Hr4(0,0,0,0)/(1.d0+x)**4
     $ + 20*Hr4(0,0,0,0)/(1.d0+x)**3
     $ - 10*Hr4(0,0,0,0)/(1.d0+x)**2
     $ + 2*Hr4(0,0,0,0)/(1.d0+x)
     $ - 128*Hr4(0,0,1,0)/(1.d0+x)**5
     $ + 320*Hr4(0,0,1,0)/(1.d0+x)**4
     $ - 320*Hr4(0,0,1,0)/(1.d0+x)**3
     $ + 160*Hr4(0,0,1,0)/(1.d0+x)**2
     $ - 32*Hr4(0,0,1,0)/(1.d0+x)
     $ + 64*Hr4(0,1,0,0)/(1.d0+x)**5
     $ - 160*Hr4(0,1,0,0)/(1.d0+x)**4
     $ + 160*Hr4(0,1,0,0)/(1.d0+x)**3
     $ - 80*Hr4(0,1,0,0)/(1.d0+x)**2
     $ + 16*Hr4(0,1,0,0)/(1.d0+x)
       endif
      return
      end

      double precision function ref12ab(x,s,HAr1,HAr2,HAr3,HAr4)
      implicit double precision (a-h,o-z) 
      dimension HAc1(-1:1),HAc2(-1:1,-1:1),HAc3(-1:1,-1:1,-1:1), 
     $          HAc4(-1:1,-1:1,-1:1,-1:1) 
      dimension HAr1(-1:1),HAr2(-1:1,-1:1),HAr3(-1:1,-1:1,-1:1), 
     $          HAr4(-1:1,-1:1,-1:1,-1:1) 
      dimension HAi1(-1:1),HAi2(-1:1,-1:1),HAi3(-1:1,-1:1,-1:1), 
     $          HAi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.3.4d0) then 
      var = - Dlog(1.d0-s/4.d0)
      ref12ab = 
     $ + 1.d0 
     $ - 2.8148148148148148148d-1*var 
     $ + 2.3350970017636684303d-2*var**2 
     $ + 4.5026958931720836483d-2*var**3 
     $ - 1.1889873794635699398d-3*var**4 
     $ - 2.4475833065951941785d-3*var**5 
     $ + 6.1657048115034573021d-5*var**6 
     $ + 1.0208266733231549513d-4*var**7 
     $ - 2.8135477669666141225d-6*var**8 
     $ - 3.7361954170466570857d-6*var**9 
     $ + 1.1336251019911585631d-7*var**10 
     $ + 1.2662234883506781727d-7*var**11 
     $ - 4.1637586967361846133d-9*var**12 
     $ - 4.0813823137151174548d-9*var**13 
     $ + 1.4309407250502841616d-10*var**14 
     $ + 1.2700613600707097482d-10*var**15
     $ - 4.6844599447401368541d-12*var**16
     $ - 3.8506235769257602207d-12*var**17
     $ + 1.4786066707857725761d-13*var**18
     $ + 1.1442377015592939720d-13*var**19
     $ - 4.5374855042020696288d-15*var**20
       endif
       if(s.ge.3.4d0.AND.s.lt.4.d0) then  
       var = - Dlog(s/4.d0)
      ref12ab = 
     $ + 3.0040290963093044911d0
     $ + 22.134413761170373827d0*var 
     $ + 100.19085282337782347d0*var**2 
     $ + 120.41498403284863764d0*var**3 
     $ + 83.515565363797341150d0*var**4 
     $ + 41.399320670378598273d0*var**5 
     $ + 16.155120666538672973d0*var**6 
     $ + 5.2327545742819423442d0*var**7 
     $ + 1.4535392196489384623d0*var**8 
     $ + 3.5388885516738708230d-1*var**9 
     $ + 7.6690647561326467514d-2*var**10 
     $ - Dsqrt(var)*(
     $ + 18.849555921538759431d0
     $ + 41.785489583964381794d0*var 
     $ + 92.466289110042469781d0*var**2 
     $ + 103.34582073034416237d0*var**3 
     $ + 73.616564026072819935d0*var**4 
     $ + 38.676750958355122410d0*var**5 
     $ + 16.243741072549717212d0*var**6 
     $ + 5.7231820529208336444d0*var**7 
     $ + 1.7454277225947180414d0*var**8 
     $ + 4.7085653615377182139d-1*var**9 
     $ + 1.1414157364277368468d-1*var**10 )
     $ + Dlog(var)*(
     $ - 9.8696044010893586188d0*var 
     $ - 1.6449340668482264365d0*var**2 
     $ + 9.5406175877197133315d0*var**3 
     $ + 11.471456861424893411d0*var**4 
     $ + 7.7272736045036922837d0*var**5 
     $ + 3.7987176872037956469d0*var**6 
     $ + 1.4999967333379824689d0*var**7 
     $ + 4.9997589743396814580d-1*var**8 
     $ + 1.4500554216047137430d-1*var**9 
     $ + 3.7348735861890670383d-2*var**10 )
     $ + Dsqrt(var)*Dlog(var)*(
     $ + 16.755160819145563938d0*var**2 
     $ + 25.411993909037438640d0*var**3 
     $ + 20.260779288151317346d0*var**4 
     $ + 11.275408744300700510d0*var**5 
     $ + 4.9024027304943311775d0*var**6 
     $ + 1.7670681012154063247d0*var**7 
     $ + 5.4738899481570033597d-1*var**8 
     $ + 1.4925412981654110016d-1*var**9 
     $ + 3.6434842589781479026d-2*var**10 )
       endif
       if(s.ge.4.d0) then
      ref12ab = 
     $ + 264*z2/(1.d0+x)**4
     $ - 528*z2/(1.d0+x)**3
     $ + 288*z2/(1.d0+x)**2
     $ - 24*z2/(1.d0+x)
     $ - 276.d0/5*z2**2/(1.d0+x)**5
     $ + 138*z2**2/(1.d0+x)**4
     $ - 138*z2**2/(1.d0+x)**3
     $ + 69*z2**2/(1.d0+x)**2
     $ - 69.d0/5*z2**2/(1.d0+x)
     $ + 48*z3/(1.d0+x)**4
     $ - 96*z3/(1.d0+x)**3
     $ + 44*z3/(1.d0+x)**2
     $ + 4*z3/(1.d0+x)
     $ + 36.d0/(1.d0+x)**2
     $ - 36.d0/(1.d0+x)
     $ + 48*HAr1(-1)*z2/(1.d0+x)**4
     $ - 96*HAr1(-1)*z2/(1.d0+x)**3
     $ + 108*HAr1(-1)*z2/(1.d0+x)**2
     $ - 60*HAr1(-1)*z2/(1.d0+x)
     $ + 144*HAr1(0)*z2/(1.d0+x)**5
     $ - 248*HAr1(0)*z2/(1.d0+x)**4
     $ + 100*HAr1(0)*z2/(1.d0+x)**3
     $ - 10*HAr1(0)*z2/(1.d0+x)**2
     $ + 14*HAr1(0)*z2/(1.d0+x)
     $ + 64*HAr1(0)*z3/(1.d0+x)**5
     $ - 160*HAr1(0)*z3/(1.d0+x)**4
     $ + 160*HAr1(0)*z3/(1.d0+x)**3
     $ - 80*HAr1(0)*z3/(1.d0+x)**2
     $ + 16*HAr1(0)*z3/(1.d0+x)
     $ + 8*HAr1(0)/(1.d0+x)**3
     $ - 12*HAr1(0)/(1.d0+x)**2
     $ + 4*HAr1(0)/(1.d0+x)
     $ + 336*HAr2(0,-1)*z2/(1.d0+x)**5
     $ - 840*HAr2(0,-1)*z2/(1.d0+x)**4
     $ + 840*HAr2(0,-1)*z2/(1.d0+x)**3
     $ - 420*HAr2(0,-1)*z2/(1.d0+x)**2
     $ + 84*HAr2(0,-1)*z2/(1.d0+x)
     $ + 8*HAr2(0,0)*z2/(1.d0+x)**5
     $ - 20*HAr2(0,0)*z2/(1.d0+x)**4
     $ + 20*HAr2(0,0)*z2/(1.d0+x)**3
     $ - 10*HAr2(0,0)*z2/(1.d0+x)**2
     $ + 2*HAr2(0,0)*z2/(1.d0+x)
     $ - 88*HAr2(0,0)/(1.d0+x)**4
     $ + 176*HAr2(0,0)/(1.d0+x)**3
     $ - 96*HAr2(0,0)/(1.d0+x)**2
     $ + 8*HAr2(0,0)/(1.d0+x)
     $ - 192*HAr2(0,1)*z2/(1.d0+x)**5
     $ + 480*HAr2(0,1)*z2/(1.d0+x)**4
     $ - 480*HAr2(0,1)*z2/(1.d0+x)**3
     $ + 240*HAr2(0,1)*z2/(1.d0+x)**2
     $ - 48*HAr2(0,1)*z2/(1.d0+x)
     $ - 16*HAr3(-1,0,0)/(1.d0+x)**4
     $ + 32*HAr3(-1,0,0)/(1.d0+x)**3
     $ - 36*HAr3(-1,0,0)/(1.d0+x)**2
     $ + 20*HAr3(-1,0,0)/(1.d0+x)
     $ + 16*HAr3(0,-1,0)/(1.d0+x)**4
     $ - 32*HAr3(0,-1,0)/(1.d0+x)**3
     $ + 20*HAr3(0,-1,0)/(1.d0+x)**2
     $ - 4*HAr3(0,-1,0)/(1.d0+x)
     $ - 48*HAr3(0,0,0)/(1.d0+x)**5
     $ + 88*HAr3(0,0,0)/(1.d0+x)**4
     $ - 44*HAr3(0,0,0)/(1.d0+x)**3
     $ + 10*HAr3(0,0,0)/(1.d0+x)**2
     $ - 6*HAr3(0,0,0)/(1.d0+x)
     $ - 64*HAr3(0,1,0)/(1.d0+x)**4
     $ + 128*HAr3(0,1,0)/(1.d0+x)**3
     $ - 80*HAr3(0,1,0)/(1.d0+x)**2
     $ + 16*HAr3(0,1,0)/(1.d0+x)
     $ - 112*HAr4(0,-1,0,0)/(1.d0+x)**5
     $ + 280*HAr4(0,-1,0,0)/(1.d0+x)**4
     $ - 280*HAr4(0,-1,0,0)/(1.d0+x)**3
     $ + 140*HAr4(0,-1,0,0)/(1.d0+x)**2
     $ - 28*HAr4(0,-1,0,0)/(1.d0+x)
     $ + 32*HAr4(0,0,-1,0)/(1.d0+x)**5
     $ - 80*HAr4(0,0,-1,0)/(1.d0+x)**4
     $ + 80*HAr4(0,0,-1,0)/(1.d0+x)**3
     $ - 40*HAr4(0,0,-1,0)/(1.d0+x)**2
     $ + 8*HAr4(0,0,-1,0)/(1.d0+x)
     $ + 8*HAr4(0,0,0,0)/(1.d0+x)**5
     $ - 20*HAr4(0,0,0,0)/(1.d0+x)**4
     $ + 20*HAr4(0,0,0,0)/(1.d0+x)**3
     $ - 10*HAr4(0,0,0,0)/(1.d0+x)**2
     $ + 2*HAr4(0,0,0,0)/(1.d0+x)
     $ - 128*HAr4(0,0,1,0)/(1.d0+x)**5
     $ + 320*HAr4(0,0,1,0)/(1.d0+x)**4
     $ - 320*HAr4(0,0,1,0)/(1.d0+x)**3
     $ + 160*HAr4(0,0,1,0)/(1.d0+x)**2
     $ - 32*HAr4(0,0,1,0)/(1.d0+x)
     $ + 64*HAr4(0,1,0,0)/(1.d0+x)**5
     $ - 160*HAr4(0,1,0,0)/(1.d0+x)**4
     $ + 160*HAr4(0,1,0,0)/(1.d0+x)**3
     $ - 80*HAr4(0,1,0,0)/(1.d0+x)**2
     $ + 16*HAr4(0,1,0,0)/(1.d0+x)
       endif
      return
      end  

      double precision function rif12a(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.4.d0) then 
      rif12a = 0.d0
       endif
       if(s.ge.4.d0) then
      rif12a = pi * (
     $ + 48*z2/(1.d0+x)**5
     $ - 72*z2/(1.d0+x)**4
     $ + 12*z2/(1.d0+x)**3
     $ + 10*z2/(1.d0+x)**2
     $ + 2*z2/(1.d0+x)
     $ + 64*z3/(1.d0+x)**5
     $ - 160*z3/(1.d0+x)**4
     $ + 160*z3/(1.d0+x)**3
     $ - 80*z3/(1.d0+x)**2
     $ + 16*z3/(1.d0+x)
     $ + 8.d0/(1.d0+x)**3
     $ - 12.d0/(1.d0+x)**2
     $ + 4.d0/(1.d0+x)
     $ + 24*Hr1(0)*z2/(1.d0+x)**5
     $ - 60*Hr1(0)*z2/(1.d0+x)**4
     $ + 60*Hr1(0)*z2/(1.d0+x)**3
     $ - 30*Hr1(0)*z2/(1.d0+x)**2
     $ + 6*Hr1(0)*z2/(1.d0+x)
     $ - 88*Hr1(0)/(1.d0+x)**4
     $ + 176*Hr1(0)/(1.d0+x)**3
     $ - 96*Hr1(0)/(1.d0+x)**2
     $ + 8*Hr1(0)/(1.d0+x)
     $ - 16*Hr2(-1,0)/(1.d0+x)**4
     $ + 32*Hr2(-1,0)/(1.d0+x)**3
     $ - 36*Hr2(-1,0)/(1.d0+x)**2
     $ + 20*Hr2(-1,0)/(1.d0+x)
     $ + 16*Hr2(0,-1)/(1.d0+x)**4
     $ - 32*Hr2(0,-1)/(1.d0+x)**3
     $ + 20*Hr2(0,-1)/(1.d0+x)**2
     $ - 4*Hr2(0,-1)/(1.d0+x)
     $ - 48*Hr2(0,0)/(1.d0+x)**5
     $ + 88*Hr2(0,0)/(1.d0+x)**4
     $ - 44*Hr2(0,0)/(1.d0+x)**3
     $ + 10*Hr2(0,0)/(1.d0+x)**2
     $ - 6*Hr2(0,0)/(1.d0+x)
     $ - 64*Hr2(0,1)/(1.d0+x)**4
     $ + 128*Hr2(0,1)/(1.d0+x)**3
     $ - 80*Hr2(0,1)/(1.d0+x)**2
     $ + 16*Hr2(0,1)/(1.d0+x)
     $ - 112*Hr3(0,-1,0)/(1.d0+x)**5
     $ + 280*Hr3(0,-1,0)/(1.d0+x)**4
     $ - 280*Hr3(0,-1,0)/(1.d0+x)**3
     $ + 140*Hr3(0,-1,0)/(1.d0+x)**2
     $ - 28*Hr3(0,-1,0)/(1.d0+x)
     $ + 32*Hr3(0,0,-1)/(1.d0+x)**5
     $ - 80*Hr3(0,0,-1)/(1.d0+x)**4
     $ + 80*Hr3(0,0,-1)/(1.d0+x)**3
     $ - 40*Hr3(0,0,-1)/(1.d0+x)**2
     $ + 8*Hr3(0,0,-1)/(1.d0+x)
     $ + 8*Hr3(0,0,0)/(1.d0+x)**5
     $ - 20*Hr3(0,0,0)/(1.d0+x)**4
     $ + 20*Hr3(0,0,0)/(1.d0+x)**3
     $ - 10*Hr3(0,0,0)/(1.d0+x)**2
     $ + 2*Hr3(0,0,0)/(1.d0+x)
     $ - 128*Hr3(0,0,1)/(1.d0+x)**5
     $ + 320*Hr3(0,0,1)/(1.d0+x)**4
     $ - 320*Hr3(0,0,1)/(1.d0+x)**3
     $ + 160*Hr3(0,0,1)/(1.d0+x)**2
     $ - 32*Hr3(0,0,1)/(1.d0+x)
     $ + 64*Hr3(0,1,0)/(1.d0+x)**5
     $ - 160*Hr3(0,1,0)/(1.d0+x)**4
     $ + 160*Hr3(0,1,0)/(1.d0+x)**3
     $ - 80*Hr3(0,1,0)/(1.d0+x)**2
     $ + 16*Hr3(0,1,0)/(1.d0+x) )
       endif
      return
      end  

      double precision function rif12at(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.4.d0) then 
      rif12at = 0.d0
       endif
       if(s.ge.4.d0) then
      rif12at = pi * (
     $ + 48*z2/(1.d0+x)**5
     $ - 72*z2/(1.d0+x)**4
     $ + 12*z2/(1.d0+x)**3
     $ + 10*z2/(1.d0+x)**2
     $ + 2*z2/(1.d0+x)
     $ + 64*z3/(1.d0+x)**5
     $ - 160*z3/(1.d0+x)**4
     $ + 160*z3/(1.d0+x)**3
     $ - 80*z3/(1.d0+x)**2
     $ + 16*z3/(1.d0+x)
     $ + 8.d0/(1.d0+x)**3
     $ - 12.d0/(1.d0+x)**2
     $ + 4.d0/(1.d0+x)
     $ + 24*Hr1(0)*z2/(1.d0+x)**5
     $ - 60*Hr1(0)*z2/(1.d0+x)**4
     $ + 60*Hr1(0)*z2/(1.d0+x)**3
     $ - 30*Hr1(0)*z2/(1.d0+x)**2
     $ + 6*Hr1(0)*z2/(1.d0+x)
     $ - 88*Hr1(0)/(1.d0+x)**4
     $ + 176*Hr1(0)/(1.d0+x)**3
     $ - 96*Hr1(0)/(1.d0+x)**2
     $ + 8*Hr1(0)/(1.d0+x)
     $ - 16*Hr2(-1,0)/(1.d0+x)**4
     $ + 32*Hr2(-1,0)/(1.d0+x)**3
     $ - 36*Hr2(-1,0)/(1.d0+x)**2
     $ + 20*Hr2(-1,0)/(1.d0+x)
     $ + 16*Hr2(0,-1)/(1.d0+x)**4
     $ - 32*Hr2(0,-1)/(1.d0+x)**3
     $ + 20*Hr2(0,-1)/(1.d0+x)**2
     $ - 4*Hr2(0,-1)/(1.d0+x)
     $ - 48*Hr2(0,0)/(1.d0+x)**5
     $ + 88*Hr2(0,0)/(1.d0+x)**4
     $ - 44*Hr2(0,0)/(1.d0+x)**3
     $ + 10*Hr2(0,0)/(1.d0+x)**2
     $ - 6*Hr2(0,0)/(1.d0+x)
     $ - 64*Hr2(0,1)/(1.d0+x)**4
     $ + 128*Hr2(0,1)/(1.d0+x)**3
     $ - 80*Hr2(0,1)/(1.d0+x)**2
     $ + 16*Hr2(0,1)/(1.d0+x)
     $ - 112*Hr3(0,-1,0)/(1.d0+x)**5
     $ + 280*Hr3(0,-1,0)/(1.d0+x)**4
     $ - 280*Hr3(0,-1,0)/(1.d0+x)**3
     $ + 140*Hr3(0,-1,0)/(1.d0+x)**2
     $ - 28*Hr3(0,-1,0)/(1.d0+x)
     $ + 32*Hr3(0,0,-1)/(1.d0+x)**5
     $ - 80*Hr3(0,0,-1)/(1.d0+x)**4
     $ + 80*Hr3(0,0,-1)/(1.d0+x)**3
     $ - 40*Hr3(0,0,-1)/(1.d0+x)**2
     $ + 8*Hr3(0,0,-1)/(1.d0+x)
     $ + 8*Hr3(0,0,0)/(1.d0+x)**5
     $ - 20*Hr3(0,0,0)/(1.d0+x)**4
     $ + 20*Hr3(0,0,0)/(1.d0+x)**3
     $ - 10*Hr3(0,0,0)/(1.d0+x)**2
     $ + 2*Hr3(0,0,0)/(1.d0+x)
     $ - 128*Hr3(0,0,1)/(1.d0+x)**5
     $ + 320*Hr3(0,0,1)/(1.d0+x)**4
     $ - 320*Hr3(0,0,1)/(1.d0+x)**3
     $ + 160*Hr3(0,0,1)/(1.d0+x)**2
     $ - 32*Hr3(0,0,1)/(1.d0+x)
     $ + 64*Hr3(0,1,0)/(1.d0+x)**5
     $ - 160*Hr3(0,1,0)/(1.d0+x)**4
     $ + 160*Hr3(0,1,0)/(1.d0+x)**3
     $ - 80*Hr3(0,1,0)/(1.d0+x)**2
     $ + 16*Hr3(0,1,0)/(1.d0+x) )
       endif
      return
      end  

      double precision function rif12ab(x,s,HAr1,HAr2,HAr3,HAr4)
      implicit double precision (a-h,o-z) 
      dimension HAc1(-1:1),HAc2(-1:1,-1:1),HAc3(-1:1,-1:1,-1:1), 
     $          HAc4(-1:1,-1:1,-1:1,-1:1) 
      dimension HAr1(-1:1),HAr2(-1:1,-1:1),HAr3(-1:1,-1:1,-1:1), 
     $          HAr4(-1:1,-1:1,-1:1,-1:1) 
      dimension HAi1(-1:1),HAi2(-1:1,-1:1),HAi3(-1:1,-1:1,-1:1), 
     $          HAi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.4.d0) then 
      rif12ab = 0.d0
       endif
       if(s.ge.4.d0) then
      rif12ab = pi * (
     $ + 48*z2/(1.d0+x)**5
     $ - 72*z2/(1.d0+x)**4
     $ + 12*z2/(1.d0+x)**3
     $ + 10*z2/(1.d0+x)**2
     $ + 2*z2/(1.d0+x)
     $ + 64*z3/(1.d0+x)**5
     $ - 160*z3/(1.d0+x)**4
     $ + 160*z3/(1.d0+x)**3
     $ - 80*z3/(1.d0+x)**2
     $ + 16*z3/(1.d0+x)
     $ + 8.d0/(1.d0+x)**3
     $ - 12.d0/(1.d0+x)**2
     $ + 4.d0/(1.d0+x)
     $ + 24*HAr1(0)*z2/(1.d0+x)**5
     $ - 60*HAr1(0)*z2/(1.d0+x)**4
     $ + 60*HAr1(0)*z2/(1.d0+x)**3
     $ - 30*HAr1(0)*z2/(1.d0+x)**2
     $ + 6*HAr1(0)*z2/(1.d0+x)
     $ - 88*HAr1(0)/(1.d0+x)**4
     $ + 176*HAr1(0)/(1.d0+x)**3
     $ - 96*HAr1(0)/(1.d0+x)**2
     $ + 8*HAr1(0)/(1.d0+x)
     $ - 16*HAr2(-1,0)/(1.d0+x)**4
     $ + 32*HAr2(-1,0)/(1.d0+x)**3
     $ - 36*HAr2(-1,0)/(1.d0+x)**2
     $ + 20*HAr2(-1,0)/(1.d0+x)
     $ + 16*HAr2(0,-1)/(1.d0+x)**4
     $ - 32*HAr2(0,-1)/(1.d0+x)**3
     $ + 20*HAr2(0,-1)/(1.d0+x)**2
     $ - 4*HAr2(0,-1)/(1.d0+x)
     $ - 48*HAr2(0,0)/(1.d0+x)**5
     $ + 88*HAr2(0,0)/(1.d0+x)**4
     $ - 44*HAr2(0,0)/(1.d0+x)**3
     $ + 10*HAr2(0,0)/(1.d0+x)**2
     $ - 6*HAr2(0,0)/(1.d0+x)
     $ - 64*HAr2(0,1)/(1.d0+x)**4
     $ + 128*HAr2(0,1)/(1.d0+x)**3
     $ - 80*HAr2(0,1)/(1.d0+x)**2
     $ + 16*HAr2(0,1)/(1.d0+x)
     $ - 112*HAr3(0,-1,0)/(1.d0+x)**5
     $ + 280*HAr3(0,-1,0)/(1.d0+x)**4
     $ - 280*HAr3(0,-1,0)/(1.d0+x)**3
     $ + 140*HAr3(0,-1,0)/(1.d0+x)**2
     $ - 28*HAr3(0,-1,0)/(1.d0+x)
     $ + 32*HAr3(0,0,-1)/(1.d0+x)**5
     $ - 80*HAr3(0,0,-1)/(1.d0+x)**4
     $ + 80*HAr3(0,0,-1)/(1.d0+x)**3
     $ - 40*HAr3(0,0,-1)/(1.d0+x)**2
     $ + 8*HAr3(0,0,-1)/(1.d0+x)
     $ + 8*HAr3(0,0,0)/(1.d0+x)**5
     $ - 20*HAr3(0,0,0)/(1.d0+x)**4
     $ + 20*HAr3(0,0,0)/(1.d0+x)**3
     $ - 10*HAr3(0,0,0)/(1.d0+x)**2
     $ + 2*HAr3(0,0,0)/(1.d0+x)
     $ - 128*HAr3(0,0,1)/(1.d0+x)**5
     $ + 320*HAr3(0,0,1)/(1.d0+x)**4
     $ - 320*HAr3(0,0,1)/(1.d0+x)**3
     $ + 160*HAr3(0,0,1)/(1.d0+x)**2
     $ - 32*HAr3(0,0,1)/(1.d0+x)
     $ + 64*HAr3(0,1,0)/(1.d0+x)**5
     $ - 160*HAr3(0,1,0)/(1.d0+x)**4
     $ + 160*HAr3(0,1,0)/(1.d0+x)**3
     $ - 80*HAr3(0,1,0)/(1.d0+x)**2
     $ + 16*HAr3(0,1,0)/(1.d0+x) )
       endif
      return
      end

c     F_{1/2}**(2l,b):
      double precision function ref12b(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.4.d0) then 
        xe = Dsqrt(s)/Dsqrt(4.d0-s)
	phy = DATAN(xe)
      ref12b =  phy/Dsqrt(s*(4.d0-s)) * (
     $ - 12.d0
     $ + 48.d0/s )
     $ + phy**2 * (
     $ - 96.d0/s**2
     $ + 12.d0/s )
     $ + 12.d0/s
       endif
       if(s.ge.4.d0) then
      ref12b = 
     $ - 144*z2/(1.d0+x)**4
     $ + 288*z2/(1.d0+x)**3
     $ - 162*z2/(1.d0+x)**2
     $ + 18*z2/(1.d0+x)
     $ - 12.d0/(1.d0+x)**2
     $ + 12.d0/(1.d0+x)
     $ + 12*Hr1(0)/(1.d0+x)**3
     $ - 18*Hr1(0)/(1.d0+x)**2
     $ + 6*Hr1(0)/(1.d0+x)
     $ + 48*Hr2(0,0)/(1.d0+x)**4
     $ - 96*Hr2(0,0)/(1.d0+x)**3
     $ + 54*Hr2(0,0)/(1.d0+x)**2
     $ - 6*Hr2(0,0)/(1.d0+x)
       endif
      return
      end  

      double precision function ref12bt(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.4.d0) then 
        xe = Dsqrt(s)/Dsqrt(4.d0-s)
	phy = DATAN(xe)
      ref12bt =  phy/Dsqrt(s*(4.d0-s)) * (
     $ - 12.d0
     $ + 48.d0/s )
     $ + phy**2 * (
     $ - 96.d0/s**2
     $ + 12.d0/s )
     $ + 12.d0/s
       endif
       if(s.ge.4.d0) then
      ref12bt = 
     $ - 144*z2/(1.d0+x)**4
     $ + 288*z2/(1.d0+x)**3
     $ - 162*z2/(1.d0+x)**2
     $ + 18*z2/(1.d0+x)
     $ - 12.d0/(1.d0+x)**2
     $ + 12.d0/(1.d0+x)
     $ + 12*Hr1(0)/(1.d0+x)**3
     $ - 18*Hr1(0)/(1.d0+x)**2
     $ + 6*Hr1(0)/(1.d0+x)
     $ + 48*Hr2(0,0)/(1.d0+x)**4
     $ - 96*Hr2(0,0)/(1.d0+x)**3
     $ + 54*Hr2(0,0)/(1.d0+x)**2
     $ - 6*Hr2(0,0)/(1.d0+x)
       endif
      return
      end  

      double precision function ref12bb(x,s,HAr1,HAr2,HAr3,HAr4)
      implicit double precision (a-h,o-z) 
      dimension HAc1(-1:1),HAc2(-1:1,-1:1),HAc3(-1:1,-1:1,-1:1), 
     $          HAc4(-1:1,-1:1,-1:1,-1:1) 
      dimension HAr1(-1:1),HAr2(-1:1,-1:1),HAr3(-1:1,-1:1,-1:1), 
     $          HAr4(-1:1,-1:1,-1:1,-1:1) 
      dimension HAi1(-1:1),HAi2(-1:1,-1:1),HAi3(-1:1,-1:1,-1:1), 
     $          HAi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.4.d0) then 
        xe = Dsqrt(s)/Dsqrt(4.d0-s)
	phy = DATAN(xe)
      ref12bb =  phy/Dsqrt(s*(4.d0-s)) * (
     $ - 12.d0
     $ + 48.d0/s )
     $ + phy**2 * (
     $ - 96.d0/s**2
     $ + 12.d0/s )
     $ + 12.d0/s
       endif
       if(s.ge.4.d0) then
      ref12bb = 
     $ - 144*z2/(1.d0+x)**4
     $ + 288*z2/(1.d0+x)**3
     $ - 162*z2/(1.d0+x)**2
     $ + 18*z2/(1.d0+x)
     $ - 12.d0/(1.d0+x)**2
     $ + 12.d0/(1.d0+x)
     $ + 12*HAr1(0)/(1.d0+x)**3
     $ - 18*HAr1(0)/(1.d0+x)**2
     $ + 6*HAr1(0)/(1.d0+x)
     $ + 48*HAr2(0,0)/(1.d0+x)**4
     $ - 96*HAr2(0,0)/(1.d0+x)**3
     $ + 54*HAr2(0,0)/(1.d0+x)**2
     $ - 6*HAr2(0,0)/(1.d0+x)
       endif
      return
      end  

      double precision function rif12b(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.4.d0) then 
      rif12b = 0.d0
       endif
       if(s.ge.4.d0) then
      rif12b = pi * (
     $ + 12.d0/(1.d0+x)**3
     $ - 18.d0/(1.d0+x)**2
     $ + 6.d0/(1.d0+x)
     $ + 48*Hr1(0)/(1.d0+x)**4
     $ - 96*Hr1(0)/(1.d0+x)**3
     $ + 54*Hr1(0)/(1.d0+x)**2
     $ - 6*Hr1(0)/(1.d0+x) )
       endif
      return
      end    

      double precision function rif12bt(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.4.d0) then 
      rif12bt = 0.d0
       endif
       if(s.ge.4.d0) then
      rif12bt = pi * (
     $ + 12.d0/(1.d0+x)**3
     $ - 18.d0/(1.d0+x)**2
     $ + 6.d0/(1.d0+x)
     $ + 48*Hr1(0)/(1.d0+x)**4
     $ - 96*Hr1(0)/(1.d0+x)**3
     $ + 54*Hr1(0)/(1.d0+x)**2
     $ - 6*Hr1(0)/(1.d0+x) )
       endif
      return
      end  

      double precision function rif12bb(x,s,HAr1,HAr2,HAr3,HAr4)
      implicit double precision (a-h,o-z) 
      dimension HAc1(-1:1),HAc2(-1:1,-1:1),HAc3(-1:1,-1:1,-1:1), 
     $          HAc4(-1:1,-1:1,-1:1,-1:1) 
      dimension HAr1(-1:1),HAr2(-1:1,-1:1),HAr3(-1:1,-1:1,-1:1), 
     $          HAr4(-1:1,-1:1,-1:1,-1:1) 
      dimension HAi1(-1:1),HAi2(-1:1,-1:1),HAi3(-1:1,-1:1,-1:1), 
     $          HAi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.4.d0) then 
      rif12bb = 0.d0
       endif
       if(s.ge.4.d0) then
      rif12bb = pi * (
     $ + 12.d0/(1.d0+x)**3
     $ - 18.d0/(1.d0+x)**2
     $ + 6.d0/(1.d0+x)
     $ + 48*HAr1(0)/(1.d0+x)**4
     $ - 96*HAr1(0)/(1.d0+x)**3
     $ + 54*HAr1(0)/(1.d0+x)**2
     $ - 6*HAr1(0)/(1.d0+x) )
       endif
      return
      end  

c     G_{1/2}**(2l,CA):
      double precision function reg12(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 )  
      parameter ( A  = 2.7605366106449011d0 )   
      parameter ( B  = 2.5838563897912388d0 )  
       if(s.lt.3.4d0) then  
      var = - Dlog(1.d0-s/4.d0)
      reg12 = 
     $ - 1.6666666666666666667d0 
     $ - 1.0740740740740740741d-1*var 
     $ + 5.1587301587301587302d-2*var**2 
     $ - 4.7372134038800705467d-3*var**3 
     $ - 1.9440168884613329058d-3*var**4 
     $ + 3.6183154210365094719d-4*var**5 
     $ + 5.8727833293215001454d-5*var**6 
     $ - 1.6377009398662504349d-5*var**7 
     $ - 1.5843238692758617688d-6*var**8 
     $ + 6.1314282560652485872d-7*var**9 
     $ + 4.0140135458767901545d-8*var**10 
     $ - 2.0788967630680028372d-8*var**11 
     $ - 9.7997651161340567699d-10*var**12 
     $ + 6.6355792353165564390d-10*var**13 
     $ + 2.3390999684338577844d-11*var**14 
     $ - 2.0339442131905860072d-11*var**15
     $ - 5.5111970434973154948d-13*var**16
     $ + 6.0559435370803137549d-13*var**17
     $ + 1.2918245981593723397d-14*var**18
     $ - 1.7640659318742736660d-14*var**19
     $ - 3.0354071583416123267d-16*var**20
       endif
       if(s.ge.3.4d0.AND.s.lt.4.d0) then  
       var = - Dlog(s/4.d0)
      reg12 = 
     $ - 1.6830553486007317270d0
     $ - 7.0733604921281959269d0*var 
     $ - 25.477287562661910953d0*var**2 
     $ - 25.526577487410146811d0*var**3 
     $ - 13.418507431344332592d0*var**4 
     $ - 4.0447897125278864531d0*var**5 
     $ - 3.5280276684791555589d-1*var**6 
     $ + 3.5826141442113880913d-1*var**7 
     $ + 2.5504996824599651363d-1*var**8 
     $ + A*(
     $ + 2.0d0*var 
     $ + 3.0d0*var**2 
     $ + 2.3333333333333333333d0*var**3 
     $ + 1.25d0*var**4 
     $ + 5.1666666666666666667d-1*var**5 
     $ + 1.75d-1*var**6 
     $ + 5.0396825396825396825d-2*var**7 
     $ + 1.2648809523809523810d-2*var**8 )
     $ + Dsqrt(var)*(
     $ + 17.039797178978297356d0*var 
     $ + 31.177670627014537352d0*var**2 
     $ + 25.889794435097049232d0*var**3 
     $ + 13.429558188920612544d0*var**4 
     $ + 4.8386073981040926853d0*var**5 
     $ + 1.2083041534870725542d0*var**6 
     $ + 1.6281394681401991385d-1*var**7 
     $ + 2.3523894813323072669d-2*var**8  )
     $ - Dsqrt(var)*B*(
     $ + 4.0d0*var 
     $ + 5.6666666666666666667d0*var**2 
     $ + 4.175d0*var**3 
     $ + 2.1250992063492063492d0*var**4 
     $ + 8.3691096230158730159d-1*var**5 
     $ + 2.7074948705808080808d-1*var**6 
     $ + 7.4631464184686840937d-2*var**7 
     $ + 1.7963631654020651416d-2*var**8  )
     $ - Dsqrt(var)*Dlog(var)*(
     $ + 4.1887902047863909846d0*var**2 
     $ + 6.0737457969402669277d0*var**3 
     $ + 4.5765026314794170549d0*var**4 
     $ + 2.3797772128234587868d0*var**5 
     $ + 9.5633453450718921843d-1*var**6 
     $ + 3.1533432479956951807d-1*var**7 
     $ + 8.8494550144802829329d-2*var**8  )
       endif
       if(s.ge.4.d0) then
      reg12 = 
     $ + 24*z2/(1.d0+x)**4
     $ - 48*z2/(1.d0+x)**3
     $ + 30*z2/(1.d0+x)**2
     $ - 6*z2/(1.d0+x)
     $ + 52.d0/5*z2**2/(1.d0+x)**4
     $ - 104.d0/5*z2**2/(1.d0+x)**3
     $ + 13*z2**2/(1.d0+x)**2
     $ - 13.d0/5*z2**2/(1.d0+x)
     $ - 64*z3/(1.d0+x)**4
     $ + 128*z3/(1.d0+x)**3
     $ - 76*z3/(1.d0+x)**2
     $ + 12*z3/(1.d0+x)
     $ + 12.d0/(1.d0+x)**2
     $ - 12.d0/(1.d0+x)
     $ + 192*Hr1(-1)*z2/(1.d0+x)**4
     $ - 384*Hr1(-1)*z2/(1.d0+x)**3
     $ + 228*Hr1(-1)*z2/(1.d0+x)**2
     $ - 36*Hr1(-1)*z2/(1.d0+x)
     $ + 96*Hr1(-1)*z3/(1.d0+x)**4
     $ - 192*Hr1(-1)*z3/(1.d0+x)**3
     $ + 120*Hr1(-1)*z3/(1.d0+x)**2
     $ - 24*Hr1(-1)*z3/(1.d0+x)
     $ + 144*Hr1(0)*z2/(1.d0+x)**5
     $ - 456*Hr1(0)*z2/(1.d0+x)**4
     $ + 516*Hr1(0)*z2/(1.d0+x)**3
     $ - 240*Hr1(0)*z2/(1.d0+x)**2
     $ + 36*Hr1(0)*z2/(1.d0+x)
     $ - 48*Hr1(0)*z3/(1.d0+x)**4
     $ + 96*Hr1(0)*z3/(1.d0+x)**3
     $ - 60*Hr1(0)*z3/(1.d0+x)**2
     $ + 12*Hr1(0)*z3/(1.d0+x)
     $ + 128*Hr2(-1,0)*z2/(1.d0+x)**4
     $ - 256*Hr2(-1,0)*z2/(1.d0+x)**3
     $ + 160*Hr2(-1,0)*z2/(1.d0+x)**2
     $ - 32*Hr2(-1,0)*z2/(1.d0+x)
     $ + 8*Hr2(0,0)*z2/(1.d0+x)**4
     $ - 16*Hr2(0,0)*z2/(1.d0+x)**3
     $ + 10*Hr2(0,0)*z2/(1.d0+x)**2
     $ - 2*Hr2(0,0)*z2/(1.d0+x)
     $ - 8*Hr2(0,0)/(1.d0+x)**4
     $ + 16*Hr2(0,0)/(1.d0+x)**3
     $ - 10*Hr2(0,0)/(1.d0+x)**2
     $ + 2*Hr2(0,0)/(1.d0+x)
     $ - 64*Hr3(-1,0,0)/(1.d0+x)**4
     $ + 128*Hr3(-1,0,0)/(1.d0+x)**3
     $ - 76*Hr3(-1,0,0)/(1.d0+x)**2
     $ + 12*Hr3(-1,0,0)/(1.d0+x)
     $ - 48*Hr3(0,0,0)/(1.d0+x)**5
     $ + 152*Hr3(0,0,0)/(1.d0+x)**4
     $ - 172*Hr3(0,0,0)/(1.d0+x)**3
     $ + 80*Hr3(0,0,0)/(1.d0+x)**2
     $ - 12*Hr3(0,0,0)/(1.d0+x)
     $ - 32*Hr4(-1,0,0,0)/(1.d0+x)**4
     $ + 64*Hr4(-1,0,0,0)/(1.d0+x)**3
     $ - 40*Hr4(-1,0,0,0)/(1.d0+x)**2
     $ + 8*Hr4(-1,0,0,0)/(1.d0+x)
     $ - 64*Hr4(-1,0,1,0)/(1.d0+x)**4
     $ + 128*Hr4(-1,0,1,0)/(1.d0+x)**3
     $ - 80*Hr4(-1,0,1,0)/(1.d0+x)**2
     $ + 16*Hr4(-1,0,1,0)/(1.d0+x)
     $ - 8*Hr4(0,0,0,0)/(1.d0+x)**4
     $ + 16*Hr4(0,0,0,0)/(1.d0+x)**3
     $ - 10*Hr4(0,0,0,0)/(1.d0+x)**2
     $ + 2*Hr4(0,0,0,0)/(1.d0+x)
     $ + 32*Hr4(0,0,1,0)/(1.d0+x)**4
     $ - 64*Hr4(0,0,1,0)/(1.d0+x)**3
     $ + 40*Hr4(0,0,1,0)/(1.d0+x)**2
     $ - 8*Hr4(0,0,1,0)/(1.d0+x)
       endif
      return
      end  

      double precision function reg12t(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 )  
      parameter ( A  = 2.7605366106449011d0 )   
      parameter ( B  = 2.5838563897912388d0 )  
       if(s.lt.3.4d0) then  
      var = - Dlog(1.d0-s/4.d0)
      reg12t = 
     $ - 1.6666666666666666667d0 
     $ - 1.0740740740740740741d-1*var 
     $ + 5.1587301587301587302d-2*var**2 
     $ - 4.7372134038800705467d-3*var**3 
     $ - 1.9440168884613329058d-3*var**4 
     $ + 3.6183154210365094719d-4*var**5 
     $ + 5.8727833293215001454d-5*var**6 
     $ - 1.6377009398662504349d-5*var**7 
     $ - 1.5843238692758617688d-6*var**8 
     $ + 6.1314282560652485872d-7*var**9 
     $ + 4.0140135458767901545d-8*var**10 
     $ - 2.0788967630680028372d-8*var**11 
     $ - 9.7997651161340567699d-10*var**12 
     $ + 6.6355792353165564390d-10*var**13 
     $ + 2.3390999684338577844d-11*var**14 
     $ - 2.0339442131905860072d-11*var**15
     $ - 5.5111970434973154948d-13*var**16
     $ + 6.0559435370803137549d-13*var**17
     $ + 1.2918245981593723397d-14*var**18
     $ - 1.7640659318742736660d-14*var**19
     $ - 3.0354071583416123267d-16*var**20
       endif
       if(s.ge.3.4d0.AND.s.lt.4.d0) then  
       var = - Dlog(s/4.d0)
      reg12t = 
     $ - 1.6830553486007317270d0
     $ - 7.0733604921281959269d0*var 
     $ - 25.477287562661910953d0*var**2 
     $ - 25.526577487410146811d0*var**3 
     $ - 13.418507431344332592d0*var**4 
     $ - 4.0447897125278864531d0*var**5 
     $ - 3.5280276684791555589d-1*var**6 
     $ + 3.5826141442113880913d-1*var**7 
     $ + 2.5504996824599651363d-1*var**8 
     $ + A*(
     $ + 2.0d0*var 
     $ + 3.0d0*var**2 
     $ + 2.3333333333333333333d0*var**3 
     $ + 1.25d0*var**4 
     $ + 5.1666666666666666667d-1*var**5 
     $ + 1.75d-1*var**6 
     $ + 5.0396825396825396825d-2*var**7 
     $ + 1.2648809523809523810d-2*var**8 )
     $ + Dsqrt(var)*(
     $ + 17.039797178978297356d0*var 
     $ + 31.177670627014537352d0*var**2 
     $ + 25.889794435097049232d0*var**3 
     $ + 13.429558188920612544d0*var**4 
     $ + 4.8386073981040926853d0*var**5 
     $ + 1.2083041534870725542d0*var**6 
     $ + 1.6281394681401991385d-1*var**7 
     $ + 2.3523894813323072669d-2*var**8  )
     $ - Dsqrt(var)*B*(
     $ + 4.0d0*var 
     $ + 5.6666666666666666667d0*var**2 
     $ + 4.175d0*var**3 
     $ + 2.1250992063492063492d0*var**4 
     $ + 8.3691096230158730159d-1*var**5 
     $ + 2.7074948705808080808d-1*var**6 
     $ + 7.4631464184686840937d-2*var**7 
     $ + 1.7963631654020651416d-2*var**8  )
     $ - Dsqrt(var)*Dlog(var)*(
     $ + 4.1887902047863909846d0*var**2 
     $ + 6.0737457969402669277d0*var**3 
     $ + 4.5765026314794170549d0*var**4 
     $ + 2.3797772128234587868d0*var**5 
     $ + 9.5633453450718921843d-1*var**6 
     $ + 3.1533432479956951807d-1*var**7 
     $ + 8.8494550144802829329d-2*var**8  )
       endif
       if(s.ge.4.d0) then
      reg12t = 
     $ + 24*z2/(1.d0+x)**4
     $ - 48*z2/(1.d0+x)**3
     $ + 30*z2/(1.d0+x)**2
     $ - 6*z2/(1.d0+x)
     $ + 52.d0/5*z2**2/(1.d0+x)**4
     $ - 104.d0/5*z2**2/(1.d0+x)**3
     $ + 13*z2**2/(1.d0+x)**2
     $ - 13.d0/5*z2**2/(1.d0+x)
     $ - 64*z3/(1.d0+x)**4
     $ + 128*z3/(1.d0+x)**3
     $ - 76*z3/(1.d0+x)**2
     $ + 12*z3/(1.d0+x)
     $ + 12.d0/(1.d0+x)**2
     $ - 12.d0/(1.d0+x)
     $ + 192*Hr1(-1)*z2/(1.d0+x)**4
     $ - 384*Hr1(-1)*z2/(1.d0+x)**3
     $ + 228*Hr1(-1)*z2/(1.d0+x)**2
     $ - 36*Hr1(-1)*z2/(1.d0+x)
     $ + 96*Hr1(-1)*z3/(1.d0+x)**4
     $ - 192*Hr1(-1)*z3/(1.d0+x)**3
     $ + 120*Hr1(-1)*z3/(1.d0+x)**2
     $ - 24*Hr1(-1)*z3/(1.d0+x)
     $ + 144*Hr1(0)*z2/(1.d0+x)**5
     $ - 456*Hr1(0)*z2/(1.d0+x)**4
     $ + 516*Hr1(0)*z2/(1.d0+x)**3
     $ - 240*Hr1(0)*z2/(1.d0+x)**2
     $ + 36*Hr1(0)*z2/(1.d0+x)
     $ - 48*Hr1(0)*z3/(1.d0+x)**4
     $ + 96*Hr1(0)*z3/(1.d0+x)**3
     $ - 60*Hr1(0)*z3/(1.d0+x)**2
     $ + 12*Hr1(0)*z3/(1.d0+x)
     $ + 128*Hr2(-1,0)*z2/(1.d0+x)**4
     $ - 256*Hr2(-1,0)*z2/(1.d0+x)**3
     $ + 160*Hr2(-1,0)*z2/(1.d0+x)**2
     $ - 32*Hr2(-1,0)*z2/(1.d0+x)
     $ + 8*Hr2(0,0)*z2/(1.d0+x)**4
     $ - 16*Hr2(0,0)*z2/(1.d0+x)**3
     $ + 10*Hr2(0,0)*z2/(1.d0+x)**2
     $ - 2*Hr2(0,0)*z2/(1.d0+x)
     $ - 8*Hr2(0,0)/(1.d0+x)**4
     $ + 16*Hr2(0,0)/(1.d0+x)**3
     $ - 10*Hr2(0,0)/(1.d0+x)**2
     $ + 2*Hr2(0,0)/(1.d0+x)
     $ - 64*Hr3(-1,0,0)/(1.d0+x)**4
     $ + 128*Hr3(-1,0,0)/(1.d0+x)**3
     $ - 76*Hr3(-1,0,0)/(1.d0+x)**2
     $ + 12*Hr3(-1,0,0)/(1.d0+x)
     $ - 48*Hr3(0,0,0)/(1.d0+x)**5
     $ + 152*Hr3(0,0,0)/(1.d0+x)**4
     $ - 172*Hr3(0,0,0)/(1.d0+x)**3
     $ + 80*Hr3(0,0,0)/(1.d0+x)**2
     $ - 12*Hr3(0,0,0)/(1.d0+x)
     $ - 32*Hr4(-1,0,0,0)/(1.d0+x)**4
     $ + 64*Hr4(-1,0,0,0)/(1.d0+x)**3
     $ - 40*Hr4(-1,0,0,0)/(1.d0+x)**2
     $ + 8*Hr4(-1,0,0,0)/(1.d0+x)
     $ - 64*Hr4(-1,0,1,0)/(1.d0+x)**4
     $ + 128*Hr4(-1,0,1,0)/(1.d0+x)**3
     $ - 80*Hr4(-1,0,1,0)/(1.d0+x)**2
     $ + 16*Hr4(-1,0,1,0)/(1.d0+x)
     $ - 8*Hr4(0,0,0,0)/(1.d0+x)**4
     $ + 16*Hr4(0,0,0,0)/(1.d0+x)**3
     $ - 10*Hr4(0,0,0,0)/(1.d0+x)**2
     $ + 2*Hr4(0,0,0,0)/(1.d0+x)
     $ + 32*Hr4(0,0,1,0)/(1.d0+x)**4
     $ - 64*Hr4(0,0,1,0)/(1.d0+x)**3
     $ + 40*Hr4(0,0,1,0)/(1.d0+x)**2
     $ - 8*Hr4(0,0,1,0)/(1.d0+x)
       endif
      return
      end  

      double precision function reg12b(x,s,HAr1,HAr2,HAr3,HAr4)
      implicit double precision (a-h,o-z) 
      dimension HAc1(-1:1),HAc2(-1:1,-1:1),HAc3(-1:1,-1:1,-1:1), 
     $          HAc4(-1:1,-1:1,-1:1,-1:1) 
      dimension HAr1(-1:1),HAr2(-1:1,-1:1),HAr3(-1:1,-1:1,-1:1), 
     $          HAr4(-1:1,-1:1,-1:1,-1:1) 
      dimension HAi1(-1:1),HAi2(-1:1,-1:1),HAi3(-1:1,-1:1,-1:1), 
     $          HAi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 )  
      parameter ( A  = 2.7605366106449011d0 )   
      parameter ( B  = 2.5838563897912388d0 )  
       if(s.lt.3.4d0) then  
      var = - Dlog(1.d0-s/4.d0)
      reg12b = 
     $ - 1.6666666666666666667d0 
     $ - 1.0740740740740740741d-1*var 
     $ + 5.1587301587301587302d-2*var**2 
     $ - 4.7372134038800705467d-3*var**3 
     $ - 1.9440168884613329058d-3*var**4 
     $ + 3.6183154210365094719d-4*var**5 
     $ + 5.8727833293215001454d-5*var**6 
     $ - 1.6377009398662504349d-5*var**7 
     $ - 1.5843238692758617688d-6*var**8 
     $ + 6.1314282560652485872d-7*var**9 
     $ + 4.0140135458767901545d-8*var**10 
     $ - 2.0788967630680028372d-8*var**11 
     $ - 9.7997651161340567699d-10*var**12 
     $ + 6.6355792353165564390d-10*var**13 
     $ + 2.3390999684338577844d-11*var**14 
     $ - 2.0339442131905860072d-11*var**15
     $ - 5.5111970434973154948d-13*var**16
     $ + 6.0559435370803137549d-13*var**17
     $ + 1.2918245981593723397d-14*var**18
     $ - 1.7640659318742736660d-14*var**19
     $ - 3.0354071583416123267d-16*var**20
       endif
       if(s.ge.3.4d0.AND.s.lt.4.d0) then  
       var = - Dlog(s/4.d0)
      reg12b = 
     $ - 1.6830553486007317270d0
     $ - 7.0733604921281959269d0*var 
     $ - 25.477287562661910953d0*var**2 
     $ - 25.526577487410146811d0*var**3 
     $ - 13.418507431344332592d0*var**4 
     $ - 4.0447897125278864531d0*var**5 
     $ - 3.5280276684791555589d-1*var**6 
     $ + 3.5826141442113880913d-1*var**7 
     $ + 2.5504996824599651363d-1*var**8 
     $ + A*(
     $ + 2.0d0*var 
     $ + 3.0d0*var**2 
     $ + 2.3333333333333333333d0*var**3 
     $ + 1.25d0*var**4 
     $ + 5.1666666666666666667d-1*var**5 
     $ + 1.75d-1*var**6 
     $ + 5.0396825396825396825d-2*var**7 
     $ + 1.2648809523809523810d-2*var**8 )
     $ + Dsqrt(var)*(
     $ + 17.039797178978297356d0*var 
     $ + 31.177670627014537352d0*var**2 
     $ + 25.889794435097049232d0*var**3 
     $ + 13.429558188920612544d0*var**4 
     $ + 4.8386073981040926853d0*var**5 
     $ + 1.2083041534870725542d0*var**6 
     $ + 1.6281394681401991385d-1*var**7 
     $ + 2.3523894813323072669d-2*var**8  )
     $ - Dsqrt(var)*B*(
     $ + 4.0d0*var 
     $ + 5.6666666666666666667d0*var**2 
     $ + 4.175d0*var**3 
     $ + 2.1250992063492063492d0*var**4 
     $ + 8.3691096230158730159d-1*var**5 
     $ + 2.7074948705808080808d-1*var**6 
     $ + 7.4631464184686840937d-2*var**7 
     $ + 1.7963631654020651416d-2*var**8  )
     $ - Dsqrt(var)*Dlog(var)*(
     $ + 4.1887902047863909846d0*var**2 
     $ + 6.0737457969402669277d0*var**3 
     $ + 4.5765026314794170549d0*var**4 
     $ + 2.3797772128234587868d0*var**5 
     $ + 9.5633453450718921843d-1*var**6 
     $ + 3.1533432479956951807d-1*var**7 
     $ + 8.8494550144802829329d-2*var**8  )
       endif
       if(s.ge.4.d0) then
      reg12b = 
     $ + 24*z2/(1.d0+x)**4
     $ - 48*z2/(1.d0+x)**3
     $ + 30*z2/(1.d0+x)**2
     $ - 6*z2/(1.d0+x)
     $ + 52.d0/5*z2**2/(1.d0+x)**4
     $ - 104.d0/5*z2**2/(1.d0+x)**3
     $ + 13*z2**2/(1.d0+x)**2
     $ - 13.d0/5*z2**2/(1.d0+x)
     $ - 64*z3/(1.d0+x)**4
     $ + 128*z3/(1.d0+x)**3
     $ - 76*z3/(1.d0+x)**2
     $ + 12*z3/(1.d0+x)
     $ + 12.d0/(1.d0+x)**2
     $ - 12.d0/(1.d0+x)
     $ + 192*HAr1(-1)*z2/(1.d0+x)**4
     $ - 384*HAr1(-1)*z2/(1.d0+x)**3
     $ + 228*HAr1(-1)*z2/(1.d0+x)**2
     $ - 36*HAr1(-1)*z2/(1.d0+x)
     $ + 96*HAr1(-1)*z3/(1.d0+x)**4
     $ - 192*HAr1(-1)*z3/(1.d0+x)**3
     $ + 120*HAr1(-1)*z3/(1.d0+x)**2
     $ - 24*HAr1(-1)*z3/(1.d0+x)
     $ + 144*HAr1(0)*z2/(1.d0+x)**5
     $ - 456*HAr1(0)*z2/(1.d0+x)**4
     $ + 516*HAr1(0)*z2/(1.d0+x)**3
     $ - 240*HAr1(0)*z2/(1.d0+x)**2
     $ + 36*HAr1(0)*z2/(1.d0+x)
     $ - 48*HAr1(0)*z3/(1.d0+x)**4
     $ + 96*HAr1(0)*z3/(1.d0+x)**3
     $ - 60*HAr1(0)*z3/(1.d0+x)**2
     $ + 12*HAr1(0)*z3/(1.d0+x)
     $ + 128*HAr2(-1,0)*z2/(1.d0+x)**4
     $ - 256*HAr2(-1,0)*z2/(1.d0+x)**3
     $ + 160*HAr2(-1,0)*z2/(1.d0+x)**2
     $ - 32*HAr2(-1,0)*z2/(1.d0+x)
     $ + 8*HAr2(0,0)*z2/(1.d0+x)**4
     $ - 16*HAr2(0,0)*z2/(1.d0+x)**3
     $ + 10*HAr2(0,0)*z2/(1.d0+x)**2
     $ - 2*HAr2(0,0)*z2/(1.d0+x)
     $ - 8*HAr2(0,0)/(1.d0+x)**4
     $ + 16*HAr2(0,0)/(1.d0+x)**3
     $ - 10*HAr2(0,0)/(1.d0+x)**2
     $ + 2*HAr2(0,0)/(1.d0+x)
     $ - 64*HAr3(-1,0,0)/(1.d0+x)**4
     $ + 128*HAr3(-1,0,0)/(1.d0+x)**3
     $ - 76*HAr3(-1,0,0)/(1.d0+x)**2
     $ + 12*HAr3(-1,0,0)/(1.d0+x)
     $ - 48*HAr3(0,0,0)/(1.d0+x)**5
     $ + 152*HAr3(0,0,0)/(1.d0+x)**4
     $ - 172*HAr3(0,0,0)/(1.d0+x)**3
     $ + 80*HAr3(0,0,0)/(1.d0+x)**2
     $ - 12*HAr3(0,0,0)/(1.d0+x)
     $ - 32*HAr4(-1,0,0,0)/(1.d0+x)**4
     $ + 64*HAr4(-1,0,0,0)/(1.d0+x)**3
     $ - 40*HAr4(-1,0,0,0)/(1.d0+x)**2
     $ + 8*HAr4(-1,0,0,0)/(1.d0+x)
     $ - 64*HAr4(-1,0,1,0)/(1.d0+x)**4
     $ + 128*HAr4(-1,0,1,0)/(1.d0+x)**3
     $ - 80*HAr4(-1,0,1,0)/(1.d0+x)**2
     $ + 16*HAr4(-1,0,1,0)/(1.d0+x)
     $ - 8*HAr4(0,0,0,0)/(1.d0+x)**4
     $ + 16*HAr4(0,0,0,0)/(1.d0+x)**3
     $ - 10*HAr4(0,0,0,0)/(1.d0+x)**2
     $ + 2*HAr4(0,0,0,0)/(1.d0+x)
     $ + 32*HAr4(0,0,1,0)/(1.d0+x)**4
     $ - 64*HAr4(0,0,1,0)/(1.d0+x)**3
     $ + 40*HAr4(0,0,1,0)/(1.d0+x)**2
     $ - 8*HAr4(0,0,1,0)/(1.d0+x)
       endif
      return
      end  

      double precision function rig12(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.4.d0) then 
      rig12 = 0.d0
       endif
       if(s.ge.4.d0) then
      rig12 = pi * (
     $ + 48*z2/(1.d0+x)**5
     $ - 152*z2/(1.d0+x)**4
     $ + 172*z2/(1.d0+x)**3
     $ - 80*z2/(1.d0+x)**2
     $ + 12*z2/(1.d0+x)
     $ - 48*z3/(1.d0+x)**4
     $ + 96*z3/(1.d0+x)**3
     $ - 60*z3/(1.d0+x)**2
     $ + 12*z3/(1.d0+x)
     $ + 64*Hr1(-1)*z2/(1.d0+x)**4
     $ - 128*Hr1(-1)*z2/(1.d0+x)**3
     $ + 80*Hr1(-1)*z2/(1.d0+x)**2
     $ - 16*Hr1(-1)*z2/(1.d0+x)
     $ - 8*Hr1(0)*z2/(1.d0+x)**4
     $ + 16*Hr1(0)*z2/(1.d0+x)**3
     $ - 10*Hr1(0)*z2/(1.d0+x)**2
     $ + 2*Hr1(0)*z2/(1.d0+x)
     $ - 8*Hr1(0)/(1.d0+x)**4
     $ + 16*Hr1(0)/(1.d0+x)**3
     $ - 10*Hr1(0)/(1.d0+x)**2
     $ + 2*Hr1(0)/(1.d0+x)
     $ - 64*Hr2(-1,0)/(1.d0+x)**4
     $ + 128*Hr2(-1,0)/(1.d0+x)**3
     $ - 76*Hr2(-1,0)/(1.d0+x)**2
     $ + 12*Hr2(-1,0)/(1.d0+x)
     $ - 48*Hr2(0,0)/(1.d0+x)**5
     $ + 152*Hr2(0,0)/(1.d0+x)**4
     $ - 172*Hr2(0,0)/(1.d0+x)**3
     $ + 80*Hr2(0,0)/(1.d0+x)**2
     $ - 12*Hr2(0,0)/(1.d0+x)
     $ - 32*Hr3(-1,0,0)/(1.d0+x)**4
     $ + 64*Hr3(-1,0,0)/(1.d0+x)**3
     $ - 40*Hr3(-1,0,0)/(1.d0+x)**2
     $ + 8*Hr3(-1,0,0)/(1.d0+x)
     $ - 64*Hr3(-1,0,1)/(1.d0+x)**4
     $ + 128*Hr3(-1,0,1)/(1.d0+x)**3
     $ - 80*Hr3(-1,0,1)/(1.d0+x)**2
     $ + 16*Hr3(-1,0,1)/(1.d0+x)
     $ - 8*Hr3(0,0,0)/(1.d0+x)**4
     $ + 16*Hr3(0,0,0)/(1.d0+x)**3
     $ - 10*Hr3(0,0,0)/(1.d0+x)**2
     $ + 2*Hr3(0,0,0)/(1.d0+x)
     $ + 32*Hr3(0,0,1)/(1.d0+x)**4
     $ - 64*Hr3(0,0,1)/(1.d0+x)**3
     $ + 40*Hr3(0,0,1)/(1.d0+x)**2
     $ - 8*Hr3(0,0,1)/(1.d0+x) )
       endif
      return
      end    

      double precision function rig12t(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.4.d0) then 
      rig12t = 0.d0
       endif
       if(s.ge.4.d0) then
      rig12t = pi * (
     $ + 48*z2/(1.d0+x)**5
     $ - 152*z2/(1.d0+x)**4
     $ + 172*z2/(1.d0+x)**3
     $ - 80*z2/(1.d0+x)**2
     $ + 12*z2/(1.d0+x)
     $ - 48*z3/(1.d0+x)**4
     $ + 96*z3/(1.d0+x)**3
     $ - 60*z3/(1.d0+x)**2
     $ + 12*z3/(1.d0+x)
     $ + 64*Hr1(-1)*z2/(1.d0+x)**4
     $ - 128*Hr1(-1)*z2/(1.d0+x)**3
     $ + 80*Hr1(-1)*z2/(1.d0+x)**2
     $ - 16*Hr1(-1)*z2/(1.d0+x)
     $ - 8*Hr1(0)*z2/(1.d0+x)**4
     $ + 16*Hr1(0)*z2/(1.d0+x)**3
     $ - 10*Hr1(0)*z2/(1.d0+x)**2
     $ + 2*Hr1(0)*z2/(1.d0+x)
     $ - 8*Hr1(0)/(1.d0+x)**4
     $ + 16*Hr1(0)/(1.d0+x)**3
     $ - 10*Hr1(0)/(1.d0+x)**2
     $ + 2*Hr1(0)/(1.d0+x)
     $ - 64*Hr2(-1,0)/(1.d0+x)**4
     $ + 128*Hr2(-1,0)/(1.d0+x)**3
     $ - 76*Hr2(-1,0)/(1.d0+x)**2
     $ + 12*Hr2(-1,0)/(1.d0+x)
     $ - 48*Hr2(0,0)/(1.d0+x)**5
     $ + 152*Hr2(0,0)/(1.d0+x)**4
     $ - 172*Hr2(0,0)/(1.d0+x)**3
     $ + 80*Hr2(0,0)/(1.d0+x)**2
     $ - 12*Hr2(0,0)/(1.d0+x)
     $ - 32*Hr3(-1,0,0)/(1.d0+x)**4
     $ + 64*Hr3(-1,0,0)/(1.d0+x)**3
     $ - 40*Hr3(-1,0,0)/(1.d0+x)**2
     $ + 8*Hr3(-1,0,0)/(1.d0+x)
     $ - 64*Hr3(-1,0,1)/(1.d0+x)**4
     $ + 128*Hr3(-1,0,1)/(1.d0+x)**3
     $ - 80*Hr3(-1,0,1)/(1.d0+x)**2
     $ + 16*Hr3(-1,0,1)/(1.d0+x)
     $ - 8*Hr3(0,0,0)/(1.d0+x)**4
     $ + 16*Hr3(0,0,0)/(1.d0+x)**3
     $ - 10*Hr3(0,0,0)/(1.d0+x)**2
     $ + 2*Hr3(0,0,0)/(1.d0+x)
     $ + 32*Hr3(0,0,1)/(1.d0+x)**4
     $ - 64*Hr3(0,0,1)/(1.d0+x)**3
     $ + 40*Hr3(0,0,1)/(1.d0+x)**2
     $ - 8*Hr3(0,0,1)/(1.d0+x) )
       endif
      return
      end  

      double precision function rig12b(x,s,HAr1,HAr2,HAr3,HAr4)
      implicit double precision (a-h,o-z) 
      dimension HAc1(-1:1),HAc2(-1:1,-1:1),HAc3(-1:1,-1:1,-1:1), 
     $          HAc4(-1:1,-1:1,-1:1,-1:1) 
      dimension HAr1(-1:1),HAr2(-1:1,-1:1),HAr3(-1:1,-1:1,-1:1), 
     $          HAr4(-1:1,-1:1,-1:1,-1:1) 
      dimension HAi1(-1:1),HAi2(-1:1,-1:1),HAi3(-1:1,-1:1,-1:1), 
     $          HAi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.4.d0) then 
      rig12b = 0.d0
       endif
       if(s.ge.4.d0) then
      rig12b = pi * (
     $ + 48*z2/(1.d0+x)**5
     $ - 152*z2/(1.d0+x)**4
     $ + 172*z2/(1.d0+x)**3
     $ - 80*z2/(1.d0+x)**2
     $ + 12*z2/(1.d0+x)
     $ - 48*z3/(1.d0+x)**4
     $ + 96*z3/(1.d0+x)**3
     $ - 60*z3/(1.d0+x)**2
     $ + 12*z3/(1.d0+x)
     $ + 64*HAr1(-1)*z2/(1.d0+x)**4
     $ - 128*HAr1(-1)*z2/(1.d0+x)**3
     $ + 80*HAr1(-1)*z2/(1.d0+x)**2
     $ - 16*HAr1(-1)*z2/(1.d0+x)
     $ - 8*HAr1(0)*z2/(1.d0+x)**4
     $ + 16*HAr1(0)*z2/(1.d0+x)**3
     $ - 10*HAr1(0)*z2/(1.d0+x)**2
     $ + 2*HAr1(0)*z2/(1.d0+x)
     $ - 8*HAr1(0)/(1.d0+x)**4
     $ + 16*HAr1(0)/(1.d0+x)**3
     $ - 10*HAr1(0)/(1.d0+x)**2
     $ + 2*HAr1(0)/(1.d0+x)
     $ - 64*HAr2(-1,0)/(1.d0+x)**4
     $ + 128*HAr2(-1,0)/(1.d0+x)**3
     $ - 76*HAr2(-1,0)/(1.d0+x)**2
     $ + 12*HAr2(-1,0)/(1.d0+x)
     $ - 48*HAr2(0,0)/(1.d0+x)**5
     $ + 152*HAr2(0,0)/(1.d0+x)**4
     $ - 172*HAr2(0,0)/(1.d0+x)**3
     $ + 80*HAr2(0,0)/(1.d0+x)**2
     $ - 12*HAr2(0,0)/(1.d0+x)
     $ - 32*HAr3(-1,0,0)/(1.d0+x)**4
     $ + 64*HAr3(-1,0,0)/(1.d0+x)**3
     $ - 40*HAr3(-1,0,0)/(1.d0+x)**2
     $ + 8*HAr3(-1,0,0)/(1.d0+x)
     $ - 64*HAr3(-1,0,1)/(1.d0+x)**4
     $ + 128*HAr3(-1,0,1)/(1.d0+x)**3
     $ - 80*HAr3(-1,0,1)/(1.d0+x)**2
     $ + 16*HAr3(-1,0,1)/(1.d0+x)
     $ - 8*HAr3(0,0,0)/(1.d0+x)**4
     $ + 16*HAr3(0,0,0)/(1.d0+x)**3
     $ - 10*HAr3(0,0,0)/(1.d0+x)**2
     $ + 2*HAr3(0,0,0)/(1.d0+x)
     $ + 32*HAr3(0,0,1)/(1.d0+x)**4
     $ - 64*HAr3(0,0,1)/(1.d0+x)**3
     $ + 40*HAr3(0,0,1)/(1.d0+x)**2
     $ - 8*HAr3(0,0,1)/(1.d0+x) )
       endif
      return
      end  
c
c     Scalar - Two loop functions
c

c     F_{0}**(1l):
      double precision function re1f0(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.4.d0) then 
        xe = Dsqrt(s)/Dsqrt(4.d0-s)
	phy = DATAN(xe)
      re1f0 = - 16.d0/s**2*phy**2 + 4.d0/s
       endif
       if(s.ge.4.d0) then
      re1f0 = 
     $ - 24.d0*z2/(1.d0+x)**4
     $ + 48.d0*z2/(1.d0+x)**3
     $ - 24.d0*z2/(1.d0+x)**2
     $ - 4.d0/(1.d0+x)**2
     $ + 4.d0/(1.d0+x)
     $ + 8.d0*Hr2(0,0)/(1.d0+x)**4
     $ - 16.d0*Hr2(0,0)/(1.d0+x)**3
     $ + 8.d0*Hr2(0,0)/(1.d0+x)**2
       endif
      return
      end  

      double precision function ri1f0(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.4.d0) then 
      ri1f0 = 0.d0
       endif
       if(s.ge.4.d0) then
      ri1f0 = pi * (
     $ + 8.d0*Hr1(0)/(1.d0+x)**4
     $ - 16.d0*Hr1(0)/(1.d0+x)**3
     $ + 8.d0*Hr1(0)/(1.d0+x)**2 )
       endif
      return
      end  

      double precision function rASYe1f0(s)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
      rASYe1f0 = 4.d0/s + 4.d0/s**2*( Dlog(s)*Dlog(s) - 6.d0*z2 )
      return
      end  

      double precision function rASYi1f0(s)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
      rASYi1f0 = - 8.d0*pi/s**2*Dlog(s)
      return
      end 
c
c     F_{0}**(2l,a): 
c
      double precision function ref0a(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.3.4d0) then 
      var = - Dlog(1.d0-s/4.d0)
      ref0a = 
     $ - 7.5d-1 
     $ - 5.3703703703703703704d-1*var 
     $ - 8.2310405643738977072d-2*var**2 
     $ + 3.3694633408919123205d-2*var**3 
     $ + 1.4976236285760095284d-2*var**4 
     $ + 1.6908287314457863486d-3*var**5 
     $ - 1.4753438869507985577d-4*var**6 
     $ - 3.1879548329199932913d-5*var**7 
     $ + 6.0148530243092357202d-6*var**8 
     $ + 1.2548415714137008776d-6*var**9 
     $ - 1.5902973609750169801d-7*var**10 
     $ - 4.0299502537626031213d-8*var**11 
     $ + 4.4073312775981334589d-9*var**12 
     $ + 1.2626463512554570350d-9*var**13 
     $ - 1.2030985715957964736d-10*var**14 
     $ - 3.8416190847159937493d-11*var**15
     $ + 3.2530083738465503608d-12*var**16
     $ + 1.1436364177678055040d-12*var**17
     $ - 8.7133225921614120449d-14*var**18
     $ - 3.3464309793103365882d-14*var**19
     $ + 2.3135017923478337770d-15*var**20
       endif
       if(s.ge.3.4d0.AND.s.lt.4.d0) then  
       var = - Dlog(s/4.d0)
      ref0a = 
     $ + 2.7488935718910690837d0/Dsqrt(var)
     $ + 2.7038612434545692097d0
     $ - 22.119670967770123068d0*var 
     $ - 41.976988214869539677d0*var**2 
     $ - 36.118730004908664664d0*var**3 
     $ - 19.554477640409319228d0*var**4 
     $ - 7.3495818780469394724d0*var**5 
     $ - 1.9048050249445051980d0*var**6 
     $ - 2.6243573208021317009d-1*var**7 
     $ + 4.2214501428142530348d-2*var**8 
     $ + 4.3497219140003050334d-2*var**9 
     $ + 1.8054241726545941423d-2*var**10 
     $ + Dsqrt(var)*(
     $ + 1.3018751314957355162d0
     $ + 16.734036172883749804d0*var 
     $ + 32.799777170726864812d0*var**2 
     $ + 32.568220397491590789d0*var**3 
     $ + 21.700547766595296109d0*var**4 
     $ + 10.953625267553392242d0*var**5 
     $ + 4.4792240259935312629d0*var**6 
     $ + 1.5480673031853917218d0*var**7 
     $ + 4.6539959457929496836d-1*var**8 
     $ + 1.2423144224521387287d-1*var**9 
     $ + 2.9894128635736082397d-2*var**10 )
     $ + Dlog(var)*(
     $ + 4.9348022005446793094d0
     $ + 3.2898681336964528729d0*var 
     $ - 3.9478417604357434475d0*var**2 
     $ - 8.0523439081903656033d0*var**3 
     $ - 7.1358806423749251601d0*var**4 
     $ - 4.3102969292780582770d0*var**5 
     $ - 2.0148589052071092473d0*var**6 
     $ - 7.7527354821396102742d-1*var**7 
     $ - 2.5479227510672222910d-1*var**8 
     $ - 7.3310437645734746381d-2*var**9 
     $ - 1.8796168648191636889d-2*var**10 )
     $ - Dsqrt(var)*Dlog(var)*(
     $ + 8.3775804095727819692d0*var
     $ + 16.894787159305110305d0*var**2 
     $ + 17.181519822132750164d0*var**3 
     $ + 11.761732273731406202d0*var**4 
     $ + 6.1026171599820167419d0*var**5 
     $ + 2.5612962129180947012d0*var**6 
     $ + 9.0593535392139924921d-1*var**7 
     $ + 2.7769217805847923122d-1*var**8 
     $ + 7.5262455940204513485d-2*var**9 
     $ + 1.8308565182323209754d-2*var**10 )
       endif
       if(s.ge.4.d0) then
      ref0a = 
     $ - 102*z2/(1.d0+x)**4
     $ + 204*z2/(1.d0+x)**3
     $ - 102*z2/(1.d0+x)**2
     $ + 69.d0/40*z2**2/(1.d0-x)
     $ + 138.d0/5*z2**2/(1.d0+x)**5
     $ - 69*z2**2/(1.d0+x)**4
     $ + 621.d0/10*z2**2/(1.d0+x)**3
     $ - 483.d0/20*z2**2/(1.d0+x)**2
     $ + 69.d0/40*z2**2/(1.d0+x)
     $ - 24*z3/(1.d0+x)**4
     $ + 48*z3/(1.d0+x)**3
     $ - 24*z3/(1.d0+x)**2
     $ - 14.d0/(1.d0+x)**2
     $ + 14.d0/(1.d0+x)
     $ - 24*Hr1(-1)*z2/(1.d0+x)**4
     $ + 48*Hr1(-1)*z2/(1.d0+x)**3
     $ - 24*Hr1(-1)*z2/(1.d0+x)**2
     $ + 36*Hr1(0)*z2/(1.d0+x)**5
     $ - 146*Hr1(0)*z2/(1.d0+x)**4
     $ + 184*Hr1(0)*z2/(1.d0+x)**3
     $ - 74*Hr1(0)*z2/(1.d0+x)**2
     $ - 2*Hr1(0)*z3/(1.d0-x)
     $ - 32*Hr1(0)*z3/(1.d0+x)**5
     $ + 80*Hr1(0)*z3/(1.d0+x)**4
     $ - 72*Hr1(0)*z3/(1.d0+x)**3
     $ + 28*Hr1(0)*z3/(1.d0+x)**2
     $ - 2*Hr1(0)*z3/(1.d0+x)
     $ + 7.d0/4*Hr1(0)/(1.d0-x)
     $ + Hr1(0)/(1.d0+x)**3
     $ - 3.d0/2*Hr1(0)/(1.d0+x)**2
     $ - 5.d0/4*Hr1(0)/(1.d0+x)
     $ - 21.d0/2*Hr2(0,-1)*z2/(1.d0-x)
     $ - 168*Hr2(0,-1)*z2/(1.d0+x)**5
     $ + 420*Hr2(0,-1)*z2/(1.d0+x)**4
     $ - 378*Hr2(0,-1)*z2/(1.d0+x)**3
     $ + 147*Hr2(0,-1)*z2/(1.d0+x)**2
     $ - 21.d0/2*Hr2(0,-1)*z2/(1.d0+x)
     $ - 1.d0/4*Hr2(0,0)*z2/(1.d0-x)
     $ - 4*Hr2(0,0)*z2/(1.d0+x)**5
     $ + 10*Hr2(0,0)*z2/(1.d0+x)**4
     $ - 9*Hr2(0,0)*z2/(1.d0+x)**3
     $ + 7.d0/2*Hr2(0,0)*z2/(1.d0+x)**2
     $ - 1.d0/4*Hr2(0,0)*z2/(1.d0+x)
     $ + 34*Hr2(0,0)/(1.d0+x)**4
     $ - 68*Hr2(0,0)/(1.d0+x)**3
     $ + 34*Hr2(0,0)/(1.d0+x)**2
     $ + 6*Hr2(0,1)*z2/(1.d0-x)
     $ + 96*Hr2(0,1)*z2/(1.d0+x)**5
     $ - 240*Hr2(0,1)*z2/(1.d0+x)**4
     $ + 216*Hr2(0,1)*z2/(1.d0+x)**3
     $ - 84*Hr2(0,1)*z2/(1.d0+x)**2
     $ + 6*Hr2(0,1)*z2/(1.d0+x)
     $ + 8*Hr3(-1,0,0)/(1.d0+x)**4
     $ - 16*Hr3(-1,0,0)/(1.d0+x)**3
     $ + 8*Hr3(-1,0,0)/(1.d0+x)**2
     $ - 8*Hr3(0,-1,0)/(1.d0+x)**4
     $ + 16*Hr3(0,-1,0)/(1.d0+x)**3
     $ - 8*Hr3(0,-1,0)/(1.d0+x)**2
     $ - 12*Hr3(0,0,0)/(1.d0+x)**5
     $ + 46*Hr3(0,0,0)/(1.d0+x)**4
     $ - 56*Hr3(0,0,0)/(1.d0+x)**3
     $ + 22*Hr3(0,0,0)/(1.d0+x)**2
     $ + 32*Hr3(0,1,0)/(1.d0+x)**4
     $ - 64*Hr3(0,1,0)/(1.d0+x)**3
     $ + 32*Hr3(0,1,0)/(1.d0+x)**2
     $ + 7.d0/2*Hr4(0,-1,0,0)/(1.d0-x)
     $ + 56*Hr4(0,-1,0,0)/(1.d0+x)**5
     $ - 140*Hr4(0,-1,0,0)/(1.d0+x)**4
     $ + 126*Hr4(0,-1,0,0)/(1.d0+x)**3
     $ - 49*Hr4(0,-1,0,0)/(1.d0+x)**2
     $ + 7.d0/2*Hr4(0,-1,0,0)/(1.d0+x)
     $ - Hr4(0,0,-1,0)/(1.d0-x)
     $ - 16*Hr4(0,0,-1,0)/(1.d0+x)**5
     $ + 40*Hr4(0,0,-1,0)/(1.d0+x)**4
     $ - 36*Hr4(0,0,-1,0)/(1.d0+x)**3
     $ + 14*Hr4(0,0,-1,0)/(1.d0+x)**2
     $ - Hr4(0,0,-1,0)/(1.d0+x)
     $ - 1.d0/4*Hr4(0,0,0,0)/(1.d0-x)
     $ - 4*Hr4(0,0,0,0)/(1.d0+x)**5
     $ + 10*Hr4(0,0,0,0)/(1.d0+x)**4
     $ - 9*Hr4(0,0,0,0)/(1.d0+x)**3
     $ + 7.d0/2*Hr4(0,0,0,0)/(1.d0+x)**2
     $ - 1.d0/4*Hr4(0,0,0,0)/(1.d0+x)
     $ + 4*Hr4(0,0,1,0)/(1.d0-x)
     $ + 64*Hr4(0,0,1,0)/(1.d0+x)**5
     $ - 160*Hr4(0,0,1,0)/(1.d0+x)**4
     $ + 144*Hr4(0,0,1,0)/(1.d0+x)**3
     $ - 56*Hr4(0,0,1,0)/(1.d0+x)**2
     $ + 4*Hr4(0,0,1,0)/(1.d0+x)
     $ - 2*Hr4(0,1,0,0)/(1.d0-x)
     $ - 32*Hr4(0,1,0,0)/(1.d0+x)**5
     $ + 80*Hr4(0,1,0,0)/(1.d0+x)**4
     $ - 72*Hr4(0,1,0,0)/(1.d0+x)**3
     $ + 28*Hr4(0,1,0,0)/(1.d0+x)**2
     $ - 2*Hr4(0,1,0,0)/(1.d0+x)
       endif
      return
      end  

      double precision function rif0a(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.4.d0) then 
      rif0a = 0.d0
       endif
       if(s.ge.4.d0) then
      rif0a = pi * (
     $ + 12*z2/(1.d0+x)**5
     $ - 54*z2/(1.d0+x)**4
     $ + 72*z2/(1.d0+x)**3
     $ - 30*z2/(1.d0+x)**2
     $ - 2*z3/(1.d0-x)
     $ - 32*z3/(1.d0+x)**5
     $ + 80*z3/(1.d0+x)**4
     $ - 72*z3/(1.d0+x)**3
     $ + 28*z3/(1.d0+x)**2
     $ - 2*z3/(1.d0+x)
     $ + 7.d0/4/(1.d0-x)
     $ + 1.d0/(1.d0+x)**3
     $ - 3.d0/2.d0/(1.d0+x)**2
     $ - 5.d0/4.d0/(1.d0+x)
     $ - 3.d0/4*Hr1(0)*z2/(1.d0-x)
     $ - 12*Hr1(0)*z2/(1.d0+x)**5
     $ + 30*Hr1(0)*z2/(1.d0+x)**4
     $ - 27*Hr1(0)*z2/(1.d0+x)**3
     $ + 21.d0/2*Hr1(0)*z2/(1.d0+x)**2
     $ - 3.d0/4*Hr1(0)*z2/(1.d0+x)
     $ + 34*Hr1(0)/(1.d0+x)**4
     $ - 68*Hr1(0)/(1.d0+x)**3
     $ + 34*Hr1(0)/(1.d0+x)**2
     $ + 8*Hr2(-1,0)/(1.d0+x)**4
     $ - 16*Hr2(-1,0)/(1.d0+x)**3
     $ + 8*Hr2(-1,0)/(1.d0+x)**2
     $ - 8*Hr2(0,-1)/(1.d0+x)**4
     $ + 16*Hr2(0,-1)/(1.d0+x)**3
     $ - 8*Hr2(0,-1)/(1.d0+x)**2
     $ - 12*Hr2(0,0)/(1.d0+x)**5
     $ + 46*Hr2(0,0)/(1.d0+x)**4
     $ - 56*Hr2(0,0)/(1.d0+x)**3
     $ + 22*Hr2(0,0)/(1.d0+x)**2
     $ + 32*Hr2(0,1)/(1.d0+x)**4
     $ - 64*Hr2(0,1)/(1.d0+x)**3
     $ + 32*Hr2(0,1)/(1.d0+x)**2
     $ + 7.d0/2*Hr3(0,-1,0)/(1.d0-x)
     $ + 56*Hr3(0,-1,0)/(1.d0+x)**5
     $ - 140*Hr3(0,-1,0)/(1.d0+x)**4
     $ + 126*Hr3(0,-1,0)/(1.d0+x)**3
     $ - 49*Hr3(0,-1,0)/(1.d0+x)**2
     $ + 7.d0/2*Hr3(0,-1,0)/(1.d0+x)
     $ - Hr3(0,0,-1)/(1.d0-x)
     $ - 16*Hr3(0,0,-1)/(1.d0+x)**5
     $ + 40*Hr3(0,0,-1)/(1.d0+x)**4
     $ - 36*Hr3(0,0,-1)/(1.d0+x)**3
     $ + 14*Hr3(0,0,-1)/(1.d0+x)**2
     $ - Hr3(0,0,-1)/(1.d0+x)
     $ - 1.d0/4*Hr3(0,0,0)/(1.d0-x)
     $ - 4*Hr3(0,0,0)/(1.d0+x)**5
     $ + 10*Hr3(0,0,0)/(1.d0+x)**4
     $ - 9*Hr3(0,0,0)/(1.d0+x)**3
     $ + 7.d0/2*Hr3(0,0,0)/(1.d0+x)**2
     $ - 1.d0/4*Hr3(0,0,0)/(1.d0+x)
     $ + 4*Hr3(0,0,1)/(1.d0-x)
     $ + 64*Hr3(0,0,1)/(1.d0+x)**5
     $ - 160*Hr3(0,0,1)/(1.d0+x)**4
     $ + 144*Hr3(0,0,1)/(1.d0+x)**3
     $ - 56*Hr3(0,0,1)/(1.d0+x)**2
     $ + 4*Hr3(0,0,1)/(1.d0+x)
     $ - 2*Hr3(0,1,0)/(1.d0-x)
     $ - 32*Hr3(0,1,0)/(1.d0+x)**5
     $ + 80*Hr3(0,1,0)/(1.d0+x)**4
     $ - 72*Hr3(0,1,0)/(1.d0+x)**3
     $ + 28*Hr3(0,1,0)/(1.d0+x)**2
     $ - 2*Hr3(0,1,0)/(1.d0+x) )
       endif
      return
      end  

      double precision function rASYef0a(s)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
      rASYef0a =        
     $ + 1.d0/s**2 * (
     $ + 6.d0
     $ - 102.d0*z2
     $ + 138.d0/5*z2**2
     $ - 24.d0*z3
     $ - 8.d0*Dlog(s)
     $ + 38.d0*Dlog(s)*z2
     $ + 32.d0*Dlog(s)*z3
     $ + 17.d0*Dlog(s)*Dlog(s)
     $ - 2.d0*Dlog(s)*Dlog(s)*z2
     $ - 5.d0/3*Dlog(s)*Dlog(s)*Dlog(s)
     $ - 1.d0/6*Dlog(s)*Dlog(s)*Dlog(s)*Dlog(s)
     $ ) + 1.d0/s * (
     $ + 14.d0
     $ - 3.d0*Dlog(s) )
      return
      end  

      double precision function rASYif0a(s)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 )  
      rASYif0a = 
     $ + 1.d0/s**2*pi * (
     $ + 8.d0
     $ - 18.d0*z2
     $ - 32.d0*z3
     $ - 34.d0*Dlog(s)
     $ + 12.d0*Dlog(s)*z2
     $ + 5.d0*Dlog(s)*Dlog(s)
     $ + 2.d0/3*Dlog(s)*Dlog(s)*Dlog(s) ) + 3.d0/s*pi 
      return
      end  

c     F_{0}**(2l,b):
      double precision function ref0b(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.4.d0) then 
        xe = Dsqrt(s)/Dsqrt(4.d0-s)
	phy = DATAN(xe)
      ref0b = - 12.d0/s*phy/Dsqrt(s*(4.d0-s))
     $ + 12.d0/s**2*phy**2 
       endif
       if(s.ge.4.d0) then
      ref0b = 
     $ + 18*z2/(1.d0+x)**4
     $ - 36*z2/(1.d0+x)**3
     $ + 18*z2/(1.d0+x)**2
     $ - 3.d0/4*Hr1(0)/(1.d0-x)
     $ - 3*Hr1(0)/(1.d0+x)**3
     $ + 9.d0/2*Hr1(0)/(1.d0+x)**2
     $ - 3.d0/4*Hr1(0)/(1.d0+x)
     $ - 6*Hr2(0,0)/(1.d0+x)**4
     $ + 12*Hr2(0,0)/(1.d0+x)**3
     $ - 6*Hr2(0,0)/(1.d0+x)**2
       endif
      return
      end  


      double precision function rif0b(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.4.d0) then 
      rif0b = 0.d0
       endif
       if(s.ge.4.d0) then
      rif0b = pi * (
     $ - 3.d0/4.d0/(1.d0-x)
     $ - 3.d0/(1.d0+x)**3
     $ + 9.d0/2.d0/(1.d0+x)**2
     $ - 3.d0/4.d0/(1.d0+x)
     $ - 6*Hr1(0)/(1.d0+x)**4
     $ + 12*Hr1(0)/(1.d0+x)**3
     $ - 6*Hr1(0)/(1.d0+x)**2 )
       endif
      return
      end  

      double precision function rASYef0b(s)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
      rASYef0b =   1.d0/s**2 * (
     $ + 18.d0*z2
     $ + 6.d0*Dlog(s)
     $ - 3.d0*Dlog(s)*Dlog(s) )
      return
      end  

      double precision function rASYif0b(s)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 )  
      rASYif0b = 1.d0/s**2*pi * ( - 6.d0 + 6.d0*Dlog(s) )
      return
      end   

c     F_{0}**(2l,c):
      double precision function ref0c(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.4.d0) then 
        xe = Dsqrt(s)/Dsqrt(4.d0-s)
	phy = DATAN(xe) 
      ref0c = 12.d0/s**2*phy**2  - 3.d0/s
       endif
       if(s.ge.4.d0) then
      ref0c = 
     $ + 18*z2/(1.d0+x)**4
     $ - 36*z2/(1.d0+x)**3
     $ + 18*z2/(1.d0+x)**2
     $ + 3.d0/(1.d0+x)**2
     $ - 3.d0/(1.d0+x)
     $ - 6*Hr2(0,0)/(1.d0+x)**4
     $ + 12*Hr2(0,0)/(1.d0+x)**3
     $ - 6*Hr2(0,0)/(1.d0+x)**2
       endif
      return
      end  

      double precision function rif0c(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.4.d0) then 
      rif0c = 0.d0
       endif
       if(s.ge.4.d0) then
      rif0c = pi * (
     $ - 6*Hr1(0)/(1.d0+x)**4
     $ + 12*Hr1(0)/(1.d0+x)**3
     $ - 6*Hr1(0)/(1.d0+x)**2 )
       endif
      return
      end  

c     G_{0}**(2l,CA):
      double precision function reg0(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 )  
      parameter ( A  = 2.7605366106449011d0 )   
      parameter ( B  = 2.5838563897912388d0 )  
       if(s.lt.3.6d0) then  
      var = - Dlog(1.d0-s/4.d0) 
      reg0 = 
     $ - 1.6666666666666666667d-1
     $ - 2.9629629629629629630d-2*var 
     $ + 1.5873015873015873016d-2*var**2 
     $ + 3.8236331569664902998d-3*var**3 
     $ - 6.2455240233018010796d-4*var**4 
     $ - 2.0521965646682199970d-4*var**5 
     $ + 2.0870733644732133016d-5*var**6 
     $ + 8.5860488421512831277d-6*var**7 
     $ - 6.4263532779594174913d-7*var**8 
     $ - 3.1548929401869814418d-7*var**9 
     $ + 1.8857951673131969858d-8*var**10 
     $ + 1.0699193526398019716d-8*var**11 
     $ - 5.3567515327430634679d-10*var**12 
     $ - 3.4368814248501485554d-10*var**13 
     $ + 1.4844384785258067169d-11*var**14 
     $ + 1.0618972676062248643d-11*var**15
     $ - 4.0295901593637783229d-13*var**16
     $ - 3.1868086791279454548d-13*var**17
     $ + 1.0739091311870492472d-14*var**18
     $ + 9.3509017342184855432d-15*var**19
     $ - 2.8130532305664394615d-16*var**20
       endif
       if(s.ge.3.6d0.AND.s.lt.4.d0) then  
       var = - Dlog(s/4.d0)  
      reg0 = 
     $ + 2.9614520216275622726d0
     $ + 13.064905404858383853d0*var 
     $ + 18.072838999522636840d0*var**2 
     $ + 13.414791292961914508d0*var**3 
     $ + 6.1622196917654713177d0*var**4 
     $ + 1.6541057116082671146d0*var**5 
     $ + 4.4033261936490237038d-2*var**6 
     $ + 2.1404604730690697411d-1*var**7 
     $ - A*(
     $ + 1.d0
     $ + 2.d0*var 
     $ + 2.d0*var**2 
     $ + 1.3333333333333333333d0*var**3 
     $ + 6.6666666666666666667d-1*var**4 
     $ + 2.6666666666666666667d-1*var**5 
     $ + 8.8888888888888888889d-2*var**6 
     $ + 2.5396825396825396825d-2*var**7 )
     $ - Dsqrt(var)*(
     $ + 7.4727010382925509319d0 
     $ + 18.330348159016776283d0*var 
     $ + 20.392447228156116892d0*var**2 
     $ + 14.017889380257231834d0*var**3 
     $ + 6.6954983366582129947d0*var**4 
     $ + 2.3161359828457195540d0*var**5 
     $ + 5.6060032823290537947d-1*var**6 
     $ + 6.9402900469956687777d-2*var**7  )
     $ + Dsqrt(var)*B*(
     $ + 2.d0 
     $ + 3.8333333333333333333d0*var 
     $ + 3.6708333333333333333d0*var**2 
     $ + 2.3424107142857142857d0*var**3 
     $ + 1.1209108382936507937d0*var**4 
     $ + 4.2921309918380230880d-1*var**5 
     $ + 1.3704122596334510397d-1*var**6 
     $ + 3.7538842048221117232d-2*var**7 )
     $ + Dsqrt(var)*Dlog(var)*(
     $ + 2.0943951023931954923d0*var
     $ + 4.0840704496667312100d0*var**2 
     $ + 3.9812206901742082171d0*var**3 
     $ + 2.5870870058207614458d0*var**4 
     $ + 1.2608902980177778006d0*var**5 
     $ + 4.9169030086857321662d-1*var**6 
     $ + 1.5981926081331177872d-1*var**7 )
       endif
       if(s.ge.4.d0) then
      reg0 = 
     $ - 12*z2/(1.d0+x)**4
     $ + 24*z2/(1.d0+x)**3
     $ - 12*z2/(1.d0+x)**2
     $ - 26.d0/5*z2**2/(1.d0+x)**4
     $ + 52.d0/5*z2**2/(1.d0+x)**3
     $ - 26.d0/5*z2**2/(1.d0+x)**2
     $ + 32*z3/(1.d0+x)**4
     $ - 64*z3/(1.d0+x)**3
     $ + 32*z3/(1.d0+x)**2
     $ - 6.d0/(1.d0+x)**2
     $ + 6.d0/(1.d0+x)
     $ - 96*Hr1(-1)*z2/(1.d0+x)**4
     $ + 192*Hr1(-1)*z2/(1.d0+x)**3
     $ - 96*Hr1(-1)*z2/(1.d0+x)**2
     $ - 48*Hr1(-1)*z3/(1.d0+x)**4
     $ + 96*Hr1(-1)*z3/(1.d0+x)**3
     $ - 48*Hr1(-1)*z3/(1.d0+x)**2
     $ - 72*Hr1(0)*z2/(1.d0+x)**5
     $ + 228*Hr1(0)*z2/(1.d0+x)**4
     $ - 240*Hr1(0)*z2/(1.d0+x)**3
     $ + 84*Hr1(0)*z2/(1.d0+x)**2
     $ + 24*Hr1(0)*z3/(1.d0+x)**4
     $ - 48*Hr1(0)*z3/(1.d0+x)**3
     $ + 24*Hr1(0)*z3/(1.d0+x)**2
     $ - 64*Hr2(-1,0)*z2/(1.d0+x)**4
     $ + 128*Hr2(-1,0)*z2/(1.d0+x)**3
     $ - 64*Hr2(-1,0)*z2/(1.d0+x)**2
     $ - 4*Hr2(0,0)*z2/(1.d0+x)**4
     $ + 8*Hr2(0,0)*z2/(1.d0+x)**3
     $ - 4*Hr2(0,0)*z2/(1.d0+x)**2
     $ + 4*Hr2(0,0)/(1.d0+x)**4
     $ - 8*Hr2(0,0)/(1.d0+x)**3
     $ + 4*Hr2(0,0)/(1.d0+x)**2
     $ + 32*Hr3(-1,0,0)/(1.d0+x)**4
     $ - 64*Hr3(-1,0,0)/(1.d0+x)**3
     $ + 32*Hr3(-1,0,0)/(1.d0+x)**2
     $ + 24*Hr3(0,0,0)/(1.d0+x)**5
     $ - 76*Hr3(0,0,0)/(1.d0+x)**4
     $ + 80*Hr3(0,0,0)/(1.d0+x)**3
     $ - 28*Hr3(0,0,0)/(1.d0+x)**2
     $ + 16*Hr4(-1,0,0,0)/(1.d0+x)**4
     $ - 32*Hr4(-1,0,0,0)/(1.d0+x)**3
     $ + 16*Hr4(-1,0,0,0)/(1.d0+x)**2
     $ + 32*Hr4(-1,0,1,0)/(1.d0+x)**4
     $ - 64*Hr4(-1,0,1,0)/(1.d0+x)**3
     $ + 32*Hr4(-1,0,1,0)/(1.d0+x)**2
     $ + 4*Hr4(0,0,0,0)/(1.d0+x)**4
     $ - 8*Hr4(0,0,0,0)/(1.d0+x)**3
     $ + 4*Hr4(0,0,0,0)/(1.d0+x)**2
     $ - 16*Hr4(0,0,1,0)/(1.d0+x)**4
     $ + 32*Hr4(0,0,1,0)/(1.d0+x)**3
     $ - 16*Hr4(0,0,1,0)/(1.d0+x)**2
       endif
      return
      end  

      double precision function rig0(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.4.d0) then 
      rig0 = 0.d0
       endif
       if(s.ge.4.d0) then
      rig0 = pi * (
     $ - 24*z2/(1.d0+x)**5
     $ + 76*z2/(1.d0+x)**4
     $ - 80*z2/(1.d0+x)**3
     $ + 28*z2/(1.d0+x)**2
     $ + 24*z3/(1.d0+x)**4
     $ - 48*z3/(1.d0+x)**3
     $ + 24*z3/(1.d0+x)**2
     $ - 32*Hr1(-1)*z2/(1.d0+x)**4
     $ + 64*Hr1(-1)*z2/(1.d0+x)**3
     $ - 32*Hr1(-1)*z2/(1.d0+x)**2
     $ + 4*Hr1(0)*z2/(1.d0+x)**4
     $ - 8*Hr1(0)*z2/(1.d0+x)**3
     $ + 4*Hr1(0)*z2/(1.d0+x)**2
     $ + 4*Hr1(0)/(1.d0+x)**4
     $ - 8*Hr1(0)/(1.d0+x)**3
     $ + 4*Hr1(0)/(1.d0+x)**2
     $ + 32*Hr2(-1,0)/(1.d0+x)**4
     $ - 64*Hr2(-1,0)/(1.d0+x)**3
     $ + 32*Hr2(-1,0)/(1.d0+x)**2
     $ + 24*Hr2(0,0)/(1.d0+x)**5
     $ - 76*Hr2(0,0)/(1.d0+x)**4
     $ + 80*Hr2(0,0)/(1.d0+x)**3
     $ - 28*Hr2(0,0)/(1.d0+x)**2
     $ + 16*Hr3(-1,0,0)/(1.d0+x)**4
     $ - 32*Hr3(-1,0,0)/(1.d0+x)**3
     $ + 16*Hr3(-1,0,0)/(1.d0+x)**2
     $ + 32*Hr3(-1,0,1)/(1.d0+x)**4
     $ - 64*Hr3(-1,0,1)/(1.d0+x)**3
     $ + 32*Hr3(-1,0,1)/(1.d0+x)**2
     $ + 4*Hr3(0,0,0)/(1.d0+x)**4
     $ - 8*Hr3(0,0,0)/(1.d0+x)**3
     $ + 4*Hr3(0,0,0)/(1.d0+x)**2
     $ - 16*Hr3(0,0,1)/(1.d0+x)**4
     $ + 32*Hr3(0,0,1)/(1.d0+x)**3
     $ - 16*Hr3(0,0,1)/(1.d0+x)**2 )
       endif
      return
      end   
c
c     Electo-Weak functions
c

c     F_{1}**(1l):
      double precision function re1f1(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.4.d0) then 
        xe = Dsqrt(s)/Dsqrt(4.d0-s)
	phy = DATAN(xe)
      re1f1 = phy**2 * (
     $ - 48.d0/s**2
     $ + 24.d0/s )
     $ + 2.d0
     $ + 12.d0/s
       endif
       if(s.ge.4.d0) then
      re1f1 = 
     $ + 2.d0
     $ - 72*z2/(1.d0+x)**4
     $ + 144*z2/(1.d0+x)**3
     $ - 108*z2/(1.d0+x)**2
     $ + 36*z2/(1.d0+x)
     $ - 12.d0/(1.d0+x)**2
     $ + 12.d0/(1.d0+x)
     $ + 24*Hr2(0,0)/(1.d0+x)**4
     $ - 48*Hr2(0,0)/(1.d0+x)**3
     $ + 36*Hr2(0,0)/(1.d0+x)**2
     $ - 12*Hr2(0,0)/(1.d0+x)
       endif
      return
      end  

      double precision function ri1f1(x,s,Hr1,Hr2,Hr3,Hr4)
      implicit double precision (a-h,o-z) 
      dimension Hc1(-1:1),Hc2(-1:1,-1:1),Hc3(-1:1,-1:1,-1:1), 
     $          Hc4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hr1(-1:1),Hr2(-1:1,-1:1),Hr3(-1:1,-1:1,-1:1), 
     $          Hr4(-1:1,-1:1,-1:1,-1:1) 
      dimension Hi1(-1:1),Hi2(-1:1,-1:1),Hi3(-1:1,-1:1,-1:1), 
     $          Hi4(-1:1,-1:1,-1:1,-1:1) 
      parameter ( pi   = 3.1415926535897932385d0 )
      parameter ( z2   = 1.6449340668482264365d0 )
      parameter ( z3   = 1.2020569031595942854d0 )
      parameter ( aln2  = 0.6931471805599453094d0 ) 
       if(s.lt.4.d0) then 
      ri1f1 = 0.d0
       endif
       if(s.ge.4.d0) then
      ri1f1 = pi * (
     $ + 24*Hr1(0)/(1.d0+x)**4
     $ - 48*Hr1(0)/(1.d0+x)**3
     $ + 36*Hr1(0)/(1.d0+x)**2
     $ - 12*Hr1(0)/(1.d0+x) )
       endif
      return
      end
