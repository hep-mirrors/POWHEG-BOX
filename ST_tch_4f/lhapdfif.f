
      subroutine genericpdfset(ndns)
      implicit none
      include 'pwhg_pdf.h'
c wrap for pdfset; avoids subsequent
c calls to pdfset (you never know)
      integer ndns
      character * 20 parm(20)
      real * 8 val(20)
      integer ndns0
      real * 8 genericxlambdL,genericxlambdNL,genericxlambdNNL,
     ,         alphasPDF,asmz,mz,lam5
      parameter (mz=91.1876d0)
      integer iord
      common/cgenericpdf/lam5,iord
      real * 8 qcdl4,qcdl5
      common/w50512/qcdl4,qcdl5
      data ndns0/-1/
      save ndns0
      if(ndns.ne.ndns0) then
         ndns0=ndns
         parm(1)='DEFAULT'
         val(1)=ndns
         call pdfset(parm,val)
 

c     find q2min and store it in common block 
         call GetQ2min(0,pdf_q2min)

c     Workaround for lhapdf-6 bug:
c     re-initialise the pdf set, since getq2min in lhapdf v6
c     can change what was chosen with the first call to pdfset
         ndns0=ndns
         parm(1)='DEFAULT'
         val(1)=ndns
         call pdfset(parm,val)
     

         asmz=alphasPDF(mz)
         write(*,*) ' check: alpha_s(Mz)=',asmz
         call GetOrderAs(iord)
         if(iord.eq.0) then
c better to use the NLO formula anyhow;
c we don't have LO alpha around
            lam5=genericxlambdNL(asmz,mz,5)
         elseif(iord.eq.1) then
            lam5=genericxlambdNL(asmz,mz,5)
         elseif(iord.eq.2) then
            lam5=genericxlambdNNL(asmz,mz,5)
         endif
         write(*,*) ' alpha_s order (0,1,2): ',iord
         write(*,*) ' Lambda 5 is ',lam5
         write(*,*) ' LHA value is ',qcdl5

ccccccccccccccccccccccccccccccccccccccccccccccccccc
c     !ER:
c     needed to have pwhg_alphas=alphasPDF(q) 
c     (i.e. POW alpha to match pdf's alpha)
         lam5=genericxlambdNL(asmz,mz,4)
         write(*,*) ' Lambda 4 is ',lam5
         write(*,*) ' LHA value is ',qcdl4
c$$$         call GetNf(nfmax)
c$$$         print*, nfmax
c$$$         call GetThreshold(3,Q)
c$$$         print*, Q
c$$$         call GetThreshold(4,Q)
c$$$         print*, Q
c$$$         call GetThreshold(5,Q)
c$$$         print*, Q
c$$$         call GetThreshold(6,Q)
c$$$         print*, Q
c$$$         stop
ccccccccccccccccccccccccccccccccccccccccccccccccccc

c         if(iord.lt.2) then
c            write(*,*) ' alfa(mz):',alfas(mz**2,lam5,5)
c         else
c            write(*,*) ' alfa(mz):',alfa3(mz**2,lam5,5)
c         endif
      endif
      end

      function genericxlambdL(as,q,nf)
      implicit none
      real * 8 genericxlambdL,as,q
      integer nf
      real * 8 pi,b,t,xlt,ot,as0,as1
      parameter (pi=3.14159265358979312D0)
      b  = (33-2*nf)/pi/12
      t  = 1/b/as
    1 xlt = log(t)
      ot = t
c-----------------------------------------------------------
c Value and Derivative of alfa with respect to t
      as0  = 1/b/t
      as1  = - 1/b/t**2
      t  = (as-as0)/as1 + t
      if(abs(ot-t)/ot.gt.0.00000001d0)goto 1
      genericxlambdL = q/exp(t/2)
      return
      end

      function genericxlambdNL(as,q,nf)
      implicit none
      real * 8 genericxlambdNL,as,q
      integer nf
      real * 8 pi,b,bp,t,xlt,ot,as0,as1
      parameter (pi=3.14159265358979312D0)
      b  = (33-2*nf)/pi/12
      bp = (153 - 19*nf) / pi / 2 / (33 - 2*nf)
      t  = 1/b/as
    1 xlt = log(t)
      ot = t
c-----------------------------------------------------------
c Value and Derivative of alfa with respect to t
      as0  = 1/b/t - bp*xlt/(b*t)**2
      as1  = - 1/b/t**2 -bp/b**2*(1-2*xlt)/t**3
      t  = (as-as0)/as1 + t
      if(abs(ot-t)/ot.gt.0.00000001d0)goto 1
      genericxlambdNL = q/exp(t/2)
      return
      end

      function genericxlambdNNL(as,q,nf)
      implicit none
      real * 8 genericxlambdNNL,as,q
      integer nf
      real * 8 pi,b0,b1,b2,t,xlt,ot,as0,as1
      integer icount
      parameter (pi=3.14159265358979312D0)
      b0  = (33.d0-2.d0*nf)/(12.d0*pi)
      b1  = (153.d0 - 19.d0*nf) / (24.d0*pi**2)
      b2  = (2857.d0/2.d0-5033.d0/18.d0*nf+325.d0/54.d0*nf**2)
     #     /(4.d0*pi)**3
      t  = 1/b0/as
      icount=0
    1 xlt = log(t)
      if(icount.gt.10000) then
          write(*,*) ' xlambd: cannot converge '
          stop
      endif
      icount=icount+1
      ot = t
c-----------------------------------------------------------
c Value and Derivative of alfa with respect to t
      as0  =   1/(t*b0)*(1-b1/b0**2*log(t)/t
     #         +(b1/b0**2*log(t)/t)**2
     #       -(b1**2*(log(t)+1)-b0*b2)/b0**4/t**2)
      as1  =
     5(-2*b1**2*log(t)**2/(b0**4*t**3)+2*(b1**2*(log(t)+1)-b0*b2)/(b0**4
     1   *t**3)+b1*log(t)/(b0**2*t**2)+2*b1**2*log(t)/(b0**4*t**3)-b1/(b
     2   0**2*t**2)-b1**2/(b0**4*t**3))/(b0*t)-(b1**2*log(t)**2/(b0**4*t
     3   **2)-(b1**2*(log(t)+1)-b0*b2)/(b0**4*t**2)-b1*log(t)/(b0**2*t)+
     4   1)/(b0*t**2)
      t  = (as-as0)/as1 + t
      if(abs(ot-t)/ot.gt.0.00000001d0)goto 1
      genericxlambdNNL = q/exp(t/2)
      return
      end

      subroutine genericpdf(ndns,ih,xmu2,x,fx)
c Interface to lhapdf package.
      implicit none
      integer ndns,ih
      real * 8 xmu2,x,fx(-6:6)
      integer j
      real * 8 tmp
      call genericpdfset(ndns)
      call evolvePDF(x,sqrt(xmu2),fx)
c pftopdg returns density times x
      do j=-6,6
         fx(j)=fx(j)/x
      enddo
c 1 is proton, -1 is antiproton, 3 is pi+, -3 is pi-
      if(ih.eq.1) then
         return
      elseif(ih.eq.-1) then
         do j=1,6
            tmp=fx(j)
            fx(j)=fx(-j)
            fx(-j)=tmp
         enddo
c Do the neutron (Suggested by Jan Kretzschmar, 17/4/2013)
      elseif(abs(ih).eq.2) then
c 2 is neutron: exchange u and d pdfs
         tmp=fx(1)
         fx(1)=fx(2)
         fx(2)=tmp
         tmp=fx(-1)
         fx(-1)=fx(-2)
         fx(-2)=tmp
         if(ih.eq.-2) then
c -2 is anti-neutron: exchange quarks and anti-quarks, then u and d pdfs
            do j=1,6
               tmp=fx(j)
               fx(j)=fx(-j)
               fx(-j)=tmp
            enddo
         endif
      elseif(ih.eq.3) then
         tmp=fx(1)
         fx(1)=fx(-1)
         fx(-1)=tmp
      elseif(ih.eq.-3) then
         tmp=fx(2)
         fx(2)=fx(-2)
         fx(-2)=tmp
      elseif(ih.eq.0) then
c 0 is deuteron
        fx(1)  = 0.5 * ( fx(1)+fx(2) )
        fx(-1) = 0.5 * ( fx(-1)+fx(-2) )
        fx(2)  = fx(1)
        fx(-2) = fx(-1)         
      elseif(ih.eq.4) then
c photon pdf
         continue
      else
         write(*,*) ' genericpdf: unimplemented hadron type ',ih
         stop
      endif
c Bug fixes for version 5.3 of lhapdf
      if(ndns.eq.363) then
         do j=1,6
            fx(j)=fx(j)/2
            fx(-j)=fx(-j)/2
         enddo
      endif
      
      end

      subroutine genericpdfpar(ndns,ih,xlam,scheme,iorder,iret)
      implicit none
      integer ndns,ih,iorder
      real * 8 xlam
      character * 2 scheme
      integer iret
      real * 8 lam5
      integer iord
      common/cgenericpdf/lam5,iord
      call genericpdfset(ndns)
      scheme='MS'
      iret=0
      xlam=lam5
      iorder=iord
      end

      function whichpdfpk()
      character * 3 whichpdfpk
      whichpdfpk='lha'
      end
