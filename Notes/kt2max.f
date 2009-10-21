      function pt2max(y)
      implicit none
      real * 8 pt2max,y
      real * 8 x1b,x2b
      common/xxx/x1b,x2b
      real * 8 xmp,xmm,xmin
      xmp=2*(1+y)*x1b**2/(sqrt((1+x1b**2)**2*(1-y)**2+16*y*x1b**2)
     # +(1-y)*(1-x1b**2))
      xmm=2*(1-y)*x2b**2/(sqrt((1+x2b**2)**2*(1+y)**2-16*y*x2b**2)
     # +(1+y)*(1-x2b**2))
      xmin=max(xmp,xmm)
      pt2max=1/xmin*(1-xmin)**2*(1-y**2)
      end

      function dfxmin(y,i)
      implicit none
      real * 8 dfxmin,y
      integer i
      real * 8 x1b,x2b
      common/xxx/x1b,x2b
      real * 8 xmin
      common/cdfxmin/xmin
      real * 8 df1,df2
c x=1-csi, and xmin=1-csimax; ximax is given in 5.13 of FNO2006,
c so that xmin is the largest between the two expressions in
c braces in 5.13, corresponding to df1 and df2 below.
c It is easy to check that the minimum is reached when df1=df2
c (in fact df1 is monotonically increasing in y and df2 is
c  monotonically decreasing in y).
c Thus, dfxmin=df1-df2 is solved for zeroes, and at the
c zero xmin=df1=df2.
c thus, after the last call by the solving program, xmin
c (in the cdfxmin common block) is at its minimum
      df1=2*(1+y)*x1b**2/(sqrt((1+x1b**2)**2*(1-y)**2+16*y*x1b**2)
     # +(1-y)*(1-x1b**2))
      df2=2*(1-y)*x2b**2/(sqrt((1+x2b**2)**2*(1+y)**2-16*y*x2b**2)
     # +(1+y)*(1-x2b**2))
      dfxmin=df1-df2
      xmin=max(df1,df2)
      if(xmin.lt.min(x1b,x2b)/4*sqrt(x1b*x2b)) then
         write(*,*) ' problem with bnd'
      endif
      end

      implicit none
      real * 8 pt2max
      real * 8 x1b,x2b
      common/xxx/x1b,x2b
      real * 8 y,tmp
      real * 8 xmin
      common/cdfxmin/xmin
      real * 8 random,dfxmin
      external random,dfxmin
      real * 8 err,kt2max,kt2max1
      integer j,m
      do m=1,100000
      if(10000*(m/10000).eq.m) write(*,*) m
      x1b=random()
      x2b=random()
      call KERSET('C205. ',0,0,101)
      call dzero(-1d0,1d0,y,err,1d-8,1000000,dfxmin)
      if(err.lt.0) then
         write(*,*) ' dzero fails'
      endif
      tmp=dfxmin(y,0)
      kt2max1=1/xmin*(1-xmin)**2*(1-y**2)
      kt2max=0
      do j=1,10000
         y=1-2*random()
         kt2max=max(kt2max,pt2max(y))
      enddo
      if(kt2max.gt.kt2max1) then
         write(*,*) 'problem at x1b,x2b,y=',x1b,x2b,y
      endif
      enddo
      end
      

      function random()
      real * 8 random
      real * 8 saverandom
      logical fixed
      COMMON/tmpfixed/fixed
      data fixed/.false./
      save saverandom
      if(fixed) then
         random=saverandom
         return
      endif
      call rm48(random,1)
      saverandom=random
      end


      subroutine resetrandom
      call RM48IN(54217137,0,0)
      end
