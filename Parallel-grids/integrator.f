c Integrator-Unweighter Package for POWHEG;
c Documented in arXiv:0709.2085: MINT: A Computer program for adaptive Monte Carlo
c     integration and generation of unweighted distributions.
c     Variations with respect to the version documented in the manual may be present
c     here, and illustrated in the comments.
c     In this version, no internal state variables are present (i.e. all internal
c     state variables must be passed as arguments). In this way, one can use mint
c     and gen for the integration-generation of different functions, provided
c     the arguments are kept different.
c

      subroutine mint(fun,ndim,ncalls0,nitmax,ifold,imode,iun,
     #     xgrid,xint,ymax,ans,err)
c     returns the function to be integrated multiplied by www;
c     xx(1:ndim) are the variables of integration
c     ifirst=0: normal behaviour
c     ifirst=1 and 2: see the explanation of ifold
c
c ndim=number of dimensions
c
c ncalls0=# of calls per iteration
c
c nitmax =# of iterations
c
c ifold(ndim)
c     If some number in the array ifold, (say, ifold(n))
c     is different from 1, it must be a divisor of 50, and the 50 intervals xgrid(0:50,n)
c     are grouped into ifold(n) groups, each group containing 50/ifold(n) nearby
c     intervals. For example, if ifold(1)=5, the 50 intervals for the first dimension
c     are divided in 5 groups of 10. The integral is then performed by folding on top
c     of each other these 5 groups. Suppose, for example, that we choose a random point
c     in xx(1) = xgrid(2,1)+x*(xgrid(3,1)-xgrid(2,1)), in the group of the first 5 interval.
c     we sum the contribution of this point to the contributions of points
c     xgrid(2+m*10,1)+x*(xgrid(3+m*10,1)-xgrid(2+m*10,1)), with m=1,...,4.
c     ifirst=0,1,2
c     In the folded sequence of calls to the
c     function fun, the call for the first point is performed with ifirst=0, and that for
c     all subsequent points with ifirst=1, so that the function can avoid to compute
c     quantities that only depend upon dimensions that have ifold=1, and do not change
c     in each group of folded call. The values returned by fun in a sequence of folded
c     calls with ifirst=0 and ifirst=1 are not used. The function itself must accumulate
c     the values, and must return them when called with ifirst=2.
c
c imode: integer flag
c     imode=0:
c     When called with imode=0 the routine integrates the absolute value of the function
c     and sets up a grid xgrid(0:50,ndim) such that in each ndim-1 dimensional slice
c     (i.e. xgrid(m-1,n)<xx(n)<xgrid(m,n)) the contribution of the integral is the same
c     the array xgrid is setup at this stage; ans and err are the integral and its error.
c     No folding is allowed at this stage;
c     imode=1 (in fact #0)
c     When called with imode=1, the routine performs the integral of the function fun
c     using the grid xgrid. Folding, if present, is used at this stage. The upper bounding
c     envelope of the function is set up, so as to allow event generation by gen.
c
c xgrid(0:nintervals,ndim)
c     integration grid; initialized and updated with the call to mint with imode=0
c 
c xint: real
c     Output value of the integral when called with imode=0,
c     input value of the integral when called with imode=1 (cannot be zero here!)
c
c ymax(nintervals,ndim):
c     integrand upper bounds, set up by mint when called with imode=1, to be used
c     by the subroutine gen for the generation of unweighted events
c
c ans: real
c     Output value of the integral (both imode=0 and imode=1)
c
c err: real
c     Output value of the error on the integral
c
      implicit none
      integer nintervals,ndimmax
      include 'nlegborn.h'
      include 'multigrid.h'
      parameter (nintervals=50,ndimmax=ndiminteg)
      integer ncalls0,ndim,nitmax,imode,iun
      real * 8 fun,xgrid(0:nintervals,ndim),xint,ymax(nintervals,ndim),
     #  ans,err
      real * 8 x(ndimmax),vol
      real * 8 xacc(0:nintervals,ndimmax)
      integer icell(ndimmax),ncell(ndimmax)
      integer ifold(ndimmax),kfold(ndimmax)
      integer nhits(1:nintervals,ndimmax),ndimc
c this common makes the accumulated information, in the phase
c of determinatio of the imortance sampling grid, available to
c store intermediate results
      common/mintregrid/xacc,nhits,ndimc
      integer iunregrid
      common/iunregrid/iunregrid
      real * 8 rand(ndimmax)
      real * 8 dx(ndimmax),f,vtot,etot,prod,vfun
      integer kdim,kint,kpoint,nit,ncalls,ibin,iret,nintcurr,ifirst
      real * 8 random,powheginput
      logical pwhg_isfinite,gridinfo
      external random,pwhg_isfinite,fun,powheginput

      if(ndim.gt.ndiminteg) then
         write(*,*) 'Mint: at most ',ndiminteg,' dimensions'
         write(*,*) 'Got ',ndim
         call exit(-1)
      endif
      iunregrid = iun
      ndimc = ndim
      if(imode.eq.0) then
         do kdim=1,ndim
            ifold(kdim)=1
         enddo
         iret = -1
         if(flg_multigrid.ge.0) then
            if(powheginput("#manyseeds").ne.1) then
               write(*,*)
     1              ' with multigrid you should also set manyseeds'
               call exit(-1)
            endif
            if(nitmax.ne.0.and.nitmax.ne.1) then
               write(*,*)
     1              ' with multigrid ncall1 should be 0 or 1'
               call exit(-1)
            endif
            if(flg_multigrid.eq.0) then
               call loadgridinfo(xgrid,xint,iret)
               if(iret.ne.0) then
                  write(*,*)  ' could not load grid info!'
                  call exit(-1)
               endif
               return
            endif
c In this case we don't want to reinitialize the grid
            if(flg_multigrid.gt.1) iret = 0
         endif
         if(iret.ne.0) then
            do kdim=1,ndim
               do kint=0,nintervals
                  xgrid(kint,kdim)=dble(kint)/nintervals
               enddo
            enddo
         endif
      elseif(imode.eq.1) then
         do kdim=1,ndim
            nintcurr=nintervals/ifold(kdim)
            if(nintcurr*ifold(kdim).ne.nintervals) then
               write(*,*)
     1    'mint: the values in the ifold array shoud be divisors of',
     2              nintervals
               stop
            endif
            do kint=1,nintcurr
               ymax(kint,kdim)=
     1              xint**(1d0/ndim)
            enddo
         enddo
      endif
      ncalls=ncalls0
      nit=0
      ans=0
      err=0
 10   continue
      nit=nit+1
 11   continue
      if(nit.gt.nitmax) then
         if(imode.eq.0) xint=ans
         return
      endif
      if(imode.eq.0) then
         do kdim=1,ndim
            do kint=0,nintervals
               xacc(kint,kdim)=0
               if(kint.gt.0) then
                  nhits(kint,kdim)=0
               endif
            enddo
         enddo
      endif
      vtot=0
      etot=0
      do kpoint=1,ncalls
c find random x, and its random cell
         do kdim=1,ndim
            kfold(kdim)=1
            ncell(kdim)=nintervals/ifold(kdim)*random()+1
            rand(kdim)=random()
         enddo
         f=0
         ifirst=0
 1       continue
         vol=1
         do kdim=1,ndim
            nintcurr=nintervals/ifold(kdim)
            icell(kdim)=ncell(kdim)+(kfold(kdim)-1)*nintcurr
            ibin=icell(kdim)
            dx(kdim)=xgrid(icell(kdim),kdim)-xgrid(icell(kdim)-1,kdim)
            vol=vol*dx(kdim)*nintcurr
            x(kdim)=xgrid(icell(kdim)-1,kdim)+rand(kdim)*dx(kdim)
            if(imode.eq.0) nhits(ibin,kdim)=nhits(ibin,kdim)+1
         enddo
c contribution to integral
         if(imode.eq.0) then
            vfun=abs(fun(x,vol,ifirst))
c If you get NaN or Inf, skip this value
            if(.not.pwhg_isfinite(vfun)) goto 11
            f=vfun+f
            f=fun(x,vol,2)
            if(.not.pwhg_isfinite(f)) goto 11
         else
c this accumulated value will not be used
            vfun=fun(x,vol,ifirst)
            if(.not.pwhg_isfinite(vfun)) goto 11
            f=vfun+f
            ifirst=1
            call nextlexi(ndim,ifold,kfold,iret)
            if(iret.eq.0) goto 1
c closing call: accumulated value with correct sign
            f=fun(x,vol,2)
            if(.not.pwhg_isfinite(f)) goto 11
         endif
c
         if(imode.eq.0) then
c accumulate the function in xacc(icell(kdim),kdim) to adjust the grid later
            do kdim=1,ndim
               xacc(icell(kdim),kdim)=xacc(icell(kdim),kdim)+f
            enddo
         else
c update the upper bounding envelope
            prod=1
            do kdim=1,ndim
               prod=prod*ymax(ncell(kdim),kdim)
            enddo
            prod=(f/prod)
            if(prod.gt.1) then
c This guarantees a 10% increase of the upper bound in this cell
               prod=1+0.1d0/ndim
               do kdim=1,ndim
                  ymax(ncell(kdim),kdim)=ymax(ncell(kdim),kdim)
     #          * prod
               enddo
            endif
         endif
         vtot=vtot+f/ncalls
         etot=etot+f**2/ncalls
      enddo
c the abs is to avoid tiny negative values
      etot=sqrt(abs(etot-vtot**2)/ncalls)
      write(*,*) vtot,etot
      if(nit.eq.1) then
         ans=vtot
         err=etot
      else
c prevent annoying division by zero for nearly zero
c integrands
         if(etot.eq.0.and.err.eq.0) then
            if(ans.eq.vtot) then
               goto 10
            else
               err=abs(vtot-ans)
               etot=abs(vtot-ans)
            endif
         elseif(etot.eq.0) then
            etot=err
         elseif(err.eq.0) then
            err=etot
         endif
c The following formulae are such that by doing nitmax iterations
c with ncalls call the result is exactly equivalent to do 1 iteration with
c nitmax*ncalls calls.
         err=sqrt((err**2*(nit-1)**2+etot**2)/nit**2
     1     +(nit-1)*(ans-vtot)**2/(ncalls*nit**3))
         ans=((nit-1)*ans+vtot)/nit
      endif
      if(imode.eq.0) then
c iteration is finished; now rearrange the grid
c flg_multigrid = 1 or 3
         if(flg_multigrid.gt.0) then
            call storegridinfo(xgrid,ans)
         endif
         do kdim=1,ndim
            call regrid(xacc(0,kdim),xgrid(0,kdim),
     #           nhits(1,kdim),kdim,nintervals)
         enddo
      endif

      goto 10
      end

      subroutine regrid(xacc,xgrid,nhits,kdim,nint)
      implicit none
      integer  nint,nhits(nint),kdim,nit
      real * 8 xacc(0:nint),xgrid(0:nint)
      integer nintervals
      parameter (nintervals=50)
      real * 8 xn(nintervals),r
      integer kint,jint
      integer iun
      common/iunregrid/iun
      do kint=1,nint
c xacc (xerr) already contains a factor equal to the interval size
c Thus the integral of rho is performed by summing up
         if(nhits(kint).ne.0) then
            xacc(kint)= xacc(kint-1)
     #           + abs(xacc(kint))/nhits(kint)
         else
            xacc(kint)=xacc(kint-1)
         endif
      enddo
c If there is no value, keep old grid!
      if(xacc(nint).eq.0) return
      do kint=1,nint
         xacc(kint)=xacc(kint)/xacc(nint)
      enddo
      write(iun,*) 'set limits x 0 1 y 0 1'
      write(iun,*) ' title top "dim=',kdim,'"'
      write(iun,*) 0, 0
      do kint=1,nint
         write(iun,*) xgrid(kint),xacc(kint)
      enddo
      write(iun,*) 'join 0'

      do kint=1,nint
         r=dble(kint)/nint

         write(iun,*) 0, r
         write(iun,*) 1, r
         write(iun,*) ' join'

         do jint=1,nint
            if(r.lt.xacc(jint)) then
               xn(kint)=xgrid(jint-1)+(r-xacc(jint-1))
     #        /(xacc(jint)-xacc(jint-1))*(xgrid(jint)-xgrid(jint-1))
               goto 11
            endif
         enddo
         if(jint.ne.nint+1.and.kint.ne.nint) then
            write(*,*) ' error',jint,nint
            stop
         endif
         xn(nint)=1
 11      continue
      enddo
      do kint=1,nint
         xgrid(kint)=xn(kint)
         write(iun,*) xgrid(kint), 0
         write(iun,*) xgrid(kint), 1
         write(iun,*) ' join'
      enddo
      write(iun,*) ' newplot'
      end


c      implicit none
c      integer ndim
c      parameter (ndim=3)
c      integer iii(ndim),kkk(ndim)
c      integer j,iret
c      data kkk/1,1,1/
c      data iii/2,3,2/
c 1    continue
c      write(*,*) (kkk(j),j=1,3)
c      call nextlexi(ndim,iii,kkk,iret)
c      if(iret.eq.0) then
c         goto 1
c      endif
c      end

      subroutine nextlexi(ndim,iii,kkk,iret)
c kkk: array of integers 1 <= kkk(j) <= iii(j), j=1,ndim
c at each call iii is increased lexicographycally.
c for example, starting from ndim=3, kkk=(1,1,1), iii=(2,3,2)
c subsequent calls to nextlexi return
c         kkk(1)      kkk(2)      kkk(3)    iret
c 0 calls   1           1           1       0
c 1         1           1           2       0    
c 2         1           2           1       0
c 3         1           2           2       0
c 4         1           3           1       0
c 5         1           3           2       0
c 6         2           1           1       0
c 7         2           1           2       0
c 8         2           2           1       0
c 9         2           2           2       0
c 10        2           3           1       0
c 11        2           3           2       0
c 12        2           3           2       1
      implicit none
      integer ndim,iret,kkk(ndim),iii(ndim)
      integer k
      k=ndim
 1    continue
      if(kkk(k).lt.iii(k)) then
         kkk(k)=kkk(k)+1
         iret=0
         return
      else
         kkk(k)=1
         k=k-1
         if(k.eq.0) then
            iret=1
            return
         endif
         goto 1
      endif
      end


      subroutine gen(fun,ndim,xgrid,ymax,xmmm,ifold,
     #     imode,mcalls,icalls,x)
c Subroutine to generated x(ndim) point distributed
c according to the function fun.
c
c fun: same as in mint
c
c ndim: same as in mint
c
c xgrid: same as in mint
c
c xmmm(nintervals,ndim):
c     internal array, initialized when
c     called with imode=0
c
c ifold: same as in mint
c
c imode:
c     imode=0 to initialize
c     imode=1 to generate
c     imode=3 store generation efficiency in x(1)
c
c mcalls:
c     current number of calls with imode=1
c
c icalls
c     current number of calls to the function fun
c
c x(ndim) 
c     the returned coordinate vector of the generated point
      implicit none
      integer ndim,imode
      integer nintervals,ndimmax
      include 'nlegborn.h'
      parameter (nintervals=50,ndimmax=ndiminteg)
      real * 8 fun,xgrid(0:nintervals,ndim),
     #         ymax(nintervals,ndim),x(ndim)
      real * 8 dx(ndimmax)
      integer icell(ndimmax),ncell(ndimmax)
      integer ifold(ndimmax),kfold(ndimmax)
      real * 8 r,f,ubound,vol,vfun,random,xmmm(nintervals,ndimmax)
      real * 8 rand(ndimmax)
      logical pwhg_isfinite
      external fun,random,pwhg_isfinite
      integer icalls,mcalls,kdim,kint,nintcurr,iret,ifirst
      if(ndim.gt.ndiminteg) then
         write(*,*) 'Mint: at most ',ndiminteg,' dimensions'
         write(*,*) 'Got ',ndim
         call exit(-1)
      endif
      if(imode.eq.0) then
         do kdim=1,ndim
            nintcurr=nintervals/ifold(kdim)
            xmmm(1,kdim)=ymax(1,kdim)
            do kint=2,nintcurr
               xmmm(kint,kdim)=xmmm(kint-1,kdim)+
     #              ymax(kint,kdim)
            enddo
            do kint=1,nintcurr
               xmmm(kint,kdim)=xmmm(kint,kdim)/xmmm(nintcurr,kdim)
            enddo
         enddo
         icalls=0
         mcalls=0
         return
      elseif(imode.eq.3) then
         if(icalls.gt.0) then
            x(1)=dble(mcalls)/icalls
         else
            x(1)=-1
         endif
         return
      endif
      mcalls=mcalls+1
 10   continue
      do kdim=1,ndim
         nintcurr=nintervals/ifold(kdim)
         r=random()
         do kint=1,nintcurr
            if(r.lt.xmmm(kint,kdim)) then
               ncell(kdim)=kint
               goto 1
            endif
         enddo
 1       continue
         rand(kdim)=random()
      enddo
      ubound=1
      do kdim=1,ndim
         ubound=ubound*ymax(ncell(kdim),kdim)
      enddo
      do kdim=1,ndim
         kfold(kdim)=1
      enddo
      f=0
      ifirst=0
 5    continue
      vol=1
      do kdim=1,ndim
         nintcurr=nintervals/ifold(kdim)
         icell(kdim)=ncell(kdim)+(kfold(kdim)-1)*nintcurr
         dx(kdim)=xgrid(icell(kdim),kdim)-xgrid(icell(kdim)-1,kdim)
         vol=vol*dx(kdim)*nintervals/ifold(kdim)
         x(kdim)=xgrid(icell(kdim)-1,kdim)+rand(kdim)*dx(kdim)
      enddo
      vfun=fun(x,vol,ifirst)
      if(.not.pwhg_isfinite(vfun)) goto 10
      f=f+vfun
      ifirst=1
      call nextlexi(ndim,ifold,kfold,iret)
      if(iret.eq.0) goto 5
c get final value (x and vol not used in this call)
      f=fun(x,vol,2)
      if(.not.pwhg_isfinite(f)) goto 10
      if(f.lt.0) then
         write(*,*) 'gen: non positive function',f
c         f=fun(x,vol,2)
c         stop
      endif
      if(f.gt.ubound) then
         call increasecnt
     #       ('upper bound failure in inclusive cross section')
      endif
      ubound=ubound*random()
      icalls=icalls+1
      if(ubound.gt.f) then
         call increasecnt
     #       ('vetoed calls in inclusive cross section')
         goto 10
      endif
      end



      subroutine storegridinfo(xgrid,ans)
      implicit none
      include 'nlegborn.h'
      include 'pwhg_rnd.h'
      include 'storelabel.h'
      integer nintervals,ndimmax
      parameter (nintervals=50,ndimmax=ndiminteg)
      real * 8 xgrid(0:nintervals,*),ans
      real * 8 xacc(0:nintervals,ndimmax)
      integer nhits(1:nintervals,ndimmax),ndimc
      common/mintregrid/xacc,nhits,ndimc
      character * 20 pwgprefix
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      integer j,k,iun
      call newunit(iun)
      open(unit=iun,
     1     file=pwgprefix(1:lprefix)//'gridinfo-'//storelabel//'-'
     2     //rnd_cwhichseed//'.dat',form='unformatted',status='unknown')
      write(iun) ndimc
      write(iun) ans
      write(iun) xgrid(0:nintervals,1:ndimc)
      write(iun) xacc(0:nintervals,1:ndimc)
      write(iun) nhits(1:nintervals,1:ndimc)
      close(iun)
      end

      subroutine loadgridinfo(xgrid,ans,iret)
      implicit none
      include 'nlegborn.h'
      include 'pwhg_rnd.h'
      include 'storelabel.h'
      integer nintervals,ndimmax
      parameter (nintervals=50,ndimmax=ndiminteg)
      real * 8 xgrid(0:nintervals,*),ans
      integer iret
      real * 8 xacc(0:nintervals,ndimmax)
      integer nhits(1:nintervals,ndimmax),ndimc
      common/mintregrid/xacc,nhits,ndimc
      real * 8 xacc0(0:nintervals,ndimmax),tmp
      integer nhits0(1:nintervals,ndimmax),ndim0
      character * 4 chseed
      character * 20 pwgprefix
      character * 100 file
      integer lprefix
      common/cpwgprefix/pwgprefix,lprefix
      integer j,k,iun,ios,kdim
      logical filefound,lpresent
      integer nfiles,jfile,jfound
c Are we required to generate a grid file or to load a bunch of them?
c See if the grid file exists already

      inquire(file=pwgprefix(1:lprefix)//'gridinfo-'//
     1     storelabel//'-'//rnd_cwhichseed//'.dat',exist=lpresent)
      if(.not.lpresent) then
         iret=-1
         return
      endif

      nfiles=9999
      filefound=.false.
      jfound=0
      xacc(:,1:ndimc)=0
      nhits(:,1:ndimc)=0
      ans=0
      do jfile=1,nfiles
         write(chseed,'(i4)') jfile
         do k=1,4
            if(chseed(k:k).eq.' ') chseed(k:k)='0'
         enddo
         file = pwgprefix(1:lprefix)//'gridinfo-'//
     1        storelabel//'-'//chseed//'.dat'
         inquire(file= file,exist=lpresent)
         if(.not.lpresent) cycle
         open(unit=iun,file= file,
     2        form='unformatted',status='old',iostat=ios)
         if(ios.ne.0) then
            iret=-1
            return
         else
            write(*,*) ' Opened ',pwgprefix(1:lprefix)//'gridinfo-'//
     1           chseed//'.dat'
         endif
         filefound=.true.
         jfound=jfound+1
         read(iun,iostat=ios) ndim0
         if(ios.ne.0.or.ndim0.ne.ndimc) goto 111
         read(iun,iostat=ios) tmp
         ans = ans + tmp
         if(ios.ne.0) goto 111
         read(iun,iostat=ios) xgrid(0:nintervals,1:ndimc)
         if(ios.ne.0) goto 111
         read(iun,iostat=ios) xacc0(0:nintervals,1:ndimc)
         if(ios.ne.0) goto 111
         xacc(:,1:ndimc) = xacc(:,1:ndimc) + xacc0(:,1:ndimc)
         read(iun,iostat=ios) nhits0(1:nintervals,1:ndimc)
         if(ios.ne.0) goto 111
         nhits(:,1:ndimc) = nhits(:,1:ndimc) + nhits0(:,1:ndimc)
      enddo
      write(*,*) ' loadgridinfo: found ',jfound,
     1     ' files with grid information'
      if(filefound) then
         ans = ans/jfound
         do kdim=1,ndimc
            call regrid(xacc(0,kdim),xgrid(0,kdim),
     1           nhits(1,kdim),kdim,nintervals)
         enddo
         iret = 0
         return
      endif

      iret = -1
      return
 111  continue
      write(*,*) ' loadgridinfo: problems loading files'
      call exit(-1)
      end

