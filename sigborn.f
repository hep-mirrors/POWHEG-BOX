      subroutine btildeborn(res)
      implicit none
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_br.h'
      include 'include/pwhg_flg.h'
      real * 8 pdf1(-6:6),pdf2(-6:6)
      real * 8 res(flst_nborn),tot
      integer j
      call pdfcall(1,kn_xb1,pdf1)
      call pdfcall(2,kn_xb2,pdf2)
      tot=0
      do j=1,flst_nborn
         res(j)=br_born(j) *
     #  pdf1(flst_born(1,j))*pdf2(flst_born(2,j))*kn_jacborn
         tot=tot+res(j)
      enddo
      end

      subroutine sigborn_rad(born)
      implicit none
      real * 8 born
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_br.h'
      include 'include/pwhg_rad.h'
      real * 8 pdf1(-6:6),pdf2(-6:6),bornjk(nlegborn,nlegborn),
     #         bmunu(0:3,0:3,nlegborn)
      call pdfcall(1,kn_xb1,pdf1)
      call pdfcall(2,kn_xb2,pdf2)
      call setborn0(kn_cmpborn,flst_born(1,rad_ubornidx),born,
     #        bornjk,bmunu)
      born=born *
     #  pdf1(flst_born(1,rad_ubornidx))*pdf2(flst_born(2,rad_ubornidx))
      end

     

      subroutine allborn
      implicit none
      include 'include/pwhg_flst.h'
      include 'include/pwhg_kn.h'
      include 'include/pwhg_br.h'
      include 'include/pwhg_flg.h'
      integer equivto(maxprocborn)
      real * 8 equivcoef(maxprocborn)
      integer nmomset
      parameter (nmomset=10)
      real * 8 pborn(0:3,nlegborn,nmomset),cprop
      real * 8 born(nmomset,maxprocborn)
      integer iborn,ibornpr,mu,nu,k,j,iret
      logical ini
      data ini/.true./
      save ini,equivto,equivcoef
      if(ini) then
         do iborn=1,flst_nborn
            equivto(iborn)=-1
         enddo
         if(flg_smartsig) then
            call randomsave
            call fillmomenta(nlegborn,nmomset,kn_masses,pborn)
            do iborn=1,flst_nborn
               do j=1,nmomset
                  call setborn0(pborn(0,1,j),flst_born(1,iborn),
     1                 born(j,iborn),br_bornjk(1,1,iborn),
     2                 br_bmunu(0,0,1,iborn))
               enddo
               call compare_vecsb(nmomset,iborn,born,ibornpr,cprop,iret)
               if(iret.eq.0) then
                  equivto(iborn)=ibornpr
                  equivcoef(iborn)=1
               elseif(iret.eq.1) then
                  equivto(iborn)=ibornpr
                  equivcoef(iborn)=cprop
               endif
            enddo
            call randomrestore
         endif
         ini=.false.
      endif
      do iborn=1,flst_nborn
         if(equivto(iborn).lt.0) then
            call setborn0(kn_cmpborn,flst_born(1,iborn),br_born(iborn),
     #        br_bornjk(1,1,iborn),br_bmunu(0,0,1,iborn))
         else
            br_born(iborn)=br_born(equivto(iborn))*equivcoef(iborn)
            do j=1,nlegborn
               do k=1,nlegborn
                  br_bornjk(j,k,iborn)=br_bornjk(j,k,equivto(iborn))
     #*equivcoef(iborn)
               enddo
            enddo
            do mu=0,3
               do nu=0,3
                  do j=1,nlegborn
                     br_bmunu(mu,nu,j,iborn)=
     #                   br_bmunu(mu,nu,j,equivto(iborn))
     #                   *equivcoef(iborn)
                  enddo
               enddo
            enddo
         endif
      enddo
      end

      subroutine compare_vecsb(nmomset,iborn,res,ibornpr,cprop,iret)
      implicit none
      real * 8 ep
      parameter (ep=1d-12)
      integer nmomset,iborn,ibornpr,iret,j,k
      real * 8 res(nmomset,iborn),cprop,rat
      do j=1,iborn-1
         rat=res(1,iborn)/res(1,j)
         do k=1,nmomset
            if(abs(1-res(k,iborn)/res(k,j)/rat).gt.ep) goto 10
         enddo
         if(abs(1-rat).lt.ep) then
            iret=0
            cprop=1
         else
            iret=1
            cprop=rat
         endif
         ibornpr=j
         return
 10      continue
      enddo
      iret=-1
      end



      subroutine setborn0(p,bflav,born,bornjk,bmunu)
c provide the flux factor to the user Born routine
      implicit none
      include '../include/pwhg_math.h'
      include '../include/pwhg_flst.h'
      integer nlegs
      parameter (nlegs=nlegborn)
      real * 8 p(0:3,nlegs)
      integer bflav(nlegs)
      real * 8 born,bornjk(nlegs,nlegs),bmunu(0:3,0:3,nlegs)
      integer j,k,mu,nu
      call setborn(p,bflav,born,bornjk,bmunu)
      born=born/(2*kn_sborn)
      do mu=0,3
         do nu=0,3
            bbmunu(mu,nu)=bbmunu(mu,nu)/(2*kn_sborn)
         enddo
      enddo
      do j=1,nlegs
         do k=1,nlegs
            bornjk(j,k)=bornjk(j,k)/(2*kn_sborn)
         enddo
      enddo
      end
