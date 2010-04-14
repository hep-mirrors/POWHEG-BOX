      subroutine switchmom(p1,p,ic,jc,nexternal)
c**************************************************************************
c     Changes stuff for crossings
c**************************************************************************
      implicit none
      integer nexternal
      integer jc(nexternal),ic(nexternal)
      real*8 p1(0:3,nexternal),p(0:3,nexternal)
      integer i,j
c-----
c Begin Code
c-----
      do i=1,nexternal
         do j=0,3
            p(j,ic(i))=p1(j,i)
         enddo
      enddo
      do i=1,nexternal
         jc(i)=1
      enddo
      jc(ic(1))=-1
      jc(ic(2))=-1
      end
     
      function flavequiv_perm(n,aflav,bflav,perm)
      implicit none
      logical flavequiv_perm
      integer n, aflav(n),bflav(n),perm(n)
      include '../nlegborn.h'
      include '../../include/pwhg_flst.h'
      integer j,k,kb,itmp,ib(nlegreal)
      call intassign(n,bflav,ib)
      do j=1,n
         perm(j)=j
      enddo
      do j=1,n
         if(aflav(j).ne.ib(j)) then
            if(j.le.2) then
               flavequiv_perm=.false.
               return
            endif
            do k=j+1,n
               if(aflav(j).eq.ib(k)) then
                  itmp=ib(j)
                  ib(j)=ib(k)
                  ib(k)=itmp
                  itmp=perm(j)
                  perm(j)=perm(k)
                  perm(k)=itmp
                  goto 10
               endif
            enddo
            flavequiv_perm=.false.
            return
         endif
 10      continue
      enddo
      flavequiv_perm=.true.
      end


c$$$      function flavequiv_perm(n,aflav,bflav,perm)
c$$$      implicit none
c$$$      include '../nlegborn.h'
c$$$      include '../../include/pwhg_flst.h'
c$$$      logical flavequiv_perm
c$$$      integer n, aflav(nlegreal),bflav(nlegreal),perm(nlegreal)
c$$$      integer j,k,kb,itmp,ib(nlegreal)
c$$$      logical presa(nlegreal)
c$$$      call intassign(n,bflav,ib)
c$$$c$$$      write(*,*) 'ref',(ib(j),j=1,n)
c$$$c$$$      write(*,*) 'current',(aflav(j),j=1,n)
c$$$      do j=1,n
c$$$         presa(j)=.false.
c$$$      enddo
c$$$      do j=1,2
c$$$         perm(j)=j
c$$$         if(ib(j).ne.aflav(j)) goto 66
c$$$      enddo
c$$$      j=3
c$$$ 666  continue
c$$$      if (j.gt.nlegreal) goto 777
c$$$      perm(j)=-1
c$$$      do k=3,n
c$$$c$$$         print*, j,k,presa(k)
c$$$         if(.not.presa(k)) then
c$$$            if(ib(j).eq.aflav(k)) then
c$$$               presa(k)=.true.
c$$$               perm(j)=k
c$$$               j=j+1
c$$$               goto 666
c$$$            endif
c$$$         endif
c$$$      enddo
c$$$ 777  continue
c$$$      do j=1,n
c$$$c$$$         print*, perm(j)
c$$$         if(perm(j).lt.0) then
c$$$            goto 66
c$$$         endif
c$$$      enddo
c$$$      flavequiv_perm=.true.
c$$$c$$$      write(*,*) 'current',(aflav(j),j=1,n), ' TRUE'
c$$$      return
c$$$ 66   continue
c$$$      flavequiv_perm=.false.
c$$$      return
c$$$      end
