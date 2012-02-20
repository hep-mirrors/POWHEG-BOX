      subroutine pwhgplotter(p,pjet,wt,nd)
      implicit none
      include 'hepevt.h'
      double precision p(12,4),pjet(12,4),ph(4),wt
      integer switch,nd,ond,nproc,j
      common/nproc/nproc
      character *4 part 
      common/part/part
      logical first
      data first/.true./
      save first,ond
      double precision pjetcom(4,12) 
      common/pjetcom/pjetcom
      if(first) then
c This is trick to communicate the number of jets
c to the histogram booking section
         if(nproc.eq.203.or.nproc.eq.204) then
            nhep=6
         else
            stop 'process not implemented' 
         endif
         call init_hist
         ond=0
         first = .false. 
      else
c put partons in the hephep interface
         if(nproc.eq.203.or.nproc.eq.204) then
            nhep=5
         endif
         if(part.eq.'real'.and.nd.eq.0) then
            nhep=nhep+1
         endif
         if(nproc.eq.203.or.nproc.eq.204) then
c            ph=p(3,:)+p(4,:)
c            write(*,*) 
c     1           sqrt(abs(ph(4)**2-ph(1)**2-ph(2)**2-ph(3)**2))
c we want the undecayed Higgs
            do j=1,2
               phep(1:4,j)=p(j,:)
            enddo
            phep(1:4,3)=p(3,:)+p(4,:)
            do j=4,nhep
               phep(1:4,j)=p(j+1,:)
            enddo
            do j=1,12
               pjetcom(1:4,j)=pjet(j,1:4)
            enddo
         else
            do j=1,nhep
               phep(1:4,j)=p(j,:)
            enddo
            do j=1,12
               pjetcom(1:4,j)=pjet(j,1:4)
            enddo
         endif

         do j=1,nhep
            idhep(j)=21
            isthep(j)=1
         enddo
         isthep(1)=-1
         isthep(2)=-1
         if(nproc.eq.11.or.nproc.eq.22) then
            idhep(3)=11
            idhep(4)=-11
         elseif(nproc.eq.203.or.nproc.eq.204) then
            idhep(3)=25
         endif

         if(part.ne.'real') then
            call analysis(wt)
            call pwhgaccumup
         else
            if(nd.eq.0.and.ond.ne.0) then
               call pwhgaccumup
               call analysis(wt)
            else
               call analysis(wt)
            endif
            ond=nd
         endif
      endif

      end


      subroutine pwhghistofin(itno,itmx)
      implicit none
      integer itno,itmx
      character *4 part 
      common/part/part
      if(part.eq.'real') then
c     in this case a call to accumup is missin
         call pwhgaccumup
      endif
c finalize histograms
      call pwhgsetout
c mcfm delivered weights divided by n; should now multiply
c by n to get proper normalization. Furthermore, if itno#0
c we should divide by itno; otherwise we should divide by itmx
      if(itno.gt.0) then
         call pwhgfixmcfm(itno)
      else
         call pwhgfixmcfm(itmx)
      endif
c print histograms
      call pwhgtopout
      end

      subroutine pwhgfixmcfm(it)
c analysis, leaving the yhistarr1 and errhistarr1 unchanged.
      implicit none
      integer it
      include 'pwhg_bookhist-new.h'
      integer j,k
      real *8 xxx,sum,sumsq
      do j=1,jhist
         xxx=ient1(j)
         xxx=xxx/it
         do k=0,nbins(j)+1
            yhistarr2(k,j)=yhistarr2(k,j)*xxx
            errhistarr2(k,j)=errhistarr2(k,j)*xxx
         enddo
      enddo
      end
