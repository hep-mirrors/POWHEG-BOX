      subroutine pwhgplotter(p,pjet,wt0,nd)
      implicit none
      include 'hepevt.h'
      include 'masses.f'
      double precision p(12,4),pjet(12,4),ph(4),wt0,pt(1:4),
     1     d1t,d2t,d1a,d2a
      integer switch,nd,ond,nproc,j
      common/nproc/nproc
      character *4 part 
      common/part/part
      logical first
      data first/.true./
      save first,ond
      double precision pjetcom(12,4),wt
      common/pjetcom/pjetcom
c from femto to pico
      wt=wt0/1000
      if(first) then
c This is trick to communicate the number of jets
c to the histogram booking section
         if(nproc.eq.44) then
            nhep=6
         elseif(nproc.eq.203) then
            nhep=6
         elseif(nproc.ge.141.or.nproc.le.151) then
            nhep=12
         else
            stop 'process not implemented' 
         endif
         call init_hist
         ond=0
         first = .false. 
      else
c put partons in the hephep interface
         if(nproc.eq.44) then
            nhep=6
         elseif(nproc.eq.203) then
            nhep=5
         elseif(nproc.ge.141.or.nproc.le.151) then
            nhep=12
         else
            stop 'process not implemented' 
         endif

         if(part.eq.'real'.and.nd.eq.0) then
            nhep=nhep+1
         endif
         if(nproc.eq.203) then
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
         else
            do j=1,nhep
               phep(1:4,j)=p(j,:)
            enddo
            do j=1,12
               pjetcom(j,1:4)=pjet(j,1:4)
            enddo
         endif

         if(nproc.eq.44) then
            do j=1,nhep
               phep(1:4,j)=p(j,:)
            enddo
         endif
            

         if(nproc.ge.141.or.nproc.le.151) then
            phep(1:4,3)=p(3,:)+p(4,:)+p(5,:)
            phep(1:4,4)=p(6,:)+p(7,:)+p(8,:)
            phep(1:4,5)=p(3,:)+p(4,:)
            phep(1:4,6)=p(7,:)+p(8,:)
            phep(1:4,7)=p(4,:)
            phep(1:4,8)=p(3,:)
            phep(1:4,9)=p(7,:)
            phep(1:4,10)=p(8,:)
            phep(1:4,11)=p(5,:)
            phep(1:4,12)=p(6,:)
            if(nhep.eq.13) then
                phep(1:4,13)=p(9,:)
c is 3 a full top?
                pt=phep(1:4,3)
                d1t=abs(pt(4)**2-pt(1)**2-pt(2)**2-pt(3)**2-mt**2)/mt**2
                pt=pt+p(9,:)
                d2t=abs(pt(4)**2-pt(1)**2-pt(2)**2-pt(3)**2-mt**2)/mt**2
                pt=phep(1:4,4)
                d1a=abs(pt(4)**2-pt(1)**2-pt(2)**2-pt(3)**2-mt**2)/mt**2
                pt=pt+p(9,:)
                d2a=abs(pt(4)**2-pt(1)**2-pt(2)**2-pt(3)**2-mt**2)/mt**2
c                write(*,*) d1t,d2t,d1a,d2a
                if(d2t.lt.d1t.and.d2a.ge.d2t) then
c                   write(*,*) ' t with radiation'
                   phep(1:4,3)=phep(1:4,3)+phep(1:4,13)
                elseif(d2a.lt.d1a.and.d2t.ge.d2a) then
c                   write(*,*) ' at with radiation'
                   phep(1:4,4)=phep(1:4,4)+phep(1:4,13)
                endif
c                read(*,*)
            endif
            idhep(3)=6
            idhep(4)=-6
         endif

         do j=1,nhep
            idhep(j)=21
            isthep(j)=1
         enddo
         isthep(1)=-1
         isthep(2)=-1
         if(nproc.eq.11.or.nproc.eq.22.or.nproc.eq.44) then
            idhep(3)=11
            idhep(4)=-11
         elseif(nproc.eq.203) then
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
