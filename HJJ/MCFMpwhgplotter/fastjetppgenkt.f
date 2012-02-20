
      subroutine fastjetppgenkt(ptrack,ntracks,r,palg,ptmin,pjet,njets,
     $                        jetvec)
      integer ntracks,palg,njets,jetvac(ntracks)
      real * 8 ptrack(4,ntracks),pjet(4,njets),
     1     pj(ntracks,4),pj_out(ntracks,4)
      real * 8 lscale,muref
      integer irec
      do j=1,ntracks
         pj(j,:)=ptracks(:,j)
      enddo
      do j=1,ntracks-njets
         call loc_scale(pj(1:ntracks-j+1,:),pj_out(1:ntracks-j,:),
     1        lscale,muf,irec)
         pj(1:ntracks-j,:)=pj_out(1:ntracks-j,:)
      enddo
      do j=1,njets
         pjet(:,j,)=pj(j,:)
      enddo
      
      
      
      
