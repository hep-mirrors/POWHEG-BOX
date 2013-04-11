      subroutine setborn(p,bflav,born,bornjk,bmunu)
c Wrapper subroutine to call the MadGraph Borns
c and set the event-by-event couplings constant
      implicit none
      include 'nlegborn.h'
      real * 8 p(0:3,nlegborn),bornjk(nlegborn,nlegborn)
      integer bflav(nlegborn)
      real * 8 bmunu(0:3,0:3,nlegborn),born
      integer idvecbos,vdecaymode
      common/cvecbos/idvecbos,vdecaymode
      integer j,mu
      call set_ebe_couplings

      if(idvecbos.eq.-24) then
         call cconj(bflav,nlegborn)
         call pconj(p,nlegborn)
      endif

      call sborn_proc(p,bflav,born,bornjk,bmunu)

      if(idvecbos.eq.-24) then
         call cconj(bflav,nlegborn)
         call pconj(p,nlegborn)
         do j=1,nlegborn
            do mu=1,3
               bmunu(0,mu,j)=-bmunu(0,mu,j)
               bmunu(mu,0,j)=-bmunu(mu,0,j)
            enddo
         enddo
      endif

      end

      subroutine pconj(p,n)
      implicit none
      real * 8 p(0:3,n)
      integer n,j,mu
      do j=1,n
         do mu=1,3
            p(mu,j)=-p(mu,j)
         enddo
      enddo
      end

      subroutine borncolour_lh
c Wrapper subroutine to call the MadGraph code to associate
c a (leading) color structure to an event.
      implicit none
      include 'nlegborn.h'
      include 'LesHouches.h'
      integer bflav(nlegborn),color(2,nlegborn)
      integer i,j,itmp
      integer idvecbos,vdecaymode
      common/cvecbos/idvecbos,vdecaymode
      do i=1,nlegborn
         bflav(i)=idup(i)
         if (bflav(i).eq.21) bflav(i)=0
      enddo

      if(idvecbos.eq.-24) then
         call cconj(bflav,nlegborn)
      endif

      call born_color(bflav,color)

      if(idvecbos.eq.-24) then
         call cconj(bflav,nlegborn)
         do j=1,nlegborn
            itmp = color(1,j)
            color(1,j) = color(2,j)
            color(2,j) = itmp
         enddo
      endif

      do i=1,2
         do j=1,nlegborn
            icolup(i,j)=color(i,j)
         enddo
      enddo

      end


      subroutine finalize_lh
c     Set up the resonances whose mass must be preserved
c     on the Les Houches interface.
c     
c     vector boson id and decay
      integer idvecbos,vdecaymode
      common/cvecbos/idvecbos,vdecaymode
c     lepton masses
      real *8 lepmass(3),decmass
      common/clepmass/lepmass,decmass

      call add_resonance(idvecbos,4,5)
c     The following routine also performs the reshuffling of momenta if
c     a massive decay is chosen
      call momenta_reshuffle(4,5,6,decmass,0d0)

c     fix here the W decay mode
      id5=vdecaymode
      id6=-vdecaymode + sign(1,idvecbos) 
      call change_id_particles(5,6,id5,id6)

      end



      subroutine change_id_particles(i1,i2,id1,id2)
      implicit none
      include 'LesHouches.h'
      integer i1,i2,id1,id2
      idup(i1)=id1
      idup(i2)=id2
      end



c     i1<i2
      subroutine momenta_reshuffle(ires,i1,i2,m1,m2)
      implicit none
      include 'LesHouches.h'
      integer ires,i1,i2
      real * 8 m1,m2
      real * 8 ptemp(0:3),pfin(0:3),beta(3),betainv(3),modbeta,m
      real * 8 mod_pfin,m0
      integer j,id,dec
      if (i1.ge.i2) then
         write(*,*) 'wrong sequence in momenta_reshuffle'
         stop
      endif
cccccccccccccccccccccccccccccc
c construct boosts from/to vector boson rest frame 
      do j=1,3
         beta(j)=-pup(j,ires)/pup(4,ires)
      enddo
      modbeta=sqrt(beta(1)**2+beta(2)**2+beta(3)**2)
      do j=1,3
         beta(j)=beta(j)/modbeta
         betainv(j)=-beta(j)
      enddo

      m0 = pup(5,ires)
      mod_pfin=
     $     1/(2*m0)*sqrt(abs((m0**2-m1**2-m2**2)**2 - 4*m1**2*m2**2))
               
cccccccccccccccccccccccccccccccccccccccc
c     loop of the two decay products
      
      do dec=1,2
         if(dec.eq.1) then
            id=i1
            m=m1
         else
            id=i2
            m=m2
         endif
         ptemp(0)=pup(4,id)
         do j=1,3
            ptemp(j)=pup(j,id)
         enddo
         call mboost(1,beta,modbeta,ptemp,ptemp)
         pfin(0)=sqrt(mod_pfin**2 + m**2)
         do j=1,3
            pfin(j)=ptemp(j)*mod_pfin/ptemp(0)
         enddo
         call mboost(1,betainv,modbeta,pfin,ptemp)
         do j=1,3
            pup(j,id)=ptemp(j)
         enddo
         pup(4,id)=ptemp(0)
         pup(5,id)=sqrt(abs(pup(4,id)**2-pup(1,id)**2
     $        -pup(2,id)**2-pup(3,id)**2))
         
      enddo

      end
