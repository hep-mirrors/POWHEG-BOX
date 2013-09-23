      subroutine setborn(p,bflav,born,bornjk,bmunu)
c Wrapper subroutine to call the MadGraph Borns
c and set the event-by-event couplings constant
      implicit none
      include 'nlegborn.h'
      real * 8 p(0:3,nlegborn),bornjk(nlegborn,nlegborn)
      integer bflav(nlegborn)
      real * 8 bmunu(0:3,0:3,nlegborn),born
      call set_ebe_couplings
      call sborn_proc(p,bflav,born,bornjk,bmunu)
      end

      subroutine borncolour_lh
c Wrapper subroutine to call the MadGraph code to associate
c a (leading) color structure to an event.
      implicit none
      include 'nlegborn.h'
      include 'pwhg_flst.h'
      include 'pwhg_rad.h'
      integer equivto(maxprocborn)
      common/cequivtoborn/equivto
      include 'LesHouches.h'
      integer bflav0(nlegborn),bflav(nlegborn),color(2,nlegborn)
      integer i,j,iborn
      logical samecol,conjcol
c We should reach the madgraph flavour configuration that
c was actually computed, in case smartsig is on
      iborn = rad_ubornidx
      bflav0 = flst_born(:,iborn)
      do while(equivto(iborn).ne.-1)
         iborn=equivto(iborn)
      enddo
      bflav = flst_born(:,iborn)
      call born_color(bflav,color)
c Now we have the colour configuration associated with the
c amplitude that was computed instead of rad_ubornidx.
c However, that amplitude may differ from the original one
c by charge conjugation. Check if this is the case
      call matchcolour(nlegborn,bflav0,color)
      icolup(:,1:nlegborn)=color(:,1:nlegborn)
      end


      subroutine matchcolour(n,flav,colour)
      implicit none
      integer n,flav(n),colour(2,n)
      integer j,k,is,iwhere
c Check that color assignment is compatible with flavour.
c If not, conjugate the colour in the whole colour chain.
c This can be done consistently if the two ends of the colour chain
c have opposite flavour.
c We assumes that the colour array is a self consistent colour assignment.

c first conjugate initial flavours and colours, in order to handle
c all particles as outgoing.
      flav(1)=-flav(1)
      flav(2)=-flav(2)
      call colour_conj(colour(:,1))
      call colour_conj(colour(:,2))

      do k=1,n
         if(flav(k).ne.0) then
            if(flav(k).gt.0) then
               is = 1
            else
               is = 2
            endif
c The following is normal assignment
            if(colour(is,k).gt.0.and.colour(3-is,k).eq.0) cycle
c the following is impossible to handle:
            if(colour(1,k).eq.0.and.colour(2,k).eq.0) goto 999
            if(colour(1,k).ne.0.and.colour(2,k).ne.0) goto 999
c the remaining possibility is colour(is,k)=0 and colour(3-is,k)!=0;
c Correct by exchanging colour and anticolour in the whole colour connected
c chain, provided the last element has opposite sign of flavour
            iwhere = k
 10         continue
            do j=1,n
               if(j.ne.iwhere) then
                  if(colour(is,j).eq.colour(3-is,iwhere)) then
                     if(flav(j).eq.0) then
                        if(colour(1,j).ne.0.and.colour(2,j).ne.0) then
                           call colour_conj(colour(:,iwhere))
                           iwhere = j
                           goto 10
                        else
c this cannot be handled
                           goto 999
                        endif
                     elseif(flav(j)*flav(k).lt.0) then
c found a connected opposite flavour;
c The following can't be, colour(is,j)!=0;
                        if(colour(3-is,j).ne.0) goto 999
                        call colour_conj(colour(:,iwhere))
                        call colour_conj(colour(:,j))
                        exit
                     else
                        goto 999
                     endif
                  endif
               endif
            enddo
         endif
      enddo

c Straighten up colour and flavour
      flav(1)=-flav(1)
      flav(2)=-flav(2)
      call colour_conj(colour(:,1))
      call colour_conj(colour(:,2))

      return

 999  continue
      write(*,*)
     1     ' matchcolour: incompatible colour-flavour configuration',
     2     ' better to switch off smartsig in sigborn.f ...'
      call pwhg_exit(-1)
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
      call momenta_reshuffle(4,5,6,decmass,decmass)

c     fix here the Z decay mode
      id5=vdecaymode
      id6=-vdecaymode 
      call change_id_particles(5,6,id5,id6)

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




      subroutine change_id_particles(i1,i2,id1,id2)
      implicit none
      include 'LesHouches.h'
      integer i1,i2,id1,id2
      idup(i1)=id1
      idup(i2)=id2
      end
