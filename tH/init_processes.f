      subroutine init_processes
      implicit none
      include 'nlegborn.h'
      include '../include/pwhg_flst.h'
      include '../include/pwhg_kn.h'
      logical debug
      parameter (debug=.false.)
      integer i,j   
         
c     check      
      if (nlegborn.ne.4) then
         write(*,*) ' ERROR: set nlegborn to the appropriate value'
         write(*,*) ' for this process in nlegborn.h'
         stop
      endif  


c     index of the first LIGHT coloured parton in the final state     
      flst_lightpart=5
      
c---------------------------------------------------------------c       
c---------------	born subprocesses  	----------------c
c---------------------------------------------------------------c
    
      flst_nborn = 2
      
      do j = 1,flst_nborn
      	do i = 1, nlegborn
	
		if (j.eq.1) then
		
			if (i.eq.1) then 
				flst_born(i,j) = 5
				
			elseif (i.eq.2) then 
				flst_born(i,j) = 0
				
			elseif (i.eq.3) then 
				flst_born(i,j) = -37
				
			elseif (i.eq.4) then 
				flst_born(i,j) = 6
				
			endif
			
		elseif (j.eq.2) then
		
			if (i.eq.1) then 
				flst_born(i,j) = 0
				
			elseif (i.eq.2) then 
				flst_born(i,j) = 5
				
			elseif (i.eq.3) then 
				flst_born(i,j) = -37
				
			elseif (i.eq.4) then 
				flst_born(i,j) = 6
				
			endif
			
		endif
	enddo     
      enddo
      
c-----------------------------------------------------------------c
c-------------------- 	real subprocesses   ----------------------c
c-----------------------------------------------------------------c

    
      flst_nreal = 30

      do j = 1,flst_nreal
      	do i = 1, nlegreal
	
	
		if (i.eq.3) then
			flst_real(i,j) = -37
		elseif 	(i.eq.4) then
				flst_real(i,j) = 6
		endif
	
c----------------------- bg -> H^{-}tg --------------------------c	
		if (j.eq.1) then	
			if (i.eq.1) then
				flst_real(i,j) = 5
			elseif (i.eq.2) then
				flst_real(i,j) = 0
			elseif (i.eq.5) then
				flst_real(i,j) = 0
			endif	
				
c---------------------- gb -> H^{-}tg ---------------------------c		
		elseif (j.eq.2) then
			if (i.eq.1) then
				flst_real(i,j) = 0
			elseif (i.eq.2) then
				flst_real(i,j) = 5
			elseif (i.eq.5) then
				flst_real(i,j) = 0
			endif	
c---------------------- gg -> H^{-}t bbar -----------------------c		
		elseif (j.eq.3) then
			if (i.eq.1) then
				flst_real(i,j) = 0
			elseif (i.eq.2) then
				flst_real(i,j) = 0
			elseif (i.eq.5) then
				flst_real(i,j) = -5
			endif	
		
c---------------------- bq -> H^{-}tq ---------------------------c
c		q = d			
		elseif (j.eq.4) then
			if (i.eq.1) then
				flst_real(i,j) = 5
			elseif (i.eq.2) then
				flst_real(i,j) = 1
			elseif (i.eq.5) then
				flst_real(i,j) = 1
			endif	
		
c		q = u		
		elseif (j.eq.5) then
			if (i.eq.1) then
				flst_real(i,j) = 5
			elseif (i.eq.2) then
				flst_real(i,j) = 2
			elseif (i.eq.5) then
				flst_real(i,j) = 2
			endif
		
c		q = s		
		elseif (j.eq.6) then
			if (i.eq.1) then
				flst_real(i,j) = 5
			elseif (i.eq.2) then
				flst_real(i,j) = 3
			elseif (i.eq.5) then
				flst_real(i,j) = 3
			endif
		
c 		q = c		
		elseif (j.eq.7) then
			if (i.eq.1) then
				flst_real(i,j) = 5
			elseif (i.eq.2) then
				flst_real(i,j) = 4
			elseif (i.eq.5) then
				flst_real(i,j) = 4
			endif
						
			
		
c---------------------- qb -> H^{-}tq ---------------------------c
c		q = d		
		elseif (j.eq.8) then
			if (i.eq.1) then
				flst_real(i,j) = 1
			elseif (i.eq.2) then
				flst_real(i,j) = 5
			elseif (i.eq.5) then
				flst_real(i,j) = 1
			endif	
		
c		q = u		
		elseif (j.eq.9) then
			if (i.eq.1) then
				flst_real(i,j) = 2
			elseif (i.eq.2) then
				flst_real(i,j) = 5
			elseif (i.eq.5) then
				flst_real(i,j) = 2
			endif
		
c		q = s		
		elseif (j.eq.10) then
			if (i.eq.1) then
				flst_real(i,j) = 3
			elseif (i.eq.2) then
				flst_real(i,j) = 5
			elseif (i.eq.5) then
				flst_real(i,j) = 3
			endif
		
c 		q = c		
		elseif (j.eq.11) then
			if (i.eq.1) then
				flst_real(i,j) = 4
			elseif (i.eq.2) then
				flst_real(i,j) = 5
			elseif (i.eq.5) then
				flst_real(i,j) = 4
			endif
		
c---------------------- b qbar -> H^{-} t qbar ---------------------------c
c		qbar = dbar		
		elseif (j.eq.12) then
			if (i.eq.1) then
				flst_real(i,j) = 5
			elseif (i.eq.2) then
				flst_real(i,j) = -1
			elseif (i.eq.5) then
				flst_real(i,j) = -1
			endif
		
c		qbar = ubar		
		elseif (j.eq.13) then
			if (i.eq.1) then
				flst_real(i,j) = 5
			elseif (i.eq.2) then
				flst_real(i,j) = -2
			elseif (i.eq.5) then
				flst_real(i,j) = -2
			endif
		
c		qbar = sbar		
		elseif (j.eq.14) then
			if (i.eq.1) then
				flst_real(i,j) = 5
			elseif (i.eq.2) then
				flst_real(i,j) = -3
			elseif (i.eq.5) then
				flst_real(i,j) = -3
			endif
		
c 		qbar = cbar			
		elseif (j.eq.15) then
			if (i.eq.1) then
				flst_real(i,j) = 5
			elseif (i.eq.2) then
				flst_real(i,j) = -4
			elseif (i.eq.5) then
				flst_real(i,j) = -4
			endif
		
c---------------------- qbar b -> H^{-} t qbar ---------------------------c
c		qbar = dbar		
		elseif (j.eq.16) then
			if (i.eq.1) then
				flst_real(i,j) = -1
			elseif (i.eq.2) then
				flst_real(i,j) = 5
			elseif (i.eq.5) then
				flst_real(i,j) = -1
			endif
		
c		qbar = ubar		
		elseif (j.eq.17) then
			if (i.eq.1) then
				flst_real(i,j) = -2
			elseif (i.eq.2) then
				flst_real(i,j) = 5
			elseif (i.eq.5) then
				flst_real(i,j) = -2
			endif
		
c		qbar = sbar		
		elseif (j.eq.18) then
			if (i.eq.1) then
				flst_real(i,j) = -3
			elseif (i.eq.2) then
				flst_real(i,j) = 5
			elseif (i.eq.5) then
				flst_real(i,j) = -3
			endif
		
c 		qbar = cbar		
		elseif (j.eq.19) then
			if (i.eq.1) then
				flst_real(i,j) = -4
			elseif (i.eq.2) then
				flst_real(i,j) = 5
			elseif (i.eq.5) then
				flst_real(i,j) = -4
			endif
		
c---------------------- q qbar -> H^{-} t bbar ---------------------------c
c		q = d			
		elseif (j.eq.20) then
			if (i.eq.1) then
				flst_real(i,j) = 1
			elseif (i.eq.2) then
				flst_real(i,j) = -1
			elseif (i.eq.5) then
				flst_real(i,j) = -5
			endif
		
c		q = u		
		elseif (j.eq.21) then
			if (i.eq.1) then
				flst_real(i,j) = 2
			elseif (i.eq.2) then
				flst_real(i,j) = -2
			elseif (i.eq.5) then
				flst_real(i,j) = -5
			endif
		
c		q = s		
		elseif (j.eq.22) then
			if (i.eq.1) then
				flst_real(i,j) = 3
			elseif (i.eq.2) then
				flst_real(i,j) = -3
			elseif (i.eq.5) then
				flst_real(i,j) = -5
			endif
		
c 		q = c		
		elseif (j.eq.23) then
			if (i.eq.1) then
				flst_real(i,j) = 4
			elseif (i.eq.2) then
				flst_real(i,j) = -4
			elseif (i.eq.5) then
				flst_real(i,j) = -5
			endif
		
c---------------------- qbar q -> H^{-} t bbar ---------------------------c	
c		q = d		
		elseif (j.eq.24) then
			if (i.eq.1) then
				flst_real(i,j) = -1
			elseif (i.eq.2) then
				flst_real(i,j) = 1
			elseif (i.eq.5) then
				flst_real(i,j) = -5
			endif
		
c		q = u		
		elseif (j.eq.25) then
			if (i.eq.1) then
				flst_real(i,j) = -2
			elseif (i.eq.2) then
				flst_real(i,j) = 2
			elseif (i.eq.5) then
				flst_real(i,j) = -5
			endif
		
c		q = s		
		elseif (j.eq.26) then
			if (i.eq.1) then
				flst_real(i,j) = -3
			elseif (i.eq.2) then
				flst_real(i,j) = 3
			elseif (i.eq.5) then
				flst_real(i,j) = -5
			endif
		
c 		q = c			
		elseif (j.eq.27) then
			if (i.eq.1) then
				flst_real(i,j) = -4
			elseif (i.eq.2) then
				flst_real(i,j) = 4
			elseif (i.eq.5) then
				flst_real(i,j) = -5
			endif
		
c---------------------- b bbar -> H^{-} t bbar ---------------------------c	
		elseif (j.eq.28) then
			if (i.eq.1) then
				flst_real(i,j) = 5
			elseif (i.eq.2) then
				flst_real(i,j) = -5
			elseif (i.eq.5) then
				flst_real(i,j) = -5
			endif
		
c---------------------- bbar b -> H^{-} t bbar ---------------------------c
		elseif (j.eq.29) then
			if (i.eq.1) then
				flst_real(i,j) = -5
			elseif (i.eq.2) then
				flst_real(i,j) = 5
			elseif (i.eq.5) then
				flst_real(i,j) = -5
			endif
			
c---------------------- b b -> H^{-} t b ---------------------------c
		elseif (j.eq.30) then
			if (i.eq.1) then
				flst_real(i,j) = 5
			elseif (i.eq.2) then
				flst_real(i,j) = 5
			elseif (i.eq.5) then
				flst_real(i,j) = 5
			endif	
		

		endif
	enddo
      enddo 

c doubly resonant contributions; we distinguish them by using a temporary
c value of -1037 for the charged higgs. They do not have an underlying Born,
c so they will be added with the regular remnants

      do j=-5,5
         flst_nreal = flst_nreal + 1
         flst_real(1,flst_nreal)=j
         flst_real(2,flst_nreal)=-j
         flst_real(3,flst_nreal)=-1037
         flst_real(4,flst_nreal)=6
         flst_real(5,flst_nreal)=-5
      enddo



      end
      


