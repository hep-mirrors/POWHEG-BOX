c fist do
c cat file.top | completetop > filec.top
c then
c cat filec.top | combinethem > a0.top
c
      implicit none
      real * 8 ptz(3,1000),a0(3,1000)
      integer nptz,na0,k,j,l
      real * 8 total,vptz,eptz,dummy,errtot
      character * 100 string
      write(*,*) ' set order x y dy'
      do k=1,10000
         read(*,'(a)',end=10) string
         if(string.eq.' (  total') then
            read(*,*)
            read(*,*) dummy,total,errtot
         endif
         if(string.eq.' (  pt(Z)') then
            read(*,*)
            do j=1,1000
               read(*,'(a)') string
               if(string.eq.'  HIST SOLID') goto 20
               read(string,*) ptz(1,j),ptz(2,j),ptz(3,j)
            enddo
 20         nptz=j-1
         endif            
         if(string.eq.' (  A0') then
            read(*,*)
            do j=1,1000
               read(*,'(a)') string
               if(string.eq.'  HIST SOLID') goto 30
               read(string,*) a0(1,j),a0(2,j),a0(3,j)
            enddo
 30         na0=j-1
         endif
      enddo
 10   continue
      do j=1,na0
         vptz=0
         eptz=0
         do l=1,5
            vptz=vptz+ptz(2,(j-1)*5+l)
            eptz=eptz+ptz(3,(j-1)*5+l)**2
         enddo
         eptz=sqrt(eptz)/5
         vptz=vptz/5
         write(*,*) a0(1,j),a0(2,j)/vptz,a0(2,j)/vptz*
     1     sqrt((a0(3,j)/a0(2,j))**2+(eptz/vptz)**2)
      enddo
      write(*,*) ' hist'
      write(*,*) ' plot'
      write(*,*) ' set color red'
      write(*,'(a)')'7.22772 -0.000882254'
      write(*,'(a)')'11.2871 0.0203243'
      write(*,'(a)')'15.4455 0.0494036'
      write(*,'(a)')'18.5149 0.0808594'
      write(*,'(a)')'21.9802 0.114672'
      write(*,'(a)')'24.4554 0.139049'
      write(*,'(a)')'27.0297 0.163425'
      write(*,'(a)')'30.495 0.19645'
      write(*,'(a)')'37.4257 0.256202'
      write(*,'(a)')'43.1683 0.311245'
      write(*,'(a)')'47.0297 0.34269'
      write(*,'(a)')'50.6931 0.375713'
      write(*,'(a)')'54.0594 0.406377'
      write(*,'(a)')'56.8317 0.429963'
      write(*,'(a)')'58.5149 0.446476'
      write(*,*) ' join'
      end
