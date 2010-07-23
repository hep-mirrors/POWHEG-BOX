      implicit none
      character * 100 line
      character * 100 command
      integer j,i,k,iline
      real * 8 x,y,e,xx,yy,ee,xl,xh,xbin
      do j=1,10000
         read(*,'(a)',end=20) line
         i=index(line,'SET LIMITS X')
         if(i.gt.0) then
            read(line(i+13:),*) xl,xh
            call writeout(line)
            cycle
         endif
         i=index(line,' ( INT=')
         if(i.gt.0) then
            call writeout(line)
            xx=0
            yy=0
            ee=0
            xbin=0
            iline=0
            do k=1,10000
               read(*,'(a)') line
               iline=iline+1
               i=index(line,'HIST')
               if(i.ne.0) then
                  do while(iline.lt.nint((xh-xl)/xbin))
                     write(*,*) (iline+0.5d0)*xbin,0,0
                     iline=iline+1
                  enddo
                  call writeout(line)
                  goto 10
               endif
               read(line,*) x,y,e
               if(xbin.eq.0) xbin=2*(x-xl)
               do while(nint((x+xbin/2)/xbin).gt.iline)
                  write(*,*) (iline-0.5d0)*xbin,0,0
                  iline=iline+1
               enddo
               call writeout(line)
            enddo
         else
            call writeout(line)
         endif
 10      continue
      enddo
 20   end

      subroutine writeout(string)
      character *(*) string
      integer l
      l=len(string)
      do while(string(l:l).eq.' ')
         l=l-1
      enddo
      write(*,'(a)') string(1:l)
      end

      
               
