      implicit none
      character * 100 line,lines(10000)
      character * 100 command
      integer j,i,k,iline
      real * 8 x,ox,y,e,xl,xh,xbin
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
c store histogram lines
            do k=1,10000
               read(*,'(a)') lines(k)
               i=index(lines(k),'HIST')
               if(i.ne.0) goto 21
               read(lines(k),*) x,y,e
               if(k.eq.1) then
                  xbin=(x-xl)*2
               else
                  xbin=min(xbin,x-ox)
               endif
               ox=x
            enddo
 21         continue
            iline=0
            do k=1,10000
               line=lines(k)
               iline=iline+1
               i=index(line,'HIST')
               if(i.ne.0) then
                  do while(iline.lt.nint((xh-xl)/xbin))
                     write(*,100) xl+(iline+0.5d0)*xbin,0E0,0E0
                     iline=iline+1
                  enddo
                  call writeout(line)
                  goto 10
               endif
               read(line,*) x,y,e
               do while(nint((x-xl+xbin/2)/xbin).gt.iline)
                  write(*,100) xl+(iline-0.5d0)*xbin,0E0,0E0
                  iline=iline+1
               enddo
               call writeout(line)
            enddo
         else
            call writeout(line)
         endif
 10      continue
      enddo
 100  format(3(F10.4,1X))
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

      
               
