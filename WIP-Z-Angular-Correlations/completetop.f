CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c     fill the top file histos with appropriate zeros!!
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      subroutine completetop(filein,fileout)  
      implicit none
      character * 100 filein,fileout
      integer MAXMOM
      parameter( MAXMOM = 3000)
      real * 8 x1(MAXMOM),y1(MAXMOM),e1(MAXMOM)
      real * 8 x2(MAXMOM),y2(MAXMOM),e2(MAXMOM)
      integer n1,n2
      character * 100 line
      character * 100 command
      integer j,i,k,iline,nline,iuni,iuno
      real * 8 xx,yy,ee,xl,xh,xbin,xtmp
      real * 8  min_bin
      external min_bin
      real * 8 tiny
      parameter (tiny=1d-10)

      iuni = 55
      iuno = 56
      open(unit=iuni,file=filein,status='old')  
      open(unit=iuno,file=fileout,status='unknown')
        
      do j=1,1000000
         read(iuni,'(a)',end=20) line
         i=index(line,'SET LIMITS X')
         if(i.gt.0) then
            read(line(i+13:),*) xl,xh
            call writeout(line,iuno)
            cycle
         endif
         i=index(line,' ( INT=')
         if(i.gt.0) then
            call writeout(line,iuno)
            n1=MAXMOM
            call gettab(iuni,x1(1),y1(1),e1(1),n1)
            backspace(iuni)
            xbin = min_bin(x1,n1)
            nline = nint((xh-xl)/xbin)
            k=1
            do iline=1,nline
               xtmp=xl+(iline*1d0-0.5d0)*xbin
               if (abs(xtmp-x1(k)).gt.tiny) then
                  write(iuno,*) xtmp,0d0,0d0
               else
                  write(iuno,*) x1(k),y1(k),e1(k)
                  k=k+1
               endif
            enddo
         else
            call writeout(line,iuno) 
         endif
      enddo
 20   continue
      close(iuni)
      close(iuno)
      end


      function min_bin(x,n)
      implicit none
      integer n
      real * 8 min_bin,x(*)
      integer i
      min_bin=1d100
      do i=2,n
         min_bin=min(min_bin,x(i)-x(i-1))
      enddo
      end

