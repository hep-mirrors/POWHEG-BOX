c Simple rebin program;
c rebins equal bin histograms into variable size bin one.
c edges must match (of course!)
      implicit none
      character * 100 file,newfile,tag
      character * 200 line
      integer i,j,k,l,ltag
      real * 8 xl,xh,y,ey,xxl,xxh,yyy,eee
      integer ibins,maxbins
      parameter (maxbins=200)
      real * 8 ptbins(maxbins),norm
      logical touched
c      data ptbins/
c     1      0d0, 10d0, 20d0, 30d0,
c     1     40d0,
c     1     50d0,60d0,70d0,80d0,100d0,
c     1    150d0,200d0,250d0,300d0,400d0/
c      data ptbins/
c     1     0d0, 0.210d0,20d0,30d0,50d0,
c     1     70d0,90d0,110d0,150d0,200d0,
c     1     250d0,300d0,400d0,500d0,600d0,
c     1     700d0,800d0/
      integer lastnonblank
      write(*,*)'enter tag to rebin'

      read(*,'(a)') tag
      tag=trim(tag)
      ltag=lastnonblank(tag)
      
      write(*,*)'enter bin edges, one per line,'
     1     //' repeat last edge to terminate'
      do j=1,maxbins+1
         read(*,*) ptbins(j)
         if(j.gt.1.and.ptbins(j).eq.ptbins(j-1)) then
            ibins=j-1
            exit
         endif
      enddo
      write(*,*) ' enter filename'
      read(*,'(a)') file
      open(unit=10,file=file,status='old')
      i=index(file,'.top')
      newfile=file(1:i-1)//'_rebinned.top'
      open(unit=11,file=newfile,status='unknown')
      do j=1,1000000
         read(10,'(a)',end=111) line
         if(index(line,tag(1:ltag)).gt.0) then
            write(11,'(a)') line(1:lastnonblank(line))
            do k=1,1000000
               read(10,'(a)',end=100) line
               if(line.eq.' ') then
                  write(11,'(a)')
                  goto 100
               endif
               read(line,*) xl,xh,y,ey
               do l=ibins-1,1,-1
                  if(xl.gt.ptbins(l)-1d-5) then
                     if(abs(xl-ptbins(l)).lt.1d-8
     1                    .and.abs(xh-ptbins(l+1)).lt.1d-8) then
                        write(11,'(a)') line(1:lastnonblank(line))
                        goto 33
                     elseif(abs(xl-ptbins(l)).lt.1d-8) then
                        xxl=xl
                        xxh=xh
                        yyy=y
                        eee=ey**2
                        goto 33
                     elseif(abs(xh-ptbins(l+1)).lt.1d-8) then
                        yyy=yyy+y
                        eee=eee+ey**2
                        xxh=xh
                        norm=(xxh-xxl)/(xh-xl)
                        write(11,'(4(1x,d14.8))')
     1                       xxl,xxh,yyy/norm,sqrt(eee/norm**2)
                        goto 33
                     elseif(ptbins(l+1)-xh.gt.1d-8) then
                        yyy=yyy+y
                        eee=eee+ey**2
                        xxh=xh
                        goto 33
                     else
                        write(*,*) ' rebin: out of range'
                        call exit(-1)
                     endif
                  endif
               enddo
 33            continue
            enddo
         else
            write(11,'(a)') line(1:lastnonblank(line))
         endif
 100     continue
      enddo
 111  continue
      close(11)
      end

      function lastnonblank(line)
      integer lastnonblank
      character *(*) line
      l=len(line)
      do j=l,1,-1
         if(line(j:j).ne.' ') then
            lastnonblank=j
            return
         endif
      enddo
      lastnonblank=0
      end
