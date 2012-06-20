c
c	This subroutine calculates the seperation in
c	R = ( deltaPhi^2 + deltaY^2 ) between two
c	four momenta.  Phi is the azimuthal angle, while
c	Y is the rapidity.
c
      real*8 function rsep(
     &                   p1,	!in:  four momentum 1
     &                   p2	!in:  four momentum 2
     &                    )
      implicit none
c
c declare input/output variables
c
      real*8 p1(0:3), p2(0:3)
c
c declare local variables
c
      real*8 r_two, r_pi
      parameter( r_two=2.0d0, r_pi=3.141592653589793238d0 )
c
      real*8 pt1, y1, phi1, pt2, y2, phi2
      real*8 dphi, dy
c
c determine "lego" plot parameters
c
      call lego( p1, pt1, y1, phi1 )
      call lego( p2, pt2, y2, phi2 )
c
      dy = y2 - y1
      dphi = abs( phi2 - phi1 )
c
      if ( dphi .gt. r_pi ) then
         rsep = sqrt( (r_two * r_pi - dphi)**2 + dy**2 )
      else
         rsep = sqrt( dphi**2 + dy**2 )
      end if
c
c done
c
      return
      end
c
c

c-------------------------------------------------------------------------------
c
c	This subroutine converts a four-momentum into
c	pT, rapidity and phi variables.
c
      subroutine lego(
     &              p,		!in:  particle four-momentum
     &              pt,		!out:  corresponding pT (transverse momentum)
     &              y,		!out:  corresponding eta (pseudorapidity)
     &              phi		!out:  corresponding phi (azimuthal angle)
     &               )
      implicit none
c
c declare input/output variables
c
      real*8 p(0:3), pt, y, phi
c
c declare local variables
c
      real*8 r_zero, r_half, r_pi, r_large
      parameter( r_zero=0.0d0, r_half=0.5d0 )
      parameter( r_pi=3.141592653589793238d0 )
      parameter( r_large=1.0d+3 )
c
      real*8 pt2, pabs, arg
c
      pt2 = p(1)**2 + p(2)**2
      if ( pt2 .gt. r_zero ) then
         pt = sqrt( pt2 )
         pabs = sqrt(pt2 + p(3)**2)
c
         if ( p(3) .ge. r_zero ) then
            arg = (pabs + p(3)) / pt
         else
            arg = pt / (pabs - p(3))
         end if
         y = log( arg )
         phi = atan2( p(2), p(1) )
c
      else
         if ( p(3) .gt. r_zero ) then
            pt = r_zero
            y = r_large
            phi = r_zero
         else if ( p(3) .lt. r_zero ) then
            pt = r_zero
            y = -r_large
            phi = r_zero
         else
            pt = r_zero
            y = r_zero
            phi = r_zero
         end if
      end if
c
      return
      end
C
