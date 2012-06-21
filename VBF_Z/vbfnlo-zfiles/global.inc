c
c	Dieter Zeppenfeld, <dieter@pheno.physics.wisc.edu>
c	Initial version:  1992 October 25 by Adam Duff
c	Last modified:  201 December 21 by Barbara Jaeger
c
      integer nx, np, nv
c
      parameter( nx=2 )		!number of internal x parameters
      parameter( np=5 )		!number of external partons
      parameter( nv=2 )		!number of decay leptons (via decay bosons)
c
c qsqAmin is the minimum virtuality for t-channel photon exchange. 
c Subamplitudes will be set to zero if photon virtuality falls below
c qsqAmin
      double precision qsqAmin
c old default:
c      parameter (qsqAmin=4d0)
c new default:
      parameter (qsqAmin=0d0)
c test:
c      parameter (qsqAmin=9d0)
c test:
c      parameter (qsqAmin=10d0)
c test:
c      parameter (qsqAmin=100d0)
c   

c distribute finite terms for Born*virtual+real emission = c*|Born|**2 + ....
c with c = -9/2 - pi^2/3 = creal + cvirt
      double precision creal, cvirt, pi2o3p9h
      parameter (pi2o3p9h = pi**2/3d0+9d0/2d0)
      parameter (cvirt = -8d0, creal = -pi2o3p9h-cvirt) ! for powheg