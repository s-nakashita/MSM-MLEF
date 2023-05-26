!ccj modified for rsm
!      subroutine ozphys (lat,im,ix,dt, prsl, ozi,ozo)
      subroutine ozphys (lat,im,ix,dt, prsl, ozi)
!
      use machine , only : kind_phys
!c
#include <paramodel.h>
#include <rscomoz.h>
!
      integer k,kmax,kmin,l,i,im,ix
      real(kind=kind_phys) pmax,pmin,ptem,ptemp,dt
!
      real(kind=kind_phys) prsl(ix,levr)
      real(kind=kind_phys) ozi(ix,levr),ozo(ix,levr)
!c
      real(kind=kind_phys) wk1(ix),   wk2(ix),  wk3(ix),  wk4(ix),       &     
     &                     wk5(ix),   prod(ix), dist(ix), wkp(ix,2),     &
     &                     wkd(ix,2)
!     &                     wkd(ix,2), ozib(ix)
!cc
!
!      print *,'in ozphys 1,im=',im,'ix=',ix,'lat=',lat
      do i=1,im
         wkp(i,1) = ozprd(i,lat,1)
         wkp(i,2) = ozprd(i,lat,lev46)
!
         wkd(i,1)    = ozdis(i,lat,1)
         wkd(i,2)    = ozdis(i,lat,lev46)
      enddo
!
      do l=1,levr
        pmin =  1.0e10
        pmax = -1.0e10
!
        do i=1,im
          wk1(i) = log(prsl(i,l))
          pmin   = min(wk1(i), pmin)
          pmax   = max(wk1(i), pmax)
          prod(i) = 0.0
          dist(i) = 0.0
        enddo
        kmax = 1
        kmin = 1
        do k=1,lev46-1
          if (pmin .lt. poz(k)) kmax = k
          if (pmax .lt. poz(k)) kmin = k
        enddo
!
        do k=kmin,kmax
          do i=1,im
            wk2(i)    = ozprd(i,lat,k)
            wk3(i)    = ozprd(i,lat,k+1)
!
            wk4(i)    = ozdis(i,lat,k)
            wk5(i)    = ozdis(i,lat,k+1)
          enddo
          ptemp = 1.0 / (poz(k) - poz(k+1))
          do i=1,im
            if (wk1(i) .lt. poz(k) .and. wk1(i) .ge. poz(k+1)) then
               ptem     = (wk1(i) - poz(k+1)) * ptemp
               prod(i)  = ptem * wk2(i) + (1.0-ptem) * wk3(i)
               dist(i)  = ptem * wk4(i) + (1.0-ptem) * wk5(i)
            endif
          enddo
        enddo
!
        do i=1,im
          if (wk1(i) .lt. poz(lev46)) then
            prod(i) = wkp(i,2)
            dist(i) = wkd(i,2)
          endif
          if (wk1(i) .ge. poz(1)) then
            prod(i) = wkp(i,1)
            dist(i) = wkd(i,1)
          endif
!          ozib(i)   = ozi(i,l)            ! no filling
          ozo(i,l)  = (ozi(i,l)+prod(i)*dt) / (1.0 + dist(i)*dt)
          ozi(i,l)  = ozo(i,l)
        enddo
!
      enddo                         ! vertical loop
!      print *,'end of ozphys'
!
      return
      end
