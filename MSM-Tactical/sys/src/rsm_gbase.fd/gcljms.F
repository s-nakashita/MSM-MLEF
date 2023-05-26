      subroutine gcljms (si,kdim)
      use machine , only : kind_phys,kind_rad
!     use physcons
      use comcd1
      implicit none
!     include 'constant.h'
      integer i,j,kl,k,kdim,kk
      real (kind=kind_rad) xthk,silowxthk,silow
      real (kind=kind_rad) si(kdim+1), ppptop(4,2)
      save ppptop
! --- pressure limits for sfc and top of each cloud domain (l,m,h)
!     in mb, model layers for cld tops are l=7,m=11,h=15 at low
!     latitudes and l= ,m= ,h=  , at pole region.
!....     ptop above h changed from 150 to 100, cause
!c    data ppptop /1050.,642.,350.,150., 1050.,750.,500.,150./
!          code was truncating tops of convective clouds
!c    data ppptop /1050.,642.,350.,100., 1050.,750.,500.,100./
!          use 0. for high domain, as clw parameterization uses 
!            all the upper model layers for clouds
      data ppptop /1050.,642.,350.,  0., 1050.,750.,500.,  0./
!
!     rocp = rd / cp
! --- inverson type cld critical value-istrat=0
!yh94 clapse = -0.055e0
      clapse = -0.06e0
! --- inverson type cld critical value-istrat=1
      clapkc = -0.05e0
!....critical dtheta/dp for ocean stratus(wgt varies 0 to 1
!                linearly from clapse to clpse)
      dclps = -0.01e0
      clpse = clapkc + dclps
      cvtop = 400.0e0
      pstrt = 800.0e0
! --- low cld bottom (at sigma=0.95) and top sigma level
      do 5 k=1,kdim
        kk=k
        if (si(kk) .le. 0.95e0) go to 10
   5  continue
  10  klowb = kk - 1
      silow = ppptop(2,1) * 1.0e-3
      do 20 k=1,kdim
        kk=k
        if (si(kk) .lt. silow) go to 30
  20  continue
  30  klowt = kk
! --- presure limit at sfc and at top of cloud domains (l,m,h) in mb
      do 40 j=1,2
      do 40 i=1,4
       ptopc(i,j) = ppptop(i,j)
  40  continue
! --- l cld vertical vel adj boundaries
      vvcld(1) =  0.0003e0
      vvcld(2) = -0.0005e0
      crhrh = 0.60e0
!--- compute llyr--which is topmost non cld(low) layer, for stratiform
      xthk = 0.e0
!....   default llyr
      kl = kdim + 1
!....   topmost noncloud layer will be the one at or above lowest
!         0.1 of the atmosphere..
      do 202 k=1,kdim
!       xthk = xthk + si(k) - si(k+1)
!       if (xthk.lt.0.1e0) go to 202
        kl = k
!       go to 204
        if (si(k).lt.0.9e0) go to 204
  202 continue
  204 llyr = kl-1
!yt      print 205,llyr,klowb
  205 format(1h ,'-------llyr,klowb =',2i5)
      return
      end
