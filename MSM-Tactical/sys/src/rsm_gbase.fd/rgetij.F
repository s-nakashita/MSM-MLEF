      subroutine rgetij (xlat,xlon,slmsk,blat,blon,                      &                  
     &                   ils,idim,jdim,ki,kj,me)
      use physcons, pi => con_pi
!cc
       integer me
       dimension blat(idim,jdim),blon(idim,jdim),keni(4),kenj(4)
       dimension slmsk(idim,jdim)
!jw       data npt/4/
       npt=4
!
       rad = pi/180.
       ki = 1
       kj = 1
       keni(1) = 1
       kenj(1) = 1
       dxmax = 1000.
!
       xg = xlon
       yg = xlat
       do i = 1,idim
       do j = 1,jdim
          xp = blon(i,j)
          yp = blat(i,j)
          distx = max(abs(xp-xg),1.e-5)
          disty = max(abs(yp-yg),1.e-5)
          ddum = (cos((yp+yg)*rad/2.))**2
          dxy = sqrt(ddum*distx**2+disty**2)
          if(dxy.lt.dxmax) then 
             dxmax = dxy
             keni(1) = i
             kenj(1) = j
          endif
       enddo
       enddo
!
       keni(2) = keni(1) + 1
       kenj(2) = kenj(1) 
       keni(3) = keni(1)
       kenj(3) = kenj(1) + 1 
       keni(4) = keni(1) + 1
       kenj(4) = kenj(1) + 1
       ki = keni(1)
       kj = kenj(1)
!
       if(me.eq.0) print 601,dxmax,keni(1),kenj(1)
       if(kenj(4).gt.jdim.or.keni(4).gt.idim.or.dxmax.gt.10.) then
          if(me.eq.0) print 602,xlat,xlon
          ki = keni(1)
          kj = kenj(1)
          return
       endif
       xils = float(ils)
       if (ils.lt.0) then
          ki = keni(1)
          kj = kenj(1)
          return
       end if
!
       if (ils.eq.0) then
!....     find nearest sea point
          do 100 kd = 1,npt
             ii = keni(kd)
             jj = kenj(kd)
             if (slmsk(ii,jj).le.xils) go to 101
  100     continue
!....     no sea points so default to nearest point
        if(me.eq.0) print 603,xlat,xlon
             ki = keni(1)
             kj = kenj(1)
             return
  101     continue
          ki = ii
          kj = jj
          return
       end if
!
       if (ils.eq.1) then
!....     find nearest land/ice point
           do 110 kd = 1,npt
              ii = keni(kd)
              jj = kenj(kd)
              if (slmsk(ii,jj).ge.xils) go to 111
  110      continue
!....     no land points so default to nearest point
        if(me.eq.0) print 604,xlat,xlon
              ki = keni(1)
              kj = kenj(1)
              return
  111      continue
           ki = ii
           kj = jj
           return
       end if
  601 format(1x,' shortest distance (degree)= ',f8.4,                    &  
     &       ' grid(i,j) = ',2i4)
  602  format(1h ,' asked point is out of domain  for ',                 &
     &            ' lat lon=',2f9.2,'..so default to nearest')
  603  format(1h ,' asked for sea point but can t find one for',         &
     &            ' lat lon=',2f9.2,'..so default to nearest')
  604  format(1h ,' asked for land sea point but can t find one for',    &
     &            ' lat lon=',2f9.2,'..so default to nearest')
!
      return
      end
