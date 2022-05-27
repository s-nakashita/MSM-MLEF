      subroutine rmakepc(theta,gamma,sigma,im,jm)
!
!===  pc: principal coordinates of each z avg orog box for l&m
!
#include <paramodel.h>
#ifdef GTOPO30
!
      parameter(imn=360*60/mtnres,jmn=180*60/mtnres)
!
      parameter(rearth=6.3712e+6)
      integer, allocatable :: zavg(:,:),zslm(:,:)
      real, allocatable ::  deltax(:)
! .................................  rid off unnecessary arrays SCC 05/27/05
!     dimension oro(im,jm),slm(im,jm),hl(im,jm),hk(im,jm)  ! oro never used
!     dimension hx2(im,jm),hy2(im,jm),hxy(im,jm),hlprim(im,jm)
!     dimension theta(im,jm),gamma(im,jm),sigma2(im,jm),sigma(im,jm)
      dimension theta(im,jm),gamma(im,jm),sigma(im,jm)
      real oroij,slmij,hlij,hkij
      real hx2ij,hy2ij,hxyij,hlprimij,sigma2ij
! .................................  
      dimension ist(im),ien(im),jst(jm),jen(jm)
      logical flag, debug
!===  data debug/.true./
      data debug/.false./
!
      pi = 4.0 * atan(1.0)
      certh = pi * rearth
!---- global xlat and xlon ( degree )
!
      im1 = im - 1
      jm1 = jm - 1
      delx  = 360./im       ! gaussian grid resolution
      delxn = 360./imn      ! mountain data resolution
      deltay =  certh / float(jmn)
      print *, '! makepc: deltay=',deltay
!
      allocate (deltax(jmn))
      do j=1,jmn
         glat = -90. + (j-1) * delxn + delxn * 0.5
!hmhj    deltax(j) = deltay * cosd(glat)
         deltax(j) = deltay * cos (glat)
      enddo
!
!---- find the average of the modes in a grid box
!
      faclon  = delx / delxn
      do i=1,im
         ist(i) = faclon * float(i-1) + faclon * 0.5
         ist(i) = ist(i) + 1
         ien(i) = faclon * float(i) + faclon * 0.5
!x       print*, ' i ist ien ',i,ist(i),ien(i)
      enddo
      do j=1,jm
         jst(j) = faclon * float(j-1) + faclon * 0.5
         jst(j) = jst(j) + 1
         jen(j) = faclon * float(j) + faclon * 0.5
!x       print*, ' j jst jen ',j,jst(j),jen(j)
      enddo
      ien(im) = min(ien(im),imn)
      jen(jm) = min(jen(jm),jmn)
!
!... derivitive tensor of height
!
      allocate (zslm(imn,jmn),zavg(imn,jmn))
      read(11,11) zavg
      read(14,12) zslm
      rewind(11)
      rewind(14)
11    format(20i4)
12    format(80i1)
!
      do j=1,jm
        do i=1,im
            oroij  = 0.0
            hx2ij = 0.0
            hy2ij = 0.0
            hxyij = 0.0
            xnsum = 0.0
            xland = 0.0
            xwatr = 0.0
            xl1 = 0.0
            xs1 = 0.0
            xfp = 0.0
            yfp = 0.0
            xfpyfp = 0.0
            xfp2 = 0.0
            yfp2 = 0.0
            hlij = 0.0
            hkij = 0.0
            hlprimij = 0.0
            sigma2ij = 0.
            theta(i,j) = 0.0 
            gamma(i,j) = 0.
            sigma(i,j) = 0.
!
            do ii1=ist(i),ien(i)
               i1 = ii1
               if(ii1.le.0.)  i1 = i1 + imn
               if(ii1.gt.imn) i1 = i1 - imn
!
!===  set the rest of the indexs for ave: 2pt staggered derivitive
!
               i0 = ii1 - 1
               if (ii1 - 1 .le. 0 )   i0 = i0 + imn
               if (ii1 - 1 .gt. imn)  i0 = i0 - imn
!
               ip1 = ii1 + 1
               if (ii1 + 1 .le. 0 )   ip1 = ip1 + imn
               if (ii1 + 1 .gt. imn)  ip1 = ip1 - imn
!
               do j1=jst(j),jen(j)
                  xland = xland + float(zslm(i1,j1))
                  xwatr = xwatr + float(1-zslm(i1,j1))
                  xnsum = xnsum + 1.
!
                  height = float(zavg(i1,j1))
                  hi0 =  float(zavg(i0,j1))
                  hip1 =  float(zavg(ip1,j1))
!
                  if(height.lt.-990.) height = 0.0
                  if(hi0 .lt. -990.)  hi0 = 0.0
                  if(hip1 .lt. -990.)  hip1 = 0.0
!........           xfp = xfp + 0.5 * ( hip1 - hi0 ) / deltax(j1)
                  xfp = 0.5 * ( hip1 - hi0 ) / deltax(j1)
                  xfp2 = xfp2 +                                               &
     &                   0.25 * ( ( hip1 - hi0 )/deltax(j1) )** 2 
!
                  if (j1 .ne. jst(1) .and. j1 .ne. jen(jm)) then
                   hj0 =  float(zavg(i1,j1-1))
                   hjp1 =  float(zavg(i1,j1+1))
                   if(hj0 .lt. -990.)  hj0 = 0.0
                   if(hjp1 .lt. -990.)  hjp1 = 0.0
!.......           yfp = yfp + 0.5 * ( hjp1 - hj0 ) / deltay
                   yfp = 0.5 * ( hjp1 - hj0 ) / deltay
                   yfp2 = yfp2 + 0.25 * ( ( hjp1 - hj0 )/deltay )**2   
!
!..............elseif ( j1 .eq. jst(j) .or. j1 .eq. jen(jm) ) then
! ===     the nh pole: nb j1 goes from high at np to low toward sp
!
                  elseif ( j1 .eq. jst(1) ) then
		   ijax = i1 + imn/2 
                   if (ijax .le. 0 )   ijax = ijax + imn
                   if (ijax .gt. imn)  ijax = ijax - imn
!..... at pole we stay at the same latitude j1 but cross to opp side
                   hijax = float(zavg(ijax,j1))
                   hi1jm1 = float(zavg(i1,j1-1))
                   hi1j1 = float(zavg(i1,j1))
                   if(hijax .lt. -990.)  hijax = 0.0
                   if(hi1jm1 .lt. -990.)  hi1jm1 = 0.0
                   if(hi1j1 .lt. -990.)  hi1j1 = 0.0
!.......           yfp = yfp + 0.5 * ( ( 0.5 * ( hijax + hi1j1) ) 
!                                - hi1jm1 )/deltay
                   yfp = 0.5 * ( ( 0.5 * ( hijax + hi1j1) )                   &
     &                           - hi1jm1 )/deltay
                   yfp2 = yfp2 + 0.25 * ( (( 0.5 *  ( hijax + hi1j1))         &
     &                           - hi1jm1)/ deltay )**2
!
! ===     the sh pole: nb j1 goes from high at np to low toward sp
!
                  elseif ( j1 .eq. jen(jm) ) then
		   ijax = i1 + imn/2 
                   if (ijax .le. 0 )   ijax = ijax + imn
                   if (ijax .gt. imn)  ijax = ijax - imn
                   hijax = float(zavg(ijax,j1))
                   j1p1=j1+1                ! SCC temp. fix 05/27/05
                   j1p1=min(j1p1,jmn)       ! SCC temp. fix 05/27/05
                   hi1jp1 = float(zavg(i1,j1p1))
                   hi1j1 = float(zavg(i1,j1))
                   if(hijax  .lt. -990.)  hijax = 0.0
                   if(hi1jp1 .lt. -990.)  hi1jp1 = 0.0
                   if(hi1j1  .lt. -990.)  hi1j1 = 0.0
!.....             yfp = yfp + 0.5 * ( hi1jp1 - (0.5 * ( hijax + 
!.....                        hi1j1)) )/deltay  
                   yfp = 0.5 * ( hi1jp1 - (0.5 * ( hijax +                    &
     &                        hi1j1)) )/deltay  
                   yfp2 = yfp2 + 0.25 * ( ( hi1jp1 -                          &
     &                    (0.5 * (hijax + hi1j1)) )/ deltay )**2  
                  endif
!
! ===    the above does an average across the pole for the bndry in j.
!23456789012345678901234567890123456789012345678901234567890123456789012......
!
                  xfpyfp = xfpyfp + xfp * yfp
                  xl1 = xl1 + height * float(zslm(i1,j1))
                  xs1 = xs1 + height * float(1-zslm(i1,j1))
!
! === average the hx2, hy2 and hxy
! === this will be done over all land
!
               enddo
            enddo
!
! ===  htensr 
!
            if(xnsum.gt.1.) then
               slmij = float(nint(xland/xnsum))
               if(slmij.ne.0.) then
                  oroij= xl1 / xland
                  hx2ij =  xfp2  / xland
                  hy2ij =  yfp2  / xland
		  hxyij =  xfpyfp / xland
               else
                  oroij= xs1 / xwatr
               endif
!=== degub testing
      if (debug) then
          print *," i,j,i1,j1,height:", i,j,i1,j1,height,                    &
     &         xland,slmij
          print *," xfpyfp,xfp2,yfp2:",xfpyfp,xfp2,yfp2
          print *," hx2,hy2,hxy:",hx2ij,hy2ij,hxyij
      endif
!
! === make the principal axes, theta, and the degree of anisotropy, 
! === and sigma2, the slope parameter
!
               hkij = 0.5 * ( hx2ij + hy2ij )
               hlij = 0.5 * ( hx2ij - hy2ij )
               hlprimij = sqrt(hlij*hlij + hxyij*hxyij)
               if( hlij.ne. 0. .and. slmij .ne. 0. ) then
!
!hmhj            theta(i,j) = 0.5 * atan2d(hxyij,hlij)
                 theta(i,j) = 0.5 * atan2 (hxyij,hlij)
! ===   for testing print out in degrees
!                theta(i,j) = 0.5 * atan2(hxyij,hlij)
               endif
               sigma2ij =  ( hkij + hlprimij )
               sigma(i,j) =  sqrt(sigma2ij )
               if (sigma2ij .ne. 0.)                                         &
     &          gamma(i,j)=sqrt((hkij-hlprimij)/sigma2ij)
!.......testing
               endif
                  if (debug) then
       print *," i,j,theta,sigma,gamma,",i,j,theta(i,j),                     &
     &                                       sigma(i,j),gamma(i,j)
       print *," hk,hl,hlprim:",hkij,hlij,hlprimij
                  endif
        enddo
      enddo
      deallocate (deltax)
      deallocate (zavg,zslm)
      write(6,*) "! make principal coord  done"
!
#endif
      return
      end
