      subroutine sg2sg(ps1,s1,var1,ps2,s2,var2,im,jm,km1,km2,in,tensn,   &    
     &                 iuv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sg2sg
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do vertical sigma to sigma interpolation
!
! program history log:
!
! usage:    call sg2sg(ps1,s1,var1,ps2,s2,var2,im,jm,km1,km2,in,tensn,
!          1                 iuv)
!   input argument list:
!          ps1   - primary ground pressure   alog(psfc)
!          ps2   - secondary gound pressure alog(psfc(cb))
!          s1    - primary sigma coordinate
!          var1  - primary 3 dimensional variable
!          s2    - secondary sigma coordinate
!          im    - dimension in x
!          jm    - dimension in y
!          km1   - dimension in z for primary coordinate and variable
!          km2   - dimension in z for secondary coordinate and variable
!          in    - control index:   0 for initial transformation
!                                   1 for transformation as previous
!          tensn - factor of tension 0 for cubic spline
!                                   50 for linear interpolation
!          iuv   - index for wind avoid extrapolation
!
!   output argument list:
!          var2  - secondary 3 dimensional variable
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
!       trispl  valts
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
      save
! **********
! this subroutine transfer from one defined sigma coordinate (s1) to
!  another sigma coordinate (s2).
! input    ps1     primary ground pressure   log(psfc)
!          ps2     secondary gound pressure log(psfc(cb))
!          s1      primary sigma coordinate
!          var1    primary 3 dimensional variable
!          s2      secondary sigma coordinate
!          var2    secondary 3 dimensional variable
!          im      dimension in x
!          jm      dimension in y
!          km1     dimension in z for primary coordinate and variable
!          km2     dimension in z for secondary coordinate and variable
!          in      control index:   0 for initial transformation
!                                   1 for transformation as previous
!          tensn   factor of tension 0 for cubic spline
!                                   50 for linear interpolation
!          iuv     index for wind avoid extrapolation
!
! local dimensions: st1 st2 sv1 sv2 q vinc
!
! common block /spl/ovh sh iflag theta
!
! routines related: trispl valts
!
! written by hann-ming henry juang
! date july 1988
!
      common /spli/iflag
      common /spl/ovh(100), sh(100), theta, undef
      dimension s1(im,jm,km1),s2(km2),var1(im,jm,km1),var2(im,jm,km2)
      dimension st1(100),st2(100),st0(im,jm,100),ps1(im,jm),ps2(im,jm)
      dimension sv1(100),sv2(100),q(100),vinc(100)
      undef=9999.9
! theta is tension factor
      theta=tensn

      if( in .eq. 0 ) then
      in=0
! let in=0 because st0 become a automatic array, cannot save.
! by zyf 10/19/09
      print *, ' initial values for transformation.'
      iflag = 0
      print *,'  iflag=',iflag,' tension factor=',theta
      do 1 i=1,im
      do 1 j=1,jm
      do 1 k=1,km1
      kk=km1-k+1
!!!      if( s1(i,j,kk) .gt. 0.0  .and.  s1(i,j,kk) .lt. 1.0 ) then
! hmhj
       st0(i,j,k)=log(s1(i,j,kk))
!!!      else
!!!       print *, 'error in s1(i,j,k) values at k=',k,' sigma=',s1(i,j,kk)
!!!       stop
!!!      endif
 1    continue
      do 2 k=1,km2
      kk=km2-k+1
      if( s2(kk) .gt. 0.0  .and.  s2(kk) .lt. 1.0 ) then
       st2(k)=log(s2(kk))
      else
       print *, ' error in s2(k) values at k=',k,' sigma=',s2(kk)
       stop
      endif
 2    continue
      vst=st2(1)
      do 3 k=1,km2-1
      vinc(k)=st2(k+1)-st2(k)
 3    continue
      endif

      do 10 i=1,im
      do 10 j=1,jm

      do 20 k=1,km1
! hmhj
      st1(k)=st0(i,j,k)+ps1(i,j)-ps2(i,j)
      sv1(k)=var1(i,j,km1-k+1)
 20   continue

      iflag=0
      call trispl(km1,st1,q,sv1)
      call valts(q,vst,vinc,st1,sv1,km2,km1,sv2)

      kmm=km2/2 + 1
      do 30 k=kmm,1,-1
      if( sv2(k) .eq. undef ) then
! hmhj
!x    sv2(k)=sv2(k+1)+(sv2(k+1)-sv2(k+2))*(st2(k)-st2(k+1))/
!x   &                (st2(k+1)-st2(k+2))
!xx   sv2(k)=sv2(k+1)
! hmhj use sv1 for sv2 
      sv2(k)=sv1(1)
      endif
 30   continue
      do 40 k=kmm,km2
      if( sv2(k) .eq. undef ) then
! hmhj
!x    sv2(k)=sv2(k-1)+(sv2(k-1)-sv2(k-2))*(st2(k)-st2(k-1))/
!x   &                (st2(k-1)-st2(k-2))
!xx   sv2(k)=sv2(k-1)
! hmhj use sv1 for sv2 
      sv2(k)=sv1(km1)
      endif
 40   continue

! ------ no extrapolation ----------
! modify lowest layer k=km2
!      ds2=abs(st2(km2)-st2(km2-2))
!      ds12=abs(st2(km2)-st1(km1))
!       if( ds2 .gt. ds12 ) then
!        sv2(km2)=sv2(km2-1)+(sv1(km1)-sv2(km2-1))*
!    &                (st2(km2)-st2(km2-1))/(st1(km1)-st2(km2-1))
!       endif
!
! modify most upper layer k=1
!      ds2=abs(st2(1)-st2(3))
!      ds12=abs(st2(1)-st1(1))
!       if( ds2 .gt. ds12 ) then
!        sv2(1)=sv2(2)+(sv1(1)-sv2(2))*(st2(1)-st2(2))/
!    &                (st1(1)-st2(2))
!       endif
!
! if iuv=1 then do not extrapolate above highest input level
!     if (iuv.eq.1) then
!       do 45 k=1,km2
!       if (st2(k).lt.st1(1)) sv2(k)=sv1(1)
!  45   continue
!     endif
!
      do 50 k=1,km2
      var2(i,j,k)=sv2(km2-k+1)
 50   continue

 10   continue

      return
      end
