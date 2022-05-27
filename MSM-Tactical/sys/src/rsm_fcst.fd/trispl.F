      subroutine trispl(n,p,q,y)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  trispl
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  calculates weights for cubic spline interpolation
!
! program history log:
!
! usage:    call trispl(n,p,q,y)
!   input argument list:
!     n         - number of observations
!     p         - coor (independent variables)
!     y         - function y(p) to be interpolated
!
!   output argument list:
!     q         - weight
!
!   common block:
!     /spl/
!
!   input files: none
!
!   output files: none
!
!   subprograms called: none
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
      save
!-----------------------------------------------------
!    subroutine calculates weights for cubic spline interpolation
!     n...number ofc     n...number of observations
!     y...function y(p) to be interpolated
!     p...coor (independent variables)
!     q,ovh,sh...contain weights
!----------------------------------------------------
!
      common/spli/iflag
      common/spl/ovh(100),sh(100),theta,undef
      dimension c(100),b(100),p(n),q(n),d(100),y(n),                    &
     &          tchs(100),tb(100)
      nu=n-1
      if (iflag.ne.0) go to 99
      do 9 i=2,n
      im=i-1
      him=p(i)-p(im)
      if(him .eq. 0.) him=0.001
      ovh(im)=1/him
      th=theta*him
       sh(im)=1/sinh(th)
 9    tchs(im)=(theta*cosh(th))*sh(im)-ovh(im)
      do 8 i=2,nu
      im=i-1
      c(im)=ovh(im)-theta*sh(im)
 8    b(i)=tchs(im)+tchs(i)
      c(n-1)=ovh(n-1)-theta*sh(n-1)
      b(1)=tchs(1)
      b(n)=tchs(n-1)
      tb(1)=c(1)/b(1)
      do 7 i=2,nu
 7    tb(i)=c(i)/(b(i)-c(i-1)*tb(i-1))
      d(1)=0.
      d(n)=0.
      iflag=1
 99   yp=(y(2)-y(1))*ovh(1)
      do 6 i=2,nu
      ynow=(y(i+1)-y(i))*ovh(i)
      d(i)=ynow-yp
 6    yp=ynow
      q(1)=d(1)/b(1)
      do 5 i=2,n
      im=i-1
 5     q(i)=(d(i)-c(im)*q(im))/(b(i)-c(im)*tb(im))
      do 4 i=1,nu
      ii=n-i
 4    q(ii)=q(ii)-tb(ii)*q(ii+1)
      return
      end
