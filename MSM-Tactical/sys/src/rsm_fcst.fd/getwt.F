      subroutine getwt(nor,nalpha,nr,wt,dwt)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  getwt
!   prgmmr:  r.j.purser     org: w/nmc20    date: 94-04-06
!
! abstract:  initialize array of interpolation coefficients according to 
!            order of accuracy required and (integer) refinement factor
!            between the two grids. 
!
! program history log:
!       hann-ming henry juang - modified it for regional model.
!
! usage:    call  getwt(nor,nalpha,nr,wt,dwt)
!   input argument list:
!     nor       -  the formal order of accuracy of the value-interpolator
!     nalpha    -  the order of smoothing of the value-interpolator
!                  (=0 defaults to nor-order lagrange interpolation,
!                   =1 is a linear averaging of two nor-order lagrange
!                      interpolators, and so on, so the overall width of
!                      the interpolation template is nor+nalpha)
!     nr        -  refinement factor
!
!   output argument list:
!     wt      - is the array of value-interpolation weights
!     dwt     - is the array of derivative-interpolation weights
!
!   input files: none
!
!   output files: none
!
!   subprograms called: 
!       ilagw  bspline  getwl  
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
!  get smooth-interpolant weights and weights for the corresponding
!  derivative.
      dimension wt(nr,1-(nor+nalpha)/2:(nor+nalpha)/2)                  &
     &	      ,dwt(nr,1-(nor+nalpha)/2:(nor+nalpha)/2)
      dimension bx(0:3),dbx(0:3),wl(20),dwl(20),qi(20)
      anr=nr
      nora=nor+nalpha
      norah=nora/2
      if(2*norah.ne.nora)  then
        print *,'nor + nalpha must be even for centered scheme'
        stop
      endif
      call ilagw(qi,nor,nr)
      do ir=1,nr
       do iw=1-norah,norah
         wt(ir,iw)=0.
         dwt(ir,iw)=0.
       enddo
       xf=ir-.5
       call bspline(xf,anr,bx,dbx,nalpha)
       do l=0,nalpha
        call getwl((l-norah)*nr,nr,xf,qi,wl,dwl,nor)
       do ic=1,nor
         iw=l+ic-norah
         wt(ir,iw)=wt(ir,iw)+bx(l)*wl(ic)
         dwt(ir,iw)=dwt(ir,iw)+bx(l)*dwl(ic)+dbx(l)*wl(ic)
       enddo
       enddo
      enddo
      return
      end
