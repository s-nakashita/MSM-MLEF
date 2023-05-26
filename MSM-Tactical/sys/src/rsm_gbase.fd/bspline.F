      subroutine bspline(x,dx,bx,dbx,nalpha)
!$$$  subprogram documentation block                                            
!                .      .    .                                       .          
! subprogram:   bspline                                                         
!   prgmmr:  r.j.purser      org: w/nmc20    date: 94-05-06                     
!                                                                               
! abstract:  evaluate a partition of unity at x, and its derivatives,           
!            induced by b-spline of presribed order on a uniform grid.          
!                                                                               
! program history log:                                                          
!   94-06-06  hann-ming henry juang                                             
!             custom it for regional spectral model.                            
!                                                                               
! usage:    call  bspline(x,dx,bx,dbx,nalpha)                                   
!   input argument list:                                                        
!     x         - the target position relative to the start of the                     
!                 basic interval.                                               
!     dx        - the width of the basic interval.                                     
!     nalpha    - order of b-spline (number of intervals comprising                
!                 its support.                                                  
!                                                                               
!   output argument list:                                                       
!     bx        - array of b-spline weights at x (starting with zeroth weight          
!     dbx       - x-derivative of bx                                                  
!                                                                               
!   input files: none                                                           
!                                                                               
!   output files: none                                                          
!                                                                               
!   subprograms called:                                                         
!       tay     - evaluate a taylor series at x, and its derivative                       
!                                                                               
!   remark: none                                                                
!                                                                               
! attributes:                                                                   
!   language: fortran 77.                                                       
!   machine:  cray c90.                                                         
!                                                                               
!$$$                                                                            
      parameter(malpha=3)
      dimension btsplin(0:malpha,0:malpha,0:malpha)                      &
     &,bx(0:*),dbx(0:*)
      data btsplin/1.,15*0., 1.,-1.,0.,0.,0.,1.,10*0.                    &
     &,.5,-1.,.5,0., .5,1.,-1.,0., 0.,0.,.5,0., 4*0.                     &
     &,.1666667,-.5,.5,-.1666667, .6666667,0.,-1.,.5                     &
     &,.1666667,.5,.5,-.5,	   0.,0.,0.,.1666667/
!jw
       save btsplin
!
      if(nalpha.lt.0.or.nalpha.gt.3) then
        print *,'nalpha out of range in bspline'
        stop
      endif
      do l=0,nalpha
       call tay(x,dx,btsplin(0,l,nalpha),nalpha,bx(l),dbx(l))
      enddo
      return
      end
