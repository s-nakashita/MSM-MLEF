      subroutine ilagw(qi,nor,nr)
!$$$  subprogram documentation block                                            
!                .      .    .                                       .          
! subprogram:  ilagw                                                            
!   prgmmr:  r.j.purser     org: w/nmc20    date: 94-04-06                      
!                                                                               
! abstract:  compute array of partial coefficients for lagrange                 
!            interpolation from a uniform integer-spaced grid                   
!                                                                               
! program history log:                                                          
!       hann-ming henry juang - modified it for regional model.                       
!                                                                               
! usage:    call  ilagw(qi,nor,nr)                                              
!   input argument list:                                                        
!     nor       -  the order of lagrange interpolator and hence                       
!                  the template size                                            
!     nr        -  the spacing of the source points of the template                    
!                                                                               
!   output argument list:                                                       
!     qi      -  array of partial weights compute                               
!                                                                               
!   input files: none                                                           
!                                                                               
!   output files: none                                                          
!                                                                               
!   subprograms called:  none                                                   
!                                                                               
!   remark: none                                                                
!                                                                               
! attributes:                                                                   
!   language: fortran 77.                                                       
!   machine:  cray ymp.                                                         
!                                                                               
!$$$                                                                            
!  compute array qi of 1./numerators for lagrange polynomial weights
!  of order nor for the case of a source-grid with uniform spacing nr.
      dimension qi(*)
      do jw=1,nor
       q=1.
       do iw=1,nor
	if(iw.ne.jw)q=q*(jw-iw)*nr
       enddo
       qi(jw)=1./q
      enddo
      return
      end
