      subroutine getwl(ixc0,idc,x,qi,wl,dwl,nor)
!$$$  subprogram documentation block                                            
!                .      .    .                                       .          
! subprogram:  getwl                                                            
!   prgmmr:  r.j.purser     org: w/nmc20    date: 94-04-06                      
!                                                                               
! abstract:  compute array of coefficients for interpolation of value and       
!            derivative from an integer-spaced source grid to a target point    
!            using the lagrange polynomial method                               
!                                                                               
! program history log:                                                          
!       hann-ming henry juang - modified it for regional model.                       
!                                                                               
! usage:    call  getwl(ixc0,idc,x,qi,wl,dwl,nor)                               
!   input argument list:                                                        
!     ixc0,idc  - indices that specify coordinate ixc0+ic*idc of                 
!                 icth node of source template                                  
!     x         - coordinate of target                                                 
!     qi        - array of partial weights precomputed in subroutine ilagw            
!     nor       - the order of lagrange interpolator and hence the template           
!                 size                                                          
!                                                                               
!   output argument list:                                                       
!     wl      - the array of value-interpolation weights for this target        
!     dwl     - the corresponding array of derivative-interpolation weights     
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
      dimension qi(*),wl(*),dwl(*),di(20)
      pd=1.
      sdi=0.
      do ic=1,nor
       xi=ixc0+ic*idc
       d=x-xi
       di(ic)=1./d
       sdi=sdi+di(ic)
       pd=pd*d
      enddo
      do ic=1,nor
       wl(ic)=pd*di(ic)*qi(ic)
       dwl(ic)=wl(ic)*(sdi-di(ic))
      enddo
      return
      end
