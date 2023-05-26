      subroutine tay(x,dx,tco,nor,bx,dbx)
!$$$  subprogram documentation block                                            
!                .      .    .                                       .          
! subprogram:  tay                                                              
!   prgmmr:  r.j.purser      org: w/nmc20    date: 94-04-06                     
!                                                                               
! abstract:  evaluate a taylor series and its derivative                        
!                                                                               
! program history log:                                                          
!                                                                               
! usage:    call tay(x,dx,tco,nor,bx,dbx)                                       
!   input argument list:                                                        
!     x         - the target position relative to the start of                         
!                 the basic interval                                            
!     dx        - scaling factor                                                       
!     tco       - taylor coefficients                                                 
!     nor       - maximum degree of taylor series                                     
!                                                                               
!   output argument list:                                                       
!     bx        - value                                                                
!     dbx       - scaled derivative                                                   
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
      dimension tco(0:*)
      y=x/dx
      bx=tco(nor)
      do k=nor-1,0,-1
       bx=bx*y+tco(k)
      enddo
      dbx=nor*tco(nor)
      do k=nor-1,1,-1
       dbx=dbx*y+k*tco(k)
      enddo
      dbx=dbx/dx
      return
      end
