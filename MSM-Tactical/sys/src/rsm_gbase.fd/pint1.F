      subroutine pint1(bgrd,fgrd,ws,lat,km,igrd1s,ib1,ib2,               &    
     &                 jb1,jb2,jbx,istr,ilen,                            &
     &                 bgf,border,jgrd1)
!fpp$ noconcur r 
!$$$  subprogram documentation block                                            
!                .      .    .                                       .          
! subprogram:  pint1                                                            
!   prgmmr:  r.j.purser     org: w/nmc20    date: 94-04-06                      
!                                                                               
! abstract:  interpolate from field of source data on a coarse grid             
!            the same field at one row  of a finer target grid                  
!                                                                               
! program history log:                                                          
!	hann-ming henry juang - modified it for regional model.                       
!                                                                               
! usage:    call pint1(bgrd,fgrd,ws,lat,km)                                     
!   input argument list:                                                        
!     bgrd	-  base field grid for interpolation.          
!     ws	-  weights of (possibly smooth) value-interpolator                     
!     lat	-  athe lat-th row of grid.                                           
!     km	-  numbers of fields.                                                  
!                                                                               
!   output argument list:                                                       
!     fgrd	-  vaues in fine grid interpolated fron base field                   
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
!c#include <paramodel.h>
      integer bgf,border
      dimension bgrd(ib1:ib2,jb1:jbx,km)                                 &   
     &,tmp(ib1:ib2)                                                      &     
     &,fgrd(igrd1s,km)                                                   &      
     &,ws(bgf,1-border:border)                                               
!
      do k=1,km                                                                 
      do i=1,igrd1s                                                            
       fgrd (i,k)=0.                                                            
      enddo                                                                     
      enddo                                                                     
!                                                                               
      lats=lat                                                                  
      jbs=(lats-1)/bgf                                                         
      jfs=lats-jbs*bgf                                                         
!                                                                               
      do k=1,km                                                                 
!
      do ib=ib1,ib2                                                             
       tmp (ib)=0.                                                            
      enddo                                                                     
!-----------------------------------------------------------------------        
!  stage 1: y-interpolate from coarse grid of points (ib,jb)                    
!	    to this row of intermediate points (ib,lat)                               
!-----------------------------------------------------------------------        
      do iw=1-border,border                                                   
       w1 = ws(jfs,iw)                                                          
       jbb1=jbs+iw                                                              
       do ib=ib1,ib2                                                            
     	   tmp(ib)=tmp (ib)+ w1*bgrd(ib,jbb1,k) 
       enddo                                                                    
      enddo                                                                     
!-----------------------------------------------------------------------        
!  stage 2: x-interpolate from intermediate points (ib,lat)                     
!	    to this row (lat) of final grid of points (ix,lat)                        
!-----------------------------------------------------------------------        
      do ir=1,bgf                                                              
       istr0=istr-1+ir-1
       irr=mod(istr0,bgf)+1
       do iw=1-border,border                                                  
        i10=ir-iw*bgf
        w = ws(irr,iw)
      	do ib=iw,iw+(ilen-ir)/bgf 
         ibb=ib+istr0/bgf
         i11=i10+ib*bgf
         fgrd (i11,k)=fgrd (i11,k)+ w*tmp (ibb)
        enddo 
       enddo                                                                    
      enddo                                                                     
!
      enddo                                                                     
!
      return                                                                    
      end                              
