      subroutine pint3(bgrd,fgrd,fgrdx,fgrdy,ws,dws,lat,km,igrd1s,       &     
     &                 ib1,ib2,jb1,jb2,jbx,istr,ilen,                    &
     &                 bgf,border,jgrd1)
!fpp$ noconcur r                                                                
!$$$  subprogram documentation block                                            
!                .      .    .                                       .          
! subprogram:  pint3                                                            
!   prgmmr:  r.j.purser     org: w/nmc20    date: 94-04-06                      
!                                                                               
! abstract:   interpolate from single field of source data on a                 
!             coarse grid the 3 fields                                          
!             (i)     value (tii)                                               
!             (ii)    x-derivative (xii)                                        
!             (iii)   y-derivative (yii)                                        
!             to one row  of a finer target grid                                
!                                                                               
! program history log:                                                          
!	hann-ming henry juang - modified it for regional model.                       
!                                                                               
! usage:    call pint3(bgrd,fgrd,fgrdx,fgrdy,ws,dws,lat,km)                     
!   input argument list:                                                        
!     bgrd	-  base field grid for interpolation.                                
!     ws	-  weights of (possibly smooth) value-interpolator                     
!     dws	-  weights of (possibly smooth) derivative-interpolator               
!     lat	-  the lat-th row of grid.                                            
!     km	-  numbers of fields.                                                  
!                                                                               
!   output argument list:                                                       
!     fgrd	-  vaues in fine grid interpolated fron base field                   
!     fgrdx	-  x-derivative of fine grid interpolated fron base field           
!     fgrdy	-  y-derivative of fine grid interpolated fron base field           
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
#include <rscomloc.h>
      integer bgf,border,jgrd1
      dimension bgrd(ib1:ib2,jb1:jbx,km)                                 &      
     &,tmp(ib1:ib2,2),tmpy(ib1:ib2,2)                                    &      
     &,fgrd(igrd1s,km),fgrdx(igrd1s,km),fgrdy(igrd1s,km)                 &   
     &,ws(bgf,1-border:border),dws(bgf,1-border:border)                   
!
      do k=1,km                                                                 
      do i=1,igrd1s                                                           
       fgrd (i,k)=0.                                                            
       fgrdx(i,k)=0.                                                            
       fgrdy(i,k)=0.                                                            
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
       tmp (ib,1)=0.                                                            
       tmpy(ib,1)=0.                                                            
       tmp (ib,2)=0.                                                            
       tmpy(ib,2)=0.                                                            
      enddo                                                                     
!-----------------------------------------------------------------------        
!  stage 1: y-interpolate from coarse grid of points (ib,jb)                    
!	    to this row of intermediate points (ib,lat)                               
!-----------------------------------------------------------------------        
      do iw=1-border,border                                                   
       w1 = ws(jfs,iw)                                                          
       dw1=dws(jfs,iw)                                                          
       jbb1=jbs+iw                                                              
       do ib=ib1,ib2                                                            
	tmp (ib,1)=tmp (ib,1)+ w1*bgrd(ib,jbb1,k)
	tmpy(ib,1)=tmpy(ib,1)+dw1*bgrd(ib,jbb1,k)
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
	dw=dws(irr,iw)
	do ib=iw,iw+(ilen-ir)/bgf
         ibb=ib+istr0/bgf
         i11=i10+ib*bgf
	 fgrd (i11,k)=fgrd (i11,k)+ w*tmp (ibb,1)                       
	 fgrdx(i11,k)=fgrdx(i11,k)+dw*tmp (ibb,1)                       
	 fgrdy(i11,k)=fgrdy(i11,k)+ w*tmpy(ibb,1)                       
	enddo
       enddo                                                                    
      enddo 
!
      enddo                                                                     
!                                                                               
      do k=1,km                                                                 
      do i=1,igrd1s                                                           
       fgrdx(i,k)=fgrdx(i,k)/rdelx                                              
       fgrdy(i,k)=fgrdy(i,k)/rdely                                              
      enddo                                                                     
      enddo                                                                     
!
      return                                                                    
      end                                                                       
