#include <define.h>
      subroutine flpflm(flp,flm,anl,lwvlen,lota                         &
#ifndef MP
     &           ,jcap1,lonf,lonf22                                     &
#else
     &           ,jcap1p,lcapp,lcap22p                                  &  
#endif
     &            )
!.................................................................              
!  for loopa                                                                    
!                                                                               
!c#include <paramodel.h>
#ifdef MP
#define JCAP1S jcap1p
#define LCAPS  lcapp
#define LCAP22S lcap22p
#else
#define JCAP1S jcap1
#define LCAPS lonf
#define LCAP22S lonf22
#endif
       dimension                                                        &
     & flp(2,JCAP1S,lota),flm(2,JCAP1S,lota),                           & 
     & anl(LCAP22S,lota)                                                     
!                                                                               
!.................................................................              
!                                                                               
      do k=1,lota                                                            
      do  ll=1,lwvlen
! do n.hemi                                                                     
       flp(1,ll,k)=anl(2*(ll-1)+1,k)+                                   &       
     &             anl(2*(ll-1)+1+LCAPS,k)
       flp(2,ll,k)=anl(2*(ll-1)+2,k)+                                   &       
     &             anl(2*(ll-1)+2+LCAPS,k)
!                                                                               
! do s.hemi                                                                     
!                                                                              
       flm(1,ll,k)=anl(2*(ll-1)+1,k)-                                   &       
     &             anl(2*(ll-1)+1+LCAPS,k)                                      
       flm(2,ll,k)=anl(2*(ll-1)+2,k)-                                   &       
     &             anl(2*(ll-1)+2+LCAPS,k)
      enddo                                                                     
      enddo                                                                     
!.................................................................              
      return                                                                    
      end                                                                       
