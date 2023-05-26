#include <define.h>
      subroutine sumtopmp(ap,top,qvv,llstr,llens,lwvdef,kall,jcap        & 
#ifndef MP
     &           ,lonf                                                   &
#else
     &           ,jcapp,lcapp                                            &
#endif
     &            )
!c                                                                              
!c#include <paramodel.h>
#ifdef MP
#define JCAPS jcapp
#define LCAPS lcapp
#else
#define JCAPS jcap
#define LCAPS lonf
#endif
!c                                                                              
      dimension     ap(2,0:LCAPS,kall)   
      dimension    top(2,0:JCAPS,kall)  
      dimension    qvv(2,0:JCAPS)      
      dimension    lwvdef(jcap+1)     
!c local array
      dimension   qtop(2,0:JCAPS)
!
!c                                                                              
      lenh=LCAPS/2
      do 1000 k=1,kall   
!
        do 500 lx=1,llens
          l=lwvdef(llstr+lx)
          ll=lx-1
!
          qtop(1,ll) = top(1,ll,k) * qvv(1,ll)
          qtop(2,ll) = top(2,ll,k) * qvv(2,ll)
!
          ap(1,ll,k) = ap(1,ll,k) + qtop(1,ll)
          ap(2,ll,k) = ap(2,ll,k) + qtop(2,ll)
!c                                                                              
          if( mod(l,2).eq.0 ) then
!
! odd wavenumber  for l=0,2,4,.....
!
            ap(1,ll+lenh,k) = ap(1,ll+lenh,k) - qtop(1,ll)
            ap(2,ll+lenh,k) = ap(2,ll+lenh,k) - qtop(2,ll)
!c  
          else
!c                                                                              
! even wavenumber for l=1,3,5,......
!
            ap(1,ll+lenh,k) = ap(1,ll+lenh,k) + qtop(1,ll)
            ap(2,ll+lenh,k) = ap(2,ll+lenh,k) + qtop(2,ll)
!
          endif
!c                                                                              
  500   continue                                                                  
!
 1000 continue
!c                                                                              
      return                                                                    
      end                                                                       
