      subroutine rbgtio(junit1,junit2,wbgt,kk,nn,tmpbgt)                        
#include <paramodel.h>
#ifdef A  
      dimension   wbgt(lngrd,kk,nn)                                            
      dimension tmpbgt(lngrd)                                                  
#endif
!                                                                               
#ifdef A  
      print *,' enter rbgtio k=',kk,' n=',nn,                            &       
     &        ' junit1=',junit1,' junit2=',junit2                               
      do n=1,nn                                                                 
       do k=1,kk                                                                
         read(junit1) tmpbgt                                                    
         do i=1,lngrd                                                          
           tmpbgt(i)=tmpbgt(i) + wbgt(i,k,n)                                    
         enddo                                                                  
         write(junit2) tmpbgt                                                   
       enddo                                                                    
      enddo                                                                     
#endif
!                                                                               
#ifdef A  
      return                                                                    
#endif
      end                                                                       
