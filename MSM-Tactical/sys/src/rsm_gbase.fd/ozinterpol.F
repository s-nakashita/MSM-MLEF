!**********************************************************************
      subroutine ozinterpol(idate,fhour)
!
      use machine , only : kind_phys
!c
#include <paramodel.h>
#ifdef MP
#define LNGRDS lngrdp
#else
#define LNGRDS lngrd
#endif
#include <rscomoz.h>
!
      integer   iday,j,j1,j2,l
      integer   mon,nskip,nskipo
      real(kind=kind_phys) fhour,ptem
!
      integer idate(4)
      integer   idat(8),jdat(8)
!
      real(kind=kind_phys) rinc(5)
!
      idat=0
      idat(1)=idate(4)
      idat(2)=idate(2)
      idat(3)=idate(3)
      idat(5)=idate(1)
      rinc=0.
      rinc(2)=fhour
      call w3movdat(rinc,idat,jdat)
      mon=jdat(2)
      iday=jdat(3)
!
      nskip = (mon-1)*3 + (iday-1) / 10
      if (iday .eq. 31) nskip = nskip -1
      nskip=nskip+1
!
      do l=1,lev46
         do j=1,LNGRDS
            j1  = jindx1(j)
            j2  = jindx2(j)
            ptem = 1.0 - ddy(j)
!
         ozprdout(j,l)=                                                  &    
     &   (ptem*ozprdin(j1,l,nskip)+ddy(j)*ozprdin(j2,l,nskip))*1.655    
!
         ozdisout(j,l)=                                                  &    
     &    ptem*ozdisin(j1,l,nskip)+ddy(j)*ozdisin(j2,l,nskip)  
!
         enddo
      enddo
!
      return
      end
