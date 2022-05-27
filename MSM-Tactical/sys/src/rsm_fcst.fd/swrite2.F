      subroutine swrite2(n,phy_f3d,phy_f2d,iflag)
!.....
!..... output for subroutine gscond in addition to swrite
!.....
      use machine , only : kind_io4
!ccj
#include <paramodel.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
      real, allocatable :: grid3d(:,:,:,:)
      real, allocatable :: grid2d(:,:,:)
#define IGRD1S igrd1p
#define JGRD1S jgrd1p
#else
#define IGRD1S igrd1
#define JGRD1S jgrd1
#endif
!
      dimension phy_f3d(IGRD1S,JGRD1S,levr,num_p3d),                     &   
     &          phy_f2d(IGRD1S,JGRD1S,num_p2d)
      real(kind=kind_io4), allocatable :: sphy_f3d(:,:,:,:)
      real(kind=kind_io4), allocatable :: sphy_f2d(:,:,:)
      real zyftemp(igrd1,jgrd1,levr,num_p3d)
  
      integer iflag
!
      allocate (sphy_f3d(igrd1,jgrd1,levr,num_p3d))
      allocate (sphy_f2d(igrd1,jgrd1,num_p2d))
#ifdef MP
      allocate (grid3d(igrd1,jgrd1,levr,num_p3d))
      allocate (grid2d(igrd1,jgrd1,num_p2d))
#define PHYS3D grid3d
#define PHYS2D grid2d
#else
#define PHYS3D phy_f3d
#define PHYS2D phy_f2d
#endif
!
#ifdef MP
#endif
      do m=1,num_p3d
#ifdef MP
      call rmpgp2f(phy_f3d(1,1,1,m),igrd1p,jgrd1p,                       &  
     &             grid3d(1,1,1,m),igrd1,jgrd1,levr)
#endif
      do k=1,levr
      do j=1,jgrd1
      do i=1,igrd1
! iflag.eq.1 for output f_ice f_rain f_rimef
! let phy_f3d=-999.0 when num_p3d=4 (zhao scheme)
         if ((num_p3d.eq.3).and.(iflag.eq.1).and.(m.ne.3)) then
           sphy_f3d(i,j,k,m)=max(0.0, min(1.0,PHYS3D(i,j,k,m)))
         else if ((num_p3d.eq.4).and.(iflag.eq.1)) then
           sphy_f3d(i,j,k,m)=-999.0
         else
           sphy_f3d(i,j,k,m)=PHYS3D(i,j,k,m)
         endif
      enddo
      enddo
      enddo
      enddo
      do m=1,num_p2d
#ifdef MP
      call rmpgp2f(phy_f2d(1,1,m),igrd1p,jgrd1p,                         & 
     &             grid2d(1,1,m),igrd1,jgrd1,1)
#endif
      do j=1,jgrd1
      do i=1,igrd1
         sphy_f2d(i,j,m)=PHYS2D(i,j,m)
      enddo
      enddo
      enddo
!
#ifdef MP
          if( mype.eq.master ) then
#endif
      zyftemp=sphy_f3d
      if (iflag.eq.0) then
        do m=1,num_p3d
        write(n)(((sphy_f3d(i,j,k,m),i=1,igrd1),j=1,jgrd1),k=1,levr)
        enddo
!
        do m=1,num_p2d
        write(n)((sphy_f2d(i,j,m),i=1,igrd1),j=1,jgrd1)
        enddo
      else
        do m=1,3      
        call maxmin(zyftemp(:,:,:,m),igrd1*jgrd1,levr,1,levr,'phys3d')
        do k=1,levr
        write(n) ((sphy_f3d(i,j,k,m),i=1,igrd1),j=1,jgrd1)
        enddo
        enddo
      endif
#ifdef MP
          endif
#endif
      deallocate (sphy_f3d)
      deallocate (sphy_f2d)
#undef PHYS3D
#undef PHYS2D
#ifdef MP
      deallocate (grid3d)
      deallocate (grid2d)
      if( mype.eq.master ) then
#endif
      print *,' ------ end swrite2 ----- '
#ifdef MP
      endif
#endif
!
      return
      end
