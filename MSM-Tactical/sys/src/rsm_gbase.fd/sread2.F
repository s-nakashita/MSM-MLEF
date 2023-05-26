      subroutine sread2(n,phy_f3d,phy_f2d)
!.....
!..... read fields for subroutine gscond in addition to sread
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
!
      allocate (sphy_f3d(igrd1,jgrd1,levr,num_p3d))
      allocate (sphy_f2d(igrd1,jgrd1,num_p2d))
#ifdef MP
      allocate (grid3d(igrd1,jgrd1,levr,num_p3d))
      allocate (grid2d(igrd1,jgrd1,num_p3d))
#define PHYS3D grid3d
#define PHYS2D grid2d
#else
#define PHYS3D phy_f3d
#define PHYS2D phy_f2d
#endif
!
#ifdef MP
          if( mype.eq.master ) then
#endif
      do m=1,num_p3d
      read(n)(((sphy_f3d(i,j,k,m),i=1,igrd1),                            & 
     &          j=1,jgrd1),k=1,levr)
      enddo
! 
      do m=1,num_p2d
      read(n)((sphy_f2d(i,j,m),i=1,igrd1),j=1,jgrd1)
      enddo
#ifdef MP
          endif
#endif
! 
      do m=1,num_p3d
      do k=1,levr
      do j=1,jgrd1
      do i=1,igrd1
         PHYS3D(i,j,k,m)=sphy_f3d(i,j,k,m)
      enddo
      enddo
      enddo
#ifdef MP
      call rmpgf2p(grid3d(1,1,1,m),igrd1,jgrd1,                          & 
     &            phy_f3d(1,1,1,m),igrd1p,jgrd1p,levr)
#endif
      enddo
      do m=1,num_p2d
      do j=1,jgrd1
      do i=1,igrd1
         PHYS2D(i,j,m)=sphy_f2d(i,j,m)
      enddo
      enddo
#ifdef MP
      call rmpgf2p(grid2d(1,1,m),igrd1,jgrd1,                            & 
     &            phy_f2d(1,1,m),igrd1p,jgrd1p,1)
#endif
      enddo
      deallocate (sphy_f3d)
      deallocate (sphy_f2d)
#undef PHYS3D
#undef PHYS2D
#ifdef MP
      deallocate (grid3d)
      deallocate (grid2d)
      if( mype.eq.master ) then
#endif
      print *,' ------ end sread2 ----- '
#ifdef MP
      endif
#endif
!
      return
      end
