      subroutine rsetsfc(gz,gzzb,gzxb,gzyb,epsx,epsy)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rsetsfc
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  prepare terrain derivative in grid from terrain height
!            in wave space.
!
! program history log:
!
! usage:    call rsetsfc(gz,gzxb,gzyb,epsx,epsy)
!   input argument list:
!     gz  - base terrain height in term of wave coefficient
!     epsx  - derivative coefficient in x direction
!     epsy  - derivative coefficient in y direction
!
!   output argument list:
!     gzxb     - base terrian derivative in x
!     gzxb     - base terrian derivative in y
!
!   common block:
! rscommap  rscompln
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
! sumfcs  ffsncs
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
!.....
!.....developed by hann-ming henry juang (nov 1990, version 0)......
!.....
      use physcons, grav => con_g
!ccj
#include <paramodel.h>
#include <rscompln.h>
#include <rscommap.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
#define IGRD1S  igrd1p
#define JGRD1S  jgrd1p
#define LNWAVS  lnwavp
#define LNGRDS  lngrdp
#else
#define IGRD1S  igrd1
#define JGRD1S  jgrd1
#define LNWAVS  lnwav
#define LNGRDS  lngrd
#endif
!cc
!....
      dimension  gz(LNWAVS),                                             &  
     & gzzb(LNGRDS), gzxb(LNGRDS), gzyb(LNGRDS),                         &
     & epsx(LNWAVS), epsy(LNWAVS)
      real, allocatable :: gzx(:),gzy(:)
      real, allocatable :: gzzg(:,:),gzxg(:,:),gzyg(:,:)
#ifdef MP
      real, allocatable :: gzk(:),gzxk(:),gzyk(:)
      real, allocatable :: gzzgk(:,:),gzxgk(:,:),gzygk(:,:)
      real, allocatable :: gzzgg(:,:),gzxgg(:,:),gzygg(:,:)
#endif
!.................................................................
!
#ifdef MP
      if(mype.eq.master) then
#endif
      print *,' ------ in setsfc ---------' 
#ifdef MP
      endif
#endif
      allocate(gzx(LNWAVS))
      allocate(gzy(LNWAVS))
      allocate(gzzg(IGRD1S,JGRD1S))
      allocate(gzxg(IGRD1S,JGRD1S))
      allocate(gzyg(IGRD1S,JGRD1S))
#ifdef MP
      allocate(gzk(llwavp))
      allocate(gzxk(llwavp))
      allocate(gzyk(llwavp))
      allocate(gzzgk(igrd1p,jgrd1))
      allocate(gzxgk(igrd1p,jgrd1))
      allocate(gzygk(igrd1p,jgrd1))
      allocate(gzzgg(igrd1,jgrd1p))
      allocate(gzxgg(igrd1,jgrd1p))
      allocate(gzygg(igrd1,jgrd1p))
#endif
      do 300 n=1,LNWAVS
      gzx(n) = -epsx(n) * gz(n)
      gzy(n) = -epsy(n) * gz(n)
 300  continue
!
!.................................................................
!
!           lat loop
!
#ifdef MP
      call rmpnn2n(gz,lnwavp,gzk,llwavp,1)
      call rmpnn2n(gzx,lnwavp,gzxk,llwavp,1)
      call rmpnn2n(gzy,lnwavp,gzyk,llwavp,1)
!
!      if ( mype.eq.0) then
!        print *,'in rsetsfc 0, gzxk=',(gzxk(i),i=1,10)
!        print *,'in rsetsfc 0, gzyk=',(gzyk(i),i=1,10)
!        print *,'in rsetsfc 0, gzk=',(gzk(i),i=1,10)
!      endif

      call ffsncsy(gzxk,gzyk,gzxgk,gzygk,1)
      call ffscosy1(gzk,gzzgk,1,iwav1p,igrd1p,1,1)
!      if ( mype.eq.0) then
!        print *,'in rsetsfc 1, iwav1p=',iwav1p,'igrd1p=',igrd1p
!        print *,'in rsetsfc 1, gzxgk=',(gzxgk(i,1),i=1,10)
!        print *,'in rsetsfc 1, gzygk=',(gzygk(i,1),i=1,10)
!        print *,'in rsetsfc 1, gzzgk=',(gzzgk(i,1),i=1,10)
!      endif
!
      call rmpnl2ny(gzzgk,igrd1p,jgrd1,gzzgg,igrd1,jgrd1p,               &  
     &              1,1,1)
      call rmpnl2ny(gzxgk,igrd1p,jgrd1,gzxgg,igrd1,jgrd1p,               &
     &              1,1,1)
      call rmpnl2ny(gzygk,igrd1p,jgrd1,gzygg,igrd1,jgrd1p,               &
     &              1,1,1)
!
      call ffsncsx( gzygg,gzxgg,jgrd1p, 1 )
      call ffscosx1( gzzgg,jgrd1p,1,1,1 )
!      if ( mype.eq.0) then
!        print *,'in rsetsfc 2, gzxgg=',(gzxgg(i,1),i=1,10)
!        print *,'in rsetsfc 2, gzygg=',(gzygg(i,1),i=1,10)
!        print *,'in rsetsfc 2, gzzgg=',(gzzgg(i,1),i=1,10)
!      endif
!
      call rmpx2nx(gzzgg,igrd1,1,gzzg,igrd1p,1,jgrd1p,                   &
     &             1,1,1)
      call rmpx2nx(gzxgg,igrd1,1,gzxg,igrd1p,1,jgrd1p,                   &
     &             1,1,1)
      call rmpx2nx(gzygg,igrd1,1,gzyg,igrd1p,1,jgrd1p,                   &
     &             1,1,1)
!
#else
!y direction: sum
      call sumfcs1(gzx,gzy,gzxg,gzyg,ccosg,csing,                        &   
     &      lnwav,iwav1,jwav1,igrd1,1)
      call sumffc1(gz,gzzg,ccosg,                                        &
     &      lnwav,iwav1,jwav1,igrd1,1)
!x direction: fft
      call ffsncsx( gzyg,gzxg,jgrd1,1)
      call ffscosx1( gzzg,jgrd1,1,1,1 )
#endif
!
! ----- grid point values obtained --------
!
#ifdef MP
#define LATLENS latlen(mype)
#define LONLENS lonlen(mype)
#else
#define LATLENS jgrd1
#define LONLENS igrd1
#endif
#ifdef OPENMP
!$omp parallel do private(lat,jlat,i,j)
#endif
      do lat=1,LATLENS
        jlat=(lat-1)*IGRD1S
        do i=1,LONLENS
          j = i + jlat
          gzdx(i,lat) = grav * ( gzxg(i,lat) + gzxb(j) )
          gzdy(i,lat) = grav * ( gzyg(i,lat) + gzyb(j) )
          gzzb(j)     =          gzzg(i,lat) + gzzb(j)
        enddo
      enddo
!
#ifdef MP
      if(mype.eq.master) then
#endif
      print *,' ----- end setsfc ---- '
#ifdef MP
      endif
      deallocate(gzk)
      deallocate(gzxk)
      deallocate(gzyk)
      deallocate(gzzgk)
      deallocate(gzxgk)
      deallocate(gzygk)
      deallocate(gzzgg)
      deallocate(gzxgg)
      deallocate(gzygg)
#endif

      deallocate(gzx)
      deallocate(gzy)
      deallocate(gzzg)
      deallocate(gzxg)
      deallocate(gzyg)
!
      return
      end
