      subroutine rzerout
!ccj
#include <paramodel.h>
#ifdef MP
#define IGRD1S igrd1p
#define JGRD1S jgrd1p
#else
#define IGRD1S igrd1
#define JGRD1S jgrd1
#endif
#include <rscomfi.h>
#include <rscomspc.h>
#include <rscomgrd.h>
#include <rscomver.h>
#include <rscomio.h>
#include <rscompln.h>
#include <rscomrad.h>
#include <rscomloc.h>
!cc
!....< zero out precipitation and some fluxes >
!
#ifdef ORIGIN_THREAD
!$doacross share(dusfc,dvsfc,dtsfc,dqsfc,
!$&              geshem,bengsh,ulwsfc,dlwsfc,
!$&              gflux,tmpmax,tmpmin,spfhmax,spfhmin
!$&              wvuflx,wvvflx,srunoff,runoff,ep,
!$&              cldwrk,dugwd,dvgwd,psmean)
!$&         local(i,lat)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(dusfc,dvsfc,dtsfc,dqsfc)
!mic$1 shared(geshem,bengsh,ulwsfc,dlwsfc)
!mic$1 shared(gflux,tmpmax,tmpmin,spfhmax,spfhmin)
!mic$1 shared(wvuflx,wvvflx,srunoff,runoff,ep)
!mic$1 shared(cldwrk,dugwd,dvgwd,psmean)
!mic$1 private(i,lat)
#endif
#ifdef OPENMP
!$omp parallel do private(i,lat)
#endif
!c
      do lat=1,JGRD1S
        do i=1,IGRD1S
          geshem(i,lat) = 0.0e0
          bengsh(i,lat) = 0.0e0
          dusfc (i,lat) = 0.0e0
          dvsfc (i,lat) = 0.0e0
          dtsfc (i,lat) = 0.0e0
          dqsfc (i,lat) = 0.0e0
          ulwsfc(i,lat) = 0.0e0
          dlwsfc(i,lat) = 0.0e0
          gflux (i,lat) = 0.0e0
          tmpmax(i,lat) = 0.0e0
          tmpmin(i,lat) = 1.0e10
          spfhmax(i,lat) = 0.0e0
          spfhmin(i,lat) = 1.0e10
          wvuflx(i,lat) = 0.0e0
          wvvflx(i,lat) = 0.0e0
         srunoff(i,lat) = 0.0e0
          runoff(i,lat) = 0.0e0
          ep    (i,lat) = 0.0e0
          cldwrk(i,lat) = 0.0e0
          dugwd (i,lat) = 0.0e0
          dvgwd (i,lat) = 0.0e0
          psmean(i,lat) = 0.0e0
        enddo
      enddo
      do iv=1,27
        do j=1,JGRD1S
          do i=1,IGRD1S
            fluxr(i,j,iv) = 0.e0
          enddo
        enddo
      enddo
      do j=1,JGRD1S
        do i=1,IGRD1S
          cvavg(i,j) = 0.e0
        enddo
      enddo
      zhour=fhour
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
