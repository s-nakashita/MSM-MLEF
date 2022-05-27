#include <define.h>
      subroutine rread(n,label,fhour,idate,km,                           &   
     &                 flat,flon,fm2,fm2x,fm2y,sl,si,                    &
     &                 gz,q,te,uu,vv,rq                                  &
#ifdef NONHYD
     &                        ,pp,tt,ww                                  &
#endif
     &                                  )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rread
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  read regional sigma surface file.
!
! program history log:
!
! usage:    call rread(n,fhour,idate,flat,flon,fm2,fm2x,fm2y,sl,si,
!          1           gz,q,te,uu,vv,rq)
!   input argument list:
!     n         - file input unit number
!
!   output argument list:
!     fhour     - forecast hour
!     idate     - integrer date of hour month day year.
!     flat      - latitudes
!     flon      - longitudes
!     fm2       - square of map factor
!     fm2x      - d (fm2) / dx
!     fm2y      - d (fm2) / dy
!     sl        - sigma layers
!     si        - sigma interfaces
!     gz        - terrain height
!     q         - log surface pressure (cb)
!     te        - virtual temperature
!     uu        - x-direction wind 
!     vv        - y-direction wind
!     rq        - specific humidity
!
!   input files: 
!       n
!
!   output files: none
!
!   subprograms called:
!       shalfi  maxmin  
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
      use machine , only : kind_io4
!ccj
#include <paramodel.h>
!cc
!.....
!.....developed by hann-ming henry juang (nov 1990, version 0)......
!.....use u v as input, update u v tendency by div vor tendency.....
!.....
      parameter(levmax=100)
      character*8 label(4)
      real(kind=kind_io4) fhours,sis(km+1),sls(km),tmps(lngrd)
      real(kind=kind_io4) dummy(2*levmax+1-km-1-km)                      & 
     &         ,ext(512-(6+2*levmax))
      dimension idate(4),si(km+1),sl(km),                                &
     & flat(lngrd), flon(lngrd),                                         &
     &  fm2(lngrd), fm2x(lngrd), fm2y(lngrd),                            &
     & gz(lngrd), q(lngrd),te(lngrd,km),                                 &
     & uu(lngrd,km),vv(lngrd,km)                                         &
#ifdef NONHYD
     &,pp(lngrd,km),tt(lngrd,km),ww(lngrd,km+1)                          &
#endif
     & ,rq(lngrd,km,ntotal)
!
!     print *,' ---- begin rread -----'
!
      read(n) label
      print 3000,label,n
3000  format(1h0,'rread label  ',4a10,' n=',i3)
!
      read(n) fhours,idate,(sis(k),k=1,km+1),(sls(k),k=1,km)              &
     &         ,dummy,ext
      fhour=fhours
      call sgl2ful(sis,si,km+1)
      call sgl2ful(sls,sl,km)
      print *,' fhour idate si sl ',fhour,idate,si,sl
      read(n) (tmps(i),i=1,lngrd)
      call sgl2ful(tmps,gz,lngrd)
      call maxmin(gz,lngrd,1,1,1,'gz')
! 
      read(n) (tmps(i),i=1,lngrd)
      call sgl2ful(tmps,q,lngrd)
      call maxmin(q,lngrd,1,1,1,'q')
!
      do 100 k=1,km
      read(n) (tmps(i),i=1,lngrd)
      call sgl2ful(tmps,te(1,k),lngrd)
100   continue
      call maxmin(te,lngrd,km,1,km,'te')
!
      do 110 k=1,km
      read(n) (tmps(i),i=1,lngrd)
      call sgl2ful(tmps,uu(1,k),lngrd)
      read(n) (tmps(i),i=1,lngrd)
      call sgl2ful(tmps,vv(1,k),lngrd)
110   continue
      call maxmin(uu,lngrd,km,1,km,'uu')
      call maxmin(vv,lngrd,km,1,km,'vv')
!
      do 120 nc=1,ntotal
      do 120 k=1,km
      read(n) (tmps(i),i=1,lngrd)
      call sgl2ful(tmps,rq(1,k,nc),lngrd)
120   continue
      call maxmin(rq,lngrd,km,1,km,'rq')
!
#ifdef NONHYD
      do k=1,km
      read(n) (tmps(i),i=1,lngrd)
      call sgl2ful(tmps,pp(1,k),lngrd)
      enddo
      call maxmin(pp,lngrd,km,1,km,'pp')
!
      do k=1,km
      read(n) (tmps(i),i=1,lngrd)
      call sgl2ful(tmps,tt(1,k),lngrd)
      enddo
      call maxmin(tt,lngrd,km,1,km,'tt')
!
      do k=1,km+1
      read(n) (tmps(i),i=1,lngrd)
      call sgl2ful(tmps,ww(1,k),lngrd)
      enddo
      call maxmin(ww,lngrd,km+1,1,km+1,'ww')
#endif

      read(n)     (tmps(i),i=1,lngrd)
      call sgl2ful(tmps,fm2   ,lngrd)
      call maxmin(fm2,lngrd,1,1,1,'fm2')
      read(n)     (tmps(i),i=1,lngrd)
      call sgl2ful(tmps,fm2x  ,lngrd)
      read(n)     (tmps(i),i=1,lngrd)
      call sgl2ful(tmps,fm2y  ,lngrd)
      read(n)     (tmps(i),i=1,lngrd)
      call sgl2ful(tmps,flat  ,lngrd)
      read(n)     (tmps(i),i=1,lngrd)
      call sgl2ful(tmps,flon  ,lngrd)
!
!      call shalfi (gz,1)
!      call shalfi (q,1)
!      call shalfi (te,km)
!      call shalfi (uu,km)
!      call shalfi (vv,km)
!      do nc=1,ntotal
!       call shalfi (rq(1,1,nc),km)
!      enddo
!      call shalfi (fm2 ,1)
!      call shalfi (fm2x,1)
!      call shalfi (fm2y,1)
!      call shalfi (flat,1)
!      call shalfi (flon,1)
!c
      print *,' ------ end rread ----- '
!
      return
      end
