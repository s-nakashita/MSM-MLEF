#include <define.h>
      subroutine rwrite(n,label,fhour,idate,km,                          &  
     &                  flat,flon,fm2,fm2x,fm2y,sl,si,                   &
     &                  gz,q,te,uu,vv,rq                                 &
#ifdef NONHYD
     &                  ,pp,tt,ww                                        &
#endif
     &                   )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rwrite
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  read regional sigma surface file.
!
! program history log:
!
! usage:    call rwrite(n,fhour,idate,flat,flon,fm2,fm2x,fm2y,sl,si,
!          1           gz,q,te,uu,vv,rq)
!   input argument list:
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
!   output argument list:
!     n         - file input unit number
!
!   input files:  none
!
!   output files:
!       n
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
!.....
!.....developed by hann-ming henry juang (nov 1990, version 0)......
!.....use u v as input, update u v tendency by div vor tendency.....
!.....
      use machine , only : kind_io4
      use physcons, only : pi => con_pi
!ccj
#include <paramodel.h>
#include <rscomloc.h>
!cc
      parameter(levmax=100)
      character*8 label(4)
      real(kind=kind_io4) fhours,sis(km+1),sls(km),tmps(lngrd)
      real(kind=kind_io4) dummy(2*levmax+1-km-1-km)                      &  
     &         ,ext(512-(6+2*levmax))
      dimension idate(4),si(km+1),sl(km),                                &
     & flat(lngrd), flon(lngrd),                                         &
     &   fm2(lngrd), fm2x(lngrd), fm2y(lngrd),                           &
     & gz(lngrd), q(lngrd),te(lngrd,km),                                 &
     & uu(lngrd,km),vv(lngrd,km),rq(lngrd,km,ntotal)                     &
#ifdef NONHYD
     &,pp(lngrd,km),tt(lngrd,km),ww(lngrd,km+1)                          &
#endif
     & , rwrttmp(2)
!
!c
      label(1)='ncep'
      label(2)='rsm'
      label(3)='mpi'
      label(4)='version'
!c
      write(n) label
      print 3000,label,n
3000  format(1h0,'rwrite label  ',4a10,' n=',i3)
!
      do i=1,2*levmax+1-km-1-km
      dummy(i)=0.0
      enddo
      do i=1,512-(6+2*levmax)
      ext(i)=0.0
      enddo
!
      ext(1)=iwav1
      ext(2)=jwav1
      ext(3)=igrd1
      ext(4)=jgrd1
      ext(5)=km
      ext(6)=2+km*4+5
      ext(7)=rproj             !projection flag
      ext(8)=rtruth
      ext(9)=rorient
      ext(10)=rcenlat
      ext(11)=rcenlon
      ext(12)=rlftgrd
      ext(13)=rbtmgrd
      ext(14)=rdelx
      ext(15)=rdely
#ifdef NONHYD
      ext(16)=1.0
#endif
!
      fhours=fhour
      call ful2sgl(si,sis,km+1)
      call ful2sgl(sl,sls,km)
      write(n) fhours,idate,(sis(k),k=1,km+1),(sls(k),k=1,km)            &  
     &         ,dummy,ext
!
      print *,' fhour idate si sl ',fhour,idate,si,sl
      print *,' other model experimental constants: '                    &
     &       ,' wavei wavej gridi gridj xlayers totfld '                 &
     &         ,(ext(i),i=1,6)                                           &
     &       ,' proj truth orient cenlat cenlon '                        &
     &         ,(ext(i),i=7,11)                                          &
     &       ,' grdleft grdbotm delx dely '                              &
     &         ,(ext(i),i=12,15)
!
!jun fft -- no shalfi in setgrd
!      call shalfo (gz,1)
!      call shalfo (q,1)
!      call shalfo (te,km)
!      call shalfo (uu,km)
!      call shalfo (vv,km)
!      do nc=1,ntotal
!       call shalfo (rq(1,1,nc),km)
!      enddo
!      call shalfo (fm2 ,1)
!      call shalfo (fm2x,1)
!      call shalfo (fm2y,1)
!      call shalfo (flat,1)
!      call shalfo (flon,1)
!
      call ful2sgl(gz,tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
!     call maxmin(gz,lngrd,1,1,1,'gz')
!
      call ful2sgl(q,tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
!     call maxmin(q,lngrd,1,1,1,'q')
!
      do 300 k=1,km
      call ful2sgl(te(1,k),tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
300   continue
!     call maxmin(te,lngrd,km,1,km,'te')
!
      do 310 k=1,km
      call ful2sgl(uu(1,k),tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
      call ful2sgl(vv(1,k),tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
310   continue
!     call maxmin(uu,lngrd,km,1,km,'uu')
!     call maxmin(vv,lngrd,km,1,km,'vv')
!
      do 320 nc=1,ntotal
      do 320 k=1,km
      call ful2sgl(rq(1,k,nc),tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
320   continue
!     call maxmin(rq,lngrd,km,1,km,'rq')
!
#ifdef NONHYD
      do k=1,km
      call ful2sgl(pp(1,k),tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
      enddo
      call maxmin(pp,lngrd,km,1,km,'pp')
      do k=1,km
      call ful2sgl(tt(1,k),tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
      enddo
      call maxmin(tt,lngrd,km,1,km,'tt')
      do k=1,km+1
      call ful2sgl(ww(1,k),tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
      enddo
      call maxmin(ww,lngrd,km+1,1,km+1,'ww')
#endif
      call maxmin(fm2,lngrd,1,1,1,'fm2')
      call ful2sgl(fm2,tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
      call ful2sgl(fm2x,tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
      call ful2sgl(fm2y,tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
      call maxmin(flat,lngrd,1,1,1,'flat')
      call ful2sgl(flat,tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
      ! add 20220720
      if(maxval(flon).gt.2.0e0*pi) then
        flon = flon - 2.0e0*pi
      end if
      call maxmin(flon,lngrd,1,1,1,'flon')
      call ful2sgl(flon,tmps,lngrd)
      write(n) (tmps(i),i=1,lngrd)
!
!      call shalfi (gz,1)
!      call shalfi (q,1)
!      call shalfi (te,km)
!      call shalfi (uu,km)
!      call shalfi (vv,km)
!      do nc=1,ntotal
!       call shalfi (rq(1,1,nc),km)
!      enddo
!      call shalfi (fm2, 1)
!      call shalfi (fm2x,1)
!      call shalfi (fm2y,1)
!      call shalfi (flat,1)
!      call shalfi (flon,1)
!
!     close(n)
      print *,' ------ end rwrite ----- '
!
      return
      end
