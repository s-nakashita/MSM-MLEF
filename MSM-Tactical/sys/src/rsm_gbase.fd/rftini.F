      subroutine rftini
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rftini
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  prepare fft initial constant arries for regional odel.
!
! program history log:
!
! usage:    call rftini
!   input argument list:
!
!   output argument list:
!
!   common block:
!      /comrfft/
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
! fftfax
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
!ccj
#include <paramodel.h>
!cc
!
! ff  for ibm sp
! ffd for ibm sp real8 essl lib
! ffs for ibm sp real8 essl lib
! ft  for fft99m and cray
! ft9 for fft99m
! ftc for cray
!
! ff should turn on if either ffd or ffs is on
! ft should turn on if either ftc or ft9 is on
!
#ifdef FT
      common /comrffti/ifax(20),jfax(20)
      common /comrfft/trigx(igrd2,2),trigy(jgrd2,2)
#endif
!
#ifdef FTC
      call fftfax (igrd2,ifax,trigx)
      call fftfax (jgrd2,ifax,trigy)
      do i=1,ifax(1)
         if ((ifax(i+1).ne.2).and.(ifax(i+1).ne.3).and.(ifax(i+1).ne.4)             &
     &      .and.(ifax(i+1).ne.4).and.(ifax(i+1).ne.7)                              &
     &      .and.(ifax(i+1).ne.11)) then
           print 120
           stop
         endif
      enddo
      do i=1,jfax(1)
         if ((jfax(i+1).ne.2).and.(jfax(i+1).ne.3).and.(jfax(i+1).ne.4)             &
     &      .and.(jfax(i+1).ne.4).and.(jfax(i+1).ne.7)                              &
     &      .and.(jfax(i+1).ne.11)) then
           print 120
           stop
         endif
      enddo
#endif
#ifdef FT9
      call    fax (ifax, igrd2,3)
      call fftrig (trigx,igrd2,3)
      call    fax (jfax, jgrd2,3)
      call fftrig (trigy,jgrd2,3)
#endif
#ifdef FT
      do i=1,ifax(1) 
          if ((ifax(i+1).ne.2).and.(ifax(i+1).ne.3)                                 &
     &         .and.(ifax(i+1).ne.4)) then
             print 120
             stop
          endif
      enddo
      do i=1,jfax(1)
          if ((jfax(i+1).ne.2).and.(jfax(i+1).ne.3)                                 &
     &         .and.(jfax(i+1).ne.4)) then
             print 130
             stop
          endif
      enddo
      if (ifax(1) .eq. -99) then
          print 120
          stop
#ifdef FT9
            else if ( jfax(1) .eq. -99) then
                print 130
                stop
#endif
      else
!         print 140,igrd2
      endif
#endif
!
120         format (' error in rftinni.  igrd2 not factorable. ')
130         format (' error in rftinni.  jgrd2 not factorable. ')
140         format (' fftfax is  called in rftini. igrd2 = ',i10)
!
      return
      end
