!fpp$ noconcur r
      subroutine gft_lonb
!ccj
#include <paramodel.h>
!cc
!
! ffs or ffd for ibm sp fft (lib essl)
! ft9 for fft99m
! ftc for cray fft
!
! when ffd is on, ff should be on
! when ffs is on, ff should be on
! when ft9 is on, ft should be on
! when ftc is on, ft should be on
!
#ifdef FT
      common /comlonbi/itest,ifax(20)
      common /com_lonb/trigs(lonb,2)
#endif
!
#ifdef FT
            itest=1
#endif
!           call set777 (trigs, ifax, lonb)
#ifdef FTC
            call fftfax (lonb,ifax,trigs)
#endif
#ifdef FT9
            call    fax (ifax, lonb,3)
            call fftrig (trigs,lonb,3)
#endif
#ifdef FT
            if (ifax(1) .eq. -99)  print 120
            if (ifax(1) .eq. -99)  stop
#endif
120         format (' error in gft_lonb.  lonb not factorable. ')
#ifdef FT
            print 140
#endif
140         format (' fftfax called in gft_lonb.  lonb = lonb ')
!
      return
      end
