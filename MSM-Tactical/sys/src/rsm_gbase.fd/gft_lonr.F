!fpp$ noconcur r
      subroutine gft_lonr
!ccj
#include <paramodel.h>
!cc
!
! fts for ibm sp fft (lib essl)
! ft9 for fft99m
! ftc for cray fft
!
! when fts is on, all others should be off
! when ft9 is on, ft should be on
! when ftc is on, ft should be on
!
#ifdef FT
      common /comlonri/itest,ifax(20)
      common /com_lonr/trigs(lonr,2)
#endif
!
#ifdef FT
            itest=1
#endif
!           call set777 (trigs, ifax, lonr)
#ifdef FTC
            call fftfax (lonr,ifax,trigs)
#endif
#ifdef FT9
            call    fax (ifax, lonr,3)
            call fftrig (trigs,lonr,3)
#endif
#ifdef FT
            if (ifax(1) .eq. -99)  print 120
            if (ifax(1) .eq. -99)  stop
#endif
120         format (' error in gft_lonr.  lonr not factorable. ')
#ifdef FT
            print 140
#endif
140         format (' fftfax called in gft_lonr.  lonr = lonr ')
!
      return
      end
