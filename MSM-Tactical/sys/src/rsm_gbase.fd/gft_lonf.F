!fpp$ noconcur r
      subroutine gft_lonf
!ccj
#include <paramodel.h>
!cc
!
! ffs for ibm sp real*4 fft (lib essl)
! ffd for ibm sp real*8 fft (lib essl)
! ft9 for fft99m
! ftc for cray fft
!
! when ffd is on, ff should be on
! when ffs is on, ff should be on
! when ft9 is on, ft should be on
! when ftc is on, ft should be on
!
#ifdef FT
      common /comlonfi/itest,ifax(20)
      common /com_lonf/trigs(lonf,2)
#endif
!
#ifdef FT
            itest=1
#endif
!           call set777 (trigs, ifax, lonf)
#ifdef FTC
            call fftfax (lonf,ifax,trigs)
#endif
#ifdef FT9
            call    fax (ifax, lonf,3)
            call fftrig (trigs,lonf,3)
#endif
#ifdef FT
            if (ifax(1) .eq. -99)  print 120
            if (ifax(1) .eq. -99)  stop
#endif
120         format (' error in gft_lonf.  lonf not factorable. ')
#ifdef FT
!            print 140
#endif
140         format (' fftfax called in gft_lonf.  lonf = lonf ')
!
      return
      end
