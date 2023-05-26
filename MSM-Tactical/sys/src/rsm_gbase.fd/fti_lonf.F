!fpp$ noconcur r
      subroutine fti_lonf (a, b, lot, isign)
!ccj
#include <paramodel.h>
!cc
      parameter (lotmin=nvect_d,lotmax=nvect_d,ncpu=1)
      dimension a(lonf22,lot)
      dimension b(lonf22,lot)
!
#ifdef FF
      parameter (jumph=(lonf+2)/2,jump=jumph*2)
      dimension table(42000)
#endif
#ifdef FT
      parameter(jump=lonf+3)
      dimension work(lonf,lotmax,2)
      common /comlonfi/itest,ifax(20)
      common /com_lonf/trigs(lonf,2)
#endif
      dimension al(jump,lotmax)
!
!
      if (isign .eq.  1)  then
!
!  multiple fast fourier transform - synthesis.  isign=1
!  good for zonal wave number jcap.
!
!     dimension a(lonf,lot)
!
!   input - lot sets of complex coefficients in
!           a(1,j), a(2,j), ..., a(jcap1*2,j), j=1,...,lot.
!           a(jcap1*2+1,j), ..., a(lonf,j), j=1,...,lot are not set
!           before call fft_lonf.
!
!  output - lot sets of grid values in
!           a(1,j), a(2,j), ..., a(lonf,j), j=1,...,lot.
!
      nlot=max0(lot/ncpu,lotmin)
      nlot=min0(nlot    ,lotmax)
      do 460 i=1,lot,nlot
      lots = min0(nlot, lot-i+1)
      do 360 j=i,i+lots-1
      k=(j+1)/2
      loff=mod(j+1,2)*lonf
      do 320 l=1,twoj1
             al(l,j-i+1) = a(l+loff,k)
320   continue
      do 340 l=twoj1 + 1 ,jump
             al(l,j-i+1) = 0.0
340   continue
360   continue
!
!     call fft for systhesis.
!     call fft777 (al,work,trigs,ifax,1,jump,lonf,lots,1)
#ifdef FTC
      call rfftmlt(al,work,trigs,ifax,1,jump,lonf,lots,1)
#endif
#ifdef FT9
      call fft99m (al,work,trigs,ifax,1,jump,lonf,lots,1)
#endif
#ifdef FF
      scale=1.0
#endif
#ifdef FFD
      call dcrft(1,al,jumph,al,jump,lonf,lots,                          &
     &           -1,scale,table,22000,table(22001),20000)
      call dcrft(0,al,jumph,al,jump,lonf,lots,                          &
     &           -1,scale,table,22000,table(22001),20000)
#endif
#ifdef FFS
      call scrft(1,al,jumph,al,jump,lonf,lots,                          &
     &           -1,scale,table,22000,table(22001),20000)
      call scrft(0,al,jumph,al,jump,lonf,lots,                          &
     &           -1,scale,table,22000,table(22001),20000)
#endif
!
      do 440 j=i,i+lots-1
      k=(j+1)/2
      loff=mod(j+1,2)*lonf
      do 420 l=1,lonf
             a(l+loff,k) = al(l,j-i+1)
420   continue
440   continue
460   continue
!
      endif
!
!
      if (isign .eq. -1)  then
!
!  multiple fast fourier transform - analysis.  isign=-1
!  good for zonal wave number jcap.
!
!     dimension a(lonf,lot), b(lonf,lot)
!
!   input - lot sets of grid values in
!           a(1,j), a(2,j), ..., a(lonf,j), j=1,...,lot.
!           a array is not changed by sr fft_lonf.
!
!  output - lot sets of complex coefficients in
!           b(1,j), b(2,j), ..., b(jcap1*2,j), j=1,...,lot.
!           b(jcap1*2+1,j), ..., b(lonf,j), j=1,...,lot are not set.
!
      nlot=max0(lot/ncpu,lotmin)
      nlot=min0(nlot    ,lotmax)
      do 660 i=1,lot,nlot
      lots = min0(nlot, lot-i+1)
      do 560 j=i,i+lots-1
      k=(j+1)/2
      loff=mod(j+1,2)*lonf
      do 520 l=1,lonf
             al(l,j-i+1) = a(l+loff,k)
520   continue
560   continue
!
!     call fft for analysis.
!     call fft777 (al,work,trigs,ifax,1,jump,lonf,lots,-1)
#ifdef FTC
      call rfftmlt(al,work,trigs,ifax,1,jump,lonf,lots,-1)
#endif
#ifdef FT9
      call fft99m (al,work,trigs,ifax,1,jump,lonf,lots,-1)
#endif
#ifdef FF
      scale=1./float(lonf)
#endif
#ifdef FFD
      call drcft(1,al,jump,al,jumph,lonf,lots,                          &
     &           1,scale,table,22000,table(22001),20000)
      call drcft(0,al,jump,al,jumph,lonf,lots,                          &
     &           1,scale,table,22000,table(22001),20000)
#endif
#ifdef FFS
      call srcft(1,al,jump,al,jumph,lonf,lots,                          &
     &           1,scale,table,22000,table(22001),20000)
      call srcft(0,al,jump,al,jumph,lonf,lots,                          &
     &           1,scale,table,22000,table(22001),20000)
#endif
!
      do 640 j=i,i+lots-1
      k=(j+1)/2
      loff=mod(j+1,2)*lonf
      do 620 l=1,twoj1
             b(l+loff,k) = al(l,j-i+1)
620   continue
640   continue
660   continue
!
      endif
!
      return
      end
