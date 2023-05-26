      subroutine plnini
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  plnini
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  prepare array in common block for y-direction grid <-->
!            coefficient transformation for regional model.
!
! program history log:
!
! usage:    call plnini
!   input argument list:
!
!   output argument list:
!
!   common block:
! rscompln
!
!   input files: none
!
!   output files: none
!
!   subprograms called: none
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
#include <paramodel.h>
!     implicit double precision (a-g,o-z)
      double precision pi, rad, fc, fs, cc, ss, cogr, cowv
!
! jgrd1 should be even number.
!
      dimension fc(jgrd1),fs(jgrd1)
!ccj
#include <rscompln.h>
!c
           pi = asin(1.0d0) * 2.0d0
           do 10 i=2,jgrd1/2
           ii = i - 1
           rad = pi * float(ii) / float(jgrd)
           fc(i) = cos(rad)
           fs(i) = sin(rad)
           l = jgrd + 2 - i
           fc(l) = - fc(i)
           fs(l) =   fs(i)
 10        continue
           fc(1) = 1.0d0
           fs(1) = 0.0d0
!
!
      do 100 lat = 1, jgrd1
      j = lat - 1
!
      cogr=2.0d0 / float(jgrd)
      if( lat.eq.1 .or. lat.eq.jgrd1 ) cogr=cogr * 0.5d0
!
      do 100 ll=1,jwav1
         cowv=1.0d0
         if( ll.eq.1 .or. ll.eq.jgrd1 ) cowv=0.5d0
         l = ll - 1
         nl = l * iwav1
!
         jl0 = j * l
         jl = mod(jl0, jgrd)
         nf = mod(jl0/jgrd, 2)
         if( nf .eq. 1 ) then
             cc = -1.0
             ss = -1.0
         else
             cc =  1.0
             ss =  1.0
         endif
         jl = jl + 1
             ccosg(ll,lat) =  fc(jl)*cowv*cc
             csing(ll,lat) =  fs(jl)*cowv*ss
             gcosc(ll,lat) =  fc(jl)*cogr*cc
             gsinc(ll,lat) =  fs(jl)*cogr*ss
!
  100 continue
!c
      return
      end
