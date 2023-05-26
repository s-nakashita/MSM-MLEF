      subroutine newsig(ci, si, del, sl, cl, rpi)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  newsig
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  set coordinates for levels from the sigma thickness of
!            layers, then use phillips method to get sigma layers.
!
! program history log:
!
! usage:    call newsig(ci, si, del, sl, cl, rpi)
!   input argument list:
!
!   output argument list:
!     ci     - 1 - sigma level (interface) values
!     si     - sigma level (interface) values
!     del    - sigma thickness
!     sl     - sigma layer values
!     cl     - 1 - sigma layer values
!     rpi    - (sl(k+1)/sl(k))**(r/cp)
!
!   input files: none
!
!   output files: none
!
!   input common block:
!       rsmodsig
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
      use physcons, rocp => con_rocp
!ccj
#include <paramodel.h>
!cc
!
      save
! ------------------********************-----------------------
! this routine sets coordinates for levels and use phillips method
! to get layers. ( can be stretched levels or not )
!  --------------------------------------------------------------
!
      parameter(idimi=igrd1i,jdimi=jgrd1i,ijdimi=idimi*jdimi,           &
     &          kdimi=levri,kdimpi=kdimi+1,kdimmi=kdimi-1)
      parameter(idimo=igrd1o,jdimo=jgrd1o,ijdimo=idimo*jdimo,           &
     &          kdimo=levro,kdimpo=kdimo+1,kdimmo=kdimo-1)
!
      parameter(kdim=kdimo,kdimp=kdim+1,kdimm=kdim-1)
!
      dimension ci(kdimp),si(kdimp),                                    &
     &          del(kdim),sl(kdim),cl(kdim),rpi(kdim)
!
      dimension delsig(kdim)
!
      real rk,rk1,rkinv
!
!  model sigma data in array delmdl of length levmdl
!ccj
#include <rsmdlsig.h>
!cc
!jw      data ifp/0/
      ifp=0
!***********************************************************************
!
      do 1 k=1,kdim
    1 del(k)=delmdl(k)
!
      ci(1) = 0.
      do 2 k=1,kdim
    2 ci(k+1)=ci(k)+del(k)
      ci(kdimp)=1.
!
      rk  = rocp
      rk1 = rk + 1.
      rkinv=1./rk
!
      do 3 li=1,kdimp
    3 si(li) = 1. - ci(li)
!
      do 4 le=1,kdim
      dif = si(le)**rk1 - si(le+1)**rk1
      dif = dif /(rk1*(si(le)-si(le+1)))
      sl(le) = dif**rkinv
      cl(le) = 1. - sl(le)
    4 continue
!
!     compute pi ratios for temp. matrix.
!
      do 5 le=1,kdimm
      rpi(le) = (sl(le+1)/sl(le))
    5 continue
      do 15 le=1,kdimm
      rpi(le) = rpi(le)**rk
   15 continue
!
      do 6 le=1,kdimp
      print 100, le, ci(le), si(le)
100   format (1h , 'level=', i2, 2x, 'ci=', f6.3, 2x, 'si=', f6.3)
    6 continue
!
      print 200
  200 format (1h0)
      do 7 le=1,kdim
      print 300, le, cl(le), sl(le), del(le)
  300 format (1h , 'layer=', i2, 2x, 'cl=', f6.3, 2x, 'sl=', f6.3, 2x,  &
     & 'del=', f6.3)
    7 continue
!
      print 400, (rpi(le), le=1,kdimm)
  400 format (1h0, 'rpi=', (18(1x,f6.3)) )
!
      return
      end
