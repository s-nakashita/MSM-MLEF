      subroutine fplfml(fpl,fml,anl,lot)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  fplfml
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do wave rearrangement of global model used in regional model.
!
! program history log:
!
! usage:    call fplfml(fpl,fml,anl,alf,lot)
!   input argument list:
!     anl       - wave coefficent with dimension of (lonf22,lot)
!     alf       - tempory array with the same dimension as anl
!     lot       - the second dimension of anl and alf as well as 
!                 fpl and fml
!
!   output argument list:
!     fpl     - wave coefficent with dimension of (2,jcap1,lot)
!     fml     - wave coefficent with dimension of (2,jcap1,lot)
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
!ccj
#include <paramodel.h>
!cc
!.................................................................
       dimension                                                         &
     & fpl(2,jcap1,lot),fml(2,jcap1,lot),                                &   
     & anl(lonf22,lot)
!
!
      do 100 k=1,lot
!
      do  ll=1,jcap1
! do n.hemi
       fpl(1,ll,k)=anl(2*(ll-1)+1,k)+                                    & 
     &             anl(2*(ll-1)+1+lonf,k)
       fpl(2,ll,k)=anl(2*(ll-1)+2,k)+                                    &
     &             anl(2*(ll-1)+2+lonf,k)
!
! do s.hemi
!
       fml(1,ll,k)=anl(2*(ll-1)+1,k)-                                    &
     &             anl(2*(ll-1)+1+lonf,k)
       fml(2,ll,k)=anl(2*(ll-1)+2,k)-                                    &
     &             anl(2*(ll-1)+2+lonf,k)
      enddo
!
100   continue
!c
      return
      end
