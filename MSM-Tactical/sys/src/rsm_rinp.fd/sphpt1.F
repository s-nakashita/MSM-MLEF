      subroutine sphpt1(kmx,wave,gridc,rlat,rlon,lngrdb1)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sphpt1
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do global spherical transform to a given latitude
!            longitude grid.
!
! program history log:
!
! usage:    call sphpt1(km,wave,gridc,rlat,rlon,lngrdb)
!   input argument list:
!     km  - dimension of horizontal surface
!     wave  - global wave coefficient
!     rlat  - regional latitude values
!     rlon  - regional longitude values
!     lngrdb  - the total regional grid on a horizontal surface
!
!   output argument list:
!     gridc - regional grid values
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
!   rpln2i  sgemvx1
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
      use physcons, pi => con_pi
!ccj
#include <paramodel.h>
!cc
!                                                                       
!  this routine performs inverse spherical transform on given lat/lon  
!  from triangular truncation to lat/lon grid with normal transform.
!c
!c    rcsln(new) = xg / xm * cos(lon)
!c
!  sphpt1  does variable itself
!  sphpt2  does d a / dx and d a /dy
!  sphpt3  does a , da/dx and da/dy
!
!                                                                       
      dimension wave(lnt22,kmx),gridc(lngrdb1,kmx)
      dimension rlat(lngrdb1),rlon(lngrdb1)
!                                                                       
!c local dimension
      dimension gfftc(2,0:jcap)
      dimension ppnm(lnt2),hhnm(lnt2)
      dimension sumc(2,0:jcap,klot)
!
      print *,' --- in sphpt1 --- '
!c----------------------------------------------------------------
       do 10000 j=1,lngrdb1
!c
!c
      colrad=pi*0.5 - rlat(j)
      call rpln2i(ppnm,hhnm,colrad)
!c
      x=rlon(j)
      gfftc(1,0)=  1.0
      gfftc(2,0)=  0.0
      do 150 mm=1,jcap
      waven=float(mm)
      gfftc(1,mm)=   cos(waven*x) * 2.0
      gfftc(2,mm)= - sin(waven*x) * 2.0
150   continue

!                                                                       
!  do transform                                                 
!                                                                       
      do 400 n=1,2
      do 400 k=1,kmx
      do 400 m=0,jcap
      sumc(n,m,k) = 0.0
400   continue
!
!fpp$ cncall
#ifdef CRA
      do l = 0, jcap
! ----------------------------------------------------
        ls=l*((2*jcap+3)-l)
         call sgemvx1( kmx ,jcap1-l,1.,wave(ls+1,1),lnt22,2,
     &                ppnm(ls+1),2,1.,sumc(1,l,1),jcap1*2)
         call sgemvx1( kmx ,jcap1-l,1.,wave(ls+2,1),lnt22,2,             &                         
     &                ppnm(ls+2),2,1.,sumc(2,l,1),jcap1*2)
! ----------------------------------------------------
      end do
#else
      do l=0,jcap
        ls=l*((2*jcap+3)-l)
        do i=1,jcap1-l
          ii=(i-1)*2
          do k=1,kmx
            sumc(1,l,k)=sumc(1,l,k)+ppnm(ls+1+ii)*wave(ls+1+ii,k)
            sumc(2,l,k)=sumc(2,l,k)+ppnm(ls+2+ii)*wave(ls+2+ii,k)
          enddo
        enddo
      enddo
#endif
!
      do 180 k=1,kmx
      gridc(j,k)=0.0
 180  continue
!fpp$ cncall
#ifdef CRA
         call sgemvx1( kmx ,2*jcap1,1.,sumc(1,0,1),2*jcap1,1,            &                          
     &                gfftc(1,0),1,1.,gridc(j,1),lngrdb1)
#else
        do i=1,jcap1
          ii=i-1
          do k=1,kmx
            gridc(j,k)=gridc(j,k)+gfftc(1,ii)*sumc(1,ii,k)               &            
     &                           +gfftc(2,ii)*sumc(2,ii,k)
          enddo
        enddo
#endif
!
!c---------------------------------------------------
10000 continue
!                                                                       
!     print *,' in sphpt1 total lgndr used ',all
!     print *,' in sphpt1 total transform 1 used ',all11
!     print *,' in sphpt1 total transform 2 used ',all12
!     print *,' in sphpt1 total transform 3 used ',all13
!     print *,' in sphpt1 total rotation used ',all2
      return                                                            
!                                                                       
      end                                                               
