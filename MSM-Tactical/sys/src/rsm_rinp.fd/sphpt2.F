      subroutine sphpt2(kmx,wave,gridx,gridy,                            &                    
     &                  rlat,rlon,rsnln,rcsln,lngrdb1)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  sphpt2
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do global spherical transform the derivatives to a given
!            latitude-longitude grid.
!
! program history log:
!
! usage:    call sphpt2(km,wave,gridx,gridy,
!                       rlat,rlon,rsnln,rcsln,lngrdb)
!   input argument list:
!     km  - dimension of horizontal surface
!     wave  - global wave coefficient
!     rlat  - regional latitude values
!     rlon  - regional longitude values
!     rsnln - the coefficient to rotate global grid to regional grid
!     rcsln - the coefficient to rotate global grid to regional grid
!     lngrdb  - the total regional grid on a horizontal surface
!
!   output argument list:
!     gridx - regional grid x derivative
!     gridy - regional grid y derivative
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
      use physcons, rerth => con_rerth, pi => con_pi
!ccj
#include <paramodel.h>
!cc
!                                                                       
!  this routine performs inverse spherical transform on given lat/lon  
!  from triangular truncation to lat/lon grid with normal transform.
!c
!c    rcsln(new) = xg / xm * cos(lon)
!c
! use only kmx=1 in the model
!
!  sphpt1  does variable itself
!  sphpt2  does d a / dx and d a /dy
!  sphpt3  does a , da/dx and da/dy
!
!                                                                       
      dimension wave(lnt22,kmx)                                          &                   
     &         ,gridx(lngrdb1,kmx),gridy(lngrdb1,kmx)
!                                                                       
      dimension rlat(lngrdb1),rlon(lngrdb1)
      dimension rsnln(lngrdb1),rcsln(lngrdb1)
!c local dimension
      dimension qex(0:jcap)
      dimension gfftc(2,0:jcap),gfftx(2,0:jcap)  
      dimension ppnm(lnt2),hhnm(lnt2)
      dimension sumc(2,0:jcap,klot),sumy(2,0:jcap,klot)
!                                                                       
! ---------get fer and qimer  for spectral transfer
      print *,' --- in sphpt2 --- '
        er = rerth
        eriv = 1./er
!
        do 23 m=0,jcap
           qex  (m)=   eriv*float(m)
  23    continue
!
!c----------------------------------------------------------------
       do 20000 j=1,lngrdb1
!c
!c
      colrad=pi*0.5 - rlat(j)
      call rpln2i(ppnm,hhnm,colrad)
!c
      x=rlon(j)
      gfftc(1,0)=  1.0
      gfftc(2,0)=  0.0
      do 250 mm=1,jcap
      waven=float(mm)
      gfftc(1,mm)=   cos(waven*x) * 2.0
      gfftc(2,mm)= - sin(waven*x) * 2.0
250   continue
      do 255 mm=0,jcap
      gfftx(2,mm)=-gfftc(1,mm)*qex(mm)
      gfftx(1,mm)= gfftc(2,mm)*qex(mm)
255   continue

!                                                                       
!  do transform                                                 
!                                                                       
      do 400 n=1,2
      do 400 k=1,kmx
      do 400 m=0,jcap
      sumc(n,m,k) = 0.0
      sumy(n,m,k) = 0.0
400   continue
!fpp$ cncall
#ifdef CRA
      do l = 0, jcap
! ----------------------------------------------------
        ls=l*((2*jcap+3)-l)
         call sgemvx1( kmx ,jcap1-l,1.,wave(ls+1,1),lnt22,2,             &               
     &                ppnm(ls+1),2,1.,sumc(1,l,1),jcap1*2)
         call sgemvx1( kmx ,jcap1-l,1.,wave(ls+2,1),lnt22,2,             &
     &                ppnm(ls+2),2,1.,sumc(2,l,1),jcap1*2)
!c 
         call sgemvx1( kmx ,jcap1-l,1.,wave(ls+1,1),lnt22,2,             &
     &                hhnm(ls+1),2,1.,sumy(1,l,1),jcap1*2)
         call sgemvx1( kmx ,jcap1-l,1.,wave(ls+2,1),lnt22,2,             &
     &                hhnm(ls+2),2,1.,sumy(2,l,1),jcap1*2)
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
            sumy(1,l,k)=sumy(1,l,k)+hhnm(ls+1+ii)*wave(ls+1+ii,k)
            sumy(2,l,k)=sumy(2,l,k)+hhnm(ls+2+ii)*wave(ls+2+ii,k)
          enddo
        enddo
      enddo
#endif
!
      do 280 k=1,kmx
      gridx(j,k)=0.0
      gridy(j,k)=0.0
 280  continue
!fpp$ cncall
#ifdef CRA
         call sgemvx1( kmx ,2*jcap1,1.,sumc(1,0,1),2*jcap1,1,            &
     &                gfftx(1,0),1,1.,gridx(j,1),lngrdb1)
         call sgemvx1( kmx ,2*jcap1,eriv,sumy(1,0,1),2*jcap1,1,          &
     &                gfftc(1,0),1,1.,gridy(j,1),lngrdb1)
#else
        do i=1,jcap1
          ii=i-1
          do k=1,kmx
            gridx(j,k)=gridx(j,k)+gfftx(1,ii)*sumc(1,ii,k)               &
     &                           +gfftx(2,ii)*sumc(2,ii,k)
            gridy(j,k)=gridy(j,k)+gfftc(1,ii)*sumy(1,ii,k)*eriv          &
     &                           +gfftc(2,ii)*sumy(2,ii,k)*eriv
          enddo
        enddo
#endif
!
!c---------------------------------------------------
20000 continue
#ifdef ORIGIN_THREAD
!$doacross share(gridx,gridy,rsnln,rcsln,lngrdb1,kmx),
!$& local(qx,qy,n,k)
#endif
#ifdef CRAY_THREAD
!mic$ do all
!mic$1 shared(gridx,gridy,rsnln,rcsln,lngrdb1)
!mic$1 shared(kmx)
!mic$1 private(qx,qy,n,k)
#endif
#ifdef OPENMP
!$omp parallel do private(qx,qy,n,k)
#endif
        do 2200 k=1,kmx
        do 2200 n=1,lngrdb1
          qx=gridx(n,k)
          qy=gridy(n,k)
          gridx(n,k)= - qx * rsnln(n) - qy * rcsln(n)
          gridy(n,k)=   qx * rcsln(n) - qy * rsnln(n)
2200    continue
!   
!                                                                       
!     print *,' in sphpt2 total lgndr used ',all
!     print *,' in sphpt2 total transform 1 used ',all11
!     print *,' in sphpt2 total transform 2 used ',all12
!     print *,' in sphpt2 total transform 3 used ',all13
!     print *,' in sphpt2 total rotation used ',all2
      return                                                            
      end
