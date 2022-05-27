      subroutine extrm2(ngg,nggs,a,amin,amax,b,bmin,bmax,lgth,ind)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    extrem      prints minimums and maximums of 2 arrays.
!   prgmmr: joseph sela      org: w/nmc23    date: 88-04-01
!
! abstract: prints minimums and maximums of 2 arrays.
!
! program history log:
!   88-04-01  joseph sela
!
! usage:    call extrem (a, b, lgth, ind)
!   input argument list:
!     a        - finds and prints minimum and maximum of array a.
!     b        - finds and prints minimum and maximum of array b.
!     lgth     - length of array a and array b.
!     ind      - indicator printed with minimums and maximums.
!
!   output files:
!     output   - printout file.
!
! remarks: lgth should be greater than 1.
!
! attributes:
!   language: fortran 200
!   machine:  cyber 205
!
!$$$
      dimension a(lgth), b(lgth)
      do 25 i=1,lgth
      if (a(i).gt.amax)  amax = a(i)
      if (a(i).lt.amin)  amin = a(i)
25    continue
!
      do 50 i=1,lgth
      if (b(i).gt.bmax)  bmax = b(i)
      if (b(i).lt.bmin)  bmin = b(i)
50    continue
      if(ngg.eq.nggs) print 100,ind,amin,amax,bmin,bmax
100   format(1h ,'ind amin amax bmin bmax ',i4,2x,4(e12.4,2x))
      return
      end
