      subroutine maxmin(a,len,k,k1,k2,ch)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  maxmin
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  do print maximum and minimum of a given array.
!
! program history log:
!
! usage:    call  maxmin(a,len,k,k1,k2,ch)
!   input argument list:
!     a         - array for computing max and min (len,k)
!     len       - the first dimension of a
!     k         - the second dimension of a
!     k1        - lower limit of second dimension to print
!     k2        - upper limit to print
!     ch        - charcter string to print
!                 fpl and fml
!
!   output argument list:
!
!   input files: none
!
!   output files:
!     standard output
!
!   subprograms called: 
!     intrinsic functions: amax1 amin1
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$
      dimension a(len,k)
      character ch*(*)
! 
      do 100 j=k1,k2
      aamax = a(1,j)
      aamin = a(1,j)
      do 10 m=1,len
      aamax = max( aamax, a(m,j) )
      aamin = min( aamin, a(m,j) )
10    continue
!     write(0,*)ch,' has max=',aamax,' min=',aamin,' at k=',j
      print   *,ch,' has max=',aamax,' min=',aamin,' at k=',j
100   continue
      return
      end
