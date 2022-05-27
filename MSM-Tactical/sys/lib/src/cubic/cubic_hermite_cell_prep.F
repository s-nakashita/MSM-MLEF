      subroutine cubic_hermite_cell_prep(len,loc,valm,valu,slop,bc)
!
! prepare hermite cubic spline 
! compute first derivative and value at interface from given cell mean value
!
! len	dimension of the cell edge
! loc	location of the cell edge, loc(len)
! valm	mean value of the cell, valm(len-1)
! valu	value at cell edge, val(len)
! slop  first derivative at cell edge, slop(len)
! bc	boundary condition, 0: cyclic, 1: side
!
      implicit none

! input
      integer            len,bc
      real               loc(len),val(len),valm(len-1)
! output
      real               valu(len),slop(len)
!
      real               slopm(0:len),delxm(0:len)
      real               aa(len),bb(len),cc(len),ff(len)
      integer            i
!
! estimate slop at bottom and top
      slop(1)=valm(2)-valm(1)
      slop(len)=valm(len)-valm(len-1)
!
! prepare coefficient to do tri_diag slover
      do i=2,len-1
        aa(i)=1.0
        bb(i)=10.
        cc(i)=1.0
        ff(i)=12*(valm(i)-valm(i-1)
      enddo
      ff(2)=ff(2)-slop_bot
      ff(len-1)=ff(len-1)-slop_top
!
! get slope for all
      call tri_diag_solver(len-2,aa(2:len-1),bb(2:len-1),cc(2:len-1),
     &                     ff(2:len-1),slop(2:len-1)
!
! estimate value at bottom
      valu(1)=valm(1)
      do i=1,len-1
        valu(i+1)=2.*valm(i)-valu(i)-(slop(i)-slop(i+1))/6.
      enddo
!
      return
      end
