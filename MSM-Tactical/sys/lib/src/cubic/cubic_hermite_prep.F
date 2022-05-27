      subroutine cubic_hermite_prep(len,loc,val,slop,bc)
!
! prepare hermite cubic spline 
! compute 1st derivative at interface
!
! len	dimension of the cell edge
! loc	location of the cell edge, loc(len)
! val	value at cell edge, val(len)
! slop  first derivative at cell edge, slop(len)
! bc	boundary condition, 0: cyclic, 1: side
!
      implicit none

      integer            len,bc
      real               loc(len),val(len),slop(len)
!
      real               slopm(0:len),delxm(0:len)
      integer            i
!
! delx
! slope at layer
      do i=1,len-1
        delxm(i)=loc(i+1)-loc(i)
        slopm(i)=(val(i+1)-val(i))/delxm(i)
      enddo
! bc=0 cyclic, other wise one side condition
      if( bc.eq.0 ) then
        delxm(0)=delxm(len-1)
        slopm(0)=slopm(len-1)
        delxm(len)=delxm(1)
        slopm(len)=slopm(1)
      else 
        delxm(0)=delxm(1)
        slopm(0)=slopm(1)
        delxm(len)=delxm(len-1)
        slopm(len)=slopm(len-1)
      endif
! slope at loc
      do i=1,len
        slop(i)=0.25*(slopm(i-1)+slopm(i))*(delxm(i-1)+delxm(i))
      enddo
!
      return
      end
