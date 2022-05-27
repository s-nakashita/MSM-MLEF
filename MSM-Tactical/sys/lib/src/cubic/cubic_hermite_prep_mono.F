      subroutine cubic_hermite_prep_mono(len,loc,val,slop,bc)
!
! prepare hermite cubic spline 
! compute 1st derivative at interface, consider monotonic
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
      real               alpha,betta,tau,cc,c1,c2,c3
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
! modify slopi to have monotonic
      do i=1,len-1
        if( slopm(i).eq.0.0 ) then
          slop(i  ) = 0.0
          slop(i+1) = 0.0
        else
          slopm(i)=slopm(i)*delxm(i)
          alpha = slop(i  )/slopm(i)
          betta = slop(i+1)/slopm(i)
          cc = alpha + betta - 2.0
          if( cc.gt.0.0 ) then
            c1 = 2.*alpha + betta - 3.0
            c2 = alpha + 2.*betta - 3.0
            c3 = alpha - c1*c1/cc/3.0
            if( c1.gt.0.0 .and. c2.gt.0.0 .and. c3.lt.0.0 ) then
              tau = 3.0/sqrt(alpha*alpha+betta*betta)
              slop(i)   = tau * alpha * slopm(i)
              slop(i+1) = tau * betta * slopm(i)
            endif
          endif
        endif
      enddo
!
      return
      end
