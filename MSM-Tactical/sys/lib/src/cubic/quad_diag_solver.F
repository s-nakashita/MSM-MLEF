      subroutine quad_diag_solver(nn,aa,bb,cc,dd,ff,val)
!
! solve quadratic diagnal matrix
! where aa, bb, cc and dd are matrix, ff is given vector 
! and val is to be solve as following for example of rank=5
!
!  |  cc1  dd1                 || val1 |   | ff1 |
!  |  bb2  cc2  dd2            || val2 |   | ff2 |
!  |  aa3  bb3  cc3  dd3       || val3 | = | ff3 |
!  |       aa4  bb4  cc4  dd4  || val4 |   | ff4 |
!  |            aa5  bb5  cc5  || val5 |   | ff5 |
!
! in this case, aa1=bb1=0, aa2=0, dd5=0
!
      implicit none
!
      integer      nn
      real         aa(nn),bb(nn),cc(nn),dd(nn)
      real         ff(nn),val(nn)
!
      real         as(nn),bs(nn),cs(nn),ds(nn),fs(nn)
      integer      k
!
      fs(nn)=ff(nn)
      cs(nn)=cc(nn)
      bs(nn)=bb(nn)
      as(nn)=aa(nn)
      do k=nn-1,1,-1
        fs(k)=cs(k+1)*ff(k)-dd(k)*fs(k+1)
        cs(k)=cs(k+1)*cc(k)-dd(k)*bs(k+1)
        bs(k)=cs(k+1)*bb(k)-dd(k)*as(k+1)
        as(k)=cs(k+1)*aa(k)
      enddo
!
      val(1)=fs(1)/cs(1)
      val(2)=(ff(1)-cc(1)*val(1))/dd(1)
      val(3)=(ff(2)-cc(2)*val(2)-bb(2)*val(1))/dd(2)
      do k=4,nn
        val(k)=(ff(k-1)-cc(k-1)*val(k-1)
     &                -bb(k-1)*val(k-2)-aa(k-1)*val(k-3))/dd(k-1)
      enddo
!
      return
      end
