      subroutine tri_diag_solver(nn,aa,bb,cc,ff,val)
!
! solve quadratic diagnal matrix
! where aa, bb, cc and dd are matrix, ff is given vector 
! and val is to be solve as following for example of rank=5
!
!  |  bb1  cc1                 || val1 |   | ff1 |
!  |  aa2  bb2  cc2            || val2 |   | ff2 |
!  |       aa3  bb3  cc3       || val3 | = | ff3 |
!  |            aa4  bb4  cc4  || val4 |   | ff4 |
!  |                 aa5  bb5  || val5 |   | ff5 |
!
!
      implicit none
!
      integer      nn
      real         aa(nn),bb(nn),cc(nn)
      real         ff(nn),val(nn)
!
      real         bs(nn),cs(nn),fs(nn)
      integer      k
!
      fs(1)=ff(1)
      bs(1)=bb(1)
      cs(1)=cc(1)
      do k=2,nn
        fs(k)=bs(k-1)*ff(k)-aa(k)*fs(k-1)
        cs(k)=bs(k-1)*cc(k)
        bs(k)=bs(k-1)*bb(k)-aa(k)*cs(k-1)
      enddo
!
      val(nn)=fs(nn)/bs(nn)
      val(nn-1)=(ff(nn)-bb(nn)*val(nn))/aa(nn)
      do k=nn-2,1,-1
        val(k)=(ff(k+1)-bb(k+1)*val(k+1)-cc(k+1)*val(k+2))/aa(k+1)
      enddo
!
      return
      end
