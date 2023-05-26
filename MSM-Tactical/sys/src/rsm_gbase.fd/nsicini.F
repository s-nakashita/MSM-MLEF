      subroutine nsicini(tor,cm,cmn,dmn,emn,fmn,                         &     
     &        gmn,hmn,dcm,wcm,x3i,x4,snnp1,dtx2)
!ccj
#include <paramodel.h>
#ifdef MP
#define LNWAVS lnwavp
#else
#define LNWAVS lnwav
#endif
#ifdef NONHYD
!cc
      dimension ddd(LNWAVS),ppp(LNWAVS),rrr(LNWAVS)
      dimension                                                          &   
     &          cm(levr,levr),tor(levr),snnp1(LNWAVS)                    &
     &         ,cmn(levr+1,levr)                                         &
     &         ,dmn(levr+1,levr)                                         &
     &         ,emn(levr  ,levr+1)                                       &
     &         ,fmn(levr  ,levr)                                         &
     &         ,gmn(levr  ,levr+1)                                       &
     &         ,hmn(levr  ,levr)                                         &
     &         ,dcm(LNWAVS,levr,levr)                                    &
     &         ,wcm(levr,levr+1)                                         &
     &         ,x3i(levr+1,levr+1)                                       &
     &         ,x4(levr+1,levr)
      common /reproni/                                                     &
     &           x1i(LNWAVS,levr,levr),x2(levr,levr+1)                   &
     &          ,tmp3(LNWAVS,levr+1,levr+1)                              &
     &          ,tmp(levr+1,levr+1)                                      &
     &          ,rim(levr+1,levr+1)
!
      dtx22 = dtx2 * dtx2
      do i=1,levr+1
        do j=1,levr+1
          rim(i,j)=0.0
        enddo
      enddo
! x1i[n k k]
      do k=1,levr
        rim(k,k)=tor(k)
      enddo
      do i=1,levr
        do j=1,levr
          tmp(i,j)=cm(i,j)
          do k=1,levr
            tmp(i,j)=tmp(i,j)-rim(i,k)*fmn(k,j)
          enddo
        enddo
      enddo
      do k=1,levr
        rim(k,k)=1.0
      enddo
      do 2000 nn=1,LNWAVS
      sn1dt2 = snnp1(nn)*dtx22
!mic$ do all
!mic$1 shared(x1i,nn,rim,sn1dt2,tmp)
!mic$1 private(j)
      do 50 i=1,levr
      do 50 j=1,levr
      x1i(nn,i,j) = rim(i,j) - sn1dt2*tmp(i,j)
 50   continue
2000  continue
!cj      print *,' before matinv for x1i'
      call matinv(x1i,LNWAVS,levr,ddd,ppp,rrr)
!cj      print *,' finish matinv for x1i'
!
! x2[k k+1]
      do k=1,levr
        rim(k,k)=tor(k)
      enddo
      do i=1,levr
        do j=1,levr+1
          x2(i,j)=0.0
          do k=1,levr
            x2(i,j)=x2(i,j)+rim(i,k)*emn(k,j)
          enddo
        enddo
      enddo
!cj      print *,'finish x2'
!
! x3i[k+1 k+1]
      do k=1,levr+1
        rim(k,k)=1.0
      enddo
      do i=1,levr+1
        do j=1,levr+1
          x3i(i,j)=rim(i,j)
          do k=1,levr
            x3i(i,j)=x3i(i,j)-dtx22*(cmn(i,k)*gmn(k,j)+dmn(i,k)*emn(k,j))
          enddo
        enddo
      enddo
!cj      print *,' before matinv x3i '
      call matinv(x3i,1,levr+1,ddd,ppp,rrr)
!cj      print *,' after matinv x3i '
!cj      call prtmtx(x3i,levr+1,levr+1,'x3i') 
!
! x4[k+1 k]
      do i=1,levr+1
        do j=1,levr
          x4(i,j)=0.0
          do k=1,levr
            x4(i,j)=x4(i,j)+dtx22*(cmn(i,k)*hmn(k,j)+dmn(i,k)*fmn(k,j))
          enddo
        enddo
      enddo
!cj      print *,' finish x4'
!cj      call prtmtx(x4,levr+1,levr,'x4') 
! --------------------------------------------------------------------------------
! dcm[n,k,k]=( 1+x1i[n k k] x2[n k k+1] x3i[k+1 k+1] x4[k+1 k] )i[n k k] x1i[n k k]
      do i=1,levr+1
        do j=1,levr
          tmp(i,j)=0.0
          do k=1,levr+1
            tmp(i,j)=tmp(i,j)+x3i(i,k)*x4(k,j)
          enddo
        enddo
      enddo
! tmp[k+1 k]
      do n=1,LNWAVS
      do i=1,levr
        do j=1,levr
          tmp3(n,i,j)=0.0
          do k=1,levr+1
            tmp3(n,i,j)=tmp3(n,i,j)+dtx22*snnp1(n)*x2(i,k)*tmp(k,j)
          enddo
        enddo
      enddo
      enddo
! tmp3[n k k]
      do k=1,levr
        rim(k,k)=1.0
      enddo
      do n=1,LNWAVS
      do i=1,levr
        do j=1,levr
          dcm(n,i,j)=rim(i,j)
          do k=1,levr
            dcm(n,i,j)=dcm(n,i,j)+x1i(n,i,k)*tmp3(n,k,j)
          enddo
        enddo
      enddo
      enddo
      call matinv(dcm,LNWAVS,levr,ddd,ppp,rrr)
! dcm[n k k]
      do n=1,LNWAVS
      do i=1,levr
        do j=1,levr
          tmp3(n,i,j)=0.0
          do k=1,levr
            tmp3(n,i,j)=tmp3(n,i,j)+dcm(n,i,k)*x1i(n,k,j)
          enddo
        enddo
      enddo
      enddo
      do n=1,LNWAVS
      do i=1,levr
        do j=1,levr
          dcm(n,i,j)=tmp3(n,i,j)
        enddo
      enddo
      enddo
!cj      print *,' finish dcm '
!--------------------------------------------------------------------
! wcm[k k+1] = x2[k k+1] x3i[k+1 k+1]
      do i=1,levr
        do j=1,levr+1
          wcm(i,j)=0.0
          do k=1,levr+1
            wcm(i,j)=wcm(i,j)+x2(i,k)*x3i(k,j)
          enddo
        enddo
      enddo
!cj      print *,' finish wcm '
!
#endif
      return
      end
