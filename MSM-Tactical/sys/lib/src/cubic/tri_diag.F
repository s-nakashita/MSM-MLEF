c-----------------------------------------------------------------------
      subroutine tri_diag(l,n,cl,cm,cu,r1,a1)
cc
      implicit none
      integer             k,n,l,i
      real                fk
cc
      real
     &          cl(l,n),cm(l,n),cu(l,n),r1(l,n),
     &          au(l,n),a1(l,n)
c-----------------------------------------------------------------------
      do i=1,l
        fk=1./cm(i,1)
        au(i,1)=fk*cu(i,1)
        a1(i,1)=fk*r1(i,1)
      enddo
      do k=2,n-1
        do i=1,l
          fk=1./(cm(i,k)-cl(i,k)*au(i,k-1))
          au(i,k)=fk*cu(i,k)
          a1(i,k)=fk*(r1(i,k)-cl(i,k)*a1(i,k-1))
        enddo
      enddo
      do i=1,l
        fk=1./(cm(i,n)-cl(i,n)*au(i,n-1))
        a1(i,n)=fk*(r1(i,n)-cl(i,n)*a1(i,n-1))
      enddo
      do k=n-1,1,-1
        do i=1,l
          a1(i,k)=a1(i,k)-au(i,k)*a1(i,k+1)
        enddo
      enddo
c-----------------------------------------------------------------------
      return
      end
