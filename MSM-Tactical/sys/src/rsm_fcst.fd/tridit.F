      subroutine tridit(l,n,nt,cl,cm,cu,rt,au,at)
!sela %include dbtridi2;
!c
      use machine     , only : kind_phys
!     use machine_rad , only : kind_phys
      implicit none
      integer             is,k,kk,n,nt,l,i
      real(kind=kind_phys) fk(l)
!c
      real(kind=kind_phys) cl(l,2:n), cm(l,n), cu(l,n-1),               &
     &                     rt(l,n*nt),                                  &
     &                     au(l,n-1), at(l,n*nt),                       &
     &                     fkk(l,2:n-1)
!-----------------------------------------------------------------------
      do i=1,l
        fk(i)   = 1./cm(i,1)
        au(i,1) = fk(i)*cu(i,1)
      enddo
      do k = 1, nt
        is = (k-1) * n
        do i = 1, l
          at(i,1+is) = fk(i) * rt(i,1+is)
        enddo
      enddo
      do k=2,n-1
        do i=1,l
          fkk(i,k) = 1./(cm(i,k)-cl(i,k)*au(i,k-1))
          au(i,k)  = fkk(i,k)*cu(i,k)
        enddo
      enddo
      do kk = 1, nt
        is = (kk-1) * n
        do k=2,n-1
          do i=1,l
            at(i,k+is) = fkk(i,k)*(rt(i,k+is)-cl(i,k)*at(i,k+is-1))
          enddo
        enddo
      enddo
      do i=1,l
        fk(i)   = 1./(cm(i,n)-cl(i,n)*au(i,n-1))
      enddo
      do k = 1, nt
        is = (k-1) * n
        do i = 1, l
          at(i,n+is) = fk(i)*(rt(i,n+is)-cl(i,n)*at(i,n+is-1))
        enddo
      enddo
      do kk = 1, nt
        is = (kk-1) * n
        do k=n-1,1,-1
          do i=1,l
            at(i,k+is) = at(i,k+is) - au(i,k)*at(i,k+is+1)
          enddo
        enddo
      enddo
!-----------------------------------------------------------------------
      return
      end
