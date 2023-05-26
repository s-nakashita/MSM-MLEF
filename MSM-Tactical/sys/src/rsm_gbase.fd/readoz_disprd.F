!
!**********************************************************************
!
      subroutine readoz_disprd(ozprdin,ozdisin,poz)
!
      use machine , only : kind_io4,kind_io8
!!
      integer lats18,lev46,n,k,kk,kozp,kozl,i
      parameter (lats18=18,lev46=46)
      real (kind=kind_io8) ozprdin(lats18,lev46,36),                     &  
     &                     ozdisin(lats18,lev46,36)
      real (kind=kind_io8) poz(lev46),dp
      real(kind=kind_io4), allocatable :: temp1in(:,:)
      real(kind=kind_io4), allocatable :: temp2in(:,:)
!
      allocate (temp1in(lats18,lev46))
      allocate (temp2in(lats18,lev46))
!
      dp = 0.2844
      do n=1,lev46
        poz(n) = 101.30 * exp(-dp*(float(n)-0.5))
        poz(n) = log(poz(n))
      enddo
!
      kozp=28
      kozl=29
      rewind(kozp)
      rewind(kozl)
      do i=1,36  
        read(kozp)temp1in
        read(kozl)temp2in
        do k=1,lev46
        do kk=1,lats18
          ozprdin(kk,k,i)=temp1in(kk,k)
          ozdisin(kk,k,i)=temp2in(kk,k)
        enddo
        enddo
      enddo
      deallocate (temp1in)
      deallocate (temp2in)
!
      return
      end
