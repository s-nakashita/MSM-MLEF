      subroutine rmpnx2nk(a,igrd1p,lota,b,igrd1,lotb,jgrd1p,             &                      
     &             levs,levsp,leva,levb,nvar)
!$$$  subprogram documentation block
!
! subprogram:  rmpnx2nk
!
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract:  transpose (ip,kf) to (if,kp)
!
! program history log:
!    99-06-27  henry juang      finish entire test for gsm
!
! usage:    call rmpnx2nk(a,igrd12p,lota,b,igrd12,lotb,jgrd12p,levs,levsp,
!          &                   leva,levb,nvar)
!
!    input argument lists:
!       a       - real (igrd12p,lota,jgrd12p) partial field in j k
!       igrd12   - integer total longitude grid x 2
!       lota    - integer total field of nvar*levsp
!       igrd12p  - integer partial longitude grid x 2
!       lotb    - integer total field of nvar*levs
!       jgrd12p  - integer partial latitude grid / 2
!       levsp   - integer partial vertical grid
!       levs    - integer total vertical grid
!       leva    - integer start vertical grid for a
!       levb    - integer start vertical grid for b
!       nvar    - integer total set of fields
!
!    output argument list:
!       b       - real (igrd12,lotb,jgrd12p) partial field in i j
!
! subprograms called:
!   mpi_alltoallv  - send and receive from all pe in the same comm
!
! attributes:
!    library: mpi
!    language: fortran 90
!$$$
      implicit none
#include <npesi.h>
#include <comrmpi.h>
      integer igrd1,levsp,jgrd1p,igrd1p,levs,nvar                        &                
     &       ,status,lota,lotb,leva,levb,ioff,koff                       &
     &       ,i,j,k,ii,ipe,ipe0,n,mn,len,kp,kf,ierr                      &
     &       ,lonf2d,lonfp,latlon,ioffi
      real    a,b
      dimension a(igrd1p,lota,jgrd1p),b(igrd1,lotb,jgrd1p)
!
      real(kind=kind_mpi),allocatable::tmpsnd(:),tmprcv(:)
      integer,allocatable::lensnd(:),lenrcv(:)
      integer,allocatable::locsnd(:),locrcv(:)
!
! option for 1-d decomposition
      if( ncol.eq.1 ) then
        do j=1,jgrd1p
          do k=1,lota
            do i=1,igrd1
              b(i,k,j) = a(i,k,j)
            enddo
          enddo
        enddo
        return
      endif
!
! otherwise
      allocate(tmpsnd(igrd1p*jgrd1p*levsp*nvar*ncol))
      allocate(tmprcv(igrd1p*jgrd1p*levsp*nvar*ncol))
      allocate(lensnd(ncol))
      allocate(lenrcv(ncol))
      allocate(locsnd(ncol))
      allocate(locrcv(ncol))
!
!
      ipe0=int(mype/ncol)*ncol
! cut in k
      mn=0
      lonfp=lonlen(mype)
      do ii=1,ncol
        locsnd(ii)=mn
        ipe=(ii-1)+ipe0
        koff=levstr(ipe)-1
        do n=1,nvar
          kf=(n-1)*levs+leva-1+koff
          do j=1,latlen(mype)
            do k=1+kf,levlen(ipe)+kf
              do i=1,lonfp
                mn=mn+1
                tmpsnd(mn) = a(i,k,j)
              enddo
            enddo
          enddo
        enddo
        lensnd(ii)=mn-locsnd(ii)
      enddo
!
      mn=0
      do ii=1,ncol
        locrcv(ii)=mn
        ipe=(ii-1)+ipe0
!
        latlon=latlen(mype)*lonlen(ipe)
!
        lenrcv(ii)=levlen(mype)*latlon*nvar
        mn=mn+lenrcv(ii)
      enddo
!
#ifdef CNT_5
      call rmpsynrow
      call cnttime_on("rmpnx2nk")
#endif
      call mpi_alltoallv(tmpsnd,lensnd,locsnd,MPIREAL,                   &                    
     &                   tmprcv,lenrcv,locrcv,MPIREAL,                   &
     &                   comm_row,ierr)
#ifdef CNT_5
      call cnttime_off("rmpnx2nk")
#endif
! put to x
      mn=0
      do ii=1,ncol
        ipe=(ii-1)+ipe0
        lonfp=lonlen(ipe)
        ioff=lonstr(ipe)-1
        do n=1,nvar
          kp=(n-1)*levsp+levb-1
          do j=1,latlen(mype)
            do k=1+kp,levlen(mype)+kp
              do i=1+ioff,lonfp+ioff
                mn=mn+1
                b(i,k,j) = tmprcv(mn)
              enddo
            enddo
          enddo
        enddo
      enddo
!
      deallocate(tmpsnd)
      deallocate(tmprcv)
      deallocate(lensnd)
      deallocate(lenrcv)
      deallocate(locsnd)
      deallocate(locrcv)
!
      return
      end
