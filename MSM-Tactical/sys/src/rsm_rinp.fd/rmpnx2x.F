      subroutine rmpnx2x(a,igrd1p,lota,b,igrd1,lotb,jgrd1p,              &                          
     &            leva,levb,nvar)
!$$$  subprogram documentation block
!
! subprogram:  rmpnx2x
!
! prgmmr:  yifeng cui modified     org:sdsc     date:02-07-05
!
! abstract:  transpose (ip,k,kf) to (if,k,kp)
!
! program history log:
!    99-06-27  henry juang      finish entire test for gsm
!
! usage:    call rmpnx2x(a,igrd12p,lota,b,igrd12,lotb,jgrd12p,
!    &                 leva,levb,nvar)
!
!    input argument lists:
!       a       - real (igrd12p,lota,jgrd12p) partial field in j k
!       igrd12   - integer total longitude grid x 2
!       lota    - integer total field of nvar
!       igrd12p  - integer partial longitude grid x 2
!       lotb    - integer total field of nvar
!       jgrd12p  - integer partial latitude grid / 2
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
      integer igrd1,jgrd1p,igrd1p,nvar,status                            &                          
     &       ,lota,lotb,leva,levb,ioff,noffa,noffb                       &
     &       ,i,j,k,ii,ipe,ipe0,n,mn,len,ierr                            &
     &       ,lonf2d,lonff,lonfp,latlon,ioffi
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
      allocate(tmpsnd(igrd1p*jgrd1p*nvar*ncol))
      allocate(tmprcv(igrd1p*jgrd1p*nvar*ncol))
      allocate(lensnd(ncol))
      allocate(lenrcv(ncol))
      allocate(locsnd(ncol))
      allocate(locrcv(ncol))
!
!
      lonfp=lonlen(mype)
      ipe0=int(mype/ncol)*ncol
! cut in k
      mn=0
      do ii=1,ncol
        locsnd(ii)=mn
        do j=1,latlen(mype)
          do noffa=leva,nvar+leva-1
            do i=1,lonfp
              mn=mn+1
              tmpsnd(mn) = a(i,noffa,j)
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
        lenrcv(ii)=latlon*nvar
        mn=mn+lenrcv(ii)
      enddo
!
      call mpi_alltoallv(tmpsnd,lensnd,locsnd,MPIREAL,                   &                        
     &                   tmprcv,lenrcv,locrcv,MPIREAL,                   &
     &                   comm_row,ierr)
! put to x
      mn=0
      do ii=1,ncol
        ipe=(ii-1)+ipe0
        ioff=lonstr(ipe)-1
        do j=1,latlen(mype)
          do noffb=levb,nvar-1+levb
            do i=1+ioff,lonlen(ipe)+ioff
              mn=mn+1
              b(i,noffb,j) = tmprcv(mn)
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
