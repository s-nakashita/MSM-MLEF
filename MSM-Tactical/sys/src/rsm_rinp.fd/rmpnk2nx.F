      subroutine rmpnk2nx(a,igrd1,lota,
     &                   b,igrd1p,lotb,jgrd1p,levsp,levs,
     &                   leva,levb,nvar)
!$$$  subprogram documentation block
!
! subprogram: 	rmpnk2nx
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract:  transpose (if,kp) to (ip,kf)
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	call rmpnk2nx(a,igrd12,lota,
!        &                   b,igrd12p,lotb,jgrd12p,levsp,levs,
!        &                   leva,levb,nvar)
!
!    input argument lists:
!	a	- real (igrd12,lota,jgrd12p) partial field in j k
!	igrd12	- integer total longitude grid x 2
!	lota	- integer total field of nvar*levsp
!	igrd12p	- integer partial longitude grid x 2
!	lotb	- integer total field of nvar*levs
!	jgrd12p	- integer partial latitude grid / 2
!	levsp	- integer partial vertical grid 
!	levs	- integer total vertical grid 
!	leva	- integer start vertical grid for a
!	levb	- integer start vertical grid for b
!	nvar	- integer total set of fields
!
!    output argument list:
!	b	- real (igrd12p,lotb,jgrd12p) partial field in i j
! 
! subprograms called:
!   mpi_alltoallv  - send and receive from all pe in the same comm
!
! attributes:
!    library: mpi
!    language: fortran 90
!$$$
      use cnttime
      implicit none
#include <define.h>
#include <npesi.h>
#include <comrmpi.h>
      integer igrd1,levsp,jgrd1p,igrd1p,levs,nvar                        &                            
     &       ,status,lota,lotb,leva,levb,ioff,koff,lonf2d                &
     &       ,i,j,k,ii,ipe,ipe0,n,m,mn,len,kp,kf,ierr                    &
     &       ,latlon,lonfp,ioffi
      real    a,b
      dimension a(igrd1,lota,jgrd1p),b(igrd1p,lotb,jgrd1p)
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
#ifdef CNT_5
      call cnttime_on("rmpnk2nx_before")
#endif
      ipe0=int(mype/ncol)*ncol
!      print*,'ipe0=',ipe0
! cut in x
      mn=0
      do ii=1,ncol
        locsnd(ii)=mn
        ipe=(ii-1)+ipe0
        lonfp=lonlen(ipe)
        ioff=lonstr(ipe)-1
        do n=1,nvar
          kp=(n-1)*levsp+leva-1
          do j=1,latlen(mype)
            do k=1+kp,levlen(mype)+kp
              do i=1+ioff,lonfp+ioff
                mn=mn+1
                tmpsnd(mn) = a(i,k,j)
              enddo
            enddo
          enddo
        enddo
        lensnd(ii)=mn-locsnd(ii)
      enddo
!
      latlon=latlen(mype)*lonlen(mype)
!
      mn=0
      do ii=1,ncol
        locrcv(ii)=mn
        ipe=(ii-1)+ipe0
        lenrcv(ii)=levlen(ipe)*latlon*nvar
        mn=mn+lenrcv(ii)
      enddo
!
#ifdef CNT_5
      call cnttime_off("rmpnk2nx_before")
      call rmpsynrow
      call cnttime_on("rmpnk2nx_call")
#endif
#ifdef CNT_6
      print *,"rmpnk2nx, mype=",                                         &           
     &     mype,"myrow=",myrow," mycol=",mycol,                          &
     &     "lensnd",lensnd, "lenrcv",lenrcv
      call cnttime_on("rmpnk2nx_call")
#endif
      call mpi_alltoallv(tmpsnd,lensnd,locsnd,MPIREAL,                   &
     &                   tmprcv,lenrcv,locrcv,MPIREAL,                   &
     &                   comm_row,ierr)
#ifdef CNT_6
      call cnttime_off("rmpnk2nx_call")
#endif
#ifdef CNT_5
      call cnttime_off("rmpnk2nx_call")
      call cnttime_on("rmpnk2nx_after")
#endif
! restore to k
      mn=0
      lonfp=lonlen(mype)
      do ii=1,ncol
        ipe=(ii-1)+ipe0
        koff=levstr(ipe)-1
        do n=1,nvar
          kf=(n-1)*levs+levb-1+koff
          do j=1,latlen(mype)
            do k=1+kf,levlen(ipe)+kf
              do i=1,lonfp
                mn=mn+1
                b(i,k,j)=tmprcv(mn)
              enddo
            enddo
          enddo
        enddo
      enddo
!
#ifdef CNT_5
      call cnttime_off("rmpnk2nx_after")
#endif
      deallocate(tmpsnd)
      deallocate(tmprcv)
      deallocate(lensnd)
      deallocate(lenrcv)
      deallocate(locsnd)
      deallocate(locrcv)
!
      return
      end
