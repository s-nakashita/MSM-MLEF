      subroutine rmpnl2ny(a,igrd1p,jgrd1,b,igrd1,jgrd1p                  &                   
     &                    ,ntotal,kstr,klen)
!$$$  subprogram documentation block
!
! subprogram: 	rmpnl2ny
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract:   transpose (ip,kp,jf) to (if,kp,jp)
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	call rmpnl2ny(igrd1,a,igrd12p,jgrd12,b,igrd12,jgrd12p,ntotal,kstr,klen)
!
!    input argument lists:
!	a	- real (jgrd12p,ntotal,jgrd12) partial field in i k
!	igrd12p	- integer partial longitude grid x 2
!	jgrd12	- integer total latitude grid / 2
!	igrd12	- integer total longitude grid x 2
!	jgrd12p	- integer partial latitude grid / 2
!	kstr	- integer starting set of fields
!	klen	- integer partial vertical layer
!	ntotal	- integer total set of fields
!
!    output argument list:
!	b	- real (igrd12,ntotal,jgrd12p) partial field in j k
! 
! subprograms called:
!   mpi_alltoallv  - send and receive from all pe in the same comm
!
! attributes:
!    library: mpi
!    language: fortran 90
!$$$
!j      implicit none
        use cnttime
#include <define.h>
#include <npesi.h>
#include <comrmpi.h>
      integer igrd1,jgrd1p,ntotal,igrd1p,jgrd1,status                    &                
     &       ,i,j,k,jj,jpe,jpe0,n,mn,len,ierr                            &
     &       ,kstr,klen,ij,ji,llens,llensi
      real    a,b
      dimension a(igrd1p,ntotal,jgrd1),b(igrd1,ntotal,jgrd1p)
!
      real(kind=kind_mpi),allocatable::tmpsnd(:),tmprcv(:)
      integer,allocatable::lensnd(:),lenrcv(:)
      integer,allocatable::locsnd(:),locrcv(:)
!
      allocate(tmpsnd(igrd1p*jgrd1p*klen*nrow))
      allocate(tmprcv(igrd1p*jgrd1p*klen*nrow))
      allocate(lensnd(nrow))
      allocate(lenrcv(nrow))
      allocate(locsnd(nrow))
      allocate(locrcv(nrow))
!
#ifdef CNT_5
      call cnttime_on("rmpnl2ny_before")
#endif
      jpe0=mod(mype,ncol)
!     lcaps=iwav1p
! cut in y
      mn=0
      llens=lwvlen(mype)
      do jj=1,nrow
        locsnd(jj)=mn
        jpe=jpe0+(jj-1)*ncol
        do ji=latstr(jpe),latlen(jpe)+latstr(jpe)-1
          do n=kstr,kstr+klen-1
            do i=1,llens
              mn=mn+1
              tmpsnd(mn)=a(i,n,ji)
            enddo
          enddo
        enddo
        lensnd(jj)=mn-locsnd(jj)
      enddo
!
      mn=0
      do jj=1,nrow
        locrcv(jj)=mn
        jpe=jpe0+(jj-1)*ncol
!
        lenrcv(jj)=latlen(mype)*lwvlen(jpe)*klen
!
        mn=mn+lenrcv(jj)
      enddo
!
#ifdef CNT_5
      call cnttime_off("rmpnl2ny_before")
      call rmpsyncol
      call cnttime_on("rmpnl2ny_call")
#endif
#ifdef CNT_6
      print *,"rmpml2ny,mype=",                                          &                
     &     mype,"myrow=",myrow," mycol=",mycol,                          &
     &     "lensnd",lensnd, "lenrcv",lenrcv
      call cnttime_on("rmpnl2ny_call")
#endif
      call mpi_alltoallv(tmpsnd,lensnd,locsnd,MPIREAL,                   &
     &                   tmprcv,lenrcv,locrcv,MPIREAL,                   &
     &                   comm_column,ierr)
#ifdef CNT_6
      call cnttime_off("rmpnl2ny_call")
#endif
#ifdef CNT_5
      call cnttime_off("rmpnl2ny_call")
      call cnttime_on("rmpnl2ny_after")
#endif
! restore l
      mn=0
      do jj=1,nrow
        jpe=jpe0+(jj-1)*ncol
        llens=lwvlen(jpe)
        do j=1,latlen(mype)
          do n=kstr,kstr+klen-1
            do ij=1+lwvstr(jpe),llens+lwvstr(jpe)
              mn=mn+1
              b(ij,n,j)=tmprcv(mn)
            enddo
          enddo
        enddo
      enddo
#ifdef CNT_5
      call cnttime_off("rmpnl2ny_after")
#endif
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
