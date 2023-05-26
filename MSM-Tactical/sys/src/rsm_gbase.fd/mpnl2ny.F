      subroutine mpnl2ny(jcapp,lonfx,a,lonfp,latg,b,lonf,latgp,ntotal    &
     &                   ,kstr,klen,lwvdef,latdef,jcapx,latgx)
!$$$  subprogram documentation block
!
! subprogram: 	mpnl2ny
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract:   transpose (ip,kp,jf) to (if,kp,jp)
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	call mpnl2ny(a,lonfp,latg,b,lonf,latgp,ntotal,kstr,klen)
!
!    input argument lists:
!	a	- real (lonfp,ntotal,latg) partial field in i k
!	lonfp	- integer partial longitude grid x 2
!	latg	- integer total latitude grid / 2
!	lonf	- integer total longitude grid x 2
!	latgp	- integer partial latitude grid / 2
!	kstr	- integer starting set of fields
!	klen	- integer partial vertical layer
!	ntotal	- integer total set of fields
!
!    output argument list:
!	b	- real (lonf,ntotal,latgp) partial field in j k
! 
! subprograms called:
!   mpi_alltoallv  - send and receive from all pe in the same comm
!
! attributes:
!    library: mpi
!    language: fortran 90
!$$$
#include <define.h>
#ifdef MP
      implicit none
#include <npesi.h>
#include <comrmpi.h>
#include <commpi.h>
      integer jcapp,lonfx,lonf,latgp,ntotal,lonfp,latg,status,lcaps      &                        
     &       ,i,i1,i2,j1,j,k,jj,jpe,jpe0,n,mn,len,ierr                   &
     &       ,kstr,klen,ij,ji,llens
      real    a,b
      dimension a(lonfp,ntotal,latg),b(lonf,ntotal,latgp)
      integer   lwvdef,latdef,jcapx,latgx
      dimension lwvdef(jcapx+1)  ,latdef(latgx/2)
!
      real(kind=kind_mpi),allocatable::tmpsnd(:),tmprcv(:)
      integer,allocatable::lensnd(:),lenrcv(:)
      integer,allocatable::locsnd(:),locrcv(:)
!
      allocate(tmpsnd(lonfp*latgp*klen*nrow))
      allocate(tmprcv(lonfp*latgp*klen*nrow))
      allocate(lensnd(nrow))
      allocate(lenrcv(nrow))
      allocate(locsnd(nrow))
      allocate(locrcv(nrow))
!
      jpe0=mod(mype,ncol)
      lcaps=(jcapp+1)*2
! cut in y
      mn=0
      llens=glwvlen(mype)*2
      do jj=1,nrow
        locsnd(jj)=mn
        jpe=jpe0+(jj-1)*ncol
        do j=1,glatlen(jpe)
          j1=j+glatstr(jpe)-1
          ji=latdef(j1)
          do n=kstr,kstr+klen-1
            do i=1,llens
              mn=mn+1
              tmpsnd(mn)=a(i      ,n,ji)
              mn=mn+1
              tmpsnd(mn)=a(i+lcaps,n,ji)
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
        lenrcv(jj)=glatlen(mype)*glwvlen(jpe)*4*klen
        mn=mn+lenrcv(jj)
      enddo
!
      call mpi_alltoallv(tmpsnd,lensnd,locsnd,MPIREAL,                   &                        
     &                   tmprcv,lenrcv,locrcv,MPIREAL,                   &
     &                   comm_column,ierr)
! restore l
      mn=0
      do jj=1,nrow
        jpe=jpe0+(jj-1)*ncol
        llens=glwvlen(jpe)
        i1=glwvstr(jpe)
        do j=1,glatlen(mype)
          do n=kstr,kstr+klen-1
            do i=1,llens
              ij=lwvdef(i+i1)*2
              mn=mn+1
              b(ij+1      ,n,j)=tmprcv(mn)
              mn=mn+1
              b(ij+1+lonfx,n,j)=tmprcv(mn)
              mn=mn+1
              b(ij+2      ,n,j)=tmprcv(mn)
              mn=mn+1
              b(ij+2+lonfx,n,j)=tmprcv(mn)
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
#endif
      return
      end
