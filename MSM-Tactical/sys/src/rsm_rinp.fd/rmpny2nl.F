      subroutine rmpny2nl(a,igrd1,jgrd1p,b,igrd1p,jgrd1,                 &                      
     &                   ntotal,kstr,klen)
!$$$  subprogram documentation block
!
! subprogram:   rmpny2nl
!
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract: transpose (if,kp,jp) to (ip,kp,jf)
!
! program history log:
!    99-06-27  henry juang      finish entire test for gsm
!
! usage:        call rmpny2nl(igrd1,a,igrd12,jgrd12p,b,igrd12p,jgrd12,ntotal,kstr,klen)
!
!    input argument lists:
!       a       - real (igrd12,ntotal,jgrd12p) partial field in j k
!       igrd12p   - integer partial longitude grid x 2
!       jgrd12    - integer total latitude grid / 2
!       igrd12    - integer total longitude grid x 2
!       jgrd12p   - integer partial latitude grid / 2
!       kstr    - integer starting set of fields
!       klen    - integer partial vertical layer
!       ntotal  - integer total set of fields
!
!    output argument list:
!       b       - real (igrd12p,ntotal,jgrd12) partial field in i k
!
! subprograms called:
!   mpi_alltoallv  - send and receive from all pe in the same comm
!
! attributes:
!    library: mpi
!    language: fortran 90
!$$$
!      implicit none
       use cnttime
#include <define.h>
#include <npesi.h>
#include <comrmpi.h>
      integer igrd1,jgrd1p,igrd1p,jgrd1,ntotal,status                    &                         
     &       ,lcaps,i,j,jj,jpe,jpe0,n,mn,len,ierr                        &
     &       ,kstr,klen,ij,ji,llens,llensi
      real    a,b
      dimension a(igrd1,ntotal,jgrd1p),b(igrd1p,ntotal,jgrd1)
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
      call cnttime_on("rmpny2nl_before")
#endif
      jpe0=mod(mype,ncol)
! cut in l
      mn=0
      do jj=1,nrow
        locsnd(jj)=mn
        jpe=jpe0+(jj-1)*ncol
        llens=lwvlen(jpe)
        do j=1,latlen(mype)
          do n=kstr,kstr+klen-1
            do ij=1+lwvstr(jpe),llens+lwvstr(jpe)
              mn=mn+1
              tmpsnd(mn)=a(ij,n,j)
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
        lenrcv(jj)=latlen(jpe)*lwvlen(mype)*klen
!
        mn=mn+lenrcv(jj)
      enddo
!      if ( mype.eq.0) then
!       print *,'rmpny2nl,pe=0,tmpsnd(1-llens)=',(tmpsnd(i),i=1,llens),
!     1  (lensnd(j),j=1,nrow),'klen=',klen,'latlen=',latlen(mype),
!     1   'llens=',lwvlen(jpe),(lenrcv(j),j=1,nrow)
!      endif
!
#ifdef CNT_5
      call cnttime_off("rmpny2nl_before")
      call rmpsyncol
      call cnttime_on("rmpny2nl_call")
#endif
#ifdef CNT_6
      print *,"rmpmy2nl,mype=",                                          &                     
     &     mype,"myrow=",myrow," mycol=",mycol,                          &
     &     "lensnd",lensnd, "lenrcv",lenrcv
      call cnttime_on("rmpny2nl_call")
#endif
      call mpi_alltoallv(tmpsnd,lensnd,locsnd,MPIREAL,                   &
     &                   tmprcv,lenrcv,locrcv,MPIREAL,                   &
     &                   comm_column,ierr)
#ifdef CNT_6
      call cnttime_off("rmpny2nl_call")
#endif
#ifdef CNT_5
      call cnttime_off("rmpny2nl_call")
      call cnttime_on("rmpny2nl_after")
#endif
!
! put to y
  
      mn=0
      llens=lwvlen(mype)
!      if (mype.eq.0) print *,mype,'llens=',llens,latstr(jpe),
!     1    latlen(jpe)+latstr(jpe)-1,(tmprcv(i),i=1,llens*klen*2,2)
      do jj=1,nrow
        jpe=jpe0+(jj-1)*ncol
        ljun=mn
        do ji=latstr(jpe),latlen(jpe)+latstr(jpe)-1
          do n=kstr,kstr+klen-1
            do i=1,llens
              mn=mn+1
              b(i,n,ji)=tmprcv(mn)
            enddo
          enddo
        enddo

      enddo
#ifdef CNT_5
      call cnttime_off("rmpny2nl_after")
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
