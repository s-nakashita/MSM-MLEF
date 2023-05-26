      subroutine equdis(ind,len,ncut,lenarr,lendef)
!$$$  subprogram documentation block
!
! subprogram: 	equdis
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract: cut len into ncut pieces with load balancing by 
!           symmetric distribution
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	equdiv(len,ncut,lenarr)
!
!    input argument lists:
!	ind	- integer spread direction: 1 for regular,
!                                          -1 for reverse
!	len	- integer total length 
!	ncut	- integer number of subgroup
!
!    output argument list:
!	lenarr	- integer (ncut) length of each subgroup
!	lendef	- integer (len) redefine the index 
! 
! subprograms called: none
!
! attributes:
!    language: fortran 90
!$$$
!
      implicit none
      integer ind,len,ncut,lenarr,lendef,nn,n,i,lens,lene
      dimension lenarr(ncut),lendef(len)
      integer, allocatable :: lentmp(:)
      allocate(lentmp(len))
!
      do i=1,ncut
        lenarr(i)=0
      enddo
      if( ind.eq.1 ) then
        lens=1
        lene=len
      else
        lens=len
        lene=1
      endif
      i=1
      n=1
      do nn=lens,lene,ind
        lenarr(n)=lenarr(n)+1
        lentmp(nn)=n
        n=n+i
        if(n.eq.ncut+1) then
          i=-1
          n=n+i
        endif
        if(n.eq.0) then
          i=1
          n=n+i
        endif
      enddo
!
      n=0
      do i=1,ncut
        do nn=1,len
          if(lentmp(nn).eq.i) then
            n=n+1
            lendef(n)=nn
          endif
        enddo
      enddo
!
      deallocate(lentmp)
!
      return
      end
