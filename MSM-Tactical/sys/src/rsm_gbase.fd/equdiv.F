      subroutine equdiv(len,ncut,lenarr)
!$$$  subprogram documentation block
!
! subprogram: 	equdiv
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract: cut len into ncut pieces with load balancing
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	equdiv(len,ncut,lenarr)
!
!    input argument lists:
!	len	- integer total length 
!	ncut	- integer number of subgroup
!
!    output argument list:
!	lenarr	- integer (ncut) length of each subgroup
! 
! subprograms called: none
!
! attributes:
!    language: fortran 90
!$$$
!
      implicit none
      integer len,ncut,lenarr,n0,n1,n
      dimension lenarr(ncut)
      n0=len/ncut
      n1=mod(len,ncut)
      do n=1,n1
        lenarr(n)=n0+1
      enddo
      do n=n1+1,ncut
        lenarr(n)=n0
      enddo
      return
      end
