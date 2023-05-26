      module module_radsw_parameters
      implicit none
      save
      integer nbd,nvb,nrb,nk0,nae,nsrc,ndm,imxae,jmxae,nxc,ndn
! - for 8 uv bands sw
      parameter (nvb=8,nrb=4,nbd=nvb+nrb,nk0=10,nsrc=5)
! - for 4 uv bands sw
!     parameter (nvb=4,nrb=4,nbd=nvb+nrb,nk0=10,nsrc=5)
! - for opac aerosol data set
      parameter (nxc=5, ndn=2, imxae=72, jmxae=37, nae=7, ndm=5)
      end module module_radsw_parameters
