      module comrfft
!
      implicit none
 
      integer, parameter :: lotmin=64,lotmax=64,ncpu=1
c
      real, allocatable ::  work(:,:,:)
      real, allocatable ::  trigs(:,:)
      real, allocatable ::  al(:,:)
      
      integer, allocatable ::  ifax(:)


      contains

      subroutine rfftini(im,iw)
c
        integer	im,iw
        integer	igrd1,igrd2,iwav1,jump

        igrd1=im
        iwav1=iw
        igrd2 = (igrd1-1)*2
        jump = igrd2 + 3
c
        allocate(trigs(igrd2,2))
        allocate(ifax(20))
        allocate(al(jump,lotmax))
        allocate(work(igrd2,lotmax,2))
c
        call    fax (ifax, igrd2,3)
        call fftrig (trigs,igrd2,3)
c
120     format (' error in rfftinni.  igrd2 not factorable. ')
140     format (' fftfax is  called in rfftini. igrd2 = ',i10)
c
        return
      end subroutine rfftini

      end module comrfft
