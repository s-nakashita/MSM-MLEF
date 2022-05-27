!c
      subroutine ful2sgl(f,s,l)
!cc
      use machine , only : kind_io4
      real(kind=kind_io4) s(l)
      dimension f(l)
      do i=1,l
        s(i)=f(i)
      enddo
      return
      end
