!c
      subroutine sgl2ful(s,f,l)
!cc
      use machine , only : kind_io4
      real(kind=kind_io4) s(l)
      dimension f(l)
      do i=1,l
        f(i)=s(i)
      enddo
      return
      end
