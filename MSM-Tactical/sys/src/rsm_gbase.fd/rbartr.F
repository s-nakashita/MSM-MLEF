      subroutine rbartr(lnwav,f,g,fgbar)
!
      dimension f(lnwav),g(lnwav)
!
      fgbar=0.e0
      do 410 l=1,lnwav
      fgbar=fgbar+f(l)*g(l)
  410 continue
      fgbar=sqrt(fgbar)*0.25
!.....
      return
      end
