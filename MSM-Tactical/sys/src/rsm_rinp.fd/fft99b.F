      subroutine fft99b(work,a,trigs,inc,jump,n,lot)

!     subroutine fft99b - postprocessing step for fft99, isign=-1
!     (gridpoint to spectral transform)
!
      dimension work(n),a(n),trigs(n)
!
      nh=n/2
      nx=n
      ink=inc+inc
!
!     a(0)   a(n/2)
      scale=1.0e0/float(n)
      ia=1
      ib=2
      ja=1
      jb=n*inc+1
!dir$ ivdep
      do 10 l=1,lot
      a(ja)=scale*(work(ia)+work(ib))
      a(ja+inc)=0.0e0
      ia=ia+nx
      ib=ib+nx
      ja=ja+jump
      jb=jb+jump
   10 continue
!
!     remaining wavenumbers
      scale=0.5e0*scale
      iabase=3
      ibbase=n-1
      jabase=2*inc+1
      jbbase=(n-2)*inc+1
!
      do 30 k=3,nh,2
      ia=iabase
      ib=ibbase
      ja=jabase
      jb=jbbase
      c=trigs(n+k)
      s=trigs(n+k+1)
!dir$ ivdep
      do 20 l=1,lot
      a(ja)=scale*((work(ia)+work(ib))                                   &              
     &   +(c*(work(ia+1)+work(ib+1))+s*(work(ia)-work(ib))))
      a(jb)=scale*((work(ia)+work(ib))                                   &
     &   -(c*(work(ia+1)+work(ib+1))+s*(work(ia)-work(ib))))
      a(ja+inc)=scale*((c*(work(ia)-work(ib))-s*(work(ia+1)+work(ib+1))) &
     &    +(work(ib+1)-work(ia+1)))
      a(jb+inc)=scale*((c*(work(ia)-work(ib))-s*(work(ia+1)+work(ib+1))) &
     &    -(work(ib+1)-work(ia+1)))
      ia=ia+nx
      ib=ib+nx
      ja=ja+jump
      jb=jb+jump
   20 continue
      iabase=iabase+2
      ibbase=ibbase-2
      jabase=jabase+ink
      jbbase=jbbase-ink
   30 continue
!
      if (iabase.ne.ibbase) go to 50
!     wavenumber n/4 (if it exists)
      ia=iabase
      ja=jabase
      scale=2.0e0*scale
!dir$ ivdep
      do 40 l=1,lot
      a(ja)=scale*work(ia)
      a(ja+inc)=-scale*work(ia+1)
      ia=ia+nx
      ja=ja+jump
   40 continue
!
   50 continue
      return
      end
