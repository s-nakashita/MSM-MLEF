      subroutine fft99a(a,work,trigs,inc,jump,n,lot)
!
!c     subroutine fft99a - preprocessing step for fft99, isign=+1
!c     (spectral to gridpoint transform)
!c
      dimension a(n),work(n),trigs(n)
      nh=n/2
      nx=n
      ink=inc+inc
!c
!c     a(0)   a(n/2)
      ia=1
      ib=n*inc+1
      ja=1
      jb=2
!cdir$ ivdep
      do 10 l=1,lot
      work(ja)=a(ia)
      work(jb)=a(ia)
      ia=ia+jump
      ib=ib+jump
      ja=ja+nx
      jb=jb+nx
   10 continue
!c
!c     remaining wavenumbers
      iabase=2*inc+1
      ibbase=(n-2)*inc+1
      jabase=3
      jbbase=n-1
!c
      do 30 k=3,nh,2
      ia=iabase
      ib=ibbase
      ja=jabase
      jb=jbbase
      c=trigs(n+k)
      s=trigs(n+k+1)
!cdir$ ivdep
      do 20 l=1,lot
      work(ja)=(a(ia)+a(ib))-						&
     &    (s*(a(ia)-a(ib))+c*(a(ia+inc)+a(ib+inc)))
      work(jb)=(a(ia)+a(ib))+						&
     &    (s*(a(ia)-a(ib))+c*(a(ia+inc)+a(ib+inc)))
      work(ja+1)=(c*(a(ia)-a(ib))-s*(a(ia+inc)+a(ib+inc)))+		&
     &    (a(ia+inc)-a(ib+inc))
      work(jb+1)=(c*(a(ia)-a(ib))-s*(a(ia+inc)+a(ib+inc)))-		&
     &    (a(ia+inc)-a(ib+inc))
      ia=ia+jump
      ib=ib+jump
      ja=ja+nx
      jb=jb+nx
   20 continue
      iabase=iabase+ink
      ibbase=ibbase-ink
      jabase=jabase+2
      jbbase=jbbase-2
   30 continue
!c
      if (iabase.ne.ibbase) go to 50
!c     wavenumber n/4 (if it exists)
      ia=iabase
      ja=jabase
!cdir$ ivdep
      do 40 l=1,lot
      work(ja)=2.0    *a(ia)
      work(ja+1)=-2.0    *a(ia+inc)
      ia=ia+jump
      ja=ja+nx
   40 continue
!c
   50 continue
      return
      end
