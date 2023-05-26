      subroutine fft99m(a,work,trigs,ifax,inc,jump,n,lot,isign)
!
      dimension a(n),work(n),trigs(n),ifax(1)
!c
      nfax=ifax(1)
      nx=n
      nh=n/2
      ink=inc+inc
      if (isign.eq.+1) go to 30
!c
!c     if necessary, transfer data to work area
      igo=50
      if (mod(nfax,2).eq.1) goto 40
      ibase=1
      jbase=1
      do 20 l=1,lot
      i=ibase
      j=jbase
!cdir$ ivdep
      do 10 m=1,n
      work(j)=a(i)
      i=i+inc
      j=j+1
   10 continue
      ibase=ibase+jump
      jbase=jbase+nx
   20 continue
!c
      igo=60
      go to 40
!c
!c     preprocessing (isign=+1)
!c     ------------------------
!c
   30 continue
      call fft99a(a,work,trigs,inc,jump,n,lot)
      igo=60
!c
!c     complex transform
!c     -----------------
!c
   40 continue
      ia=1
      la=1
      do 80 k=1,nfax
      if (igo.eq.60) go to 60
   50 continue
      call vpassm(a(ia),a(ia+inc),work(1),work(2),trigs,		&
     &   ink,2,jump,nx,lot,nh,ifax(k+1),la)
      igo=60
      go to 70
   60 continue
      call vpassm(work(1),work(2),a(ia),a(ia+inc),trigs,		&
     &    2,ink,nx,jump,lot,nh,ifax(k+1),la)
      igo=50
   70 continue
      la=la*ifax(k+1)
   80 continue
!c
      if (isign.eq.-1) go to 130
!c
!c     if necessary, transfer data from work area
      if (mod(nfax,2).eq.1) go to 110
      ibase=1
      jbase=1
      do 100 l=1,lot
      i=ibase
      j=jbase
!cdir$ ivdep
      do 90 m=1,n
      a(j)=work(i)
      i=i+1
      j=j+inc
   90 continue
      ibase=ibase+nx
      jbase=jbase+jump
  100 continue
!c
!c     fill in zeros at end
  110 continue
      go to 140
!c
!c     postprocessing (isign=-1):
!c     --------------------------
!c
  130 continue
      call fft99b(work,a,trigs,inc,jump,n,lot)
!c
  140 continue
      return
      end
