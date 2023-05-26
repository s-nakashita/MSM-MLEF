      subroutine matinv(a,m,n,d,p,r)
      dimension a(m,n,n),d(m),p(m),r(m)
!-----------------------------------------------------------------------
! viminv does multple matrix inversions.
!
! input : a(i,*,*) contains the i-th  n x n  matrix.
!         m is the total number of matrices to be inverted.
!         n is the dimensionality of each matrix.
!         d(*),p(*) and r(*) are arrays of length n.
!
! output: a(i,*,*) is replaced by its inverse.
!         m and n are unmodified.
!         d(i) contains the determinant of the i-th matrix.
!         p(*) and r(*) contain undefined results.
!-----------------------------------------------------------------------
      do 200 l=1,m
      d(l)=1.0e0
  200 continue
!
      do 100 k=1,n
      do 250 l=1,m
      p(l)=a(l,k,k)
  250 continue
      do 300 l=1,m
      r(l)=-1.0e0/p(l)
  300 continue
      do 350 l=1,m
      a(l,k,k)=0.0e0
  350 continue
      do  20 i=1,n
      do 400 l=1,m
      a(l,i,k)=a(l,i,k)*r(l)
  400 continue
   20 continue
      do 60 i=1,n
      if(i.eq.k) go to 60
      do  40 j=1,n
      do 450 l=1,m
      a(l,i,j)=a(l,i,k)*a(l,k,j)+a(l,i,j)
  450 continue
   40 continue
   60 continue
      do 600 l=1,m
      r(l)=-r(l)
  600 continue
      do  80 j=1,n
      do 650 l=1,m
      a(l,k,j)=a(l,k,j)*r(l)
  650 continue
   80 continue
      do 700 l=1,m
      d(l)=d(l)*p(l)
  700 continue
      do 750 l=1,m
      a(l,k,k)=r(l)
  750 continue
  100 continue
!-----------------------------------------------------------------------
      return
      end
