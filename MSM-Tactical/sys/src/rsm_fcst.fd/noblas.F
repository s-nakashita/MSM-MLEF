      subroutine dgemm ( transa, transb, m, n, k, alpha, a, lda, b, ldb, &
     &                   beta, c, ldc )
!     .. scalar arguments ..
      character*1        transa, transb
      integer            m, n, k, lda, ldb, ldc
      real               alpha, beta
!     .. array arguments ..
      real               a( lda, * ), b( ldb, * ), c( ldc, * )
!     ..
!
!  purpose
!  =======
!
!  dgemm  performs one of the matrix-matrix operations
!
!     c := alpha*op( a )*op( b ) + beta*c,
!
!  where  op( x ) is one of
!
!     op( x ) = x   or   op( x ) = x',
!
!  alpha and beta are scalars, and a, b and c are matrices, with op( a )
!  an m by k matrix,  op( b )  a  k by n matrix and  c an m by n matrix.
!
!  parameters
!  ==========
!
!  transa - character*1.
!           on entry, transa specifies the form of op( a ) to be used in
!           the matrix multiplication as follows:
!
!              transa = 'n' or 'n',  op( a ) = a.
!
!              transa = 't' or 't',  op( a ) = a'.
!
!              transa = 'c' or 'c',  op( a ) = a'.
!
!           unchanged on exit.
!
!  transb - character*1.
!           on entry, transb specifies the form of op( b ) to be used in
!           the matrix multiplication as follows:
!
!              transb = 'n' or 'n',  op( b ) = b.
!
!              transb = 't' or 't',  op( b ) = b'.
!
!              transb = 'c' or 'c',  op( b ) = b'.
!
!           unchanged on exit.
!
!  m      - integer.
!           on entry,  m  specifies  the number  of rows  of the  matrix
!           op( a )  and of the  matrix  c.  m  must  be at least  zero.
!           unchanged on exit.
!
!  n      - integer.
!           on entry,  n  specifies the number  of columns of the matrix
!           op( b ) and the number of columns of the matrix c. n must be
!           at least zero.
!           unchanged on exit.
!
!  k      - integer.
!           on entry,  k  specifies  the number of columns of the matrix
!           op( a ) and the number of rows of the matrix op( b ). k must
!           be at least  zero.
!           unchanged on exit.
!
!  alpha  - real            .
!           on entry, alpha specifies the scalar alpha.
!           unchanged on exit.
!
!  a      - real             array of dimension ( lda, ka ), where ka is
!           k  when  transa = 'n' or 'n',  and is  m  otherwise.
!           before entry with  transa = 'n' or 'n',  the leading  m by k
!           part of the array  a  must contain the matrix  a,  otherwise
!           the leading  k by m  part of the array  a  must contain  the
!           matrix a.
!           unchanged on exit.
!
!  lda    - integer.
!           on entry, lda specifies the first dimension of a as declared
!           in the calling (sub) program. when  transa = 'n' or 'n' then
!           lda must be at least  max( 1, m ), otherwise  lda must be at
!           least  max( 1, k ).
!           unchanged on exit.
!
!  b      - real             array of dimension ( ldb, kb ), where kb is
!           n  when  transb = 'n' or 'n',  and is  k  otherwise.
!           before entry with  transb = 'n' or 'n',  the leading  k by n
!           part of the array  b  must contain the matrix  b,  otherwise
!           the leading  n by k  part of the array  b  must contain  the
!           matrix b.
!           unchanged on exit.
!
!  ldb    - integer.
!           on entry, ldb specifies the first dimension of b as declared
!           in the calling (sub) program. when  transb = 'n' or 'n' then
!           ldb must be at least  max( 1, k ), otherwise  ldb must be at
!           least  max( 1, n ).
!           unchanged on exit.
!
!  beta   - real            .
!           on entry,  beta  specifies the scalar  beta.  when  beta  is
!           supplied as zero then c need not be set on input.
!           unchanged on exit.
!
!  c      - real             array of dimension ( ldc, n ).
!           before entry, the leading  m by n  part of the array  c must
!           contain the matrix  c,  except when  beta  is zero, in which
!           case c need not be set on entry.
!           on exit, the array  c  is overwritten by the  m by n  matrix
!           ( alpha*op( a )*op( b ) + beta*c ).
!
!  ldc    - integer.
!           on entry, ldc specifies the first dimension of c as declared
!           in  the  calling  (sub)  program.   ldc  must  be  at  least
!           max( 1, m ).
!           unchanged on exit.
!
!
!  level 3 blas routine.
!
!  -- written on 8-february-1989.
!     jack dongarra, argonne national laboratory.
!     iain duff, aere harwell.
!     jeremy du croz, numerical algorithms group ltd.
!     sven hammarling, numerical algorithms group ltd.
!
!
!     .. external functions ..
      logical            lsame
      external           lsame
!     .. external subroutines ..
      external           xerbla
!     .. intrinsic functions ..
      intrinsic          max
!     .. local scalars ..
      logical            nota, notb
      integer            i, info, j, l, ncola, nrowa, nrowb
      real               temp
!     .. parameters ..
      real               one         , zero
      parameter        ( one = 1.0e+0, zero = 0.0e+0 )
!     ..
!     .. executable statements ..
!
!     set  nota  and  notb  as  true if  a  and  b  respectively are not
!     transposed and set  nrowa, ncola and  nrowb  as the number of rows
!     and  columns of  a  and the  number of  rows  of  b  respectively.
!
      nota  = lsame( transa, 'n' )
      notb  = lsame( transb, 'n' )
      if( nota )then
         nrowa = m
         ncola = k
      else
         nrowa = k
         ncola = m
      end if
      if( notb )then
         nrowb = k
      else
         nrowb = n
      end if
!
!     test the input parameters.
!
      info = 0
      if(      ( .not.nota                 ).and.                       &
     &         ( .not.lsame( transa, 'c' ) ).and.                       &
     &         ( .not.lsame( transa, 't' ) )      )then
         info = 1
      else if( ( .not.notb                 ).and.                       &
     &         ( .not.lsame( transb, 'c' ) ).and.                       &
     &         ( .not.lsame( transb, 't' ) )      )then
         info = 2
      else if( m  .lt.0               )then
         info = 3
      else if( n  .lt.0               )then
         info = 4
      else if( k  .lt.0               )then
         info = 5
      else if( lda.lt.max( 1, nrowa ) )then
         info = 8
      else if( ldb.lt.max( 1, nrowb ) )then
         info = 10
      else if( ldc.lt.max( 1, m     ) )then
         info = 13
      end if
      if( info.ne.0 )then
         call xerbla( 'dgemm ', info )
         return
      end if
!
!     quick return if possible.
!
      if( ( m.eq.0 ).or.( n.eq.0 ).or.                                  &
     &    ( ( ( alpha.eq.zero ).or.( k.eq.0 ) ).and.( beta.eq.one ) ) ) &
     &   return
!
!     and if  alpha.eq.zero.
!
      if( alpha.eq.zero )then
         if( beta.eq.zero )then
            do 20, j = 1, n
               do 10, i = 1, m
                  c( i, j ) = zero
   10          continue
   20       continue
         else
            do 40, j = 1, n
               do 30, i = 1, m
                  c( i, j ) = beta*c( i, j )
   30          continue
   40       continue
         end if
         return
      end if
!
!     start the operations.
!
      if( notb )then
         if( nota )then
!
!           form  c := alpha*a*b + beta*c.
!
            do 90, j = 1, n
               if( beta.eq.zero )then
                  do 50, i = 1, m
                     c( i, j ) = zero
   50             continue
               else if( beta.ne.one )then
                  do 60, i = 1, m
                     c( i, j ) = beta*c( i, j )
   60             continue
               end if
               do 80, l = 1, k
                  if( b( l, j ).ne.zero )then
                     temp = alpha*b( l, j )
                     do 70, i = 1, m
                        c( i, j ) = c( i, j ) + temp*a( i, l )
   70                continue
                  end if
   80          continue
   90       continue
         else
!
!           form  c := alpha*a'*b + beta*c
!
            do 120, j = 1, n
               do 110, i = 1, m
                  temp = zero
                  do 100, l = 1, k
                     temp = temp + a( l, i )*b( l, j )
  100             continue
                  if( beta.eq.zero )then
                     c( i, j ) = alpha*temp
                  else
                     c( i, j ) = alpha*temp + beta*c( i, j )
                  end if
  110          continue
  120       continue
         end if
      else
         if( nota )then
!
!           form  c := alpha*a*b' + beta*c
!
            do 170, j = 1, n
               if( beta.eq.zero )then
                  do 130, i = 1, m
                     c( i, j ) = zero
  130             continue
               else if( beta.ne.one )then
                  do 140, i = 1, m
                     c( i, j ) = beta*c( i, j )
  140             continue
               end if
               do 160, l = 1, k
                  if( b( j, l ).ne.zero )then
                     temp = alpha*b( j, l )
                     do 150, i = 1, m
                        c( i, j ) = c( i, j ) + temp*a( i, l )
  150                continue
                  end if
  160          continue
  170       continue
         else
!
!           form  c := alpha*a'*b' + beta*c
!
            do 200, j = 1, n
               do 190, i = 1, m
                  temp = zero
                  do 180, l = 1, k
                     temp = temp + a( l, i )*b( j, l )
  180             continue
                  if( beta.eq.zero )then
                     c( i, j ) = alpha*temp
                  else
                     c( i, j ) = alpha*temp + beta*c( i, j )
                  end if
  190          continue
  200       continue
         end if
      end if
!
      return
!
!     end of dgemm .
!
      end
      subroutine dgetri( n, a, lda, ipiv, work, lwork, info )
!
!  -- lapack routine (version 2.0) --
!     univ. of tennessee, univ. of california berkeley, nag ltd.,
!     courant institute, argonne national lab, and rice university
!     september 30, 1994
!
!     .. scalar arguments ..
      integer            info, lda, lwork, n
!     ..
!     .. array arguments ..
      integer            ipiv( * )
      real               a( lda, * ), work( lwork )
!     ..
!
!  purpose
!  =======
!
!  dgetri computes the inverse of a matrix using the lu factorization
!  computed by dgetrf.
!
!  this method inverts u and then computes inv(a) by solving the system
!  inv(a)*l = inv(u) for inv(a).
!
!  arguments
!  =========
!
!  n       (input) integer
!          the order of the matrix a.  n >= 0.
!
!  a       (input/output) real array, dimension (lda,n)
!          on entry, the factors l and u from the factorization
!          a = p*l*u as computed by dgetrf.
!          on exit, if info = 0, the inverse of the original matrix a.
!
!  lda     (input) integer
!          the leading dimension of the array a.  lda >= max(1,n).
!
!  ipiv    (input) integer array, dimension (n)
!          the pivot indices from dgetrf; for 1<=i<=n, row i of the
!          matrix was interchanged with row ipiv(i).
!
!  work    (workspace/output) real array, dimension (lwork)
!          on exit, if info=0, then work(1) returns the optimal lwork.
!
!  lwork   (input) integer
!          the dimension of the array work.  lwork >= max(1,n).
!          for optimal performance lwork >= n*nb, where nb is
!          the optimal blocksize returned by ilaenv.
!
!  info    (output) integer
!          = 0:  successful exit
!          < 0:  if info = -i, the i-th argument had an illegal value
!          > 0:  if info = i, u(i,i) is exactly zero; the matrix is
!                singular and its inverse could not be computed.
!
!  =====================================================================
!
!     .. parameters ..
      real               zero, one
      parameter          ( zero = 0.0e+0, one = 1.0e+0 )
!     ..
!     .. local scalars ..
      integer            i, iws, j, jb, jj, jp, ldwork, nb, nbmin, nn
!     ..
!     .. external functions ..
      integer            ilaenv
      external           ilaenv
!     ..
!     .. external subroutines ..
      external           dgemm, dgemv, dswap, dtrsm, dtrtri, xerbla
!     ..
!     .. intrinsic functions ..
      intrinsic          max, min
!     ..
!     .. executable statements ..
!
!     test the input parameters.
!
      info = 0
      work( 1 ) = max( n, 1 )
      if( n.lt.0 ) then
         info = -1
      else if( lda.lt.max( 1, n ) ) then
         info = -3
      else if( lwork.lt.max( 1, n ) ) then
         info = -6
      end if
      if( info.ne.0 ) then
         call xerbla( 'dgetri', -info )
         return
      end if
!
!     quick return if possible
!
      if( n.eq.0 )                                                      &
     &   return
!
!     form inv(u).  if info > 0 from dtrtri, then u is singular,
!     and the inverse is not computed.
!
      call dtrtri( 'upper', 'non-unit', n, a, lda, info )
      if( info.gt.0 )                                                   &
     &   return
!
!     determine the block size for this environment.
!
      nb = ilaenv( 1, 'dgetri', ' ', n, -1, -1, -1 )
      nbmin = 2
      ldwork = n
      if( nb.gt.1 .and. nb.lt.n ) then
         iws = max( ldwork*nb, 1 )
         if( lwork.lt.iws ) then
            nb = lwork / ldwork
            nbmin = max( 2, ilaenv( 2, 'dgetri', ' ', n, -1, -1, -1 ) )
         end if
      else
         iws = n
      end if
!
!     solve the equation inv(a)*l = inv(u) for inv(a).
!
      if( nb.lt.nbmin .or. nb.ge.n ) then
!
!        use unblocked code.
!
         do 20 j = n, 1, -1
!
!           copy current column of l to work and replace with zeros.
!
            do 10 i = j + 1, n
               work( i ) = a( i, j )
               a( i, j ) = zero
   10       continue
!
!           compute current column of inv(a).
!
            if( j.lt.n )                                                &
     &         call dgemv( 'no transpose', n, n-j, -one, a( 1, j+1 ),   &
     &                     lda, work( j+1 ), 1, one, a( 1, j ), 1 )
   20    continue
      else
!
!        use blocked code.
!
         nn = ( ( n-1 ) / nb )*nb + 1
         do 50 j = nn, 1, -nb
            jb = min( nb, n-j+1 )
!
!           copy current block column of l to work and replace with
!           zeros.
!
            do 40 jj = j, j + jb - 1
               do 30 i = jj + 1, n
                  work( i+( jj-j )*ldwork ) = a( i, jj )
                  a( i, jj ) = zero
   30          continue
   40       continue
!
!           compute current block column of inv(a).
!
            if( j+jb.le.n )                                             &
     &         call dgemm( 'no transpose', 'no transpose', n, jb,       &
     &                     n-j-jb+1, -one, a( 1, j+jb ), lda,           &
     &                     work( j+jb ), ldwork, one, a( 1, j ), lda )
            call dtrsm( 'right', 'lower', 'no transpose', 'unit', n, jb, &
     &                  one, work( j ), ldwork, a( 1, j ), lda )
   50    continue
      end if
!
!     apply column interchanges.
!
      do 60 j = n - 1, 1, -1
         jp = ipiv( j )
         if( jp.ne.j )                                                  &
     &      call dswap( n, a( 1, j ), 1, a( 1, jp ), 1 )
   60 continue
!
      work( 1 ) = iws
      return
!
!     end of dgetri
!
      end
      subroutine dgetrf( m, n, a, lda, ipiv, info )
!
!  -- lapack routine (version 2.0) --
!     univ. of tennessee, univ. of california berkeley, nag ltd.,
!     courant institute, argonne national lab, and rice university
!     march 31, 1993 
!
!     .. scalar arguments ..
      integer            info, lda, m, n
!     ..
!     .. array arguments ..
      integer            ipiv( * )
      real               a( lda, * )
!     ..
!
!  purpose
!  =======
!
!  dgetrf computes an lu factorization of a general m-by-n matrix a
!  using partial pivoting with row interchanges.
!
!  the factorization has the form
!     a = p * l * u
!  where p is a permutation matrix, l is lower triangular with unit
!  diagonal elements (lower trapezoidal if m > n), and u is upper
!  triangular (upper trapezoidal if m < n).
!
!  this is the right-looking level 3 blas version of the algorithm.
!
!  arguments
!  =========
!
!  m       (input) integer
!          the number of rows of the matrix a.  m >= 0.
!
!  n       (input) integer
!          the number of columns of the matrix a.  n >= 0.
!
!  a       (input/output) real array, dimension (lda,n)
!          on entry, the m-by-n matrix to be factored.
!          on exit, the factors l and u from the factorization
!          a = p*l*u; the unit diagonal elements of l are not stored.
!
!  lda     (input) integer
!          the leading dimension of the array a.  lda >= max(1,m).
!
!  ipiv    (output) integer array, dimension (min(m,n))
!          the pivot indices; for 1 <= i <= min(m,n), row i of the
!          matrix was interchanged with row ipiv(i).
!
!  info    (output) integer
!          = 0:  successful exit
!          < 0:  if info = -i, the i-th argument had an illegal value
!          > 0:  if info = i, u(i,i) is exactly zero. the factorization
!                has been completed, but the factor u is exactly
!                singular, and division by zero will occur if it is used
!                to solve a system of equations.
!
!  =====================================================================
!
!     .. parameters ..
      real               one
      parameter          ( one = 1.0e+0 )
!     ..
!     .. local scalars ..
      integer            i, iinfo, j, jb, nb
!     ..
!     .. external subroutines ..
      external           dgemm, dgetf2, dlaswp, dtrsm, xerbla
!     ..
!     .. external functions ..
      integer            ilaenv
      external           ilaenv
!     ..
!     .. intrinsic functions ..
      intrinsic          max, min
!     ..
!     .. executable statements ..
!
!     test the input parameters.
!
      info = 0
      if( m.lt.0 ) then
         info = -1
      else if( n.lt.0 ) then
         info = -2
      else if( lda.lt.max( 1, m ) ) then
         info = -4
      end if
      if( info.ne.0 ) then
         call xerbla( 'dgetrf', -info )
         return
      end if
!
!     quick return if possible
!
      if( m.eq.0 .or. n.eq.0 )                                           &
     &   return
!
!     determine the block size for this environment.
!
      nb = ilaenv( 1, 'dgetrf', ' ', m, n, -1, -1 )
      if( nb.le.1 .or. nb.ge.min( m, n ) ) then
!
!        use unblocked code.
!
         call dgetf2( m, n, a, lda, ipiv, info )
      else
!
!        use blocked code.
!
         do 20 j = 1, min( m, n ), nb
            jb = min( min( m, n )-j+1, nb )
!
!           factor diagonal and subdiagonal blocks and test for exact
!           singularity.
!
            call dgetf2( m-j+1, jb, a( j, j ), lda, ipiv( j ), iinfo )
!
!           adjust info and the pivot indices.
!
            if( info.eq.0 .and. iinfo.gt.0 )                            &
     &         info = iinfo + j - 1
            do 10 i = j, min( m, j+jb-1 )
               ipiv( i ) = j - 1 + ipiv( i )
   10       continue
!
!           apply interchanges to columns 1:j-1.
!
            call dlaswp( j-1, a, lda, j, j+jb-1, ipiv, 1 )
!
            if( j+jb.le.n ) then
!
!              apply interchanges to columns j+jb:n.
!
               call dlaswp( n-j-jb+1, a( 1, j+jb ), lda, j, j+jb-1,     &
     &                      ipiv, 1 )
!
!              compute block row of u.
!
               call dtrsm( 'left', 'lower', 'no transpose', 'unit', jb, &
     &                     n-j-jb+1, one, a( j, j ), lda, a( j, j+jb ), &
     &                     lda )
               if( j+jb.le.m ) then
!
!                 update trailing submatrix.
!
                  call dgemm( 'no transpose', 'no transpose', m-j-jb+1, &
     &                        n-j-jb+1, jb, -one, a( j+jb, j ), lda,    &
     &                        a( j, j+jb ), lda, one, a( j+jb, j+jb ),  &
     &                        lda )
               end if
            end if
   20    continue
      end if
      return
!
!     end of dgetrf
!
      end
      subroutine xerbla( srname, info )
!
!  -- lapack auxiliary routine (preliminary version) --
!     univ. of tennessee, univ. of california berkeley, nag ltd.,
!     courant institute, argonne national lab, and rice university
!     february 29, 1992
!
!     .. scalar arguments ..
      character*6        srname
      integer            info
!     ..
!
!  purpose
!  =======
!
!  xerbla  is an error handler for the lapack routines.
!  it is called by an lapack routine if an input parameter has an
!  invalid value.  a message is printed and execution stops.
!
!  installers may consider modifying the stop statement in order to
!  call system-specific exception-handling facilities.
!
!  arguments
!  =========
!
!  srname  (input) character*6
!          the name of the routine which called xerbla.
!
!  info    (input) integer
!          the position of the invalid parameter in the parameter list
!          of the calling routine.
!
!
      write( *, fmt = 9999 )srname, info
!
      stop
!
 9999 format( ' ** on entry to ', a6, ' parameter number ', i2, ' had ', &
     &      'an illegal value' )
!
!     end of xerbla
!
      end
      subroutine dtrtri( uplo, diag, n, a, lda, info )
!
!  -- lapack routine (version 2.0) --
!     univ. of tennessee, univ. of california berkeley, nag ltd.,
!     courant institute, argonne national lab, and rice university
!     march 31, 1993 
!
!     .. scalar arguments ..
      character          diag, uplo, opt*2
      integer            info, lda, n
!     ..
!     .. array arguments ..
      real               a( lda, * )
!     ..
!
!  purpose
!  =======
!
!  dtrtri computes the inverse of a real upper or lower triangular
!  matrix a.
!
!  this is the level 3 blas version of the algorithm.
!
!  arguments
!  =========
!
!  uplo    (input) character*1
!          = 'u':  a is upper triangular;
!          = 'l':  a is lower triangular.
!
!  diag    (input) character*1
!          = 'n':  a is non-unit triangular;
!          = 'u':  a is unit triangular.
!
!  n       (input) integer
!          the order of the matrix a.  n >= 0.
!
!  a       (input/output) real array, dimension (lda,n)
!          on entry, the triangular matrix a.  if uplo = 'u', the
!          leading n-by-n upper triangular part of the array a contains
!          the upper triangular matrix, and the strictly lower
!          triangular part of a is not referenced.  if uplo = 'l', the
!          leading n-by-n lower triangular part of the array a contains
!          the lower triangular matrix, and the strictly upper
!          triangular part of a is not referenced.  if diag = 'u', the
!          diagonal elements of a are also not referenced and are
!          assumed to be 1.
!          on exit, the (triangular) inverse of the original matrix, in
!          the same storage format.
!
!  lda     (input) integer
!          the leading dimension of the array a.  lda >= max(1,n).
!
!  info    (output) integer
!          = 0: successful exit
!          < 0: if info = -i, the i-th argument had an illegal value
!          > 0: if info = i, a(i,i) is exactly zero.  the triangular
!               matrix is singular and its inverse can not be computed.
!
!  =====================================================================
!
!     .. parameters ..
      real               one, zero
      parameter          ( one = 1.0e+0, zero = 0.0e+0 )
!     ..
!     .. local scalars ..
      logical            nounit, upper
      integer            j, jb, nb, nn
!     ..
!     .. external functions ..
      logical            lsame
      integer            ilaenv
      external           lsame, ilaenv
!     ..
!     .. external subroutines ..
      external           dtrmm, dtrsm, dtrti2, xerbla
!     ..
!     .. intrinsic functions ..
      intrinsic          max, min
!     ..
!     .. executable statements ..
!
!     test the input parameters.
!
      info = 0
      upper = lsame( uplo, 'u' )
      nounit = lsame( diag, 'n' )
      if( .not.upper .and. .not.lsame( uplo, 'l' ) ) then
         info = -1
      else if( .not.nounit .and. .not.lsame( diag, 'u' ) ) then
         info = -2
      else if( n.lt.0 ) then
         info = -3
      else if( lda.lt.max( 1, n ) ) then
         info = -5
      end if
      if( info.ne.0 ) then
         call xerbla( 'dtrtri', -info )
         return
      end if
!
!     quick return if possible
!
      if( n.eq.0 )                                                      &
     &   return
!
!     check for singularity if non-unit.
!
      if( nounit ) then
         do 10 info = 1, n
            if( a( info, info ).eq.zero )                               &
     &         return
   10    continue
         info = 0
      end if
!
!     determine the block size for this environment.
!
!hmhj nb = ilaenv( 1, 'dtrtri', uplo // diag, n, -1, -1, -1 )
      write(opt,"(a,a)") uplo,diag
      nb = ilaenv( 1, 'dtrtri', opt, n, -1, -1, -1 )
      if( nb.le.1 .or. nb.ge.n ) then
!
!        use unblocked code
!
         call dtrti2( uplo, diag, n, a, lda, info )
      else
!
!        use blocked code
!
         if( upper ) then
!
!           compute inverse of upper triangular matrix
!
            do 20 j = 1, n, nb
               jb = min( nb, n-j+1 )
!
!              compute rows 1:j-1 of current block column
!
               call dtrmm( 'left', 'upper', 'no transpose', diag, j-1,  &
     &                     jb, one, a, lda, a( 1, j ), lda )
               call dtrsm( 'right', 'upper', 'no transpose', diag, j-1, &
     &                     jb, -one, a( j, j ), lda, a( 1, j ), lda )
!
!              compute inverse of current diagonal block
!
               call dtrti2( 'upper', diag, jb, a( j, j ), lda, info )
   20       continue
         else
!
!           compute inverse of lower triangular matrix
!
            nn = ( ( n-1 ) / nb )*nb + 1
            do 30 j = nn, 1, -nb
               jb = min( nb, n-j+1 )
               if( j+jb.le.n ) then
!
!                 compute rows j+jb:n of current block column
!
                  call dtrmm( 'left', 'lower', 'no transpose', diag,    &
     &                        n-j-jb+1, jb, one, a( j+jb, j+jb ), lda,  &
     &                        a( j+jb, j ), lda )
                  call dtrsm( 'right', 'lower', 'no transpose', diag,   &
     &                        n-j-jb+1, jb, -one, a( j, j ), lda,       &
     &                        a( j+jb, j ), lda )
               end if
!
!              compute inverse of current diagonal block
!
               call dtrti2( 'lower', diag, jb, a( j, j ), lda, info )
   30       continue
         end if
      end if
!
      return
!
!     end of dtrtri
!
      end
      subroutine dgemv ( trans, m, n, alpha, a, lda, x, incx,           &
     &                   beta, y, incy )
!     .. scalar arguments ..
      real               alpha, beta
      integer            incx, incy, lda, m, n
      character*1        trans
!     .. array arguments ..
      real               a( lda, * ), x( * ), y( * )
!     ..
!
!  purpose
!  =======
!
!  dgemv  performs one of the matrix-vector operations
!
!     y := alpha*a*x + beta*y,   or   y := alpha*a'*x + beta*y,
!
!  where alpha and beta are scalars, x and y are vectors and a is an
!  m by n matrix.
!
!  parameters
!  ==========
!
!  trans  - character*1.
!           on entry, trans specifies the operation to be performed as
!           follows:
!
!              trans = 'n' or 'n'   y := alpha*a*x + beta*y.
!
!              trans = 't' or 't'   y := alpha*a'*x + beta*y.
!
!              trans = 'c' or 'c'   y := alpha*a'*x + beta*y.
!
!           unchanged on exit.
!
!  m      - integer.
!           on entry, m specifies the number of rows of the matrix a.
!           m must be at least zero.
!           unchanged on exit.
!
!  n      - integer.
!           on entry, n specifies the number of columns of the matrix a.
!           n must be at least zero.
!           unchanged on exit.
!
!  alpha  - real            .
!           on entry, alpha specifies the scalar alpha.
!           unchanged on exit.
!
!  a      - real             array of dimension ( lda, n ).
!           before entry, the leading m by n part of the array a must
!           contain the matrix of coefficients.
!           unchanged on exit.
!
!  lda    - integer.
!           on entry, lda specifies the first dimension of a as declared
!           in the calling (sub) program. lda must be at least
!           max( 1, m ).
!           unchanged on exit.
!
!  x      - real             array of dimension at least
!           ( 1 + ( n - 1 )*abs( incx ) ) when trans = 'n' or 'n'
!           and at least
!           ( 1 + ( m - 1 )*abs( incx ) ) otherwise.
!           before entry, the incremented array x must contain the
!           vector x.
!           unchanged on exit.
!
!  incx   - integer.
!           on entry, incx specifies the increment for the elements of
!           x. incx must not be zero.
!           unchanged on exit.
!
!  beta   - real            .
!           on entry, beta specifies the scalar beta. when beta is
!           supplied as zero then y need not be set on input.
!           unchanged on exit.
!
!  y      - real             array of dimension at least
!           ( 1 + ( m - 1 )*abs( incy ) ) when trans = 'n' or 'n'
!           and at least
!           ( 1 + ( n - 1 )*abs( incy ) ) otherwise.
!           before entry with beta non-zero, the incremented array y
!           must contain the vector y. on exit, y is overwritten by the
!           updated vector y.
!
!  incy   - integer.
!           on entry, incy specifies the increment for the elements of
!           y. incy must not be zero.
!           unchanged on exit.
!
!
!  level 2 blas routine.
!
!  -- written on 22-october-1986.
!     jack dongarra, argonne national lab.
!     jeremy du croz, nag central office.
!     sven hammarling, nag central office.
!     richard hanson, sandia national labs.
!
!
!     .. parameters ..
      real               one         , zero
      parameter        ( one = 1.0e+0, zero = 0.0e+0 )
!     .. local scalars ..
      real               temp
      integer            i, info, ix, iy, j, jx, jy, kx, ky, lenx, leny
!     .. external functions ..
      logical            lsame
      external           lsame
!     .. external subroutines ..
      external           xerbla
!     .. intrinsic functions ..
      intrinsic          max
!     ..
!     .. executable statements ..
!
!     test the input parameters.
!
      info = 0
      if     ( .not.lsame( trans, 'n' ).and.                            &
     &         .not.lsame( trans, 't' ).and.                            &
     &         .not.lsame( trans, 'c' )      )then
         info = 1
      else if( m.lt.0 )then
         info = 2
      else if( n.lt.0 )then
         info = 3
      else if( lda.lt.max( 1, m ) )then
         info = 6
      else if( incx.eq.0 )then
         info = 8
      else if( incy.eq.0 )then
         info = 11
      end if
      if( info.ne.0 )then
         call xerbla( 'dgemv ', info )
         return
      end if
!
!     quick return if possible.
!
      if( ( m.eq.0 ).or.( n.eq.0 ).or.                                  &
     &    ( ( alpha.eq.zero ).and.( beta.eq.one ) ) )                   &
     &   return
!
!     set  lenx  and  leny, the lengths of the vectors x and y, and set
!     up the start points in  x  and  y.
!
      if( lsame( trans, 'n' ) )then
         lenx = n
         leny = m
      else
         lenx = m
         leny = n
      end if
      if( incx.gt.0 )then
         kx = 1
      else
         kx = 1 - ( lenx - 1 )*incx
      end if
      if( incy.gt.0 )then
         ky = 1
      else
         ky = 1 - ( leny - 1 )*incy
      end if
!
!     start the operations. in this version the elements of a are
!     accessed sequentially with one pass through a.
!
!     first form  y := beta*y.
!
      if( beta.ne.one )then
         if( incy.eq.1 )then
            if( beta.eq.zero )then
               do 10, i = 1, leny
                  y( i ) = zero
   10          continue
            else
               do 20, i = 1, leny
                  y( i ) = beta*y( i )
   20          continue
            end if
         else
            iy = ky
            if( beta.eq.zero )then
               do 30, i = 1, leny
                  y( iy ) = zero
                  iy      = iy   + incy
   30          continue
            else
               do 40, i = 1, leny
                  y( iy ) = beta*y( iy )
                  iy      = iy           + incy
   40          continue
            end if
         end if
      end if
      if( alpha.eq.zero )                                               &
     &   return
      if( lsame( trans, 'n' ) )then
!
!        form  y := alpha*a*x + y.
!
         jx = kx
         if( incy.eq.1 )then
            do 60, j = 1, n
               if( x( jx ).ne.zero )then
                  temp = alpha*x( jx )
                  do 50, i = 1, m
                     y( i ) = y( i ) + temp*a( i, j )
   50             continue
               end if
               jx = jx + incx
   60       continue
         else
            do 80, j = 1, n
               if( x( jx ).ne.zero )then
                  temp = alpha*x( jx )
                  iy   = ky
                  do 70, i = 1, m
                     y( iy ) = y( iy ) + temp*a( i, j )
                     iy      = iy      + incy
   70             continue
               end if
               jx = jx + incx
   80       continue
         end if
      else
!
!        form  y := alpha*a'*x + y.
!
         jy = ky
         if( incx.eq.1 )then
            do 100, j = 1, n
               temp = zero
               do 90, i = 1, m
                  temp = temp + a( i, j )*x( i )
   90          continue
               y( jy ) = y( jy ) + alpha*temp
               jy      = jy      + incy
  100       continue
         else
            do 120, j = 1, n
               temp = zero
               ix   = kx
               do 110, i = 1, m
                  temp = temp + a( i, j )*x( ix )
                  ix   = ix   + incx
  110          continue
               y( jy ) = y( jy ) + alpha*temp
               jy      = jy      + incy
  120       continue
         end if
      end if
!
      return
!
!     end of dgemv .
!
      end
      subroutine dtrsm ( side, uplo, transa, diag, m, n, alpha, a, lda, &
     &                   b, ldb )
!     .. scalar arguments ..
      character*1        side, uplo, transa, diag
      integer            m, n, lda, ldb
      real               alpha
!     .. array arguments ..
      real               a( lda, * ), b( ldb, * )
!     ..
!
!  purpose
!  =======
!
!  dtrsm  solves one of the matrix equations
!
!     op( a )*x = alpha*b,   or   x*op( a ) = alpha*b,
!
!  where alpha is a scalar, x and b are m by n matrices, a is a unit, or
!  non-unit,  upper or lower triangular matrix  and  op( a )  is one  of
!
!     op( a ) = a   or   op( a ) = a'.
!
!  the matrix x is overwritten on b.
!
!  parameters
!  ==========
!
!  side   - character*1.
!           on entry, side specifies whether op( a ) appears on the left
!           or right of x as follows:
!
!              side = 'l' or 'l'   op( a )*x = alpha*b.
!
!              side = 'r' or 'r'   x*op( a ) = alpha*b.
!
!           unchanged on exit.
!
!  uplo   - character*1.
!           on entry, uplo specifies whether the matrix a is an upper or
!           lower triangular matrix as follows:
!
!              uplo = 'u' or 'u'   a is an upper triangular matrix.
!
!              uplo = 'l' or 'l'   a is a lower triangular matrix.
!
!           unchanged on exit.
!
!  transa - character*1.
!           on entry, transa specifies the form of op( a ) to be used in
!           the matrix multiplication as follows:
!
!              transa = 'n' or 'n'   op( a ) = a.
!
!              transa = 't' or 't'   op( a ) = a'.
!
!              transa = 'c' or 'c'   op( a ) = a'.
!
!           unchanged on exit.
!
!  diag   - character*1.
!           on entry, diag specifies whether or not a is unit triangular
!           as follows:
!
!              diag = 'u' or 'u'   a is assumed to be unit triangular.
!
!              diag = 'n' or 'n'   a is not assumed to be unit
!                                  triangular.
!
!           unchanged on exit.
!
!  m      - integer.
!           on entry, m specifies the number of rows of b. m must be at
!           least zero.
!           unchanged on exit.
!
!  n      - integer.
!           on entry, n specifies the number of columns of b.  n must be
!           at least zero.
!           unchanged on exit.
!
!  alpha  - real            .
!           on entry,  alpha specifies the scalar  alpha. when  alpha is
!           zero then  a is not referenced and  b need not be set before
!           entry.
!           unchanged on exit.
!
!  a      - real             array of dimension ( lda, k ), where k is m
!           when  side = 'l' or 'l'  and is  n  when  side = 'r' or 'r'.
!           before entry  with  uplo = 'u' or 'u',  the  leading  k by k
!           upper triangular part of the array  a must contain the upper
!           triangular matrix  and the strictly lower triangular part of
!           a is not referenced.
!           before entry  with  uplo = 'l' or 'l',  the  leading  k by k
!           lower triangular part of the array  a must contain the lower
!           triangular matrix  and the strictly upper triangular part of
!           a is not referenced.
!           note that when  diag = 'u' or 'u',  the diagonal elements of
!           a  are not referenced either,  but are assumed to be  unity.
!           unchanged on exit.
!
!  lda    - integer.
!           on entry, lda specifies the first dimension of a as declared
!           in the calling (sub) program.  when  side = 'l' or 'l'  then
!           lda  must be at least  max( 1, m ),  when  side = 'r' or 'r'
!           then lda must be at least max( 1, n ).
!           unchanged on exit.
!
!  b      - real             array of dimension ( ldb, n ).
!           before entry,  the leading  m by n part of the array  b must
!           contain  the  right-hand  side  matrix  b,  and  on exit  is
!           overwritten by the solution matrix  x.
!
!  ldb    - integer.
!           on entry, ldb specifies the first dimension of b as declared
!           in  the  calling  (sub)  program.   ldb  must  be  at  least
!           max( 1, m ).
!           unchanged on exit.
!
!
!  level 3 blas routine.
!
!
!  -- written on 8-february-1989.
!     jack dongarra, argonne national laboratory.
!     iain duff, aere harwell.
!     jeremy du croz, numerical algorithms group ltd.
!     sven hammarling, numerical algorithms group ltd.
!
!
!     .. external functions ..
      logical            lsame
      external           lsame
!     .. external subroutines ..
      external           xerbla
!     .. intrinsic functions ..
      intrinsic          max
!     .. local scalars ..
      logical            lside, nounit, upper
      integer            i, info, j, k, nrowa
      real               temp
!     .. parameters ..
      real               one         , zero
      parameter        ( one = 1.0e+0, zero = 0.0e+0 )
!     ..
!     .. executable statements ..
!
!     test the input parameters.
!
      lside  = lsame( side  , 'l' )
      if( lside )then
         nrowa = m
      else
         nrowa = n
      end if
      nounit = lsame( diag  , 'n' )
      upper  = lsame( uplo  , 'u' )
!
      info   = 0
      if(      ( .not.lside                ).and.                       &
     &         ( .not.lsame( side  , 'r' ) )      )then
         info = 1
      else if( ( .not.upper                ).and.                       &
     &         ( .not.lsame( uplo  , 'l' ) )      )then
         info = 2
      else if( ( .not.lsame( transa, 'n' ) ).and.                       &
     &         ( .not.lsame( transa, 't' ) ).and.                       &
     &         ( .not.lsame( transa, 'c' ) )      )then
         info = 3
      else if( ( .not.lsame( diag  , 'u' ) ).and.                       &
     &         ( .not.lsame( diag  , 'n' ) )      )then
         info = 4
      else if( m  .lt.0               )then
         info = 5
      else if( n  .lt.0               )then
         info = 6
      else if( lda.lt.max( 1, nrowa ) )then
         info = 9
      else if( ldb.lt.max( 1, m     ) )then
         info = 11
      end if
      if( info.ne.0 )then
         call xerbla( 'dtrsm ', info )
         return
      end if
!
!     quick return if possible.
!
      if( n.eq.0 )                                                      &
     &   return
!
!     and when  alpha.eq.zero.
!
      if( alpha.eq.zero )then
         do 20, j = 1, n
            do 10, i = 1, m
               b( i, j ) = zero
   10       continue
   20    continue
         return
      end if
!
!     start the operations.
!
      if( lside )then
         if( lsame( transa, 'n' ) )then
!
!           form  b := alpha*inv( a )*b.
!
            if( upper )then
               do 60, j = 1, n
                  if( alpha.ne.one )then
                     do 30, i = 1, m
                        b( i, j ) = alpha*b( i, j )
   30                continue
                  end if
                  do 50, k = m, 1, -1
                     if( b( k, j ).ne.zero )then
                        if( nounit )                                       &
     &                     b( k, j ) = b( k, j )/a( k, k )
                        do 40, i = 1, k - 1
                           b( i, j ) = b( i, j ) - b( k, j )*a( i, k )
   40                   continue
                     end if
   50             continue
   60          continue
            else
               do 100, j = 1, n
                  if( alpha.ne.one )then
                     do 70, i = 1, m
                        b( i, j ) = alpha*b( i, j )
   70                continue
                  end if
                  do 90 k = 1, m
                     if( b( k, j ).ne.zero )then
                        if( nounit )                                       &
     &                     b( k, j ) = b( k, j )/a( k, k )
                        do 80, i = k + 1, m
                           b( i, j ) = b( i, j ) - b( k, j )*a( i, k )
   80                   continue
                     end if
   90             continue
  100          continue
            end if
         else
!
!           form  b := alpha*inv( a' )*b.
!
            if( upper )then
               do 130, j = 1, n
                  do 120, i = 1, m
                     temp = alpha*b( i, j )
                     do 110, k = 1, i - 1
                        temp = temp - a( k, i )*b( k, j )
  110                continue
                     if( nounit )                                       &
     &                  temp = temp/a( i, i )
                     b( i, j ) = temp
  120             continue
  130          continue
            else
               do 160, j = 1, n
                  do 150, i = m, 1, -1
                     temp = alpha*b( i, j )
                     do 140, k = i + 1, m
                        temp = temp - a( k, i )*b( k, j )
  140                continue
                     if( nounit )                                       &
     &                  temp = temp/a( i, i )
                     b( i, j ) = temp
  150             continue
  160          continue
            end if
         end if
      else
         if( lsame( transa, 'n' ) )then
!
!           form  b := alpha*b*inv( a ).
!
            if( upper )then
               do 210, j = 1, n
                  if( alpha.ne.one )then
                     do 170, i = 1, m
                        b( i, j ) = alpha*b( i, j )
  170                continue
                  end if
                  do 190, k = 1, j - 1
                     if( a( k, j ).ne.zero )then
                        do 180, i = 1, m
                           b( i, j ) = b( i, j ) - a( k, j )*b( i, k )
  180                   continue
                     end if
  190             continue
                  if( nounit )then
                     temp = one/a( j, j )
                     do 200, i = 1, m
                        b( i, j ) = temp*b( i, j )
  200                continue
                  end if
  210          continue
            else
               do 260, j = n, 1, -1
                  if( alpha.ne.one )then
                     do 220, i = 1, m
                        b( i, j ) = alpha*b( i, j )
  220                continue
                  end if
                  do 240, k = j + 1, n
                     if( a( k, j ).ne.zero )then
                        do 230, i = 1, m
                           b( i, j ) = b( i, j ) - a( k, j )*b( i, k )
  230                   continue
                     end if
  240             continue
                  if( nounit )then
                     temp = one/a( j, j )
                     do 250, i = 1, m
                       b( i, j ) = temp*b( i, j )
  250                continue
                  end if
  260          continue
            end if
         else
!
!           form  b := alpha*b*inv( a' ).
!
            if( upper )then
               do 310, k = n, 1, -1
                  if( nounit )then
                     temp = one/a( k, k )
                     do 270, i = 1, m
                        b( i, k ) = temp*b( i, k )
  270                continue
                  end if
                  do 290, j = 1, k - 1
                     if( a( j, k ).ne.zero )then
                        temp = a( j, k )
                        do 280, i = 1, m
                           b( i, j ) = b( i, j ) - temp*b( i, k )
  280                   continue
                     end if
  290             continue
                  if( alpha.ne.one )then
                     do 300, i = 1, m
                        b( i, k ) = alpha*b( i, k )
  300                continue
                  end if
  310          continue
            else
               do 360, k = 1, n
                  if( nounit )then
                     temp = one/a( k, k )
                     do 320, i = 1, m
                        b( i, k ) = temp*b( i, k )
  320                continue
                  end if
                  do 340, j = k + 1, n
                     if( a( j, k ).ne.zero )then
                        temp = a( j, k )
                        do 330, i = 1, m
                           b( i, j ) = b( i, j ) - temp*b( i, k )
  330                   continue
                     end if
  340             continue
                  if( alpha.ne.one )then
                     do 350, i = 1, m
                        b( i, k ) = alpha*b( i, k )
  350                continue
                  end if
  360          continue
            end if
         end if
      end if
!
      return
!
!     end of dtrsm .
!
      end

      subroutine dgetf2( m, n, a, lda, ipiv, info )
!
!  -- lapack routine (version 2.0) --
!     univ. of tennessee, univ. of california berkeley, nag ltd.,
!     courant institute, argonne national lab, and rice university
!     june 30, 1992
!
!     .. scalar arguments ..
      integer            info, lda, m, n
!     ..
!     .. array arguments ..
      integer            ipiv( * )
      real               a( lda, * )
!     ..
!
!  purpose
!  =======
!
!  dgetf2 computes an lu factorization of a general m-by-n matrix a
!  using partial pivoting with row interchanges.
!
!  the factorization has the form
!     a = p * l * u
!  where p is a permutation matrix, l is lower triangular with unit
!  diagonal elements (lower trapezoidal if m > n), and u is upper
!  triangular (upper trapezoidal if m < n).
!
!  this is the right-looking level 2 blas version of the algorithm.
!
!  arguments
!  =========
!
!  m       (input) integer
!          the number of rows of the matrix a.  m >= 0.
!
!  n       (input) integer
!          the number of columns of the matrix a.  n >= 0.
!
!  a       (input/output) real array, dimension (lda,n)
!          on entry, the m by n matrix to be factored.
!          on exit, the factors l and u from the factorization
!          a = p*l*u; the unit diagonal elements of l are not stored.
!
!  lda     (input) integer
!          the leading dimension of the array a.  lda >= max(1,m).
!
!  ipiv    (output) integer array, dimension (min(m,n))
!          the pivot indices; for 1 <= i <= min(m,n), row i of the
!          matrix was interchanged with row ipiv(i).
!
!  info    (output) integer
!          = 0: successful exit
!          < 0: if info = -k, the k-th argument had an illegal value
!          > 0: if info = k, u(k,k) is exactly zero. the factorization
!               has been completed, but the factor u is exactly
!               singular, and division by zero will occur if it is used
!               to solve a system of equations.
!
!  =====================================================================
!
!     .. parameters ..
      real               one, zero
      parameter          ( one = 1.0e+0, zero = 0.0e+0 )
!     ..
!     .. local scalars ..
      integer            j, jp
!     ..
!     .. external functions ..
      integer            isamax
      external           isamax
!     ..
!     .. external subroutines ..
      external           dger, dscal, dswap, xerbla
!     ..
!     .. intrinsic functions ..
      intrinsic          max, min
!     ..
!     .. executable statements ..
!
!     test the input parameters.
!
      info = 0
      if( m.lt.0 ) then
         info = -1
      else if( n.lt.0 ) then
         info = -2
      else if( lda.lt.max( 1, m ) ) then
         info = -4
      end if
      if( info.ne.0 ) then
         call xerbla( 'dgetf2', -info )
         return
      end if
!
!     quick return if possible
!
      if( m.eq.0 .or. n.eq.0 )                                          &
     &   return
!
      do 10 j = 1, min( m, n )
!
!        find pivot and test for singularity.
!
         jp = j - 1 + isamax( m-j+1, a( j, j ), 1 )
         ipiv( j ) = jp
         if( a( jp, j ).ne.zero ) then
!
!           apply the interchange to columns 1:n.
!
            if( jp.ne.j )                                               &
     &         call dswap( n, a( j, 1 ), lda, a( jp, 1 ), lda )
!
!           compute elements j+1:m of j-th column.
!
            if( j.lt.m )                                                &
     &         call dscal( m-j, one / a( j, j ), a( j+1, j ), 1 )
!
         else if( info.eq.0 ) then
!
            info = j
         end if
!
         if( j.lt.min( m, n ) ) then
!
!           update trailing submatrix.
!
            call dger( m-j, n-j, -one, a( j+1, j ), 1, a( j, j+1 ), lda, &
     &                 a( j+1, j+1 ), lda )
         end if
   10 continue
      return
!
!     end of dgetf2
!
      end
      subroutine dlaswp( n, a, lda, k1, k2, ipiv, incx )
!
!  -- lapack auxiliary routine (version 2.0) --
!     univ. of tennessee, univ. of california berkeley, nag ltd.,
!     courant institute, argonne national lab, and rice university
!     october 31, 1992
!
!     .. scalar arguments ..
      integer            incx, k1, k2, lda, n
!     ..
!     .. array arguments ..
      integer            ipiv( * )
      real               a( lda, * )
!     ..
!
!  purpose
!  =======
!
!  dlaswp performs a series of row interchanges on the matrix a.
!  one row interchange is initiated for each of rows k1 through k2 of a.
!
!  arguments
!  =========
!
!  n       (input) integer
!          the number of columns of the matrix a.
!
!  a       (input/output) real array, dimension (lda,n)
!          on entry, the matrix of column dimension n to which the row
!          interchanges will be applied.
!          on exit, the permuted matrix.
!
!  lda     (input) integer
!          the leading dimension of the array a.
!
!  k1      (input) integer
!          the first element of ipiv for which a row interchange will
!          be done.
!
!  k2      (input) integer
!          the last element of ipiv for which a row interchange will
!          be done.
!
!  ipiv    (input) integer array, dimension (m*abs(incx))
!          the vector of pivot indices.  only the elements in positions
!          k1 through k2 of ipiv are accessed.
!          ipiv(k) = l implies rows k and l are to be interchanged.
!
!  incx    (input) integer
!          the increment between successive values of ipiv.  if ipiv
!          is negative, the pivots are applied in reverse order.
!
! =====================================================================
!
!     .. local scalars ..
      integer            i, ip, ix
!     ..
!     .. external subroutines ..
      external           dswap
!     ..
!     .. executable statements ..
!
!     interchange row i with row ipiv(i) for each of rows k1 through k2.
!
      if( incx.eq.0 )                                                   &
     &   return
      if( incx.gt.0 ) then
         ix = k1
      else
         ix = 1 + ( 1-k2 )*incx
      end if
      if( incx.eq.1 ) then
         do 10 i = k1, k2
            ip = ipiv( i )
            if( ip.ne.i )                                               &
     &         call dswap( n, a( i, 1 ), lda, a( ip, 1 ), lda )
   10    continue
      else if( incx.gt.1 ) then
         do 20 i = k1, k2
            ip = ipiv( ix )
            if( ip.ne.i )                                               &
     &         call dswap( n, a( i, 1 ), lda, a( ip, 1 ), lda )
            ix = ix + incx
   20    continue
      else if( incx.lt.0 ) then
         do 30 i = k2, k1, -1
            ip = ipiv( ix )
            if( ip.ne.i )                                               &
     &         call dswap( n, a( i, 1 ), lda, a( ip, 1 ), lda )
            ix = ix + incx
   30    continue
      end if
!
      return
!
!     end of dlaswp
!
      end
      subroutine dswap (n,sx,incx,sy,incy)
!
!     interchanges two vectors.
!     uses unrolled loops for increments equal to 1.
!     jack dongarra, linpack, 3/11/78.
!     modified 12/3/93, array(1) declarations changed to array(*)
!
      real sx(*),sy(*),stemp
      integer i,incx,incy,ix,iy,m,mp1,n
!
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
!
!       code for unequal increments or equal increments not equal
!         to 1
!
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        stemp = sx(ix)
        sx(ix) = sy(iy)
        sy(iy) = stemp
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
!
!       code for both increments equal to 1
!
!
!       clean-up loop
!
   20 m = mod(n,3)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        stemp = sx(i)
        sx(i) = sy(i)
        sy(i) = stemp
   30 continue
      if( n .lt. 3 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,3
        stemp = sx(i)
        sx(i) = sy(i)
        sy(i) = stemp
        stemp = sx(i + 1)
        sx(i + 1) = sy(i + 1)
        sy(i + 1) = stemp
        stemp = sx(i + 2)
        sx(i + 2) = sy(i + 2)
        sy(i + 2) = stemp
   50 continue
      return
      end
      subroutine dscal(n,sa,sx,incx)
!
!     scales a vector by a constant.
!     uses unrolled loops for increment equal to 1.
!     jack dongarra, linpack, 3/11/78.
!     modified 3/93 to return if incx .le. 0.
!     modified 12/3/93, array(1) declarations changed to array(*)
!
      real sa,sx(*)
      integer i,incx,m,mp1,n,nincx
!
      if( n.le.0 .or. incx.le.0 )return
      if(incx.eq.1)go to 20
!
!        code for increment not equal to 1
!
      nincx = n*incx
      do 10 i = 1,nincx,incx
        sx(i) = sa*sx(i)
   10 continue
      return
!
!        code for increment equal to 1
!
!
!        clean-up loop
!
   20 m = mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        sx(i) = sa*sx(i)
   30 continue
      if( n .lt. 5 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,5
        sx(i) = sa*sx(i)
        sx(i + 1) = sa*sx(i + 1)
        sx(i + 2) = sa*sx(i + 2)
        sx(i + 3) = sa*sx(i + 3)
        sx(i + 4) = sa*sx(i + 4)
   50 continue
      return
      end
      subroutine dger  ( m, n, alpha, x, incx, y, incy, a, lda )
!     .. scalar arguments ..
      real               alpha
      integer            incx, incy, lda, m, n
!     .. array arguments ..
      real               a( lda, * ), x( * ), y( * )
!     ..
!
!  purpose
!  =======
!
!  dger   performs the rank 1 operation
!
!     a := alpha*x*y' + a,
!
!  where alpha is a scalar, x is an m element vector, y is an n element
!  vector and a is an m by n matrix.
!
!  parameters
!  ==========
!
!  m      - integer.
!           on entry, m specifies the number of rows of the matrix a.
!           m must be at least zero.
!           unchanged on exit.
!
!  n      - integer.
!           on entry, n specifies the number of columns of the matrix a.
!           n must be at least zero.
!           unchanged on exit.
!
!  alpha  - real            .
!           on entry, alpha specifies the scalar alpha.
!           unchanged on exit.
!
!  x      - real             array of dimension at least
!           ( 1 + ( m - 1 )*abs( incx ) ).
!           before entry, the incremented array x must contain the m
!           element vector x.
!           unchanged on exit.
!
!  incx   - integer.
!           on entry, incx specifies the increment for the elements of
!           x. incx must not be zero.
!           unchanged on exit.
!
!  y      - real             array of dimension at least
!           ( 1 + ( n - 1 )*abs( incy ) ).
!           before entry, the incremented array y must contain the n
!           element vector y.
!           unchanged on exit.
!
!  incy   - integer.
!           on entry, incy specifies the increment for the elements of
!           y. incy must not be zero.
!           unchanged on exit.
!
!  a      - real             array of dimension ( lda, n ).
!           before entry, the leading m by n part of the array a must
!           contain the matrix of coefficients. on exit, a is
!           overwritten by the updated matrix.
!
!  lda    - integer.
!           on entry, lda specifies the first dimension of a as declared
!           in the calling (sub) program. lda must be at least
!           max( 1, m ).
!           unchanged on exit.
!
!
!  level 2 blas routine.
!
!  -- written on 22-october-1986.
!     jack dongarra, argonne national lab.
!     jeremy du croz, nag central office.
!     sven hammarling, nag central office.
!     richard hanson, sandia national labs.
!
!
!     .. parameters ..
      real               zero
      parameter        ( zero = 0.0e+0 )
!     .. local scalars ..
      real               temp
      integer            i, info, ix, j, jy, kx
!     .. external subroutines ..
      external           xerbla
!     .. intrinsic functions ..
      intrinsic          max
!     ..
!     .. executable statements ..
!
!     test the input parameters.
!
      info = 0
      if     ( m.lt.0 )then
         info = 1
      else if( n.lt.0 )then
         info = 2
      else if( incx.eq.0 )then
         info = 5
      else if( incy.eq.0 )then
         info = 7
      else if( lda.lt.max( 1, m ) )then
         info = 9
      end if
      if( info.ne.0 )then
         call xerbla( 'dger  ', info )
         return
      end if
!
!     quick return if possible.
!
      if( ( m.eq.0 ).or.( n.eq.0 ).or.( alpha.eq.zero ) )               &
     &   return
!
!     start the operations. in this version the elements of a are
!     accessed sequentially with one pass through a.
!
      if( incy.gt.0 )then
         jy = 1
      else
         jy = 1 - ( n - 1 )*incy
      end if
      if( incx.eq.1 )then
         do 20, j = 1, n
            if( y( jy ).ne.zero )then
               temp = alpha*y( jy )
               do 10, i = 1, m
                  a( i, j ) = a( i, j ) + x( i )*temp
   10          continue
            end if
            jy = jy + incy
   20    continue
      else
         if( incx.gt.0 )then
            kx = 1
         else
            kx = 1 - ( m - 1 )*incx
         end if
         do 40, j = 1, n
            if( y( jy ).ne.zero )then
               temp = alpha*y( jy )
               ix   = kx
               do 30, i = 1, m
                  a( i, j ) = a( i, j ) + x( ix )*temp
                  ix        = ix        + incx
   30          continue
            end if
            jy = jy + incy
   40    continue
      end if
!
      return
!
!     end of dger  .
!
      end
      subroutine dtrti2( uplo, diag, n, a, lda, info )
!
!  -- lapack routine (version 2.0) --
!     univ. of tennessee, univ. of california berkeley, nag ltd.,
!     courant institute, argonne national lab, and rice university
!     february 29, 1992
!
!     .. scalar arguments ..
      character          diag, uplo
      integer            info, lda, n
!     ..
!     .. array arguments ..
      real               a( lda, * )
!     ..
!
!  purpose
!  =======
!
!  dtrti2 computes the inverse of a real upper or lower triangular
!  matrix.
!
!  this is the level 2 blas version of the algorithm.
!
!  arguments
!  =========
!
!  uplo    (input) character*1
!          specifies whether the matrix a is upper or lower triangular.
!          = 'u':  upper triangular
!          = 'l':  lower triangular
!
!  diag    (input) character*1
!          specifies whether or not the matrix a is unit triangular.
!          = 'n':  non-unit triangular
!          = 'u':  unit triangular
!
!  n       (input) integer
!          the order of the matrix a.  n >= 0.
!
!  a       (input/output) real array, dimension (lda,n)
!          on entry, the triangular matrix a.  if uplo = 'u', the
!          leading n by n upper triangular part of the array a contains
!          the upper triangular matrix, and the strictly lower
!          triangular part of a is not referenced.  if uplo = 'l', the
!          leading n by n lower triangular part of the array a contains
!          the lower triangular matrix, and the strictly upper
!          triangular part of a is not referenced.  if diag = 'u', the
!          diagonal elements of a are also not referenced and are
!          assumed to be 1.
!
!          on exit, the (triangular) inverse of the original matrix, in
!          the same storage format.
!
!  lda     (input) integer
!          the leading dimension of the array a.  lda >= max(1,n).
!
!  info    (output) integer
!          = 0: successful exit
!          < 0: if info = -k, the k-th argument had an illegal value
!
!  =====================================================================
!
!     .. parameters ..
      real               one
      parameter          ( one = 1.0e+0 )
!     ..
!     .. local scalars ..
      logical            nounit, upper
      integer            j
      real               ajj
!     ..
!     .. external functions ..
      logical            lsame
      external           lsame
!     ..
!     .. external subroutines ..
      external           dscal, dtrmv, xerbla
!     ..
!     .. intrinsic functions ..
      intrinsic          max
!     ..
!     .. executable statements ..
!
!     test the input parameters.
!
      info = 0
      upper = lsame( uplo, 'u' )
      nounit = lsame( diag, 'n' )
      if( .not.upper .and. .not.lsame( uplo, 'l' ) ) then
         info = -1
      else if( .not.nounit .and. .not.lsame( diag, 'u' ) ) then
         info = -2
      else if( n.lt.0 ) then
         info = -3
      else if( lda.lt.max( 1, n ) ) then
         info = -5
      end if
      if( info.ne.0 ) then
         call xerbla( 'dtrti2', -info )
         return
      end if
!
      if( upper ) then
!
!        compute inverse of upper triangular matrix.
!
         do 10 j = 1, n
            if( nounit ) then
               a( j, j ) = one / a( j, j )
               ajj = -a( j, j )
            else
               ajj = -one
            end if
!
!           compute elements 1:j-1 of j-th column.
!
            call dtrmv( 'upper', 'no transpose', diag, j-1, a, lda,     &
     &                  a( 1, j ), 1 )
            call dscal( j-1, ajj, a( 1, j ), 1 )
   10    continue
      else
!
!        compute inverse of lower triangular matrix.
!
         do 20 j = n, 1, -1
            if( nounit ) then
               a( j, j ) = one / a( j, j )
               ajj = -a( j, j )
            else
               ajj = -one
            end if
            if( j.lt.n ) then
!
!              compute elements j+1:n of j-th column.
!
               call dtrmv( 'lower', 'no transpose', diag, n-j,          &
     &                     a( j+1, j+1 ), lda, a( j+1, j ), 1 )
               call dscal( n-j, ajj, a( j+1, j ), 1 )
            end if
   20    continue
      end if
!
      return
!
!     end of dtrti2
!
      end
      subroutine dtrmm ( side, uplo, transa, diag, m, n, alpha, a, lda, &
     &                   b, ldb )
!     .. scalar arguments ..
      character*1        side, uplo, transa, diag
      integer            m, n, lda, ldb
      real               alpha
!     .. array arguments ..
      real               a( lda, * ), b( ldb, * )
!     ..
!
!  purpose
!  =======
!
!  dtrmm  performs one of the matrix-matrix operations
!
!     b := alpha*op( a )*b,   or   b := alpha*b*op( a ),
!
!  where  alpha  is a scalar,  b  is an m by n matrix,  a  is a unit, or
!  non-unit,  upper or lower triangular matrix  and  op( a )  is one  of
!
!     op( a ) = a   or   op( a ) = a'.
!
!  parameters
!  ==========
!
!  side   - character*1.
!           on entry,  side specifies whether  op( a ) multiplies b from
!           the left or right as follows:
!
!              side = 'l' or 'l'   b := alpha*op( a )*b.
!
!              side = 'r' or 'r'   b := alpha*b*op( a ).
!
!           unchanged on exit.
!
!  uplo   - character*1.
!           on entry, uplo specifies whether the matrix a is an upper or
!           lower triangular matrix as follows:
!
!              uplo = 'u' or 'u'   a is an upper triangular matrix.
!
!              uplo = 'l' or 'l'   a is a lower triangular matrix.
!
!           unchanged on exit.
!
!  transa - character*1.
!           on entry, transa specifies the form of op( a ) to be used in
!           the matrix multiplication as follows:
!
!              transa = 'n' or 'n'   op( a ) = a.
!
!              transa = 't' or 't'   op( a ) = a'.
!
!              transa = 'c' or 'c'   op( a ) = a'.
!
!           unchanged on exit.
!
!  diag   - character*1.
!           on entry, diag specifies whether or not a is unit triangular
!           as follows:
!
!              diag = 'u' or 'u'   a is assumed to be unit triangular.
!
!              diag = 'n' or 'n'   a is not assumed to be unit
!                                  triangular.
!
!           unchanged on exit.
!
!  m      - integer.
!           on entry, m specifies the number of rows of b. m must be at
!           least zero.
!           unchanged on exit.
!
!  n      - integer.
!           on entry, n specifies the number of columns of b.  n must be
!           at least zero.
!           unchanged on exit.
!
!  alpha  - real            .
!           on entry,  alpha specifies the scalar  alpha. when  alpha is
!           zero then  a is not referenced and  b need not be set before
!           entry.
!           unchanged on exit.
!
!  a      - real             array of dimension ( lda, k ), where k is m
!           when  side = 'l' or 'l'  and is  n  when  side = 'r' or 'r'.
!           before entry  with  uplo = 'u' or 'u',  the  leading  k by k
!           upper triangular part of the array  a must contain the upper
!           triangular matrix  and the strictly lower triangular part of
!           a is not referenced.
!           before entry  with  uplo = 'l' or 'l',  the  leading  k by k
!           lower triangular part of the array  a must contain the lower
!           triangular matrix  and the strictly upper triangular part of
!           a is not referenced.
!           note that when  diag = 'u' or 'u',  the diagonal elements of
!           a  are not referenced either,  but are assumed to be  unity.
!           unchanged on exit.
!
!  lda    - integer.
!           on entry, lda specifies the first dimension of a as declared
!           in the calling (sub) program.  when  side = 'l' or 'l'  then
!           lda  must be at least  max( 1, m ),  when  side = 'r' or 'r'
!           then lda must be at least max( 1, n ).
!           unchanged on exit.
!
!  b      - real             array of dimension ( ldb, n ).
!           before entry,  the leading  m by n part of the array  b must
!           contain the matrix  b,  and  on exit  is overwritten  by the
!           transformed matrix.
!
!  ldb    - integer.
!           on entry, ldb specifies the first dimension of b as declared
!           in  the  calling  (sub)  program.   ldb  must  be  at  least
!           max( 1, m ).
!           unchanged on exit.
!
!
!  level 3 blas routine.
!
!  -- written on 8-february-1989.
!     jack dongarra, argonne national laboratory.
!     iain duff, aere harwell.
!     jeremy du croz, numerical algorithms group ltd.
!     sven hammarling, numerical algorithms group ltd.
!
!
!     .. external functions ..
      logical            lsame
      external           lsame
!     .. external subroutines ..
      external           xerbla
!     .. intrinsic functions ..
      intrinsic          max
!     .. local scalars ..
      logical            lside, nounit, upper
      integer            i, info, j, k, nrowa
      real               temp
!     .. parameters ..
      real               one         , zero
      parameter        ( one = 1.0e+0, zero = 0.0e+0 )
!     ..
!     .. executable statements ..
!
!     test the input parameters.
!
      lside  = lsame( side  , 'l' )
      if( lside )then
         nrowa = m
      else
         nrowa = n
      end if
      nounit = lsame( diag  , 'n' )
      upper  = lsame( uplo  , 'u' )
!
      info   = 0
      if(      ( .not.lside                ).and.                       &
     &         ( .not.lsame( side  , 'r' ) )      )then
         info = 1
      else if( ( .not.upper                ).and.                       &
     &         ( .not.lsame( uplo  , 'l' ) )      )then
         info = 2
      else if( ( .not.lsame( transa, 'n' ) ).and.                       &
     &         ( .not.lsame( transa, 't' ) ).and.                       &
     &         ( .not.lsame( transa, 'c' ) )      )then
         info = 3
      else if( ( .not.lsame( diag  , 'u' ) ).and.                       &
     &         ( .not.lsame( diag  , 'n' ) )      )then
         info = 4
      else if( m  .lt.0               )then
         info = 5
      else if( n  .lt.0               )then
         info = 6
      else if( lda.lt.max( 1, nrowa ) )then
         info = 9
      else if( ldb.lt.max( 1, m     ) )then
         info = 11
      end if
      if( info.ne.0 )then
         call xerbla( 'dtrmm ', info )
         return
      end if
!
!     quick return if possible.
!
      if( n.eq.0 )                                                      &
     &   return
!
!     and when  alpha.eq.zero.
!
      if( alpha.eq.zero )then
         do 20, j = 1, n
            do 10, i = 1, m
               b( i, j ) = zero
   10       continue
   20    continue
         return
      end if
!
!     start the operations.
!
      if( lside )then
         if( lsame( transa, 'n' ) )then
!
!           form  b := alpha*a*b.
!
            if( upper )then
               do 50, j = 1, n
                  do 40, k = 1, m
                     if( b( k, j ).ne.zero )then
                        temp = alpha*b( k, j )
                        do 30, i = 1, k - 1
                           b( i, j ) = b( i, j ) + temp*a( i, k )
   30                   continue
                        if( nounit )                                          &
     &                     temp = temp*a( k, k )
                        b( k, j ) = temp
                     end if
   40             continue
   50          continue
            else
               do 80, j = 1, n
                  do 70 k = m, 1, -1
                     if( b( k, j ).ne.zero )then
                        temp      = alpha*b( k, j )
                        b( k, j ) = temp
                        if( nounit )                                          &
     &                     b( k, j ) = b( k, j )*a( k, k )
                        do 60, i = k + 1, m
                           b( i, j ) = b( i, j ) + temp*a( i, k )
   60                   continue
                     end if
   70             continue
   80          continue
            end if
         else
!
!           form  b := alpha*a'*b.
!
            if( upper )then
               do 110, j = 1, n
                  do 100, i = m, 1, -1
                     temp = b( i, j )
                     if( nounit )                                          &
     &                  temp = temp*a( i, i )
                     do 90, k = 1, i - 1
                        temp = temp + a( k, i )*b( k, j )
   90                continue
                     b( i, j ) = alpha*temp
  100             continue
  110          continue
            else
               do 140, j = 1, n
                  do 130, i = 1, m
                     temp = b( i, j )
                     if( nounit )                                          &
     &                  temp = temp*a( i, i )
                     do 120, k = i + 1, m
                        temp = temp + a( k, i )*b( k, j )
  120                continue
                     b( i, j ) = alpha*temp
  130             continue
  140          continue
            end if
         end if
      else
         if( lsame( transa, 'n' ) )then
!
!           form  b := alpha*b*a.
!
            if( upper )then
               do 180, j = n, 1, -1
                  temp = alpha
                  if( nounit )                                          &
     &               temp = temp*a( j, j )
                  do 150, i = 1, m
                     b( i, j ) = temp*b( i, j )
  150             continue
                  do 170, k = 1, j - 1
                     if( a( k, j ).ne.zero )then
                        temp = alpha*a( k, j )
                        do 160, i = 1, m
                           b( i, j ) = b( i, j ) + temp*b( i, k )
  160                   continue
                     end if
  170             continue
  180          continue
            else
               do 220, j = 1, n
                  temp = alpha
                  if( nounit )                                          &
     &               temp = temp*a( j, j )
                  do 190, i = 1, m
                     b( i, j ) = temp*b( i, j )
  190             continue
                  do 210, k = j + 1, n
                     if( a( k, j ).ne.zero )then
                        temp = alpha*a( k, j )
                        do 200, i = 1, m
                           b( i, j ) = b( i, j ) + temp*b( i, k )
  200                   continue
                     end if
  210             continue
  220          continue
            end if
         else
!
!           form  b := alpha*b*a'.
!
            if( upper )then
               do 260, k = 1, n
                  do 240, j = 1, k - 1
                     if( a( j, k ).ne.zero )then
                        temp = alpha*a( j, k )
                        do 230, i = 1, m
                           b( i, j ) = b( i, j ) + temp*b( i, k )
  230                   continue
                     end if
  240             continue
                  temp = alpha
                  if( nounit )                                          &
     &               temp = temp*a( k, k )
                  if( temp.ne.one )then
                     do 250, i = 1, m
                        b( i, k ) = temp*b( i, k )
  250                continue
                  end if
  260          continue
            else
               do 300, k = n, 1, -1
                  do 280, j = k + 1, n
                     if( a( j, k ).ne.zero )then
                        temp = alpha*a( j, k )
                        do 270, i = 1, m
                           b( i, j ) = b( i, j ) + temp*b( i, k )
  270                   continue
                     end if
  280             continue
                  temp = alpha
                  if( nounit )                                          &
     &               temp = temp*a( k, k )
                  if( temp.ne.one )then
                     do 290, i = 1, m
                        b( i, k ) = temp*b( i, k )
  290                continue
                  end if
  300          continue
            end if
         end if
      end if
!
      return
!
!     end of dtrmm .
!
      end
      subroutine dtrmv ( uplo, trans, diag, n, a, lda, x, incx )
!     .. scalar arguments ..
      integer            incx, lda, n
      character*1        diag, trans, uplo
!     .. array arguments ..
      real               a( lda, * ), x( * )
!     ..
!
!  purpose
!  =======
!
!  dtrmv  performs one of the matrix-vector operations
!
!     x := a*x,   or   x := a'*x,
!
!  where x is an n element vector and  a is an n by n unit, or non-unit,
!  upper or lower triangular matrix.
!
!  parameters
!  ==========
!
!  uplo   - character*1.
!           on entry, uplo specifies whether the matrix is an upper or
!           lower triangular matrix as follows:
!
!              uplo = 'u' or 'u'   a is an upper triangular matrix.
!
!              uplo = 'l' or 'l'   a is a lower triangular matrix.
!
!           unchanged on exit.
!
!  trans  - character*1.
!           on entry, trans specifies the operation to be performed as
!           follows:
!
!              trans = 'n' or 'n'   x := a*x.
!
!              trans = 't' or 't'   x := a'*x.
!
!              trans = 'c' or 'c'   x := a'*x.
!
!           unchanged on exit.
!
!  diag   - character*1.
!           on entry, diag specifies whether or not a is unit
!           triangular as follows:
!
!              diag = 'u' or 'u'   a is assumed to be unit triangular.
!
!              diag = 'n' or 'n'   a is not assumed to be unit
!                                  triangular.
!
!           unchanged on exit.
!
!  n      - integer.
!           on entry, n specifies the order of the matrix a.
!           n must be at least zero.
!           unchanged on exit.
!
!  a      - real             array of dimension ( lda, n ).
!           before entry with  uplo = 'u' or 'u', the leading n by n
!           upper triangular part of the array a must contain the upper
!           triangular matrix and the strictly lower triangular part of
!           a is not referenced.
!           before entry with uplo = 'l' or 'l', the leading n by n
!           lower triangular part of the array a must contain the lower
!           triangular matrix and the strictly upper triangular part of
!           a is not referenced.
!           note that when  diag = 'u' or 'u', the diagonal elements of
!           a are not referenced either, but are assumed to be unity.
!           unchanged on exit.
!
!  lda    - integer.
!           on entry, lda specifies the first dimension of a as declared
!           in the calling (sub) program. lda must be at least
!           max( 1, n ).
!           unchanged on exit.
!
!  x      - real             array of dimension at least
!           ( 1 + ( n - 1 )*abs( incx ) ).
!           before entry, the incremented array x must contain the n
!           element vector x. on exit, x is overwritten with the
!           tranformed vector x.
!
!  incx   - integer.
!           on entry, incx specifies the increment for the elements of
!           x. incx must not be zero.
!           unchanged on exit.
!
!
!  level 2 blas routine.
!
!  -- written on 22-october-1986.
!     jack dongarra, argonne national lab.
!     jeremy du croz, nag central office.
!     sven hammarling, nag central office.
!     richard hanson, sandia national labs.
!
!
!     .. parameters ..
      real               zero
      parameter        ( zero = 0.0e+0 )
!     .. local scalars ..
      real               temp
      integer            i, info, ix, j, jx, kx
      logical            nounit
!     .. external functions ..
      logical            lsame
      external           lsame
!     .. external subroutines ..
      external           xerbla
!     .. intrinsic functions ..
      intrinsic          max
!     ..
!     .. executable statements ..
!
!     test the input parameters.
!
      info = 0
      if     ( .not.lsame( uplo , 'u' ).and.                            &
     &         .not.lsame( uplo , 'l' )      )then
         info = 1
      else if( .not.lsame( trans, 'n' ).and.                            &
     &         .not.lsame( trans, 't' ).and.                            &
     &         .not.lsame( trans, 'c' )      )then
         info = 2
      else if( .not.lsame( diag , 'u' ).and.                            &
     &         .not.lsame( diag , 'n' )      )then
         info = 3
      else if( n.lt.0 )then
         info = 4
      else if( lda.lt.max( 1, n ) )then
         info = 6
      else if( incx.eq.0 )then
         info = 8
      end if
      if( info.ne.0 )then
         call xerbla( 'dtrmv ', info )
         return
      end if
!
!     quick return if possible.
!
      if( n.eq.0 )                                                      &
     &   return
!
      nounit = lsame( diag, 'n' )
!
!     set up the start point in x if the increment is not unity. this
!     will be  ( n - 1 )*incx  too small for descending loops.
!
      if( incx.le.0 )then
         kx = 1 - ( n - 1 )*incx
      else if( incx.ne.1 )then
         kx = 1
      end if
!
!     start the operations. in this version the elements of a are
!     accessed sequentially with one pass through a.
!
      if( lsame( trans, 'n' ) )then
!
!        form  x := a*x.
!
         if( lsame( uplo, 'u' ) )then
            if( incx.eq.1 )then
               do 20, j = 1, n
                  if( x( j ).ne.zero )then
                     temp = x( j )
                     do 10, i = 1, j - 1
                        x( i ) = x( i ) + temp*a( i, j )
   10                continue
                     if( nounit )                                          &
     &                  x( j ) = x( j )*a( j, j )
                  end if
   20          continue
            else
               jx = kx
               do 40, j = 1, n
                  if( x( jx ).ne.zero )then
                     temp = x( jx )
                     ix   = kx
                     do 30, i = 1, j - 1
                        x( ix ) = x( ix ) + temp*a( i, j )
                        ix      = ix      + incx
   30                continue
                     if( nounit )                                          &
     &                  x( jx ) = x( jx )*a( j, j )
                  end if
                  jx = jx + incx
   40          continue
            end if
         else
            if( incx.eq.1 )then
               do 60, j = n, 1, -1
                  if( x( j ).ne.zero )then
                     temp = x( j )
                     do 50, i = n, j + 1, -1
                        x( i ) = x( i ) + temp*a( i, j )
   50                continue
                     if( nounit )                                          &
     &                  x( j ) = x( j )*a( j, j )
                  end if
   60          continue
            else
               kx = kx + ( n - 1 )*incx
               jx = kx
               do 80, j = n, 1, -1
                  if( x( jx ).ne.zero )then
                     temp = x( jx )
                     ix   = kx
                     do 70, i = n, j + 1, -1
                        x( ix ) = x( ix ) + temp*a( i, j )
                        ix      = ix      - incx
   70                continue
                     if( nounit )                                          &
     &                  x( jx ) = x( jx )*a( j, j )
                  end if
                  jx = jx - incx
   80          continue
            end if
         end if
      else
!
!        form  x := a'*x.
!
         if( lsame( uplo, 'u' ) )then
            if( incx.eq.1 )then
               do 100, j = n, 1, -1
                  temp = x( j )
                  if( nounit )                                          &
     &               temp = temp*a( j, j )
                  do 90, i = j - 1, 1, -1
                     temp = temp + a( i, j )*x( i )
   90             continue
                  x( j ) = temp
  100          continue
            else
               jx = kx + ( n - 1 )*incx
               do 120, j = n, 1, -1
                  temp = x( jx )
                  ix   = jx
                  if( nounit )                                          &
     &               temp = temp*a( j, j )
                  do 110, i = j - 1, 1, -1
                     ix   = ix   - incx
                     temp = temp + a( i, j )*x( ix )
  110             continue
                  x( jx ) = temp
                  jx      = jx   - incx
  120          continue
            end if
         else
            if( incx.eq.1 )then
               do 140, j = 1, n
                  temp = x( j )
                  if( nounit )                                          &
     &               temp = temp*a( j, j )
                  do 130, i = j + 1, n
                     temp = temp + a( i, j )*x( i )
  130             continue
                  x( j ) = temp
  140          continue
            else
               jx = kx
               do 160, j = 1, n
                  temp = x( jx )
                  ix   = jx
                  if( nounit )                                          &
     &               temp = temp*a( j, j )
                  do 150, i = j + 1, n
                     ix   = ix   + incx
                     temp = temp + a( i, j )*x( ix )
  150             continue
                  x( jx ) = temp
                  jx      = jx   + incx
  160          continue
            end if
         end if
      end if
!
      return
!
!     end of dtrmv .
!
      end



      integer          function ilaenv( ispec, name, opts, n1, n2, n3,  &
     &                 n4 )
!
!  -- lapack auxiliary routine (version 2.0) --
!     univ. of tennessee, univ. of california berkeley, nag ltd.,
!     courant institute, argonne national lab, and rice university
!     september 30, 1994
!
!     .. scalar arguments ..
      character*( * )    name, opts
      integer            ispec, n1, n2, n3, n4
!     ..
!
!  purpose
!  =======
!
!  ilaenv is called from the lapack routines to choose problem-dependent
!  parameters for the local environment.  see ispec for a description of
!  the parameters.
!
!  this version provides a set of parameters which should give good,
!  but not optimal, performance on many of the currently available
!  computers.  users are encouraged to modify this subroutine to set
!  the tuning parameters for their particular machine using the option
!  and problem size information in the arguments.
!
!  this routine will not function correctly if it is converted to all
!  lower case.  converting it to all upper case is allowed.
!
!  arguments
!  =========
!
!  ispec   (input) integer
!          specifies the parameter to be returned as the value of
!          ilaenv.
!          = 1: the optimal blocksize; if this value is 1, an unblocked
!               algorithm will give the best performance.
!          = 2: the minimum block size for which the block routine
!               should be used; if the usable block size is less than
!               this value, an unblocked routine should be used.
!          = 3: the crossover point (in a block routine, for n less
!               than this value, an unblocked routine should be used)
!          = 4: the number of shifts, used in the nonsymmetric
!               eigenvalue routines
!          = 5: the minimum column dimension for blocking to be used;
!               rectangular blocks must have dimension at least k by m,
!               where k is given by ilaenv(2,...) and m by ilaenv(5,...)
!          = 6: the crossover point for the svd (when reducing an m by n
!               matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
!               this value, a qr factorization is used first to reduce
!               the matrix to a triangular form.)
!          = 7: the number of processors
!          = 8: the crossover point for the multishift qr and qz methods
!               for nonsymmetric eigenvalue problems.
!
!  name    (input) character*(*)
!          the name of the calling subroutine, in either upper case or
!          lower case.
!
!  opts    (input) character*(*)
!          the character options to the subroutine name, concatenated
!          into a single character string.  for example, uplo = 'u',
!          trans = 't', and diag = 'n' for a triangular routine would
!          be specified as opts = 'utn'.
!
!  n1      (input) integer
!  n2      (input) integer
!  n3      (input) integer
!  n4      (input) integer
!          problem dimensions for the subroutine name; these may not all
!          be required.
!
! (ilaenv) (output) integer
!          >= 0: the value of the parameter specified by ispec
!          < 0:  if ilaenv = -k, the k-th argument had an illegal value.
!
!  further details
!  ===============
!
!  the following conventions have been used when calling ilaenv from the
!  lapack routines:
!  1)  opts is a concatenation of all of the character options to
!      subroutine name, in the same order that they appear in the
!      argument list for name, even if they are not used in determining
!      the value of the parameter specified by ispec.
!  2)  the problem dimensions n1, n2, n3, n4 are specified in the order
!      that they appear in the argument list for name.  n1 is used
!      first, n2 second, and so on, and unused problem dimensions are
!      passed a value of -1.
!  3)  the parameter value returned by ilaenv is checked for validity in
!      the calling subroutine.  for example, ilaenv is used to retrieve
!      the optimal blocksize for dtrtri as follows:
!
!      nb = ilaenv( 1, 'dtrtri', uplo // diag, n, -1, -1, -1 )
!      if( nb.le.1 ) nb = max( 1, n )
!
!  =====================================================================
!
!     .. local scalars ..
      logical            cname, sname
      character*1        c1
      character*2        c2, c4
      character*3        c3
      character*6        subnam
      integer            i, ic, iz, nb, nbmin, nx
!     ..
!     .. intrinsic functions ..
      intrinsic          char, ichar, int, min, real
!     ..
!     .. executable statements ..
!
      go to ( 100, 100, 100, 400, 500, 600, 700, 800 ) ispec
!
!     invalid value for ispec
!
      ilaenv = -1
      return
!
  100 continue
!
!     convert name to upper case if the first character is lower case.
!
      ilaenv = 1
      subnam = name
      ic = mova2i( subnam( 1:1 ) )
      iz = mova2i( 'z' )
      if( iz.eq.90 .or. iz.eq.122 ) then
!
!        ascii character set
!
         if( ic.ge.97 .and. ic.le.122 ) then
            subnam( 1:1 ) = char( ic-32 )
            do 10 i = 2, 6
               ic = mova2i( subnam( i:i ) )
               if( ic.ge.97 .and. ic.le.122 )                           &
     &            subnam( i:i ) = char( ic-32 )
   10       continue
         end if
!
      else if( iz.eq.233 .or. iz.eq.169 ) then
!
!        ebcdic character set
!
         if( ( ic.ge.129 .and. ic.le.137 ) .or.                         &
     &       ( ic.ge.145 .and. ic.le.153 ) .or.                         &
     &       ( ic.ge.162 .and. ic.le.169 ) ) then
            subnam( 1:1 ) = char( ic+64 )
            do 20 i = 2, 6
               ic = mova2i( subnam( i:i ) )
               if( ( ic.ge.129 .and. ic.le.137 ) .or.                   &
     &             ( ic.ge.145 .and. ic.le.153 ) .or.                   &
     &             ( ic.ge.162 .and. ic.le.169 ) )
     &            subnam( i:i ) = char( ic+64 )
   20       continue
         end if
!
      else if( iz.eq.218 .or. iz.eq.250 ) then
!
!        prime machines:  ascii+128
!
         if( ic.ge.225 .and. ic.le.250 ) then
            subnam( 1:1 ) = char( ic-32 )
            do 30 i = 2, 6
               ic = mova2i( subnam( i:i ) )
               if( ic.ge.225 .and. ic.le.250 )                          &
     &            subnam( i:i ) = char( ic-32 )
   30       continue
         end if
      end if
!
      c1 = subnam( 1:1 )
      sname = c1.eq.'s' .or. c1.eq.'d'
      cname = c1.eq.'c' .or. c1.eq.'z'
      if( .not.( cname .or. sname ) )                                   &
     &   return
      c2 = subnam( 2:3 )
      c3 = subnam( 4:6 )
      c4 = c3( 2:3 )
!
      go to ( 110, 200, 300 ) ispec
!
  110 continue
!
!     ispec = 1:  block size
!
!     in these examples, separate code is provided for setting nb for
!     real and complex.  we assume that nb will take the same value in
!     single or double precision.
!
      nb = 1
!
      if( c2.eq.'ge' ) then
         if( c3.eq.'trf' ) then
            if( sname ) then
               nb = 64
            else
               nb = 64
            end if
         else if( c3.eq.'qrf' .or. c3.eq.'rqf' .or. c3.eq.'lqf' .or.         &
     &            c3.eq.'qlf' ) then
            if( sname ) then
               nb = 32
            else
               nb = 32
            end if
         else if( c3.eq.'hrd' ) then
            if( sname ) then
               nb = 32
            else
               nb = 32
            end if
         else if( c3.eq.'brd' ) then
            if( sname ) then
               nb = 32
            else
               nb = 32
            end if
         else if( c3.eq.'tri' ) then
            if( sname ) then
               nb = 64
            else
               nb = 64
            end if
         end if
      else if( c2.eq.'po' ) then
         if( c3.eq.'trf' ) then
            if( sname ) then
               nb = 64
            else
               nb = 64
            end if
         end if
      else if( c2.eq.'sy' ) then
         if( c3.eq.'trf' ) then
            if( sname ) then
               nb = 64
            else
               nb = 64
            end if
         else if( sname .and. c3.eq.'trd' ) then
            nb = 1
         else if( sname .and. c3.eq.'gst' ) then
            nb = 64
         end if
      else if( cname .and. c2.eq.'he' ) then
         if( c3.eq.'trf' ) then
            nb = 64
         else if( c3.eq.'trd' ) then
            nb = 1
         else if( c3.eq.'gst' ) then
            nb = 64
         end if
      else if( sname .and. c2.eq.'or' ) then
         if( c3( 1:1 ).eq.'g' ) then
            if( c4.eq.'qr' .or. c4.eq.'rq' .or. c4.eq.'lq' .or.         &
     &          c4.eq.'ql' .or. c4.eq.'hr' .or. c4.eq.'tr' .or.         &
     &          c4.eq.'br' ) then
               nb = 32
            end if
         else if( c3( 1:1 ).eq.'m' ) then
            if( c4.eq.'qr' .or. c4.eq.'rq' .or. c4.eq.'lq' .or.         &
     &          c4.eq.'ql' .or. c4.eq.'hr' .or. c4.eq.'tr' .or.         &
     &          c4.eq.'br' ) then
               nb = 32
            end if
         end if
      else if( cname .and. c2.eq.'un' ) then
         if( c3( 1:1 ).eq.'g' ) then
            if( c4.eq.'qr' .or. c4.eq.'rq' .or. c4.eq.'lq' .or.         &
     &          c4.eq.'ql' .or. c4.eq.'hr' .or. c4.eq.'tr' .or.         &
     &          c4.eq.'br' ) then
               nb = 32
            end if
         else if( c3( 1:1 ).eq.'m' ) then
            if( c4.eq.'qr' .or. c4.eq.'rq' .or. c4.eq.'lq' .or.         &
     &          c4.eq.'ql' .or. c4.eq.'hr' .or. c4.eq.'tr' .or.         &
     &          c4.eq.'br' ) then
               nb = 32
            end if
         end if
      else if( c2.eq.'gb' ) then
         if( c3.eq.'trf' ) then
            if( sname ) then
               if( n4.le.64 ) then
                  nb = 1
               else
                  nb = 32
               end if
            else
               if( n4.le.64 ) then
                  nb = 1
               else
                  nb = 32
               end if
            end if
         end if
      else if( c2.eq.'pb' ) then
         if( c3.eq.'trf' ) then
            if( sname ) then
               if( n2.le.64 ) then
                  nb = 1
               else
                  nb = 32
               end if
            else
               if( n2.le.64 ) then
                  nb = 1
               else
                  nb = 32
               end if
            end if
         end if
      else if( c2.eq.'tr' ) then
         if( c3.eq.'tri' ) then
            if( sname ) then
               nb = 64
            else
               nb = 64
            end if
         end if
      else if( c2.eq.'la' ) then
         if( c3.eq.'uum' ) then
            if( sname ) then
               nb = 64
            else
               nb = 64
            end if
         end if
      else if( sname .and. c2.eq.'st' ) then
         if( c3.eq.'ebz' ) then
            nb = 1
         end if
      end if
      ilaenv = nb
      return
!
  200 continue
!
!     ispec = 2:  minimum block size
!
      nbmin = 2
      if( c2.eq.'ge' ) then
         if( c3.eq.'qrf' .or. c3.eq.'rqf' .or. c3.eq.'lqf' .or.         &
     &       c3.eq.'qlf' ) then
            if( sname ) then
               nbmin = 2
            else
               nbmin = 2
            end if
         else if( c3.eq.'hrd' ) then
            if( sname ) then
               nbmin = 2
            else
               nbmin = 2
            end if
         else if( c3.eq.'brd' ) then
            if( sname ) then
               nbmin = 2
            else
               nbmin = 2
            end if
         else if( c3.eq.'tri' ) then
            if( sname ) then
               nbmin = 2
            else
               nbmin = 2
            end if
         end if
      else if( c2.eq.'sy' ) then
         if( c3.eq.'trf' ) then
            if( sname ) then
               nbmin = 8
            else
               nbmin = 8
            end if
         else if( sname .and. c3.eq.'trd' ) then
            nbmin = 2
         end if
      else if( cname .and. c2.eq.'he' ) then
         if( c3.eq.'trd' ) then
            nbmin = 2
         end if
      else if( sname .and. c2.eq.'or' ) then
         if( c3( 1:1 ).eq.'g' ) then
            if( c4.eq.'qr' .or. c4.eq.'rq' .or. c4.eq.'lq' .or.         &
     &          c4.eq.'ql' .or. c4.eq.'hr' .or. c4.eq.'tr' .or.         &
     &          c4.eq.'br' ) then
               nbmin = 2
            end if
         else if( c3( 1:1 ).eq.'m' ) then
            if( c4.eq.'qr' .or. c4.eq.'rq' .or. c4.eq.'lq' .or.         &
     &          c4.eq.'ql' .or. c4.eq.'hr' .or. c4.eq.'tr' .or.         &
     &          c4.eq.'br' ) then
               nbmin = 2
            end if
         end if
      else if( cname .and. c2.eq.'un' ) then
         if( c3( 1:1 ).eq.'g' ) then
            if( c4.eq.'qr' .or. c4.eq.'rq' .or. c4.eq.'lq' .or.         &
     &          c4.eq.'ql' .or. c4.eq.'hr' .or. c4.eq.'tr' .or.         &
     &          c4.eq.'br' ) then
               nbmin = 2
            end if
         else if( c3( 1:1 ).eq.'m' ) then
            if( c4.eq.'qr' .or. c4.eq.'rq' .or. c4.eq.'lq' .or.         &
     &          c4.eq.'ql' .or. c4.eq.'hr' .or. c4.eq.'tr' .or.         &
     &          c4.eq.'br' ) then
               nbmin = 2
            end if
         end if
      end if
      ilaenv = nbmin
      return
!
  300 continue
!
!     ispec = 3:  crossover point
!
      nx = 0
      if( c2.eq.'ge' ) then
         if( c3.eq.'qrf' .or. c3.eq.'rqf' .or. c3.eq.'lqf' .or.         &
     &       c3.eq.'qlf' ) then
            if( sname ) then
               nx = 128
            else
               nx = 128
            end if
         else if( c3.eq.'hrd' ) then
            if( sname ) then
               nx = 128
            else
               nx = 128
            end if
         else if( c3.eq.'brd' ) then
            if( sname ) then
               nx = 128
            else
               nx = 128
            end if
         end if
      else if( c2.eq.'sy' ) then
         if( sname .and. c3.eq.'trd' ) then
            nx = 1
         end if
      else if( cname .and. c2.eq.'he' ) then
         if( c3.eq.'trd' ) then
            nx = 1
         end if
      else if( sname .and. c2.eq.'or' ) then
         if( c3( 1:1 ).eq.'g' ) then
            if( c4.eq.'qr' .or. c4.eq.'rq' .or. c4.eq.'lq' .or.         &
     &          c4.eq.'ql' .or. c4.eq.'hr' .or. c4.eq.'tr' .or.         &
     &          c4.eq.'br' ) then
               nx = 128
            end if
         end if
      else if( cname .and. c2.eq.'un' ) then
         if( c3( 1:1 ).eq.'g' ) then
            if( c4.eq.'qr' .or. c4.eq.'rq' .or. c4.eq.'lq' .or.         &
     &          c4.eq.'ql' .or. c4.eq.'hr' .or. c4.eq.'tr' .or.         &
     &          c4.eq.'br' ) then
               nx = 128
            end if
         end if
      end if
      ilaenv = nx
      return
!
  400 continue
!
!     ispec = 4:  number of shifts (used by xhseqr)
!
      ilaenv = 6
      return
!
  500 continue
!
!     ispec = 5:  minimum column dimension (not used)
!
      ilaenv = 2
      return
!
  600 continue 
!
!     ispec = 6:  crossover point for svd (used by xgelss and xgesvd)
!
      ilaenv = int( real( min( n1, n2 ) )*1.6e0 )
      return
!
  700 continue
!
!     ispec = 7:  number of processors (not used)
!
      ilaenv = 1
      return
!
  800 continue
!
!     ispec = 8:  crossover point for multishift (used by xhseqr)
!
      ilaenv = 50
      return
!
!     end of ilaenv
!
      end





      logical          function lsame( ca, cb )
!
!  -- lapack auxiliary routine (version 2.0) --
!     univ. of tennessee, univ. of california berkeley, nag ltd.,
!     courant institute, argonne national lab, and rice university
!     september 30, 1994
!
!     .. scalar arguments ..
      character          ca, cb
!     ..
!
!  purpose
!  =======
!
!  lsame returns .true. if ca is the same letter as cb regardless of
!  case.
!
!  arguments
!  =========
!
!  ca      (input) character*1
!  cb      (input) character*1
!          ca and cb specify the single characters to be compared.
!
! =====================================================================
!
!     .. intrinsic functions ..
      intrinsic          ichar
!     ..
!     .. local scalars ..
      integer            inta, intb, zcode
!     ..
!     .. executable statements ..
!
!     test if the characters are equal
!
      lsame = ca.eq.cb
      if( lsame )                                                       &
     &   return
!
!     now test for equivalence if both characters are alphabetic.
!
      zcode = mova2i( 'z' )
!
!     use 'z' rather than 'a' so that ascii can be detected on prime
!     machines, on which ichar returns a value with bit 8 set.
!     mova2i('a') on prime machines returns 193 which is the same as
!     mova2i('a') on an ebcdic machine.
!
      inta = mova2i( ca )
      intb = mova2i( cb )
!
      if( zcode.eq.90 .or. zcode.eq.122 ) then
!
!        ascii is assumed - zcode is the ascii code of either lower or
!        upper case 'z'.
!
         if( inta.ge.97 .and. inta.le.122 ) inta = inta - 32
         if( intb.ge.97 .and. intb.le.122 ) intb = intb - 32
!
      else if( zcode.eq.233 .or. zcode.eq.169 ) then
!
!        ebcdic is assumed - zcode is the ebcdic code of either lower or
!        upper case 'z'.
!
         if( inta.ge.129 .and. inta.le.137 .or.                         &
     &       inta.ge.145 .and. inta.le.153 .or.                         &
     &       inta.ge.162 .and. inta.le.169 ) inta = inta + 64
         if( intb.ge.129 .and. intb.le.137 .or.                         &
     &       intb.ge.145 .and. intb.le.153 .or.                         &
     &       intb.ge.162 .and. intb.le.169 ) intb = intb + 64
!
      else if( zcode.eq.218 .or. zcode.eq.250 ) then
!
!        ascii is assumed, on prime machines - zcode is the ascii code
!        plus 128 of either lower or upper case 'z'.
!
         if( inta.ge.225 .and. inta.le.250 ) inta = inta - 32
         if( intb.ge.225 .and. intb.le.250 ) intb = intb - 32
      end if
      lsame = inta.eq.intb
!
!     return
!
!     end of lsame
!
      end



      integer function isamax(n,sx,incx)
!
!     finds the index of element having max. absolute value.
!     jack dongarra, linpack, 3/11/78.
!     modified 3/93 to return if incx .le. 0.
!     modified 12/3/93, array(1) declarations changed to array(*)
!
      real sx(*),smax
      integer i,incx,ix,n
!
      isamax = 0
      if( n.lt.1 .or. incx.le.0 ) return
      isamax = 1
      if(n.eq.1)return
      if(incx.eq.1)go to 20
!
!        code for increment not equal to 1
!
      ix = 1
      smax = abs(sx(1))
      ix = ix + incx
      do 10 i = 2,n
         if(abs(sx(ix)).le.smax) go to 5
         isamax = i
         smax = abs(sx(ix))
    5    ix = ix + incx
   10 continue
      return
!
!        code for increment equal to 1
!
   20 smax = abs(sx(1))
      do 30 i = 2,n
         if(abs(sx(i)).le.smax) go to 30
         isamax = i
         smax = abs(sx(i))
   30 continue
      return
      end


      subroutine dcopy(n,sx,incx,sy,incy)
!
!     copies a vector, x, to a vector, y.
!     uses unrolled loops for increments equal to 1.
!     jack dongarra, linpack, 3/11/78.
!     modified 12/3/93, array(1) declarations changed to array(*)
!
      real sx(*),sy(*)
      integer i,incx,incy,ix,iy,m,mp1,n
!
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
!
!        code for unequal increments or equal increments
!          not equal to 1
!
      ix = 1
      iy = 1
      if(incx.lt.0)ix = (-n+1)*incx + 1
      if(incy.lt.0)iy = (-n+1)*incy + 1
      do 10 i = 1,n
        sy(iy) = sx(ix)
        ix = ix + incx
        iy = iy + incy
   10 continue
      return
!
!        code for both increments equal to 1
!
!
!        clean-up loop
!
   20 m = mod(n,7)
      if( m .eq. 0 ) go to 40
      do 30 i = 1,m
        sy(i) = sx(i)
   30 continue
      if( n .lt. 7 ) return
   40 mp1 = m + 1
      do 50 i = mp1,n,7
        sy(i) = sx(i)
        sy(i + 1) = sx(i + 1)
        sy(i + 2) = sx(i + 2)
        sy(i + 3) = sx(i + 3)
        sy(i + 4) = sx(i + 4)
        sy(i + 5) = sx(i + 5)
        sy(i + 6) = sx(i + 6)
   50 continue
      return
      end
