
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
            if( j.lt.n )                                               &
     &         call dgemv( 'no transpose', n, n-j, -one, a( 1, j+1 ),  &
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
            call dtrsm( 'right', 'lower', 'no transpose', 'unit', n, jb,&
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


      integer          function ilaenv( ispec, name, opts, n1, n2, n3,   &
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
     &             ( ic.ge.162 .and. ic.le.169 ) )                      &
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
         else if( c3.eq.'qrf' .or. c3.eq.'rqf' .or. c3.eq.'lqf' .or.    &
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

