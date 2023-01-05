module mlef_module
!=======================================================================
!
! [PURPOSE:] Maximum Likelihood Ensemble Filer (MLEF) 
!            with observation space localization
!            Model Independent Core Module
!
! [REFERENCES:]
!  [1] Zupanski, 2005: Maximum Likelihood Ensemble Filter: 
!    Theoretical Aspects., Monthly Weather Review, 133(6), 1710-1726.
!  [2] Zupanski, 2021: The Maximum Likelihood Ensemble Filter with 
!    State Space Localization. Appendix A, Monthly Weather Review, 
!    149(10), 3505-3524.
!  [3] Yokota et al., 2016: Comparison between Four-Dimensional LETKF
!    and Ensemble-Based Variational Data Assimilation with Observational
!    Localization., SOLA, 12, 80-85.
!
! [HISTORY:]
!  10/14/2021 Saori Nakashita  Created at Kyoto University, Japan.
!  06/02/2022 Yokota form integrated
!
! [CAUTION:]
!  This module requires cgsearch.f
!=======================================================================
!$USE OMP_LIB
  use kind_module
  use mtx_module
  use nml_module

  implicit none

  private

  ! NAME LIST
  logical, save :: debug = .FALSE.
  logical, save :: jout = .FALSE.
  integer, save :: opt = 0
  ! opt : optimization method in mlef_core
  ! opt=0 : Newton
  ! opt>0 : conjugate gradient (with preconditioning)
  ! opt<0 : conjugate gradient (without preconditioning)
  integer, save :: ls = 0
  ! ls : line search type
  ! ls=0 : More-Thuente (cgsearch.f)
  ! for mlef_core
  ! ls>0 : quadratic interpolation (contained in this module)
  ! ls<0 : step-size is fixed to 1
  ! for line_search
  ! ls>0 : backtracking (contained in this module)
  ! ls<0 : deterministic step-size
  logical, save :: ngauss = .FALSE.
  ! ngauss : background term type
  ! ngauss=.FALSE. : Gaussian
  ! ngauss=.TRUE.  : non-Gausian
  NAMELIST /NAMLST_COMMLEF/ debug, jout, ls, opt, ngauss
  ! PARAMETER AND ARRAYS FOR MINIMIZATION
  real(kind=dp),parameter :: glim=1.0d-5
  real(kind=dp),allocatable,save :: grad(:)
  real(kind=dp),allocatable,save :: gold(:)
  real(kind=dp),allocatable,save :: desc(:)
  real(kind=dp),allocatable,save :: dold(:)
  real(kind=dp),allocatable,save :: wa(:)
  real(kind=dp),allocatable,save :: trans0(:,:) ! for preconditioning
  ! PARAMETER FOR LINE SEARCH ROUTINE (CVSMOD)
  integer,parameter      :: maxfev=40 !!default
  !integer,parameter      :: maxfev=10
  real(kind=dp),parameter :: ftol=1.0d-4, xtol=1.0d-17
  real(kind=dp),save      :: gtol=9.0d-1
  real(kind=dp),parameter :: stpmin=1.0d-20, stpmax=1.0d20
  ! for debug
  character(len=4) :: cn
  character(len=14) :: cform

  PUBLIC :: debug, jout, mlef_init, &
  & mlef_core, mlefy_core, line_search, calc_trans, &
  & est_infl, solve_trisystem
contains
subroutine mlef_init
  implicit none

  rewind(5)
  read (5,NAMLST_COMMLEF)
  write(6,NAMLST_COMMLEF)
  if(opt/=0) then
    gtol=1.0d-1 !Nocedal and Wright (2006,Chapter 3,pp.34)
  end if
!  allocate( grad(member) )
!  allocate( gold(member) )
!  allocate( desc(member) )
!  allocate( wa(member) )
  return
end subroutine mlef_init
!=======================================================================
!  Main Subroutine of MLEF Core
!   INPUT
!     ne               : ensemble size                                           
!     nobs             : array size, but only first nobsl elements are used
!     nobsl            : total number of observation assimilated at the point
!     nfun             : function evaluation counter
!     zxb(nobs,ne)     : Z matrix (=R^{-1/2}@[h(x+pi)-h(x)]_{i=1,...,ne})
!     dep(nobs)        : normalized observation departure R^{-1/2}(yo-Hx)
!     parm_infl        : covariance inflation parameter
!     alpha            : previous step length
!     wupd(ne)         : previous control vector
!   OUTPUT
!     alpha            : current step length
!     wupd(ne)         : updated control vector
!     flag             : convergence flag (0=convergence)
!=======================================================================
!  Gaussian form
!  Cost Function
!    J(w) = w^Tw/(1+parm_infl)/2 + dep^Tdep/2
!  Gradient
!    \nabla_w J = w/(1+parm_infl) - Z^Tdep
!  Hessian
!    \nabla^2_w J = I/(1+parm_infl) + Z^TZ
!=======================================================================
!  Non-Gaussian form
!  w2 = dot(w,w)^(1/2)
!  Cost Function
!    J(w) = w^Tw/(1+parm_infl*w2)/2 + dep^Tdep/2
!  Gradient
!    \nabla_w J = w*(2+parm_infl*w2)/(1+parm_infl*w2)**2/2
!                 - Z^Tdep
!  Hessian
!    [\nabla^2_w J]_{ij}
!     = [(2+parm_infl*w2)/2/(1+parm_infl*w2)**2
!      + parm_infl*wj*wi/(1+parm_infl*w2)**2/2/w2 
!      - (2+parm_infl*w2)*parm_infl*wj*wi/(1+parm_infl*w2)**3/w2] + Z^TZ (i=j)
!     = [parm_infl*wj*wi/(1+parm_infl*w2)**2/2/w2 
!      - (2+parm_infl*w2)*parm_infl*wj*wi/(1+parm_infl*w2)**3/w2] + Z^TZ (i/=j)
!=======================================================================
subroutine mlef_core(ne,nobs,nobsl,nfun,zxb,dep,parm_infl,alpha,wupd,flag&
                   & ,jb,jo,gnorm_out)
!                   & ,trans,jb,jo,gnorm_out)
  implicit none
  integer, intent(in) :: ne 
  integer, intent(in) :: nobs
  integer, intent(in) :: nobsl
  integer, intent(inout) :: nfun
  real(kind=dp), intent(in) :: zxb(1:nobs,1:ne)
  real(kind=dp), intent(in) :: dep(1:nobs)
  real(kind=dp), intent(inout) :: parm_infl
  real(kind=dp), intent(inout) :: alpha
  real(kind=dp), intent(inout) :: wupd(ne)
  integer, intent(inout)        :: flag
  ! input
  !   flag = 1 : calculate new descent direction
  !   flag = -1 : during line search
  !   flag = -2 : only calculate cost
  ! output
  !   flag = 0 : convergence
  !   flag != 0 : depends on line search routine
!  real(kind=dp),optional,intent(inout) :: trans(1:ne,1:ne)
  real(kind=dp),optional,intent(out) :: jb
  real(kind=dp),optional,intent(out) :: jo
  real(kind=dp),optional,intent(out) :: gnorm_out
  
  real(kind=dp) :: fval
  real(kind=dp),allocatable :: hess(:,:)
  real(kind=dp),allocatable :: gmat(:,:)
  real(kind=dp),allocatable :: work1(:), work2(:)
  real(kind=dp),allocatable :: work(:,:)
  !integer,allocatable :: ipiv(:)
  real(kind=dp) :: gnorm, tmp, rho
  real(kind=dp) :: goldnorm, beta !for conjugate gradient
  real(kind=dp) :: w2, wij, cij
  integer      :: i,j,info
  !integer      :: k,lwork 
  ! for line search routine
  integer,save :: nfev, linfo
  real(kind=dp),save :: dginit, dgout, dgold
  
  !flag = -1
  !if (debug.or.jout) then
    write(cn,'(I4)') ne
    cform = '(A,'//trim(cn)//'es12.4)'
  !endif
  !if( jout ) write(6,'(A, i4)') 'iteration ', niter
  !niter = niter + 1
  if(nobsl == 0) then
    if(debug) write(6,'(A)') 'No observation to be assimilated'
    if(present(jb)) jb=0.0d0
    if(present(jo)) jo=0.0d0
    if(present(gnorm_out)) gnorm_out=0.0d0
    flag = 0
    return
  end if
  if (debug) then
    print '(A,I3)', 'flag=', flag
    print cform, 'w', wupd
  end if
  if(ngauss) then
    w2 = 0.0_dp
    do i=1, ne
      w2 = w2 + wupd(i)*wupd(i)
    end do
    w2 = sqrt(w2)
    if(debug) print '(A,es12.4)', 'w2', w2
    rho = 1.0_dp / (1.0_dp + parm_infl*w2)
  else 
    rho = 1.0_dp / (1.0_dp + parm_infl)
  end if
  if(debug) print '(A,es12.4)', 'rho', rho
  fval = 0.0_dp
  if (nfun == 0) then
    dgout = 0.0_dp
    if (flag == 1) then !FIRST
      if (allocateD(grad)) deallocate(grad)
      allocate( grad(ne) )
      if (allocateD(gold)) deallocate(gold)
      allocate( gold(ne) )
      if (allocateD(desc)) deallocate(desc)
      allocate( desc(ne) )
      if (allocateD(dold)) deallocate(dold)
      allocate( dold(ne) )
      if (allocateD(wa)) deallocate(wa)
      allocate( wa(ne) )
      if (opt>0) then
        if (allocateD(trans0)) deallocate(trans0)
        allocate( trans0(ne,ne) )
        trans0 = 0.0_dp
        do i=1,ne
          trans0(i,i) = 1.0_dp
        end do
      end if
    end if
  end if
!-----------------------------------------------------------------------
!  Jb = w^T  @ w / 2 / parm_infl (covariance inflation)
!-----------------------------------------------------------------------
  do i = 1, ne 
    fval = fval + wupd(i) * wupd(i)
  end do
  fval = fval * 0.5_dp * rho
  if(present(jb)) then
    jb = fval
  end if
  if( jout ) write(6,'(A, es12.4)') 'Jb   ', fval
!-----------------------------------------------------------------------
!  Jo = dep^Tdep/2
!-----------------------------------------------------------------------
  tmp = 0.0_dp
  do i = 1, nobsl
    tmp = tmp + dep(i) * dep(i)
  end do
  tmp = tmp * 0.5_dp
  if(present(jo)) then
    jo = tmp
  end if
  if( jout ) write(6,'(A, es12.4)') 'Jo   ', tmp
  fval = fval + tmp
  if( jout ) write(6,'(A, es12.4)') 'Jall ', fval
  if( isnan(fval) ) then
    STOP 'Divergence'
  end if
  if( flag == -2 ) then
    flag = 0
    return
  end if
!-----------------------------------------------------------------------
! compute gradient
!-----------------------------------------------------------------------
!  grad = w/(1+parm_infl) - Z^T @ dep (Gauss)
!       = w*(2+parm_infl*w2)/(1+parm_infl*w2)**2/2 - Z^T @ dep (non-Gauss)
!-----------------------------------------------------------------------
  allocate( work1(ne), work2(nobsl) )
  if(ngauss) then
    rho = (2.0_dp + parm_infl*w2)/2.0_dp/(1.0_dp + parm_infl*w2)**2
  end if
  if(debug) print '(A,es12.4)', 'rho', rho
  work1 = wupd * rho
  work2 = dep
  call dgemv('t',nobsl,ne,-1.0_dp,zxb(1:nobsl,:),nobsl,work2,1,1.0_dp,work1,1)
  grad = work1
  if(opt>0) then
    work1 = wupd
    call dgemv('t',ne,ne,1.0_dp,trans0,ne,work1,1,0.0_dp,wupd,1)
    call solve_trisystem('l',ne,trans0,work1)
  end if
  if (debug) then
    print cform, 'grad', grad
  end if
  gnorm = 0.0_dp
  do i = 1, ne
    gnorm = gnorm + grad(i)*grad(i)
    !gnorm = gnorm + work1(i)*work1(i)
  end do
  gnorm = sqrt(gnorm)
  if (debug) then
    print '(A,es12.4)', 'gnorm ', gnorm
  end if
  if (present(gnorm_out)) then
    gnorm_out = gnorm
  end if
  if (gnorm < glim) then ! converge
    if (jout) then
      write(6,'(A, es12.4)') 'convergence, |g|=', gnorm
      write(6,'(A, es12.4)') 'final Jb   ', fval - tmp
      write(6,'(A, es12.4)') 'final Jo   ', tmp
    end if
    flag = 0
    deallocate( grad, gold, desc, wa )
    deallocate( work1,work2 )
    return
  end if
if (flag == 1) then
!-----------------------------------------------------------------------
!  update descent direction 
!-----------------------------------------------------------------------
  if ((opt==0).or.((opt>0).and.(nfun==0))) then
!-----------------------------------------------------------------------
!  for Newton(opt=0) and first preconditioned CG(opt>0)
!  solve Newton equation : desc = -(hess)^{-1}grad
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!  hess = I / (1+parm_infl) + zxb^T @ zxb (Gauss)
!  hess_{ij} 
!  = [(2+parm_infl*w2)/2/(1+parm_infl*w2)**2
!   + parm_infl*wj*wi/(1+parm_infl*w2)**2/2/w2 
!   - (2+parm_infl*w2)*parm_infl*wj*wi/(1+parm_infl*w2)**3/w2] + zxb^T @ zxb (i=j)
!  = [parm_infl*wj*wi/(1+parm_infl*w2)**2/2/w2 
!   - (2+parm_infl*w2)*parm_infl*wj*wi/(1+parm_infl*w2)**3/w2] + zxb^T @ zxb (i/=j)
!  (non-Gauss)
!-----------------------------------------------------------------------
    allocate( hess(ne,ne) )
    hess = 0.0_dp
    if(ngauss) then
      if(w2 > 1.0e-16) then
        cij = parm_infl*(3.0_dp+parm_infl*w2)/w2/2.0_dp/(1.0_dp + parm_infl*w2)**3
      else 
        cij = 0.0_dp
      end if
      if(debug) print '(A,es12.4)', 'cij', cij
      do j=1,ne 
        hess(j,j) = rho
        do i=j,ne 
          wij = wupd(i)*wupd(j)
          hess(i,j) = hess(i,j) - cij*wij
          hess(j,i) = hess(i,j)
        end do
      end do
    else
      do i = 1, ne 
        hess(i,i) = rho
      end do
    end if
    call dgemm('t','n',ne,ne,nobsl,1.0_dp,zxb(1:nobsl,:),nobsl,zxb(1:nobsl,:),&
      & nobsl,1.0_dp,hess,ne)
    if (debug) then
      print '(A)', 'hess'
      do i=1,ne
        print '(i2,'//trim(cn)//'es12.4)', i, hess(i,:)
      enddo
    end if
!-----------------------------------------------------------------------
!  Cholesky decomposition of Hessian : hess = gmat @ gmat^T
!  where gmat is lower triangular matrix
!-----------------------------------------------------------------------
    allocate( gmat(ne,ne), work(ne,ne) )
    gmat(:,:) = hess(:,:)
    call dpotrf('l',ne,gmat,ne,info)
    do j = 2, ne
      do i = 1, j-1
        gmat(i,j) = 0.0_dp
      end do
    end do
    if (debug) then
      print '(A, i1)', 'INFO=', info
      call dgemm('n','t',ne,ne,ne,1.0_dp,gmat,ne,gmat,ne,0.0_dp,work,ne)
      print '(A)', 'gmat @ gmat^T'
      do i=1,ne
        print '(i2,'//trim(cn)//'es12.4)', i, work(i,:)
      enddo
    end if
    if (opt>0) then ! (inverse of) preconditioning matrix
      trans0(:,:) = gmat(:,:)
    end if
!    if (present(trans)) then ! ensemble tranformation matrix=(gmat)^{-T}
!      work=0.0_dp
!      do j=1,ne
!        do i=1,ne
!          work(i,j) = gmat(j,i)
!        end do
!      end do
!      call mtx_inv_rg(ne,work,trans)
!    end if
!-----------------------------------------------------------------------
!  solve linear system
!  hess @ desc = (gmat @ gmat^T) @ desc = -grad
!  ->   gmat @ q    = -grad (gmat is lower triangular matrix)
!     gmat^T @ desc = q   (gmat^T is upper triangular matrix)
!-----------------------------------------------------------------------
    !desc = -1.0_dp*work1
    desc = -1.0_dp*grad
    if (debug) then
      print cform, '-grad', desc
    end if
    work = 0.0_dp
    do j = 1, ne
      do i = j, ne
        work(j,i) = gmat(i,j)
      end do
    end do
    if (debug) then
      print '(A)', 'gmat'
      do i=1,ne
        print '(i2,'//trim(cn)//'es12.4)', i, gmat(i,:)
      enddo
      print '(A)', 'gmat^T'
      do i=1,ne
        print '(i2,'//trim(cn)//'es12.4)', i, work(i,:)
      enddo
    end if
    call solve_trisystem('l',ne,gmat,desc)
    call solve_trisystem('u',ne,work,desc)
    !! using lapack
    !work(:,1) = -1.0_dp*grad
    !allocate( ipiv(ne) )
    !deallocate( work1 )
    !allocate( work1(1) )
    !call dsysv('l',ne,1,hess,ne,ipiv,work,ne,work1,-1,info)
    !lwork = int(work1(1))
    !deallocate( work1 )
    !allocate( work1(lwork) )
    !call dsysv('l',ne,1,hess,ne,ipiv,work,ne,work1,lwork,info)
    !desc(:) = work(:,1)
    !if (debug) print '(A, i1)', 'INFO=', info
    deallocate( hess,gmat,work )
    deallocate( work1,work2 )
!-----------------------------------------------------------------------
  else
!-----------------------------------------------------------------------
!  conjugate gradient method (Fletcher and Reeves, 1964)
!  beta = (grad^T grad) / (gold^T gold)
!  desc = -grad + beta * dold
!-----------------------------------------------------------------------
    goldnorm = 0.0_dp
    do i=1,ne 
      goldnorm = goldnorm + gold(i)*gold(i)
    end do
    if (debug) then
      print '(A,es12.4)', 'goldnorm ', sqrt(goldnorm)
    end if
    if ( nfun==0 ) then
      beta = 0.0_dp ! steepest descent (first iteration)
      alpha = 1.0_dp/sqrt(gnorm)
    else
      beta = gnorm * gnorm / goldnorm 
    end if
    desc = -grad + beta * dold 
  end if
  nfev = 0
  linfo = 0
  gold(:) = grad(:)
  dgold = dginit
  dginit = 0.0_dp
  do i=1,ne 
    dginit = dginit + desc(i)*grad(i)
  end do
  if (debug) then
    print '(A, es12.4)', 'dginit', dginit
  end if
end if
!-----------------------------------------------------------------------
!  calculate new step length and update control vector
!-----------------------------------------------------------------------
if (ls == 0) then
  ! More-Thuente
  if (debug) then
    print cform, 'desc', desc
  end if
  if ((opt/=0).and.(flag==1).and.(nfun>0)) then
    ! Shanno-Phua's formula for trial step
    alpha = alpha * dgold / dginit
  end if
  call CVSMOD(ne,wupd,fval,grad,desc,alpha,&
  & ftol,gtol,xtol,stpmin,stpmax, &
  & maxfev,linfo,nfev,wa,dginit,dgout)
  if (debug) then
    print '(A,I3)', 'linfo=', linfo
    print '(A,es12.4,x,A,es12.4)', 'dginit=', dginit, 'dgout=',dgout
  end if
  if (opt > 0) then
    allocate( work(ne,ne) )
    work = 0.0_dp
    do j = 1, ne
      do i = j, ne
        work(j,i) = trans0(i,j)
      end do
    end do
    call solve_trisystem('u',ne,work,wupd)
    deallocate( work )
  end if
  if (linfo .eq. -1) then
    ! return to reevaluate function and gradient
    flag = -1
    if (debug) then
      print '(A,I3)', 'flag=', flag
      print '(A, es12.4)', 'step length', alpha
      print cform, 'w', wupd
    end if
    return
  else if (linfo .eq. 0) then
    ! improper input parameters
    if (jout) print *, 'IMPROPER INPUT FOR CVSMOD'
    flag = -2
    deallocate( grad, gold, desc, wa )
    return
  else if (linfo .ge. 2) then
    if (jout) write(6,'(A,I3)') 'Line Search failed. INFO=',linfo
    flag = -2
    deallocate( grad, gold, desc, wa )
    return
  else ! line search success
    flag = 1
    nfun = nfun + nfev
    if (jout) then
      print '(A,I3)', 'flag=', flag
      print '(A, es12.4)', 'step length', alpha
      print cform, 'w', wupd
    end if
    return
  end if
else if( ls>0 ) then 
  ! quadratic interpolation
  flag = 1
  nfun = nfun + 1
  call quad_itpl(ne,nobsl,alpha,fval,wupd,desc,grad,zxb(1:nobsl,:),dep(1:nobsl),&
  & parm_infl)
  if (opt > 0) then
    allocate( work(ne,ne) )
    work = 0.0_dp
    do j = 1, ne
      do i = j, ne
        work(j,i) = trans0(i,j)
      end do
    end do
    call solve_trisystem('u',ne,work,wupd)
    deallocate( work ) 
  end if
  if (jout) then
    print '(A,I3)', 'flag=', flag
    print '(A, es12.4)', 'step length', alpha
    print cform, 'w', wupd
  end if
  return
else
  ! fixed to 1
  flag = 1
  alpha = 1.0D0
  do i = 1, ne
    wupd(i) = wupd(i) + alpha * desc(i)
  end do
  if (opt > 0) then
    allocate( work(ne,ne) )
    work = 0.0_dp
    do j = 1, ne
      do i = j, ne
        work(j,i) = trans0(i,j)
      end do
    end do
    call solve_trisystem('u',ne,work,wupd)
    deallocate( work ) 
  end if
  if (jout) then
    print '(A,I3)', 'flag=', flag
    print '(A, es12.4)', 'step length', alpha
    print cform, 'w', wupd
  end if
  return
end if
end subroutine mlef_core
!=======================================================================
!  Main Subroutine of MLEFY Core
!   INPUT
!     ne               : ensemble size                                           
!     nobs             : array size, but only first nobsl elements are used
!     nobsl            : total number of observation assimilated at the point
!     zxb(nobs,ne)     : Z matrix (=R^{-1/2}@[h(x+pi)-h(x)]_{i=1,...,ne})
!     dep(nobs)        : normalized observation departure R^{-1/2}(yo-Hx)
!     parm_infl        : covariance inflation parameter
!     wupd(ne)         : previous (local) control vector
!     gold(ne)         : previous (local) gradient
!     dold(ne)         : previous (local) search direction
!   OUTPUT
!     fval             : current (local) function value
!     grad(ne)         : current (local) gradient
!     desc(ne)         : current (local) search direction
!     flag             : convergence flag 
!          flag=0  : convergence
!          flag=1  : calculate new search direction
!          flag=-1 : calculate new function and gradient (under line search)
!=======================================================================
!  Cost Function
!    J(w) = w^Tw/2 + dep^Tdep/2
!  Gradient
!    \nabla_w J = w - Z^Tdep
!  Hessian
!    \nabla^2_w J = I + Z^TZ
!=======================================================================
subroutine mlefy_core(ne,nobs,nobsl,zxb,dep,wupd,parm_infl,fval,lgrad,lgold,ldesc,ldold,flag,jb,jo,gnorm_out)
  implicit none
  integer, intent(in) :: ne 
  integer, intent(in) :: nobs
  integer, intent(in) :: nobsl
  real(kind=dp), intent(in) :: zxb(1:nobs,1:ne)
  !real(kind=dp), intent(in) :: transb(1:ne,1:ne)
  real(kind=dp), intent(in) :: dep(1:nobs)
  real(kind=dp), intent(in) :: wupd(ne)
  real(kind=dp), intent(inout) :: parm_infl
  real(kind=dp), intent(inout) :: fval
  real(kind=dp), intent(inout) :: lgrad(ne)
  real(kind=dp), intent(inout) :: lgold(ne)
  real(kind=dp), intent(inout) :: ldesc(ne)
  real(kind=dp), intent(inout) :: ldold(ne)
  integer, intent(inout)      :: flag
  real(kind=dp),optional,intent(out) :: jb
  real(kind=dp),optional,intent(out) :: jo
  real(kind=dp),optional,intent(out) :: gnorm_out
  
  real(kind=dp),allocatable :: work1(:), work2(:)
!  real(kind=dp),allocatable :: hess(:,:)
!  real(kind=dp),allocatable :: gmat(:,:)
!  real(kind=dp),allocatable :: work(:,:)
  !integer,allocatable :: ipiv(:)
  real(kind=dp) :: gnorm, goldnorm, beta, tmp, rho
  integer      :: i
!  integer      :: j,info
  !integer      :: k,lwork 
  
  !flag = -1
  if (debug) then
    write(cn,'(I4)') ne
    cform = '(A,'//trim(cn)//'es12.4)'
  endif
  !if( jout ) write(6,'(A, i4)') 'iteration ', niter
  !niter = niter + 1
  if(nobsl == 0) then
    write(6,'(A)') 'No observation to be assimilated'
    flag = 0
    return
  end if
  if (debug) then
    print '(A,I3)', 'flag=', flag
    print cform, 'w', wupd
  end if
  rho = 1.0_dp / (1.0_dp + parm_infl)
  if(debug) print '(A,es12.4)', 'rho', rho
  fval = 0.0_dp
!-----------------------------------------------------------------------
!  Jb = w^T  @ w / 2 / parm_infl (covariance inflation)
!-----------------------------------------------------------------------
  do i = 1, ne 
    fval = fval + wupd(i) * wupd(i)
  end do
  fval = fval * 0.5_dp * rho
  if(present(jb)) then
    jb = fval
  end if
!-----------------------------------------------------------------------
!  Jo = dep^Tdep/2
!-----------------------------------------------------------------------
  tmp = 0.0_dp
  do i = 1, nobsl
    tmp = tmp + dep(i) * dep(i)
  end do
  tmp = tmp * 0.5_dp
  if(present(jo)) then
    jo = tmp
  end if
  if( debug ) then
    write(6,'(A, es12.4)') 'Jb   ', fval
    write(6,'(A, es12.4)') 'Jo   ', tmp
  end if
  fval = fval + tmp
  if( debug ) write(6,'(A, es12.4)') 'Jall ', fval
  !nfun = nfun + 1
  if( isnan(fval) ) then
    STOP 'Divergence'
  end if
!-----------------------------------------------------------------------
! compute gradient
!-----------------------------------------------------------------------
  allocate( work1(ne), work2(nobsl) )
  work1 = wupd * rho
  work2 = dep
!-----------------------------------------------------------------------
!  w / parm_infl - Z^T @ dep = grad (covariance inflation)
!-----------------------------------------------------------------------
  call dgemv('t',nobsl,ne,-1.0_dp,zxb(1:nobsl,:),nobsl,work2,1,1.0_dp,work1,1)
  lgrad = work1
  if (debug) then
    print cform, 'grad', lgrad
  end if
  gnorm = 0.0_dp
  do i = 1, ne
    gnorm = gnorm + work1(i)*work1(i)
  end do
  gnorm = sqrt(gnorm)
  if (debug) then
    print '(A,es12.4)', 'gnorm ', gnorm
  end if
  if (present(gnorm_out)) then
    gnorm_out = gnorm
  end if
  if (gnorm < glim) then ! converge
    if (debug) then
      write(6,'(A, es12.4)') 'convergence, |g|=', gnorm
      write(6,'(A, es12.4)') 'final Jb   ', fval - tmp
      write(6,'(A, es12.4)') 'final Jo   ', tmp
    end if
    flag = 0
    deallocate( work1,work2 )
    return
  end if
!-----------------------------------------------------------------------
!  update descent direction : conjugate gradient method (Fletcher and Reeves, 1964)
!  beta = (grad^T grad) / (gold^T gold)
!  desc = -grad + beta * dold
!-----------------------------------------------------------------------
  if (flag == 1) then
    goldnorm = 0.0_dp
    do i=1,ne 
      goldnorm = goldnorm + lgold(i)*lgold(i)
    end do
    if (debug) then
      print '(A,es12.4)', 'goldnorm ', sqrt(goldnorm)
    end if
    if ( goldnorm < 1e-16 ) then
      beta = 0.0_dp ! steepest descent (first iteration)
    else
      beta = gnorm * gnorm / goldnorm 
    end if
    ldesc = -lgrad + beta * ldold 
  end if
  return
end subroutine mlefy_core
!-----------------------------------------------------------------------
!  calculate new step length and update control vector
!   INPUT
!     ne               : control vector size                                           
!     nfun             : function evaluation counter
!     parm_infl        : covariance inflation parameter
!     alpha            : previous step length
!     wupd(ne)         : previous control vector
!     fval             : current (global) function value
!     grad(ne)         : current (global) gradient
!     desc(ne)         : current (global) search direction
!     gold(ne)         : previous (global) gradient
!     work(ne)         : working array
!   OUTPUT
!     alpha            : current step length
!     wupd(ne)         : updated control vector
!     flag             : result flag 
!          flag=0  : converge
!          flag=1  : return to calculate new search direction (line search success)
!          flag=-1 : return to calculate new function and gradient (under line search)
!          flag=-2 : line search failure
!-----------------------------------------------------------------------
subroutine line_search(ne,nfun,wupd,fval,ggrad,ggold,gdesc,gdold,work,alpha,dginit,dgout,flag)
  implicit none 
  integer, intent(in) :: ne 
  integer, intent(inout) :: nfun
  real(kind=dp), intent(inout) :: wupd(ne)
  real(kind=dp), intent(inout) :: fval
  real(kind=dp), intent(inout) :: ggrad(ne)
  real(kind=dp), intent(inout) :: ggold(ne)
  real(kind=dp), intent(inout) :: gdesc(ne)
  real(kind=dp), intent(inout) :: gdold(ne)
  real(kind=dp), intent(inout) :: work(ne)
  real(kind=dp), intent(inout) :: alpha
  real(kind=dp), intent(inout) :: dginit, dgout
  integer, intent(inout)      :: flag
  ! for line search routine
  integer,save :: nfev, linfo
  real(kind=dp),save :: dgold
  real(kind=dp) :: gnorm
  real(kind=dp),save :: fpre,fder,fderpre
  ! for backtracing
  real(kind=dp),parameter :: rhob=8.0d-1
  real(kind=dp) :: flh1,frh1,flh2,frh2
  ! common
  integer :: i, nx
  
  ! gradient norm check
  gnorm = 0.0_dp
  do i=1,ne 
    gnorm = gnorm + ggrad(i)*ggrad(i)
  end do
  !if(debug) then
    write(6,'(A,i4,A,es12.4)') 'flag=',flag,' |g|=', gnorm
  !end if
  if (gnorm < glim) then ! converge
    if (jout) then
      write(6,'(A, es12.4)') 'convergence, |g|=', gnorm
    end if
    flag = 0
    return
  end if
if (ls == 0) then
  ! More-Thuente
  if (nfun == 0) then ! first trial
    dgout = 0.0_dp
    alpha = 1.0_dp/sqrt(gnorm)
    !alpha = 1.0d-2
    nfun = nfun + 1
  end if
  if (flag == 1) then
    nfev = 0
    linfo = 0
    ggold(:) = ggrad(:)
    gdold(:) = gdesc(:)
    dginit = 0.0_dp
    do i=1,ne 
      dginit = dginit + gdesc(i)*ggrad(i)
    end do
    if (nfun > 1) then
      ! Shanno-Phua's formula for trial step
      alpha = alpha * dgold / dginit
    end if
    dgold = dginit
    if (jout) then
      print '(A, es12.4)', 'dginit', dginit
      print '(A, es12.4)', 'trial step', alpha
    end if
  end if
  if (debug) then
    nx = min(100, ne / member)
    do i=1,nx
      print cform, 'desc', gdesc((i-1)*member+1:i*member)
    end do
  end if
  if (debug) then
    print '(A,I3)', 'linfo(in)=', linfo
  end if
  call CVSMOD(ne,wupd,fval,ggrad,gdesc,alpha,&
  & ftol,gtol,xtol,stpmin,stpmax, &
  & maxfev,linfo,nfev,work,dginit,dgout)
  if (debug) then
    print '(A,I3)', 'linfo(out)=', linfo
    print '(A,es12.4,x,A,es12.4)', 'dginit=', dginit, 'dgout=',dgout
  end if
  if (linfo .eq. -1) then
    ! return to reevaluate function and gradient
    flag = -1
    if (jout) then
      print '(A,I3)', 'flag=', flag
      print '(A, es12.4)', 'step length', alpha
    end if
    if (debug) then
      do i=1,nx
        print cform, 'w', wupd((i-1)*member+1:i*member)
      end do
    end if
    return
  else if (linfo .eq. 0) then
    ! improper input parameters
    print '(A)', 'IMPROPER INPUT FOR CVSMOD'
    flag = -2
    if (jout) then
      print '(A,I3)', 'flag=', flag
      print '(A, es12.4)', 'step length', alpha
    end if
    return
  else if (linfo .ge. 2) then
    write(6,'(A,I3)') 'Line Search failed. INFO=',linfo
    flag = -2
    if (jout) then
      print '(A,I3)', 'flag=', flag
      print '(A, es12.4)', 'step length', alpha
    end if
    return
  else ! line search success
    flag = 1
    nfun = nfun + nfev
    if (jout) then
      print '(A,I3)', 'flag=', flag
      print '(A, es12.4)', 'step length', alpha
    end if
    if (debug) then
      do i=1,nx
        print cform, 'w', wupd((i-1)*member+1:i*member)
      end do
    end if
    return
  end if
else if (ls > 0) then
  ! backtracking (Nocedal and Wright 2006;Chapter 3. pp.37)
  ! modified for strong Wolfe conditions
  if (nfun == 0) then ! first trial
    dgout = 0.0_dp
    alpha = 1.0_dp/sqrt(gnorm)
    !alpha = 1.0d-2
    nfun = nfun + 1
  end if
  if (flag == 1) then
    nfev = 0
    linfo = 0
    ggold(:) = ggrad(:)
    gdold(:) = gdesc(:)
    dginit = 0.0_dp
    do i=1,ne 
      dginit = dginit + gdesc(i)*ggrad(i)
    end do
    if (nfun > 1) then
      ! Shanno-Phua's formula for trial step
      alpha = alpha * dgold / dginit
    end if
    dgold = dginit
    if (jout) then
      print '(2(A, es12.4))', 'dginit', dginit,' dgold',dgold
      print '(A, es12.4)', 'trial step', alpha
    end if
    if (alpha .lt. stpmin) then
      write(6,'(A,es12.4)') 'Too small step size ',alpha
      flag = -2
      return
    end if
    do i = 1, ne
      work(i) = wupd(i)
      wupd(i) = work(i) + alpha * gdesc(i)
    end do
    fpre = fval
    flag = -1
    return
  else if (flag==-1) then
    nfev=nfev+1
    if (nfev .gt. maxfev) then
      write(6,'(A,I4)') 'Function evaluation exceeds ',maxfev
      flag = -2
      do i = 1, ne
        wupd(i) = work(i)
      end do
      return
    end if
    dginit = 0.0_dp
    do i=1,ne 
      dginit = dginit + gdesc(i)*ggrad(i)
    end do
    ! first condition
    flh1 = fval
    frh1 = fpre + ftol*alpha*dginit
    if(jout) then
      write(6,'(2(A,es12.4))') 'First: l.h. ',flh1,' r.h. ',frh1
    end if
    !! second condition
    !flh2 = abs(dginit) ! dginit for weak Wolfe
    !frh2 = gtol*abs(dgold) ! gtol*dgold for weak Wolfe
    !!if(debug) then
    !  write(6,'(2(A,es12.4))') 'Second: l.h. ',flh2,' r.h. ',frh2
    !!end if
    if ((flh1.gt.frh1)) then !.or.(flh2.gt.frh2)) then
    ! return to reevaluate function and gradient
      alpha=rhob*alpha
      if (alpha .lt. stpmin) then
        write(6,'(A,es12.4)') 'Too small step size ',alpha
        flag = -2
        return
      end if
      if (jout) then
        print '(A,I3)', 'flag=', flag
        print '(A, es12.4)', 'step length', alpha
      end if
      do i = 1, ne
        wupd(i) = work(i) + alpha * gdesc(i)
      end do
      if (debug) then
        do i=1,nx
          print cform, 'w', wupd((i-1)*member+1:i*member)
        end do
      end if
      return
    else ! line search success
      flag = 1
      nfun = nfun + nfev
      do i = 1, ne
        wupd(i) = work(i) + alpha * gdesc(i)
      end do
      if (jout) then
        print '(A,I3)', 'flag=', flag
        print '(A, es12.4)', 'step length', alpha
      end if
      if (debug) then
        do i=1,nx
          print cform, 'w', wupd((i-1)*member+1:i*member)
        end do
      end if
      return
    end if
  end if
else
  ! fixed
  if (nfun == 0) then ! first trial
    dgout = 0.0_dp
    fpre = 0.0_dp
    fder = 0.0_dp
    alpha = 1.0_dp/sqrt(gnorm)
  end if
  nfun = nfun + 1
  ggold(:) = ggrad(:)
  gdold(:) = gdesc(:)
  fderpre = fder
  fder = fval - fpre
  dginit = 0.0_dp
  do i=1,ne 
    dginit = dginit + gdesc(i)*ggrad(i)
  end do
  if (nfun > 1) then
    ! sufficient decrease check
    if (fder > abs(fderpre)) then
    !if (fder > fderpre) then
      write(6,'(A)') 'insufficient decrease'
      wupd(:) = work(:)
      flag=-2
      return
    end if
    ! Shanno-Phua's formula for trial step
    alpha = alpha * dgold / dginit
  end if
  if (alpha < stpmin) then
    write(6,'(A,es12.4)') 'too small step size ',alpha
    flag = -2
    return
  end if
  dgold = dginit
  if (debug) then
    print '(A, es12.4)', 'dginit', dginit
  end if
  flag = 1
  !alpha = 1.0D0
  fpre = fval
  do i = 1, ne
    work(i) = wupd(i)
    wupd(i) = wupd(i) + alpha * gdesc(i)
  end do
  if (jout) then
    print '(A,I3)', 'flag=', flag
    print '(A, es12.4)', 'step length', alpha
  end if
  if (debug) then
    do i=1,nx
      print cform, 'w', wupd((i-1)*member+1:i*member)
    end do
  end if
  return
end if
end subroutine line_search
!=======================================================================
!  transform matrix : trans = [I+Z^TZ]^{-1/2} = V\Sigma^{-1/2}V^T
!  where I+Z^TZ = V\SigmaV^T (Eigen value decomposition)
!=======================================================================
subroutine calc_trans(ne,nobs,nobsl,wupd,zxb,trans,parm_infl,dfs,dfn,evalout)
  implicit none
  integer, intent(in) :: ne
  integer, intent(in) :: nobs
  integer, intent(in) :: nobsl
  real(kind=dp), intent(in) :: wupd(ne)
  real(kind=dp), intent(in) :: zxb(nobs,ne)
  real(kind=dp), intent(out) :: trans(ne,ne)
  real(kind=dp), intent(in) :: parm_infl
  real(kind=dp), intent(out), optional :: dfs ! degree of freedom for signal
  real(kind=dp), intent(out), optional :: dfn ! degree of freedom for noise
  real(kind=dp), intent(out), optional :: evalout(ne)

  real(kind=dp), allocatable :: work(:,:)
  real(kind=dp), allocatable :: eival(:)
  real(kind=dp), allocatable :: eivec(:,:)
  real(kind=dp) :: rho, w2, wij, cij, evaltmp
  integer :: i, j
!-----------------------------------------------------------------------
!  hess = I / (1+parm_infl) + zxb^T @ zxb (Gauss)
!  hess_{ij} 
!  = [(2+parm_infl*w2)/2/(1+parm_infl*w2)**2
!   + parm_infl*wj*wi/(1+parm_infl*w2)**2/2/w2 
!   - (2+parm_infl*w2)*parm_infl*wj*wi/(1+parm_infl*w2)**3/w2] + zxb^T @ zxb (i=j)
!  = [parm_infl*wj*wi/(1+parm_infl*w2)**2/2/w2 
!   - (2+parm_infl*w2)*parm_infl*wj*wi/(1+parm_infl*w2)**3/w2] + zxb^T @ zxb (i/=j)
!  (non-Gauss)
!-----------------------------------------------------------------------
  allocate( work(ne,ne) )
  if(ngauss) then
    w2 = 0.0_dp
    do i=1,ne 
      w2 = w2 + wupd(i)*wupd(i)
    end do
    w2 = sqrt(w2)
    rho = (2.0_dp + parm_infl*w2)/2.0_dp/(1.0_dp + parm_infl*w2)**2
  else
    rho = 1.0_dp / (1.0_dp + parm_infl)
  end if
  work = 0.0_dp
  if (ngauss) then
    if (w2 > 1.0e-16) then
      cij = parm_infl*(3.0_dp+parm_infl*w2)/w2/2.0_dp/(1.0_dp + parm_infl*w2)**3
    else
      cij = 0.0_dp
    end if
    do j=1,ne 
      work(j,j) = rho
      do i=j,ne 
        wij = wupd(i)*wupd(j)
        work(i,j) = work(i,j) -cij*wij
        work(j,i) = work(i,j)
      end do
    end do
  else
    do i = 1, ne 
      work(i,i) = rho
    end do
  end if
  if(nobsl == 0) then
    if(debug) write(6,'(A)') 'No observation to be assimilated'
    trans = 0.0_dp
    do i=1,ne
      trans(i,i) = sqrt(1.0_dp / rho)
    end do
    return
  end if
  call dgemm('t','n',ne,ne,nobsl,1.0_dp,zxb(1:nobsl,:),nobsl,zxb(1:nobsl,:),&
    & nobsl,1.0_dp,work,ne)
!-----------------------------------------------------------------------
!  eigen value decomposition of I + zxb^T @ zxb
!-----------------------------------------------------------------------
  allocate( eival(ne), eivec(ne,ne) )
  call mtx_eigen(1,ne,work,eival,eivec,i)
  if (debug) then
    write(cn, '(i4)') ne
    print '(A,'//trim(cn)//'es13.5)', 'eival', eival(:)
    !call dgemm('n','t',ne,ne,ne,1.0_dp,eivec,ne,eivec,ne,0.0_dp,work,ne)
    !print '(A)', 'eivec @ eivec^T'
    !do i=1,ne
    !  print '(i2,'//trim(cn)//'es12.4)', i, work(i,:)
    !enddo
  end if
  if(present(dfs).and.present(dfn)) then
    dfs=0.0_dp
    dfn=0.0_dp
    do j=1,i
      evaltmp = eival(j) - rho
      if(evaltmp.gt.0.0d0) then
        dfs = dfs + evaltmp / (1.0d0 + evaltmp)
        dfn = dfn + 1.0d0 / (1.0d0 + evaltmp)
      end if
    end do
    if(jout) write(6,'(2(a,es13.5))') 'dfs=',dfs,' dfn=',dfn
  end if
  if(present(evalout)) then
    evalout = 0.0_dp
    do i=1,ne
      if(eival(i) > rho) evalout(i) = eival(i) - rho 
    end do
  end if
!-----------------------------------------------------------------------
!  trans = eivec @ 1/sqrt(eival) @ eivec^T
!-----------------------------------------------------------------------
  work = 0.0_dp
  do j = 1, ne
    do i = 1, ne 
      work(i,j) = eivec(i,j) / dsqrt(eival(j))
    end do 
  end do
  call dgemm('n','t',ne,ne,ne,1.0_dp,work,ne,eivec,&
  &          ne,0.0_dp,trans,ne)
  if (debug) then
    write(cn, '(i4)') ne
    print '(A)', 'trans'
    do i=1,ne
      print '(i2,'//trim(cn)//'es13.5)', i, trans(i,:)
    enddo
  end if
  deallocate( work, eival, eivec )
  return
end subroutine calc_trans
!
! PRIVATE ROUTINES
!
!=======================================================================
!  linear system solver with lower triangular matrix : Ax = b
!=======================================================================
subroutine solve_trisystem(TRANS,n,A,x)
  implicit none 
  character(len=1),intent(in) :: TRANS ! U(u):upper, L(l):lower
  integer,intent(in)          :: n     ! system dimension
  real(kind=dp),intent(in)     :: A(:,:)! lower triangular matrix
  real(kind=dp),intent(inout)  :: x(:)  ! (in) right hand side vector b
                                       ! (out) solution vector
  real(kind=dp) :: c, tmp
  integer :: i,j 
  if ((TRANS=='L') .or. (TRANS=='l')) then ! lower system
    if (A(1,1) == 0.0_dp) then
      write(6,'(A,i3)') 'ERR! system is underdetermined at',1
      STOP 99
    end if 
    c = 1.0_dp / A(1,1)
    x(1) = x(1) * c
    do i=2, n 
      if (A(i,i) == 0.0_dp) then
        write(6,'(A,i3)') 'ERR! system is underdetermined at',i
        CYCLE
      end if 
      c = 1.0_dp / A(i,i)
      tmp = x(i)
      do j=1, i-1
        tmp = tmp - A(i,j)*x(j)
      end do 
      x(i) = tmp * c  
    end do
    return
  else if ((TRANS=='U') .or. (TRANS=='u')) then! upper system
    if (A(n,n) == 0.0_dp) then
      write(6,'(A,i3)') 'ERR! system is underdetermined at',n
      STOP 99
    end if 
    c = 1.0_dp / A(n,n)
    x(n) = x(n) * c 
    do i = n-1, 1, -1
      if (A(i,i) == 0.0_dp) then
        write(6,'(A,i3)') 'ERR! system is underdetermined at',i
      end if 
      c = 1.0_dp / A(i,i)
      tmp = x(i)
      do j=i+1, n 
        tmp = tmp - A(i,j)*x(j)
      end do 
      x(i) = tmp * c 
    end do
    return
  else
    write(6,'(2A)') 'ERR! invalid option, TRANS=', TRANS
  end if
end subroutine solve_trisystem
!=======================================================================
!  step length calculation using quadratic interpolation
!=======================================================================
subroutine quad_itpl(ne,nobs,alpha,fval,w,d,g,zxb,dep,parm_infl)
  implicit none
  integer, intent(in) :: ne, nobs
  real(kind=dp), intent(inout) :: alpha
  real(kind=dp), intent(in) :: fval
  real(kind=dp), intent(inout) :: w(ne)
  real(kind=dp), intent(in) :: d(ne)
  real(kind=dp), intent(in) :: g(ne)
  real(kind=dp), intent(in) :: zxb(nobs,ne)
  real(kind=dp), intent(in) :: dep(nobs)
  real(kind=dp), intent(in) :: parm_infl

  real(kind=dp),parameter :: alpha_min=1.0d-7 ! minimum step length
  real(kind=dp) :: w_t(ne) ! w_trial - w = alpha_trial * desc
  real(kind=dp) :: work(nobs)
  real(kind=dp) :: f0, df0, fa
  real(kind=dp) :: num, den 
  integer :: i

  if (debug) then
    print '(A,es12.4)', 'alpha(guess) ', alpha
  end if
  do i = 1, ne
    w_t(i) = w(i) + alpha * d(i)
  end do
  ! f(0) = fval
  f0 = fval
  ! f'(0) = \nabla f^T desc
  df0 = 0.0_dp
  do i = 1, ne
    df0 = df0 + g(i)*d(i)
  end do
  ! f(\alpha) = f(w+\alpha*desc)
  fa = 0.0_dp
  do i = 1, ne
    fa = fa + w_t(i)*w_t(i)
  end do
  fa = fa / (1.0_dp + parm_infl)
  !work = dep
  call dgemv('n',nobs,ne,1.0_dp,zxb,nobs,w_t,1,0.0_dp,work,1)
  work = work - dep
  !work = -1.0_dp * dep
  !do j = 1, ne
  !  do i = 1, nobs
  !    work(i) = work(i) + zxb(i,j)*w_t(j)
  !  end do
  !end do
  do i = 1, nobs
    fa = fa + work(i)*work(i)
  end do
  fa = fa * 0.5_dp
  if (debug) then
    print '(A,es12.4,x,A,es12.4,x,A,es12.4)', 'f(0) ', f0, 'df(0)', df0, 'f(a)', fa
  end if
  num = alpha * df0
  den = 2.0_dp * (f0 + df0*alpha - fa)
  if (debug) then
    print '(A,es12.4,x,A,es12.4)', 'den=',den,'num=',num
  end if
  if (abs(den) > 1e-10) then
    alpha = alpha * num / den
  end if
  alpha = max(alpha,alpha_min)
  do i = 1, ne
    w(i) = w(i) + alpha * d(i)
  end do
  return
end subroutine quad_itpl
!=======================================================================
! Inflation estimation
! Reference : Wang and Bishop (2003)
!=======================================================================
subroutine est_infl(parm_infl,nobsl,zxb,dep,rloc)
  implicit none
  real(kind=dp),intent(inout) :: parm_infl 
  integer,intent(in) :: nobsl
  real(kind=dp),intent(in) :: zxb(:,:) !R^{-1/2}HPf^{1/2}
  real(kind=dp),intent(in) :: dep(:)   !R^{-1/2}(y-Hx)
  real(kind=dp),intent(in),optional :: rloc(:) !localization function
  real(kind=dp) :: parm(4)
  real(kind=dp) :: sigma_o2
  real(kind=dp),parameter :: sigma_b=0.04_dp
  real(kind=dp) :: gain
  integer :: i,j

  parm_infl = parm_infl + 1.0_dp
  parm(:) = 0.0_dp
  ! trace(<d,d^T>)
  do i=1,nobsl
    parm(1) = parm(1) + dep(i)*dep(i)
  end do
  ! trace(<Z,Z^T>)
  do j=1,member
    do i=1,nobsl
      parm(2) = parm(2) + zxb(i,j)*zxb(i,j)
    end do
  end do
  !parm(2) = parm(2) / REAL(member-1,kind=dp)
  ! trace(L)
  if( present(rloc) ) then
    parm(3) = SUM(rloc(1:nobsl))
  else
    parm(3) = nobsl
  end if
  parm(4) = (parm(1)-parm(3))/parm(2) - parm_infl
  if(jout) write(6,'(A,4ES13.5)') 'est_infl : parm = ', parm
  sigma_o2 = 2.0_dp/parm(3)*((parm_infl*parm(2)+parm(3))/parm(2))**2
  gain = sigma_b**2 / (sigma_o2 + sigma_b**2)
  if(jout) write(6,'(A,ES13.5)') 'est_infl : beta = ', gain
  parm_infl = parm_infl + gain * parm(4)
  parm_infl = parm_infl - 1.0_dp
  !if(parm_infl < 0.0_dp) then
  !  parm_infl = 0.0_dp
  !end if
  return
end subroutine est_infl
end module mlef_module
