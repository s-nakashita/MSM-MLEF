module lmlef_tools
!=======================================================================
!
! [PURPOSE:] Module for LMLEFY with MSM
!
! [HISTORY:]
!   06/02/2022 copied from lmlefcw_tools
!   07/25/2022 construct
!   08/26/2022 option for saving cost and ensemble weights added
!   12/15/2022 modified for MSM
!   01/09/2023 add surface variables to control variables
!   01/10/2023 add RTPP & RTPS inflation methods
!
!=======================================================================
  use kind_module
  use nml_module
  use co_module
  use rsmcom_module
  use corsm_module
  use func_module, only : calc_pfull, calc_rh, calc_q2
  use mlef_module, only : debug, jout, mlef_init, &
!  & mlefy_core, line_search, &
  & mlef_core, calc_trans, est_infl
  use lmlef_obs

  implicit none

  private
  public :: init_das_lmlef, das_lmlefy

  integer,save :: nobstotal
  real(kind=dp),save,public :: pscale
  real(kind=dp),allocatable :: var_local(:,:)
  integer,allocatable,save  :: var_local_n2n(:)
  integer,allocatable,save  :: var_update(:) !=1:updated =0:not updated
! for obs_local & obs_update
  real(kind=dp),allocatable,save :: obsdepk(:)
  real(kind=dp),allocatable,save :: obshdxk(:,:)
!!DEBUG
  character(len=4) :: cn
  integer, parameter :: timeunit=7
  character(len=18) :: recfilename='debug_time-MMM.txt'
  real(kind=dp) :: rtimer00,rtimer
!!DEBUG

contains
subroutine init_das_lmlef
  implicit none
  if (scl_mem) then
  if (mean) then
    pscale = 1.0d0 / dsqrt(real(member-1,kind=dp))
  else
    pscale = 1.0d0 / dsqrt(real(member,kind=dp))
  end if
  else 
    pscale = 1.0d0
  end if
  write(6,'(A,F7.4)') 'pscale=', pscale
!! variable localization
  allocate( var_local(nv3d+nv2d_sig,nobstype) )
  allocate( var_local_n2n(nv3d+nv2d_sig) )
  allocate( var_update(nv3d+nv2d) )
  if(nonhyd.eq.1) then !nonhydrostatic
  var_local = reshape( &
!!          Th     U      V      Q     OZ     CW     Pn     Tn      W     GZ     Ps     Wb
   & (/ 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, & ! U
   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, & ! V
   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, & ! T
   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, & ! Q
   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, & ! RH
   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, & ! Ps
   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, & ! T2m
   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, & ! Td
   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, & ! Wind Direction
   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0/)& ! Wind speed
   & ,(/nv3d+nv2d_sig,nobstype/))
!!                              Th  U  V  Q OZ CW Pn Tn  W GZ Ps Wb
  var_update(1:nv3d+nv2d_sig)=(/ 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0/)
  iv3d_t = iv3d_tn !non-hydrostatic temperature updated
  iv3d_tt= iv3d_th !hydrostatic temperature not updated
  else !hydrostatic
  var_local = reshape( &
!!          T      U      V      Q     OZ     CW     GZ     Ps 
   & (/ 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, & ! U
   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, & ! V
   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, & ! T
   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, & ! Q
   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, & ! RH
   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, & ! Ps
   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, & ! T2m
   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, & ! Td
   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, & ! Wind Direction
   &    1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0, 1.0d0/)& ! Wind speed
   & ,(/nv3d+nv2d_sig,nobstype/))
!!                                  T  U  V  Q OZ CW GZ Ps
   var_update(1:nv3d+nv2d_sig) = (/ 1, 1, 1, 1, 1, 1, 0, 1/)
   iv3d_t = iv3d_th
   iv3d_tt= iv3d_t
  end if
  var_update(nv3d+nv2d_sig+1:) = 0 ! r_sfc variables not update
  write(cn,'(i4)') nv3d+nv2d
  write(6,'(a,a4)') '#variables = ',cn
  write(6,'(a)') 'var_update'
  write(6,'('//trim(cn)//'(a6,x))') varnames
  write(6,'('//trim(cn)//'(i6,x))') var_update
  if(debug_time) then
    write(recfilename(12:14),'(i3.3)') myimage
    open(timeunit,file=recfilename)
  end if
  return
end subroutine init_das_lmlef
!-----------------------------------------------------------------------
! Data Assimilation
!-----------------------------------------------------------------------
subroutine das_lmlefy(gues3dc,gues2dc,gues3d,gues2d,anal3dc,anal2dc,anal3d,anal2d)
  implicit none
  real(kind=dp),intent(inout) :: gues3dc(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d)[*] ! background control
  real(kind=dp),intent(inout) :: gues2dc(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nv2d)[*]      !  output: destroyed
  real(kind=dp),intent(inout) :: gues3d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,member,nv3d)[*] ! background ensemble
  real(kind=dp),intent(inout) :: gues2d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,member,nv2d)[*]      !  output: destroyed
  real(kind=dp),intent(out) :: anal3dc(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d)[*] ! analysis control
  real(kind=dp),intent(out) :: anal2dc(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nv2d)[*]
  real(kind=dp),intent(out) :: anal3d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,member,nv3d)[*] ! analysis ensemble
  real(kind=dp),intent(out) :: anal2d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,member,nv2d)[*]
  real(kind=dp),allocatable :: mean3d(:,:,:,:)
  real(kind=dp),allocatable :: mean2d(:,:,:)
  real(kind=dp),allocatable :: zxb(:,:)
  real(kind=dp),allocatable :: dep(:)
  real(kind=dp),allocatable :: rloc(:)
  real(kind=dp),allocatable :: w(:,:,:)[:] !ensemble size * (local)horizontal grids * levels
  real(kind=dp),allocatable :: grad(:,:,:)[:] !ensemble size * (local)horizontal grids * levels
  real(kind=dp),allocatable :: gold(:,:,:)[:] !ensemble size * (local)horizontal grids * levels
  real(kind=dp),allocatable :: desc(:,:,:)[:] !ensemble size * (local)horizontal grids * levels
  real(kind=dp),allocatable :: dold(:,:,:)[:] !ensemble size * (local)horizontal grids * levels
  real(kind=dp),allocatable :: fval(:,:)[:] !(local)horizontal grids * levels
  real(kind=dp),allocatable :: gnorm(:,:)[:] !(local)horizontal grids * levels
  integer      ,allocatable :: flag(:,:)[:] !(local)horizontal grids * levels
  integer                   :: ne, ngrd
!  real(kind=dp),allocatable :: gw(:,:,:) !ensemble size * (global)horizontal grids * levels
!  real(kind=dp),allocatable :: ggrad(:,:,:) !ensemble size * (global)horizontal grids * levels
!  real(kind=dp),allocatable :: ggold(:,:,:) !ensemble size * (global)horizontal grids * levels
!  real(kind=dp),allocatable :: gdesc(:,:,:) !ensemble size * (global)horizontal grids * levels
!  real(kind=dp),allocatable :: gdold(:,:,:) !ensemble size * (global)horizontal grids * levels
!  real(kind=dp),allocatable :: gwork(:,:,:) !ensemble size * (global)horizontal grids * levels
  integer      ,save        :: gflag[*]
  real(kind=dp),save        :: fglb[*]
  real(kind=dp),allocatable :: work3d(:,:,:,:)[:]
  real(kind=dp),allocatable :: work2d(:,:,:)[:]
  real(kind=dp),allocatable :: work3de(:,:,:,:,:)[:]
  real(kind=dp),allocatable :: work2de(:,:,:,:)[:]
  real(kind=dp),allocatable :: jwork3d(:,:,:,:)[:]
  real(kind=dp),allocatable :: jwork2d(:,:,:)[:]
  real(kind=dp),allocatable :: rtps3d(:,:,:,:)[:] !RTPS coefficients
  real(kind=dp),allocatable :: rtps2d(:,:,:)[:] !RTPS coefficients
  real(kind=dp),allocatable :: work3dg(:,:,:,:)
  real(kind=dp),allocatable :: work2dg(:,:,:)
  real(kind=dp),allocatable :: logpfm(:,:)[:]
  real(kind=dp),allocatable :: infl_mul3d(:,:,:) !multiplicative inflation parameter
  real(kind=dp),allocatable :: tmpps(:)
  real(kind=dp) :: alpha,tmpbeta
  real(kind=dp) :: fsum, dginit, dgout, gnormall
  real(kind=dp) :: parm, parm_upd
  real(kind=dp) :: jb1,jo1
  real(kind=dp) :: dfs,dfn
  real(kind=dp),allocatable :: evalout(:)
  real(kind=dp),allocatable :: trans(:,:)
  real(kind=dp),allocatable :: transrlx(:,:)
  real(kind=dp),allocatable :: pao(:,:)
!  real(kind=dp),allocatable :: trans(:,:,:,:)
  integer, allocatable :: nobs_use(:,:,:), nobsl(:,:)
  integer, allocatable :: nfun(:,:)
  logical :: ex
  logical :: hfirst ! for obs_local
  integer :: ij,ilev,n,m,i,j,k,ierr
  integer :: niter, flagall
! for q adjustment
  real(kind=dp) :: rh, qlim

  if(debug_time) call cpu_time(rtimer00)
  write(6,'(A)') 'Hello from das_lmlef'
  nobstotal = obsdasort%nobs !+ ntvs
  write(6,'(A,I8)') 'Target observation numbers : NOBS=',obsdasort%nobs!,', NTVS=',ntvs
  !
  ! In case of no obs
  !
  if(nobstotal == 0) then
    write(6,'(A)') 'No observation assimilated'
    anal3dc = gues3dc
    anal2dc = gues2dc
    anal3d = gues3d
    anal2d = gues2d
    return
  end if
  !
  ! Variable localization
  !
  var_local_n2n(1) = 1
  do n=2,nv3d+nv2d_sig
    do i=1,n
      var_local_n2n(n) = i
      if(maxval(abs(var_local(i,:)-var_local(n,:))) < TINY(var_local)) EXIT
    end do
  end do
  write(cn,'(i4)') nv3d+nv2d_sig
  write(6,'(a)') 'var_local'
  write(6,'('//trim(cn)//'(a6,x))') varnames(1:nv3d+nv2d_sig)
  write(6,'('//trim(cn)//'(i6,x))') var_local_n2n
  !
  ! FCST PERTURBATIONS
  !
  if(mean) then
!    allocate(mean3d(ni1,nj1,nlev,nv3d))
!    allocate(mean2d(ni1,nj1,nv2d))
    call ensmean_grd(member,ni1+2*ighost,nj1+2*jghost,&
            gues3d(1-ighost:ni1+ighost,1-jghost:nj1+jghost,:,:,:),&
            gues2d(1-ighost:ni1+ighost,1-jghost:nj1+jghost,:,:),&
            gues3dc(1-ighost:ni1+ighost,1-jghost:nj1+jghost,:,:),&
            gues2dc(1-ighost:ni1+ighost,1-jghost:nj1+jghost,:))
!    do n=1,nv3d
!      do k=1,nlev
!        do j=1,nj1
!          do i=1,ni1
!            gues3dc(i,j,k,n) = mean3d(i,j,k,n)
!          end do
!        end do
!      end do
!    end do
!    do n=1,nv2d
!      do j=1,nj1
!        do i=1,ni1
!          gues2dc(i,j,n) = mean2d(i,j,n)
!        end do
!      end do
!    end do
!    deallocate(mean3d,mean2d)
  end if
  ! (optional) scaling by ensemble size
  do n=1,nv3d
    do m=1,member
      do k=1,nlev
        do j=1-jghost,nj1+jghost
          do i=1-ighost,ni1+ighost
            gues3d(i,j,k,m,n) = (gues3d(i,j,k,m,n) - gues3dc(i,j,k,n))*pscale
          end do
        end do
      end do
    end do
  end do
  do n=1,nv2d
    do m=1,member
      do j=1-jghost,nj1+jghost
        do i=1-ighost,ni1+ighost
          gues2d(i,j,m,n) = (gues2d(i,j,m,n) - gues2dc(i,j,n))*pscale
        end do
      end do
    end do
  end do
  !
  ! multiplicative inflation
  !
  allocate( work3d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d)[*] )
  allocate( work2d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nv2d)[*] )
  allocate( infl_mul3d(1:ni1,1:nj1,nlev) ) !inflation parameter
  allocate( work3dg(nlon,nlat,nlev,nv3d) ) !also used for saving cost
  allocate( work2dg(nlon,nlat,nv2d) ) !also used for saving cost
  if(cov_infl_mul >= 0.0d0) then ! fixed multiplicative inflation parameter
    work3d = cov_infl_mul
    work2d = cov_infl_mul
!    work3d(:,:,nlev,:) = 0.01d0
    infl_mul3d = cov_infl_mul
  end if
  if(cov_infl_mul < 0.0d0) then ! 3D parameter values are read-in
    inquire(file=infl_mul_in_basename,exist=ex)
    if(ex) then
      if(myimage == print_img) then
        write(6,'(A,I3.3,2A)') 'MYIMAGE ',myimage,' is reading.. ',infl_mul_in_basename
        call read_restart(infl_mul_in_basename,work3dg,work2dg,convert=.false.)
      end if
      call scatter_grd(print_img,work3dg,work2dg,work3d,work2d)
      infl_mul3d = work3d(1:ni1,1:nj1,1:nlev,1)
    else
      write(6,'(2A)') '!!WARNING: no such file exist: ',infl_mul_in_basename
      work3d = -1.0d0 * cov_infl_mul
      work2d = -1.0d0 * cov_infl_mul
      infl_mul3d = -1.0d0 * cov_infl_mul
!      work3dg = -1.0d0 * cov_infl_mul !for global cost
    end if
  end if
  if(debug) then
    write(6,'(A,2F13.5)') 'prior inflation = ', minval(infl_mul3d(:,:,:)), maxval(infl_mul3d(:,:,:))
  end if
  !
  ! p_full for background control
  !
  allocate(logpfm(1:nij1max,nlev)[*])
  if(nonhyd.eq.1) then !non-hydrostatic
    do ij=1,nij1
      i=mod(ij-1,ni1)+1
      j=(ij-1)/ni1 + 1
      logpfm(ij,:) = gues3dc(i,j,:,iv3d_pn)
    end do
  else
    allocate( tmpps(1:nij1) )
    do ij=1,nij1
      i=mod(ij-1,ni1)+1
      j=(ij-1)/ni1 + 1
      tmpps(ij)=gues2dc(i,j,iv2d_ps)
    end do
    call calc_pfull(nij1,1,nlev,sig,tmpps,logpfm(1:nij1,:))
    deallocate( tmpps )
  end if
  logpfm = log(logpfm)
  if(debug) then
    write(6,'(A,2F13.5)') 'log p_full = ', minval(logpfm(1:nij1,:)), maxval(logpfm(1:nij1,:))
  end if
  !
  ! MAIN ASSIMILATION LOOP
  !
  !! obs_local
  allocate( nobs_use(1:nobstotal,1:nij1,1:nlev), nobsl(1:nij1,1:nlev) )
  allocate( zxb(1:nobstotal,1:member),dep(1:nobstotal) )
  allocate( rloc(1:nobstotal) )
  allocate( obsdepk(1:nobstotal),obshdxk(1:nobstotal,1:member) )
  !! local arrays
  allocate( w(1:member,1:nij1max,1:nlev)[*] )
  allocate( grad(1:member,1:nij1max,1:nlev)[*], gold(1:member,1:nij1max,1:nlev)[*] )
  allocate( desc(1:member,1:nij1max,1:nlev)[*], dold(1:member,1:nij1max+1,1:nlev)[*] )
  allocate( fval(1:nij1max,1:nlev)[*], gnorm(1:nij1max,1:nlev)[*], flag(1:nij1max,1:nlev)[*] )
  allocate( nfun(1:nij1,1:nlev) )
!  !! global arrays
!  allocate( gw(1:member,1:lngrd,1:nlev) )
!  allocate( ggrad(1:member,1:lngrd,1:nlev), ggold(1:member,1:lngrd,1:nlev) )
!  allocate( gdesc(1:member,1:lngrd,1:nlev), gdold(1:member,1:lngrd,1:nlev) )
!  allocate( gwork(1:member,1:lngrd,1:nlev) )
  !! ensemble transformation matrix
  allocate( trans(member,member) )
!  allocate( trans(member,member,nij1,nlev) )
  allocate( transrlx(member,member) )
  if(sp_infl_rtps > 0.0d0 ) then
    allocate( pao(member,member) )
  end if
  if(save_info) then
    ! for save local cost functions, local DFS and obs number per grids
    allocate( jwork3d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d)[*] )
    allocate( jwork2d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nv2d)[*] )
    jwork3d = 0.0d0
    jwork2d = 0.0d0
    ! for save ensemble weights and eigenvalues of tranform matrix
    allocate( work3de(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,member,nv3d)[*])
    allocate( work2de(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,member,nv2d)[*] )
    allocate( evalout(member) )
    work3de = 0.0d0
    work2de = 0.0d0
  end if
  ! initialize
  niter = 0
  nfun(:,:)  = 0
  gflag = 1
  alpha = 1.0d0
  dginit = 0.0d0
  dgout = 0.0d0
  w(:,:,:) = 0.0d0
  grad(:,:,:) = 0.0d0
  gold(:,:,:) = 0.0d0
  desc(:,:,:) = 0.0d0
  dold(:,:,:) = 0.0d0
  fval(:,:) = 0.0d0
  flag(:,:) = 1
  hfirst = .TRUE.
  if(debug_time) then
    call cpu_time(rtimer)
    write(timeunit,'(A,2F12.7)') '### DAS_LMLEF TIMER(INITIALIZE):',rtimer,rtimer-rtimer00
    rtimer00=rtimer
  end if
  ! start optimization for all grids and variables
  do while ( niter <= maxiter )
    if( jout .and. (gflag==1) ) write(6,'(A,i4)') 'iteration', niter
    ! global cost
    ngrd=ni1*nj1*nlev
    ne=member*ngrd
    call global_cost(ne,ngrd,reshape(w(:,1:nij1,:),(/ne/)),reshape(infl_mul3d,(/ngrd/)),fglb)
    sync all
    if(myimage==print_img) then
      do k=1,nimages
        if(k==myimage) cycle
        fglb[myimage] = fglb[myimage] + fglb[k]
      end do
    end if
  if(debug_time) then
    call cpu_time(rtimer)
    write(timeunit,'(A,2F12.7)') '### DAS_LMLEF TIMER(GLOBALCOST):',rtimer,rtimer-rtimer00
    rtimer00=rtimer
  end if
    do ilev=1,nlev
      write(6,'(A,I3)') 'ilev = ',ilev
      do ij=1,nij1
        i=mod(ij-1,ni1)+1
        j=(ij-1)/ni1 + 1
        if(debug) write(6,'(3(a,i6))') 'ij ',ij,' i ',i,' j ',j
        if(flag(ij,ilev)/=0.and.flag(ij,ilev).ge.-1) then
          flag(ij,ilev) = gflag
          if(niter==maxiter) flag(ij,ilev) = -2
          !if( debug .and. (flag(ij,ilev)==1) ) write(6,'(A,i4)') 'iteration', niter
          n=1 !dummy
          call obs_local(ij,ilev,n,zxb,dep,rloc,&
          & nobsl(ij,ilev),nobs_use(:,ij,ilev),&
          & logpfm,hfirst)
  if(debug_time) then
    call cpu_time(rtimer)
    write(timeunit,'(A,I5,L2,F12.7)') '### DAS_LMLEF TIMER(OBS_LOCAL):',nobsl(ij,ilev),hfirst,rtimer-rtimer00
    rtimer00=rtimer
  end if
          parm = infl_mul3d(i,j,ilev)
          ! adaptive inflation
          if((cov_infl_mul < 0.0d0).and.(nobsl(ij,ilev).gt.0).and.hfirst) then
            parm_upd = parm
            call est_infl(parm_upd,nobsl(ij,ilev),zxb,dep,rloc)
            work3d(i,j,ilev,1) = parm_upd   
          end if
          call mlef_core(member,nobstotal,nobsl(ij,ilev),nfun(ij,ilev),&
            & zxb,dep,parm,alpha,w(:,ij,ilev),flag(ij,ilev)&
!            & ,trans(:,:,ij,ilev)&
            & ,jb1,jo1,gnorm(ij,ilev))
          fval(ij,ilev) = jb1 + jo1
          if(save_info) then
            jwork3d(i,j,ilev,1) = jb1
            jwork3d(i,j,ilev,2) = jo1
          end if
          if(debug) write(6,'(2I4,A,I4)') ij, ilev, ' flag=', flag(ij,ilev) 
  if(debug_time) then
    call cpu_time(rtimer)
    write(timeunit,'(A,I5,F12.7)') '### DAS_LMLEF TIMER(MLEF_CORE):',nobsl(ij,ilev),rtimer-rtimer00
    rtimer00=rtimer
  end if
        end if
      end do
    end do
    sync all
    if(myimage==print_img) then
      flagall=0
      do k=1,nimages
        do ilev=1,nlev
          do ij=1,nij1node(k)
            if(flag(ij,ilev)[k]/=0) then
              flagall=flagall+ABS(flag(ij,ilev)[k])
            end if
          end do
        end do
      end do
      gnormall=0.0d0
      fsum=0.0d0
      ne=0
!      n=0
      do k=1,nimages
        do ilev=1,nlev
          do ij=1,nij1node(k)
            ne=ne+member
            fsum = fsum + fval(ij,ilev)[k]
            gnormall = gnormall + gnorm(ij,ilev)[k]**2
!            gw   (:,n+ij,ilev) = w   (:,ij,ilev)[k]
!            ggrad(:,n+ij,ilev) = grad(:,ij,ilev)[k]
!            ggold(:,n+ij,ilev) = gold(:,ij,ilev)[k]
!            gdesc(:,n+ij,ilev) = desc(:,ij,ilev)[k]
!            gdold(:,n+ij,ilev) = dold(:,ij,ilev)[k]
          end do
        end do
!        n=n+nij1node(k)
      end do
!      if(debug) then
!        write(6,'(A,2F13.5)') 'inflation = ', minval(work3dg(:,:,:,1)), maxval(work3dg(:,:,:,1))
!      end if
!      call global_cost(ne,nlon*nlat*nlev,gw,work3dg(:,:,:,1),fglb)
!      fglb=fsum
      if(flagall==0.or.niter==maxiter) then !all grids converged
        gflag=0
      else
!        call line_search(ne,nfun,gw,fglb,ggrad,ggold,&
!        & gdesc,gdold,gwork,alpha,dginit,dgout,gflag)
        gflag=1
      end if
      if(gflag>=0) then
      !if(jout) then
        write(6,'(A,I12)')    'ne=',ne
        write(6,'(2(A,I7))') 'flagall=',flagall,' / ',lngrd*nlev
        write(6,'(A,ES12.5)') 'J(sum)=',fsum
        write(6,'(A,ES12.5)') 'J(global)=',fglb
        if(niter<maxiter) write(6,'(A,ES12.5)') '|g(global)|=',sqrt(gnormall)
      !end if
      end if
!      n=0
      do k=1,nimages
        gflag[k] = gflag[myimage]
!        do ilev=1,nlev
!          do ij=1,nij1node(k)
!            w   (:,ij,ilev)[k] = gw   (:,n+ij,ilev)
!            grad(:,ij,ilev)[k] = ggrad(:,n+ij,ilev)
!            gold(:,ij,ilev)[k] = ggold(:,n+ij,ilev)
!            desc(:,ij,ilev)[k] = gdesc(:,n+ij,ilev)
!            dold(:,ij,ilev)[k] = gdold(:,n+ij,ilev)
!          end do
!        end do
!        n=n+nij1node(k)
      end do      
    end if
    sync all
  if(debug_time) then
    call cpu_time(rtimer)
    write(timeunit,'(A,2F12.7)') '### DAS_LMLEF TIMER(SYNC_MINIMIZE):',rtimer,rtimer-rtimer00
    rtimer00=rtimer
  end if
    if ( gflag==0 ) then
      EXIT 
    else
      if ( gflag==1 ) niter=niter+1
      ! update Z & dep in entire region
      call obs_update( gues3dc,gues2dc,gues3d,gues2d,w )
      hfirst=.FALSE.
  if(debug_time) then
    call cpu_time(rtimer)
    write(timeunit,'(A,2F12.7)') '### DAS_LMLEF TIMER(OBS_UPDATE):',rtimer,rtimer-rtimer00
    rtimer00=rtimer
  end if
    end if
  end do ! while ( niter <= maxiter )
  if (niter>=maxiter) write(6,'(A,I4)') 'iteration number exceeds ',maxiter
  ! update analysis
  if(relax_spread_out) then
    allocate( rtps3d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d)[*] )
    allocate( rtps2d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nv2d)[*] )
    rtps3d = 1.0_dp
    rtps2d = 1.0_dp
  end if
  do ilev=1,nlev
   do ij=1,nij1
      i=mod(ij-1,ni1)+1
      j=(ij-1)/ni1 + 1
      ! calculate trans (inverse Hessian)
      n=1 !dummy
      call obs_local(ij,ilev,n,zxb,dep,rloc,&
      & nobsl(ij,ilev),nobs_use(:,ij,ilev),&
      & logpfm,hfirst)
      if(save_info) then
        jwork3d(i,j,ilev,5) = real(nobsl(ij,ilev),kind=dp)
        jwork3d(i,j,ilev,6) = real(sum(rloc(1:nobsl(ij,ilev))),kind=dp)
      end if
  if(debug_time) then
    call cpu_time(rtimer)
    write(timeunit,'(A,I5,L2,F12.7)') '### DAS_LMLEF TIMER(OBS_LOCAL):',nobsl(ij,ilev),hfirst,rtimer-rtimer00
    rtimer00=rtimer
  end if
      parm = infl_mul3d(i,j,ilev)
      if(save_info) then
        if(sp_infl_rtps>0.0d0) then
        call calc_trans(member,nobstotal,nobsl(ij,ilev),w(:,ij,ilev),zxb,trans,parm,&
        & pao=pao,dfs=dfs,dfn=dfn,evalout=evalout)
        else
        call calc_trans(member,nobstotal,nobsl(ij,ilev),w(:,ij,ilev),zxb,trans,parm,&
        & dfs=dfs,dfn=dfn,evalout=evalout)
        end if
        jwork3d(i,j,ilev,3) = dfs
        jwork3d(i,j,ilev,4) = dfn
        ! save ensemble weights and eigenvalues
        do m=1,member
          work3de(i,j,ilev,m,1) = w(m,ij,ilev)
          work3de(i,j,ilev,m,2) = evalout(m)
        end do
      else
        if(sp_infl_rtps>0.0d0) then
        call calc_trans(member,nobstotal,nobsl(ij,ilev),w(:,ij,ilev),zxb,trans,parm,&
                pao=pao)
        else
        call calc_trans(member,nobstotal,nobsl(ij,ilev),w(:,ij,ilev),zxb,trans,parm)
        end if
      end if
  if(debug_time) then
    call cpu_time(rtimer)
    write(timeunit,'(A,I5,F12.7)') '### DAS_LMLEF TIMER(CALC_TRANS):',nobsl(ij,ilev),rtimer-rtimer00
    rtimer00=rtimer
  end if
      if(sp_infl_rtpp>0.0d0) then
      !
      ! Relaxation-to-prior-perturbations (Zhang et al. 2004)
      !
      !  write(6,'(A)') '===== Relaxation to prior perturbations ====='
      !  write(6,'(A,F10.4)') '  parameter:',sp_infl_rtpp
      !  write(6,'(A)') '============================================='
        call trans_rtpp(trans,transrlx)
      else
        transrlx = trans
      end if
      if(mean) then
        do k=1,member
          do m=1,member
            transrlx(k,m) = transrlx(k,m) + w(k,ij,ilev)*pscale
          end do
        end do
      end if
      do n=1,nv3d
      ! update control
        anal3dc(i,j,ilev,n) = gues3dc(i,j,ilev,n)
        if(var_update(n)==1) then
          if(.not.mean) then
            do m=1,member
              anal3dc(i,j,ilev,n) = anal3dc(i,j,ilev,n) + gues3d(i,j,ilev,m,n)*w(m,ij,ilev)
            end do
          end if
        end if
      ! update ensemble
        if(sp_infl_rtps>0.0d0.and.var_update(n)==1) then
        !
        ! Relaxation-to-prior-spread (Whitaker and Hamill 2012)
        !
        !  write(6,'(A)') '===== Relaxation to prior spread ====='
        !  write(6,'(A,F10.4)') '  parameter:',sp_infl_rtps
        !  write(6,'(A)') '======================================'
          if(relax_spread_out) then
          call trans_rtps(trans,pao,gues3d(i,j,ilev,:,n),transrlx,rtps3d(i,j,ilev,n))
          else
          call trans_rtps(trans,pao,gues3d(i,j,ilev,:,n),transrlx,tmpbeta)
          end if
          if(mean) then
            do k=1,member
              do m=1,member
                transrlx(k,m) = transrlx(k,m) + w(k,ij,ilev)*pscale
              end do
            end do
          end if
        end if
        do m=1,member
          anal3d(i,j,ilev,m,n) = anal3dc(i,j,ilev,n)
          if(var_update(n)==1) then
            do k=1,member
              anal3d(i,j,ilev,m,n) = anal3d(i,j,ilev,m,n) &
                & + gues3d(i,j,ilev,k,n) * transrlx(k,m) / pscale
            end do
          else
            anal3d(i,j,ilev,m,n) = anal3d(i,j,ilev,m,n) + gues3d(i,j,ilev,m,n) / pscale
          end if
        end do
        ! copy adaptive inflation parameters
        if( n>1 .and. cov_infl_mul < 0.0d0 )then
          work3d(i,j,ilev,n) = work3d(i,j,ilev,1)
        end if
      end do
      if(ilev == 1) then !update 2d variable at ilev=1
        do n=1,nv2d
          ! update control
          anal2dc(i,j,n) = gues2dc(i,j,n)
          if(var_update(nv3d+n)==1) then
            if(.not.mean) then
              do m=1,member
                anal2dc(i,j,n) = anal2dc(i,j,n) + gues2d(i,j,m,n)*w(m,ij,ilev)
              end do
            end if
          end if
          ! update ensemble
          if(sp_infl_rtps>0.0d0.and.var_update(nv3d+n)==1) then
          !
          ! Relaxation-to-prior-spread (Whitaker and Hamill 2012)
          !
          !  write(6,'(A)') '===== Relaxation to prior spread ====='
          !  write(6,'(A,F10.4)') '  parameter:',sp_infl_rtps
          !  write(6,'(A)') '======================================'
            if(relax_spread_out) then
            call trans_rtps(trans,pao,gues2d(i,j,:,n),transrlx,rtps2d(i,j,n))
            else
            call trans_rtps(trans,pao,gues2d(i,j,:,n),transrlx,tmpbeta)
            end if
            if(mean) then
              do k=1,member
                do m=1,member
                  transrlx(k,m) = transrlx(k,m) + w(k,ij,ilev)*pscale
                end do
              end do
            end if
          end if
          do m=1,member
            anal2d(i,j,m,n) = anal2dc(i,j,n)
            if(var_update(nv3d+n)==1) then
              do k=1,member
                anal2d(i,j,m,n) = anal2d(i,j,m,n) &
                & + gues2d(i,j,k,n) * transrlx(k,m) / pscale
              end do
            else
              anal2d(i,j,m,n) = anal2d(i,j,m,n) + gues2d(i,j,m,n) / pscale
            end if
          end do
          ! copy adaptive inflation parameters
          if(cov_infl_mul < 0.0d0) then
            work2d(i,j,n) = work3d(i,j,ilev,1)
          end if
        end do
      end if !ilev==1
      if(exp(logpfm(ij,ilev)) .lt. q_update_top) then !no analysis for upper-level Q and CW
        anal3dc(i,j,ilev,iv3d_q) = gues3dc(i,j,ilev,iv3d_q)
        anal3dc(i,j,ilev,iv3d_cw) = gues3dc(i,j,ilev,iv3d_cw)
        do m=1,member
!          if(save_info) then
!            work3de(i,j,ilev,m,iv3d_q) = 0.0d0
!            work3de(i,j,ilev,m,iv3d_cw) = 0.0d0
!          end if
          anal3d(i,j,ilev,m,iv3d_q) = gues3dc(i,j,ilev,iv3d_q) &
                                 & + gues3d(i,j,ilev,m,iv3d_q) / pscale
          anal3d(i,j,ilev,m,iv3d_cw) = gues3dc(i,j,ilev,iv3d_cw) &
                                 & + gues3d(i,j,ilev,m,iv3d_cw) / pscale
        end do
      else if( q_adjust ) then ! super saturation (dry) adjustment
        if(.not.mean) then !control
          ! specific humidity
          if(anal3dc(i,j,ilev,iv3d_q).lt.0.0d0) then !super dry
            anal3dc(i,j,ilev,iv3d_q)=0.0d0
          else if(anal3dc(i,j,ilev,iv3d_t).gt.(t0-30.0d0).and.&
                  anal3dc(i,j,ilev,iv3d_t).lt.(t0+35.0d0)) then
            rh=1.0d0
            if(nonhyd.eq.1) then
              call calc_q2(anal3dc(i,j,ilev,iv3d_t),rh,anal3dc(i,j,ilev,iv3d_pn),qlim)
            else                       
              call calc_q2(anal3dc(i,j,ilev,iv3d_t),rh,anal2dc(i,j,iv2d_ps)*sig(ilev),qlim)
            end if
            if(anal3dc(i,j,ilev,iv3d_q).gt.qlim) then !super saturation
              anal3dc(i,j,ilev,iv3d_q)=qlim
            end if
          end if
!          ! cloud water
!          if(anal3dc(i,j,ilev,iv3d_cw).lt.0.0d0) then
!            anal3dc(i,j,ilev,iv3d_cw)=0.0d0
!          end if
        end if
        !member
        do m=1,member
          ! specific humidity
          if(anal3d(i,j,ilev,m,iv3d_q).lt.0.0d0) then !super dry
            anal3d(i,j,ilev,m,iv3d_q)=0.0d0
          else if(anal3d(i,j,ilev,m,iv3d_t).gt.(t0-30.0d0).and.&
                  anal3d(i,j,ilev,m,iv3d_t).lt.(t0+35.0d0)) then
            rh=1.0d0
            if(nonhyd.eq.1) then
              call calc_q2(anal3d(i,j,ilev,m,iv3d_t),rh,anal3d(i,j,ilev,m,iv3d_pn),qlim)
            else                       
              call calc_q2(anal3d(i,j,ilev,m,iv3d_t),rh,anal2d(i,j,m,iv2d_ps)*sig(ilev),qlim)
            end if
            if(anal3d(i,j,ilev,m,iv3d_q).gt.qlim) then !super saturation
              anal3d(i,j,ilev,m,iv3d_q)=qlim
            end if
          end if
!          ! cloud water
!          if(anal3d(i,j,ilev,m,iv3d_cw).lt.0.0d0) then
!            anal3d(i,j,ilev,m,iv3d_cw)=0.0d0
!          end if
        end do
      end if ! q limit and/or super saturation (dry) adjustment
  if(debug_time) then
    call cpu_time(rtimer)
    write(timeunit,'(A,2F12.7)') '### DAS_LMLEF TIMER(VAR_UPDATE):',rtimer,rtimer-rtimer00
    rtimer00=rtimer
  end if
    end do !ij=1,nij1
  end do !ilev=1,nlev
  ! (for non-hydrostatic) pn, tt and wn is replaced by hydrostatic 
  if(nonhyd.eq.1) then
    if(var_update(iv3d_pn)==0) then !pn
      do ilev=1,nlev
        do j=1,nj1
          do i=1,ni1
            anal3dc(i,j,ilev,iv3d_pn) = anal2dc(i,j,iv2d_ps) * sig(ilev)
            do m=1,member
              anal3d(i,j,ilev,m,iv3d_pn) = anal2d(i,j,m,iv2d_ps) * sig(ilev)
            end do
          end do
        end do
      end do
    end if !pn
    if(var_update(iv3d_tt)==0) then !tt
      do ilev=1,nlev
        do j=1,nj1
          do i=1,ni1
            anal3dc(i,j,ilev,iv3d_tt) = anal3dc(i,j,ilev,iv3d_t)
            do m=1,member
              anal3d(i,j,ilev,m,iv3d_tt) = anal3d(i,j,ilev,m,iv3d_t)
            end do
          end do
        end do
      end do
    end if !tt
    if(var_update(iv3d_wn)==0) then !wn
      do ilev=1,nlev
        do j=1,nj1
          do i=1,ni1
            anal3dc(i,j,ilev,iv3d_wn) = 0.0_dp
            do m=1,member
              anal3d(i,j,ilev,m,iv3d_wn) = 0.0_dp
            end do
          end do
        end do
      end do
    end if !ww
    if(var_update(nv3d+iv2d_wb)==0) then !wb
      do j=1,nj1
        do i=1,ni1
          anal2dc(i,j,iv2d_wb) = 0.0_dp
          do m=1,member
            anal2d(i,j,m,iv2d_wb) = 0.0_dp
          end do
        end do
      end do
    end if !wb
  end if
  sync all
  ! end optimization
  deallocate( zxb,dep,trans,transrlx,nobsl,nobs_use,rloc )
  deallocate( w,grad,gold,desc,dold,fval,flag )
!  deallocate( gw,ggrad,ggold,gdesc,gdold )
  if(save_info) then
    ! write out information
    call gather_grd(print_img,jwork3d,jwork2d,work3dg,work2dg)
    if(myimage == print_img) then
      write(6,'(A,I3.3,2A)') 'MYIMAGE ',myimage,' is writing a file ',trim(info_out_basename)
      call write_restart(info_out_basename,work3dg,work2dg,convert=.false.)
    end if
    sync all
    deallocate(jwork3d,jwork2d)
    ! write out ensemble weights
    call write_ens(ewgt_basename,work3de,work2de,convert=.false.)
    sync all
    deallocate(work3de,work2de)
  end if
  ! write out adaptive inflation parameters
  if(cov_infl_mul < 0.0d0) then
    call gather_grd(print_img,work3d,work2d,work3dg,work2dg)
    if(myimage == print_img) then
      write(6,'(A,I3.3,2A)') 'MYIMAGE ',myimage,' is writing a file ',trim(infl_mul_out_basename)
      call write_restart(infl_mul_out_basename,work3dg,work2dg,convert=.false.)
    end if
    sync all
    deallocate(work3d,work2d)
  end if
  ! write out RTPS coefficients
  if(relax_spread_out) then
    call gather_grd(print_img,rtps3d,rtps2d,work3dg,work2dg)
    if(myimage == print_img) then
      write(6,'(A,I3.3,2A)') 'MYIMAGE ',myimage,' is writing a file ',trim(relax_spread_out_basename)
      call write_restart(relax_spread_out_basename,work3dg,work2dg,convert=.false.)
    end if
    sync all
    deallocate(work3dg,work2dg,rtps3d,rtps2d)
  end if
  !
  ! Additive inflation
  !
  if(sp_infl_add > 0.0d0) then
    call read_ens(infl_add_in_basename,gues3d,gues2d)
    do n=1,nv3d
      do m=1,member
        do k=1,nlev
          do j=1,nj1
            do i=1,ni1
              gues3d(i,j,k,m,n) = gues3d(i,j,k,m,n) - gues3dc(i,j,k,n)
            end do
          end do
        end do
      end do
    end do
    do n=1,nv2d
      do m=1,member
        do j=1,nj1
          do i=1,ni1
            gues2d(i,j,m,n) = gues2d(i,j,m,n) - gues2dc(i,j,n)
          end do
        end do
      end do
    end do

    write(6,'(A)') '===== Additive covariance inflation ====='
    write(6,'(A,F10.4)') '  parameter:',sp_infl_add
    write(6,'(A)') '========================================='
!    parm = 0.7d0
!    do ilev=1,nlev
!      parm_infl_damp(ilev) = 1.0d0 + parm &
!        & + parm * REAL(1-ilev,kind=dp)/REAL(nlev_dampinfl,kind=dp)
!      parm_infl_damp(ilev) = MAX(parm_infl_damp(ilev),1.0d0)
!    end do
    do n=1,nv3d
      do m=1,member
        do ilev=1,nlev
          do j=1,nj1
            do i=1,ni1
              anal3d(i,j,ilev,m,n) = anal3d(i,j,ilev,m,n) &
                & + gues3d(i,j,ilev,m,n) * sp_infl_add
            end do
          end do
        end do
      end do
    end do
    do n=1,nv2d
      do m=1,member
        do j=1,nj1
          do i=1,ni1
            anal2d(i,j,m,n) = anal2d(i,j,m,n) + gues2d(i,j,m,n) * sp_infl_add
          end do
        end do
      end do
    end do
  end if

  deallocate(logpfm)
  sync all
  if(debug_time) then
    call cpu_time(rtimer)
    write(timeunit,'(A,2F12.7)') '### DAS_LMLEF TIMER(FINALIZE):',rtimer,rtimer-rtimer00
    rtimer00=rtimer
  end if
  return
end subroutine das_lmlefy
!-----------------------------------------------------------------------
! Project global observations to local
!     (hdxf_g,dep_g,rdiag_g) -> (zxb,dep,rdiag)
!-----------------------------------------------------------------------
subroutine obs_local(ij,ilev,nvar,zxb,dep,rloc,nobsl,nobs_use,logpfm,hfirst)
  use func_module, only: distll_1
  use obs_module
  implicit none
  integer,intent(in) :: ij,ilev,nvar
  real(kind=dp),intent(in) :: logpfm(1:nij1max,nlev)[*]
  real(kind=dp),intent(out) :: zxb(nobstotal,member)
  real(kind=dp),intent(out) :: dep(nobstotal)
  real(kind=dp),intent(out) :: rloc(nobstotal)
  integer,intent(inout) :: nobsl
  integer,intent(inout) :: nobs_use(nobstotal)
  LOGICAL,intent(in) :: hfirst
  real(kind=dp) :: minlon,maxlon,minlat,maxlat,dist,dlev
  real(kind=dp) :: tmplon,tmplat,tmplev,tmperr,tmpwgt(nlev)
  integer :: tmpqc
!TVS  integer,allocatable:: ntvs_use_prof(:),ntvs_use_inst(:),ntvs_use_slot(:)
  integer :: i,j
  integer :: imin,imax,jmin,jmax,im,ichan
  integer :: n,nn,tvnn,iobs
!
! INITIALIZE
!
!  if( nobs > 0 ) then
!    allocate(nobs_use(nobs))
!  end if
!TVS  if( ntvs > 0 ) then
!TVS    allocate(ntvs_use_prof(ntvs))
!TVS    allocate(ntvs_use_inst(ntvs))
!TVS    allocate(ntvs_use_slot(ntvs))
!TVS  end if
  i=mod(ij-1,ni1) + 1
  j=(ij-1)/ni1 + 1
if(hfirst) then
  obsdepk = obsdasort%hxf
  do im=1,member
    obshdxk(:,im) = obsdasort%hxe(im,:)
  end do
!
! data search
!
  minlon = myrlon(i) - dlon_zero(j)
  maxlon = myrlon(i) + dlon_zero(j)
  minlat = myrlat(j) - dlat_zero
  maxlat = myrlat(j) + dlat_zero
  if(maxlon - minlon >= 360.0d0) then
    minlon = 0.0d0
    maxlon = 360.0d0
  end if
  if(debug) &
          write(6,'(a,I6,2f10.2,a,4f10.2)') 'ij,rlon,rlat ',ij,myrlon(i),myrlat(j),&
          ' minlon,maxlon,minlat,maxlat ',minlon,maxlon,minlat,maxlat

  do jmin=1,nlat-2
    if(minlat < rlat(jmin+1)) EXIT
  end do
  do jmax=1,nlat-2
    if(maxlat < rlat(jmax+1)) EXIT
  end do
  nn = 1
!TVS  tvnn = 1
  if(minlon >= 0 .and. maxlon <= 360.0) then
    do imin=1,nlon-1
      if(minlon < rlon(imin+1)) EXIT
    end do
    do imax=1,nlon-1
      if(maxlon < rlon(imax+1)) EXIT
    end do
    if( obsdasort%nobs > 0 ) &
    & call obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS    if( ntvs > 0 ) &
!TVS    & call tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS    &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
  else if(minlon >= 0 .and. maxlon > 360.0) then
    do imin=1,nlon-1
      if(minlon < rlon(imin+1)) EXIT
    end do
    maxlon = maxlon - 360.0d0
    if(maxlon > 360.0d0) then
      imin = 1
      imax = nlon
      if( obsdasort%nobs > 0 ) &
      & call obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS      if( ntvs > 0 ) &
!TVS      & call tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS      &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
    else
      do imax=1,nlon-1
        if(maxlon < rlon(imax+1)) EXIT
      end do
      if(imax > imin) then
        imin = 1
        imax = nlon
        if( obsdasort%nobs > 0 ) &
        & call obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS        if( ntvs > 0 ) &
!TVS        & call tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS        &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
      else
        imin = 1
        if( obsdasort%nobs > 0 ) &
        & call obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS        if( ntvs > 0 ) &
!TVS        & call tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS        &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
        do imin=1,nlon-1
          if(minlon < rlon(imin+1)) EXIT
        end do
        imax = nlon
        if( obsdasort%nobs > 0 ) &
        & call obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS        if( ntvs > 0 ) &
!TVS        & call tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS        &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
      end if
    end if
  else if(minlon < 0 .and. maxlon <= 360.0d0) then
    do imax=1,nlon-1
      if(maxlon < rlon(imax+1)) EXIT
    end do
    minlon = minlon + 360.0d0
    if(minlon < 0) then
      imin = 1
      imax = nlon
      if( obsdasort%nobs > 0 ) &
      & call obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS      if( ntvs > 0 ) &
!TVS      & call tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS      &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
    else
      do imin=1,nlon-1
        if(minlon < rlon(imin+1)) EXIT
      end do
      if(imin < imax) then
        imin = 1
        imax = nlon
        if( obsdasort%nobs > 0 ) &
        & call obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS        if( ntvs > 0 ) &
!TVS        & call tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS        &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
      else
        imin = 1
        if( obsdasort%nobs > 0 ) &
        & call obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS        if( ntvs > 0 ) &
!TVS        & call tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS        &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
        do imin=1,nlon-1
          if(minlon < rlon(imin+1)) EXIT
        end do
        imax = nlon
        if( obsdasort%nobs > 0 ) &
        & call obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS        if( ntvs > 0 ) &
!TVS        & call tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS        &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
      end if
    end if
  else
    maxlon = maxlon - 360.0d0
    minlon = minlon + 360.0d0
    if(maxlon > 360.0 .or. minlon < 0) then
      imin = 1
      imax = nlon
      if( obsdasort%nobs > 0 ) &
      & call obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS      if( ntvs > 0 ) &
!TVS      & call tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS      &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
    else
      do imin=1,nlon-1
        if(minlon < rlon(imin+1)) EXIT
      end do
      do imax=1,nlon-1
        if(maxlon < rlon(imax+1)) EXIT
      end do
      if(imin > imax) then
        imin = 1
        imax = nlon
        if( obsdasort%nobs > 0 ) &
        & call obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS        if( ntvs > 0 ) &
!TVS        & call tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS        &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
      else
        if( obsdasort%nobs > 0 ) &
        & call obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
!TVS        if( ntvs > 0 ) &
!TVS        & call tvs_local_sub(imin,imax,jmin,jmax,tvnn, &
!TVS        &                    ntvs_use_prof,ntvs_use_inst,ntvs_use_slot)
      end if
    end if
  end if
  nn = nn-1
!TVS  tvnn = tvnn -1
!TVS  if( nn < 1 .and. tvnn < 1 ) then
  if(nn < 1) then
    nobsl = 0
    return
  end if
!
! CONVENTIONAL
!
  nobsl = 0
  if(nn > 0) then
    do n=1,nn
      !
      ! vertical localization
      !
      if(obsdasort%elem(nobs_use(n)) > 10000 .and. ilev > 1) then
        tmplev = LOG(obsdasort%dat(nobs_use(n)))
        dlev = ABS(tmplev - logpfm(ij,ilev))
      else if(obsdasort%elem(nobs_use(n)) < 10000) then
        tmplev = LOG(obsdasort%lev(nobs_use(n)))
        dlev = ABS(tmplev - logpfm(ij,ilev))
      else
        dlev = 0.0d0
      end if
      if(dlev > dist_zerov) CYCLE
      !
      ! horizontal localization
      !
      tmplon=obsdasort%lon(nobs_use(n))
      tmplat=obsdasort%lat(nobs_use(n))
      call distll_1( tmplon, tmplat,myrlon(i), myrlat(j), dist)
      if(dist > dist_zero ) CYCLE
!      !
!      ! variable localization
!      !
!      iobs = uid_obs(obsdasort%elem(nobs_use(n)))
!      SELECT CASE(obsdasort%elem(nobs_use(n)))
!      CASE(id_u_obs)
!        iobs=1
!      CASE(id_v_obs)
!        iobs=2
!      CASE(id_t_obs)
!        iobs=3
!      CASE(id_q_obs)
!        iobs=4
!      CASE(id_rh_obs)
!        iobs=5
!      CASE(id_ps_obs)
!        iobs=6
!      CASE(id_rain_obs)
!        iobs=7
!      end SELECT
!      if(var_local(nvar,iobs) < TINY(var_local)) CYCLE

      nobsl = nobsl + 1
      zxb(nobsl,:)  = obshdxk(nobs_use(n),:)*pscale
      dep(nobsl)    = obsdepk(nobs_use(n))
      !
      ! Observational localization
      !
      tmperr=obsdasort%err(nobs_use(n))
      rloc(nobsl) =EXP(-0.5d0 * ((dist/sigma_obs)**2 + (dlev/sigma_obsv)**2))! &
      !            & * var_local(nvar,iobs)
      zxb(nobsl,:) = zxb(nobsl,:) / tmperr * SQRT(rloc(nobsl))
      dep(nobsl)   = dep(nobsl)   / tmperr * SQRT(rloc(nobsl))
      nobs_use(nobsl) = nobs_use(n)
    end do
  end if
else
  do n=1,nobsl
    !
    ! vertical localization
    !
    if(obsdasort%elem(nobs_use(n)) > 10000 .and. ilev > 1) then
      tmplev = LOG(obsdasort%dat(nobs_use(n)))
      dlev = ABS(tmplev - logpfm(ij,ilev))
    else if(obsdasort%elem(nobs_use(n)) < 10000) then
      tmplev = LOG(obsdasort%lev(nobs_use(n)))
      dlev = ABS(tmplev - logpfm(ij,ilev))
    else
      dlev = 0.0d0
    end if
    !
    ! horizontal localization
    !
    tmplon=obsdasort%lon(nobs_use(n))
    tmplat=obsdasort%lat(nobs_use(n))
    call distll_1( tmplon, tmplat, myrlon(i), myrlat(j), dist)
    !
    zxb(n,:)  = obshdxk(nobs_use(n),:)
    dep(n)    = obsdepk(nobs_use(n))
    !
    ! Observational localization
    !
    tmperr=obsdasort%err(nobs_use(n))
    rloc(n) =EXP(-0.5d0 * ((dist/sigma_obs)**2 + (dlev/sigma_obsv)**2))! &
    !            & * var_local(nvar,iobs)
    zxb(n,:) = zxb(n,:) / tmperr * SQRT(rloc(n))
    dep(n)   = dep(n)   / tmperr * SQRT(rloc(n))
  end do 
end if
if(debug) then
  write(6,'(A, L2, A, I4)') 'hfirst=',hfirst,' nobsl=', nobsl
  if(nobsl > 0) then
  write(cn,'(I4)') nobsl
  write(6,'(A9,'//trim(cn)//'I12)') 'nobs_use=', nobs_use(1:nobsl)
  write(6,'(A9,'//trim(cn)//'ES12.4)') 'dep=', dep(1:nobsl)
  end if
end if
!TVS!
!TVS! ATOVS
!TVS!
!TVS  if(tvnn > 0) then
!TVS    do n=1,tvnn
!TVS      tmplon=tvslon(ntvs_use_prof(n),ntvs_use_inst(n),ntvs_use_slot(n))
!TVS      tmplat=tvslat(ntvs_use_prof(n),ntvs_use_inst(n),ntvs_use_slot(n))
!TVS      call com_distll_1( tmplon, tmplat, lon1(ij), lat1(ij), dist)
!TVS      if( dist > dist_zero) CYCLE
!TVS
!TVS      do ichan=1,ntvsch(ntvs_use_inst(n))
!TVS        tmperr=tvserr(ichan,ntvs_use_prof(n),ntvs_use_inst(n),ntvs_use_slot(n))
!TVS        tmpqc=tvsqc(ichan,ntvs_use_prof(n),ntvs_use_inst(n),ntvs_use_slot(n))
!TVS        tmpwgt(:)=tvswgt(:,ichan, &
!TVS                         & ntvs_use_prof(n), &
!TVS                         & ntvs_use_inst(n), &
!TVS                         & ntvs_use_slot(n))
!TVS        if( tmpqc == 1 .and. tmpwgt(ilev) > 0.05D0 ) then
!TVS          nobsl = nobsl + 1
!TVS          do im = 1, member
!TVS            hdxf(nobsl,im) = tvshdxf(im,ichan, &
!TVS                              & ntvs_use_prof(n), &
!TVS                              & ntvs_use_inst(n), &
!TVS                              & ntvs_use_slot(n))
!TVS          end do
!TVS          dep(nobsl)    = tvsdep(ichan, &
!TVS                              & ntvs_use_prof(n), &
!TVS                              & ntvs_use_inst(n), &
!TVS                              & ntvs_use_slot(n))
!TVS          rdiag(nobsl)  = tmperr * tmperr &
!TVS                        & * exp(0.5d0 * (dist/sigma_obs)**2) &
!TVS                        & / (tmpwgt(ilev) * tmpwgt(ilev))
!TVS        end if
!TVS      end do
!TVS    end do
!TVS  end if
!
! DEBUG
! if( ILEV == 1 .and. ILON == 1 ) &
! & write(6,*) 'ILEV,ILON,ILAT,NN,TVNN,NOBSL=',ilev,ij,nn,tvnn,nobsl
!
  if( nobsl > nobstotal ) then
    write(6,'(A,I5,A,I5)') 'FATAL ERROR, NOBSL=',nobsl,' > NOBSTOTAL=',nobstotal
    write(6,*) 'IJ,NN,TVNN=', ij, nn, tvnn
    STOP 99
  end if
!
!  if( nobs > 0 ) then
!    deallocate(nobs_use)
!  end if
!TVS  if( ntvs > 0 ) then
!TVS    deallocate(ntvs_use_prof)
!TVS    deallocate(ntvs_use_inst)
!TVS    deallocate(ntvs_use_slot)
!TVS  end if
!
  return
end subroutine obs_local

subroutine obs_local_sub(imin,imax,jmin,jmax,nn,nobs_use)
  integer,intent(in) :: imin,imax,jmin,jmax
  integer,intent(inout) :: nn, nobs_use(nobs)
  integer :: j,n,ib,ie,ip

  do j=jmin,jmax
    if(imin > 1) then
      ib = nobsgrd(imin-1,j)+1
    else
      if(j > 1) then
        ib = nobsgrd(nlon,j-1)+1
      else
        ib = 1
      end if
    end if
    ie = nobsgrd(imax,j)
    n = ie - ib + 1
    if(n == 0) CYCLE
    do ip=ib,ie
      if(nn > nobs) then
        write(6,*) 'FATALERROR, NN > NOBS', NN, NOBS
      end if
      nobs_use(nn) = ip
      nn = nn + 1
    end do
  end do

  return
end subroutine obs_local_sub
!-----------------------------------------------------------------------
! Calculate Z & departure at current iteration
!-----------------------------------------------------------------------
subroutine obs_update( gues3dc,gues2dc,gues3d,gues2d,w )
    use obs_module
    use obsope_module, only: obsope_update, monit_dep, monit_print
!    use common_obs_speedy_tl, only: Trans_XtoY_tl
    implicit none
!    integer,intent(in) :: member ! ensemble size
    real(kind=dp),intent(in) :: gues3dc(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d)[*]    !control or mean
    real(kind=dp),intent(in) :: gues2dc(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nv2d)[*]         !control or mean
    real(kind=dp),intent(in) :: gues3d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,member,nv3d)[*] !ensemble perturbation
    real(kind=dp),intent(in) :: gues2d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,member,nv2d)[*]      !ensemble perturbation
    real(kind=dp),intent(in) :: w(member,nij1max,nlev)[*]           !ensemble weights
    real(kind=dp),allocatable :: tmphxf(:,:)[:]
    real(kind=dp),allocatable :: v3d(:,:,:,:),v2d(:,:,:)
    real(kind=dp),allocatable :: v3dp(:,:,:,:),v2dp(:,:,:)
    real(kind=dp),allocatable :: work3d(:,:,:,:), work2d(:,:,:)
    real(kind=dp),allocatable :: work3de(:,:,:,:), work2de(:,:,:)
    real(kind=dp),allocatable :: p_full(:,:,:)
    integer       :: i,j,ii,jj,ij,ijg,ilev, m, n, k 
! monitor
    integer :: monit_nobs(nobstype)
    real(kind=dp) :: monit_bias(nobstype)
    real(kind=dp) :: monit_rmse(nobstype)
    integer :: monit_nqc(nobstype,nqctype)
  
    allocate( tmphxf(obsdasort%nobs,0:member)[*] )
    allocate( work3d (1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d) )
    allocate( work2d (1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,     nv2d) )
    allocate( work3de(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d) )
    allocate( work2de(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,     nv2d) )
    allocate( p_full (1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev) )
    allocate( v3d(nlon,nlat,nlev,nv3d), v2d(nlon,nlat,nv2d) )
    allocate( v3dp(nlon,nlat,nlev,nv3d), v2dp(nlon,nlat,nv2d) )
    ! full pressure levels (identical to all members)
    if(nonhyd.eq.1) then !non-hydrostatic
      p_full(:,:,:) = gues3dc(:,:,:,iv3d_pn)
    else
      call calc_pfull(ni1max+2*ighost,nj1max+2*jghost,nlev,sig,gues2dc(:,:,iv2d_ps),p_full)
    end if
    ! update control
    work3d = gues3dc
    work2d = gues2dc
    do n=1,nv3d
      if(var_update(n)/=1) cycle
      print *, varnames(n)
      do ilev=1,nlev
        do ij=1,nij1
          i=mod(ij-1,ni1) + 1
          j=(ij-1)/ni1 + 1
          do m=1,member
            work3d(i,j,ilev,n) = work3d(i,j,ilev,n) + gues3d(i,j,ilev,m,n)*w(m,ij,ilev)
          end do
        end do
        ! ghost point
        if(nisep.gt.1.and.ighost.gt.0) then
          do j=1,nj1
            if(nidom(myimage).gt.1) then
              do ii=1,ighost
                ijg=(j-1)*ni1node(imgdom(nidom(myimage)-1,njdom(myimage)))&
                        +ni1node(imgdom(nidom(myimage)-1,njdom(myimage)))-ii+1
                do m=1,member
                  work3d(1-ii,j,ilev,n) = work3d(1-ii,j,ilev,n) &
                          + gues3d(1-ii,j,ilev,m,n)*w(m,ijg,ilev)[imgdom(nidom(myimage)-1,njdom(myimage))]
                end do
              end do
            end if
            if(nidom(myimage).lt.nisep) then
              do ii=1,ighost
                ijg=(j-1)*ni1node(imgdom(nidom(myimage)+1,njdom(myimage)))&
                        +ii
                do m=1,member
                  work3d(ni1+ii,j,ilev,n) = work3d(ni1+ii,j,ilev,n) &
                          + gues3d(ni1+ii,j,ilev,m,n)*w(m,ijg,ilev)[imgdom(nidom(myimage)+1,njdom(myimage))]
                end do
              end do
            end if
          end do
        end if
        if(njsep.gt.1.and.jghost.gt.0) then
          do i=1,ni1
            if(njdom(myimage).gt.1) then
              do jj=1,jghost
                ijg=(nj1node(imgdom(nidom(myimage),njdom(myimage)-1))-jj)&
                        *ni1node(imgdom(nidom(myimage),njdom(myimage)-1))+i
                do m=1,member
                  work3d(i,1-jj,ilev,n) = work3d(i,1-jj,ilev,n) &
                        + gues3d(i,1-jj,ilev,m,n)*w(m,ijg,ilev)[imgdom(nidom(myimage),njdom(myimage)-1)]
                end do
              end do
            end if
            if(njdom(myimage).lt.njsep) then
              do jj=1,jghost
                ijg=(jj-1)&
                        *ni1node(imgdom(nidom(myimage),njdom(myimage)+1))+i
                do m=1,member
                  work3d(i,nj1+jj,ilev,n) = work3d(i,nj1+jj,ilev,n) &
                        + gues3d(i,nj1+jj,ilev,m,n)*w(m,ijg,ilev)[imgdom(nidom(myimage),njdom(myimage)+1)]
                end do
              end do
            end if
          end do
        end if
        ! ghost point
      end do
    end do
    do n=1,nv2d
      if(var_update(nv3d+n)/=1) cycle
      print *, varnames(nv3d+n)
      do ij=1,nij1
        i=mod(ij-1,ni1) + 1
        j=(ij-1)/ni1 + 1
        do m=1,member
          work2d(i,j,n) = work2d(i,j,n) + gues2d(i,j,m,n)*w(m,ij,1)
        end do
      end do
      ! ghost point
      if(nisep.gt.1.and.ighost.gt.0) then
        do j=1,nj1
          if(nidom(myimage).gt.1) then
            do ii=1,ighost
              ijg=(j-1)*ni1node(imgdom(nidom(myimage)-1,njdom(myimage)))&
                      +ni1node(imgdom(nidom(myimage)-1,njdom(myimage)))-ii+1
              do m=1,member
                work2d(1-ii,j,n) = work2d(1-ii,j,n) &
                        + gues2d(1-ii,j,m,n)*w(m,ijg,1)[imgdom(nidom(myimage)-1,njdom(myimage))]
              end do
            end do
          end if
          if(nidom(myimage).lt.nisep) then
            do ii=1,ighost
              ijg=(j-1)*ni1node(imgdom(nidom(myimage)+1,njdom(myimage)))+ii
              do m=1,member
                work2d(ni1+ii,j,n) = work2d(ni1+ii,j,n) &
                        + gues2d(ni1+ii,j,m,n)*w(m,ijg,1)[imgdom(nidom(myimage)+1,njdom(myimage))]
              end do
            end do
          end if
        end do
      end if
      if(njsep.gt.1.and.jghost.gt.0) then
        do i=1,ni1
          if(njdom(myimage).gt.1) then
            do jj=1,jghost
              ijg=(nj1node(imgdom(nidom(myimage),njdom(myimage)-1))-jj)&
                      *ni1node(imgdom(nidom(myimage),njdom(myimage)-1))+i
              do m=1,member
                work2d(i,1-jj,n) = work2d(i,1-jj,n) &
                      + gues2d(i,1-jj,m,n)*w(m,ijg,1)[imgdom(nidom(myimage),njdom(myimage)-1)]
              end do
            end do
          end if
          if(njdom(myimage).lt.njsep) then
            do jj=1,jghost
              ijg=(jj-1)*ni1node(imgdom(nidom(myimage),njdom(myimage)+1))+i
              do m=1,member
                work2d(i,nj1+jj,n) = work2d(i,nj1+jj,n) &
                      + gues2d(i,nj1+jj,m,n)*w(m,ijg,1)[imgdom(nidom(myimage),njdom(myimage)+1)]
              end do
            end do
          end if
        end do
      end if
      ! ghost point
    end do
    if(.not.mean) then
      call obsope_update(obsdasort,0,work3d,work2d,p_full)
      tmphxf(:,0) = obsdasort%hxf(:)
    end if
    ! ensemble
    do m=1,member
      work3de = gues3d(:,:,:,m,:) + work3d
      work2de = gues2d(:,:,m,:) + work2d
      call obsope_update(obsdasort,m,work3de,work2de,p_full)
      tmphxf(:,m) = obsdasort%hxe(m,:)
    end do
    sync all
  
    ! broadcast
    if(mean) then
      tmphxf(:,0) = tmphxf(:,1)
      do m=2,member
        tmphxf(:,0) = tmphxf(:,0) + tmphxf(:,m)
      end do
      tmphxf(:,0) = tmphxf(:,0) / real(member,kind=dp)
    end if
    if(myimage == print_img) then
      do k=1,nimages
        if(k.eq.myimage) cycle
        tmphxf(:,:)[myimage] = tmphxf(:,:)[myimage] + tmphxf(:,:)[k]
      end do
      do k=1,nimages
        tmphxf(:,:)[k] = tmphxf(:,:)[myimage]
      end do
    end if
    sync all
!! departure
    do n=1,obsdasort%nobs
      if(debug) then
        write(6,'(2(A,I4))') 'nobs ', n, ' / ', obsdasort%nobs
        if(zupd.or.mean) then
          write(cn,'(I4)') member+1
          write(6,'(A,'//trim(cn)//'ES12.5)') 'hx =', tmphxf(n,:)
        else
          write(6,'(A,ES12.5)') 'hx =', tmphxf(n,0)
        end if
      end if
      ! update
      obsdepk(n) = obsdasort%dat(n) - tmphxf(n,0)
      if(zupd) then
        if(tl) then
          obshdxk(n,:) = tmphxf(n,1:member)
        else
          obshdxk(n,:) = tmphxf(n,1:member) - tmphxf(n,0)
        end if
      end if
      if(obsdasort%elem(n).eq.id_wd_obs) then !wind direction
        if(abs(obsdepk(n)).gt.180.0d0) then
          obsdepk(n)=obsdepk(n)-sign(360.0d0,obsdepk(n))
        end if
        do m=1,member
          if(abs(obshdxk(n,m)).gt.180.0d0) then
            obshdxk(n,m)=obshdxk(n,m)-sign(360.0d0,obshdxk(n,m))
          end if
        end do
      end if
      obsdasort%hxf(n) = obsdepk(n)
      do m=1,member
        obsdasort%hxe(m,n) = obshdxk(n,m)
      end do
    end do
    if(oma_monit) then
      call monit_dep(obsdasort%nobs,obsdasort%elem,obsdasort%hxf,obsdasort%qc,&
       & monit_nobs,monit_bias,monit_rmse,monit_nqc)
      call monit_print(monit_nobs,monit_bias,monit_rmse,monit_nqc)
    end if
    deallocate( tmphxf,work3d,work2d,work3de,work2de,p_full )
    return 
  end subroutine obs_update
!-----------------------------------------------------------------------
! Calculate global cost
!-----------------------------------------------------------------------
  subroutine global_cost(ne,ngrd,w,parm_infl,fglb)
    implicit none
    integer,intent(in) :: ne,ngrd
    real(kind=dp),intent(in) :: w(ne)
    real(kind=dp),intent(in) :: parm_infl(ngrd)
    real(kind=dp),intent(out):: fglb
    real(kind=dp) :: jb, jo, rho, rinv
    integer :: i,j,k

    fglb=0.0d0
    jb=0.0d0
    k=ne/ngrd
    if(debug) print *, 'member:',k
    do j=1,ngrd
      rho = 1.0d0 / (1.0d0 + parm_infl(j) )
      do i=1,k
        jb=jb+w(i+(j-1)*k)*w(i+(j-1)*k)*rho
      end do
    end do
    jb=jb*0.5d0
    if(jout) write(6,'(A,I4,A,ES12.5)') 'MYIMAGE ',myimage,' Jb(global)=',jb
    jo=0.0d0
    do i=1,nobstotal
      if(obsdasort%img(i)/=myimage) cycle
      rinv=1.0d0/obsdasort%err(i)/obsdasort%err(i)
      jo=jo+obsdasort%hxf(i)*obsdasort%hxf(i)*rinv
    end do
    jo=jo*0.5d0
    if(jout) write(6,'(A,I4,A,ES12.5)') 'MYIMAGE ',myimage,' Jo(global)=',jo
    fglb=jb+jo
    if(jout) write(6,'(A,I4,A,ES12.5)') 'MYIMAGE ',myimage,' J(global)=',fglb
    return
  end subroutine global_cost
!-----------------------------------------------------------------------
! Relaxation via transform matrix - RTPP method
!-----------------------------------------------------------------------
  subroutine trans_rtpp(trans,transrlx)
    implicit none
    real(kind=dp),intent(in) :: trans(member,member)
    real(kind=dp),intent(out):: transrlx(member,member)
    integer :: m

    transrlx = (1.0_dp - sp_infl_rtpp) * trans
    do m=1,member
      transrlx(m,m) = transrlx(m,m) + sp_infl_rtpp
    end do
    return
  end subroutine trans_rtpp
!-----------------------------------------------------------------------
! Relaxation via transform matrix - RTPS method
!-----------------------------------------------------------------------
  subroutine trans_rtps(trans,pao,spf,transrlx,beta)
    implicit none
    real(kind=dp),intent(in) :: trans(member,member)
    real(kind=dp),intent(in) :: pao(member,member)
    real(kind=dp),intent(in) :: spf(member)
    real(kind=dp),intent(out):: transrlx(member,member)
    real(kind=dp),intent(out) :: beta
    real(kind=dp) :: vf,va
    integer :: k,m

    vf = 0.0_dp
    va = 0.0_dp
    do m=1,member
      vf = vf + spf(m)*spf(m)
      do k=1,member
        va = va + spf(k)*pao(k,m)*spf(m)
      end do
    end do
    if (vf.gt.0.0_dp .and. va.gt.0.0_dp) then
      beta = sp_infl_rtps * sqrt(vf / va) + 1.0_dp - sp_infl_rtps
      transrlx = beta * trans
    else
      beta = 1.0_dp
      transrlx = trans
    end if
    return
  end subroutine trans_rtps
!TVSsubroutine tvs_local_sub(imin,imax,jmin,jmax,nn,ntvs_prof,ntvs_inst,ntvs_slot)
!TVS  integer,intent(in) :: imin,imax,jmin,jmax
!TVS  integer,intent(inout) :: nn, ntvs_prof(ntvs), ntvs_inst(ntvs), ntvs_slot(ntvs)
!TVS  integer :: j,n,ib,ie,ip
!TVS  integer :: islot, iinst
!TVS
!TVS  do j=jmin,jmax
!TVS    do islot=1,nslots
!TVS      do iinst=1,ninstrument
!TVS        if(imin > 1) then
!TVS          ib = ntvsgrd(imin-1,j,iinst,islot)+1
!TVS        else
!TVS          if(j > 1) then
!TVS            ib = ntvsgrd(nlon,j-1,iinst,islot)+1
!TVS          else
!TVS            ib = 1
!TVS          end if
!TVS        end if
!TVS        ie = ntvsgrd(imax,j,iinst,islot)
!TVS        n = ie - ib + 1
!TVS        if(n == 0) CYCLE
!TVS        do ip=ib,ie
!TVS          if(nn > nobs) then
!TVS            write(6,*) 'FATALERROR, NN > NTVS', NN, NTVS
!TVS          end if
!TVS          ntvs_prof(nn)=ip
!TVS          ntvs_inst(nn)=iinst
!TVS          ntvs_slot(nn)=islot
!TVS          nn = nn + 1
!TVS        end do
!TVS      end do
!TVS    end do
!TVS  end do
!TVS  return
!TVSend subroutine tvs_local_sub
!TVS!-----------------------------------------------------------------------
!TVS! Data Assimilation for VARBC
!TVS!-----------------------------------------------------------------------
!TVSsubroutine das_vbc(um,vm,tm,qm,qlm,psm,vbcf,vbca)
!TVS  USE common_mtx
!TVS  IMPLICIT NONE
!TVS  real(kind=dp),intent(in) :: um(nij1,nlev)
!TVS  real(kind=dp),intent(in) :: vm(nij1,nlev)
!TVS  real(kind=dp),intent(in) :: tm(nij1,nlev)
!TVS  real(kind=dp),intent(in) :: qm(nij1,nlev)
!TVS  real(kind=dp),intent(in) :: qlm(nij1,nlev)
!TVS  real(kind=dp),intent(in) :: psm(nij1)
!TVS  real(kind=dp),intent(inout) :: vbcf(maxvbc,maxtvsch,ninstrument)
!TVS  real(kind=dp),intent(out)   :: vbca(maxvbc,maxtvsch,ninstrument)
!TVS  REAL(r_sngl) :: u4(nlon,nlat,nlev)
!TVS  REAL(r_sngl) :: v4(nlon,nlat,nlev)
!TVS  REAL(r_sngl) :: t4(nlon,nlat,nlev)
!TVS  REAL(r_sngl) :: q4(nlon,nlat,nlev)
!TVS  REAL(r_sngl) :: ql4(nlon,nlat,nlev)
!TVS  REAL(r_sngl) :: ps4(nlon,nlat)
!TVS  real(kind=dp) :: u(nlon,nlat,nlev)
!TVS  real(kind=dp) :: v(nlon,nlat,nlev)
!TVS  real(kind=dp) :: t(nlon,nlat,nlev)
!TVS  real(kind=dp) :: q(nlon,nlat,nlev)
!TVS  real(kind=dp) :: ql(nlon,nlat,nlev)
!TVS  real(kind=dp) :: ps(nlon,nlat)
!TVS  real(kind=dp) :: p_full(nlon,nlat,nlev)
!TVS  real(kind=dp),allocatable :: hx(:,:,:,:)
!TVS  real(kind=dp),allocatable :: pred(:,:,:,:,:)
!TVS  integer,allocatable :: tmpqc(:,:,:)
!TVS  real(kind=dp),allocatable :: tmpwgt(:,:,:,:)
!TVS  real(kind=dp) :: a(maxvbc,maxvbc)
!TVS  real(kind=dp) :: b(maxvbc)
!TVS  real(kind=dp) :: ainv(maxvbc,maxvbc)
!TVS  integer:: ntvschan1(maxtvsch,ninstrument)
!TVS  integer:: i,j,k,n,islot,nn
!TVS
!TVS  print *,'Hello from das_vbc'
!TVS
!TVS  if(ntvs == 0) then
!TVS    print *,'No radiance data: das_vbc skipped..'
!TVS!$OMP PARALLEL WORKSHARE
!TVS    vbca = vbcf
!TVS!$OMP end PARALLEL WORKSHARE
!TVS    return
!TVS  end if
!TVS
!TVS  call gather_grd_co(0,um,vm,tm,qm,qlm,psm,u4,v4,t4,q4,ql4,ps4)
!TVS  n = nlon*nlat*nlev
!TVS  call MPI_BARRIER(MPI_COMM_WORLD,i)
!TVS  call MPI_BCAST(u4(1,1,1),n,MPI_REAL,0,MPI_COMM_WORLD,i)
!TVS  call MPI_BCAST(v4(1,1,1),n,MPI_REAL,0,MPI_COMM_WORLD,i)
!TVS  call MPI_BCAST(t4(1,1,1),n,MPI_REAL,0,MPI_COMM_WORLD,i)
!TVS  call MPI_BCAST(q4(1,1,1),n,MPI_REAL,0,MPI_COMM_WORLD,i)
!TVS  call MPI_BCAST(ql4(1,1,1),n,MPI_REAL,0,MPI_COMM_WORLD,i)
!TVS  n = nlon*nlat
!TVS  call MPI_BCAST(ps4(1,1),n,MPI_REAL,0,MPI_COMM_WORLD,i)
!TVS!$OMP PARALLEL WORKSHARE
!TVS  u = REAL(u4,kind=dp)
!TVS  v = REAL(v4,kind=dp)
!TVS  t = REAL(t4,kind=dp)
!TVS  q = REAL(q4,kind=dp)
!TVS  ql = REAL(ql4,kind=dp)
!TVS  ps = REAL(ps4,kind=dp)
!TVS!$OMP end PARALLEL WORKSHARE
!TVS  call calc_pfull(ps,p_full)
!TVS
!TVS  allocate( hx(maxtvsch,maxtvsprof,ninstrument,nslots) )
!TVS  allocate( pred(maxvbc,maxtvsch,maxtvsprof,ninstrument,nslots) )
!TVS  allocate( tmpqc(maxtvsch,maxtvsprof,ninstrument) )
!TVS  allocate( tmpwgt(nlev,maxtvsch,maxtvsprof,ninstrument) )
!TVS  do islot=1,nslots
!TVS!    if(SUM(ntvsprofslots(:,islot)) == 0) CYCLE
!TVS    ntvsprof(:) = ntvsprofslots(:,islot)
!TVS    call Trans_XtoY_tvs(u,v,t,q,ql,ps,p_full, &
!TVS      & tvslon(:,:,islot),tvslat(:,:,islot),tvszenith(:,:,islot),&
!TVS      & tvsskin(:,:,islot),tvsstmp(:,:,islot),tvsclw(:,:,islot),&
!TVS      & tvsemis(:,:,:,islot),tmpqc,hx(:,:,:,islot),tmpwgt,pred(:,:,:,:,islot))
!TVS  end do
!TVS  deallocate(tmpqc,tmpwgt)
!TVS
!TVS!$OMP PARALLEL PRIVATE(j,k,n,a,b,ainv)
!TVS!$OMP WORKSHARE
!TVS  vbca = 0.0d0
!TVS!$OMP end WORKSHARE
!TVS!$OMP do SCHEDULE(DYNAMIC)
!TVS  do k=1,ninstrument
!TVS    do j=1,maxtvsch
!TVS      !
!TVS      ! Parallel processing
!TVS      !
!TVS      if(MOD(j+maxtvsch*(k-1)-1,nprocs) /= myrank) CYCLE
!TVS      !
!TVS      ! DATA NUMBER
!TVS      !
!TVS      ntvschan(j,k) = SUM(tvsqc(j,:,k,:))
!TVS      if(msw_vbc .and. ntvschan(j,k) /= 0 ) then
!TVS        print '(3A,I3,A,I6)',' >> VBC executed for instrument,channel,ntvsl: ',&
!TVS                            & tvsname(k),',',tvsch(j,k),',',ntvschan(j,k)
!TVS        call vbc_local(j,k,ntvschan(j,k),hx,pred,a,b)
!TVS        call mtx_inv(maxvbc,a,ainv)
!TVS        vbca(:,j,k) = vbcf(:,j,k)
!TVS        do n=1,maxvbc
!TVS          vbca(:,j,k) = vbca(:,j,k) - ainv(:,n)*b(n) !ATTN: sign for beta
!TVS        end do
!TVS      else
!TVS        print '(3A,I3,A,I6)',' !! NO VBC executed for instrument,channel,ntvsl: ',&
!TVS                            & tvsname(k),',',tvsch(j,k),',',ntvschan(j,k)
!TVS        vbca(:,j,k) = vbcf(:,j,k)
!TVS      end if
!TVS    end do
!TVS  end do
!TVS!$OMP end do
!TVS!$OMP WORKSHARE
!TVS  vbcf = vbca
!TVS  ntvschan1 = ntvschan
!TVS!$OMP end WORKSHARE
!TVS!$OMP end PARALLEL
!TVS  deallocate(hx,pred)
!TVS  n = maxvbc*maxtvsch*ninstrument
!TVS  call MPI_BARRIER(MPI_COMM_WORLD,j)
!TVS  call MPI_ALLREDUCE(vbcf,vbca,n,MPI_doUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,j)
!TVS  n = maxtvsch*ninstrument
!TVS  call MPI_BARRIER(MPI_COMM_WORLD,j)
!TVS  call MPI_ALLREDUCE(ntvschan1,ntvschan,n,MPI_integer,MPI_SUM,MPI_COMM_WORLD,j)
!TVS
!TVS  return
!TVSend subroutine das_vbc
!TVS!-----------------------------------------------------------------------
!TVS!  (in) ichan: channnel
!TVS!  (in) iinst: sensor
!TVS!  (out) a = B_beta^-1 + p R^-1 p^T
!TVS!  (out) b = p R^-1 d
!TVS!-----------------------------------------------------------------------
!TVSsubroutine vbc_local(ichan,iinst,ntvsl,hx,pred,a,b)
!TVS  IMPLICIT NONE
!TVS  integer,PARAMETER :: msw=1
!TVS  integer,PARAMETER :: nmin=400
!TVS  integer,intent(in) :: ichan,iinst,ntvsl
!TVS  real(kind=dp),intent(in) :: hx(maxtvsch,maxtvsprof,ninstrument,nslots)
!TVS  real(kind=dp),intent(in) :: pred(maxvbc,maxtvsch,maxtvsprof,ninstrument,nslots)
!TVS  real(kind=dp),intent(out) :: a(maxvbc,maxvbc)
!TVS  real(kind=dp),intent(out) :: b(maxvbc)
!TVS  real(kind=dp) :: dep,dep0
!TVS  real(kind=dp) :: bias,bias0
!TVS  real(kind=dp) :: r,tmp
!TVS  integer:: islot, iprof, i,j,n
!TVS
!TVS  a = 0.0d0
!TVS  b = 0.0d0
!TVS  dep = 0.0d0
!TVS  dep0 = 0.0d0
!TVS  bias = 0.0d0
!TVS  bias0 = 0.0d0
!TVS  n = 0
!TVS  do islot=1,nslots
!TVS    do iprof=1,maxtvsprof
!TVS      if(tvsqc(ichan,iprof,iinst,islot)/=1) CYCLE
!TVS      !
!TVS      ! R
!TVS      !
!TVS      r = tvserr(ichan,iprof,iinst,islot)**2
!TVS      !
!TVS      ! p R^-1 p^T
!TVS      !
!TVS      do j=1,maxvbc
!TVS        do i=1,maxvbc
!TVS          a(i,j) = a(i,j) &
!TVS               & + pred(i,ichan,iprof,iinst,islot) &
!TVS               & * pred(j,ichan,iprof,iinst,islot) / r
!TVS        end do
!TVS      end do
!TVS      !
!TVS      ! B_beta^-1
!TVS      !
!TVS      if(msw == 1) then ! Y.Sato
!TVS        if(ntvsl < nmin) then
!TVS          tmp = REAL(nmin,kind=dp) / r
!TVS
!TVS        else
!TVS          tmp = (REAL(ntvsl,kind=dp) &
!TVS            & / (LOG10(REAL(ntvsl,kind=dp)/REAL(nmin,kind=dp))+1.0d0)) / r
!TVS        end if
!TVS      else if(msw == 2) then ! D.Dee
!TVS        tmp = REAL(ntvsl,kind=dp) / r
!TVS      else ! Constant
!TVS        tmp = 100.0d0
!TVS      end if
!TVS      do i=1,maxvbc
!TVS        a(i,i) = a(i,i) + tmp
!TVS      end do
!TVS      !
!TVS      ! p R^-1 d
!TVS      !
!TVS      b(:) = b(:) + pred(:,ichan,iprof,iinst,islot) / r &
!TVS                & *(tvsdat(ichan,iprof,iinst,islot)-hx(ichan,iprof,iinst,islot))
!TVS      bias = bias+tvsdat(ichan,iprof,iinst,islot)-hx(ichan,iprof,iinst,islot)
!TVS      dep = dep+(tvsdat(ichan,iprof,iinst,islot)-hx(ichan,iprof,iinst,islot))**2
!TVS      bias0= bias0+tvsdep(ichan,iprof,iinst,islot)
!TVS      dep0= dep0+tvsdep(ichan,iprof,iinst,islot)**2
!TVS      n = n+1
!TVS    end do
!TVS  end do
!TVS
!TVS  dep = SQRT(dep / REAL(n,kind=dp))
!TVS  dep0 = SQRT(dep0 / REAL(n,kind=dp))
!TVS  bias = bias / REAL(n,kind=dp)
!TVS  bias0 = bias0 / REAL(n,kind=dp)
!TVS  print '(2A,I3,4F12.4)',' >> D monit: ',tvsname(iinst),tvsch(ichan,iinst),bias0,bias,dep0,dep
!TVS
!TVS  return
!TVSend subroutine vbc_local

end module lmlef_tools
