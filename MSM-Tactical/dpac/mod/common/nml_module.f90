module nml_module
!
! namelist settings
! history:
! 22-12-08 SN create
!
  use kind_module
  implicit none
  public

  integer, parameter :: filelenmax=256

  !! ensemble
  integer, parameter :: memberflen=4   ! length of member num in filename
  integer, save :: member=10

  !! obsope
  integer, parameter :: nobsfilemax=10
  integer, save :: obsin_num=1
  character(filelenmax) :: obsin_name(nobsfilemax) = 'obs'
!  logical, save :: obsda_run(nobsfilemax) = .true.
  logical, save :: obs_out = .false.
  character(filelenmax) :: obsout_basename = 'obsda.@@@@'
  character(filelenmax) :: fguess_basename = 'gues.@@@@'

  logical, save :: single_obs=.false.
  real(kind=dp),save :: lonw=0.0d0
  real(kind=dp),save :: lone=0.0d0
  real(kind=dp),save :: lats=0.0d0
  real(kind=dp),save :: latn=0.0d0
  logical, save :: luseobs(10)=(/&
  !!     U       V       T       Q      RH
  & .true., .true., .true., .true., .true., &
  !!    Ps      T2      Td      Wd      Ws
  & .true., .true., .true., .true., .true./)
  integer, save :: nobsmax=0 !only effective for nobsmax > 0

  logical, save :: fixed_level=.false. ! only used mandatory level data

  integer, save :: slot_start = 1
  integer, save :: slot_end = 1
  integer, save :: slot_base = 1
  real(kind=dp), save :: slot_tint = 60.0_dp !minutes

  !! corsm
  integer, save :: njsep = 1 ! number of separation in latitude
  integer, save :: nisep = 1 ! number of separation in longitude
  ! note: njsep * nisep = nimages
  integer, save :: jghost = 0  ! number of ghost point in latitude
  integer, save :: ighost = 0  ! number of ghost point in longitude
  integer, save :: print_img=1 ! standart output image

  !! lmlef
  logical, save :: obsda_in = .false.
  character(filelenmax) :: obsda_in_basename = 'obsda.@@@@'
  character(filelenmax) :: obsg_out_basename = 'obsg.@@@@'
  character(filelenmax) :: obsa_out_basename = 'obsa.@@@@'
  character(filelenmax) :: gues_in_basename = 'gues.@@@@'
  character(filelenmax) :: anal_out_basename = 'anal.@@@@'
  logical,save      :: mean = .FALSE. ! If True, ensemble mean is analyzed
  logical,save      :: tl = .FALSE. ! If True, tangent linear operator used
  logical,save      :: scl_mem = .FALSE. ! If True, forecast ensemble perturbations are scaled by member size
  !!! lmlef_obs
  logical,save :: debug_obs=.false.
  real(kind=dp),save :: sigma_obs=500.0d3
!  real(kind=dp),save :: sigma_obsv=0.4d0
  real(kind=dp),save :: sigma_obsv=0.1d0
  real(kind=dp),save :: sigma_obst=3.0d0
  real(kind=dp),save :: gross_error=10.0d0
  !!! lmlef_tools
  real(kind=dp),save    :: cov_infl_mul = 0.d0 !multiplicative inflation
! > 0: globally constant covariance inflation
! < 0: 3D inflation values input from a GPV file
  character(filelenmax) :: infl_mul_in_basename = 'infl'
  character(filelenmax) :: infl_mul_out_basename = 'infl'
  real(kind=dp),save    :: sp_infl_add = 0.d0 !additive inflation
  character(filelenmax) :: infl_add_in_basename = 'addi.@@@@'
  real(kind=dp),save    :: sp_infl_rtpp = 0.d0 !relaxation to prior perturbations
  real(kind=dp),save    :: sp_infl_rtps = 0.d0 !relaxation to prior spread
  logical,save          :: relax_spread_out = .FALSE.
  character(filelenmax) :: relax_spread_out_basename = 'rtps'
!TVS  logical,parameter :: msw_vbc = .FALSE.
  integer,save          :: maxiter = 5
  logical,save          :: nonlinear=.TRUE. ! If True, observation operator is explicitly evaluated for each iteration
  logical,save          :: zupd = .TRUE. ! If True, Zmat is updated for each iteration
  logical,save          :: save_info=.FALSE. ! If True, save cost functions and ensemble weights
  character(filelenmax) :: info_out_basename = 'dainfo'
  character(filelenmax) :: ewgt_basename = 'ewgt.@@@@'
!
  real(kind=dp),save    :: q_update_top = 0.0d0 ! watar vapor and hydrometeors are updated only below this pressure level (Pa)
  logical,save          :: q_adjust = .false. ! super saturation (dry) adjustment
! monitor
  logical, save :: oma_monit=.true.
  logical, save :: obsgues_output=.false.
  logical, save :: obsanal_output=.false.
! debug
  logical, save :: debug_time=.false.
!
contains
  subroutine read_nml_ens
    implicit none
    integer :: ierr

    namelist /param_ens/ member

    rewind(5)
    read (5,nml=param_ens,iostat=ierr)
    if (ierr<0) then
      write(6,*) 'error: /param_ens/ is not found in namelist'
      stop
    elseif (ierr>0) then
      write(6,'(a,i5,a)') 'ierr',ierr,':invalid names in namelist param_ens'
      stop
    end if
    write(6,nml=param_ens)
    return
  end subroutine read_nml_ens

  subroutine read_nml_obsope
    implicit none
    integer :: ierr

    namelist /param_obsope/ &
      obsin_num, &
      obsin_name, &
      obs_out, &
      obsout_basename, &
      fguess_basename, &
      single_obs, &
      lonw, lone, lats, latn, &
      luseobs, &
      nobsmax, &
      fixed_level, &
      slot_start, &
      slot_end, &
      slot_base, &
      slot_tint
      
    rewind(5)
    read (5,nml=param_obsope,iostat=ierr)
    if (ierr<0) then
      write(6,*) 'error: /param_obsope/ is not found in namelist'
      stop
    elseif (ierr>0) then
      write(6,'(a,i5,a)') 'ierr',ierr,':invalid names in namelist param_obsope'
      stop
    end if
    write(6,nml=param_obsope)
    return
  end subroutine read_nml_obsope

  subroutine read_nml_corsm
    implicit none
    integer :: ierr

    namelist /param_corsm/ &
      njsep, &
      nisep, &
      jghost, &
      ighost, &
      print_img

    rewind(5)
    read (5,nml=param_corsm,iostat=ierr)
    if (ierr<0) then
      write(6,*) 'error: /param_corsm/ is not found in namelist'
      stop
    else if(ierr>0) then
      write(6,'(a,i5,a)') 'ierr',ierr,':invalid names in namelist param_corsm'
      stop
    end if
    write(6,nml=param_corsm)
    return
  end subroutine read_nml_corsm

  subroutine read_nml_lmlef
    implicit none
    integer :: ierr

    namelist /param_lmlef/ &
      obsda_in, &
      obsda_in_basename, &
      obsg_out_basename, &
      obsa_out_basename, &
      gues_in_basename, &
      anal_out_basename, &
      mean, &
      tl, &
      scl_mem, &
      debug_obs, &
      sigma_obs, &
      sigma_obsv, &
      sigma_obst, &
      gross_error, &
      cov_infl_mul, &
      infl_mul_in_basename, &
      infl_mul_out_basename, &
      sp_infl_add, &
      infl_add_in_basename, &
      sp_infl_rtpp, &
      sp_infl_rtps, &
      relax_spread_out, &
      relax_spread_out_basename, &
      maxiter, &
      nonlinear, &
      zupd, &
      save_info, &
      info_out_basename, &
      ewgt_basename, &
      q_update_top, &
      q_adjust, &
      oma_monit, &
      obsgues_output, &
      obsanal_output, &
      debug_time

    rewind(5)
    read (5,nml=param_lmlef,iostat=ierr)
    if (ierr<0) then
      write(6,*) 'error: /param_lmlef/ is not found in namelist'
      stop
    elseif (ierr>0) then
      write(6,'(a,i5,a)') 'ierr',ierr,':invalid names in namelist param_lmlef'
      stop
    end if
    write(6,nml=param_lmlef)
    return
  end subroutine read_nml_lmlef

  subroutine file_member_replace(mem,fin,fout)
    implicit none
    integer, intent(in) :: mem
    character(len=*), intent(in) :: fin
    character(len=filelenmax), intent(out) :: fout
    character(len=memberflen) :: memstr='@@@@'
    integer :: s, is

    s=0
    fout=fin
    do is=1,len(fin)-memberflen+1
      if(fin(is:is+memberflen-1).eq.memstr) then
        if(mem.le.member) then
          write(fout(is:is+memberflen-1), '(i4.4)') mem
        else if(mem.eq.member+1) then
          write(fout(is:is+memberflen-1), '(a4)') 'mean'
        else if(mem.eq.member+2) then
          write(fout(is:is+memberflen-1), '(a4)') 'sprd'
        end if
        s=is
        exit
      end if
    end do

    if(s.eq.0) then
      write(6,'(3a)') "Warning: Keyword '@@@@' not found in '",fin,"'"
      stop 1
    end if
    return
  end subroutine file_member_replace
end module nml_module
