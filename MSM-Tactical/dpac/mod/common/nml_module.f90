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
  character(filelenmax) :: obsin_name(nobsfilemax) = 'obs.dat'
!  logical, save :: obsda_run(nobsfilemax) = .true.
  logical, save :: obs_out = .true.
  character(filelenmax) :: obsout_basename = 'obsda.@@@@'
  character(filelenmax) :: fguess_basename = 'gues.@@@@'

  integer, save :: slot_start = 1
  integer, save :: slot_end = 1
  integer, save :: slot_base = 1
  real(kind=dp), save :: slot_tint = 60.0_dp !minutes

  !! lmlef
  character(filelenmax) :: obsda_in_basename = 'obsda.@@@@'
  character(filelenmax) :: gues_in_basename = 'gues.@@@@'
  character(filelenmax) :: anal_out_basename = 'anal.@@@@'
  !!! 
  real(kind=dp),save :: sigma_obs=500.0d3
!  real(kind=dp),save :: sigma_obsv=0.4d0
  real(kind=dp),save :: sigma_obsv=0.1d0
  real(kind=dp),save :: sigma_obst=3.0d0
  real(kind=dp),save :: gross_error=10.0d0
  logical,save      :: mean = .FALSE. ! If True, ensemble mean is analyzed
  logical,save      :: tl = .FALSE. ! If True, tangent linear operator used
  logical,save      :: scl_mem = .FALSE. ! If True, forecast ensemble perturbations are scaled by member size
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

  subroutine read_nml_lmlef
    implicit none
    integer :: ierr

    namelist /param_lmlef/ &
      obsda_in_basename, &
      gues_in_basename, &
      anal_out_basename, &
      sigma_obs, &
      sigma_obsv, &
      sigma_obst, &
      gross_error, &
      mean, &
      tl, &
      scl_mem

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
