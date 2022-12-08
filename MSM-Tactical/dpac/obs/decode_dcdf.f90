program decode_dcdf
  use kind_module
  use func_module, only : calc_td, calc_rh, calc_q, calc_wd, calc_uv
  use obs_module, only : obstype, get_nobs_upper, read_upper, &
     & obsin_allocate, obs_preproc, &
     & get_nobs, read_obs, write_obs, &
     & id_t_obs, id_td_obs, id_ws_obs, id_wd_obs

  implicit none
  character(len=17) :: cfile='dcdf.upper.220101'
  character(len=19) :: ofile='obs.220101hhnn-hhnn'
  character(len=4) :: chhnn
  integer :: inn, ihh
  integer, parameter :: smin=0, emin=1
  integer, dimension(5) :: atime
  integer :: ndataall
  type(obstype) :: obs
  integer :: i

  atime = (/2022,1,1,0,0/)
  inn = atime(5) + smin
  ihh = inn / 60
  inn = inn - ihh*60
  write(chhnn,'(i2.2,i2.2)') ihh,inn
  print *, chhnn
  write(ofile(11:14),'(a4)') chhnn
  inn = atime(5) + emin
  ihh = inn / 60
  inn = inn - ihh*60
  write(chhnn,'(i2.2,i2.2)') ihh,inn
  print *, chhnn
  write(ofile(16:19),'(a4)') chhnn
  print *, ofile

  call get_nobs_upper(cfile,atime,smin,emin,ndataall,obs%nobs)
  print *, 'total(data) ',ndataall
  print *, 'total(obs)  ', obs%nobs
  call read_upper(cfile,atime,smin,emin,ndataall,obs)
  print *, 'total(use)  ',obs%nobs

  call obs_preproc(obs)
  do i=1,min(obs%nobs,100)
    print '(i6,x,i5,3f9.1,es15.4,2f7.1)', i,obs%elem(i),&
    &  obs%lon(i),obs%lat(i),obs%lev(i),obs%dat(i),&
    &  obs%err(i),obs%dmin(i)
  end do
  call write_obs(ofile,obs)

end program decode_dcdf
