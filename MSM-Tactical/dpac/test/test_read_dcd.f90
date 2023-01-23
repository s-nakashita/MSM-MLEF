program test_read_dcd
  use kind_module
  use func_module, only : calc_td, calc_rh, calc_q, calc_wd, calc_uv
  use obs_module, only : obstype, get_nobs_dcdf, read_upper,read_synop, &
     & obsin_allocate, obs_preproc, monit_obsin, &
     & get_nobs, read_obs, write_obs, &
     & id_t_obs, id_td_obs, id_ws_obs, id_wd_obs

  implicit none
  !character(len=17) :: cfile='dcdf.upper.yymmdd'
  character(len=6)  :: yymmdd
  character(len=16) :: cfile='dcdf.surf.yymmdd'
  character(len=19) :: ofile='obs.yymmddhhnn-hhnn'
  character(len=20) :: ofile2='obs2.yymmddhhnn-hhnn'
  character(len=4) :: chhnn
  integer :: inn, ihh
  integer, parameter :: smin=0, emin=1
  integer, dimension(5) :: atime
  integer :: ndataall
  type(obstype) :: obs, obs2
  real(kind=dp) :: t,td,p_t,p_td
  real(kind=dp) :: q,rh
  real(kind=dp) :: ws,wd,p_ws,p_wd
  real(kind=dp) :: u,v
  integer :: i

  atime = (/2022,6,1,0,0/)
  write(yymmdd,'(3i2.2)') mod(atime(1),100),atime(2),atime(3)
  write(cfile(11:16),'(a6)') yymmdd
  write(ofile(5:10),'(a6)') yymmdd
  write(ofile2(6:11),'(a6)') yymmdd
  inn = atime(5) + smin
  ihh = inn / 60
  inn = inn - ihh*60
  write(chhnn,'(i2.2,i2.2)') ihh,inn
  print *, chhnn
  write(ofile(11:14),'(a4)') chhnn
  write(ofile2(12:15),'(a4)') chhnn
  inn = atime(5) + emin
  ihh = inn / 60
  inn = inn - ihh*60
  write(chhnn,'(i2.2,i2.2)') ihh,inn
  print *, chhnn
  write(ofile(16:19),'(a4)') chhnn
  write(ofile2(17:20),'(a4)') chhnn
  print *, ofile
  print *, ofile2

  call get_nobs_dcdf('surf ',cfile,atime,smin,emin,ndataall,obs%nobs)
  print *, 'total(data) ',ndataall
  print *, 'total(obs)  ', obs%nobs
!  allocate( obs(nobs) )
  !call read_upper(cfile,atime,smin,emin,ndataall,obs)
  call read_synop(cfile,atime,smin,emin,ndataall,obs)
  print *, 'total(use)  ',obs%nobs
  call write_obs(ofile,obs)
  call get_nobs(ofile,6,obs2%nobs)
  call obsin_allocate(obs2)
  print *, size(obs2%elem)
  call read_obs(ofile,obs2)
  call monit_obsin(obs2%nobs,obs2%elem,obs2%dat)
  do i=1,obs2%nobs
    if(obs2%lev(i)==0.0) then
    print '(i6,x,i5,3f9.1,es15.4,f7.1)', i,obs2%elem(i),&
    &  obs2%lon(i),obs2%lat(i),obs2%lev(i),obs2%dat(i),&
    &  obs2%dmin(i)
    end if
  end do
  stop 99

  t = -999.9
  td = -999.9
  ws = -999.9
  wd = -999.9
  print '(a)','number elem  lon     lat      lev      dat      err      dmin'
  do i=100,min(obs2%nobs,200)
    print '(i6,x,i5,5f9.1)', i,obs2%elem(i),&
    &  obs2%lon(i),obs2%lat(i),obs2%lev(i),obs2%dat(i),&
    &  obs2%dmin(i)
    if(t.lt.0.0.and.obs2%elem(i).eq.id_t_obs) then
      p_t = obs2%lev(i)
      t = obs2%dat(i)
    end if
    if(td.lt.0.0.and.obs2%elem(i).eq.id_td_obs) then
      p_td = obs2%lev(i)
      td = obs2%dat(i)
    end if
    if(ws.lt.0.0.and.obs2%elem(i).eq.id_ws_obs) then
      p_ws = obs2%lev(i)
      ws = obs2%dat(i)
    end if
    if(wd.lt.0.0.and.obs2%elem(i).eq.id_wd_obs) then
      p_wd = obs2%lev(i)
      wd = obs2%dat(i)
    end if
  end do
  print *, 'lev=',p_t,'Pa t=',t,'K'
  print *, 'lev=',p_td,'Pa td=',td,'K'
  print *, 'lev=',p_ws,'Pa ws=',ws,'m/s'
  print *, 'lev=',p_wd,'Pa wd=',wd,'degree'
  call calc_q(td,p_t,q)
  print *, 'calc_q', q,'kg/kg'
!  q = 0.000229_dp
  call calc_td(q,p_t,td)
  print *, 'calc_td ',td,'K'
  call calc_rh(t,q,p_t,rh)
  print *, 'calc_rh ',rh
!  u = 0.866
!  v = 0.5
  call calc_uv(ws,wd,u,v)
  print *, 'calc_uv ',u,v
  ws = sqrt(u**2 + v**2)
  call calc_wd(u,v,wd)
  print *, 'calc_ws ',ws
  print *, 'calc_wd ',wd
  u = -2.0
  v = 3.0
  ws = sqrt(u**2 + v**2)
  call calc_wd(u,v,wd)
  print *, 'calc_ws ',ws
  print *, 'calc_wd ',wd

  call obs_preproc(obs2)
  do i=1,obs2%nobs
    print '(i6,x,i5,3f9.1,es15.4,f7.1)', i,obs2%elem(i),&
    &  obs2%lon(i),obs2%lat(i),obs2%lev(i),obs2%dat(i),&
    &  obs2%dmin(i)
  end do
  call write_obs(ofile2,obs2)
end program test_read_dcd
