program decode_dcdf
!
! decode dcdf files
! history:
! 22-12-09 SN create
! 23-01-18 add surface observation decode
!
  use kind_module
  use obs_module, only : obstype, get_nobs_dcdf, read_upper, read_synop, &
     & obsin_allocate, obs_preproc, monit_obsin, &
     & write_obs
  use func_module, only : ndate

  implicit none
  character(len=5), dimension(4), parameter :: datatype=&
   & (/'upper','surf ','tovs ','misc '/)
  integer, dimension(4), parameter :: iuse=(/1,1,0,0/)
  character(len=100) :: cfile,ofile
  character(len=15) :: odate='yymmddhhnn-hhnn'
  character(len=6) :: cyymmdd
  character(len=4) :: chhnn
  integer :: inn, ihh, idd, imm, iyy
  integer :: ndataall
  type(obstype) :: obs
  integer :: i, iof
  integer, dimension(5) :: btime
  integer, dimension(5) :: atime = (/2022,1,1,0,0/)
  integer :: lmin=0, rmin=1
  logical :: lpreproc=.true.
  integer :: iwnd=1,iq=1
  namelist /param_decode/ atime, lmin, rmin, lpreproc, iwnd, iq

  read(5,nml=param_decode)
  write(6,nml=param_decode)
  call ndate(atime,lmin,btime)
  iyy = btime(1) - (btime(1)/100)*100
  imm = btime(2)
  idd = btime(3)
  write(cyymmdd,'(3i2.2)') iyy,imm,idd
  write(chhnn,'(2i2.2)') btime(4:5)
  print *, cyymmdd
  print *, chhnn
  write(odate(1:6),'(a6)') cyymmdd
  write(odate(7:10),'(a4)') chhnn
  call ndate(atime,rmin,btime)
  write(chhnn,'(2i2.2)') btime(4:5)
  print *, chhnn
  write(odate(12:15),'(a4)') chhnn
  print *, odate

  iyy = atime(1) - (atime(1)/100)*100
  imm = atime(2)
  idd = atime(3)
  write(cyymmdd,'(3i2.2)') iyy,imm,idd
  do iof=1,4
    if(iuse(iof)==1) then
      cfile='dcdf.'//trim(datatype(iof))//'.'//cyymmdd
      print *, cfile
      call get_nobs_dcdf(datatype(iof),cfile,atime,lmin,rmin,ndataall,obs%nobs)
      print *, 'total(data) ',ndataall
      print *, 'total(obs)  ', obs%nobs
      if(datatype(iof)=='upper') then
        call read_upper(cfile,atime,lmin,rmin,ndataall,obs)
      else if(datatype(iof)=='surf ') then
        call read_synop(cfile,atime,lmin,rmin,ndataall,obs)
      end if
      print *, 'total(use)  ',obs%nobs
      call monit_obsin(obs%nobs,obs%elem,obs%dat)

      ofile=trim(datatype(iof))//'.'//odate
      if(lpreproc.and.(datatype(iof)=='upper')) then
        call obs_preproc(obs,iwnd,iq)
        call monit_obsin(obs%nobs,obs%elem,obs%dat)
        if(iq.eq.2) then
          ofile=trim(datatype(iof))//'_preprh.'//odate
        else
          ofile=trim(datatype(iof))//'_prep.'//odate
        end if
      end if
!      do i=1,min(obs%nobs,100)
!        print '(i6,x,i5,3f9.1,es15.4,f7.1)', i,obs%elem(i),&
!        &  obs%lon(i),obs%lat(i),obs%lev(i),obs%dat(i),&
!        &  obs%dmin(i)
!      end do
      print *, ofile
      call write_obs(ofile,obs)
    end if
  end do
end program decode_dcdf
