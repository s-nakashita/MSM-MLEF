program decode_dcdf
!
! decode dcdf files
! history:
! 22-12-09 SN create
!
  use kind_module
  use obs_module, only : obstype, get_nobs_upper, read_upper, &
     & obsin_allocate, obs_preproc, monit_obsin, &
     & write_obs

  implicit none
  character(len=5), dimension(4), parameter :: datatype=&
   & (/'upper','surf ','tovs ','misc '/)
  integer, dimension(4), parameter :: iuse=(/1,0,0,0/)
  character(len=100) :: cfile,ofile
  character(len=15) :: odate='yymmddhhnn-hhnn'
  character(len=4) :: osuffix='.dat'
  character(len=6) :: cyymmdd
  character(len=4) :: chhnn
  integer :: inn, ihh, idd, imm, iyy
  integer :: ndataall
  type(obstype) :: obs
  integer :: i, iof
  integer, dimension(5) :: atime = (/2022,1,1,0,0/)
  integer :: lmin=0, rmin=1
  logical :: lpreproc=.true.
  namelist /param_decode/ atime, lmin, rmin, lpreproc

  read(5,nml=param_decode)
  write(6,nml=param_decode)
  iyy = atime(1) - (atime(1)/100)*100
  imm = atime(2)
  idd = atime(3)
  write(cyymmdd,'(3i2.2)') iyy,imm,idd
  write(odate(1:6),'(a6)') cyymmdd
  inn = atime(4)*60 + atime(5) + lmin
  ihh = inn / 60
  inn = inn - ihh*60
  write(chhnn,'(2i2.2)') ihh,inn
  print *, chhnn
  write(odate(7:10),'(a4)') chhnn
  inn = atime(4)*60 + atime(5) + rmin
  ihh = inn / 60
  inn = inn - ihh*60
  write(chhnn,'(2i2.2)') ihh,inn
  print *, chhnn
  write(odate(12:15),'(a4)') chhnn
  print *, odate

  do iof=1,4
    if(iuse(iof)==1) then
      cfile='dcdf.'//trim(datatype(iof))//'.'//cyymmdd
      print *, cfile
      call get_nobs_upper(cfile,atime,lmin,rmin,ndataall,obs%nobs)
      print *, 'total(data) ',ndataall
      print *, 'total(obs)  ', obs%nobs
      call read_upper(cfile,atime,lmin,rmin,ndataall,obs)
      print *, 'total(use)  ',obs%nobs
      call monit_obsin(obs%nobs,obs%elem,obs%dat)

      if(lpreproc) then
        call obs_preproc(obs)
        call monit_obsin(obs%nobs,obs%elem,obs%dat)
      end if
!      do i=1,min(obs%nobs,100)
!        print '(i6,x,i5,3f9.1,es15.4,2f7.1)', i,obs%elem(i),&
!        &  obs%lon(i),obs%lat(i),obs%lev(i),obs%dat(i),&
!        &  obs%err(i),obs%dmin(i)
!      end do
      if(lpreproc) then
      ofile=trim(datatype(iof))//'_prep.'//odate//osuffix
      else
      ofile=trim(datatype(iof))//'.'//odate//osuffix
      end if
      print *, ofile
      call write_obs(ofile,obs)
    end if
  end do
end program decode_dcdf
