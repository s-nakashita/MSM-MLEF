module lmlef_obs
!=======================================================================
!
! [PURPOSE:] Observational procedures
!
! [HISTORY:]
!   06/01/2022 created
!   07/07/2022 TL option added
!   12/13/2022 modified for MSM
!
!=======================================================================
!$USE OMP_LIB
  use kind_module
  use co_module
  use phconst_module
  use rsmcom_module
  use nml_module
  use obs_module
  use obsope_module, only: monit_dep, monit_print
!  use common_obs_speedy_tl, only: read_obs3
  use corsm_module

  implicit none
  public

  integer,save :: nobs
  integer :: nslots=1 ! number of time slots for 4D-LETKF
  integer :: nbslot=1 ! basetime slot
  real(kind=dp),save :: dist_zero
  real(kind=dp),save :: dist_zerov
  real(kind=dp),allocatable,save :: dlon_zero(:)
  real(kind=dp),save :: dlat_zero

  type(obstype ),allocatable,save :: obs(:) !input
  type(obstype2),save :: obsda
  type(obstype2),save :: obsdasort

  integer,save :: nobs_ext

  integer,allocatable,save :: nobsgrd(:,:)
!  integer,allocatable,save :: obsij1(:)
!  integer,allocatable,save :: obsnode(:)

contains
!-----------------------------------------------------------------------
! Initialize
!-----------------------------------------------------------------------
subroutine set_lmlef_obs
  implicit none
  real(kind=dp) :: dlon1,dlon2,dlon,dlat
  type(obstype2) :: obsda_ext ! externally processed obs
  type(obstype2) :: tmpobs ! intermediate sorting values
  real(kind=dp),allocatable :: wk2d(:,:)
  integer,allocatable :: iwk2d(:,:)
  real(kind=dp),allocatable :: tmphdxf(:,:)[:]
  real(kind=dp),allocatable :: tmpdata(:,:)[:]
  integer,allocatable :: tmpqc0(:,:)[:]
  integer,allocatable :: nobslots(:)[:]
  integer,allocatable :: tmpimg(:)[:]
  integer,allocatable :: tmpelm(:)[:]
  integer :: is,ie,js,je
  integer :: n,i,j,ierr,islot,nn,l,im
  integer, allocatable :: nj(:)
  integer, allocatable :: njs(:)
  integer :: tmpij1,tmpnode
  character(filelenmax) :: obsdafile
!! monitor
  integer :: monit_nobs(nobstype)
  real(kind=dp) :: monit_bias(nobstype)
  real(kind=dp) :: monit_rmse(nobstype)
  integer :: monit_nqc(nobstype,nqctype)

  write(6,'(A)') 'Hello from set_lmlef_obs'

  nslots = slot_end - slot_start + 1
  nbslot = slot_base
  dist_zero = sigma_obs * sqrt(10.0d0/3.0d0) * 2.0d0
  dist_zerov = sigma_obsv * sqrt(10.0d0/3.0d0) * 2.0d0
  dlat_zero = dist_zero / re * rad2deg
  allocate(dlon_zero(nj1))
  do j=1,nj1
    dlon_zero(j) = dlat_zero / cos(myrlat(j)*deg2rad)
  end do

  write(6,'(I10,A)') obsda%nobs-nobs_ext,' INTERNAL PROCESSED OBSERVATIONS INPUT'
  write(6,'(I10,A)') nobs_ext,' EXTERNAL PROCESSED OBSERVATIONS INPUT'
  write(6,'(I10,A)') obsda%nobs,' TOTAL OBSERVATIONS INPUT'

  if(obsda%nobs == 0) then
    write(6,'(A)') 'No observation assimilated'
    return
  end if
!
! INITIALIZE GLOBAL VARIABLES
!
!  obsda%nobs = nobs
!  call obsout_allocate( obsda,member )

!
! reading externally processed observation data
!
  if(obsda_in.and.nobs_ext>0) then
    obsda_ext%nobs = nobs_ext
    call obsout_allocate( obsda_ext,member )
    if(mean) then
      allocate( tmphdxf(obsda_ext%nobs,member)[*] )
      allocate( tmpqc0(obsda_ext%nobs,member)[*] )
    else
      allocate( tmphdxf(obsda_ext%nobs,0:member)[*] )
      allocate( tmpqc0(obsda_ext%nobs,0:member)[*] )
    end if
    if(nimages>member) then
      allocate( tmpelm(obsda_ext%nobs)[*] ) !elem
      allocate( tmpdata(obsda_ext%nobs,6)[*] ) !lon,lat,lev,dat,dmin,err
    end if
    tmphdxf=0.0d0
    tmpqc0 = 0
    nn=0
    l=0
    do
      if(mean) then
      im = myimage   + nimages * l
      else
      im = myimage-1 + nimages * l
      end if
      if(im > member) EXIT
      call file_member_replace(im,obsda_in_basename,obsdafile)
      write(6,'(a,i4.4,2a)') 'MYIMAGE ',myimage,' is reading a file ',obsdafile
      call read_obsout(obsdafile,obsda_ext,im)
      if(im.eq.0) then
        tmphdxf(:,im) = obsda_ext%hxf(:)
      else
        tmphdxf(:,im) = obsda_ext%hxe(im,:)
      end if
      tmpqc0(:,im) = obsda_ext%qc(:)
      if(nn.eq.0) then
        obsda%elem(obsda%nobs-nobs_ext+1:obsda%nobs) = obsda_ext%elem(:)
        obsda%lon(obsda%nobs-nobs_ext+1:obsda%nobs) = obsda_ext%lon(:)
        obsda%lat(obsda%nobs-nobs_ext+1:obsda%nobs) = obsda_ext%lat(:)
        obsda%lev(obsda%nobs-nobs_ext+1:obsda%nobs) = obsda_ext%lev(:)
        obsda%dat(obsda%nobs-nobs_ext+1:obsda%nobs) = obsda_ext%dat(:)
        obsda%dmin(obsda%nobs-nobs_ext+1:obsda%nobs) = obsda_ext%dmin(:)
        obsda%err(obsda%nobs-nobs_ext+1:obsda%nobs) = obsda_ext%err(:)
        if(nimages>member) then
          tmpelm(:)=obsda_ext%elem(:)
          tmpdata(:,1)=obsda_ext%lon(:)
          tmpdata(:,2)=obsda_ext%lat(:)
          tmpdata(:,3)=obsda_ext%lev(:)
          tmpdata(:,4)=obsda_ext%dat(:)
          tmpdata(:,5)=obsda_ext%dmin(:)
          tmpdata(:,6)=obsda_ext%err(:)
        end if
      end if
      l = l+1
      nn=nn+1
    end do
    sync all
    if(myimage>member) then !copy observation information
      obsda%elem(obsda%nobs-nobs_ext+1:obsda%nobs) = tmpelm(:)[mod(myimage,member)]
      obsda%lon(obsda%nobs-nobs_ext+1:obsda%nobs) = tmpdata(:,1)[mod(myimage,member)]
      obsda%lat(obsda%nobs-nobs_ext+1:obsda%nobs) = tmpdata(:,2)[mod(myimage,member)]
      obsda%lev(obsda%nobs-nobs_ext+1:obsda%nobs) = tmpdata(:,3)[mod(myimage,member)]
      obsda%dat(obsda%nobs-nobs_ext+1:obsda%nobs) = tmpdata(:,4)[mod(myimage,member)]
      obsda%dmin(obsda%nobs-nobs_ext+1:obsda%nobs) = tmpdata(:,5)[mod(myimage,member)]
      obsda%err(obsda%nobs-nobs_ext+1:obsda%nobs) = tmpdata(:,6)[mod(myimage,member)]
    end if
    if(nimages>member) deallocate( tmpelm,tmpdata )
  !
  ! broadcast
  !
    if(debug_obs) then
      do im=0,member
        if(im.eq.0) then
        write(6,*) im,'hxf',maxval(obsda_ext%hxf(:)),minval(obsda_ext%hxf(:))
        else
        write(6,*) im,'hxf',maxval(obsda_ext%hxe(im,:)),minval(obsda_ext%hxe(im,:))
        end if
        write(6,*) im,'qc  ',maxval(tmpqc0(:,im)),minval(tmpqc0(:,im))
      end do
    end if
    if(mean) then
      allocate(wk2d(obsda_ext%nobs,member))
      allocate(iwk2d(obsda_ext%nobs,member))
    else
      allocate(wk2d(obsda_ext%nobs,member+1))
      allocate(iwk2d(obsda_ext%nobs,member+1))
    end if
    wk2d = 0.0d0
    iwk2d = 0
    if(myimage==print_img) then
      do i=1, nimages 
        wk2d = wk2d + tmphdxf(:,:)[i]
        iwk2d = iwk2d + tmpqc0(:,:)[i]
      end do 
      do i=1, nimages 
        tmphdxf(:,:)[i] = wk2d
        tmpqc0(:,:)[i] = iwk2d
      end do 
    end if
    sync all
    if(.not.mean) obsda%hxf(obsda%nobs-nobs_ext+1:obsda%nobs) = tmphdxf(:,0)
    do im=1,member
      do n=1,obsda_ext%nobs
        obsda%hxe(im,obsda%nobs-nobs_ext+n)=tmphdxf(n,im)
      end do
    end do
    do n=1,obsda_ext%nobs
      obsda%qc(obsda%nobs-nobs_ext+n) = maxval(tmpqc0(n,:))
    end do
    if(debug_obs) then
      do im=0,member
        if(im.eq.0) then
        write(6,*) im,'hxf',maxval(obsda%hxf(:)),minval(obsda%hxf(:))
        else
        write(6,*) im,'hxf',maxval(obsda%hxe(im,:)),minval(obsda%hxe(im,:))
        end if
        write(6,*) im,'qc  ',maxval(tmpqc0(:,im)),minval(tmpqc0(:,im))
      end do
    end if
    deallocate(wk2d)
    deallocate(iwk2d)
    deallocate(tmphdxf)
    deallocate(tmpqc0)
  end if !obsda_in.and.nobs_ext>0
!
! compute hxf mean, perturbation and departure
! & gross error check
! obsda%hxe : Hdx
! obsda%hxf : y-Hx (Hx:mean or control)
!
  nobs=0
!$OMP PARALLEL DO SCHEDULE(DYNAMIC) PRIVATE(n,i)
  do n=1,obsda%nobs
    if(obsda%qc(n) /= iqc_good) cycle
    nobs=nobs+1
    if(mean) then
      obsda%hxf(n)=obsda%hxe(1,n)
      do i=2,member
        obsda%hxf(n) = obsda%hxf(n) + obsda%hxe(i,n)
      end do
      obsda%hxf(n) = obsda%hxf(n) / real(member,kind=dp)
    end if
    do i=1,member
      obsda%hxe(i,n) = obsda%hxe(i,n) - obsda%hxf(n) ! Hdx
    end do
    obsda%hxf(n) = obsda%dat(n) - obsda%hxf(n) ! y-Hx
    if(obsda%elem(n).eq.id_wd_obs) then !wind direction
      if(abs(obsda%hxf(n)).gt.180.0d0) then
        obsda%hxf(n)=obsda%hxf(n)-sign(360.0d0,obsda%hxf(n))
      end if
      do i=1,member
        if(abs(obsda%hxe(i,n)).gt.180.0d0) then
          obsda%hxe(i,n)=obsda%hxe(i,n)-sign(360.0d0,obsda%hxe(i,n))
        end if
      end do
    end if
    if(abs(obsda%hxf(n)) > gross_error*obsda%err(n)) then !gross error
      if(debug_obs) then
        write(6,'(2i6,a3,es10.2,a4,es10.2,a3,f8.2)') &
        n, obsda%elem(n), 'y', obsda%dat(n), 'dep', &
        abs(obsda%hxf(n)), '>', gross_error*obsda%err(n)
      end if
      obsda%qc(n) = iqc_gross_err
    end if
  end do
!$OMP END PARALLEL DO

  write(6,'(I10,A)') nobs,' OBSERVATIONS TO BE ASSIMILATED'
!
! output departure statistics before sort
!
  call monit_dep(obsda%nobs,obsda%elem,obsda%hxf,obsda%qc,&
   &  monit_nobs,monit_bias,monit_rmse,monit_nqc)
  call monit_print(monit_nobs,monit_bias,monit_rmse,monit_nqc)
!!
!! temporal observation localization
!!
!  nn = 0
!  do islot=1,nslots
!    obsda%err(nn+1:nn+nobslots(islot)) = obsda%err(nn+1:nn+nobslots(islot)) &
!      & * exp(0.25d0 * (real(islot-nbslot,kind=dp) / sigma_obst)**2)
!    nn = nn + nobslots(islot)
!  end do
!
! search which image contains observation
!
  allocate( tmpimg(obsda%nobs)[*] )
  tmpimg = 0
  nn=0
  is=1
  ie=ni1+ighost
  if(nidom(myimage)==nisep) ie=ni1
  js=1
  je=nj1+jghost
  if(njdom(myimage)==njsep) je=nj1
  do n=1,obsda%nobs
    if(obsda%qc(n) /= iqc_good) cycle
    if(obsda%lat(n) < myrlat(js) .or. myrlat(je) <= obsda%lat(n)) & ! latitude's order is S->N
      cycle
    if(obsda%lon(n) < myrlon(is) .or. myrlon(ie) <= obsda%lon(n)) & ! longitude's order is W->E
      cycle
    tmpimg(n) = myimage
    nn=nn+1
  end do
  sync all
  if(myimage.eq.print_img) then
    do l=1,nimages
      if(l.eq.myimage) cycle
      tmpimg(:)[myimage] = tmpimg(:)[myimage]+tmpimg(:)[l]
    end do
    do l=1,nimages
      tmpimg(:)[l] = tmpimg(:)[myimage]
    end do
  end if
  sync all
  obsda%img(:) = tmpimg
  if(debug_obs) write(6,*) 'obs%img ',obsda%img(:)
  write(6,'(I10,A,I3.3)') nn,' OBSERVATIONS TO BE PROCESSED IN MYIMAGE ',myimage
  deallocate( tmpimg )
!
! SORT
!
  allocate( nobsgrd(nlon,nlat) )
  allocate( nj(0:nlat) )
  allocate( njs(1:nlat) )
  tmpobs%nobs = obsda%nobs
  call obsout_allocate( tmpobs,member )
!  allocate( obsij1(nobs) )
!  allocate( obsnode(nobs) )
  nobsgrd = 0
  nj = 0
!$OMP PARALLEL PRIVATE(i,j,n,nn)
!$OMP DO SCHEDULE(DYNAMIC)
  do j=1,nlat-1
    do n=1,obsda%nobs
      if(obsda%qc(n) /= iqc_good) cycle
      if(obsda%lat(n) < rlat(j) .or. rlat(j+1) <= obsda%lat(n)) cycle
      nj(j) = nj(j) + 1
    end do
  end do
!$OMP END DO
!$OMP DO SCHEDULE(DYNAMIC)
  do j=1,nlat
    njs(j) = sum(nj(0:j-1))
  end do
!$OMP END DO
  if(debug_obs) then
    write(6,'(3A6)') 'j','nj','njs'
    write(6,'(3I6)') 0,nj(0),0
    do j=1,nlat
      write(6,'(3I6)') j,nj(j),njs(j)
    end do
  end if
  obsdasort%nobs=0
!$OMP DO SCHEDULE(DYNAMIC)
  do j=1,nlat
    nn = 0
    do n=1,obsda%nobs
      if(obsda%qc(n) /= iqc_good) cycle
      if(obsda%lat(n) < rlat(j) .OR. rlat(j+1) <= obsda%lat(n)) cycle
      obsdasort%nobs = obsdasort%nobs + 1
      nn = nn + 1
      tmpobs%elem(njs(j)+nn) = obsda%elem(n)
      tmpobs%lon(njs(j)+nn) = obsda%lon(n)
      tmpobs%lat(njs(j)+nn) = obsda%lat(n)
      tmpobs%lev(njs(j)+nn) = obsda%lev(n)
      tmpobs%dat(njs(j)+nn) = obsda%dat(n)
      tmpobs%err(njs(j)+nn) = obsda%err(n)
      tmpobs%dmin(njs(j)+nn) = obsda%dmin(n)
      tmpobs%hxf(njs(j)+nn) = obsda%hxf(n)
      tmpobs%hxe(:,njs(j)+nn) = obsda%hxe(:,n)
      tmpobs%qc(njs(j)+nn) = obsda%qc(n)
      tmpobs%img(njs(j)+nn) = obsda%img(n)
    end do
  end do
!$OMP END DO
  call obsout_allocate( obsdasort,member )
!$OMP DO SCHEDULE(DYNAMIC)
  do j=1,nlat
    if(nj(j) == 0) then
      nobsgrd(:,j) = njs(j)
      cycle
    end if
    nn = 0
    do i=1,nlon
!      tmpnode = ijnode(i,j)/10000
!      tmpij1 = ijnode(i,j)-tmpnode*10000
!      if(debug_obs) write(6,'(2(A,I3),A,I6,A,I2,A,I5,A)') &
!               & '(i,j)=(',i,',',j,') ijnode=',ijnode(i,j),&
!               & ' (node,ij1)=(',tmpnode,',',tmpij1,')'
      do n=njs(j)+1,njs(j)+nj(j)
        if(tmpobs%lon(n) < rlon(i) .OR. rlon(i+1) <= tmpobs%lon(n)) CYCLE
        nn = nn + 1
        obsdasort%elem(njs(j)+nn) = tmpobs%elem(n)
        obsdasort%lon(njs(j)+nn) = tmpobs%lon(n)
        obsdasort%lat(njs(j)+nn) = tmpobs%lat(n)
        obsdasort%lev(njs(j)+nn) = tmpobs%lev(n)
        obsdasort%dat(njs(j)+nn) = tmpobs%dat(n)
        obsdasort%err(njs(j)+nn) = tmpobs%err(n)
        obsdasort%dmin(njs(j)+nn) = tmpobs%dmin(n)
        obsdasort%hxf(njs(j)+nn) = tmpobs%hxf(n)
        obsdasort%hxe(:,njs(j)+nn) = tmpobs%hxe(:,n)
        obsdasort%qc(njs(j)+nn) = tmpobs%qc(n)
        obsdasort%img(njs(j)+nn) = tmpobs%img(n)
!        obsnode(njs(j)+nn) = tmpnode
!        obsij1(njs(j)+nn) = tmpij1
      end do
      nobsgrd(i,j) = njs(j) + nn
    end do
    if(nn /= nj(j)) then
!$OMP CRITICAL
      write(6,'(A,2I6)') 'OBS DATA SORT ERROR: ',nn,nj(j)
      write(6,'(F6.2,A,F6.2)') rlat(j),'< LAT <',rlat(j+1)
      write(6,'(F6.2,A,F6.2)') MINVAL(tmpobs%lat(njs(j)+1:njs(j)+nj(j))),'< OBSLAT <',MAXVAL(tmpobs%lat(njs(j)+1:njs(j)+nj(j)))
!$OMP END CRITICAL
    end if
  end do
!$OMP END DO
!$OMP END PARALLEL
!
! output departure statistics after sort
!
  call monit_dep(obsdasort%nobs,obsdasort%elem,obsdasort%hxf,obsdasort%qc,&
   &  monit_nobs,monit_bias,monit_rmse,monit_nqc)
  call monit_print(monit_nobs,monit_bias,monit_rmse,monit_nqc)
  if(debug_obs) then
    write(6,'(9a10)') 'elem','lon','lat','lev','dat','err','dmin','dep','qc'
    do n=1,obsdasort%nobs
      write(6,'(i10,2f10.2,f10.1,4es10.2,2i10)') &
       & obsdasort%elem(n),&
       & obsdasort%lon(n),obsdasort%lat(n),obsdasort%lev(n),&
       & obsdasort%dat(n),obsdasort%err(n),obsdasort%dmin(n),&
       & obsdasort%hxf(n),obsdasort%qc(n),obsdasort%img(n)
    end do
!    write(6,'(A)') 'nobsgrd'
!    do j=1,nlat
!      write(6,'(2I4,24I6)') j,1,nobsgrd(1:24,j)
!      write(6,'(2I4,24I6)') j,2,nobsgrd(25:48,j)
!      write(6,'(2I4,24I6)') j,3,nobsgrd(49:72,j)
!      write(6,'(2I4,24I6)') j,4,nobsgrd(73:96,j)
!    end do
  end if
  call obsout_deallocate( tmpobs )

  return
end subroutine set_lmlef_obs

end module lmlef_obs
