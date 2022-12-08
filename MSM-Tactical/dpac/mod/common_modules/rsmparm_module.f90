module rsmparm_module
!
! setting model grids and variables indexes
! history:
! 22-08-04 SN create
!
  use kind_module
  use phconst_module, only : rad2deg
  use read_module, only : read_header, nwext, levmax
  implicit none
  private
! 
! parameters read from namelists
!
  !!! horizontal grid numbers
  integer,save,public :: igrd1, jgrd1
  integer,save,public :: nlon, nlat
  integer,save,public :: lngrd
  !!! horizontal wave numbers
  integer,save,public :: iwav1, jwav1
  integer,save,public :: lnwav
  !!! vertical levels
  integer,save,public :: nlev
  !!! projection parameters
  integer,save,public :: nproj !0=mercator, 1.or.-1=polar
  real(kind=dp),save,public :: rtruth,rorient,rdelx,rdely,rcenlat,rcenlon
  real(kind=dp),save,public :: rlftgrd, rbtmgrd
  !!! model version
  integer,save,public :: nonhyd=1 !0=hydrostatic, 1=nonhydrostatic
  namelist /rsmparm/ igrd1,jgrd1,nlev&
  & ,nproj,rtruth,rorient,rdelx,rdely,rcenlat,rcenlon,rlftgrd,rbtmgrd &
  & ,nonhyd
!
! grid information
!
  real(kind=dp),allocatable,save,public :: rlon(:),rlat(:)
  real(kind=dp),allocatable,save,public :: sig(:),sigh(:)
!
! map factors
!
  real(kind=dp),allocatable,save,public :: mapf(:,:) !1:square of map factor
                                                     !2:d(fm2)/dx
                                                     !3:d(fm2)/dy
!
! variables' indexes
!
  !!! in r_sig.fNN
  integer,save,public :: nflds
  !!! 2D variables
  integer,parameter,public :: nv2d=2 !gz,ps
  integer,parameter,public :: iv2d_gz=1 ! terrain height
  integer,parameter,public :: iv2d_ps=2 ! surface pressure
  !!! 3D variables
  !!! pp,tt,ww are only for nonhydrostatic
  integer,parameter,public :: nv3d=8 !t,u,v,q,oz,cw,(pp,tt) 
  integer,parameter,public :: nv3dp=1 !ww(nlev+1)
  integer,parameter,public :: iv3d_t=1  ! temperature
  integer,parameter,public :: iv3d_u=2  ! x-direction wind
  integer,parameter,public :: iv3d_v=3  ! y-direction wind
  integer,parameter,public :: iv3d_q=4  ! specific humidity
  integer,parameter,public :: iv3d_oz=5 ! ozone mixing ratio
  integer,parameter,public :: iv3d_cw=6 ! cloud water
  integer,parameter,public :: iv3d_pp=7 ! full pressure perturbation
  integer,parameter,public :: iv3d_tt=8 ! temperature perturbation
  integer,parameter,public :: iv3d_ww=1 ! vertical velocity at half levels
  
  public :: set_rsmparm, clean_rsmparm
  contains
!
! set grid information
!
  subroutine set_rsmparm(nsig)
    implicit none
    integer,intent(in) :: nsig ! input sigma file unit
    integer :: nskip
    integer :: icld
    character(len=8) :: label(4)
    integer :: idate(4)
    real(kind=sp) :: fhour,ext(nwext)
    real(kind=dp) :: sisl(2*levmax+1)
    real(kind=sp),allocatable :: sfld(:)
    integer :: iwav, jwav
    integer :: i,j,k
    
    write(6,'(A)') 'start set_rsmparm'
    rewind(5)
    read(5,rsmparm)
    write(6,rsmparm)
    lngrd=igrd1*jgrd1
    write(6,'(A,I8)') 'lngrd=',lngrd
    if(nonhyd.eq.1) then
      icld=1
    else
      icld=0
    end if
    ! calculate wave numbers
    iwav = (igrd1 - 13)/3*2
    jwav = iwav*(jgrd1-1)/2/(igrd1-1)*2
    iwav1 = iwav + 1
    jwav1 = jwav + 1
    lnwav = iwav1*jwav1
    write(6,'(A,2i6)') 'iwav1 jwav1=',iwav1,jwav1
    
    nlon = igrd1
    nlat = jgrd1
    allocate( rlon(nlon), rlat(nlat) )
    allocate( sig(nlev), sigh(nlev+1) )
    allocate( mapf(lngrd,3) )

    call read_header(nsig,icld,label,idate,fhour,sisl(1:levmax+1),sisl(levmax+2:),ext,nflds)
    nonhyd = int(ext(16))
    do i=1,nlev
      sig(i) = real(sisl(levmax+1+i),kind=dp)
      sigh(i) = real(sisl(i),kind=dp)
    end do
    sigh(nlev+1) = real(sisl(nlev+1),kind=dp)
    write(6,*) 'sig=',sig
    write(6,*) 'sigh=',sigh

    if(nonhyd.eq.1) then
      write(6,*) 'model version is nonhydrostatic'
      nskip=2+nv2d+nv3d*nlev+nv3dp*(nlev+1)!+3
    else
      write(6,*) 'model version is hydrostatic'
      nskip=2+nv2d+(nv3d-2)*nlev!+3
    end if
    allocate( sfld(lngrd) )
    rewind(nsig)
    do i=1,nskip
      read(nsig)
    end do
    !fm2
    read(nsig) (sfld(i),i=1,lngrd)
    do i=1,lngrd
      mapf(i,1) = real(sfld(i),kind=dp)
    end do
    !fm2x
    read(nsig) (sfld(i),i=1,lngrd)
    do i=1,lngrd
      mapf(i,2) = real(sfld(i),kind=dp)
    end do
    !fm2y
    read(nsig) (sfld(i),i=1,lngrd)
    do i=1,lngrd
      mapf(i,3) = real(sfld(i),kind=dp)
    end do
    !rlat(S->N)
    read(nsig) (sfld(i),i=1,lngrd)
    k=1
    do j=1,nlat
      rlat(j) = real(sfld(k),kind=dp)*rad2deg
      k=k+nlon
    end do
    write(6,*) 'rlat=',rlat
    !rlon(W->E)
    read(nsig) (sfld(i),i=1,lngrd)
    do i=1,nlon
      rlon(i) = real(sfld(i),kind=dp)*rad2deg
    end do
    write(6,*) 'rlon=',rlon
    deallocate(sfld)
  end subroutine set_rsmparm
!
! clean up
!
  subroutine clean_rsmparm
    implicit none

    deallocate(rlon,rlat,sig,sigh,mapf)
  end subroutine clean_rsmparm
!
end module rsmparm_module
