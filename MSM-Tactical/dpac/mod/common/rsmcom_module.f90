module rsmcom_module
!
! RSM common information and routines
!
! history:
! 22-08-04 SN create
!
  use kind_module
  use phconst_module, only : rad2deg
  use read_module, only: levmax,nwext,read_header,read_sig
  use write_module, only: write_sig
  implicit none
  public
! 
! parameters read from header
!
  !!! horizontal grid numbers
  integer,save :: igrd1, jgrd1
  integer,save :: nlon, nlat
  integer,save :: lngrd
  !!! horizontal wave numbers
  integer,save :: iwav1, jwav1
  integer,save :: lnwav
  !!! vertical levels
  integer,save :: nlev
  !!! projection parameters
  integer,save :: nproj !0=mercator, 1.or.-1=polar
  real(kind=dp),save :: rtruth,rorient,rdelx,rdely,rcenlat,rcenlon
  real(kind=dp),save :: rlftgrd, rbtmgrd
  !!! other header information (saved for write restart file)
  real(kind=sp),save :: fhour
  character(len=8),save :: label(4)
  integer,save :: idate(4)
  !!! model version
  integer,save :: nonhyd !0=hydrostatic, 1=nonhydrostatic
  integer,parameter   :: icld=0 !1=output includes 3D physics, 0=not includes
!
! grid information
!
  real(kind=dp),allocatable,save :: rlon(:),rlat(:)
  real(kind=dp),allocatable,save :: sig(:),sigh(:)
!
! map factors
!
  real(kind=dp),allocatable,save :: mapf(:,:) !1:square of map factor
                                                     !2:d(fm2)/dx
                                                     !3:d(fm2)/dy
!
! variables' indexes
!
  !!! in r_sig.fNN
  integer,save :: nflds
  !!! 2D variables
  integer,parameter :: nv2d=2 !gz,ps
  integer,parameter :: iv2d_gz=1 ! terrain height
  integer,parameter :: iv2d_ps=2 ! surface pressure
  !!! 3D variables
  !!! pp,tt,ww are only for nonhydrostatic
  integer,save      :: nv3d
  integer,parameter :: nv3d_hyd=6    !t,u,v,q,oz,cw 
  integer,parameter :: nv3d_nonhyd=2 !pp,tt
  integer,parameter :: nv3dh=1       !ww(nlev+1)
  integer,parameter :: iv3d_t=1  ! temperature
  integer,parameter :: iv3d_u=2  ! x-direction wind
  integer,parameter :: iv3d_v=3  ! y-direction wind
  integer,parameter :: iv3d_q=4  ! specific humidity
  integer,parameter :: iv3d_oz=5 ! ozone mixing ratio
  integer,parameter :: iv3d_cw=6 ! cloud water
  integer,parameter :: iv3d_pp=7 ! full pressure perturbation
  integer,parameter :: iv3d_tt=8 ! temperature perturbation
  integer,parameter :: iv3d_ww=1 ! vertical velocity at half levels
  
  contains
!
! set grid information
!
  subroutine set_rsmparm(cfile)
    implicit none
    character(len=*), intent(in) :: cfile !input sigma file
  !  integer,intent(in) :: nsig ! input sigma file unit
    integer :: nsig
    integer :: nskip
    real(kind=sp) :: ext(nwext)
    real(kind=dp) :: sisl(2*levmax+1)
    real(kind=sp),allocatable :: sfld(:)
    integer :: iwav, jwav
    integer :: i,j,k
    
    write(6,'(A)') 'start set_rsmparm'
    
    nsig=70
    open(nsig,file=cfile,access='sequential',form='unformatted',action='read')
    call read_header(nsig,icld,label,idate,fhour,sisl(1:levmax+1),sisl(levmax+2:),ext,nflds)
    iwav1  = int(ext(1))
    jwav1  = int(ext(2))
    igrd1  = int(ext(3))
    jgrd1  = int(ext(4))
    nlev   = int(ext(5))
    !nfldx  = int(ext(6))
    nproj  = int(ext(7))
    rtruth =real(ext(8),kind=dp)
    rorient=real(ext(9),kind=dp)
    rcenlat=real(ext(10),kind=dp) 
    rcenlon=real(ext(11),kind=dp)
    rlftgrd=real(ext(12),kind=dp)
    rbtmgrd=real(ext(13),kind=dp)
    rdelx  =real(ext(14),kind=dp)
    rdely  =real(ext(15),kind=dp)
    nonhyd =int(ext(16))
    lngrd=igrd1*jgrd1
    write(6,'(A,2i6)') 'igrd1 jgrd1=',igrd1,jgrd1
    write(6,'(A,I8)') 'lngrd=',lngrd
    lnwav = iwav1*jwav1
    write(6,'(A,2i6)') 'iwav1 jwav1=',iwav1,jwav1
    write(6,'(A,I8)') 'lnwav=',lnwav
    nlon = igrd1
    nlat = jgrd1
    allocate( rlon(nlon), rlat(nlat) )
    allocate( sig(nlev), sigh(nlev+1) )
    allocate( mapf(lngrd,3) )
    do i=1,nlev
      sig(i) = real(sisl(levmax+1+i),kind=dp)
      sigh(i) = real(sisl(i),kind=dp)
    end do
    sigh(nlev+1) = real(sisl(nlev+1),kind=dp)
    write(6,'(a,f7.5,x,f7.5)') 'sig=',minval(sig),maxval(sig)
    write(6,'(a,f7.5,x,f7.5)') 'sigh=',minval(sigh),maxval(sigh)

    if(nonhyd.eq.1) then
      nv3d=nv3d_hyd+nv3d_nonhyd
      write(6,*) 'model version is nonhydrostatic'
      nskip=2+nv2d+nv3d*nlev+nv3dh*(nlev+1)!+3
    else
      nv3d=nv3d_hyd
      write(6,*) 'model version is hydrostatic'
      nskip=2+nv2d+nv3d*nlev!+3
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
    write(6,'(a,f9.2,x,f9.2)') 'rlat=',minval(rlat),maxval(rlat)
    !rlon(W->E)
    read(nsig) (sfld(i),i=1,lngrd)
    do i=1,nlon
      rlon(i) = real(sfld(i),kind=dp)*rad2deg
    end do
    write(6,'(a,f9.2,x,f9.2)') 'rlon=',minval(rlon),maxval(rlon)
    deallocate(sfld)
    close(nsig)
    return

  end subroutine set_rsmparm
!
! clean up
!
  subroutine clean_rsmparm
    implicit none

    deallocate(rlon,rlat,sig,sigh,mapf)
  end subroutine clean_rsmparm
!
! read restart file
!
  subroutine read_restart(cfile,v3dg,v3dhg,v2dg)
    implicit none
    character(len=*), intent(in) :: cfile
    !integer, intent(in) :: nsig
    real(kind=dp), intent(out) :: v3dg(nlon,nlat,nlev,nv3d)
    real(kind=dp), intent(out) :: v3dhg(nlon,nlat,nlev+1,nv3dh)
    real(kind=dp), intent(out) :: v2dg(nlon,nlat,nv2d)

    integer :: nsig
    real(kind=dp), allocatable :: dfld(:,:,:)
    real(kind=dp), allocatable :: dummp(:,:,:),dumlat(:),dumlon(:) !dummy
    integer :: k,kk

    allocate( dfld(igrd1,jgrd1,nflds) )
    allocate( dummp(igrd1,jgrd1,3) )
    allocate( dumlat(jgrd1), dumlon(igrd1) )

    nsig=70
    open(nsig,file=cfile,access='sequential',form='unformatted',action='read')
    call read_sig( nsig,igrd1,jgrd1,nlev,nflds,nonhyd,icld,fhour,sig,&
      &  dfld,dummp,dumlat,dumlon )
    kk=1
    v2dg(:,:,iv2d_gz)=dfld(:,:,kk)
    kk=kk+1
    v2dg(:,:,iv2d_ps)=dfld(:,:,kk)
    kk=kk+1
    do k=1,nlev
      v3dg(:,:,k,iv3d_t) = dfld(:,:,kk) 
      kk=kk+1
    end do
    do k=1,nlev
      v3dg(:,:,k,iv3d_u) = dfld(:,:,kk)
      kk=kk+1
    end do
    do k=1,nlev
      v3dg(:,:,k,iv3d_v) = dfld(:,:,kk)
      kk=kk+1
    end do
    do k=1,nlev
      v3dg(:,:,k,iv3d_q) = dfld(:,:,kk)
      kk=kk+1
    end do
    do k=1,nlev
      v3dg(:,:,k,iv3d_oz) = dfld(:,:,kk)
      kk=kk+1
    end do
    do k=1,nlev
      v3dg(:,:,k,iv3d_cw) = dfld(:,:,kk)
      kk=kk+1
    end do
    if(nonhyd.eq.1) then
      do k=1,nlev
        v3dg(:,:,k,iv3d_pp) = dfld(:,:,kk)
        kk=kk+1
      end do
      do k=1,nlev
        v3dg(:,:,k,iv3d_tt) = dfld(:,:,kk)
        kk=kk+1
      end do
      do k=1,nlev+1
        v3dhg(:,:,k,iv3d_ww) = dfld(:,:,kk)
        kk=kk+1
      end do
    end if
    close(nsig)

    return
  end subroutine read_restart
!
! write restart file
!
  subroutine write_restart(cfile,v3dg,v3dhg,v2dg)
    implicit none
    character(len=*), intent(in) :: cfile
    !integer, intent(in) :: nsig
    real(kind=dp), intent(in) :: v3dg(nlon,nlat,nlev,nv3d)
    real(kind=dp), intent(in) :: v3dhg(nlon,nlat,nlev+1,nv3dh)
    real(kind=dp), intent(in) :: v2dg(nlon,nlat,nv2d)

    integer :: nsig
    real(kind=dp), allocatable :: dfld(:,:,:)
    real(kind=sp) :: ext(nwext)
    integer :: k,kk

    allocate( dfld(igrd1,jgrd1,nflds) )
    ext(1) = real(iwav1,kind=sp)
    ext(2) = real(jwav1,kind=sp)
    ext(3) = real(igrd1,kind=sp)
    ext(4) = real(jgrd1,kind=sp)
    ext(5) = real(nlev,kind=sp)
    ext(6) = real(2+nlev*4+5,kind=sp)
    ext(7) = real(nproj,kind=sp)
    ext(8) = real(rtruth,kind=sp)
    ext(9) = real(rorient,kind=sp)
    ext(10)= real(rcenlat,kind=sp)
    ext(11)= real(rcenlon,kind=sp)
    ext(12)= real(rlftgrd,kind=sp)
    ext(13)= real(rbtmgrd,kind=sp)
    ext(14)= real(rdelx,kind=sp)
    ext(15)= real(rdely,kind=sp)
    ext(16)= real(nonhyd,kind=sp)
    kk=1
    dfld(:,:,kk)=v2dg(:,:,iv2d_gz)
    kk=kk+1
    dfld(:,:,kk)=v2dg(:,:,iv2d_ps)
    kk=kk+1
    do k=1,nlev
      dfld(:,:,kk)=v3dg(:,:,k,iv3d_t)
      kk=kk+1
    end do
    do k=1,nlev
      dfld(:,:,kk)=v3dg(:,:,k,iv3d_u)
      kk=kk+1
    end do
    do k=1,nlev
      dfld(:,:,kk)=v3dg(:,:,k,iv3d_v)
      kk=kk+1
    end do
    do k=1,nlev
      dfld(:,:,kk)=v3dg(:,:,k,iv3d_q)
      kk=kk+1
    end do
    do k=1,nlev
      dfld(:,:,kk)=v3dg(:,:,k,iv3d_oz)
      kk=kk+1
    end do
    do k=1,nlev
      dfld(:,:,kk)=v3dg(:,:,k,iv3d_cw)
      kk=kk+1
    end do
    if(nonhyd.eq.1) then
      do k=1,nlev
        dfld(:,:,kk)=v3dg(:,:,k,iv3d_pp)
        kk=kk+1
      end do
      do k=1,nlev
        dfld(:,:,kk)=v3dg(:,:,k,iv3d_tt)
        kk=kk+1
      end do
      do k=1,nlev+1
        dfld(:,:,kk)=v3dhg(:,:,k,iv3d_ww)
        kk=kk+1
      end do
    end if
    nsig=80
    open(nsig,file=cfile,form='unformatted',access='sequential')
    call write_sig(nsig,label,idate,fhour,sig,sigh,ext,&
     &  igrd1,jgrd1,nlev,nflds,nonhyd,icld,dfld,mapf,rlat,rlon)
    close(nsig)

    return
  end subroutine write_restart
!
end module rsmcom_module
