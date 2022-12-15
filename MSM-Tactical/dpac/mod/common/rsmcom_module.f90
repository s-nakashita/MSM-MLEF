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
  integer,save      :: nv2d=2    !gz,ps,(wbtm)
  integer,parameter :: iv2d_gz=1 ! terrain height
  integer,parameter :: iv2d_ps=2 ! surface pressure
  integer,parameter :: iv2d_wb=3 ! vertical velocity at bottom level
  !!! 3D variables
  !!! pp,tt,ww are only for nonhydrostatic
  integer,save      :: nv3d
  integer,parameter :: nv3d_hyd=6    !t,u,v,q,oz,cw 
  integer,parameter :: nv3d_nonhyd=3 !pp,tt,ww(halflevel)
  integer,parameter :: iv3d_t=1  ! temperature
  integer,parameter :: iv3d_u=2  ! x-direction wind
  integer,parameter :: iv3d_v=3  ! y-direction wind
  integer,parameter :: iv3d_q=4  ! specific humidity
  integer,parameter :: iv3d_oz=5 ! ozone mixing ratio
  integer,parameter :: iv3d_cw=6 ! cloud water
  integer,parameter :: iv3d_pp=7 ! full pressure perturbation
  integer,parameter :: iv3d_tt=8 ! temperature perturbation
  integer,parameter :: iv3d_ww=9 ! vertical velocity at half levels (except bottom)
  
  integer,save :: nlevall
  character(len=4),allocatable :: varnames(:)
!
! IO
!
  character(len=4) :: filesuffix='.grd'
!
  contains
!
! set grid information
!
  subroutine set_rsmparm(cfile)
    implicit none
!    integer,intent(in) :: nsig ! input sigma file unit
    character(len=*), intent(in) :: cfile !input sigma file
    integer :: nsig
    integer :: nskip
    real(kind=sp) :: ext(nwext)
    real(kind=dp) :: sisl(2*levmax+1)
    real(kind=sp),allocatable :: sfld(:)
    integer :: iwav, jwav
    integer :: i,j,k
    
    write(6,'(A)') 'start set_rsmparm'
    
    !nsig=70
    call search_fileunit(nsig)
    write(6,'(3a,i3)') 'open file ',trim(cfile)//filesuffix,' unit=',nsig
    open(nsig,file=trim(cfile)//filesuffix,access='sequential',form='unformatted',action='read')
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
      nv2d=3
      nv3d=nv3d_hyd+nv3d_nonhyd
      write(6,*) 'model version is nonhydrostatic'
      allocate( varnames(nv3d+nv2d) )
      varnames = (/'   T','   U','   V','   Q','  OZ','  CW',&
                   '  Pn','  Tn','  Wn','  GZ','  Ps','  Wb'/)
    else
      nv2d=2
      nv3d=nv3d_hyd
      write(6,*) 'model version is hydrostatic'
      allocate( varnames(nv3d+nv2d) )
      varnames = (/'   T','   U','   V','   Q','  OZ','  CW',&
                   '  GZ','  Ps'/)
    end if
    nlevall=nv3d*nlev+nv2d
    nskip=2+nlevall!+3
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
! ensemble mean
!
  subroutine ensmean_grd(mem,nij,v3d,v2d,v3dm,v2dm)
    implicit none
    integer, intent(in) :: mem, nij
    real(kind=dp),intent(in) :: v3d(nij,nlev,mem,nv3d)
    real(kind=dp),intent(in) :: v2d(nij,     mem,nv2d)
    real(kind=dp),intent(out):: v3dm(nij,nlev,nv3d)
    real(kind=dp),intent(out):: v2dm(nij,     nv2d)
    integer :: i,k,m,n

    do n=1,nv3d
!$OMP PARALLEL DO PRIVATE(i,k,m)
      do k=1,nlev
        do i=1,nij
          v3dm(i,k,n)=v3d(i,k,1,n)
          do m=2,mem
            v3dm(i,k,n)=v3dm(i,k,n)+v3d(i,k,m,n)
          end do
          v3dm(i,k,n)=v3dm(i,k,n)/real(mem,kind=dp)
        end do
      end do
!$OMP END PARALLEL DO
    end do

    do n=1,nv2d
!$OMP PARALLEL DO PRIVATE(i,m)
      do i=1,nij
        v2dm(i,n)=v2d(i,1,n)
        do m=2,mem
          v2dm(i,n)=v2dm(i,n)+v2d(i,m,n)
        end do
        v2dm(i,n)=v2dm(i,n)/real(mem,kind=dp)
      end do
!$OMP END PARALLEL DO
    end do

    return
  end subroutine ensmean_grd
!
! read restart file
!
  subroutine read_restart(cfile,v3dg,v2dg)
    implicit none
    character(len=*), intent(in) :: cfile
    !integer, intent(in) :: nsig
    real(kind=dp), intent(out) :: v3dg(nlon,nlat,nlev,nv3d)
    real(kind=dp), intent(out) :: v2dg(nlon,nlat,nv2d)

    integer :: nsig
    real(kind=dp), allocatable :: dfld(:,:,:)
    real(kind=dp), allocatable :: dummp(:,:,:),dumlat(:),dumlon(:) !dummy
    integer :: k,kk

    allocate( dfld(igrd1,jgrd1,nflds) )
    allocate( dummp(igrd1,jgrd1,3) )
    allocate( dumlat(jgrd1), dumlon(igrd1) )

    !nsig=70
    call search_fileunit(nsig)
    write(6,'(3a,i3)') 'open file ',trim(cfile)//filesuffix,' unit=',nsig
    open(nsig,file=trim(cfile)//filesuffix,access='sequential',form='unformatted',action='read')
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
      v2dg(:,:,iv2d_wb) = dfld(:,:,kk)
      kk=kk+1
      do k=1,nlev
        v3dg(:,:,k,iv3d_ww) = dfld(:,:,kk)
        kk=kk+1
      end do
    end if
    close(nsig)

    return
  end subroutine read_restart
!
! write restart file
!
  subroutine write_restart(cfile,v3dg,v2dg)
    implicit none
    character(len=*), intent(in) :: cfile
    !integer, intent(in) :: nsig
    real(kind=dp), intent(in) :: v3dg(nlon,nlat,nlev,nv3d)
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
      dfld(:,:,kk)=v2dg(:,:,iv2d_wb)
      kk=kk+1
      do k=1,nlev
        dfld(:,:,kk)=v3dg(:,:,k,iv3d_ww)
        kk=kk+1
      end do
    end if
    !nsig=80
    call search_fileunit(nsig)
    write(6,'(3a,i3)') 'open file ',trim(cfile)//filesuffix,' unit=',nsig
    open(nsig,file=trim(cfile)//filesuffix,form='unformatted',access='sequential')
    call write_sig(nsig,label,idate,fhour,sig,sigh,ext,&
     &  igrd1,jgrd1,nlev,nflds,nonhyd,icld,dfld,mapf,rlat,rlon)
    close(nsig)

    return
  end subroutine write_restart
!
! search empty file unit (referring to JMA GSM)
!
  subroutine search_fileunit(iunit)
    implicit none
    integer, intent(out) :: iunit
    integer, parameter :: iunit_s=11, iunit_e=99
    integer :: iu
    logical :: lopened, lexist

    iunit=-999

    do iu=iunit_s,iunit_e
      inquire(unit=iu,opened=lopened,exist=lexist)
      if(lexist.and.(.not.lopened)) then
        iunit=iu
        exit
      end if
    end do

    if(iunit.eq.-999) then
      write(6,'(a,i3,a,i3,a)') 'search_fileunit : error : unit number from ', &
       & iunit_s, ' to ', iunit_e, ' are opened.'
      stop 999
    end if
    return
  end subroutine search_fileunit 
!
end module rsmcom_module
