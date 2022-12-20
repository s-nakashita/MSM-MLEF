module corsm_module
  use kind_module
  use co_module
  use nml_module
  use rsmcom_module

  implicit none
  public

  integer,save :: nij1
  integer,save :: nij1max
  integer,allocatable,save :: nij1node(:)
  real(kind=dp),allocatable,save :: myrlon(:)[:], myrlat(:)[:]

contains
!
! initialize
!
  subroutine set_corsm
    implicit none
    real(kind=dp), allocatable :: v2dg(:,:,:)
    real(kind=dp), allocatable :: v2d(:,:)[:]
    integer :: i,n

    write(6,'(a)') 'Hello from set_corsm'
    allocate( nij1node(nimages) )
    i = mod(nlon*nlat,nimages)
    nij1max = (nlon*nlat - i)/nimages + 1
    if(myimage .le. i) then
      nij1 = nij1max
    else
      nij1 = nij1max - 1
    end if
    write(6,'(a,i4.4,a,i6)') 'MYIMAGE ',myimage,' number of grid points: nij1=',nij1
    do n=1,nimages
      if(n.le.i) then
        nij1node(n) = nij1max
      else
        nij1node(n) = nij1max - 1
      end if
    end do

    allocate( myrlon(nij1max)[*] )
    allocate( myrlat(nij1max)[*] )
    allocate( v2dg(nlon,nlat,2) )
    allocate( v2d(nij1max,2)[*] )
    if(myimage.eq.1) then
      do i=1,nlat
        v2dg(:,i,1) = rlon(:)
        v2dg(:,i,2) = rlat(i)
      end do
      call grd_to_buf(v2dg(:,:,1),v2d(:,1))
      call grd_to_buf(v2dg(:,:,2),v2d(:,2))
    end if
    sync all
    myrlon(1:nij1) = v2d(1:nij1,1)
    myrlat(1:nij1) = v2d(1:nij1,2)

    write(6,'(a,i4.4,a,10f8.3)') &
    & 'MYIMAGE ',myimage,' lon = ',myrlon(1:10)
    write(6,'(a,i4.4,a,10f8.3)') &
    & 'MYIMAGE ',myimage,' lat = ',myrlat(1:10)

    deallocate( v2dg,v2d )
    return
  end subroutine set_corsm
!
! read control data and distribute to processes
!
  subroutine read_cntl(filename,v3d,v2d)
    implicit none
    character(filelenmax), intent(in) :: filename
    real(kind=dp), intent(out) :: v3d(nij1max,nlev,nv3d)[*]
    real(kind=dp), intent(out) :: v2d(nij1max,nv2d)[*]
    real(kind=dp), allocatable :: v3dg(:,:,:,:), v2dg(:,:,:)

    allocate( v3dg(nlon,nlat,nlev,nv3d), v2dg(nlon,nlat,nv2d) )
    if(myimage.eq.1) then
      write(6,'(a,i4.4,2a)') 'MYIMAGE ',myimage,' is reading a file ',filename
      call read_restart(filename,v3dg,v2dg)
    end if
    sync all
    call scatter_grd(1,v3dg,v2dg,v3d,v2d)
    return
  end subroutine read_cntl
!
! write control data after collecting data from processes
!
  subroutine write_cntl(filename,v3d,v2d)
    implicit none
    character(filelenmax), intent(in) :: filename
    real(kind=dp), intent(in) :: v3d(nij1max,nlev,nv3d)[*]
    real(kind=dp), intent(in) :: v2d(nij1max,nv2d)[*]
    real(kind=dp), allocatable :: v3dg(:,:,:,:), v2dg(:,:,:)

    allocate( v3dg(nlon,nlat,nlev,nv3d), v2dg(nlon,nlat,nv2d) )
    call gather_grd(1,v3d,v2d,v3dg,v2dg)
    sync all

    if(myimage.eq.1) then
      write(6,'(a,i4.4,2a)') 'MYIMAGE ',myimage,' is writing a file ',filename
      call write_restart(filename,v3dg,v2dg)
    end if
    return
  end subroutine write_cntl
!
! read ensemble data and distribute to processes
!
  subroutine read_ens(basename,v3d,v2d)
    implicit none
    character(filelenmax), intent(in) :: basename
    real(kind=dp), intent(out) :: v3d(nij1max,nlev,member,nv3d)[*]
    real(kind=dp), intent(out) :: v2d(nij1max,     member,nv2d)[*]
    real(kind=dp), allocatable :: v3dg(:,:,:,:), v2dg(:,:,:)
    real(kind=dp), allocatable :: work3d(:,:,:)[:], work2d(:,:)[:]
    character(filelenmax) :: filename
    integer :: l,n,ll,im

    allocate( v3dg(nlon,nlat,nlev,nv3d), v2dg(nlon,nlat,nv2d) )
    allocate( work3d(nij1max,nlev,nv3d)[*], work2d(nij1max,nv2d)[*] )
    ll = ceiling(real(member)/real(nimages))
    do l=1,ll
      im = myimage + (l-1)*nimages
      if(im.le.member) then
        call file_member_replace(im,basename,filename)
        write(6,'(a,i4.4,2a)') 'MYIMAGE ',myimage,' is reading a file ',filename
        call read_restart(filename,v3dg,v2dg)
      end if

      do n=1,nimages
        im = n + (l-1)*nimages
        if(im.le.member) then
          call scatter_grd(n,v3dg,v2dg,work3d,work2d)
          v3d(:,:,im,:) = work3d
          v2d(:,  im,:) = work2d
        end if
      end do
    end do
    deallocate( v3dg,v2dg )
    deallocate( work3d,work2d )
    return
  end subroutine read_ens
!
! write ensemble data after collecting data from processes
!
  subroutine write_ens(basename,v3d,v2d)
    implicit none
    character(filelenmax), intent(in) :: basename
    real(kind=dp), intent(in) :: v3d(nij1max,nlev,member,nv3d)[*]
    real(kind=dp), intent(in) :: v2d(nij1max,     member,nv2d)[*]
    real(kind=dp), allocatable :: v3dg(:,:,:,:), v2dg(:,:,:)
    real(kind=dp), allocatable :: work3d(:,:,:)[:], work2d(:,:)[:]
    character(filelenmax) :: filename
    integer :: l,n,ll,im

    allocate( v3dg(nlon,nlat,nlev,nv3d), v2dg(nlon,nlat,nv2d) )
    allocate( work3d(nij1max,nlev,nv3d)[*], work2d(nij1max,nv2d)[*] )
    ll = ceiling(real(member)/real(nimages))
    do l=1,ll
      do n=1,nimages
        im = n + (l-1)*nimages
        if(im.le.member) then
          work3d = v3d(:,:,im,:)
          work2d = v2d(:,  im,:)
          call gather_grd(n,work3d,work2d,v3dg,v2dg)
        end if
      end do

      im = myimage + (l-1)*nimages
      if(im.le.member) then
        call file_member_replace(im,basename,filename)
        write(6,'(a,i4.4,2a)') 'MYIMAGE ',myimage,' is writing a file ',filename
        call write_restart(filename,v3dg,v2dg)
      end if
    end do
    deallocate( v3dg,v2dg )
    deallocate( work3d,work2d )
    return
  end subroutine write_ens
!
! write ensemble mean and spread
!
  subroutine write_ensmspr(basename,v3d,v2d)
    implicit none
    character(filelenmax), intent(in) :: basename
    real(kind=dp), intent(in) :: v3d(nij1max,nlev,member,nv3d)[*]
    real(kind=dp), intent(in) :: v2d(nij1max,     member,nv2d)[*]
    real(kind=dp), allocatable :: v3dm(:,:,:)[:]
    real(kind=dp), allocatable :: v2dm(:,:)[:]
    real(kind=dp), allocatable :: v3ds(:,:,:)[:]
    real(kind=dp), allocatable :: v2ds(:,:)[:]
    real(kind=dp), allocatable :: v3dg(:,:,:,:), v2dg(:,:,:)
    integer :: i,k,m,n
    character(filelenmax) :: filename

    allocate( v3dm(nij1max,nlev,nv3d)[*], v2dm(nij1max,nv2d)[*] )
    allocate( v3ds(nij1max,nlev,nv3d)[*], v2ds(nij1max,nv2d)[*] )
    allocate( v3dg(nlon,nlat,nlev,nv3d), v2dg(nlon,nlat,nv2d) )
    call ensmean_grd(member,nij1max,v3d,v2d,v3dm,v2dm)
    v3ds = 0.0_dp
    v2ds = 0.0_dp
    do n=1,nv3d
!$OMP PARALLEL DO PRIVATE(i,k,m)
      do k=1,nlev
        do i=1,nij1
          do m=1,member
            v3ds(i,k,n)=v3ds(i,k,n)+(v3d(i,k,m,n)-v3dm(i,k,n))**2
          end do
        end do
      end do
!$OMP END PARALLEL DO
    end do
    do n=1,nv2d
!$OMP PARALLEL DO PRIVATE(i,m)
      do i=1,nij1
        do m=1,member
          v2ds(i,n)=v2ds(i,n)+(v2d(i,m,n)-v2dm(i,n))**2
        end do
      end do
!$OMP END PARALLEL DO
    end do
    do n=1,nv3d
!$OMP PARALLEL DO PRIVATE(i,k)
      do k=1,nlev
        do i=1,nij1
          v3ds(i,k,n)=sqrt(v3ds(i,k,n)/real(member-1,kind=dp))
        end do
      end do
!$OMP END PARALLEL DO
    end do
    do n=1,nv2d
!$OMP PARALLEL DO PRIVATE(i)
      do i=1,nij1
        v2ds(i,n)=sqrt(v2ds(i,n)/real(member-1,kind=dp))
      end do
!$OMP END PARALLEL DO
    end do

    call gather_grd(1,v3dm,v2dm,v3dg,v2dg)
    if(myimage.eq.1) then
      call file_member_replace(member+1,basename,filename)
      write(6,'(a,i4.4,2a)') 'MYIMAGE ',myimage,' is writing a file ',filename
      call write_restart(filename,v3dg,v2dg)
    end if
    sync all
    call gather_grd(1,v3ds,v2ds,v3dg,v2dg)
    if(myimage.eq.1) then
      call file_member_replace(member+2,basename,filename)
      write(6,'(a,i4.4,2a)') 'MYIMAGE ',myimage,' is writing a file ',filename
      call write_restart(filename,v3dg,v2dg)
    end if
    return
  end subroutine write_ensmspr
!
! scatter gridded data to processes (nrank -> all)
!
  subroutine scatter_grd(nrank,v3dg,v2dg,v3d,v2d)
    implicit none
    integer, intent(in) :: nrank
    real(kind=dp), intent(in) :: v3dg(nlon,nlat,nlev,nv3d)
    real(kind=dp), intent(in) :: v2dg(nlon,nlat,nv2d)
    real(kind=dp), intent(out):: v3d(nij1max,nlev,nv3d)[*]
    real(kind=dp), intent(out):: v2d(nij1max,nv2d)[*]

    real(kind=dp), allocatable :: buf(:,:)[:]
    integer :: j,k,n

    allocate( buf(nij1max,nlevall)[*] )
    if(myimage .eq. nrank) then
      j=0
      do n=1,nv3d
        do k=1,nlev
          j=j+1
          call grd_to_buf(v3dg(:,:,k,n),buf(:,j))
        end do
      end do
      do n=1,nv2d
        j=j+1
        call grd_to_buf(v2dg(:,:,n),buf(:,j))
      end do
    end if
    sync all

    j=0
    do n=1,nv3d
      do k=1,nlev
        j=j+1
        v3d(1:nij1,k,n)=buf(1:nij1,j)
      end do
    end do
    do n=1,nv2d
      j=j+1
      v2d(1:nij1,n)=buf(1:nij1,j)
    end do
    sync all

    deallocate( buf )
    return
  end subroutine scatter_grd
!
! gather gridded data from processes (all -> nrank)
!
  subroutine gather_grd(nrank,v3d,v2d,v3dg,v2dg)
    integer, intent(in) :: nrank
    real(kind=dp), intent(in) :: v3d(nij1max,nlev,nv3d)[*]
    real(kind=dp), intent(in) :: v2d(nij1max,nv2d)[*]
    real(kind=dp), intent(out):: v3dg(nlon,nlat,nlev,nv3d)
    real(kind=dp), intent(out):: v2dg(nlon,nlat,nv2d)

    real(kind=dp), allocatable :: buf(:,:)[:]
    integer :: j,k,n

    allocate( buf(nij1max,nlevall)[*] )
    j=0
    do n=1,nv3d
      do k=1,nlev
        j=j+1
        buf(1:nij1,j) = v3d(1:nij1,k,n)
      end do
    end do
    do n=1,nv2d
      j=j+1
      buf(1:nij1,j) = v2d(1:nij1,n)
    end do
    sync all

    if(myimage.eq.nrank) then
      j=0
      do n=1,nv3d
        do k=1,nlev
          j=j+1
          call buf_to_grd(buf(:,j),v3dg(:,:,k,n))
        end do
      end do
      do n=1,nv2d
        j=j+1
        call buf_to_grd(buf(:,j),v2dg(:,:,n))
      end do
    end if
    sync all

    deallocate( buf )
    return
  end subroutine gather_grd
!
! gridded data -> buffer
!
  subroutine grd_to_buf(grd,buf)
    implicit none
    real(kind=dp), intent(in) :: grd(nlon,nlat)
    real(kind=dp), intent(out):: buf(nij1max)[*]
    integer :: i,j,m,ilon,ilat

    do m=1,nimages
      do i=1,nij1node(m)
        j = m-1 + nimages * (i-1)
        ilon = mod(j,nlon) + 1
        ilat = (j-ilon+1)/nlon + 1
        buf(i)[m] = grd(ilon,ilat)
      end do
    end do
 
    do m=1,nimages
      if(nij1node(m)<nij1max) buf(nij1max)[m] = undef
    end do

    return
  end subroutine grd_to_buf
!
! buffer -> gridded data
!
  subroutine buf_to_grd(buf,grd)
    implicit none
    real(kind=dp), intent(in) :: buf(nij1max)[*]
    real(kind=dp), intent(out):: grd(nlon,nlat)
    integer :: i,j,m,ilon,ilat

    do m=1,nimages
      do i=1,nij1node(m)
        j = m-1 + nimages * (i-1)
        ilon = mod(j,nlon) + 1
        ilat = (j-ilon+1)/nlon + 1
        grd(ilon,ilat) = buf(i)[m]
      end do
    end do

    return
  end subroutine buf_to_grd
!
end module corsm_module
