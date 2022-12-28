module corsm_module
  use kind_module
  use co_module
  use nml_module
  use rsmcom_module

  implicit none
  public

  ! longitudinal, latitudinal and horizontal number of points in each process (not including ghost points)
  integer,save :: ni1, nj1, nij1 
  ! indexes of boundary points in each process (including ghost points)
  integer,save :: iminp, imaxp, jminp, jmaxp
  ! image number for local domain
  integer,allocatable,save :: nidom(:), njdom(:), imgdom(:,:)

  ! max number of points (not including ghost point, used for allocating coarrays)
  integer,save :: nij1max,ni1max,nj1max
  integer,allocatable,save :: nij1node(:),ni1node(:),nj1node(:)
  real(kind=dp),allocatable,save :: myrlon(:)[:], myrlat(:)[:]

contains
!
! initialize
!
  subroutine set_corsm
    implicit none
    real(kind=dp), allocatable :: v2dg(:,:,:)
    real(kind=dp), allocatable :: buf(:,:,:)
    integer :: i,ii
    integer :: is,ie,js,je

    write(6,'(a)') 'Hello from set_corsm'
    call read_nml_corsm

    call set_domain

    allocate( myrlon(1-ighost:ni1max+ighost)[*] )
    allocate( myrlat(1-jghost:nj1max+jghost)[*] )

    allocate( v2dg(nlon,nlat,2) )
    allocate( buf(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nimages) )
    do i=1,nlat
      v2dg(:,i,1) = rlon(:)
      v2dg(:,i,2) = rlat(i)
    end do
    call grd_to_buf(v2dg(:,:,1),buf)
    myrlon(:) = buf(:,1,myimage)
    call grd_to_buf(v2dg(:,:,2),buf)
    myrlat(:) = buf(1,:,myimage)
!    if(nidom(myimage)==1) then
!      ii=1
!    else
!      ii=0
!    end if
!    do i=iminp,imaxp
!      myrlon(ii) = rlon(i)
!      ii=ii+1
!    end do
!    if(njdom(myimage)==1) then
!      ii=1
!    else
!      ii=0
!    end if
!    do i=jminp,jmaxp
!      myrlat(ii) = rlat(i)
!      ii=ii+1
!    end do

    is=1;ie=ni1;js=1;je=nj1
    if(nisep.gt.1.and.ighost.gt.0) then
      if(nidom(myimage)==1) then
        is=1
        ie=ni1+ighost
      else if(nidom(myimage)==nisep) then
        is=1-ighost
        ie=ni1
      else
        is=1-ighost
        ie=ni1+ighost
      end if
    end if
    if(njsep.gt.1.and.jghost.gt.0) then
      if(njdom(myimage)==1) then
        js=1
        je=nj1+jghost
      else if(njdom(myimage)==njsep) then
        js=1-jghost
        je=nj1
      else
        js=1-jghost
        je=nj1+jghost
      end if
    end if
    write(6,'(a,i4.4,f8.3,a,f8.3)') &
    & 'MYIMAGE ',myimage,myrlon(is),'<=lon<=',myrlon(ie)
    write(6,'(a,i4.4,f8.3,a,f8.3)') &
    & 'MYIMAGE ',myimage,myrlat(js),'<=lat<=',myrlat(je)

    sync all
    return
  end subroutine set_corsm
!
! set local domain
!
  subroutine set_domain
    implicit none
    integer :: i,j,m,n

    allocate( nidom(nimages),njdom(nimages),imgdom(nisep,njsep) )
    m=1
    do j=1,njsep
      do i=1,nisep
        imgdom(i,j) = m
        nidom(m) = i
        njdom(m) = j
        m=m+1
      end do
    end do
    write(6,*) 'nidom=',nidom
    write(6,*) 'njdom=',njdom

    allocate( ni1node(nimages) )
    i = mod(nlon,nisep)
    ni1max = (nlon - i)/nisep + 1
    if(nidom(myimage) .le. i) then
      ni1 = ni1max
    else
      ni1 = ni1max - 1
    end if
    do m=1,nimages
      if(nidom(m) .le. i) then
        ni1node(m) = ni1max
      else
        ni1node(m) = ni1max - 1
      end if
    end do
    iminp=1
    do i=2,nidom(myimage)
      iminp=iminp+ni1node(imgdom(i-1,njdom(myimage)))
    end do
    imaxp=iminp+ni1-1
    iminp=max(iminp-ighost,1)
    imaxp=min(imaxp+ighost,nlon)
    write(6,'(a,i4.4,3(a,i6))') &
            'MYIMAGE ',myimage,' number of longitudinal grid points: ni1=',ni1,' imin=',iminp,' imax=',imaxp
    
    allocate( nj1node(nimages) )
    i = mod(nlat,njsep)
    nj1max = (nlat - i)/njsep + 1
    if(njdom(myimage).le.i) then
      nj1 = nj1max
    else
      nj1 = nj1max - 1
    end if
    do m=1,nimages
      if(njdom(m).le.i) then
        nj1node(m) = nj1max
      else
        nj1node(m) = nj1max - 1
      end if
    end do
    jminp=1
    do j=2,njdom(myimage)
      jminp=jminp+nj1node(imgdom(nidom(myimage),j-1))
    end do
    jmaxp=jminp+nj1-1
    jminp=max(jminp-jghost,1)
    jmaxp=min(jmaxp+jghost,nlat)
    write(6,'(a,i4.4,3(a,i6))') &
            'MYIMAGE ',myimage,' number of latitudinal grid points: nj1=',nj1,' jmin=',jminp,' jmax=',jmaxp

    allocate( nij1node(nimages) )
    nij1 = ni1*nj1
    nij1max = ni1max*nj1max
    write(6,'(a,i4.4,a,i6)') 'MYIMAGE ',myimage,' number of grid points: nij1=',nij1
    do n=1,nimages
      nij1node(n) = ni1node(n)*nj1node(n)
    end do
    write(6,*) 'grid points per node: ',nij1node,' sum: ',sum(nij1node)

  end subroutine set_domain
!
! read control data and distribute to processes
!
  subroutine read_cntl(filename,v3d,v2d)
    implicit none
    character(filelenmax), intent(in) :: filename
    real(kind=dp), intent(out) :: v3d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d)[*]
    real(kind=dp), intent(out) :: v2d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nv2d)[*]
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
    real(kind=dp), intent(in) :: v3d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d)[*]
    real(kind=dp), intent(in) :: v2d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nv2d)[*]
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
    real(kind=dp), intent(out) :: v3d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,member,nv3d)[*]
    real(kind=dp), intent(out) :: v2d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,     member,nv2d)[*]
    real(kind=dp), allocatable :: v3dg(:,:,:,:), v2dg(:,:,:)
    real(kind=dp), allocatable :: work3d(:,:,:,:)[:], work2d(:,:,:)[:]
    character(filelenmax) :: filename
    integer :: l,n,ll,im

    allocate( v3dg(nlon,nlat,nlev,nv3d), v2dg(nlon,nlat,nv2d) )
    allocate( work3d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d)[*] )
    allocate( work2d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nv2d)[*] )
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
          v3d(:,:,:,im,:) = work3d
          v2d(:,:,  im,:) = work2d
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
    real(kind=dp), intent(in) :: v3d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,member,nv3d)[*]
    real(kind=dp), intent(in) :: v2d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,     member,nv2d)[*]
    real(kind=dp), allocatable :: v3dg(:,:,:,:), v2dg(:,:,:)
    real(kind=dp), allocatable :: work3d(:,:,:,:)[:], work2d(:,:,:)[:]
    character(filelenmax) :: filename
    integer :: l,n,ll,im

    allocate( v3dg(nlon,nlat,nlev,nv3d), v2dg(nlon,nlat,nv2d) )
    allocate( work3d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d)[*] )
    allocate( work2d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nv2d)[*] )
    ll = ceiling(real(member)/real(nimages))
    do l=1,ll
      do n=1,nimages
        im = n + (l-1)*nimages
        if(im.le.member) then
          work3d = v3d(:,:,:,im,:)
          work2d = v2d(:,:,  im,:)
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
    real(kind=dp), intent(in) :: v3d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,member,nv3d)[*]
    real(kind=dp), intent(in) :: v2d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,     member,nv2d)[*]
    real(kind=dp), allocatable :: v3dm(:,:,:,:)[:]
    real(kind=dp), allocatable :: v2dm(:,:,:)[:]
    real(kind=dp), allocatable :: v3ds(:,:,:,:)[:]
    real(kind=dp), allocatable :: v2ds(:,:,:)[:]
    real(kind=dp), allocatable :: v3dg(:,:,:,:), v2dg(:,:,:)
    integer :: i,j,k,m,n
    character(filelenmax) :: filename

    allocate( v3dm(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d)[*] )
    allocate( v2dm(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nv2d)[*] )
    allocate( v3ds(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d)[*] )
    allocate( v2ds(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nv2d)[*] )
    allocate( v3dg(nlon,nlat,nlev,nv3d), v2dg(nlon,nlat,nv2d) )
    call ensmean_grd(member,ni1max+2*ighost,nj1max+2*jghost,v3d,v2d,v3dm,v2dm)
    v3ds = 0.0_dp
    v2ds = 0.0_dp
    do n=1,nv3d
!$OMP PARALLEL DO PRIVATE(i,j,k,m)
      do k=1,nlev
        do j=1,nj1
          do i=1,ni1
            do m=1,member
              v3ds(i,j,k,n)=v3ds(i,j,k,n)+(v3d(i,j,k,m,n)-v3dm(i,j,k,n))**2
            end do
          end do
        end do
      end do
!$OMP END PARALLEL DO
    end do
    do n=1,nv2d
!$OMP PARALLEL DO PRIVATE(i,j,m)
      do j=1,nj1
        do i=1,ni1
          do m=1,member
            v2ds(i,j,n)=v2ds(i,j,n)+(v2d(i,j,m,n)-v2dm(i,j,n))**2
          end do
        end do
      end do
!$OMP END PARALLEL DO
    end do
    do n=1,nv3d
!$OMP PARALLEL DO PRIVATE(i,j,k)
      do k=1,nlev
        do j=1,nj1
          do i=1,ni1
            v3ds(i,j,k,n)=sqrt(v3ds(i,j,k,n)/real(member-1,kind=dp))
          end do
        end do
      end do
!$OMP END PARALLEL DO
    end do
    do n=1,nv2d
!$OMP PARALLEL DO PRIVATE(i,j)
      do j=1,nj1
        do i=1,ni1
          v2ds(i,j,n)=sqrt(v2ds(i,j,n)/real(member-1,kind=dp))
        end do
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
    real(kind=dp), intent(out):: v3d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d)[*]
    real(kind=dp), intent(out):: v2d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nv2d)[*]

    real(kind=dp), allocatable :: buf3d(:,:,:,:,:), buf2d(:,:,:,:), buf(:,:,:)
    integer :: j,k,n

    allocate( buf(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nimages) )
    allocate( buf3d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d,nimages) )
    allocate( buf2d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,     nv2d,nimages) )
    if(myimage .eq. nrank) then
      do n=1,nv3d
        do k=1,nlev
          call grd_to_buf(v3dg(:,:,k,n),buf)
          do j=1,nimages
            buf3d(:,:,k,n,j) = buf(:,:,j)
          end do
        end do
      end do
      do n=1,nv2d
        call grd_to_buf(v2dg(:,:,n),buf)
        do j=1,nimages
          buf2d(:,:,n,j) = buf(:,:,j)
        end do
      end do

      do j=1,nimages
        v3d(:,:,:,:)[j]=buf3d(:,:,:,:,j)
        v2d(:,:,  :)[j]=buf2d(:,:,:,j)
      end do
    end if
    sync all

    deallocate( buf,buf3d,buf2d )
    return
  end subroutine scatter_grd
!
! gather gridded data from processes (all -> nrank)
!
  subroutine gather_grd(nrank,v3d,v2d,v3dg,v2dg)
    integer, intent(in) :: nrank
    real(kind=dp), intent(in) :: v3d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d)[*]
    real(kind=dp), intent(in) :: v2d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nv2d)[*]
    real(kind=dp), intent(out):: v3dg(nlon,nlat,nlev,nv3d)
    real(kind=dp), intent(out):: v2dg(nlon,nlat,nv2d)

    real(kind=dp), allocatable :: buf3d(:,:,:,:,:), buf2d(:,:,:,:), buf(:,:,:)
    integer :: j,k,n

    allocate( buf(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nimages) )
    allocate( buf3d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d,nimages) )
    allocate( buf2d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,     nv2d,nimages) )

    if(myimage.eq.nrank) then
      do j=1,nimages
        buf3d(:,:,:,:,j)=v3d(:,:,:,:)[j]
        buf2d(:,:,:,  j)=v2d(:,:,  :)[j]
      end do
      do n=1,nv3d
        do k=1,nlev
          buf = buf3d(:,:,k,n,:)
          call buf_to_grd(buf,v3dg(:,:,k,n))
        end do
      end do
      do n=1,nv2d
        buf = buf2d(:,:,n,:)
        call buf_to_grd(buf,v2dg(:,:,n))
      end do
    end if
    sync all

    deallocate( buf3d,buf2d,buf )
    return
  end subroutine gather_grd
!
! gridded data -> buffer
!
  subroutine grd_to_buf(grd,buf)
    implicit none
    real(kind=dp), intent(in) :: grd(nlon,nlat)
    real(kind=dp), intent(out):: buf(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nimages)
    integer :: nslon,nslat
    integer :: i,j,m,ii,ilon,ilat

    buf = undef
    do m=1,nimages
      nslon=0
      nslat=0
      do ii=2,nidom(m)
        nslon=nslon+ni1node(imgdom(ii-1,njdom(m)))
      end do
      do ii=2,njdom(m)
        nslat=nslat+nj1node(imgdom(nidom(m),ii-1))
      end do
!!DEBUG      print *, 'image ',m,' nslon ',nslon,' nslat ',nslat
      do j=1-jghost,nj1node(m)+jghost
        ilat=j+nslat
        if(ilat.lt.1) cycle
        if(ilat.gt.nlat) cycle
        do i=1-ighost,ni1node(m)+ighost
          ilon=i+nslon
          if(ilon.lt.1) cycle
          if(ilon.gt.nlon) cycle
          buf(i,j,m) = grd(ilon,ilat)
        end do
      end do
    end do
 
    return
  end subroutine grd_to_buf
!
! buffer -> gridded data
!
  subroutine buf_to_grd(buf,grd)
    implicit none
    real(kind=dp), intent(in) :: buf(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nimages)
    real(kind=dp), intent(out):: grd(nlon,nlat)
    integer :: nslon,nslat
    integer :: i,j,m,ii,ilon,ilat

    do m=1,nimages
      nslon=0
      nslat=0
      do ii=2,nidom(m)
        nslon=nslon+ni1node(imgdom(ii-1,njdom(m)))
      end do
      do ii=2,njdom(m)
        nslat=nslat+nj1node(imgdom(nidom(m),ii-1))
      end do
!!DEBUG      print *, 'image ',m,' nslon ',nslon,' nslat ',nslat
      do j=1,nj1node(m)
        ilat=j+nslat
!        if(ilat.lt.1) cycle
!        if(ilat.gt.nlat) cycle
        do i=1,ni1node(m)
          ilon=i+nslon
!          if(ilon.lt.1) cycle
!          if(ilat.gt.nlon) cycle
          grd(ilon,ilat) = buf(i,j,m)
        end do
      end do
    end do

    return
  end subroutine buf_to_grd
end module corsm_module
