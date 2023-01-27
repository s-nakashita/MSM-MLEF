module spectral_module
!
! module : spectral_module   routines for grid <-> wave transformation
!
! note:
! This module provides wrapper routines for fft functions in sys/src/rsm_rinp.fd.
! for only serial use
!
! history:
! 22-09-13 create
! 
  use kind_module
  use rsmcom_module, only : iwav1, jwav1, lnwav, igrd1, jgrd1, lngrd
  use phconst_module
  use read_module
  use write_module

  integer, save :: igrd2, jgrd2
  ! common arrays
  real(kind=dp), save, allocatable :: ccosg(:,:), csing(:,:), gcosc(:,:), gsinc(:,:)
  integer, save :: ifax(20),jfax(20)
  real(kind=dp), save, allocatable :: trigx(:,:),trigy(:,:)
!  common /comrffti/ifax(20),jfax(20)
!  common /comrfft/trigx(igrd2,2),trigy(jgrd2,2)
  ! truncation
  integer, save :: ntrunc_

  public :: spectral_init, spectral_clean&
          , gdtocc, cctogd&
          , gdtosc, sctogd&
          , gdtocs, cstogd&
          , spectral_trunc
  contains
!======================================================================
! initialize & clean
!  include plnini.F and rftini.F contents
!======================================================================
subroutine spectral_init(ntrunc)
  implicit none
  integer, intent(in), optional :: ntrunc

  igrd2 = (igrd1-1)*2
  jgrd2 = (jgrd1-1)*2
  call plnini
  call rftini

  ntrunc_=0
  if(present(ntrunc)) ntrunc_=ntrunc
  if(ntrunc_.gt.0) then
    write(6,*) 'truncation number =',ntrunc_
  end if
  return
end subroutine spectral_init
!
subroutine spectral_clean
  implicit none
  deallocate( ccosg,csing,gcosc,gsinc,trigx,trigy )
  return
end subroutine spectral_clean
!======================================================================
! grid -> cos-cos wave
! input : grid(lngrd,km) - grid field
!         km - the second dimension of grid and ff
! output : coef(lnwav,km) - wave coefficients
!======================================================================
subroutine gdtocc(grid,coef,km)
  implicit none
  real(kind=dp), intent(inout)  :: grid(lngrd,km)
  real(kind=dp), intent(out) :: coef(lnwav,km)
  integer, intent(in) :: km
  real(kind=dp), allocatable :: grid3d(:,:,:)
  integer :: i,j,k,ij,jlat

  allocate( grid3d(igrd1,km,jgrd1) )
  do k=1,km
    do i=1,lnwav
      coef(i,k) = 0.0d0
    end do
  end do
  do k=1,km
    do j=1,jgrd1
      jlat=(j-1)*igrd1
      do i=1,igrd1
        ij=i+jlat
        grid3d(i,k,j) = grid(ij,k)
      end do
    end do
  end do
  call ffacosx1(grid3d,jgrd1,1,km,km)
  call ffacosy1(grid3d,coef,1,igrd1,iwav1,km,km)
  !call ffacosx1(grid, jgrd1, 1, 1, 1)
  !call ffacosy1(grid, coef, 1, igrd1, iwav1, 1, 1)
  deallocate(grid3d)

  return
end subroutine gdtocc
!
!======================================================================
! cos-cos wave -> grid
! input : coef(lnwav,km) - wave coefficients
!         km - the second dimension of grid and ff
! output : grid(lngrd,km) - grid field
!======================================================================
subroutine cctogd(coef,grid,km)
  implicit none
  real(kind=dp), intent(inout)  :: coef(lnwav,km)
  real(kind=dp), intent(out) :: grid(lngrd,km)
  integer, intent(in) :: km
  real(kind=dp), allocatable :: grid3d(:,:,:)
  integer :: i,j,k,ij,jlat

  allocate( grid3d(igrd1,km,jgrd1) )
  call ffscosy1(coef,grid3d,1,iwav1,igrd1,km,km)
  call ffscosx1(grid3d,jgrd1,1,km,km)
  do j=1,jgrd1
    jlat=(j-1)*igrd1
    do k=1,km
      do i=1,igrd1
        ij=i+jlat
        grid(ij,k) = grid3d(i,k,j)
      end do
    end do
  end do
  deallocate( grid3d )
!  call ffscosy1(coef,grid,1,iwav1,igrd1,1,1)
!  call ffscosx1(grid,jgrd1,1,1,1)

  return
end subroutine cctogd
!
!======================================================================
! grid -> sin-cos wave
! input : grid(lngrd,km) - grid field
!         km - the second dimension of grid and ff
! output : coef(lnwav,km) - wave coefficients
!======================================================================
subroutine gdtosc(grid,coef,km)
  implicit none
  real(kind=dp), intent(inout)  :: grid(lngrd,km)
  real(kind=dp), intent(out) :: coef(lnwav,km)
  integer, intent(in) :: km
  real(kind=dp), allocatable :: grid3d(:,:,:)
  integer :: i,j,k,ij,jlat

  allocate( grid3d(igrd1,km,jgrd1) )
  do k=1,km
    do i=1,lnwav
      coef(i,k) = 0.0d0
    end do
  end do
  do k=1,km
    do j=1,jgrd1
      jlat=(j-1)*igrd1
      do i=1,igrd1
        ij=i+jlat
        grid3d(i,k,j) = grid(ij,k)
      end do
    end do
  end do
  call ffasinx1(grid3d,jgrd1,1,km,km)
  call ffacosy1(grid3d,coef,1,igrd1,iwav1,km,km)
  !call ffasinx1(grid, jgrd1, 1, 1, 1)
  !call ffacosy1(grid, coef, 1, igrd1, iwav1, 1, 1)
  deallocate(grid3d)

  return
end subroutine gdtosc
!
!======================================================================
! sin-cos wave -> grid
! input : coef(lnwav,km) - wave coefficients
!         km - the second dimension of grid and ff
! output : grid(lngrd,km) - grid field
!======================================================================
subroutine sctogd(coef,grid,km)
  implicit none
  real(kind=dp), intent(inout)  :: coef(lnwav,km)
  real(kind=dp), intent(out) :: grid(lngrd,km)
  integer, intent(in) :: km
  real(kind=dp), allocatable :: grid3d(:,:,:)
  integer :: i,j,k,ij,jlat

  allocate( grid3d(igrd1,km,jgrd1) )
  call ffscosy1(coef,grid3d,1,iwav1,igrd1,km,km)
  call ffssinx1(grid3d,jgrd1,1,km,km)
  do j=1,jgrd1
    jlat=(j-1)*igrd1
    do k=1,km
      do i=1,igrd1
        ij=i+jlat
        grid(ij,k) = grid3d(i,k,j)
      end do
    end do
  end do
  deallocate( grid3d )
!  call ffscosy1(coef,grid,1,iwav1,igrd1,1,1)
!  call ffssinx1(grid,jgrd1,1,1,1)

  return
end subroutine sctogd
!
!======================================================================
! grid -> cos-sin wave
! input : grid(lngrd,km) - grid field
!         km - the second dimension of grid and ff
! output : coef(lnwav,km) - wave coefficients
!======================================================================
subroutine gdtocs(grid,coef,km)
  implicit none
  real(kind=dp), intent(inout)  :: grid(lngrd,km)
  real(kind=dp), intent(out) :: coef(lnwav,km)
  integer, intent(in) :: km
  real(kind=dp), allocatable :: grid3d(:,:,:)
  integer :: i,j,k,ij,jlat

  allocate( grid3d(igrd1,km,jgrd1) )
  do k=1,km
    do i=1,lnwav
      coef(i,k) = 0.0d0
    end do
  end do
  do k=1,km
    do j=1,jgrd1
      jlat=(j-1)*igrd1
      do i=1,igrd1
        ij=i+jlat
        grid3d(i,k,j) = grid(ij,k)
      end do
    end do
  end do
  call ffacosx1(grid3d,jgrd1,1,km,km)
  call ffasiny1(grid3d,coef,1,igrd1,iwav1,km,km)
  !call ffacosx1(grid, jgrd1, 1, 1, 1)
  !call ffasiny1(grid, coef, 1, igrd1, iwav1, 1, 1)
  deallocate(grid3d)

  return
end subroutine gdtocs
!
!======================================================================
! cos-sin wave -> grid
! input : coef(lnwav,km) - wave coefficients
!         km - the second dimension of grid and ff
! output : grid(lngrd,km) - grid field
!======================================================================
subroutine cstogd(coef,grid,km)
  implicit none
  real(kind=dp), intent(inout)  :: coef(lnwav,km)
  real(kind=dp), intent(out) :: grid(lngrd,km)
  integer, intent(in) :: km
  real(kind=dp), allocatable :: grid3d(:,:,:)
  integer :: i,j,k,ij,jlat

  allocate( grid3d(igrd1,km,jgrd1) )
  call ffssiny1(coef,grid3d,1,iwav1,igrd1,km,km)
  call ffscosx1(grid3d,jgrd1,1,km,km)
  do j=1,jgrd1
    jlat=(j-1)*igrd1
    do k=1,km
      do i=1,igrd1
        ij=i+jlat
        grid(ij,k) = grid3d(i,k,j)
      end do
    end do
  end do
  deallocate( grid3d )
!  call ffssiny1(coef,grid,1,iwav1,igrd1,1,1)
!  call ffscosx1(grid,jgrd1,1,1,1)

  return
end subroutine cstogd
!
!======================================================================
! truncation
! in-output : grid(lngrd,km) - grid field
! input : km - the second dimension of grid and ff
!         (optional) stype - FFT type cc(default) or sc or cs
!======================================================================
subroutine spectral_trunc(grid,km,stype)
  implicit none
  real(kind=dp), intent(inout) :: grid(lngrd,km)
  integer, intent(in) :: km
  character(len=2), intent(in), optional :: stype
  real(kind=dp), allocatable :: coef(:,:)
  real(kind=dp), allocatable :: grid3d(:,:,:)
  character(len=2) :: stype_
  integer :: i,j,k,ij,jlat

  stype_="cc"
  if(present(stype)) stype_=stype
  allocate( coef(lnwav,km) )

  if(stype_.eq."sc") then
    call gdtosc(grid,coef,km)
  else if(stype_.eq."cs") then
    call gdtocs(grid,coef,km)
  else
    call gdtocc(grid,coef,km)
  end if

  do i=1,iwav1
    do j=1,jwav1
      ij=(i-1)*jwav1+j
      k=(i-1)+(j-1)
      if(k.gt.ntrunc_) then
        coef(ij,:) = 0.0d0
      end if
    end do
  end do

  if(stype_.eq."sc") then
    call sctogd(coef,grid,km)
  else if(stype_.eq."cs") then
    call cstogd(coef,grid,km)
  else
    call cctogd(coef,grid,km)
  end if
  return
end subroutine spectral_trunc
!
!======================================================================
! private subroutines
!======================================================================
!
! sum coefficients of cosine function at ap in y direction
! into fln after fft(ffacosx)
!
subroutine sumggc1(ap,fln,qln,lnwavs,iwav1s,jwav1,igrd1s,n)
  implicit none
  real(kind=dp), intent(in) :: ap(igrd1s,n)
  real(kind=dp), intent(out):: fln(lnwavs,n)
  real(kind=dp), intent(in) :: qln(jwav1)
  integer, intent(in) :: lnwavs,iwav1s,jwav1,igrd1s,n
  real(kind=dp) :: sev,sod
  integer :: i,j,k,jplus0

  do k=1,n
    do i=1,iwav1s
      jplus0 = (i-1)*jwav1
      sev = ap(i,k)
      sod = ap(i,k)
      fln  (1+jplus0  ,k) = fln(1+jplus0  ,k) + qln(1)*sev
      do j=2,jwav1,2
        fln(j+jplus0  ,k) = fln(j+jplus0  ,k) + qln(j)*sod
        fln(j+1+jplus0,k) = fln(j+1+jplus0,k) + qln(j)*sev
      end do
    end do
  end do
  return
end subroutine sumggc1
!
! sum coefficients (fln) of cosine function in y direction into ap
! before doing fft in x direction
!
subroutine sumffc1(fln,ap,qln,lnwavs,iwav1s,jwav1,igrd1s,n)
  implicit none
  real(kind=dp), intent(in) :: fln(lnwavs,n)
  real(kind=dp), intent(out):: ap(igrd1s,n)
  real(kind=dp), intent(in) :: qln(jwav1)
  integer, intent(in) :: lnwavs,iwav1s,jwav1,igrd1s,n
  real(kind=dp) :: sev,sod
  integer :: i,j,k,jplus0
  
  do k=1,n
    do i=1,iwav1s
      sev = qln(1) * fln((i-1)*jwav1+1,k)
      sod = 0.0

      jplus0 = (i-1)*jwav1
      do j=2,jwav1,2
        sod = sod + qln(j  ) * fln(j  +jplus0,k)
        sev = sev + qln(j+1) * fln(j+1+jplus0,k)
      end do
      ap(i,k) = sev + sod
    end do
  end do
  return
end subroutine sumffc1
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  plnini
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  prepare array in common block for y-direction grid <-->
!            coefficient transformation for regional model.
!
! program history log:
!
! usage:    call plnini
!   input argument list:
!
!   output argument list:
!
!   input files: none
!
!   output files: none
!
!   subprograms called: none
!
!   remark: none
!
!
subroutine plnini
  implicit none
  real(kind=dp) :: rad, cc, ss, cogr, cowv
  real(kind=dp),dimension(jgrd1) ::  fc,fs
  integer :: jgrd
  integer :: i,ii,j,jj,l,ll,jl,jl0,nl,nf
!
  allocate( ccosg(jwav1,jgrd1),csing(jwav1,jgrd1),&
    &       gcosc(jwav1,jgrd1),gsinc(jwav1,jgrd1) )
  jgrd = jgrd1-1
!  do i=2,jgrd
  do i=2,jgrd1/2
    ii = i - 1
    rad = pi * float(ii) / float(jgrd)
    fc(i) = cos(rad)
    fs(i) = sin(rad)
    l = jgrd + 2 - i
    fc(l) = -1.0d0 * fc(i)
    fs(l) = fs(i)
  end do
  fc(1) = 1.0d0
  fs(1) = 0.0d0
!  fc(jgrd1) = -1.0d0
!  fs(jgrd1) =  0.0d0
  print *, 'fc=',fc
  print *, 'fs=',fs
!
!
  do j = 1, jgrd1
    jj = j - 1
!
    cogr=2.0d0 / float(jgrd)
    if( j.eq.1 .or. j.eq.jgrd1 ) cogr=cogr * 0.5d0
!
    do ll=1,jwav1
      cowv=1.0d0
      if( ll.eq.1 .or. ll.eq.jgrd1 ) cowv=0.5d0
      l = ll - 1
      nl = l * iwav1

      jl0 = jj * l
      jl = mod(jl0, jgrd)
      nf = mod(jl0/jgrd, 2)
      if( nf .eq. 1 ) then
        cc = -1.0
        ss = -1.0
      else
        cc =  1.0
        ss =  1.0
      endif
      jl = jl + 1
      ccosg(ll,j) =  fc(jl)*cowv*cc
      csing(ll,j) =  fs(jl)*cowv*ss
      gcosc(ll,j) =  fc(jl)*cogr*cc
      gsinc(ll,j) =  fs(jl)*cogr*ss
    end do
  end do
  print *, 'ccosg = ', maxval(ccosg), minval(ccosg)
  print *, 'csing = ', maxval(csing), minval(csing)
  print *, 'gcosc = ', maxval(gcosc), minval(gcosc)
  print *, 'gsinc = ', maxval(gsinc), minval(gsinc)
  return
end subroutine plnini
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  rftini
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  prepare fft initial constant arries for regional odel.
!
! program history log:
!
! usage:    call rftini
!   input argument list:
!
!   output argument list:
!
!   common block:
!      /comrfft/
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
! fftfax
!
!   remark: none
!
!
subroutine rftini
  implicit none
  real(kind=dp) :: trigs(1)
  integer :: i
!
  allocate( trigx(igrd2,2), trigy(jgrd2,2) )
  call fftfax (igrd2,ifax,trigx)
  call fftfax (jgrd2,ifax,trigy)
  do i=1,ifax(1)
    if ((ifax(i+1).ne.2).and.(ifax(i+1).ne.3).and.(ifax(i+1).ne.4) &
            .and.(ifax(i+1).ne.7).and.(ifax(i+1).ne.11)) then
            print 120
            stop
    end if
  end do
  do i=1,jfax(1)
    if ((jfax(i+1).ne.2).and.(jfax(i+1).ne.3).and.(jfax(i+1).ne.4) &
            .and.(jfax(i+1).ne.7).and.(jfax(i+1).ne.11)) then
            print 120
            stop
    end if
  end do
!  call fftfax (igrd2,ifax,trigs)
  if (ifax(1) .eq. -99) then
    print 120
    stop
  else
    print 140,igrd2
  endif
!
120   format (' error in rftini.  igrd2 not factorable. ')
130   format (' error in rftini.  jgrd2 not factorable. ')
140   format (' fftfax is  called in rftini. igrd2 = ',i10)
!
  return
end subroutine rftini
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  ffacos
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  fast fourier grid transform of cos wave in x direction.
!            for regional spectral model.
!
! program history log:
!
! usage:   call  ffacos (a, lot)
!   input argument list:
!     a   - grid values with dimension of igrd1
!     lot - second dimension of a
!
!   output argument list:
!     a     - coefficient values
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
!
! rfftmlt - fast fourier transform in x direction
!
!   remark: none
!
subroutine ffacosx1(a,jgrd1s, kx,lotsynk, lot)
  implicit none
  real(kind=dp),intent(inout) :: a(igrd1,lotsynk,jgrd1s)
  integer, intent(in) :: jgrd1s, kx, lotsynk, lot
  integer, parameter :: lotmin=64,lotmax=64,ncpu=1
!
  integer :: jump
  integer :: nlot,i,j,k,l,ll,lots
  real(kind=dp), allocatable :: work(:,:,:)
  real(kind=dp), allocatable :: al(:,:)
!
! grid to coefficient ( for regional use )
! cosine function only in x direction
!
!  multiple fast fourier transform - analysis.  isign=-1
!
  jump=igrd2+3
  allocate( work(igrd2,lotmax,2), al(jump,lotmax) )
  nlot=lotmax
  do k=1,lot

    do i=1,jgrd1s,nlot
      lots = min0(nlot, jgrd1s-i+1)
      do j=i,i+lots-1
        do l=1,igrd1
          al(l,j-i+1) = a(l,k+kx-1,j)
        end do
        do l=igrd1+1,igrd2
          ll=igrd2+2-l
          al(l,j-i+1) = a(ll,k+kx-1,j)
        end do
        do l=igrd2+1,jump
          al(l,j-i+1) = 0.0
        end do
      end do
!      print *, 'al in ffacosx ',minval(al),maxval(al)
!
!     call -1 fft for analysis.
      call rfftmlt(al,work,trigx,ifax,1,jump,igrd2,lots,-1)
!      print *, 'al in ffacosx ',minval(al),maxval(al)
!
      do j=i,i+lots-1
        do l=1,iwav1
          a(l,k+kx-1,j) = al(2*l-1,j-i+1) 
        end do
        do l=iwav1+1,igrd1
          a(l,k+kx-1,j) = 0.0
        end do
      end do
    end do
!
  enddo
  return
end subroutine ffacosx1
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  ffacos
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  fast fourier grid transform of cos wave in x direction.
!            for regional spectral model.
!
! program history log:
!
! usage:   call  ffacos (a, lot)
!   input argument list:
!     a   - grid values with dimension of jgrd1
!     lot - second dimension of a
!
!   output argument list:
!     a     - coefficient values
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
!
! rfftmlt - fast fourier transform in x direction
!
!   remark: none
!
subroutine ffacosy1(a,b,ky,igrd1s,iwav1s,lotsynk, lot)
  implicit none
  real(kind=dp),intent(inout) :: a(igrd1s,lotsynk,jgrd1)
  real(kind=dp),intent(out) :: b(jwav1,iwav1s,lot)
  integer, intent(in) :: ky,igrd1s,iwav1s,lotsynk,lot
  integer, parameter :: lotmin=64,lotmax=64,ncpu=1
  integer :: nlot,i,j,k,l,ll,lots
  integer :: jump
  real(kind=dp),allocatable :: work(:,:,:)
  real(kind=dp),allocatable :: al(:,:)
!
! grid to coefficient ( for regional use )
! cosine function only in x direction
!
!  multiple fast fourier transform - analysis.  isign=-1
!
  jump=jgrd2+3
  allocate( work(jgrd2,lotmax,2), al(jump,lotmax) )
  nlot=lotmax
  do k=1,lot

    do i=1,iwav1s,nlot
      lots = min0(nlot, iwav1s-i+1)
      do j=i,i+lots-1
        do l=1,jgrd1
          al(l,j-i+1) = a(j,k+ky-1,l)
        end do
        do l=jgrd1+1,jgrd2
          ll=jgrd2+2-l
          al(l,j-i+1) = a(j,k+ky-1,ll)
        end do
        do l=jgrd2+1,jump
          al(l,j-i+1) = 0.0
        end do
      end do
!      print *, 'al in ffacosy ',minval(al),maxval(al)
!
!     call -1 fft for analysis.
      call rfftmlt(al,work,trigy,jfax,1,jump,jgrd2,lots,-1)
!      print *, 'al in ffacosy ',minval(al),maxval(al)
!
      do j=i,i+lots-1
        do l=1,jwav1
          b(l,j,k) = al(2*l-1,j-i+1) 
        end do
      end do
    end do
!
  enddo

  return
end subroutine ffacosy1
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  ffasin
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  fast fourier grid transform of sin wave in x direction.
!
! program history log:
!
! usage:   call  ffasin (a, lot)
!   input argument list:
!     a   - grid values with dimension of igrd1
!     lot - second dimension of a
!
!   output argument list:
!     a     - coefficient values
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
!
! rfftmlt - fast fourier transform in x direction
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray c90.
!
!$$$
subroutine ffasinx1(a,jgrd1s,kx,lotsynk, lot)
  implicit none
  real(kind=dp),intent(inout) :: a(igrd1,lotsynk,jgrd1s)
  integer, intent(in) :: jgrd1s, kx, lotsynk, lot
  integer, parameter :: lotmin=64,lotmax=64,ncpu=1
!
  integer :: jump
  integer :: nlot,i,j,k,l,ll,lots
  real(kind=dp), allocatable :: work(:,:,:)
  real(kind=dp), allocatable :: al(:,:)
!
! grid to coefficient ( for regional use)
! sine function only in x direction
!
!  multiple fast fourier transform - analysis.  isign=-1
!
  jump=igrd2+3
  allocate( work(igrd2,lotmax,2), al(jump,lotmax) )
  nlot=lotmax
  do k=1,lot

    do i=1,jgrd1s,nlot
      lots = min0(nlot, jgrd1s-i+1)
      do j=i,i+lots-1
        do l=1,igrd1
          al(l,j-i+1) = a(l,k+kx-1,j)
        end do
        do l=igrd1+1,igrd2
          ll=igrd2+2-l
          al(l,j-i+1) = - a(ll,k+kx-1,j)
        end do
        do l=igrd2+1,jump
          al(l,j-i+1) = 0.0
        end do
      end do
!
!     call -1 fft for analysis.
      call rfftmlt(al,work,trigx,ifax,1,jump,igrd2,lots,-1)
!
      do j=i,i+lots-1
        do l=1,iwav1
          a(l,k+kx-1,j) = - al(2*l,j-i+1) 
        end do
        do l=iwav1+1,igrd1
          a(l,k+kx-1,j) = 0.0
        end do
      end do
    end do
!
  end do

  return
end subroutine ffasinx1
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  ffasin
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  fast fourier grid transform of sin wave in x direction.
!
! program history log:
!
! usage:   call  ffasin (a, lot)
!   input argument list:
!     a   - grid values with dimension of jgrd1
!     lot - second dimension of a
!
!   output argument list:
!     a     - coefficient values
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
!
! rfftmlt - fast fourier transform in x direction
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray c90.
!
!$$$
subroutine ffasiny1(a,b,ky,igrd1s,iwav1s,lotsynk, lot)
  implicit none
  real(kind=dp),intent(inout) :: a(igrd1s,lotsynk,jgrd1)
  real(kind=dp),intent(out) :: b(jwav1,iwav1s,lot)
  integer, intent(in) :: ky,igrd1s,iwav1s,lotsynk,lot
  integer, parameter :: lotmin=64,lotmax=64,ncpu=1
  integer :: nlot,i,j,k,l,ll,lots
  integer :: jump
  real(kind=dp),allocatable :: work(:,:,:)
  real(kind=dp),allocatable :: al(:,:)
!
! grid to coefficient ( for regional use)
! sine function only in x direction
!
!  multiple fast fourier transform - analysis.  isign=-1
!
  jump=jgrd2+3
  allocate( work(jgrd2,lotmax,2), al(jump,lotmax) )
  nlot=lotmax
  do k =1,lot

    do i=1,iwav1s,nlot
      lots = min0(nlot, iwav1s-i+1)
      do j=i,i+lots-1
        do l=1,jgrd1
          al(l,j-i+1) = a(j,k+ky-1,l)
        end do
        do l=jgrd1+1,jgrd2
          ll=jgrd2+2-l
          al(l,j-i+1) = - a(j,k+ky-1,ll)
        end do
        do l=jgrd2+1,jump
          al(l,j-i+1) = 0.0
        end do
      end do
!
!     call -1 fft for analysis.
      call rfftmlt(al,work,trigy,jfax,1,jump,jgrd2,lots,-1)
!
      do j=i,i+lots-1
        do l=1,jwav1
          b(l,j,k) = - al(2*l,j-i+1) 
        end do
      end do
    end do
!
  end do
  return
end subroutine ffasiny1

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  ffscos
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  fast fourier coefficient transform of cos wave in x direction.
!
! program history log:
!
! usage:   call  ffscos (a, lot)
!   input argument list:
!     a   - coefficient values with dimension of igrd1_
!     lot - second dimension of a
!
!   output argument list:
!     a     - grid values
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
!
! rfftmlt - fast fourier transform in x direction
!
!   remark: none
!
subroutine ffscosx1(a,jgrd1s,kx,lotsynk, lot)
  implicit none
  real(kind=dp),intent(inout) :: a(igrd1,lotsynk,jgrd1s)
  integer, intent(in) :: jgrd1s, kx, lotsynk, lot
  integer, parameter :: lotmin=64,lotmax=64,ncpu=1
!
  integer :: jump
  integer :: nlot,i,j,k,l,ll,lots
  real(kind=dp), allocatable :: work(:,:,:)
  real(kind=dp), allocatable :: al(:,:)
!
! coefficient to grid ( for regional use )
! cosine function only in x direction.
!
!  multiple fast fourier transform - synthesis.  isign=1
!
  jump=igrd2+3
  allocate( work(igrd2,lotmax,2), al(jump,lotmax) )
  nlot=lotmax
  do k=1,lot

    do i=1,jgrd1s,nlot
      lots = min0(nlot, jgrd1s-i+1)
      do j=i,i+lots-1
        do l=1,jump
          al(l,j-i+1) = 0.0
        end do
        do l=1,iwav1
          al(2*l-1,j-i+1) = a(l,k+kx-1,j) 
        end do
      end do
!
!     call 1 fft for systhesis.
      call rfftmlt(al,work,trigx,ifax,1,jump,igrd2,lots,1)
!
      do j=i,i+lots-1
        do l=1,igrd1
          a(l,kx+k-1,j) = al(l,j-i+1)
        end do
      end do
    end do

  enddo
!
  return
end
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  ffscos
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  fast fourier coefficient transform of cos wave in x direction.
!
! program history log:
!
! usage:   call  ffscos (a, lot)
!   input argument list:
!     a   - coefficient values with dimension of igrd1_
!     lot - second dimension of a
!
!   output argument list:
!     a     - grid values
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
!
! rfftmlt - fast fourier transform in x direction
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray c90.
!
subroutine ffscosy1(a, b,ky,iwav1s,igrd1s,lotsynk,lot)
  implicit none
  real(kind=dp),intent(inout) :: a(jwav1,iwav1s,lot)
  real(kind=dp),intent(out) :: b(igrd1s,lotsynk,jgrd1)
  integer, intent(in) :: ky,iwav1s,igrd1s,lotsynk,lot
  integer, parameter :: lotmin=64,lotmax=64,ncpu=1
  integer :: nlot,i,j,k,l,ll,lots
  integer :: jump
  real(kind=dp),allocatable :: work(:,:,:)
  real(kind=dp),allocatable :: al(:,:)
!
! coefficient to grid ( for regional use )
! cosine function only in x direction.
!
!  multiple fast fourier transform - synthesis.  isign=1
!
  jump=jgrd2+3
  allocate( work(jgrd2,lotmax,2), al(jump,lotmax) )
  nlot=lotmax
  do k=1,lot

    do i=1,iwav1s,nlot
      lots = min0(nlot, iwav1s-i+1)
      do j=i,i+lots-1
        do l=1,jump
          al(l,j-i+1) = 0.0
        end do
        do l=1,jwav1
          al(2*l-1,j-i+1) = a(l,j,k) 
        end do
      end do
!
!     call 1 fft for systhesis.
      call rfftmlt(al,work,trigy,jfax,1,jump,jgrd2,lots,1)
!
      do j=i,i+lots-1
        do l=1,jgrd1
          b(j,ky+k-1,l) = al(l,j-i+1)
        end do
      end do
    end do
  enddo
!
  return
end
!
!fpp$ noconcur r
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  ffssin
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  fast fourier coefficient transform of sin wave in x direction.
!
! program history log:
!
! usage:   call  ffssin (a, lot)
!   input argument list:
!     a   - coefficient values with dimension of igrd1
!     lot - second dimension of a
!
!   output argument list:
!     a     - grid values
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
!
! rfftmlt - fast fourier transform in x direction
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray c90.
!
!$$$
subroutine ffssinx1(a,jgrd1s,kx,lotsynk,lot)
  implicit none
  real(kind=dp),intent(inout) :: a(igrd1,lotsynk,jgrd1s)
  integer, intent(in) :: jgrd1s, kx, lotsynk, lot
  integer, parameter :: lotmin=64,lotmax=64,ncpu=1
!
  integer :: jump
  integer :: nlot,i,j,k,l,ll,lots
  real(kind=dp), allocatable :: work(:,:,:)
  real(kind=dp), allocatable :: al(:,:)
!
! coeficient to grid ( for regional use )
! sine function only in x direction
!
!  multiple fast fourier transform - synthesis.  isign=1
!
  jump=igrd2+3
  allocate( work(igrd2,lotmax,2), al(jump,lotmax) )
  nlot=lotmax
  do k=1,lot

    do i=1,jgrd1s,nlot
      lots = min0(nlot, jgrd1s-i+1)
      do j=i,i+lots-1
        do l=1,jump
          al(l,j-i+1) = 0.0
        end do
        do l=1,iwav1
          al(2*l,j-i+1) = - a(l,k+kx-1,j) 
        end do
      end do
!
!     call 1 fft for systhesis.
      call rfftmlt(al,work,trigx,ifax,1,jump,igrd2,lots,1)
!
      do j=i,i+lots-1
        do l=1,igrd1
          a(l,k+kx-1,j) = al(l,j-i+1)
        end do
      end do
    end do
!
  end do

  return
end
!
!fpp$ noconcur r
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  ffssin
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06
!
! abstract:  fast fourier coefficient transform of sin wave in x direction.
!
! program history log:
!
! usage:   call  ffssin (a, lot)
!   input argument list:
!     a   - coefficient values with dimension of igrd1
!     lot - second dimension of a
!
!   output argument list:
!     a     - grid values
!
!   input files: none
!
!   output files: none
!
!   subprograms called:
!
! rfftmlt - fast fourier transform in x direction
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray c90.
!
!$$$
subroutine ffssiny1 (a,b,ky,iwav1s,igrd1s,lotsynk,lot)
  implicit none
  real(kind=dp),intent(inout) :: a(jwav1,iwav1s,lot)
  real(kind=dp),intent(out) :: b(igrd1s,lotsynk,jgrd1)
  integer, intent(in) :: ky,iwav1s,igrd1s,lotsynk,lot
  integer, parameter :: lotmin=64,lotmax=64,ncpu=1
  integer :: nlot,i,j,k,l,ll,lots
  integer :: jump
  real(kind=dp),allocatable :: work(:,:,:)
  real(kind=dp),allocatable :: al(:,:)
!
! coeficient to grid ( for regional use )
! sine function only in x direction
!
!  multiple fast fourier transform - synthesis.  isign=1
!
  jump=jgrd2+3
  allocate( work(jgrd2,lotmax,2), al(jump,lotmax) )
  nlot=lotmax
  do k=1,lot

    do i=1,iwav1s,nlot
      lots = min0(nlot, iwav1s-i+1)
      do j=i,i+lots-1
        do l=1,jump
          al(l,j-i+1) = 0.0
        end do
        do l=1,jwav1
          al(2*l,j-i+1) = - a(l,j,k) 
        end do
      end do
!
!     call 1 fft for systhesis.
      call rfftmlt(al,work,trigy,jfax,1,jump,jgrd2,lots,1)
!
      do j=i,i+lots-1
        do l=1,jgrd1
          b(j,k+ky-1,l) = al(l,j-i+1)
        end do
      end do
    end do
!
  end do

  return
end
!
end module spectral_module
