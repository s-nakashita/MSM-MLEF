module norm_module
!
! module : norm_module   calculate various norm
!
! history:
! 22-09-13 create
! 
  use kind_module
  use phconst_module
  use read_module
  use write_module

  public :: calc_te, calc_tegrd
  contains
!======================================================================
! calculate moist total energy
!======================================================================
subroutine calc_te(u,v,t,q,ps,epsq,clat,si,nlon,nlat,te)
  implicit none
  integer, intent(in) :: nlon, nlat ! boundaries
  real(kind=dp), intent(in) :: u(:,:,:),v(:,:,:),t(:,:,:),q(:,:,:)
  real(kind=dp), intent(in) :: ps(:,:)
  real(kind=dp), intent(in) :: epsq ! weight for moist term
  real(kind=dp), intent(in) :: clat(:),si(:)
  real(kind=dp), intent(out):: te(4)
  ! for energy calculation
  integer, parameter :: kmax=21
  real(kind=dp), parameter :: tr=300.0d0, pr=800.0d2![Pa]
  real(kind=dp) :: area,coef
  integer :: igrd1, jgrd1
  integer :: n,i,j,k

  ! calculate energy
  te=0.0d0
  area=0.0d0
  do k=1,kmax
    do j=1,nlat
      coef=(si(k)-si(k+1))*cos(clat(j)*deg2rad)
      do i=1,nlon
        !KE
        te(1)=te(1)+(u(i,j,k)*u(i,j,k)+v(i,j,k)*v(i,j,k))*coef
        !PE(T)
        te(2)=te(2)+cp/tr*t(i,j,k)*t(i,j,k)*coef
        !LE
        te(3)=te(3)+epsq*lh**2/cp/tr*q(i,j,k)*q(i,j,k)*coef
      end do
    end do
  end do
  do j=1,nlat
    coef=cos(clat(j)*deg2rad)
    do i=1,nlon
      !PE(Ps)
      te(4)=te(4)+rd*tr*ps(i,j)*ps(i,j)/pr/pr*coef
      area=area+coef
    end do
  end do
  do i=1,4
    te(i)=te(i)*0.5d0/area
  end do
  return
end subroutine calc_te

!======================================================================
! calculate moist total energy for each grids
!======================================================================
subroutine calc_tegrd(u,v,t,q,ps,epsq,clat,si,nlon,nlat,te)
  implicit none
  integer, intent(in) :: nlon, nlat ! boundaries
  real(kind=dp), intent(in) :: u(:,:,:),v(:,:,:),t(:,:,:),q(:,:,:)
  real(kind=dp), intent(in) :: ps(:,:)
  real(kind=dp), intent(in) :: epsq ! weight for moist term
  real(kind=sp), intent(in) :: clat(:),si(:)
  real(kind=dp), intent(out):: te(:,:,:,:)
  ! for energy calculation
  integer, parameter :: kmax=21
  real(kind=dp), parameter :: tr=300.0d0, pr=800.0d2![Pa]
  real(kind=dp) :: area,coef
  integer :: igrd1, jgrd1
  integer :: n,i,j,k

  ! calculate energy
  te=0.0d0
  do k=1,kmax
    do j=1,nlat
      do i=1,nlon
        !KE
        te(i,j,k,1)=0.5d0*(u(i,j,k)*u(i,j,k)+v(i,j,k)*v(i,j,k))
        !PE(T)
        te(i,j,k,2)=0.5d0*cp/tr*t(i,j,k)*t(i,j,k)
        !LE
        te(i,j,k,3)=0.5d0*epsq*lh**2/cp/tr*q(i,j,k)*q(i,j,k)
      end do
    end do
  end do
  do j=1,nlat
    do i=1,nlon
      !PE(Ps)
      te(i,j,1,4)=0.5d0*rd*tr*ps(i,j)*ps(i,j)/pr/pr
    end do
  end do
  return
end subroutine calc_tegrd

end module norm_module