module phconst_module
!
! physics constants
!
  use kind_module
  implicit none
  public
  real(kind=dp), parameter :: pi=acos(-1.0d0)
  real(kind=dp), parameter :: rad2deg=180.0d0/pi, deg2rad=pi/180.0d0
  real(kind=dp), parameter :: re=6.371d6     ! radius of Earth [m]
  real(kind=dp), parameter :: grav=9.80665d0 ! acceleration rat of the Earth [m/s^2]
end module phconst_module
