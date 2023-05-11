module random_module
  use kind_module
  use phconst_module, only: pi
  implicit none
  private
  integer, save :: seedsize
  integer, allocatable, save :: seed(:)

  public :: random_init, random_uniform, random_normal

  contains
subroutine random_init
  implicit none
  integer :: i

  call random_seed(size=seedsize)
  if(allocated(seed)) deallocate(seed)
  allocate(seed(seedsize))
  do i=1,seedsize
    call system_clock(count=seed(i))
  end do
  call random_seed(put=seed(:))
  return
end subroutine random_init
!
! uniform distribution
!
subroutine random_uniform(x)
  implicit none
  real(kind=dp), intent(out) :: x(:)

  call random_number(x)
  return
end subroutine random_uniform
!
! normal distribution by Box-Muller's method
!
subroutine random_normal(x)
  implicit none
  real(kind=dp), intent(out) :: x(:)
  integer :: n
  real(kind=dp), allocatable :: z1(:),z2(:)

  n = size(x)
  allocate( z1(n), z2(n) )
  call random_number(z1)
  call random_number(z2)
  z1 = sqrt(-2.0 * log(z1))
  z2 = 2.0 * pi * z2
  x = z1 * cos(z2)
  deallocate( z1,z2 )
  return
end subroutine random_normal

end module random_module
