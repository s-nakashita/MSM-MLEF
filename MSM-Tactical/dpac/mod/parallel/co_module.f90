module co_module
!
! coarray procedures
!
  use kind_module
  implicit none
  public

  integer, save :: nimages
  integer, save :: myimage

contains
  subroutine initialize_co
    implicit none

    nimages = Num_Images()
    myimage = This_image()
    write(6,'(2(a,i6.6))') 'Hello from MYIMAGE ',myimage,'/',nimages

    return
  end subroutine initialize_co

end module co_module
