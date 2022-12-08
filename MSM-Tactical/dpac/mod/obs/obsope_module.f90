module obsope_module
!
! observation operators
!
! history:
! 22-12-05 create
!
  use kind_module
  use rsmcom_module
  use func_module, only : calc_td, calc_wd
  use obs_module
  implicit none
  private

  public :: trans_xtoy, phys2ijk
contains
!
! main routine for observation operator
!
  subroutine obsope_serial(obsin,v3dg,v2dg,obsout)
    implicit none
    type(obstype), intent(in) :: obsin
    real(kind=dp), intent(in) :: v3dg(nlon,nlat,nlev,nv3d)
    real(kind=dp), intent(in) :: v2dg(nlon,nlat,nlev,nv2d)
    type(obstype2), intent(out):: obsout
    integer :: nobsin,nobsout
    
    return
  end subroutine obsope_serial
!
!
! model variables => observation
!
  subroutine trans_xtoy(elm,ri,rj,rk,v3d,v2d,p_full,yobs)
    implicit none
    real(kind=dp), intent(in) :: elm
    real(kind=dp), intent(in) :: ri,rj,rk
    real(kind=dp), intent(in) :: v3d(nlon,nlat,nlev,nv3d)
    real(kind=dp), intent(in) :: v2d(nlon,nlat,nv2d)
    real(kind=dp), intent(in) :: p_full(nlon,nlat,nlev)
    real(kind=dp), intent(out):: yobs
    
    real(kind=dp) :: t,q,p
    real(kind=dp) :: u,v
    
    integer :: i,j,k
    integer :: is,ie,js,je,ks,ke

    ie = ceiling( ri )
    is = ie - 1
    je = ceiling( rj )
    js = je - 1
    ke = ceiling( rk )
    ks = ke - 1

    select case( nint(elm) )
    case(id_t_obs) !Temperature
      call itpl_3d(v3d(:,:,:,iv3d_t),ri,rj,rk,yobs)
    case(id_td_obs) !Dewpoint Temperature
      call itpl_3d(v3d(:,:,:,iv3d_t),ri,rj,rk,t)
      call itpl_3d(v3d(:,:,:,iv3d_q),ri,rj,rk,q)
      call itpl_3d(p_full(:,:,:),ri,rj,rk,p)
      call calc_td(t,q,p,yobs) 
    case(id_ws_obs) !Wind Speed
      call itpl_3d(v3d(:,:,:,iv3d_u),ri,rj,rk,u)
      call itpl_3d(v3d(:,:,:,iv3d_v),ri,rj,rk,v)
      yobs = sqrt(u**2 + v**2)
    case(id_wd_obs) !Wind Direction
      call itpl_3d(v3d(:,:,:,iv3d_u),ri,rj,rk,u)
      call itpl_3d(v3d(:,:,:,iv3d_v),ri,rj,rk,v)
      call calc_wd(u,v,yobs)
    end select
    return

  end subroutine trans_xtoy
!
! coordinate conversion
!
  subroutine phys2ijk(p_full,elm,rlon1,rlat1,rlev1,ri,rj,rk,qc)
    implicit none
    real(kind=dp),intent(in) :: p_full(nlon,nlat,nlev)
    real(kind=dp),intent(in) :: elm
    real(kind=dp),intent(in) :: rlon1,rlat1
    real(kind=dp),intent(in) :: rlev1 !pressure level
    real(kind=dp),intent(out):: ri,rj,rk
    integer,      intent(out):: qc
    real(kind=dp) :: ai,aj,ak
    real(kind=dp) :: lnps(nlon,nlat)
    real(kind=dp) :: plev(nlev)
    real(kind=dp) :: ptmp
    integer :: i,j,k

    qc=iqc_good
    ! rlon1 -> ri
    if(rlon1.lt.rlon(1)) then
      ri=0.0
    end if
    do i=1,nlon
      if(rlon1.ge.rlon(i)) exit
    end do
    if(i.ge.nlon) then
      ri=real(nlon+1,kind=dp)
    else
      ai=(rlon1-rlon(i))/(rlon(i+1)-rlon(i))
      ri=real(i,kind=dp)+ai 
    end if
    ! rlat1 -> rj
    if(rlat1.lt.rlat(1)) then
      rj=0.0
    end if
    do j=1,nlat
      if(rlat1.ge.rlat(j)) exit
    end do
    if(j.ge.nlat) then
      rj=real(nlat+1,kind=dp)
    else
      aj=(rlat1-rlat(j))/(rlat(j+1)-rlat(j))
      rj=real(j,kind=dp)+aj
    end if
    ! check whether observation is within horizontal domain or not
    if(ri.lt.1.0.or.ri.gt.nlon.or.rj.lt.1.0.or.rj.gt.nlat) then
      write(6,'(a)') 'warning: observation is outside of the horizontal domain'
      qc=iqc_out_h
      return
    end if
    ! rlev1 -> rk
    if(nint(elm).gt.9999) then !surface observation
      rk=0.0
    else
      ! horizontal interpolation
      do k=1,nlev
        lnps(i:i+1,j:j+1)=log(p_full(i:i+1,j:j+1,k))
        call itpl_2d(lnps,ri,rj,plev(k))
      end do
      ! find rk
      rk=log(rlev1)
      if(rk.gt.plev(1)) then
        call itpl_2d(p_full(:,:,1),ri,rj,ptmp)
        write(6,'(a,f8.1,a,f8.1,a,i5)') &
        & 'warning: observation is too low, pbtm=',ptmp,', lev=',rlev1,' elem=',nint(elm)
        qc=iqc_out_vlo
        return
      end if
      if(rk.lt.plev(nlev)) then
        call itpl_2d(p_full(:,:,nlev),ri,rj,ptmp)
        write(6,'(a,f8.1,a,f8.1,a,i5)') &
        & 'warning: observation is too high, ptop=',ptmp,', lev=',rlev1,' elem=',nint(elm)
        qc=iqc_out_vhi
        return
      end if
      do k=1,nlev
        if(plev(k).gt.rk) exit
      end do
      ak=(rk-plev(k-1))/(plev(k)-plev(k-1))
      rk=real(k-1,kind=dp)+ak
    end if
    return
  end subroutine phys2ijk
!
! 2-dimensional interpolation
!
  subroutine itpl_2d(v2d,ri,rj,vout)
    implicit none
    real(kind=dp), intent(in) :: v2d(nlon,nlat)
    real(kind=dp), intent(in) :: ri, rj
    real(kind=dp), intent(out):: vout
    real(kind=dp) :: ai, aj
    integer :: i,j

    i = floor(ri)
    ai = ri - real(i,kind=dp)
    j = floor(rj)
    aj = rj - real(j,kind=dp)

    vout = v2d(i  ,j  ) * (1.0 - ai) * (1.0 - aj) &
       & + v2d(i+1,j  ) *        ai  * (1.0 - aj) &
       & + v2d(i  ,j+1) * (1.0 - ai) *        aj  &
       & + v2d(i+1,j+1) *        ai  *        aj
    return
  end subroutine itpl_2d
!
! 3-dimensional interpolation
!
  subroutine itpl_3d(v3d,ri,rj,rk,vout)
    implicit none
    real(kind=dp), intent(in) :: v3d(nlon,nlat,nlev)
    real(kind=dp), intent(in) :: ri,rj,rk
    real(kind=dp), intent(out):: vout
    real(kind=dp) :: ai,aj,ak
    integer :: i,j,k

    i = floor(ri)
    ai = ri - real(i,kind=dp)
    j = floor(rj)
    aj = rj - real(j,kind=dp)
    k = floor(rk)
    ak = rk - real(k,kind=dp)
    
    vout = v3d(i  ,j  ,k  ) * (1.0 - ai) * (1.0 - aj) * (1.0 - ak) &
       & + v3d(i+1,j  ,k  ) *        ai  * (1.0 - aj) * (1.0 - ak) &
       & + v3d(i  ,j+1,k  ) * (1.0 - ai) *        aj  * (1.0 - ak) &
       & + v3d(i+1,j+1,k  ) *        ai  *        aj  * (1.0 - ak) &
       & + v3d(i  ,j  ,k+1) * (1.0 - ai) * (1.0 - aj) *        ak  &
       & + v3d(i+1,j  ,k+1) *        ai  * (1.0 - aj) *        ak  &
       & + v3d(i  ,j+1,k+1) * (1.0 - ai) *        aj  *        ak  &
       & + v3d(i+1,j+1,k+1) *        ai  *        aj  *        ak

    return
  end subroutine itpl_3d
end module obsope_module
