module obsope_module
!
! observation operators
!
! history:
! 22-12-05 create
!
  use kind_module
  use nml_module
  use rsmcom_module
  use func_module, only : calc_pfull, calc_td, calc_wd, calc_rh
  use obs_module
  implicit none
  private

  public :: obsope_serial, obsope_update, monit_dep, monit_print
contains
!
! main routine for observation operator
!
  subroutine obsope_serial(obsin,obsout)
    implicit none
    type(obstype), intent(in) :: obsin(obsin_num)
    type(obstype2), intent(out):: obsout
    integer :: nobsin,nobsout
    integer :: iof
    character(len=filelenmax) :: guesf, outf
    real(kind=dp) :: v3dg(nlon,nlat,nlev,nv3d)
    real(kind=dp) :: v2dg(nlon,nlat,nv2d)
    real(kind=dp) :: p_full(nlon,nlat,nlev)
    real(kind=dp) :: ri,rj,rk
    integer :: im, n, nn
!!! for single point observation
    real(kind=dp) :: lonb,latb
    real(kind=dp) :: hxf,dep

    nobsin=0
    do iof=1,obsin_num
      nobsin=nobsin+obsin(iof)%nobs
    end do
    if(nobsin.le.0) then
      write(6,'(a)') 'no observation to be assimilated'
      return
    end if
    obsout%nobs = nobsin
    call obsout_allocate(obsout,member)
   
    do im=0,member
      nobsout=0
      nn=0
      call file_member_replace(im,fguess_basename,guesf)
      call read_restart(guesf,v3dg,v2dg)
!      call calc_pfull(nlon,nlat,nlev,sig,v2dg(:,:,iv2d_ps),p_full)
      if(im.eq.0) then !all members assumed to have the same pressure levels
        p_full = v3dg(:,:,:,iv3d_pp)
      end if
      lonb=undef
      latb=undef
      do iof=1,obsin_num
        do n=1,obsin(iof)%nobs
          if(nobsmax.gt.0.and.nn.ge.nobsmax) exit
          !!! single point observation
          if(single_obs) then
            if(lonb.ne.undef.and.latb.ne.undef) then
              if(lonb.ne.obsin(iof)%lon(n).or.latb.ne.obsin(iof)%lat(n)) exit
            end if
          end if
          nobsout=nobsout+1
          call phys2ijk(p_full,obsin(iof)%elem(n),&
             &  obsin(iof)%lon(n),obsin(iof)%lat(n),obsin(iof)%lev(n), &
             &  ri,rj,rk,obsout%qc(nobsout))
          if(obsout%qc(nobsout).eq.iqc_good) then
            if(.not.luseobs(uid_obs(obsin(iof)%elem(n)))) then
              obsout%qc(nobsout)=iqc_otype
            else
            nn=nn+1
            if(im.eq.0) then
              call trans_xtoy(obsin(iof)%elem(n),ri,rj,rk,&
               &  v3dg,v2dg,p_full,obsout%hxf(nobsout))
!!! debug
!              dep = obsin(iof)%dat(n) - obsout%hxf(nobsout)
!              print *, obsin(iof)%elem(n),obsin(iof)%lon(n),obsin(iof)%lat(n),&
!               &  obsin(iof)%lev(n)
!              print *,obsin(iof)%dat(n),obsout%hxf(nobsout), dep
!!! debug
            else
              call trans_xtoy(obsin(iof)%elem(n),ri,rj,rk,&
               &  v3dg,v2dg,p_full,obsout%hxe(im,nobsout))
!!! debug
!              dep = obsin(iof)%dat(n) - obsout%hxe(im,nobsout)
!              print *, obsin(iof)%elem(n),obsin(iof)%lon(n),obsin(iof)%lat(n),&
!               &  obsin(iof)%lev(n)
!              print *,obsin(iof)%dat(n),obsout%hxe(im,nobsout), dep
!!! debug
            end if
            end if !luseobs
            if(single_obs) then
              if(lonb.eq.undef.and.latb.eq.undef) then
                lonb=obsin(iof)%lon(n)
                latb=obsin(iof)%lat(n)
                if(latb.lt.28.0.or.latb.gt.32.0.or.&
                   lonb.lt.125.0.or.lonb.gt.131.0) then
                  lonb=undef; latb=undef
                  obsout%qc(nobsout)=iqc_out_h
                end if
              end if
            end if
          end if !iqc_good
          if(single_obs.and.&
            obsout%qc(nobsout)/=iqc_good) then
            nobsout=nobsout-1
            cycle
          end if
          obsout%elem(nobsout) = obsin(iof)%elem(n)
          obsout%lon(nobsout)  = obsin(iof)%lon(n)
          obsout%lat(nobsout)  = obsin(iof)%lat(n)
          obsout%lev(nobsout)  = obsin(iof)%lev(n)
          obsout%dat(nobsout)  = obsin(iof)%dat(n)
          obsout%dmin(nobsout) = obsin(iof)%dmin(n)
          obsout%err(nobsout)  = obserr(uid_obs(obsin(iof)%elem(n)))
        end do
      end do
      obsout%nobs = nobsout
      if(single_obs) then
        print '(10a10)','number','elem','lon','lat','lev','dat','err','dmin','hxf','qc'
        do n=1,obsout%nobs
          if(im.eq.0) then
            hxf=obsout%hxf(n)
          else
            hxf=obsout%hxe(im,n)
          end if
          print '(i10,a10,2f10.2,f10.1,4es10.2,i10)', n,obelmlist(uid_obs(obsout%elem(n))),&
           &  obsout%lon(n),obsout%lat(n),obsout%lev(n),obsout%dat(n),&
           &  obsout%err(n),obsout%dmin(n), &
           &  hxf,obsout%qc(n)
        end do
      end if
      if(obs_out) then
        call file_member_replace(im,obsout_basename,outf)
        call write_obsout(outf,obsout,im)
      end if
    end do
    return
  end subroutine obsope_serial
!
! observation operator applied to updated model variables in DA
!
  subroutine obsope_update(obs,mem,v3dg,v2dg)
    implicit none
    type(obstype2), intent(inout):: obs
    integer,       intent(in) :: mem
    real(kind=dp), intent(in) :: v3dg(nlon,nlat,nlev,nv3d)
    real(kind=dp), intent(in) :: v2dg(nlon,nlat,nv2d)
    integer :: nobsin,nobsout
    real(kind=dp) :: p_full(nlon,nlat,nlev)
    real(kind=dp) :: ri,rj,rk
    integer :: im, n, nn
!!! debug
!    integer :: nobsmax
!    real(kind=dp) :: dep
!    
!    nobsmax=100 !debug
!!! debug

    if(obs%nobs==0) then
      write(6,'(a)') 'no observation to be assimilated'
      return
    end if
   
    nobsout=0
    nn=0
!    call calc_pfull(nlon,nlat,nlev,sig,v2dg(:,:,iv2d_ps),p_full)
    p_full = v3dg(:,:,:,iv3d_pp)
    do n=1,obs%nobs
      nobsout=nobsout+1
      call phys2ijk(p_full,obs%elem(n),&
         &  obs%lon(n),obs%lat(n),obs%lev(n), &
         &  ri,rj,rk,obs%qc(n))
      if(obs%qc(n).eq.iqc_good) then
        nn=nn+1
        if(mem.eq.0) then
          call trans_xtoy(obs%elem(n),ri,rj,rk,&
           &  v3dg,v2dg,p_full,obs%hxf(n))
!!! debug
!          dep = obs%dat(n) - obs%hxf(n)
!          print *, obs%elem(n),obs%lon(n),obs%lat(n),&
!           &  obs%lev(n)
!          print *,obs%dat(n),obs%hxf(n), dep
!!! debug
        else
          call trans_xtoy(obs%elem(n),ri,rj,rk,&
           &  v3dg,v2dg,p_full,obs%hxe(mem,n))
!!! debug
!          dep = obs%dat(n) - obs%hxe(im,n)
!          print *, obs%elem(n),obs%lon(n),obs%lat(n),&
!           &  obs%lev(n)
!              print *,obs%dat(n),obs%hxe(im,n), dep
!!! debug
        end if
      end if
    end do
!    obs%nobs = nobsout
    return
  end subroutine obsope_update
!
!
! model variables => observation
!
  subroutine trans_xtoy(elm,ri,rj,rk,v3d,v2d,p_full,yobs)
    implicit none
    integer, intent(in) :: elm
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

    select case( elm )
    case(id_u_obs) !U-wind
      call itpl_3d(v3d(:,:,:,iv3d_u),ri,rj,rk,yobs)
    case(id_v_obs) !V-wind
      call itpl_3d(v3d(:,:,:,iv3d_v),ri,rj,rk,yobs)
    case(id_t_obs) !Temperature
      call itpl_3d(v3d(:,:,:,iv3d_t),ri,rj,rk,yobs)
    case(id_q_obs) !Specific humidity
      call itpl_3d(v3d(:,:,:,iv3d_q),ri,rj,rk,yobs)
    case(id_td_obs) !Dewpoint Temperature
      call itpl_3d(v3d(:,:,:,iv3d_t),ri,rj,rk,t)
      call itpl_3d(v3d(:,:,:,iv3d_q),ri,rj,rk,q)
      call itpl_3d(p_full(:,:,:),ri,rj,rk,p)
      call calc_td(t,q,p,yobs) 
    case(id_rh_obs) !Relative humidity
      call itpl_3d(v3d(:,:,:,iv3d_t),ri,rj,rk,t)
      call itpl_3d(v3d(:,:,:,iv3d_q),ri,rj,rk,q)
      call itpl_3d(p_full(:,:,:),ri,rj,rk,p)
      call calc_rh(t,q,p,yobs) 
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
    integer,intent(in) :: elm
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
    else
      do i=1,nlon
        if(rlon1.lt.rlon(i)) exit
      end do
      if(i.ge.nlon) then
        ri=real(nlon+1,kind=dp)
      else
        ai=(rlon1-rlon(i-1))/(rlon(i)-rlon(i-1))
        ri=real(i-1,kind=dp)+ai 
      end if
    end if
    ! rlat1 -> rj
    if(rlat1.lt.rlat(1)) then
      rj=0.0
    else
      do j=1,nlat
        if(rlat1.lt.rlat(j)) exit
      end do
      if(j.ge.nlat) then
        rj=real(nlat+1,kind=dp)
      else
        aj=(rlat1-rlat(j-1))/(rlat(j)-rlat(j-1))
        rj=real(j-1,kind=dp)+aj
      end if
    end if
    ! check whether observation is within horizontal domain or not
    if(ri.lt.1.0.or.ri.gt.nlon.or.rj.lt.1.0.or.rj.gt.nlat) then
      write(0,'(a,4(a,f8.2))') &
        & 'warning: observation is outside of the horizontal domain ', &
        & 'lon=',rlon1,' lat=',rlat1,' ri=',ri,' rj=',rj
      qc=iqc_out_h
      return
    end if
    ! rlev1 -> rk
    if(elm.gt.9999) then !surface observation
      rk=0.0
    else
      ! horizontal interpolation
      i = ceiling(ri)
      j = ceiling(rj)
      do k=1,nlev
        lnps=0.0_dp
        lnps(i-1:i,j-1:j)=log(p_full(i-1:i,j-1:j,k))
        call itpl_2d(lnps,ri,rj,plev(k))
      end do
      !print *, 'plev ',plev(1),plev(nlev) !debug
      ! find rk
      rk=log(rlev1)
      if(rk.gt.plev(1)) then
!        call itpl_2d(p_full(:,:,1),ri,rj,ptmp)
        ptmp=exp(plev(1))
        write(0,'(a,f8.1,a,f8.1,a,i5)') &
        & 'warning: observation is too low, pbtm=',ptmp,', lev=',rlev1,' elem=',elm
        qc=iqc_out_vlo
        return
      end if
      if(rk.lt.plev(nlev)) then
!        call itpl_2d(p_full(:,:,nlev),ri,rj,ptmp)
        ptmp=exp(plev(nlev))
        write(0,'(a,f8.1,a,f8.1,a,i5)') &
        & 'warning: observation is too high, ptop=',ptmp,', lev=',rlev1,' elem=',elm
        qc=iqc_out_vhi
        return
      end if
      do k=1,nlev
        if(plev(k).lt.rk) exit
      end do
      ak=(plev(k-1)-rk)/(plev(k-1)-plev(k))
      rk=real(k-1,kind=dp)+ak
    end if
!! debug
!    write(6,'(3(a,f8.1))') 'ri=',ri,' rj=',rj,' rk=',rk
!! debug
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

    i = ceiling(ri)
    ai = ri - real(i-1,kind=dp)
    j = ceiling(rj)
    aj = rj - real(j-1,kind=dp)

    vout = v2d(i-1,j-1) * (1.0 - ai) * (1.0 - aj) &
       & + v2d(i  ,j-1) *        ai  * (1.0 - aj) &
       & + v2d(i-1,j  ) * (1.0 - ai) *        aj  &
       & + v2d(i  ,j  ) *        ai  *        aj
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

    i = ceiling(ri)
    ai = ri - real(i-1,kind=dp)
    j = ceiling(rj)
    aj = rj - real(j-1,kind=dp)
    k = ceiling(rk)
    ak = rk - real(k-1,kind=dp)
    
    vout = v3d(i-1,j-1,k-1) * (1.0 - ai) * (1.0 - aj) * (1.0 - ak) &
       & + v3d(i  ,j-1,k-1) *        ai  * (1.0 - aj) * (1.0 - ak) &
       & + v3d(i-1,j  ,k-1) * (1.0 - ai) *        aj  * (1.0 - ak) &
       & + v3d(i  ,j  ,k-1) *        ai  *        aj  * (1.0 - ak) &
       & + v3d(i-1,j-1,k  ) * (1.0 - ai) * (1.0 - aj) *        ak  &
       & + v3d(i  ,j-1,k  ) *        ai  * (1.0 - aj) *        ak  &
       & + v3d(i-1,j  ,k  ) * (1.0 - ai) *        aj  *        ak  &
       & + v3d(i  ,j  ,k  ) *        ai  *        aj  *        ak

    return
  end subroutine itpl_3d
!
! Monitor departure
!
  subroutine monit_dep(nobsall,elem,dep,qc,nobs,bias,rmse,nqc)
    use phconst_module, only: pi, rad2deg
    implicit none
    integer, intent(in) :: nobsall
    integer, intent(in) :: elem(nobsall)
    real(kind=dp), intent(in) :: dep(nobsall)
    integer, intent(in) :: qc(nobsall)
    integer, intent(out) :: nobs(nobstype)
    real(kind=dp), intent(out) :: bias(nobstype)
    real(kind=dp), intent(out) :: rmse(nobstype)
    integer, intent(out) :: nqc(nobstype,nqctype)
    real(kind=dp) :: dep1
    integer :: n,i

    nobs(:) = 0
    bias(:) = 0.0_dp
    rmse(:) = 0.0_dp
    nqc(:,:)= 0
    do n=1,nobsall
      i = uid_obs(elem(n))
      nobs(i)=nobs(i)+1
      select case(qc(n))  
      case(iqc_gross_err)
        nqc(i,2)=nqc(i,2)+1
      case(iqc_out_vhi)
        nqc(i,3)=nqc(i,3)+1
      case(iqc_out_vlo)
        nqc(i,4)=nqc(i,4)+1
      case(iqc_out_h)
        nqc(i,5)=nqc(i,5)+1
      case(iqc_otype)
        nqc(i,6)=nqc(i,6)+1
      case(iqc_good)
        nqc(i,1)=nqc(i,1)+1
        dep1 = dep(n)
        if(elem(n)==id_wd_obs) then !wind direction
          if(abs(dep1).gt.180.0d0) then
            dep1=dep1-sign(360.0d0,dep1)
          end if
        end if
        bias(i)=bias(i)+dep1
        rmse(i)=rmse(i)+dep1**2
      end select
    end do
    do i=1,nobstype
      if(nqc(i,1).eq.0) then
        bias(i)=undef
        rmse(i)=undef
      else
        bias(i)=bias(i)/real(nobs(i),kind=dp)
        rmse(i)=sqrt(rmse(i)/real(nobs(i),kind=dp))
      end if
    end do
    return
  end subroutine monit_dep
!
! print monitor
!
  subroutine monit_print(nobs,bias,rmse,nqc)
    implicit none
    integer, intent(in) :: nobs(nobstype)
    real(kind=dp), intent(in) :: bias(nobstype)
    real(kind=dp), intent(in) :: rmse(nobstype)
    integer, intent(in) :: nqc(nobstype,nqctype)

    character(len=12) :: var_show(nobstype)
    character(len=12) :: nobs_show(nobstype)
    character(len=12) :: bias_show(nobstype)
    character(len=12) :: rmse_show(nobstype)
    character(len=12) :: nqc_show(nobstype,nqctype)

    integer :: i,iqc,n
    character(len=4) :: nstr
    character(len=12) :: tmpstr(nobstype), tmpstr2(nobstype)

    n=0
    do i=1,nobstype
      n=n+1
      write(var_show(n),'(a12)') obelmlist(i)
      write(nobs_show(n),'(i12)') nobs(i)
      if(nqc(i,1).gt.0) then
        write(bias_show(n),'(es12.3)') bias(i)
        write(rmse_show(n),'(es12.3)') rmse(i)
      else
        write(bias_show(n),'(a12)') 'N/A'
        write(rmse_show(n),'(a12)') 'N/A'
      end if
      do iqc=1,nqctype
        write(nqc_show(n,iqc),'(i12)') nqc(n,iqc)
      end do
    end do
    write(nstr,'(i4)') n
    tmpstr(1:n) = '============'
    tmpstr2(1:n) = '------------'

    write(6,'(a,'//trim(nstr)//'a)') '======',tmpstr(1:n)
    write(6,'(6x,'//trim(nstr)//'a)') var_show(1:n)
    write(6,'(a,'//trim(nstr)//'a)') '------',tmpstr2(1:n)
    write(6,'(a,'//trim(nstr)//'a)') 'BIAS  ',bias_show(1:n)
    write(6,'(a,'//trim(nstr)//'a)') 'RMSE  ',rmse_show(1:n)
    write(6,'(a,'//trim(nstr)//'a)') 'NUMBER',nobs_show(1:n)
    do iqc=1,nqctype
    write(6,'(a,'//trim(nstr)//'a)') qctype(iqc),nqc_show(1:n,iqc)
    end do
    write(6,'(a,'//trim(nstr)//'a)') '======',tmpstr(1:n)
  end subroutine monit_print
end module obsope_module
