!
! get cloud cover from MSM outputs
!

subroutine get_cc(t,q,cwc,w,slmsk,cv,cvt,cvb,prsi,prsl,&
    & cldtot,cldcnv,cldsa)
    use kind_module
    use phconst_module
    use func_module, only: calc_qs, calc_rh
    use rsmcom_module, only: igrd1,jgrd1,nlev,rlon,rlat

    implicit none
    integer, parameter :: ncld=1 !ferrier scheme
    !! input
    real(kind=dp), intent(in) :: t(igrd1,jgrd1,nlev) ! temperature[K]
    real(kind=dp), intent(in) :: q(igrd1,jgrd1,nlev) ! specific humidity [kg/kg]
    real(kind=dp), intent(in) :: cwc(igrd1,jgrd1,nlev) ! cloud water [kg/kg]
    real(kind=dp), intent(in) :: w(igrd1,jgrd1,nlev+1) ! vertical velocity [m/s]
    real(kind=dp), intent(in) :: slmsk(igrd1,jgrd1) ! sea-land mask (1=land,0=sea)
    real(kind=dp), intent(in) :: cv(igrd1,jgrd1) ! convective cloud fraction
    real(kind=dp), intent(in) :: cvt(igrd1,jgrd1) ! convective cloud top [sigma layer]
    real(kind=dp), intent(in) :: cvb(igrd1,jgrd1) ! convective cloud bottom [sigma layer]
    real(kind=dp), intent(in) :: prsi(igrd1,jgrd1,nlev+1) ! half level pressure [kPa]
    real(kind=dp), intent(in) :: prsl(igrd1,jgrd1,nlev) ! full level pressure [kPa]

    !! output
    real(kind=dp), intent(out) :: cldtot(igrd1,jgrd1,nlev) ! stratiform cloud cover
    real(kind=dp), intent(out) :: cldcnv(igrd1,jgrd1,nlev) ! convective cloud cover
    real(kind=dp), intent(out) :: cldsa(igrd1,jgrd1,5) ! cloud cover for low, middle, top, total and bl

    real(kind=dp) :: tgrs(igrd1,nlev), qgrs(igrd1,nlev), clw(igrd1,nlev)
    real(kind=dp) :: pri2(igrd1,nlev+1), prl2(igrd1,nlev)
    real(kind=dp) :: vvel(igrd1,nlev) ! pressure velocity [cb/s]=[kPa/s]
    real(kind=dp) :: rh(igrd1,nlev) ! relative humidity
    real(kind=dp) :: qs(igrd1,nlev) ! saturation specific humidity
    real(kind=dp) :: cldt(igrd1,nlev), cldc(igrd1,nlev), cls(igrd1,5)
    integer, parameter :: nbin=100,nlon=2,nlat=4,mcld=4,nseal=2
    real(kind=dp) :: rhcl(nbin,nlon,nlat,mcld,nseal) !cl-rh relation
    integer :: istrat
    integer :: mbota(igrd1,3) ! bottom layer numbers of low, middle and high cloud
    integer :: mtopa(igrd1,3) ! top layer numbers of low, middle and high cloud
    real(kind=dp) :: xlat(igrd1), xlon(igrd1)
    real(kind=dp) :: ppa, wmean, tem
    integer :: me, ier, ilon, ilat, ilev, kk
    
    !! get cl-rh relation from table
    istrat = 1
    call crhtab(rhcl,ier,me)

    !! start latitude loop
    xlon(:) = rlon(:)
    do ilat=1,jgrd1
      xlat(:) = rlat(ilat)
      !! calc qs and rh
      do ilev=1,nlev
        do ilon=1,igrd1
          tgrs(ilon,ilev) = t(ilon,ilat,ilev)
          qgrs(ilon,ilev) = q(ilon,ilat,ilev)
          pri2(ilon,ilev) = prsi(ilon,ilev,ilev)
          prl2(ilon,ilev) = prsl(ilon,ilat,ilev)
          ppa = prl2(ilon,ilev)*1.0e3 !Pa
          call calc_qs(tgrs(ilon,ilev),ppa,qs(ilon,ilev))
          call calc_rh(tgrs(ilon,ilev),qgrs(ilon,ilev),ppa,rh(ilon,ilev))
        end do
        !print *, ilev, ' qs max=',maxval(qs(:,ilev)),' min=',minval(qs(:,ilev))
        !print *, ilev, ' rh max=',maxval(rh(:,ilev)),' min=',minval(rh(:,ilev))
      end do
      do ilon=1,nlev
        pri2(ilon,nlev+1) = prsi(ilon,ilat,nlev+1)
      end do

      !! calc vvel
      do ilev=1,nlev
        do ilon=1,igrd1
          wmean = -0.5*(w(ilon,ilat,ilev)+w(ilon,ilat,ilev+1))
          vvel(ilon,ilev) = wmean*prl2(ilon,ilev)*grav/(rd*tgrs(ilon,ilev))
        end do
        !print *, ilev, ' vvel max=',maxval(vvel(:,ilev)),' min=',minval(vvel(:,ilev))
      end do

      !! calc clw
      do ilev=1,nlev
        do ilon=1,igrd1
          clw(ilon,ilev) = cwc(ilon,ilat,ilev)
        end do
      end do

      cldt=0.0d0
      cldc=0.0d0
      cls=0.0d0
      call getclds(igrd1,igrd1,nlev,tgrs,qgrs,vvel,rh,qs,&
      & slmsk(:,ilat), xlon, xlat, cv(:,ilat), cvt(:,ilat), cvb(:,ilat),&
      & rhcl, istrat, pri2, prl2, cldt, cldc, cls, mbota, mtopa, &
      & clw, ncld)

      do ilev=1,nlev
        do ilon=1,igrd1
          cldtot(ilon,ilat,ilev) = cldt(ilon,ilev)
          cldcnv(ilon,ilat,ilev) = cldc(ilon,ilev)
        end do
      end do
      do ilev=1,5
        do ilon=1,igrd1
          cldsa(ilon,ilat,ilev) = cls(ilon,ilev)
        end do
      end do
    ! end latitude loop
    end do
    return
end subroutine get_cc