!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!---------------- calculate ice deposition at t<t_ice ------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
      real function deposit (pp, rhgrd, tdum, wvdum)
      use machine , only : kind_phys
      use funcphys , only : fpvsi
      use physcons, cp => con_cp, rd => con_rd, rv => con_rv            &
     &,             hvap => con_hvap, hfus => con_hfus                  &
     &,             eps => con_eps,  epsm1 => con_epsm1
!
!--- also uses the asai (1965) algorithm, but uses a different target
!      vapor pressure for the adjustment
!
      integer, parameter :: high_pres=kind_phys
!     integer, parameter :: high_pres=selected_real_kind(15)
      real (kind=high_pres), parameter :: rhlimit=.001,                 &
     & rhlimit1=-rhlimit
!     real, parameter :: cp=1004.6, rd=287.04, rv=461.5, xls=2.834e6,   &
!    & eps=rd/rv, rcp=1./cp, rcprv=rcp/rv, xls1=xls*rcp,                &
!    & xls2=xls*xls*rcprv, epsm1=rd/rv-1.
      real, parameter :: rcp=1./cp, rcprv=rcp/rv, xls=hvap+hfus         &
     &,                  xls1=xls*rcp, xls2=xls*xls*rcprv
      real (kind=high_pres) :: dep, ssat
!
!-----------------------------------------------------------------------
!
      esi=min(pp, fpvsi(tdum))                  ! saturation vapor press w/r/t ice
!     ws=rhgrd*eps*esi/(pp-esi)                 ! saturation mixing ratio
      ws=rhgrd*eps*esi/(pp+epsm1*esi)           ! saturation mixing ratio
      dwv=wvdum-ws                              ! deficit grid-scale water vapor mixing ratio
      ssat=dwv/ws                               ! supersaturation ratio
      deposit=0.
      do while (ssat.gt.rhlimit .or. ssat.lt.rhlimit1)
   !
   !--- note that xlvs2=ls*lv/(cp*rv)=lv*ws/(rv*t*t)*(ls/cp*dep1),
   !     where ws is the saturation mixing ratio following clausius-
   !     clapeyron (see asai,1965; young,1993,p.405)
   !
        dep=dwv/(1.+xls2*ws/(tdum*tdum))        ! asai (1965, j. japan)
        tdum=tdum+xls1*dep                      ! updated temperature
        wvdum=wvdum-dep                         ! updated ice mixing ratio
        deposit=deposit+dep                     ! total ice deposition
        esi=min(pp, fpvsi(tdum))                ! updated saturation vapor press w/r/t ice
!       ws=rhgrd*eps*esi/(pp-esi)               ! updated saturation mixing ratio w/r/t ice
        ws=rhgrd*eps*esi/(pp+epsm1*esi)         ! updated saturation mixing ratio w/r/t ice
        dwv=wvdum-ws                            ! deficit grid-scale water vapor mixing ratio
        ssat=dwv/ws                             ! grid-scale supersaturation ratio
      enddo
      return
      end
