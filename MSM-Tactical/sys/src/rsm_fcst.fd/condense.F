!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!--------- produces accurate calculation of cloud condensation ---------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
      real function condense (pp, qw, rhgrd, tk, wv)
!
!---------------------------------------------------------------------------------
!------   the asai (1965) algorithm takes into consideration the release of ------
!------   latent heat in increasing the temperature & in increasing the     ------
!------   saturation mixing ratio (following the clausius-clapeyron eqn.).  ------
!---------------------------------------------------------------------------------
      use machine , only : kind_phys
      use funcphys , only : fpvsl
      use physcons, cp => con_cp, rd => con_rd, rv => con_rv            &
     &,             eps => con_eps,  epsm1 => con_epsm1
!
      integer, parameter :: high_pres=kind_phys
!     integer, parameter :: high_pres=selected_real_kind(15)
      real (kind=high_pres), parameter :: epsq=1.e-12,                  &
     & rhlimit=.001, rhlimit1=-rhlimit
!     real, parameter :: cp=1004.6, rd=287.04, rv=461.5, eps=rd/rv,     &
!    & rcp=1./cp, rcprv=rcp/rv, epsm1=rd/rv-1.
      real, parameter :: rcp=1./cp, rcprv=rcp/rv
      real (kind=high_pres) :: cond, ssat, wcdum, tsq
!
!-----------------------------------------------------------------------
!
!--- lv (t) is from bolton (jas, 1980)
!
!     xlv=3.148e6-2370.*tk
!     xlv1=xlv*rcp
!     xlv2=xlv*xlv*rcprv
!
      tdum=tk
      wvdum=wv
      wcdum=qw
      esw=min(pp, fpvsl(tdum))                  ! saturation vapor press w/r/t water
!     ws=rhgrd*eps*esw/(pp-esw)                 ! saturation mixing ratio w/r/t water
      ws=rhgrd*eps*esw/(pp+epsm1*esw)           ! saturation mixing ratio w/r/t water
      dwv=wvdum-ws                              ! deficit grid-scale water vapor mixing ratio
      ssat=dwv/ws                               ! supersaturation ratio
      condense=0.
      do while ((ssat.lt.rhlimit1 .and. wcdum.gt.epsq)                  &
     &           .or. ssat.gt.rhlimit)
!
        xlv=3.148e6-2370.*tdum
        xlv1=xlv*rcp
        xlv2=xlv*xlv*rcprv
!
!       cond=dwv/(1.+xlv2*ws/(tdum*tdum))       ! asai (1965, j. japan)
        tsq =tdum*tdum
        cond=dwv*tsq/(tsq+xlv2*ws)              ! asai (1965, j. japan)
        cond=max(cond, -wcdum)                  ! limit cloud water evaporation
        tdum=tdum+xlv1*cond                     ! updated temperature
        wvdum=wvdum-cond                        ! updated water vapor mixing ratio
        wcdum=wcdum+cond                        ! updated cloud water mixing ratio
        condense=condense+cond                  ! total cloud water condensation
        esw=min(pp, fpvsl(tdum))                ! updated saturation vapor press w/r/t water
!       ws=rhgrd*eps*esw/(pp-esw)               ! updated saturation mixing ratio w/r/t water
        ws=rhgrd*eps*esw/(pp+epsm1*esw)         ! updated saturation mixing ratio w/r/t water
        dwv=wvdum-ws                            ! deficit grid-scale water vapor mixing ratio
        ssat=dwv/ws                             ! grid-scale supersaturation ratio
      enddo

      return
      end
