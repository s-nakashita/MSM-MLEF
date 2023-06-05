!      
Subroutine rttov_integratesource_ad (&        
     & ccthres,       &! in
     & nlevels,       &! in
     & nchannels,     &! in
     & nprofiles,     &! in
     & lprofiles,     &! in
     & angles,        &! in
     & scatt_aux,     &! in
     & scatt_aux_ad,  &! inout
     & dp,            &! in
     & dp_ad,         &! inout
     & dm,            &! in
     & dm_ad,         &! inout
     & j_do,          &! inout
     & j_do_ad,       &! inout
     & j_up,          &! inout
     & j_up_ad,       &! inout 
     & adk)            ! in

  ! Description:
  ! integrate source in Eddington
  !
  ! Copyright:
  !    This software was developed within the context of
  !    the EUMETSAT Satellite Application Facility on
  !    Numerical Weather Prediction (NWP SAF), under the
  !    Cooperation Agreement dated 7 December 2016, between
  !    EUMETSAT and the Met Office, UK, by one or more partners
  !    within the NWP SAF. The partners in the NWP SAF are
  !    the Met Office, ECMWF, DWD and MeteoFrance.
  !
  !    Copyright 2002, EUMETSAT, All Rights Reserved.
  !
  ! Method:
  ! - Bauer, P., 2002: Microwave radiative transfer modeling in clouds and precipitation.
  !     Part I: Model description.
  !     NWP SAF Report No. NWPSAF-EC-TR-005, 27 pp.
  ! - Chevallier, F., and P. Bauer, 2003:
  !     Model rain and clouds over oceans: comparison with SSM/I observations.
  !     Mon. Wea. Rev., 131, 1240-1255.
  ! - Moreau, E., P. Bauer and F. Chevallier, 2002: Microwave radiative transfer modeling in clouds and precipitation.
  !     Part II: Model evaluation.
  !     NWP SAF Report No. NWPSAF-EC-TR-006, 27 pp.
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0       09/2002   Initial version     (E. Moreau)
  !  1.1       05/2003   RTTOV7.3 compatible (F. Chevallier)
  !  1.2       03/2004   Added polarimetry   (R. Saunders)
  !  1.3       08/2004   Polarimetry fixes   (U. O'Keefe)
  !  1.4       11/2004   Clean-up            (P. Bauer)
  !  1.5       11/2007   RTTOV9 version      (A. Geer)
  !  1.6       07/2008   Clear sky speed-ups (A. Geer)
  !  1.7       03/2010   Use mclayer rather than min_ssa (A. Geer)
  !  1.8       15/2014   Cure numerical instability (A. Geer)
  !  1.10      07/2020   Bugfix downward aa/cm/cp (H. Xie / A. Geer)
  !  1.11      04/2022   Much tidying / broaden "unstable" range (A. Geer)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  ! Imported Type Definitions:

  Use rttov_types, Only :    &
       & rttov_profile_scatt_aux    ,&
       & rttov_geometry 

  Use parkind1, Only : jpim     ,jprb
!INTF_OFF
  Use rttov_const, Only: adk_adjoint, adk_k

  USE yomhook, ONLY : lhook, dr_hook, jphook
!INTF_ON
  Implicit None

!* Subroutine arguments:
  Real    (Kind=jprb), Intent (in) :: ccthres
  Integer (Kind=jpim), Intent (in) :: nlevels      ! Number of levels
  Integer (Kind=jpim), Intent (in) :: nprofiles    ! Number of profiles
  Integer (Kind=jpim), Intent (in) :: nchannels    ! Number of channels*profiles=radiances
  Integer (Kind=jpim), Intent (in) :: lprofiles (nchannels) ! Profile indices

  Type (rttov_profile_scatt_aux), Intent (in)    :: scatt_aux          ! Auxiliary profile variables for RTTOV_SCATT
  Type (rttov_profile_scatt_aux), Intent (inout) :: scatt_aux_ad       ! Auxiliary profile variables for RTTOV_SCATT
  Type (rttov_geometry),          Intent (in)    :: angles (nprofiles) ! Zenith angles

  Real (Kind=jprb), Intent (in)   , dimension (nchannels,nlevels) :: dp       ! D+ for boundary conditions
  Real (Kind=jprb), Intent (in)   , dimension (nchannels,nlevels) :: dm       ! D- for boundary conditions
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nlevels) :: j_do     ! Downward source terms 
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nlevels) :: j_up     ! Upward source terms
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nlevels) :: dp_ad    ! D+ for boundary conditions
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nlevels) :: dm_ad    ! D- for boundary conditions
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nlevels) :: j_do_ad  ! Downward source terms 
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nlevels) :: j_up_ad  ! Upward source terms
  Integer (Kind=jpim), Intent (in) :: adk ! AD/K switch
!INTF_END

!* Local variables
  Real    (Kind=jprb) :: ja1, jb1, jc1, jd1, aa_up, aa_do, apm, bb, cp_do, cm_do, cp_up, cm_up, cpm, bau
  Real    (Kind=jprb) :: ja2, jb2, jc2, jd2
  Real    (Kind=jprb) :: ja_ad, jb_ad, jc_ad, jd_ad, aa_up_ad, aa_do_ad, apm_ad, bb_ad
  Real    (Kind=jprb) :: cp_do_ad, cm_do_ad, cp_up_ad, cm_up_ad, cpm_ad, bau_ad
  Real    (Kind=jprb) :: coszen
  Integer (Kind=jpim) :: iprof, ichan
  Integer (Kind=jpim) :: iprofad, i
  Logical             :: lstable
    
  REAL(jphook) :: zhook_handle

  !- End of header --------------------------------------------------------

  IF (LHOOK) CALL DR_HOOK('RTTOV_INTEGRATESOURCE_AD',0_jpim,ZHOOK_HANDLE)

  !* Channels * Profiles
  do i=1,nlevels
    do ichan = 1, nchannels
      iprof = lprofiles (ichan)
      if (adk == adk_adjoint) then
        iprofad = iprof
      else if (adk == adk_k) then
        iprofad = ichan
      endif

      if (i >= scatt_aux % mclayer(ichan) .and. scatt_aux % cfrac (iprof) > ccthres ) then

        !* Local constant
        coszen = angles (iprof) % coszen

        !* Reset
        aa_do_ad = 0.0_JPRB
        aa_up_ad = 0.0_JPRB
        apm_ad   = 0.0_JPRB
        bb_ad    = 0.0_JPRB
        cp_do_ad = 0.0_JPRB
        cm_do_ad = 0.0_JPRB
        cp_up_ad = 0.0_JPRB
        cm_up_ad = 0.0_JPRB
        cpm_ad   = 0.0_JPRB
        ja_ad  = 0.0_JPRB
        jb_ad  = 0.0_JPRB
        jc_ad  = 0.0_JPRB
        jd_ad  = 0.0_JPRB
        bau_ad = 0.0_JPRB

        !* Coefficients
        apm = 1.5_JPRB * scatt_aux % asm (ichan,i) * scatt_aux % ssa (ichan,i) &
          & * coszen * scatt_aux % b1 (ichan,i) / scatt_aux % h (ichan,i)
        aa_up = scatt_aux % b0 (ichan,i) - apm
        aa_do = scatt_aux % b0 (ichan,i) + apm
        bb  = scatt_aux % b1 (ichan,i)
        cpm = 1.5_JPRB * scatt_aux % asm (ichan,i) &
          & * coszen * scatt_aux % lambda (ichan,i) / scatt_aux % h (ichan,i)
        cp_up = dp (ichan,i) * scatt_aux % ssa (ichan,i) * (1.0_JPRB - cpm)
        cm_up = dm (ichan,i) * scatt_aux % ssa (ichan,i) * (1.0_JPRB + cpm)
        cp_do = dp (ichan,i) * scatt_aux % ssa (ichan,i) * (1.0_JPRB + cpm)
        cm_do = dm (ichan,i) * scatt_aux % ssa (ichan,i) * (1.0_JPRB - cpm)
        bau = exp (scatt_aux % dz (iprof,i) * scatt_aux % lambda (ichan,i))

        ja1  = 0.0_JPRB
        jb1  = 0.0_JPRB
        jc1  = 0.0_JPRB
        jd1  = 0.0_JPRB

        ja2  = 0.0_JPRB
        jb2  = 0.0_JPRB
        jc2  = 0.0_JPRB
        jd2  = 0.0_JPRB

        !* Downward radiance source terms
        !* FORWARD PART
        ja1 = 1.0_JPRB - scatt_aux % tau (ichan,i)
        jb1 = coszen / scatt_aux % ext (ichan,i) * (1.0_JPRB - scatt_aux % tau (ichan,i)) &
          & - scatt_aux % tau (ichan,i) * scatt_aux % dz (iprof,i) 

        lstable = abs(scatt_aux % ext (ichan,i) - scatt_aux % lambda (ichan,i) * coszen) > 1E-3_JPRB
        if (lstable) then
          jc1 = scatt_aux % ext (ichan,i) / (scatt_aux % lambda (ichan,i) * coszen - scatt_aux % ext (ichan,i)) &
            & * (bau * scatt_aux % tau (ichan,i) - 1.0_JPRB)
        else
          ! Numerically unstable case needs an alternative formulation, valid only for very small dz*(lambda-ext/coszen)
          jc1 = scatt_aux % delta (ichan,i) 
        endif

        jd1 = scatt_aux % ext (ichan,i) / (scatt_aux % lambda (ichan,i) * coszen + scatt_aux % ext (ichan,i)) &
          & * (1.0_JPRB - scatt_aux % tau (ichan,i) / bau) 

        j_do (ichan,i) = ja1 * aa_do + jb1 * bb + jc1 * cp_do + jd1 * cm_do

        !* Upward radiance source terms
        ja2 = 1.0_JPRB - scatt_aux % tau (ichan,i)
        jb2 = scatt_aux % dz (iprof,i) - coszen / scatt_aux % ext (ichan,i) &
          & * (1.0_JPRB - scatt_aux % tau (ichan,i)) 

        jc2 = scatt_aux % ext (ichan,i) / (scatt_aux % ext (ichan,i) + scatt_aux % lambda (ichan,i) * coszen) &
          & * (bau  - scatt_aux % tau (ichan,i)) 
        if (lstable) then
          jd2 = scatt_aux % ext (ichan,i) / (scatt_aux % ext (ichan,i) - scatt_aux % lambda (ichan,i) * coszen) &
             & * (1.0_JPRB / bau  - scatt_aux % tau (ichan,i)) 
        else
          ! Numerically unstable case needs an alternative formulation, valid only for very small dz*(lambda-ext/coszen)
          jd2 = scatt_aux % delta (ichan,i) / bau
        endif

        j_up (ichan,i) = ja2 * aa_up + jb2 * bb + jc2 * cp_up + jd2 * cm_up

        !* ADJOINT PART
        !* Upward radiance source terms

        ja_ad    = ja_ad    + j_up_ad (ichan,i) * aa_up
        aa_up_ad = aa_up_ad + j_up_ad (ichan,i) * ja2
        jb_ad    = jb_ad    + j_up_ad (ichan,i) * bb
        bb_ad    = bb_ad    + j_up_ad (ichan,i) * jb2
        jc_ad    = jc_ad    + j_up_ad (ichan,i) * cp_up
        cp_up_ad = cp_up_ad + j_up_ad (ichan,i) * jc2
        jd_ad    = jd_ad    + j_up_ad (ichan,i) * cm_up
        cm_up_ad = cm_up_ad + j_up_ad (ichan,i) * jd2
        
        j_up_ad (ichan,i) = 0.0_JPRB

        if (lstable) then
          scatt_aux_ad % ext (ichan,i) = scatt_aux_ad % ext (ichan,i) &
            & - jd_ad * scatt_aux % lambda (ichan,i) * coszen &
            & / ( (scatt_aux % ext (ichan,i) - scatt_aux % lambda (ichan,i) * coszen) ** 2) &
            & * (1.0_JPRB/ bau  - scatt_aux % tau (ichan,i)) 
          scatt_aux_ad % lambda (ichan,i) = scatt_aux_ad % lambda (ichan,i) &
            & + jd_ad * coszen * scatt_aux % ext (ichan,i) &
            & / ( (scatt_aux % ext (ichan,i) - scatt_aux % lambda (ichan,i) * coszen) ** 2) &
            & * (1.0_JPRB/ bau  - scatt_aux % tau (ichan,i)) 
          bau_ad = bau_ad -1.0_JPRB * jd_ad  / bau  / bau  * scatt_aux % ext (ichan,i) &
            & / (scatt_aux % ext (ichan,i) - scatt_aux % lambda (ichan,i) * coszen) 
          scatt_aux_ad % tau (ichan,i) = scatt_aux_ad % tau (ichan,i) - jd_ad  * scatt_aux % ext (ichan,i) &
            & / (scatt_aux % ext (ichan,i) - scatt_aux % lambda (ichan,i) * coszen) 
        else
          scatt_aux_ad % delta (ichan,i) = scatt_aux_ad % delta (ichan,i) + jd_ad / bau 
          bau_ad = bau_ad - jd_ad * scatt_aux % delta (ichan,i) / bau / bau 
        endif
        jd_ad  = 0.0_JPRB
    
        scatt_aux_ad % ext (ichan,i) = scatt_aux_ad % ext (ichan,i) &
          & + jc_ad * (bau  - scatt_aux % tau (ichan,i)) &
          & * scatt_aux % lambda (ichan,i) * coszen & 
          & / ( (scatt_aux % ext (ichan,i) + scatt_aux % lambda (ichan,i) * coszen) ** 2)
        scatt_aux_ad % lambda (ichan,i) = scatt_aux_ad % lambda (ichan,i) &
          & - jc_ad * (bau  - scatt_aux % tau (ichan,i)) &
          & * scatt_aux % ext (ichan,i) * coszen  &
          & / ( (scatt_aux % ext (ichan,i) + scatt_aux % lambda (ichan,i) * coszen) ** 2) 
        bau_ad = bau_ad  + jc_ad  * scatt_aux % ext (ichan,i) &
          & / (scatt_aux % ext (ichan,i) + scatt_aux % lambda (ichan,i) * coszen) 
        scatt_aux_ad % tau (ichan,i) = scatt_aux_ad % tau (ichan,i) - jc_ad  * scatt_aux % ext (ichan,i) &
          & / (scatt_aux % ext (ichan,i) + scatt_aux % lambda (ichan,i) * coszen) 
        jc_ad = 0.0_JPRB

        scatt_aux_ad % dz (iprofad,i) = scatt_aux_ad % dz (iprofad,i) + jb_ad 
        scatt_aux_ad % ext (ichan,i) = scatt_aux_ad % ext (ichan,i) + jb_ad &
          & / scatt_aux % ext (ichan,i) / scatt_aux % ext (ichan,i) * coszen * (1.0_JPRB - scatt_aux % tau (ichan,i)) 
        scatt_aux_ad % tau (ichan,i) = scatt_aux_ad % tau (ichan,i) + jb_ad * coszen / scatt_aux % ext (ichan,i)
        jb_ad  = 0.0_JPRB

        scatt_aux_ad % tau (ichan,i)  = scatt_aux_ad % tau (ichan,i)  - ja_ad 
        ja_ad  = 0.0_JPRB

        !* Downward radiance source terms

        ja_ad    = ja_ad    + j_do_ad (ichan,i) * aa_do
        aa_do_ad = aa_do_ad + j_do_ad (ichan,i) * ja1
        jb_ad    = jb_ad    + j_do_ad (ichan,i) * bb
        bb_ad    = bb_ad    + j_do_ad (ichan,i) * jb1
        jc_ad    = jc_ad    + j_do_ad (ichan,i) * cp_do
        cp_do_ad = cp_do_ad + j_do_ad (ichan,i) * jc1
        jd_ad    = jd_ad    + j_do_ad (ichan,i) * cm_do
        cm_do_ad = cm_do_ad + j_do_ad (ichan,i) * jd1
        j_do_ad (ichan,i) = 0.0_JPRB

        scatt_aux_ad % ext (ichan,i) = scatt_aux_ad % ext (ichan,i) &
          & + jd_ad * (1.0_JPRB - scatt_aux % tau (ichan,i) / bau) * scatt_aux % lambda (ichan,i) * coszen &
          & / ( (scatt_aux % lambda (ichan,i) * coszen + scatt_aux % ext (ichan,i)) ** 2)
        scatt_aux_ad % lambda (ichan,i) = scatt_aux_ad % lambda (ichan,i) &
          & - jd_ad * coszen * scatt_aux % ext (ichan,i) * (1.0_JPRB - scatt_aux % tau (ichan,i) / bau ) &
          & / ( (scatt_aux % lambda (ichan,i) * coszen + scatt_aux % ext (ichan,i)) ** 2)
        bau_ad = bau_ad + jd_ad * scatt_aux % ext (ichan,i) * scatt_aux % tau (ichan,i) / bau / bau &
          & / (scatt_aux % lambda (ichan,i) * coszen + scatt_aux % ext (ichan,i)) 
        scatt_aux_ad % tau (ichan,i) = scatt_aux_ad % tau (ichan,i) - jd_ad * scatt_aux % ext (ichan,i) / bau &
          & / (scatt_aux % lambda (ichan,i) * coszen + scatt_aux % ext (ichan,i)) 
        jd_ad  = 0.0_JPRB
 
        if (lstable) then 
       
          scatt_aux_ad % ext (ichan,i) = scatt_aux_ad % ext (ichan,i) &
            & + jc_ad  / (scatt_aux % lambda (ichan,i) * coszen - scatt_aux % ext (ichan,i)) & 
            & * (bau * scatt_aux % tau (ichan,i) - 1.0_JPRB) &
            & * (1.0_JPRB + scatt_aux % ext (ichan,i) / (scatt_aux % lambda (ichan,i) * coszen - scatt_aux % ext (ichan,i))) 
          scatt_aux_ad % lambda (ichan,i) = scatt_aux_ad % lambda (ichan,i) &
            & - jc_ad  * coszen * scatt_aux % ext (ichan,i) * (bau * scatt_aux % tau (ichan,i) - 1.0_JPRB) &
            & / (scatt_aux % lambda (ichan,i) * coszen - scatt_aux % ext (ichan,i)) &
            & / (scatt_aux % lambda (ichan,i) * coszen - scatt_aux % ext (ichan,i))  
          bau_ad = bau_ad + jc_ad * scatt_aux % tau (ichan,i) * scatt_aux % ext (ichan,i) &
            & / (scatt_aux % lambda (ichan,i) * coszen - scatt_aux % ext (ichan,i))  
          scatt_aux_ad % tau (ichan,i) = scatt_aux_ad % tau (ichan,i) + jc_ad * bau * scatt_aux % ext (ichan,i)  &
            & / (scatt_aux % lambda (ichan,i) * coszen - scatt_aux % ext (ichan,i))  
          jc_ad  = 0.0_JPRB

        else
          scatt_aux_ad % delta (ichan,i) = scatt_aux_ad % delta (ichan,i) + jc_ad 
          jc_ad  = 0.0_JPRB      
        endif
        
        scatt_aux_ad % ext (ichan,i) = scatt_aux_ad % ext (ichan,i) &
          & - jb_ad  / scatt_aux % ext (ichan,i) / scatt_aux % ext (ichan,i) * coszen &
          & * (1.0_JPRB - scatt_aux % tau (ichan,i))
        scatt_aux_ad % tau(ichan,i) = scatt_aux_ad % tau(ichan,i)  &
          & - jb_ad  * (coszen / scatt_aux % ext (ichan,i) + scatt_aux % dz (iprof,i))
        scatt_aux_ad % dz (iprofad,i) = scatt_aux_ad % dz (iprofad,i) - jb_ad  * scatt_aux % tau (ichan,i)
        jb_ad  = 0.0_JPRB

        scatt_aux_ad % tau (ichan,i)  = scatt_aux_ad % tau (ichan,i) - ja_ad
        ja_ad  = 0.0_JPRB

        ! Adjoint of coefficients
        scatt_aux_ad % dz (iprofad,i) = scatt_aux_ad % dz (iprofad,i) + bau_ad * scatt_aux % lambda (ichan,i) * bau 
        scatt_aux_ad % lambda (ichan,i) = scatt_aux_ad % lambda (ichan,i) + bau_ad * scatt_aux % dz (iprof,i) * bau 
        bau_ad = 0.0_JPRB

        dm_ad (ichan,i) = dm_ad (ichan,i) + cm_do_ad * scatt_aux % ssa (ichan,i) * (1.0_JPRB - cpm)
        scatt_aux_ad % ssa (ichan,i) = scatt_aux_ad % ssa (ichan,i) + cm_do_ad * dm (ichan,i) * (1.0_JPRB - cpm)
        cpm_ad = cpm_ad - cm_do_ad * dm (ichan,i) * scatt_aux % ssa (ichan,i)
        cm_do_ad = 0.0_JPRB

        dp_ad (ichan,i) = dp_ad (ichan,i) + cp_do_ad * scatt_aux % ssa (ichan,i) * (1.0_JPRB + cpm)
        scatt_aux_ad % ssa (ichan,i) = scatt_aux_ad % ssa (ichan,i) + cp_do_ad * dp (ichan,i) * (1.0_JPRB + cpm)
        cpm_ad = cpm_ad + cp_do_ad  * dp (ichan,i) * scatt_aux % ssa (ichan,i)
        cp_do_ad  = 0.0_JPRB

        dm_ad (ichan,i) = dm_ad (ichan,i) + cm_up_ad * scatt_aux % ssa (ichan,i) * (1.0_JPRB + cpm)
        scatt_aux_ad % ssa (ichan,i) = scatt_aux_ad % ssa (ichan,i) + cm_up_ad * dm (ichan,i) * (1.0_JPRB + cpm)
        cpm_ad = cpm_ad + cm_up_ad * dm (ichan,i) * scatt_aux % ssa (ichan,i)
        cm_up_ad = 0.0_JPRB

        dp_ad (ichan,i) = dp_ad (ichan,i) + cp_up_ad * scatt_aux % ssa (ichan,i) * (1.0_JPRB - cpm)
        scatt_aux_ad % ssa (ichan,i) = scatt_aux_ad % ssa (ichan,i) + cp_up_ad * dp (ichan,i) * (1.0_JPRB - cpm)
        cpm_ad = cpm_ad - cp_up_ad  * dp (ichan,i) * scatt_aux % ssa (ichan,i)
        cp_up_ad  = 0.0_JPRB

        scatt_aux_ad % asm (ichan,i) = scatt_aux_ad % asm (ichan,i) &
          & + cpm_ad * 1.5_JPRB * coszen * scatt_aux % lambda (ichan,i) / scatt_aux % h (ichan,i)
        scatt_aux_ad % lambda (ichan,i) = scatt_aux_ad % lambda (ichan,i) &
          & + cpm_ad * 1.5_JPRB * coszen * scatt_aux % asm (ichan,i) / scatt_aux % h (ichan,i)
        scatt_aux_ad % h (ichan,i) = scatt_aux_ad % h (ichan,i) &
          & - cpm_ad * 1.5_JPRB * coszen * scatt_aux % asm (ichan,i) &
          & * scatt_aux % lambda (ichan,i) / scatt_aux % h (ichan,i) / scatt_aux % h (ichan,i)
        cpm_ad = 0.0_JPRB

        scatt_aux_ad % b1 (ichan,i) = scatt_aux_ad % b1 (ichan,i) + bb_ad
        bb_ad = 0.0_JPRB

        scatt_aux_ad % b0 (ichan,i) = scatt_aux_ad % b0 (ichan,i) + aa_do_ad + aa_up_ad
        apm_ad = apm_ad + aa_do_ad - aa_up_ad
        aa_do_ad = 0._JPRB
        aa_up_ad = 0._JPRB

        scatt_aux_ad % asm (ichan,i) = scatt_aux_ad % asm (ichan,i) &
          & + apm_ad * 1.5_JPRB * coszen * scatt_aux % ssa (ichan,i) &
          & * scatt_aux % b1 (ichan,i) / scatt_aux % h (ichan,i)
        scatt_aux_ad % ssa (ichan,i) = scatt_aux_ad % ssa (ichan,i) &
          & + apm_ad * 1.5_JPRB * coszen * scatt_aux % asm (ichan,i) &
          & * scatt_aux % b1 (ichan,i) / scatt_aux % h (ichan,i)
        scatt_aux_ad % b1 (ichan,i) = scatt_aux_ad % b1 (ichan,i) &
          & + apm_ad * 1.5_JPRB * coszen * scatt_aux % asm (ichan,i) &
          & * scatt_aux % ssa (ichan,i) / scatt_aux % h (ichan,i)
        scatt_aux_ad % h (ichan,i) = scatt_aux_ad % h (ichan,i) &
          & - apm_ad * 1.5_JPRB * coszen * scatt_aux % asm (ichan,i) &
          & * scatt_aux % ssa (ichan,i) * scatt_aux % b1 (ichan,i) &
          & / scatt_aux % h( ichan,i) / scatt_aux % h (ichan,i)
        apm_ad = 0._JPRB
      endif
    end do
  end do

  IF (LHOOK) CALL DR_HOOK('RTTOV_INTEGRATESOURCE_AD',1_jpim,ZHOOK_HANDLE)

End subroutine rttov_integratesource_ad
