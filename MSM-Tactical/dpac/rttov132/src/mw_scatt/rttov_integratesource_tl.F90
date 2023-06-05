!      
Subroutine rttov_integratesource_tl (&        
     & ccthres,       &! in
     & nlevels,       &! in
     & nchannels,     &! in
     & nprofiles,     &! in
     & lprofiles,     &! in
     & angles,        &! in
     & scatt_aux,     &! in
     & scatt_aux_tl,  &! in
     & dp,            &! in
     & dp_tl,         &! in
     & dm,            &! in
     & dm_tl,         &! in
     & j_do,          &! inout
     & j_do_tl,       &! inout
     & j_up,          &! inout
     & j_up_tl)        ! inout 

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
  !  1.6       07/2008   Speed-ups / tidied  (A. Geer)
  !  1.7       03/2010   Use mclayer rather than min_ssa (A. Geer)
  !  1.8       11/2012   Trajectory multiplication order changed so as to reproduce AD exactly (A. Geer)
  !  1.9       01/2014   Numerical instability fixed (A. Geer)
  !  1.10      07/2020   Bugfix downward aa/cm/cp (H. Xie / A. Geer)
  !  1.11      04/2022   Much tidying / broaden "unstable" range (A. Geer)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !   Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  ! Imported Type Definitions:

  Use rttov_types, Only :    &
       & rttov_profile_scatt_aux    ,&
       & rttov_geometry 

  Use parkind1, Only : jpim     ,jprb
!INTF_OFF
  USE yomhook, ONLY : lhook, dr_hook, jphook
!INTF_ON
  Implicit None

!* Subroutine arguments:
  Real    (Kind=jprb), Intent (in) :: ccthres
  Integer (Kind=jpim), Intent (in) :: nlevels               ! Number of levels
  Integer (Kind=jpim), Intent (in) :: nprofiles             ! Number of profiles
  Integer (Kind=jpim), Intent (in) :: nchannels             ! Number of channels*profiles=radiances
  Integer (Kind=jpim), Intent (in) :: lprofiles (nchannels) ! Profile indices

  Type (rttov_profile_scatt_aux), Intent(in) :: scatt_aux         ! Auxiliary profile variables for RTTOV_SCATT
  Type (rttov_profile_scatt_aux), Intent(in) :: scatt_aux_tl      ! Auxiliary profile variables for RTTOV_SCATT
  Type (rttov_geometry),          Intent(in) :: angles (nprofiles)! Zenith angles

  Real (Kind=jprb), Intent (in)   , dimension (nchannels,nlevels) :: dp       ! D+ for boundary conditions
  Real (Kind=jprb), Intent (in)   , dimension (nchannels,nlevels) :: dm       ! D- for boundary conditions
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nlevels) :: j_do     ! Downward source terms 
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nlevels) :: j_up     ! Upward source terms
  Real (Kind=jprb), Intent (in)   , dimension (nchannels,nlevels) :: dp_tl    ! D+ for boundary conditions
  Real (Kind=jprb), Intent (in)   , dimension (nchannels,nlevels) :: dm_tl    ! D- for boundary conditions
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nlevels) :: j_do_tl  ! Downward source terms 
  Real (Kind=jprb), Intent (inout), dimension (nchannels,nlevels) :: j_up_tl  ! Upward source terms

!INTF_END

!* Local variables
  Real    (Kind=jprb) :: ja   , jb   , jc   , jd   , aa_up   , aa_do   , apm   , bb
  Real    (Kind=jprb) :: ja_tl, jb_tl, jc_tl, jd_tl, aa_up_tl, aa_do_tl, apm_tl, bb_tl
  Real    (Kind=jprb) :: cp_do   , cm_do   , cp_up   , cm_up    , cpm    , bau
  Real    (Kind=jprb) :: cp_do_tl, cm_do_tl, cp_up_tl, cm_up_tl , cpm_tl , bau_tl
  Real    (Kind=jprb) :: coszen
  Integer (Kind=jpim) :: iprof, ichan, i
  Logical             :: lstable
  
  REAL(jphook) :: zhook_handle

  !- End of header --------------------------------------------------------

  IF (LHOOK) CALL DR_HOOK('RTTOV_INTEGRATESOURCE_TL',0_jpim,ZHOOK_HANDLE)

  !* Channels * profiles
  do i=1,nlevels
    do ichan = 1, nchannels
      iprof = lprofiles (ichan)
    
      if (i >= scatt_aux % mclayer(ichan) .and. scatt_aux % cfrac (iprof) > ccthres ) then

        !* Local constant
        coszen = angles (iprof) % coszen

        !* Coefficients
        apm_tl = 1.5_JPRB * angles( iprof) % coszen /  scatt_aux % h (ichan,i) &
             & * (scatt_aux_tl % asm (ichan,i) * scatt_aux    % ssa (ichan,i) * scatt_aux    % b1 (ichan,i)  &
             & +  scatt_aux    % asm (ichan,i) * scatt_aux_tl % ssa (ichan,i) * scatt_aux    % b1 (ichan,i)  &
             & +  scatt_aux    % asm (ichan,i) * scatt_aux    % ssa (ichan,i) * scatt_aux_tl % b1 (ichan,i)) &
             & - 1.5_JPRB * scatt_aux % asm (ichan,i) * scatt_aux % ssa (ichan,i) &
             & *  coszen * scatt_aux % b1 (ichan,i) * scatt_aux_tl % h (ichan,i) &
             & / (scatt_aux % h (ichan,i) * scatt_aux % h (ichan,i))
        aa_up_tl = scatt_aux_tl % b0  (ichan,i) - apm_tl
        aa_do_tl = scatt_aux_tl % b0  (ichan,i) + apm_tl

        apm = 1.5_JPRB * scatt_aux % asm (ichan,i) * scatt_aux % ssa (ichan,i) * coszen &
          & * scatt_aux % b1 (ichan,i) / scatt_aux % h (ichan,i)
        aa_up = scatt_aux % b0 (ichan,i) - apm
        aa_do = scatt_aux % b0 (ichan,i) + apm

        bb_tl = scatt_aux_tl % b1 (ichan,i)
        bb    = scatt_aux    % b1 (ichan,i)

        cpm_tl = 1.5_JPRB * coszen  &
             & * (scatt_aux_tl % asm (ichan,i) * scatt_aux    % lambda (ichan,i) / scatt_aux    % h( ichan,i) &
             & +  scatt_aux    % asm (ichan,i) * scatt_aux_tl % lambda (ichan,i) / scatt_aux    % h (ichan,i) &
             & -  scatt_aux    % asm (ichan,i) * scatt_aux    % lambda (ichan,i) * scatt_aux_tl % h (ichan,i) &
             & / (scatt_aux    % h   (ichan,i) * scatt_aux    % h      (ichan,i)))
        cpm = 1.5_JPRB * scatt_aux % asm (ichan,i) * coszen &
          & * scatt_aux % lambda (ichan,i) / scatt_aux % h (ichan,i)

        cp_up_tl = (dp_tl (ichan,i) * scatt_aux % ssa (ichan,i) + dp (ichan,i) * scatt_aux_tl % ssa (ichan,i)) &
               & * (1.0_JPRB - cpm) &
               & - dp (ichan,i) * scatt_aux % ssa (ichan,i) * cpm_tl
        cp_up    = dp (ichan,i) * scatt_aux % ssa (ichan,i) * (1.0_JPRB - cpm)

        cm_up_tl = (dm_tl (ichan,i) * scatt_aux % ssa (ichan,i) + dm (ichan,i) * scatt_aux_tl % ssa (ichan,i)) &
               & * (1.0_JPRB + cpm)&
               & + dm (ichan,i) * scatt_aux % ssa (ichan,i) * cpm_tl
        cm_up    = dm (ichan,i) * scatt_aux % ssa (ichan,i) * (1.0_JPRB + cpm)

        cp_do_tl = (dp_tl (ichan,i) * scatt_aux % ssa (ichan,i) + dp (ichan,i) * scatt_aux_tl % ssa (ichan,i)) &
               & * (1.0_JPRB + cpm) &
               & + dp (ichan,i) * scatt_aux % ssa (ichan,i) * cpm_tl
        cp_do    = dp (ichan,i) * scatt_aux % ssa (ichan,i) * (1.0_JPRB + cpm)

        cm_do_tl = (dm_tl (ichan,i) * scatt_aux % ssa (ichan,i) + dm (ichan,i) * scatt_aux_tl % ssa (ichan,i)) &
               & * (1.0_JPRB - cpm)&
               & - dm (ichan,i) * scatt_aux % ssa (ichan,i) * cpm_tl
        cm_do    = dm (ichan,i) * scatt_aux % ssa (ichan,i) * (1.0_JPRB - cpm)

        bau    = exp (scatt_aux % dz (iprof,i) * scatt_aux % lambda (ichan,i))
        bau_tl = (scatt_aux_tl % dz (iprof,i) * scatt_aux    % lambda (ichan,i) &
           &   +  scatt_aux    % dz (iprof,i) * scatt_aux_tl % lambda (ichan,i)) * bau

        !* Downward radiance source terms
        ja_tl = -1.0_JPRB * scatt_aux_tl % tau (ichan,i)
        ja    =  1.0_JPRB - scatt_aux    % tau (ichan,i)
 
        jb_tl = -1.0_JPRB * coszen * (scatt_aux_tl % ext (ichan,i) / (scatt_aux % ext (ichan,i) * scatt_aux % ext (ichan,i)) &
            & * (1.0_JPRB - scatt_aux % tau (ichan,i)) &
            & + scatt_aux_tl % tau (ichan,i) /  scatt_aux % ext (ichan,i)) &
            & - scatt_aux_tl % tau (ichan,i) *  scatt_aux % dz  (iprof,i) &
            & - scatt_aux % tau (ichan,i) * scatt_aux_tl % dz (iprof,i) 
        jb = coszen / scatt_aux % ext (ichan,i) * (1.0_JPRB - scatt_aux % tau (ichan,i)) &
         & - scatt_aux % tau (ichan,i) * scatt_aux % dz (iprof,i) 

        lstable = abs(scatt_aux % ext (ichan,i) - scatt_aux % lambda (ichan,i) * coszen) > 1E-3_JPRB
        if (lstable) then

          jc_tl = (scatt_aux_tl % ext (ichan,i) * scatt_aux % lambda (ichan,i) &
              & - scatt_aux_tl % lambda (ichan,i) * scatt_aux % ext (ichan,i)) &
              & * (bau * scatt_aux % tau (ichan,i) - 1.0_JPRB) * coszen &
              & / ((scatt_aux % lambda (ichan,i) * coszen - scatt_aux % ext (ichan,i)) **2) &
              & + (bau_tl * scatt_aux % tau (ichan,i) + scatt_aux_tl % tau (ichan,i) * bau) &
              & * scatt_aux % ext (ichan,i) / (scatt_aux % lambda (ichan,i) * coszen - scatt_aux % ext (ichan,i))     
          jc    = scatt_aux % ext (ichan,i) / (scatt_aux % lambda (ichan,i) * coszen - scatt_aux % ext (ichan,i)) &
              & * (bau * scatt_aux % tau (ichan,i) - 1.0_JPRB)
 
        else
        
          ! Numerically unstable case needs an alternative formulation, valid only for very small dz*(lambda-ext/coszen)
          jc_tl = scatt_aux_tl % delta (ichan,i) 
          jc    = scatt_aux    % delta (ichan,i) 

        endif

        jd_tl = (scatt_aux_tl % ext (ichan,i)    * scatt_aux % lambda (ichan,i) &
            & -  scatt_aux_tl % lambda (ichan,i) * scatt_aux % ext (ichan,i)) &
            & * (1.0_JPRB - scatt_aux % tau (ichan,i) / bau) * coszen &
            & / ((scatt_aux % lambda (ichan,i) * coszen + scatt_aux % ext (ichan,i)) ** 2) &
            & + (bau_tl * scatt_aux % tau (ichan,i) / bau  / bau - scatt_aux_tl % tau (ichan,i) / bau) &
            & * scatt_aux % ext (ichan,i) / (scatt_aux % lambda (ichan,i) * coszen + scatt_aux % ext (ichan,i)) 
        jd    = scatt_aux % ext (ichan,i) / (scatt_aux % lambda (ichan,i) * coszen + scatt_aux % ext (ichan,i)) &
            & * (1.0_JPRB - scatt_aux % tau (ichan,i) / bau )
  
        j_do_tl (ichan,i) = ja_tl * aa_do + ja * aa_do_tl + jb_tl * bb    + jb * bb_tl  &
                        & + jc_tl * cp_do + jc * cp_do_tl + jd_tl * cm_do + jd * cm_do_tl
        j_do    (ichan,i) = ja * aa_do + jb * bb + jc * cp_do + jd * cm_do

        !* Upward radiance source terms

        ja_tl  = -1.0_JPRB * scatt_aux_tl % tau (ichan,i)
        ja     =  1.0_JPRB - scatt_aux    % tau (ichan,i)
       
        jb_tl = scatt_aux_tl % dz (iprof,i) &
            & + scatt_aux_tl % ext (ichan,i) / (scatt_aux % ext (ichan,i) * scatt_aux % ext (ichan,i)) &
            & * coszen * (1.0_JPRB - scatt_aux % tau (ichan,i)) &
            & + scatt_aux_tl % tau (ichan,i) * coszen / scatt_aux % ext (ichan,i)  
        jb    = scatt_aux % dz (iprof,i) - coszen / scatt_aux % ext (ichan,i) * (1.0_JPRB - scatt_aux % tau (ichan,i)) 

        jc_tl = (scatt_aux_tl % ext (ichan,i) * scatt_aux % lambda (ichan,i)  &
            & -  scatt_aux_tl % lambda(ichan,i) * scatt_aux % ext (ichan,i)) &
            & * (bau - scatt_aux % tau (ichan,i)) * coszen &
            & / ((scatt_aux % ext (ichan,i) + scatt_aux % lambda (ichan,i) * coszen) ** 2) &
            & + (bau_tl - scatt_aux_tl % tau (ichan,i)) &
            & * scatt_aux % ext (ichan,i) / (scatt_aux % ext (ichan,i) + scatt_aux % lambda (ichan,i) * coszen) 
        jc    = scatt_aux % ext (ichan,i) / (scatt_aux % ext (ichan,i) + scatt_aux % lambda (ichan,i) * coszen) &
            & * (bau - scatt_aux % tau (ichan,i)) 

        if(lstable) then

          jd_tl = (scatt_aux_tl % ext (ichan,i) * (-1.0_JPRB) * scatt_aux % lambda (ichan,i) &
              & + scatt_aux_tl % lambda (ichan,i) * scatt_aux % ext (ichan,i)) &
              & * (1.0_JPRB/bau - scatt_aux % tau (ichan,i)) * coszen &
              & / ((scatt_aux % ext (ichan,i) - scatt_aux % lambda (ichan,i) * coszen) ** 2) & 
              & - (bau_tl / bau / bau + scatt_aux_tl % tau (ichan,i)) &
              & * scatt_aux % ext (ichan,i) / (scatt_aux % ext (ichan,i) - scatt_aux % lambda (ichan,i) * coszen) 
          jd    = scatt_aux % ext (ichan,i) / (scatt_aux % ext (ichan,i) - scatt_aux % lambda (ichan,i) * coszen) &
              & * (1.0_JPRB / bau - scatt_aux % tau (ichan,i)) 

        else

          ! Numerically unstable case needs an alternative formulation, valid only for very small dz*(lambda-ext/coszen)
          jd_tl = scatt_aux_tl % delta (ichan,i) / bau - scatt_aux % delta (ichan,i) * bau_tl / bau / bau 
          jd    = scatt_aux % delta (ichan,i) / bau 
  
        endif

        j_up_tl (ichan,i) = ja_tl * aa_up + ja * aa_up_tl + jb_tl * bb    + jb * bb_tl &
                        & + jc_tl * cp_up + jc * cp_up_tl + jd_tl * cm_up + jd * cm_up_tl
        j_up    (ichan,i) = ja * aa_up + jb * bb + jc * cp_up + jd * cm_up

      end if
    end do
  end do
  
  IF (LHOOK) CALL DR_HOOK('RTTOV_INTEGRATESOURCE_TL',1_jpim,ZHOOK_HANDLE)

End subroutine rttov_integratesource_tl
