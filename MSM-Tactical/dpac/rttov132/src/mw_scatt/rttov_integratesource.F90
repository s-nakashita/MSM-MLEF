!      
Subroutine rttov_integratesource (&        
     & ccthres,       &! in
     & nlevels,       &! in
     & nchannels,     &! in
     & nprofiles,     &! in
     & lprofiles,     &! in
     & angles,        &! in
     & scatt_aux,     &! in
     & dp,            &! in
     & dm,            &! in
     & j_do,          &! inout
     & j_up)           ! inout 

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
  ! - Bauer, P., 2002: Microwave radiative transfer modeling in clouds and 
  !     precipitation. Part I: Model description.
  !     NWP SAF Report No. NWPSAF-EC-TR-005, 27 pp.
  ! - Chevallier, F., and P. Bauer, 2003:
  !     Model rain and clouds over oceans: comparison with SSM/I observations.
  !     Mon. Wea. Rev., 131, 1240-1255.
  ! - Moreau, E., P. Bauer and F. Chevallier, 2002: Microwave radiative transfer 
  !     modeling in clouds and precipitation. Part II: Model evaluation.
  !     NWP SAF Report No. NWPSAF-EC-TR-006, 27 pp.
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0       09/2002   Initial version (P. Bauer, E. Moreau)
  !  1.1       05/2003   RTTOV7.3 compatible (F. Chevallier)
  !  1.2       03/2004   Added polarimetry   (R. Saunders)
  !  1.3       08/2004   Polarimetry fixes   (U. O'Keefe)
  !  1.4       11/2004   Clean-up            (P. Bauer)
  !  1.5       11/2007   RTTVOV9 version     (A. Geer)
  !  1.6       07/2008   Clear sky speed-ups (A. Geer)
  !  1.7       03/2010   Use mclayer rather than min_ssa (A. Geer)
  !  1.8       01/2014   Very rare numerical instability fixed (A. Geer)
  !  1.9       11/2017   R/T now done with radiances, not Tb (A. Geer)
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
  Integer (Kind=jpim), Intent (in) :: nlevels   ! Number of levels
  Integer (Kind=jpim), Intent (in) :: nprofiles ! Number of profiles
  Integer (Kind=jpim), Intent (in) :: nchannels ! Number of channels*profiles=radiances
  Integer (Kind=jpim), Intent (in) :: lprofiles (nchannels) ! Profile indices

  ! Auxiliary profile variables for RTTOV_SCATT
  Type (rttov_profile_scatt_aux), Intent (in) :: scatt_aux

  Type (rttov_geometry),          Intent (in) :: angles (nprofiles) ! Zenith angles

  Real (Kind=jprb), Intent (in)    :: dp  (nchannels,nlevels) ! D+ for boundary conditions
  Real (Kind=jprb), Intent (in)    :: dm  (nchannels,nlevels) ! D- for boundary conditions
  Real (Kind=jprb), Intent (inout) :: j_do(nchannels,nlevels) ! Downward source terms 
  Real (Kind=jprb), Intent (inout) :: j_up(nchannels,nlevels) ! Upward source terms

!INTF_END

!* Local variables
  Real    (Kind=jprb) :: ja, jb, jc, jd, aa_up, aa_do, apm, bb, cp_do, cm_do, cp_up, cm_up, cpm, bau
  Real    (Kind=jprb) :: coszen
  Integer (Kind=jpim) :: iprof, ichan, i
  Logical             :: lstable
  
  REAL(jphook) :: zhook_handle

  !- End of header --------------------------------------------------------

  IF (LHOOK) CALL DR_HOOK('RTTOV_INTEGRATESOURCE',0_jpim,ZHOOK_HANDLE)
  
  !* Channels * Profiles
  do i=1,nlevels
    do ichan = 1, nchannels
      iprof = lprofiles (ichan)

      !* Reset
      if (i >= scatt_aux % mclayer(ichan) .and. scatt_aux % cfrac (iprof) > ccthres ) then

        ja  = 0.0_JPRB
        jb  = 0.0_JPRB
        jc  = 0.0_JPRB
        jd  = 0.0_JPRB

        !* Local constant
        coszen = angles (iprof) % coszen

        !* Coefficients
        apm = 1.5_JPRB * scatt_aux % asm (ichan,i) * scatt_aux % ssa (ichan,i) * coszen &
          & * scatt_aux % b1 (ichan,i) / scatt_aux % h (ichan,i)

        aa_up = scatt_aux % b0 (ichan,i) - apm
        aa_do = scatt_aux % b0 (ichan,i) + apm

        bb = scatt_aux % b1 (ichan,i)

        cpm = 1.5_JPRB * scatt_aux % asm (ichan,i) * coszen &
          & * scatt_aux % lambda (ichan,i) / scatt_aux % h (ichan,i)

        cp_up = dp (ichan,i) * scatt_aux % ssa (ichan,i) * (1.0_JPRB - cpm)
        cm_up = dm (ichan,i) * scatt_aux % ssa (ichan,i) * (1.0_JPRB + cpm)
        cp_do = dp (ichan,i) * scatt_aux % ssa (ichan,i) * (1.0_JPRB + cpm)
        cm_do = dm (ichan,i) * scatt_aux % ssa (ichan,i) * (1.0_JPRB - cpm)

        bau = exp (scatt_aux % dz (iprof,i) * scatt_aux % lambda (ichan,i))

        !* Downward radiance source terms    
        ja = 1.0_JPRB - scatt_aux % tau (ichan,i)
        jb = coszen / scatt_aux % ext (ichan,i) * (1.0_JPRB - scatt_aux % tau (ichan,i)) &
         & - scatt_aux % tau (ichan,i) * scatt_aux % dz (iprof,i) 

        lstable = abs(scatt_aux % ext (ichan,i) - scatt_aux % lambda (ichan,i) * coszen) > 1E-3_JPRB
        if (lstable) then
          jc = scatt_aux % ext (ichan,i) / (scatt_aux % lambda (ichan,i) * coszen - scatt_aux % ext (ichan,i)) &
           & * (bau * scatt_aux % tau (ichan,i) - 1.0_JPRB) 
        else
          ! Numerically unstable case needs an alternative formulation, valid only for very small dz*(lambda-ext/coszen)
          jc = scatt_aux % delta (ichan,i)     
        endif
        
        jd = scatt_aux % ext (ichan,i) / (scatt_aux % lambda (ichan,i) * coszen + scatt_aux % ext (ichan,i)) &
         & * (1.0_JPRB - scatt_aux % tau (ichan,i) / bau) 

        j_do (ichan,i) = ja * aa_do + jb * bb + jc * cp_do + jd * cm_do

        !* Upward radiance source terms    
        ja = 1.0_JPRB - scatt_aux % tau (ichan,i)
        jb = scatt_aux % dz (iprof,i) &
         & - coszen / scatt_aux % ext (ichan,i) * (1.0_JPRB - scatt_aux % tau (ichan,i)) 

        jc = scatt_aux % ext (ichan,i) / (scatt_aux % ext (ichan,i) + scatt_aux % lambda (ichan,i) * coszen) &
         & * (bau  - scatt_aux % tau (ichan,i)) 
        if (lstable) then             
          jd = scatt_aux % ext (ichan,i) / (scatt_aux % ext (ichan,i) - scatt_aux % lambda (ichan,i) * coszen) &
           & * (1.0_JPRB / bau  - scatt_aux % tau (ichan,i)) 
        else
          ! Numerically unstable case needs an alternative formulation, valid only for very small dz*(lambda-ext/coszen)
          jd = scatt_aux % delta (ichan,i) / bau
        endif

        j_up (ichan,i) = ja * aa_up + jb * bb + jc * cp_up + jd * cm_up

      endif
    enddo
  enddo

  IF (LHOOK) CALL DR_HOOK('RTTOV_INTEGRATESOURCE',1_jpim,ZHOOK_HANDLE)

End subroutine rttov_integratesource
