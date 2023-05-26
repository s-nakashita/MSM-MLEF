      subroutine gaeros(si,sl,km,kmp1,naee,nbdd,ndmm                    &
     &,                 haer,idm)
!fpp$ noconcur r
!********************************************************************
!  setup common block 'swaer' for aerosols and rayleigh scattering
!  optical properties in eight uv+vis bands and four nir bands.
!   band: 1. 0.175-0.225 (uv-c)     2. 0.225-0.245;0.260-0.280 (uv-c)
!         3. 0.245-0.260 (uv-c)     4. 0.280-0.295 (uv-b)
!         5. 0.295-0.310 (uv-b)     6. 0.310-0.320 (uv-b)
!         7. 0.320-0.400 (uv-a)     8. 0.400-0.700 (par)
!         9. 2.27 - 4.0  (nir)     10. 1.22 - 2.27 (nir)
!        11. 0.70 - 1.22 (nir)     12. 0.70 - 4.0  (nir)
!  ref: wmo report wcp-112 (1986)
!       opac (bull. am. meteo. soc., 1998)
!
!  1999-10-13 y.h. updated to opac data (1998)
!
!  there are seven typical vertical structures:
!     1.antarctic, 2.arctic, 3.continent, 4.maritime, 5.desert,
!     6.maritime with mineral overlay, 7.continent with mineral overlay
!
!     sigref  - ref. sigma level                   -    ndm*nae
!  arrays in the common block:
!     haer    - scale height of aerosols           km   ndm*nae
!     idm     - domain index                       -    l*nae
!********************************************************************
!
      use machine , only : kind_rad
      use module_radsw_parameters
!     use rnddta , only : nbly
      implicit none
!
      integer km, kmp1, ndmm, naee, nbdd
!
      real (kind=kind_rad) sigref(ndm,nae),sigln(kmp1),haer1(ndm,nae)
!
      real (kind=kind_rad) si(kmp1), sl(km), haer(ndmm,naee)
      integer idm(km,naee), idom, k, iaer, n
!jW
      save sigref,haer1
!
! --- sigref(ndm,nae)
!
      data  sigref                                                      &
     &      / 0.261, 0.193, 0.193, .0058, 0.000,                        &
     &        0.785, 0.193, 0.193, .0058, 0.000,                        &
     &        0.785, 0.193, 0.193, .0058, 0.000,                        &
     &        0.785, 0.193, 0.193, .0058, 0.000,                        &
     &        0.463, 0.193, 0.193, .0058, 0.000,                        &
     &        0.785, 0.648, 0.193, .0058, 0.000,                        &
     &        0.785, 0.648, 0.193, .0058, 0.000 /
!
! --- scale height of aerosol of each domain and profile
!
      data haer1                                                        &
     & / 8.0, 0.0, 8.0, 0.0, 0.0,                                       &
     &   0.0, 0.0, 8.0, 0.0, 0.0,                                       &
     &   8.0, 0.0, 8.0, 0.0, 0.0,                                       &
     &   1.0, 0.0, 8.0, 0.0, 0.0,                                       &
     &   2.0, 0.0, 8.0, 0.0, 0.0,                                       &
     &   1.0, 0.0, 8.0, 0.0, 0.0,                                       &
     &   8.0, 0.0, 8.0, 0.0, 0.0 /
!
      if (ndm .ne. ndmm .or. nae .ne. naee) then
        print *,' parameters in gaeros not ok -- run aborted'
        call abort
      endif
      haer = haer1
!
      do 60 iaer=1,nae
        idom = 1
        do 50 k=km,1,-1
          if (si(kmp1-k+1) .lt. sigref(idom,iaer)) then
            idom = idom + 1
            if (idom.eq.2 .and.                                         &
     &          sigref(2,iaer).eq.sigref(3,iaer)) then
              idom = 3
            end if
          end if
          idm (k,iaer) = idom
  50    continue
  60  continue
!
      return
      end
