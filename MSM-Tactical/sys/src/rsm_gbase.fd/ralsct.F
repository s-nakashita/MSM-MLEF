      subroutine ralsct(si,km,nbdd,taur)
!fpp$ noconcur r
!********************************************************************
!  setup common block 'swaer' for rayleigh scattering
!  optical properties in eight uv+vis bands and four nir bands.
!   band: 1. 0.175-0.225 (uv-c)     2. 0.225-0.245;0.260-0.280 (uv-c)
!         3. 0.245-0.260 (uv-c)     4. 0.280-0.295 (uv-b)
!         5. 0.295-0.310 (uv-b)     6. 0.310-0.320 (uv-b)
!         7. 0.320-0.400 (uv-a)     8. 0.400-0.700 (par)
!         9. 2.27 - 4.0  (nir)     10. 1.22 - 2.27 (nir)
!        11. 0.70 - 1.22 (nir)     12. 0.70 - 4.0  (nir)
!
!     taur    - rayleigh scattering optical depth  -    km*nbd
!********************************************************************
!
      use machine , only : kind_rad
      use module_radsw_parameters
!     use rnddta , only : nbly
      implicit none
!
      integer km, nbdd
!
      real (kind=kind_rad),save ::  tauray(nbd)
!
      real (kind=kind_rad) si(km+1), taur(km,nbdd)
      integer k, n
!
! --- tauray(nbd)
!
      data  tauray                                                       &  
     &      / 7.006,   2.117,   2.453,   1.398                           &
     &,       1.133,   .9532,   .6104,   .1096                           &
     &,      .289e-3, .375e-2, .354e-1, .128e-1 /
!    1      / 6.262,   1.848,   2.370,   1.357
!    2,       1.133,   .9532,   .6104,   .1096
!    3,      .723e-4, .868e-3, .103e-1, .838e-2 /
!
!===> ... compute layer distributions of rayleigh scattering
!
      do n=1,nbd
        do k=1,km
          taur(k,n) = tauray(n) * (si(km+1-k)-si(km+2-k))
        enddo
      enddo
!
      return
      end
