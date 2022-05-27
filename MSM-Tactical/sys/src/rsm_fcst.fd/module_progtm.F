      module module_progtm
      use machine , only : kind_phys
      implicit none
      save
!
      integer,parameter:: ntype=9
      integer,parameter:: ngrid=22
      real(kind=kind_phys) b(ntype), satpsi(ntype), satkt(ntype),        &                     
     &                     tsat(ntype),                                  &
     &                     dfk(ngrid,ntype),                             &
     &                     ktk(ngrid,ntype),                             &
     &                     dfkt(ngrid,ntype)
!
!  the nine soil types are:
!    1  ... loamy sand (coarse)
!    2  ... silty clay loam (medium)
!    3  ... light clay (fine)
!    4  ... sandy loam (coarse-medium)
!    5  ... sandy clay (coarse-fine)
!    6  ... clay loam  (medium-fine)
!    7  ... sandy clay loam (coarse-med-fine)
!    8  ... loam  (organic)
!    9  ... ice (use loamy sand property)
!
!     data b/4.05,4.38,4.9,5.3,5.39,7.12,7.75,8.52,                      &                         
!    &       10.4,10.4,11.4/
!     data satpsi/.121,.09,.218,.786,.478,.299,.356,.63,                 &
!    &            .153,.49,.405/
!     data satkt/1.76e-4,1.5633e-4,3.467e-5,7.2e-6,6.95e-6,              &
!    &           6.3e-6,1.7e-6,2.45e-6,2.167e-6,1.033e-6,
!    &           1.283e-6/
!     data tsat/.395,.41,.435,.485,.451,.42,.477,.476,
!    &          .426,.492,.482/
      data b/4.26,8.72,11.55,4.74,10.73,8.17,6.77,5.25,4.26/
      data satpsi/.04,.62,.47,.14,.10,.26,.14,.36,.04/
      data satkt/1.41e-5,.20e-5,.10e-5,.52e-5,.72e-5,                    &  
     &           .25e-5,.45e-5,.34e-5,1.41e-5/
      data tsat/.421,.464,.468,.434,.406,.465,.404,.439,.421/
!
      contains
      subroutine grddf
      use machine , only : kind_phys
      implicit none
      integer              i,    k
      real(kind=kind_phys) dynw, f1, f2, theta
!
!  grddf sets up moisture diffusivity and hydrolic conductivity
!  for all soil types
!  grddfs sets up thermal diffusivity for all soil types
!
      do k = 1, ntype
        dynw = tsat(k) * .05
        f1 = b(k) * satkt(k) * satpsi(k) / tsat(k) ** (b(k) + 3.)
        f2 = satkt(k) / tsat(k) ** (b(k) * 2. + 3.)
!
!  convert from m/s to kg m-2 s-1 unit
!
        f1 = f1 * 1000.
        f2 = f2 * 1000.
        do i = 1, ngrid
          theta = float(i-1) * dynw
          theta = min(tsat(k),theta)
          dfk(i,k) = f1 * theta ** (b(k) + 2.)
          ktk(i,k) = f2 * theta ** (b(k) * 2. + 3.)
        enddo
      enddo
      end subroutine
      subroutine grdkt
      use machine , only : kind_phys
      implicit none
      integer              i,    k
      real(kind=kind_phys) dynw, f1, theta, pf
      do k = 1, ntype
        dynw = tsat(k) * .05
        f1 = log10(satpsi(k)) + b(k) * log10(tsat(k)) + 2.
        do i = 1, ngrid
          theta = float(i-1) * dynw
          theta = min(tsat(k),theta)
          if(theta.gt.0.) then
            pf = f1 - b(k) * log10(theta)
          else
            pf = 5.2
          endif
          if(pf.le.5.1) then
            dfkt(i,k) = exp(-(2.7+pf)) * 420.
          else
            dfkt(i,k) = .1744
          endif
        enddo
      enddo
      end subroutine
!
      end module module_progtm
