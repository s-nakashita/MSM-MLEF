
      subroutine swflux(tau,ssc,g0,ff,csm,zth,alb,ald,                   &  
     &                  upflux,dwflux,dwsfcb,dwsfcd, l, lp1, imax)
!    &,                lprnt)
!fpp$ noconcur r
!********************************************************************
!  uses the delta-eddington approximation to compute the bulk
!  scattering properties of a single layer coded following
!  coakley et al.  (jas, 1982)
!
!  inputs:
!    tau: the effective optical thickness
!    ssc: the effective single scattering albedo
!    g0:  the effective asymmetry factor
!    ff:  the effective forward scattering factor
!    csm: the effective secant of the zenith angle
!    alb: surface albedo for direct radiation
!    ald: surface albedo for diffused radiation
!
!  outputs:
!    upflux: upward fluxes
!    dwflux: downward fluxes
!    dwsfcb: downward surface flux direct component
!    dwsfcd: downward surface flux diffused component
!********************************************************************
!
!
      use machine , only : kind_rad
      implicit none
!
      integer l, lp1, imax
!
! --- input
      real (kind=kind_rad)                                               &  
     &  tau(imax,l), ssc(imax,l), g0(imax,l), ff(imax,l)                 &
     &, csm(imax,l), zth(imax,l), alb(imax),  ald(imax)
! --- output
      real (kind=kind_rad)                                               &  
     &  upflux(imax,lp1),dwflux(imax,lp1),dwsfcb(imax),dwsfcd(imax)
! --- temporary
      real (kind=kind_rad)                                               &  
     &  ttb(imax,lp1),tdn(imax,lp1),rup(imax,lp1), tt (imax,lp1,2)       &
     &, rfu(imax,lp1),rfd(imax,lp1),tb (imax,lp1), rr (imax,lp1,2)
!     logical lprnt
!
      real (kind=kind_rad) zthd, csmd,  epsln, aa,   taup, sscp, gp      &
     &,                     oms1, ogs1,  tlam,  u1,   u1p1, u1m1, e1     &
     &,                     u1e,  u1epe, u1eme, den,  rf1,  tf1,  zzth   &
     &,                     zz,   den1,  gama,  alfa, amg,  apg,  za     &
     &,                     slam, bb
      integer i, k, km1
!
!===> ... diffuse incident radiation is approximated by beam radiation
!         with an incident angle of 53 degrees. cos(53) = 0.602
      zthd = 0.602
      csmd = 1.0 / zthd
      epsln = 1.0e-30
!
!===> ... delta-eddington scaling of single scattering albedo,
!         optical thickness, and asymmetry factor, k & h eqs(27-29)
!
      do k=1,l
        do i=1,imax
          aa    = 1.0 - ff(i,k)*ssc(i,k)
          taup  = tau(i,k) * aa
          sscp  = ssc(i,k) * (1.0 - ff(i,k)) / aa
          gp    = (g0(i,k) - ff(i,k)) / (1.0 - ff(i,k))
!
          oms1  = 1.0 - sscp
          ogs1  = 1.0 - sscp*gp
          tlam  = 3.0 * oms1*ogs1
          slam  = sqrt(tlam)
!
          u1    = 1.5 * ogs1 / slam
          u1p1  = u1 + 1.0
          u1m1  = u1 - 1.0
          e1    = exp(max(-taup*slam, -30.0))
          u1e   = u1 * e1
          u1epe = u1e + e1
          u1eme = u1e - e1
          den   = 1.0 / ((u1p1 + u1eme)*(u1p1 - u1eme))
          rf1   = (u1p1 + u1epe) * (u1m1 - u1eme) * den
          tf1   = 4.0 * u1e * den
!
!===> ... compute layer transmissions and reflections
!         (i,k,j) j=1,2 for layer k illuminated by diffuse and
!                       direct incoming radiation
!         rr   :  layer reflection
!         tt   :  layer total transmission
!         tb   :  layer direct transmission
!
!      diffuse radiation
!      -----------------
          zzth = zthd
          zz   = zzth * zzth
          den1 = 1.0 - tlam*zz
          if (abs(den1) .lt. 1.0e-8) then    !===> ... safety check
            zzth = zzth + 0.001
            zz   = zzth * zzth
            den1 = 1.0 - tlam*zz
          end if
          den1   = sscp / den1
!
          gama   = 0.50 * (1.0 + 3.0*gp*oms1*zz)*den1
          alfa   = 0.75 * zthd * (gp + ogs1)*den1
          amg    = alfa - gama
          apg    = alfa + gama
!
          tb(i,k)   = exp( -min(30.0, taup*csmd) )
          za        = amg * tb(i,k)
          rr(i,k,1) = za*tf1 + apg*rf1 - amg
          tt(i,k,1) = za*rf1 + apg*tf1 + (1.0-apg)*tb(i,k)
!
!      direct radiation
!      ----------------
          zzth = zth(i,k)
          zz   = zzth * zzth
          den1 = 1.0 - tlam*zz
          if (abs(den1) .lt. 1.0e-8) then    !===> ... safety check
            zzth = zzth + 0.001
            zz   = zzth * zzth
            den1 = 1.0 - tlam*zz
          end if
          den1   = sscp / den1
!
          gama   = 0.50 * (1.0 + 3.0*gp*oms1*zz)*den1
          alfa   = 0.75 * zth(i,k) * (gp + ogs1)*den1
          amg    = alfa - gama
          apg    = alfa + gama
!
          tb(i,k)   = exp( -min(30.0, taup*csm(i,k)) )
          za        = amg * tb(i,k)
          rr(i,k,2) = za*tf1 + apg*rf1 - amg
          tt(i,k,2) = za*rf1 + apg*tf1 + (1.0-apg)*tb(i,k)
!
          tb(i,k)   = max(0.0, tb(i,k))
          rr(i,k,2) = max(0.0, rr(i,k,2))
          tt(i,k,2) = max(0.0, tt(i,k,2))
          rr(i,k,1) = max(0.0, rr(i,k,1))
          tt(i,k,1) = max(0.0, tt(i,k,1))
        enddo
      enddo
!
! --- at the surface
!
      do i=1,imax
        tb(i,lp1)   = 0.0
        rr(i,lp1,2) = alb(i)
        tt(i,lp1,2) = 0.0
        rr(i,lp1,1) = ald(i)
        tt(i,lp1,1) = 0.0
      end do
!
      do i=1,imax
        ttb(i,1) = tb(i,1)
        tdn(i,1) = tt(i,1,2)
        rfd(i,1) = rr(i,1,1)
        ttb(i,l) = 0.0
      enddo
!
!===> ... layers added downward starting from top
!
      do k=2,lp1
        do i=1,imax
           den = tt(i,k,1) / (1.0 - rfd(i,k-1) * rr(i,k,1))
           ttb(i,k) = ttb(i,k-1) * tb(i,k)
           if (ttb(i,k) .lt. epsln) ttb(i,k) = 0.0
           tdn(i,k) = ttb(i,k-1)*tt(i,k,2)+(tdn(i,k-1)-ttb(i,k-1)        &  
     &              + ttb(i,k-1)*rr(i,k,2)*rfd(i,k-1)) * den
           rfd(i,k) = rr(i,k,1) + tt(i,k,1)*rfd(i,k-1) * den
        enddo
      enddo
!
!===> ... layers added upward starting from surface
!
      do i=1,imax
        rfu(i,lp1) = rr(i,lp1,1)
        rup(i,lp1) = rr(i,lp1,2)
      enddo
      do k=l,1,-1
        do i=1,imax
           den = tt(i,k,1) / (1.0 - rfu(i,k+1) * rr(i,k,1))
           rup(i,k) = rr(i,k,2) + ((tt(i,k,2)-tb(i,k))*rfu(i,k+1)        &  
     &              + tb(i,k)*rup(i,k+1)) * den
           rfu(i,k) = rr(i,k,1) + tt(i,k,1)*rfu(i,k+1) * den
        enddo
      enddo
!
!===> ... find upward and downward fluxes
!
      do i=1,imax
          upflux(i,1) = rup(i,1)
          dwflux(i,1) = 1.0
      enddo
      do k=2,lp1
        km1 = k - 1
        do i=1,imax
            den         = 1.0 / (1.0 - rfd(i,km1)*rfu(i,k))
            aa          = ttb(i,km1) * rup(i,k)
            bb          = tdn(i,km1) - ttb(i,km1)
            upflux(i,k) = (aa + bb*rfu(i,k)) * den
            dwflux(i,k) = ttb(i,km1) + (aa*rfd(i,km1) + bb) * den
        enddo
      enddo
!
!===> ... surface downward fluxes
!
      do i=1,imax
        dwsfcb(i) = ttb(i,l)
        dwsfcd(i) = dwflux(i,lp1)-dwsfcb(i)
      enddo
!
      return
      end
