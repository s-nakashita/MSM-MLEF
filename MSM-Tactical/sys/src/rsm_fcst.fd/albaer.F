      subroutine albaer(slmsk,snowf,zorlf,coszf,tseaf,hprif,tgdf,        &   
     &                  alvsf,alnsf,alvwf,alnwf,facsf,facwf,pi,dltg,     &
     &                  kprfg,idxcg,cmixg,denng,xlat,xlon,               &
     &                  alvbr,alnbr,alvdr,alndr,kaer,kprf,idxc,          &
     &                  cmix,denn,nxc,ndn,imxae,jmxae,len,t0c)
!fpp$ noconcur r
!*******************************************************************
!   this program computes four components of surface albedos (i.e.
!     vis-nir, direct-diffused), from vis/nir strong/weak zenith angle
!          dependency, based on briegleb scheme. and
!     bilinearly interpolates albedo and aerosol distribution to
!     radiation grid....seasonal interpolation done in cycle..hlp feb98
!
!   input variables:
!     slmsk   - sea(0),land(1),ice(2) mask on fcst model grid
!     snowf   - snow depth water equivalent in mm
!     zorlf   - surface roughness in cm
!     coszf   - cosin of solar zenith angle
!     tseaf   - sea surface temperature in k
!     hprif   - topographic sdv in m
!     tgdf    - ground soil temperature in k
!     alvsf   - mean vis albedo with strong cosz dependency
!     alnsf   - mean nir albedo with strong cosz dependency
!     alvwf   - mean vis albedo with weak cosz dependency
!     alnwf   - mean nir albedo with weak cosz dependency
!     facsf   - fractional coverage with strong cosz dependency
!     facwf   - fractional coverage with weak cosz dependency
!     kprfg, idxcg, cmixg, denng
!             - global dist of tropospheric aerosols data:
!               profile type, component index, component mixing ratio,
!               and number density (for first and second layers)
!     xlon, xlat
!             - longitude and latitude of given points in radiance
!
!   output variables:   (all on radiation grid)
!     alvbr   - vis beam surface albedo
!     alnbr   - nir beam surface albedo
!     alvdr   - vis diff surface albedo
!     alndr   - nir diff surface albedo
!     kprf, idxc, cmix, denn
!             - aerosol data for the selected grid points
!
!   nov. 97  - modified snow albedo (use hprif, jsno)  - yh
!   jan 98   - to include grumbine seaice scheme
!              and use slmskr to setup two basic
!              types of aerosols (cont-i, mar-i)       - yh
!   mar 00   - modified to use opac aerosol data(1998) - yh
!
!******************************************************************
! --- input
!
      use machine , only : kind_rad
      implicit none
!
      integer len, imxae, jmxae, nxc, ndn, kaer
! --- input
      real (kind=kind_rad) slmsk(len), snowf(len), zorlf(len)            &    
     &,                     tseaf(len), coszf(len), hprif(len)           &
     &,                     alvsf(len), alnsf(len), alvwf(len)           &
     &,                     alnwf(len), facsf(len), facwf(len)           &
     &,                     tgdf(len),  pi,         dltg                 &
     &,                     cmixg(nxc,imxae,jmxae), xlon(len)            &
     &,                     denng(ndn,imxae,jmxae), xlat(len)
!
      integer idxcg(nxc,imxae,jmxae),kprfg(imxae,jmxae)
! --- output
      real (kind=kind_rad) alvbr(len), alnbr(len), alvdr(len)            &
     &,                     alndr(len), cmix(nxc,len), denn(ndn,len)     &
     &,                     temp1,      temp2     
      integer               idxc(nxc,len), kprf(len)
! ---  local variables
      real (kind=kind_rad) asnvb, asnnb, asnvd, asnnd, asevb             &
     &,                     asenb, asevd, asend, fsno,  fsea             &
     &,                     rfcs,  rfcw,  flnd                           &
!
     &,                     asnow, argh,  hrgh,  fsno0, fsno1            &
     &,                     flnd0, fsea0, dtsq,  dtgd,  csnow            &
     &,                     a1, a2, b1, b2, t0c                          &
     &,                     tmp1, tmp2, hdlt, rdg                        
      integer i,i1, i2, j1, j2, j3, k
!
      do i=1,len
!
! --- modified snow albedo scheme - units convert to m
!     (originally snowf in mm; zorlf in cm)
         asnow = 0.02*snowf(i)
         argh  = min(0.50, max(.025, 0.01*zorlf(i)))
         hrgh  = min(1.0, max(0.20, 1.0577-1.1538e-3*hprif(i) ) )
         fsno0 = asnow / (argh + asnow) * hrgh
         if (slmsk(i).eq.0.0 .and. tseaf(i).gt.271.2) fsno0 = 0.0
         fsno1 = 1.0 - fsno0
         flnd0 = min(1.0, facsf(i)+facwf(i))
         fsea0 = max(0.0, 1.0 - flnd0)
         fsno  = fsno0
         fsea  = fsea0 * fsno1
         flnd  = flnd0 * fsno1
! --- diffused sea surface albedo
         if (tseaf(i) .ge. 271.5) then
            asevd = 0.06
            asend = 0.06
         else if (tseaf(i) .lt. 271.1) then
            asevd = 0.70
            asend = 0.65
         else
            dtsq = (tseaf(i) - 271.1)**2
            asevd = 0.7 - 4.0*dtsq
            asend = 0.65 - 3.6875*dtsq
         end if
! --- diffused snow albedo
         if (slmsk(i) .eq. 2.0) then
!    (bob grumbine method for snow covered sea ice)
            dtgd  = max(0.0, min(5.0, 273.16-tgdf(i)) )
            asnvd = 0.70 + 0.03 * dtgd
            asnnd = 0.60 + 0.03 * dtgd
            asevd = 0.70
            asend = 0.65
         else
            asnvd = 0.90
            asnnd = 0.75
         end if
!
! --- direct snow albedo
         if (coszf(i) .lt. 0.5) then
            csnow = 0.5 * (3.0 / (1.0+4.0*coszf(i)) - 1.0)
            asnvb = min( 0.98, asnvd+(1.0-asnvd)*csnow )
            asnnb = min( 0.98, asnnd+(1.0-asnnd)*csnow )
         else
            asnvb = asnvd
            asnnb = asnnd
         end if
! --- direct sea surface albedo
         if (coszf(i) .gt.0.0) then
            rfcs = 1.4 / (1.0 + 0.8*coszf(i))
!error      rfcs = 1.4 / (1.0 + 0.4*coszf(i))
            rfcw = 1.1 / (1.0 + 0.2*coszf(i))
            if (tseaf(i) .ge. t0c) then
              asevb = max(asevd, 0.026/(coszf(i)**1.7+0.065)             &      
     &              + 0.15 * (coszf(i)-0.1) * (coszf(i)-0.5)             &
     &              * (coszf(i)-1.0))
              asenb = asevb
            else
              asevb = asevd
              asenb = asend
            end if
         else
            rfcs  = 1.0
            rfcw  = 1.0
            asevb = asevd
            asenb = asend
        end if
!
        a1   = alvsf(i) * facsf(i)
        b1   = alvwf(i) * facwf(i)
        a2   = alnsf(i) * facsf(i)
        b2   = alnwf(i) * facwf(i)
        alvbr(i) = (a1*rfcs + b1*rfcw)     *flnd                         &   
     &           + asevb*fsea + asnvb*fsno
        alvdr(i) = (a1        + b1        )*flnd                         &   
     &           + asevd*fsea + asnvd*fsno
        alnbr(i) = (a2*rfcs   + b2*rfcw)   *flnd                         &   
     &           + asenb*fsea + asnnb*fsno
        alndr(i) = (a2        + b2        )*flnd                         &   
     &           + asend*fsea + asnnd*fsno
!
      enddo
!
      if (kaer .gt. 0) then
!
!....    aerosol distributions
!yh0400   use opac aerosol data
        rdg = 180.0 / pi
        hdlt = 0.5 * dltg
!
        do i=1,len
          i2 = 1
          j2 = 1
          tmp1 = xlon(i) * rdg
          do i1=1,imxae
            tmp2 = dltg * (i1 - 1)
            if (tmp2 .gt. 360.0-hdlt) then
              tmp2 = tmp2 - 360.0
            end if
            if (abs(tmp1-tmp2) .le. hdlt) then
              i2 = i1
              go to 40
            end if
          end do
  40      tmp1 = xlat(i) * rdg
          do j1=1,jmxae
            tmp2 = 90.0 - dltg * (j1 - 1)
            if (abs(tmp1-tmp2) .le. hdlt) then
              j2 = j1
              go to 50
            end if
          end do
!
  50      kprf(i) = kprfg(i2,j2)
          do k=1,nxc
            idxc(k,i) = idxcg(k,i2,j2)
            cmix(k,i) = cmixg(k,i2,j2)
          end do
          do k=1,ndn
            denn(k,i) = denng(k,i2,j2)
          end do
        enddo
      endif
!
      return
      end
