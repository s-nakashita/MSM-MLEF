      subroutine lwr88(heatra,grnflx,topflx,                            &
     &                 press,temp,rh2o,qo3,cldfac,                      &
     &                 camt,nclds,ktop,kbtm                             &
!
     &,                l, lp1, lp1v, llp1, imax                         &
!
     &,                stemp, gtemp                                     &
     &,                cdtm51, co2m51, c2dm51, cdtm58, co2m58, c2dm58   &
     &,                cdt51,  co251,  c2d51,  cdt58,  co258,  c2d58    &
     &,                cdt31,  co231,  c2d31,  cdt38,  co238,  c2d38    &
     &,                cdt71,  co271,  c2d71,  cdt78,  co278,  c2d78    &
!
!    &,                co211,co218   ! not used!!!
!
     &,                source,dsrce)
! 
!fpp$ noconcur r
!     subroutine lwr88 computes temperature-corrected co2 transmission
!   functions and also computes the pressure grid and layer optical
!   paths.
!          inputs:                (module blocks)
!      cldfac                          cldcom
!      press,temp,rh2o,qo3             radisw
!      camt,nclds,ktop,kbtm            radisw
!      co251,co258,cdt51,cdt58         co2bd3
!      c2d51,c2d58,co2m51,co2m58       co2bd3
!      cdtm51,cdtm58,c2dm51,c2dm58     co2bd3
!      stemp,gtemp                     co2bd3
!      co231,co238,cdt31,cdt38         co2bd2
!      c2d31,c2d38                     co2bd2
!      co271,co278,cdt71,cdt78         co2bd4
!      c2d71,c2d78                     co2bd4
!      betinw                          bdwide
!          outputs:
!      heatra,grnflx,topflx            lwout
!          called by:
!      radmn or input routine of model
!          calls:
!      fst88
!
      use machine , only : kind_rad
      use hcon
      use rnddta
      implicit none
!
!   b0,b1,b2,b3 are coefficients used to correct for the use of 250k in
!   the planck function used in evaluating planck-weighted co2
!   transmission functions. (see ref. 4)
!
      real (kind=kind_rad) b0, b1, b2, b3
      parameter (b0=-.51926410e-4, b1=-.18113332e-3,                    &
     &           b2=-.10680132e-5, b3=-.67303519e-7)
!
      integer l, lp1, lp1v, llp1, imax
      integer nclds(imax), ktop(imax,lp1), kbtm(imax,lp1)
!
      real (kind=kind_rad) co251(lp1,lp1), co258(lp1,lp1)               &
     &,                     cdt51(lp1,lp1), cdt58(lp1,lp1)              &
     &,                     c2d51(lp1,lp1), c2d58(lp1,lp1)              &
     &,                     co2m51(l),      co2m58(l)                   &
     &,                     cdtm51(l),      cdtm58(l)                   &
     &,                     c2dm51(l),      c2dm58(l)                   &
     &,                     stemp(lp1),     gtemp(lp1)
!
      real (kind=kind_rad) co231(lp1), co238(lp1), cdt31(lp1)           &
     &,                     cdt38(lp1), c2d31(lp1), c2d38(lp1)
!
      real (kind=kind_rad) co271(lp1), co278(lp1), cdt71(lp1)           &
     &,                     cdt78(lp1), c2d71(lp1), c2d78(lp1)
!
      real (kind=kind_rad) source(28,nbly), dsrce(28,nbly)
!
      real (kind=kind_rad) press(imax,lp1),      temp(imax,lp1)         &
     &,                     rh2o(imax,l),         qo3(imax,l)           &
     &,                     cldfac(imax,lp1,lp1), camt(imax,lp1)        &
     &,                     heatra(imax,l),       grnflx(imax)          &
     &,                     topflx(imax),         delp2(imax,l)
!
      real (kind=kind_rad) qh2o(imax,l),       t(imax,lp1)              &
     &,                     p(imax,lp1),        delp(imax,l)            &
     &,                     co21(imax,lp1,lp1), co2nbl(imax,l)          &
     &,                     co2sp1(imax,lp1),   co2sp2(imax,lp1)        &
     &,                     var1(imax,l),       var2(imax,l)            &
     &,                     var3(imax,l),       var4(imax,l)            &
     &,                     toto3(imax,lp1),    tphio3(imax,lp1)        &
     &,                     totphi(imax,lp1),   totvo2(imax,lp1)        &
     &,                     emx1(imax),         emx2(imax)              &
     &,                     empl(imax,llp1),    cntval(imax,lp1)
!
      real (kind=kind_rad) co2r1(imax,lp1),    dco2d1(imax,lp1)         &
     &,                     d2cd21(imax,lp1),   d2cd22(imax,lp1)        &
     &,                     co2r2(imax,lp1),    dco2d2(imax,lp1)        &
     &,                     co2mr(imax,l),      co2md(imax,l)           &
     &,                     co2m2d(imax,l),     tdav(imax,lp1)          &
     &,                     tstdav(imax,lp1)  , vv(imax,l)              &
     &,                     vsum3(imax,lp1),    dift(imax,lp1)          &
     &,                     a1(imax),           a2(imax)                &
     &,                     tlsqu(imax,lp1)
!
!
      real (kind=kind_rad) texpsl, tem, vsum2, co2r, dco2dt, d2cdt2
      integer ll, lm1, lp2, k, i, kp, k1, kk
!
      ll  = llp1 - 1
      lm1 = l - 1
      lp2 = l + 2
!
!****compute flux pressures (p) and differences (delp2,delp)
!****compute flux level temperatures (t) and continuum temperature
!    corrections (texpsl)
!
      do k=2,l
        do i=1,imax
          p(i,k) = haf*(press(i,k-1)+press(i,k))
          t(i,k) = haf*(temp(i,k-1)+temp(i,k))
        enddo
      enddo
      do i=1,imax
        p(i,1)   = zero
        p(i,lp1) = press(i,lp1)
        t(i,1)   = temp(i,1)
        t(i,lp1) = temp(i,lp1)
      enddo
      do k=1,l
        do i=1,imax
          delp2(i,k) = p(i,k+1) - p(i,k)
          delp(i,k)  = one / delp2(i,k)
        enddo
      enddo
!****compute argument for cont.temp.coeff.
!    (this is 1800.(1./temp-1./296.))..then take exponential
!     do  i=1,imax*lp1
!       texpsl(i,1) = exp(h18e3/temp(i,1)-h6p08108)
!     enddo
!***compute optical paths for h2o and o3, using the diffusivity
!   approximation for the angular integration (1.66). obtain the
!   unweighted values(var1,var3) and the weighted values(var2,var4).
!   the quantities h3m4(.0003) and h3m3(.003) appearing in the var2 and
!   var4 expressions are the approximate voigt corrections for h2o and
!   o3,respectively.
!
      do k=1,l
        do i=1,imax
          qh2o(i,k) = rh2o(i,k)*diffctr
!
!---vv is the layer-mean pressure (in atm),which is not the same as
!   the level pressure (press)
!
          vv(i,k)   = haf*(p(i,k+1)+p(i,k))*p0inv
          var1(i,k) = delp2(i,k) * qh2o(i,k)*ginv
          var3(i,k) = delp2(i,k) * qo3(i,k)*diffctr*ginv
          var2(i,k) = var1(i,k)  * (vv(i,k)+h3m4)
          var4(i,k) = var3(i,k)  * (vv(i,k)+h3m3)
!
!  compute optical path for the h2o continuum, using roberts coeffs.
!  (betinw),and temp. correction (texpsl). the diffusivity factor
!  (which cancels out in this expression) is assumed to be 1.66. the
!  use of the diffusivity factor has been shown to be a significant
!  source of error in the continuum calcs.,but the time penalty of
!  an angular integration is severe.
!
          texpsl      = exp(h18e3/temp(i,k)-h6p08108)
          cntval(i,k) = texpsl*rh2o(i,k)*var2(i,k)*betinw/              &
     &                 (rh2o(i,k)+rath2omw)
        enddo
      enddo
!   compute summed optical paths for h2o,o3 and continuum
      do i=1,imax
        totphi(i,1) = zero
        toto3(i,1)  = zero
        tphio3(i,1) = zero
        totvo2(i,1) = zero
      enddo
      do k=2,lp1
        do i=1,imax
          totphi(i,k) = totphi(i,k-1) + var2(i,k-1)
          toto3(i,k)  = toto3(i,k-1)  + var3(i,k-1)
          tphio3(i,k) = tphio3(i,k-1) + var4(i,k-1)
          totvo2(i,k) = totvo2(i,k-1) + cntval(i,k-1)
        enddo
      enddo
!
!---emx1 is the additional pressure-scaled mass from press(l) to
!   p(l). it is used in nearby layer and emiss calculations.
!---emx2 is the additional pressure-scaled mass from press(l) to
!   p(lp1). it is used in calculations between flux levels l and lp1.
!
      do i=1,imax
        tem     = qh2o(i,l)*press(i,l)*gp0inv
        emx1(i) = tem * (press(i,l)-p(i,l))
        emx2(i) = tem * (p(i,lp1)-press(i,l))
      enddo
!---empl is the pressure scaled mass from p(k) to press(k) (index 2-lp1)
!   or to press(k+1) (index lp2-ll)
      do k=1,l
        do i=1,imax
          empl(i,k+1)=qh2o(i,k)*p(i,k+1)*(p(i,k+1)-press(i,k))*gp0inv
        enddo
      enddo
      do k=1,lm1
        kk = k + lp1
        k1 = k + 1
        do i=1,imax
          empl(i,kk)=qh2o(i,k1)*p(i,k1)*(press(i,k1)-p(i,k1))*gp0inv
        enddo
      enddo
      do i=1,imax
        empl(i,1)    = var2(i,l)
        empl(i,llp1) = empl(i,ll)
      enddo
!***compute weighted temperature (tdav) and pressure (tstdav) integrals
!   for use in obtaining temp. difference bet. sounding and std.
!   temp. sounding (dift)
      do i=1,imax
        tstdav(i,1) = zero
        tdav(i,1)   = zero
      enddo
      do k=1,lp1
        do i=1,imax
          vsum3(i,k) = temp(i,k) - stemp(k)
        enddo
      enddo
      do k=1,l
        do i=1,imax
          vsum2         = gtemp(k)    * delp2(i,k)
          tstdav(i,k+1) = tstdav(i,k) + vsum2   
          tdav(i,k+1)   = tdav(i,k)   + vsum2 * vsum3(i,k)
        enddo
      enddo
!
!****evaluate coefficients for co2 pressure interpolation (a1,a2)
      tem = 1.0 / p0xzp2
      do i=1,imax
        a1(i) = (press(i,lp1)-p0xzp8)*tem
        a2(i) = (p0-press(i,lp1))*tem
      enddo
!
!***perform co2 pressure interpolation on all inputted transmission
!   functions and temp. derivatives
!---successively computing co2r,dco2dt and d2cdt2 is done to save
!   storage (at a slight loss in computation time)
      do k=1,lp1
        do i=1,imax
          co2r1(i,k)  =       a1(i)*co231(k)+a2(i)*co238(k)
          d2cd21(i,k) = h1m3*(a1(i)*c2d31(k)+a2(i)*c2d38(k))
          dco2d1(i,k) = h1m2*(a1(i)*cdt31(k)+a2(i)*cdt38(k))
          co2r2(i,k)  =       a1(i)*co271(k)+a2(i)*co278(k)
          d2cd22(i,k) = h1m3*(a1(i)*c2d71(k)+a2(i)*c2d78(k))
          dco2d2(i,k) = h1m2*(a1(i)*cdt71(k)+a2(i)*cdt78(k))
        enddo
      enddo
      do k=1,l
        do i=1,imax
          co2mr(i,k)  =       a1(i)*co2m51(k)+a2(i)*co2m58(k)
          co2md(i,k)  = h1m2*(a1(i)*cdtm51(k)+a2(i)*cdtm58(k))
          co2m2d(i,k) = h1m3*(a1(i)*c2dm51(k)+a2(i)*c2dm58(k))
        enddo
      enddo
!
!***compute co2 temperature interpolations for all bands,using dift
!
!   the case where k=1 is handled first. we are now replacing
!   3-dimensional arrays by 2-d arrays, to save space. thus this
!   calculation is for (i,kp,1)
!
      do kp=2,lp1
        do i=1,imax
          dift(i,kp) = tdav(i,kp) / tstdav(i,kp)
        enddo
      enddo
      do i=1,imax
        co21(i,1,1) = 1.0
        co2sp1(i,1) = 1.0
        co2sp2(i,1) = 1.0
      enddo
      do kp=2,lp1
        do i=1,imax
!---calculations for kp>1 for k=1
          co2r         =       a1(i)*co251(kp,1)+a2(i)*co258(kp,1)
          dco2dt       = h1m2*(a1(i)*cdt51(kp,1)+a2(i)*cdt58(kp,1))
          d2cdt2       = h1m3*(a1(i)*c2d51(kp,1)+a2(i)*c2d58(kp,1))
          co21(i,kp,1) = co2r+dift(i,kp)*(dco2dt+haf*dift(i,kp)*d2cdt2)
!---calculations for (effectively) kp=1,k>kp. these use the
!   same value of dift due to symmetry
          co2r         =       a1(i)*co251(1,kp)+a2(i)*co258(1,kp)
          dco2dt       = h1m2*(a1(i)*cdt51(1,kp)+a2(i)*cdt58(1,kp))
          d2cdt2       = h1m3*(a1(i)*c2d51(1,kp)+a2(i)*c2d58(1,kp))
          co21(i,1,kp) = co2r+dift(i,kp)*(dco2dt+haf*dift(i,kp)*d2cdt2)
        enddo
      enddo
!
!   the transmission functions used in spa88 may be computed now.
!---(in the 250 loop,dift really should be (i,1,k), but dift is
!    invariant with respect to k,kp,and so (i,1,k)=(i,k,1))
      do k=2,lp1
        do i=1,imax
          co2sp1(i,k) = co2r1(i,k)+dift(i,k)*(dco2d1(i,k)+haf*dift(i,k)*&
     &                                                     d2cd21(i,k))
          co2sp2(i,k) = co2r2(i,k)+dift(i,k)*(dco2d2(i,k)+haf*dift(i,k)*&
     &                                                     d2cd22(i,k))
        enddo
      enddo
!
!   next the case when k=2...l
      do k=2,l
        do kp=k+1,lp1
          do i=1,imax
           dift(i,kp)   = (tdav(i,kp)-tdav(i,k))/                       &
     &                    (tstdav(i,kp)-tstdav(i,k))
!
           co2r         =       a1(i)*co251(kp,k)+a2(i)*co258(kp,k)
           dco2dt       = h1m2*(a1(i)*cdt51(kp,k)+a2(i)*cdt58(kp,k))
           d2cdt2       = h1m3*(a1(i)*c2d51(kp,k)+a2(i)*c2d58(kp,k))
           co21(i,kp,k) = co2r+dift(i,kp)*(dco2dt+haf*dift(i,kp)*d2cdt2)
!
           co2r         =       a1(i)*co251(k,kp)+a2(i)*co258(k,kp)
           dco2dt       = h1m2*(a1(i)*cdt51(k,kp)+a2(i)*cdt58(k,kp))
           d2cdt2       = h1m3*(a1(i)*c2d51(k,kp)+a2(i)*c2d58(k,kp))
           co21(i,k,kp) = co2r+dift(i,kp)*(dco2dt+haf*dift(i,kp)*d2cdt2)
          enddo
        enddo
      enddo
!   finally the case when k=kp,k=2..lp1
      do k=2,lp1
        do i=1,imax
          dift(i,k)   = haf*(vsum3(i,k)+vsum3(i,k-1))
          co2r        =       a1(i)*co251(k,k)+a2(i)*co258(k,k)
          dco2dt      = h1m2*(a1(i)*cdt51(k,k)+a2(i)*cdt58(k,k))
          d2cdt2      = h1m3*(a1(i)*c2d51(k,k)+a2(i)*c2d58(k,k))
          co21(i,k,k) = co2r+dift(i,k)*(dco2dt+haf*dift(i,k)*d2cdt2)
        enddo
      enddo
!--- we are not doing nbl tfs on the 100 cm-1 bands .
      do k=1,l
        do i=1,imax
          co2nbl(i,k) = co2mr(i,k) + vsum3(i,k) *                      &
     &                  (co2md(i,k)+haf*vsum3(i,k)*co2m2d(i,k))
        enddo
      enddo
!***compute temp. coefficient based on t(k) (see ref.2)
      do k=1,lp1
        do i=1,imax
          if (t(i,k).le.h25e2) then
            tem        = t(i,k) - h25e2
            tlsqu(i,k) = b0 + tem * (b1 + tem * (b2 + b3*tem))
          else
            tlsqu(i,k) = b0
          endif
        enddo
      enddo
!***apply to all co2 tfs
      do k=1,lp1
        do kp=1,lp1
          do i=1,imax
            co21(i,kp,k) = co21(i,kp,k)*(one-tlsqu(i,kp)) + tlsqu(i,kp)
          enddo
        enddo
        do i=1,imax
          co2sp1(i,k) = co2sp1(i,k)*(one-tlsqu(i,1)) + tlsqu(i,1)
          co2sp2(i,k) = co2sp2(i,k)*(one-tlsqu(i,1)) + tlsqu(i,1)
        enddo
      enddo
      do k=1,l
        do i=1,imax
          co2nbl(i,k) = co2nbl(i,k)*(one-tlsqu(i,k))+tlsqu(i,k)
        enddo
      enddo
      call fst88(heatra,grnflx,topflx,                                  &
     &           qh2o,press,p,delp,delp2,temp,t,                        &
     &           cldfac,                                                &
!    &           cldfac,nclds,ktop,kbtm,camt,                           &
     &           co21,co2nbl,co2sp1,co2sp2,                             &
     &           var1,var2,var3,var4,cntval,                            &
     &           toto3,tphio3,totphi,totvo2,                            &
     &           emx1,emx2,empl                                         &
     &,          l, lp1, lp1v, llp1, imax                               &
     &,          source,dsrce)
      return
      end
