!fpp$ noconcur r                                                        
!     subroutine gwdps(im,ix,iy,km,a,b,u1,v1,t1,q1,pstar,kpbl,
      subroutine gwdps(im,ix,iy,km,a,b,u1,v1,t1,q1,kpbl,                &
     &                 prsi,del,prsl,prslk,phil,rcl,deltim,kdt,         &
     &                 hprime,oc,oa4,clx4,dusfc,dvsfc,                  &
     &                 g, cp, rd, rv, imx,lprnt)
!
!   ********************************************************************
! ----->  i m p l e m e n t a t i o n    v e r s i o n   <----------
!
!          --- not in this code --  history of gwdp at ncep----
!              ----------------     -----------------------
!  version 3  modified for gravity waves, location: .fr30(v3gwd)  *j*
!---       3.1 includes variable saturation flux profile cf isigst
!---       3.g includes ps combined w/ ph (glas and gfdl)
!-----         also included is ri  smooth over a thick lower layer
!-----         also included is decrease in de-acc at top by 1/2
!-----     the nmc gwd incorporating both glas(p&s) and gfdl(migwd)
!-----        mountain induced gravity wave drag 
!-----    code from .fr30(v3monnx) for monin3
!-----        this version (06 mar 1987)
!-----        this version (26 apr 1987)    3.g
!-----        this version (01 may 1987)    3.9
!-----    change to fortran 77 (feb 1989)     --- hann-ming henry juang
!----- 
!
!   version 4
!                ----- this code -----
!
!-----   modified to implement the enhanced low tropospheric gravity
!-----   wave drag developed by kim and arakawa(jas, 1995).
!        orographic variance (hprime), convexity (oc), asymmetry (oa4)
!        and lx (clx4) are input topographic statistics needed.
!
!-----   programmed and debugged by hong, alpert and kim --- jan 1996.
!-----   debugged again - moorthi and iredell --- may 1998.
!-----
!       further cleanup, optimization and modification
!                                       - s. moorthi may 98, march 99.
!-----   modified for usgs orography data (ncep office note 424)
!        and with several bugs fixed  - moorthi and hong --- july 1999.
!
!-----   modified & implemented into nrl nogaps
!                                       - young-joon kim, july 2000
!-----
!----------------------------------------------------------------------c
!    use
!        routine is called from gbphys  (after call to monnin)
!
!    purpose
!        using the gwd parameterizations of ps-glas and ph-
!        gfdl technique.  the time tendencies of u v
!        are altered to include the effect of mountain induced
!        gravity wave drag from sub-grid scale orography including
!        convective breaking, shear breaking and the presence of
!        critical levels
!
!  input
!        a(iy,km)  non-lin tendency for v wind component
!        b(iy,km)  non-lin tendency for u wind component
!        u1(ix,km) zonal wind / sqrt(rcl)  m/sec  at t0-dt
!        v1(ix,km) meridional wind / sqrt(rcl) m/sec at t0-dt
!        t1(ix,km) temperature deg k at t0-dt
!        q1(ix,km) specific humidity at t0-dt
!
!        rcl     a scaling factor = reciprocal of square of cos(lat)
!                for mrf gsm.  rcl=1 if u1 and v1 are wind components.
!        deltim  time step    secs
!        si(n)   p/psfc at base of layer n
!        sl(n)   p/psfc at middle of layer n
!        del(n)  positive increment of p/psfc across layer n
!        kpbl(ix) is the index of the top layer of the pbl
!
!  output
!        a, b    as augmented by tendency due to gwdps
!                other input variables unmodified.
!   ********************************************************************
      use machine , only : kind_phys
!     use machine_rad , only : kind_phys
      implicit none
      integer im, iy, ix, km, imx, lat, kdt
      integer kpbl(ix)                 ! index for the pbl top layer!
      real(kind=kind_phys) deltim, g, cp, rd, rv
!     real(kind=kind_phys) a(iy,km),    b(iy,km),      pstar(ix)
      real(kind=kind_phys) a(iy,km),    b(iy,km),                       &
     &                     u1(ix,km),   v1(ix,km),     t1(ix,km),       &
     &                     q1(ix,km),   prsi(ix,km+1), del(ix,km),      &
     &                     prsl(ix,km), prslk(ix,km),  phil(ix,km)
      real(kind=kind_phys) oc(ix),     oa4(iy,4), clx4(iy,4)            &
     &,                    hprime(ix), rcl(ix)
!
!     some constants
!
      real(kind=kind_phys) pi, dw2min, rimin, ric, bnv2min, efmin       &
     &,                    efmax,hpmax
      parameter (pi=3.1415926535897931)
      parameter (dw2min=1., rimin=-100., ric=0.25, bnv2min=1.0e-5)
      parameter (efmin=0.0, efmax=10.0, hpmax=200.0)
!     parameter (efmin=0.0, efmax=10.0, hpmax=2500.0)
!
      real(kind=kind_phys) frc,    ce,     ceofrc, frmax, cg, gmax      &
     &,                    critac, veleps, factop, rlolev, rdi
      parameter (frc=1.0, ce=0.8, ceofrc=ce/frc, frmax=100., cg=0.5)
      parameter (gmax=1.0, critac=5.0e-4, veleps=1.0, factop=0.5)
      parameter (rlolev=500.0) 
!     parameter (rlolev=0.5)
!
      real(kind=kind_phys) dpmin
!     parameter (dpmin=00.0)     ! minimum thickness of the reference layer
      parameter (dpmin=05.0)     ! minimum thickness of the reference layer
!     parameter (dpmin=20.0)     ! minimum thickness of the reference layer
                                 ! in centibars
!
      real(kind=kind_phys) fdir
      integer mdir
      parameter(mdir=8, fdir=mdir/(pi+pi))
      integer nwdir(mdir)
      data nwdir/6,7,5,8,2,3,1,4/
      save nwdir
!
      logical icrilv(ix)
!
!----   mountain induced gravity wave drag
!
      real(kind=kind_phys) taub(ix),  xn(ix),     yn(ix),    ubar(ix)    &
     &,                    vbar(ix),  ulow(ix),   oa(ix),    clx(ix)     &
     &,                    roll(ix),  uloi(ix),   dusfc(ix), dvsfc(ix)   &
     &,                    dtfac(ix), xlinv(ix),  delks(ix), delks1(ix)
!
      real(kind=kind_phys) bnv2(ix,km),  taup(ix,km+1), ri_n(ix,km)      &
     &,                    taud(ix,km),  ro(ix,km),     vtk(ix,km)       &
     &,                    vtj(ix,km),   scor(ix),      velco(ix,km-1)   &
     &,                    bnv2bar(ix),                 rcs(ix)
!
      real(kind=kind_phys) velko(km-1)
      integer   kref(ix), kint(ix), iwk(ix), ipt(ix)
!
      real(kind=kind_phys) gor,    gocp,  fv,    xl,    gr2,  bnv,  fr   &
     &,                    brvf,   cleff, tem,   tem1,  tem2, temc, temv &
     &,                    wdir,   ti,    rdz,   dw2,   shr2, bvf2       &
     &,                    rdelks, wtkbj, efact, coefm, gfobnv           &
     &,                    scork,  rscor, hd,    fro,   rim,  sira       &
     &,                    dtaux,  dtauy, rcsks
      integer ncnt, kmm1, kmm2, lcap, lcapp1, kbps, kbpsp1,kbpsm1        &
     &,       kmps, kmpsp1, idir, nwd, i, j, k, klcap, kp1, kmpbl, npt
      logical lprnt
!
      do i = 1,im
         dusfc(i) = 0.
         dvsfc(i) = 0.
      enddo
!
      npt     = 0
      do i = 1,im
        if (hprime(i) .gt. 0.0001) then
           npt      = npt + 1
           ipt(npt) = i
        endif
      enddo
!
      if (npt .eq. 0) return     ! no gwd calculation done!
!
!---- constants
!
      rdi  = 1.0 / rd
      gor  = g/rd
      gr2  = g*gor
      gocp = g/cp
      fv   = rv/rd - 1
!
!     ncnt   = 0
      kmm1   = km - 1
      kmm2   = km - 2
      lcap   = km
      lcapp1 = lcap + 1
      kmpbl  = km / 2 ! maximum pbl height : no. of vertical levels / 2
!
!  scale cleff between im=384*2 and 192*2 for t126/t170 and t62
!
      if (imx .gt. 0) then
!       cleff = 1.0e-5 * sqrt(float(imx)/384.0) !  this is inverse of cleff!
!       cleff = 1.0e-5 * sqrt(float(imx)/192.0) !  this is inverse of cleff!
        cleff = 0.5e-5 * sqrt(float(imx)/192.0) !  this is inverse of cleff!
!       cleff = 2.0e-5 * sqrt(float(imx)/192.0) !  this is inverse of cleff!
!       cleff = 2.5e-5 * sqrt(float(imx)/192.0) !  this is inverse of cleff!
      endif
!
      do i=1,npt
        rcs(i) = sqrt(rcl(ipt(i)))
      enddo
!
      do k = 1,km
        do i =1,npt
          j         = ipt(i)
          vtj(i,k)  = t1(j,k)  * (1.+fv*q1(j,k))
          vtk(i,k)  = vtj(i,k) / prslk(j,k)
          ro(i,k)   = rdi * prsl(j,k) / vtj(i,k) ! density tons/m**3
          taup(i,k) = 0.0
        enddo
      enddo
      do k = 1,kmm1
        do i =1,npt
          j         = ipt(i)
          ti        = 2.0 / (t1(j,k)+t1(j,k+1))
          tem       = ti  / (prsl(j,k)-prsl(j,k+1))
!         rdz       = gor * prsi(j,k+1) * tem
          rdz       = g   / (phil(j,k+1) - phil(j,k))
          tem1      = u1(j,k) - u1(j,k+1)
          tem2      = v1(j,k) - v1(j,k+1)
          dw2       = rcl(j)*(tem1*tem1 + tem2*tem2)
          shr2      = max(dw2,dw2min) * rdz * rdz
          bvf2      = g*(gocp+rdz*(vtj(i,k+1)-vtj(i,k))) * ti
          ri_n(i,k) = max(bvf2/shr2,rimin)   ! richardson number
!                                              brunt-vaisala frequency
!         tem       = gr2 * (prsl(j,k)+prsl(j,k+1)) * tem
!         bnv2(i,k) = tem * (vtk(i,k+1)-vtk(i,k))/(vtk(i,k+1)+vtk(i,k))
          bnv2(i,k) = (g+g) * rdz * (vtk(i,k+1)-vtk(i,k))               &
     &                            / (vtk(i,k+1)+vtk(i,k))
          bnv2(i,k) = max( bnv2(i,k), bnv2min )
        enddo
      enddo
!
!     apply 3 point smoothing on bnv2
!
!     do k=1,km
!       do i=1,im
!         vtk(i,k) = bnv2(i,k)
!       enddo
!     enddo
!     do k=2,kmm1
!       do i=1,im
!         bnv2(i,k) = 0.25*(vtk(i,k-1)+vtk(i,k+1)) + 0.5*vtk(i,k)
!       enddo
!     enddo
!
!     finding the first interface index above 50 hpa level
!
      do i=1,npt
        iwk(i) = 2
      enddo
      do k=3,kmpbl
        do i=1,npt
          j   = ipt(i)
          tem = (prsi(j,1) - prsi(j,k))
          if (tem .lt. dpmin) iwk(i) = k
        enddo
      enddo
!
      kbps = 1
      kmps = km
      do i=1,npt
        j         = ipt(i)
        kref(i)   = max(iwk(i), kpbl(j)+1)              ! reference level 
        delks(i)  = 1.0 / (prsi(j,1) - prsi(j,kref(i)))
        delks1(i) = 1.0 / (prsl(j,1) - prsl(j,kref(i)))
        ubar (i)  = 0.0
        vbar (i)  = 0.0
        roll (i)  = 0.0
        kbps      = max(kbps,  kref(i))
        kmps      = min(kmps,  kref(i))
!
        bnv2bar(i) = (prsl(j,1)-prsl(j,2)) * delks1(i) * bnv2(i,1)
      enddo
      kbpsp1 = kbps + 1
      kbpsm1 = kbps - 1
      do k = 1,kbps
        do i = 1,npt
          if (k .lt. kref(i)) then
            j          = ipt(i)
            rdelks     = del(j,k) * delks(i)
            rcsks      = rcs(i)   * rdelks
            ubar(i)    = ubar(i)  + rcsks  * u1(j,k)   ! mean u below kref
            vbar(i)    = vbar(i)  + rcsks  * v1(j,k)   ! mean v below kref
!
            roll(i)    = roll(i)  + rdelks * ro(i,k)   ! mean ro below kref
            rdelks     = (prsl(j,k)-prsl(j,k+1)) * delks1(i)
            bnv2bar(i) = bnv2bar(i) + bnv2(i,k) * rdelks
          endif
        enddo
      enddo
!
!     figure out low-level horizontal wind direction and find 'oa'
!
!             nwd  1   2   3   4   5   6   7   8
!              wd  w   s  sw  nw   e   n  ne  se
!
      do i = 1,npt
        j      = ipt(i)
        wdir   = atan2(ubar(i),vbar(i)) + pi
        idir   = mod(nint(fdir*wdir),mdir) + 1
        nwd    = nwdir(idir)
        oa(i)  = (1-2*int( (nwd-1)/4 )) * oa4(j,mod(nwd-1,4)+1)
        clx(i) = clx4(j,mod(nwd-1,4)+1)
      enddo
!
!-----xn,yn            "low-level" wind projections in zonal
!                                    & meridional directions
!-----ulow             "low-level" wind magnitude -        (= u)
!-----bnv2             bnv2 = n**2
!-----taub             base momentum flux
!-----= -(ro * u**3/(n*xl)*gf(fr) for n**2 > 0
!-----= 0.                        for n**2 < 0
!-----fr               froude    =   n*hprime / u
!-----g                gmax*fr**2/(fr**2+cg/oc)
!
!-----initialize some arrays
!
      do i = 1,npt
        xn(i)     = 0.0
        yn(i)     = 0.0
        taub (i)  = 0.0
        ulow (i)  = 0.0
        dtfac(i)  = 1.0
        icrilv(i) = .false. ! initialize critical level control vector
!
!----compute the "low level" wind magnitude (m/s)
!
        ulow(i) = max(sqrt(ubar(i)*ubar(i) + vbar(i)*vbar(i)), 1.0)
        uloi(i) = 1.0 / ulow(i)
      enddo
!
      do  k = 1,kmm1
        do  i = 1,npt
          j            = ipt(i)
          velco(i,k)   = (0.5*rcs(i)) * ((u1(j,k)+u1(j,k+1))*ubar(i)    &
     &                                +  (v1(j,k)+v1(j,k+1))*vbar(i))
          velco(i,k)   = velco(i,k) * uloi(i)
!         if ((velco(i,k).lt.veleps) .and. (velco(i,k).gt.0.)) then
!           velco(i,k) = veleps
!         endif
        enddo
      enddo
!
!   find the interface level of the projected wind where
!   low levels & upper levels meet above pbl
!
      do i=1,npt
        kint(i) = km
      enddo
      do k = 1,kmm1
        do i = 1,npt
          if (k .gt. kref(i)) then
            if(velco(i,k) .lt. veleps .and. kint(i) .eq. km) then
              kint(i) = k+1
            endif
          endif
        enddo
      enddo
!  warning  kint = kref !!!!!!!!!
      do i=1,npt
        kint(i) = kref(i)
      enddo
!
!     if(lprnt) print *,' ubar=',ubar                                   &
!    &,' vbar=',vbar,' ulow=',ulow,' veleps=',veleps
!
      do i = 1,npt
        j      = ipt(i)
        bnv    = sqrt( bnv2bar(i) )
        fr     = bnv     * uloi(i) * min(hprime(j),hpmax)
        fr     = min(fr, frmax)
        xn(i)  = ubar(i) * uloi(i)
        yn(i)  = vbar(i) * uloi(i)
!
!     compute the base level stress and store it in taub
!     calculate enhancement factor, number of mountains & aspect
!     ratio const. use simplified relationship between standard
!     deviation & critical hgt
!
        efact    = (oa(i) + 2.) ** (ceofrc*fr)
        efact    = min( max(efact,efmin), efmax )
!
        coefm    = (1. + clx(i)) ** (oa(i)+1.)
!
        xlinv(i) = coefm * cleff
!
        tem      = fr    * fr * oc(j)
        gfobnv   = gmax  * tem / ((tem + cg)*bnv)  ! g/n0
!
        taub(i)  = xlinv(i) * roll(i) * ulow(i) * ulow(i)               &
     &           * ulow(i)  * gfobnv  * efact         ! base flux tau0
!
!         tem      = min(hprime(i),hpmax)
!         taub(i)  = xlinv(i) * roll(i) * ulow(i) * bnv * tem * tem
!
        k        = max(1, kref(i)-1)
        tem      = max(velco(i,k)*velco(i,k), 0.0001)
        scor(i)  = bnv2(i,k) / tem  ! scorer parameter below ref level
      enddo
!     if(lprnt) print *,' taub=',taub
!                                                                       
!----set up bottom values of stress
!
      do k = 1, kbps
        do i = 1,npt
          if (k .le. kref(i)) taup(i,k) = taub(i)
        enddo
      enddo
!
!   now compute vertical structure of the stress.
!
      do k = kmps, kmm1                   ! vertical level k loop!
        kp1 = k + 1
        do i = 1, npt
!
!-----unstable layer if ri < ric
!-----unstable layer if upper air vel comp along surf vel <=0 (crit lay)
!---- at (u-c)=0. crit layer exists and bit vector should be set (.le.)
!
          if (k .ge. kref(i)) then
            icrilv(i) = icrilv(i) .or. ( ri_n(i,k) .lt. ric)            &
     &                            .or. (velco(i,k) .le. 0.0)
          endif
        enddo
!
        do i = 1,npt
          if (k .ge. kref(i))   then
            if (.not.icrilv(i) .and. taup(i,k) .gt. 0.0 ) then
              temv = 1.0 / max(velco(i,k), 0.01)
!             if (oa(i) .gt. 0. .and.  prsi(ipt(i),kp1).gt.rlolev) then
              if (oa(i).gt.0. .and. kp1 .lt. kint(i)) then
                scork   = bnv2(i,k) * temv * temv
                rscor   = min(1.0, scork / scor(i))
                scor(i) = scork
              else 
                rscor   = 1.
              endif
!
              brvf = sqrt(bnv2(i,k))        ! brunt-vaisala frequency
!             tem1 = xlinv(i)*(ro(i,kp1)+ro(i,k))*brvf*velco(i,k)*0.5
              tem1 = xlinv(i)*(ro(i,kp1)+ro(i,k))*brvf*0.5              &
     &                       * max(velco(i,k),0.01)
              hd   = sqrt(taup(i,k) / tem1)
              fro  = brvf * hd * temv
!
!    rim is the  minimum-richardson number by shutts (1985)
!
              tem2   = sqrt(ri_n(i,k))
              tem    = 1. + tem2 * fro
              rim    = ri_n(i,k) * (1.-fro) / (tem * tem)
!
!    check stability to employ the 'saturation hypothesis'
!    of lindzen (1981) except at tropospheric downstream regions
!
!                                       ----------------------
              if (rim .le. ric .and.                                    &
!    &           (oa(i) .le. 0. .or.  prsi(ipt(i),kp1).le.rlolev )) then
     &           (oa(i) .le. 0. .or.  kp1 .ge. kint(i) )) then 
                 temc = 2.0 + 1.0 / tem2
                 hd   = velco(i,k) * (2.*sqrt(temc)-temc) / brvf
                 taup(i,kp1) = tem1 * hd * hd
              else 
                 taup(i,kp1) = taup(i,k) * rscor
              endif
              taup(i,kp1) = min(taup(i,kp1), taup(i,k))
            endif
          endif
        enddo
      enddo
!
!     do i=1,im
!       taup(i,km+1) = taup(i,km)
!     enddo
!
      if(lcap .le. km) then
         do klcap = lcapp1, km+1
            do i = 1,npt
              sira          = prsi(ipt(i),klcap) / prsi(ipt(i),lcap)
              taup(i,klcap) = sira * taup(i,lcap)
            enddo
         enddo
      endif
!
!     calculate - (g/pstar)*d(tau)/d(sigma) and deceleration terms dtaux, dtauy
!
      do i=1,npt
!       scor(i) = 1.0 / (rcs(i) * pstar(i))
        scor(i) = 1.0 / rcs(i)
      enddo
      do k = 1,km
        do i = 1,npt
          taud(i,k) = g * (taup(i,k+1) - taup(i,k)) * scor(i)           &
     &                                              / del(ipt(i),k)
        enddo
      enddo
!
!------limit de-acceleration (momentum deposition ) at top to 1/2 value
!------the idea is some stuff must go out the 'top'
!
      do klcap = lcap, km
         do i = 1,npt
            taud(i,klcap) = taud(i,klcap) * factop
         enddo
      enddo
!
!------if the gravity wave drag would force a critical line in the
!------layers below sigma=rlolev during the next deltim timestep,
!------then only apply drag until that critical line is reached.
!
      do k = 1,kmm1
        do i = 1,npt
           if (k .gt. kref(i) .and. prsi(ipt(i),k) .ge. rlolev) then
             if(taud(i,k).ne.0.) then
               tem = deltim *  rcs(i) * taud(i,k)
               dtfac(i) = min(dtfac(i),abs(velco(i,k)/tem))
             endif
           endif
        enddo
      enddo
!
      do k = 1,km
        do i = 1,npt
          j          = ipt(i)
          taud(i,k)  = taud(i,k) * dtfac(i)
          dtaux      = taud(i,k) * xn(i)
          dtauy      = taud(i,k) * yn(i)
!
          a(j,k)     = dtauy     + a(j,k)
          b(j,k)     = dtaux     + b(j,k)
          dusfc(j)   = dusfc(j)  + dtaux * del(j,k)
          dvsfc(j)   = dvsfc(j)  + dtauy * del(j,k)
        enddo
      enddo
      do i = 1,npt
        j          = ipt(i)
!       tem    = (-1.e3/g) * rcs(i) * pstar(i)
        tem    = (-1.e3/g) * rcs(i)
        dusfc(j) = tem * dusfc(j)
        dvsfc(j) = tem * dvsfc(j)
      enddo
!                                                                       
!    monitor for excessive gravity wave drag tendencies if ncnt>0
!
!     if(ncnt.gt.0) then
!        if(lat.ge.38.and.lat.le.42) then
!cmic$ guard 37
!           do 92 i = 1,im
!              if(ikount.gt.ncnt) go to 92
!              if(i.lt.319.or.i.gt.320) go to 92
!              do 91 k = 1,km
!                 if(abs(rcs*taud(i,k)) .gt. critac) then
!                    if(i.le.im) then
!                       ikount = ikount+1
!                       print 123,i,lat,kdt
!                       print 124,taub(i),bnv(i),ulow(i),
!    1                  gf(i),fr(i),roll(i),hprime(i),xn(i),yn(i)
!                       print 124,(taud(i,kk),kk = 1,km)
!                       print 124,(taup(i,kk),kk = 1,km+1)
!                       print 124,(ri_n(i,kk),kk = 1,km)
!                       do 93 kk = 1,kmm1
!                          velko(kk) =
!    1                  0.5*rcs*((u1(i,kk)+u1(i,kk+1))*ubar(i)+
!    2                  (v1(i,kk)+v1(i,kk+1))*vbar(i))*uloi(i)
!93                     continue
!                       print 124,(velko(kk),kk = 1,kmm1)
!                       print 124,(a    (i,kk),kk = 1,km)
!                       print 124,(dtauy(i,kk),kk = 1,km)
!                       print 124,(b    (i,kk),kk = 1,km)
!                       print 124,(dtaux(i,kk),kk = 1,km)
!                       go to 92
!                    endif
!                 endif
!91            continue
!92         continue
!cmic$ end guard 37
!123        format('  *** migwd print *** i=',i3,' lat=',i3,' kdt=',i3)
!124        format(2x,  10e13.6)
!        endif
!     endif
!
      return
      end
