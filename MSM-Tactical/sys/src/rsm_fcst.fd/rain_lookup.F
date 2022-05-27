!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!-------------- creates lookup tables for rain processes ---------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
      subroutine rain_lookup
!
!--- parameters & arrays for fall speeds of rain as a function of rain drop
!      diameter.  these quantities are integrated over exponential size
!      distributions of rain drops at 1 micron intervals (ddelr) from minimum 
!      drop sizes of .05 mm (50 microns, dminr) to maximum drop sizes of 10 mm 
!      (dmaxr). 
!
      real, parameter :: dminr=.05e-3, dmaxr=10.e-3, ddelr=1.e-6,        &       
     & xrmin=1.e6*dminr, xrmax=1.e6*dmaxr
      integer, parameter :: idrmin=xrmin, idrmax=xrmax
      real diam(idrmin:idrmax), vel(idrmin:idrmax)
!
!--- parameters rain lookup tables, which establish the range of mean drop
!      diameters; from a minimum mean diameter of 0.05 mm (dmrmin) to a 
!      maximum mean diameter of 0.45 mm (dmrmax).  the tables store solutions
!      at 1 micron intervals (deldmr) of mean drop diameter.  
!
      real, parameter :: dmrmin=.05e-3, dmrmax=.45e-3, deldmr=1.e-6,      &
     & xmrmin=1.e6*dmrmin, xmrmax=1.e6*dmrmax
      integer, parameter :: mdrmin=xmrmin, mdrmax=xmrmax
      real mdiam, mass
!
!--- rain lookup tables for mean rain drop diameters from dmrmin to dmrmax,
!      assuming exponential size distributions for the rain drops
!
      common /rvent_tables/ ventr1(mdrmin:mdrmax), ventr2(mdrmin:mdrmax)
      common /raccr_tables/ accrr(mdrmin:mdrmax)
      common /rmass_tables/ massr(mdrmin:mdrmax)
      real massr
      common /rrate_tables/ rrate(mdrmin:mdrmax)
      common /rvelr_tables/ vrain(mdrmin:mdrmax)
!
      logical, parameter :: print_diag=.false.
!
!-----------------------------------------------------------------------
!------- fall speeds of rain as function of rain drop diameter ---------
!-----------------------------------------------------------------------
!
      do i=idrmin,idrmax
        diam(i)=float(i)*ddelr
        d=100.*diam(i)         ! diameter in cm
        if (d .le. .42) then
   !
   !--- rutledge & hobbs (1983); vel (m/s), d (cm)
   !
          vel(i)=max(0., -.267+51.5*d-102.25*d*d+75.7*d**3)
        else if (d.gt.0.42 .and. d.le..58) then
   !
   !--- linear interpolation of gunn & kinzer (1949) data
   !
          vel(i)=8.92+.25/(.58-.42)*(d-.42)
        else
          vel(i)=9.17
        endif
      enddo
      do i=1,100
        i1=(i-1)*100+idrmin
        i2=i1+90
   !
   !--- print out rain fall speeds only for d<=5.8 mm (.58 cm)
   !
        if (diam(i1) .gt. .58e-2) exit
        if (print_diag) then
          write(6,"(1x)")
          write(6,"('d(mm)->  ',10f7.3)") (1000.*diam(j), j=i1,i2,10)
          write(6,"('v(m/s)-> ',10f7.3)") (vel(j), j=i1,i2,10)
        endif
      enddo
!
!-----------------------------------------------------------------------
!------------------- lookup tables for rain processes ------------------
!-----------------------------------------------------------------------
!
      pi=acos(-1.)
      pi2=2.*pi
      cmass=1000.*pi/6.
      if (print_diag) then
        write(6,"(/'diam - mean diameter (mm)'                           & 
     &          /'ventr1 - 1st ventilation coefficient (m**2)'           &
     &          /'ventr2 - 2nd ventilation coefficient (m**3/s**.5)'     &
     &          /'accrr - accretion moment (m**4/s)'                     &
     &          /'rho*qr - mass content (kg/m**3) for n0r=8e6'           &
     &          /'rrate - rain rate moment (m**5/s)'                     &
     &          /'vr - mass-weighted rain fall speed (m/s)'              &
     &    /' diam      ventr1      ventr2       accrr      ',            &
     &    'rho*qr       rrate    vr')")
      endif
      do j=mdrmin,mdrmax
        mdiam=float(j)*deldmr
        ventr2(j)=0.
        accrr(j)=0.
        massr(j)=0.
        rrate(j)=0.
        do i=idrmin,idrmax
          expf=exp(-diam(i)/mdiam)*ddelr
          ventr2(j)=ventr2(j)+diam(i)**1.5*vel(i)**.5*expf
          accrr(j)=accrr(j)+diam(i)*diam(i)*vel(i)*expf
          massr(j)=massr(j)+diam(i)**3*expf
          rrate(j)=rrate(j)+diam(i)**3*vel(i)*expf
        enddo
   !
   !--- derived based on ventilation, f(d)=0.78+.31*schmidt**(1/3)*reynold**.5,
   !      where reynold=(v*d*rho/dyn_vis), v is velocity, d is particle diameter,
   !      rho is air density, & dyn_vis is dynamic viscosity.  only terms 
   !      containing velocity & diameter are retained in these tables.  
   !
        ventr1(j)=.78*pi2*mdiam**2
        ventr2(j)=.31*pi2*ventr2(j)
   !
        massr(j)=cmass*massr(j)
        rrate(j)=cmass*rrate(j)
        vrain(j)=rrate(j)/massr(j)
        if (print_diag) write(6,"(f5.3,5g12.5,f6.3))") 1000.*mdiam,      &    
     &    ventr1(j),ventr2(j),accrr(j),8.e6*massr(j),rrate(j),vrain(j)
      enddo
!
!-----------------------------------------------------------------------
!
      return
      end
