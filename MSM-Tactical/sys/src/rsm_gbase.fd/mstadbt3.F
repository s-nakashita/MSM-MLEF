!-----------------------------------------------------------------------
      subroutine mstadbt3(im,km,k1,k2,prsl,prslk,tenv,qenv,              &   
     &                  klcl,kbot,ktop,tcld,qcld)
!$$$     subprogram documentation block
!
! subprogram: mstadb       compute moist adiabatic cloud soundings
!
!   author: n phillips    date: nov 1983
!
! abstract: atmospheric columns of temperature and specific humidity
!   are examined by this routine for conditional instability.
!   the test parcel is chosen from the layer between layers k1 and k2
!   that has the warmest potential wet-bulb temperature.
!   excess cloud temperatures and specific humidities are returned
!   where the lifted parcel is found to be buoyant.
!   fast inlinable functions are invoked to compute
!   dewpoint and lifting condensation level temperatures,
!   equivalent potential temperature at the lcl, and
!   temperature and specific humidity of the ascending parcel.
!
! program history log:
!   83-11     phillips
!   91-05-07  iredell             arguments changed, code tidied
!
! usage:    call mstadb(im,km,k1,k2,sl,slk,ps,tenv,qenv,
!    &                  klcl,kbot,ktop,tcld,qcld)
!
!   input argument list:
!     im       - integer number of atmospheric columns
!     km       - integer number of sigma levels in a column
!     k1       - integer lowest level from which a parcel can originate
!     k2       - integer highest level from which a parcel can originate
!     sl       - real (km) sigma values
!     slk      - real (km) sigma values to the kappa
!     ps       - real (im) surface pressure in kilopascals (cb)
!     tenv     - real (im,km) environment temperatures
!     qenv     - real (im,km) environment specific humidities
!
!   output argument list:
!     klcl     - integer (im) level just above lcl (km+1 if no lcl)
!     kbot     - integer (im) level just above cloud bottom
!     ktop     - integer (im) level just below cloud top
!              - note that kbot(i) gt ktop(i) if no cloud.
!     tcld     - real (im,km) of excess cloud temperatures.
!                (parcel t minus environ t, or 0. where no cloud)
!     qcld     - real (im,km) of excess cloud specific humidities.
!                (parcel q minus environ q, or 0. where no cloud)
!
! subprograms called:
!     fpkap    - function to compute pressure to the kappa power
!     ftdp     - function to compute dewpoint temperature
!     ftlcl    - function to compute lcl temperature
!     fthe     - function to compute equivalent potential temperature
!     ftma     - function to compute parcel temperature and humidity
!
! remarks: all functions are inlined by fpp.
!          nonstandard automatic arrays are used.
!
! attributes:
!   language: fortran 77.
!   machine:  cray.
!
!$$$
!c
      use machine , only : kind_phys
      use funcphys , only : ftdp, fthe, ftlcl, stma
      use physcons, eps => con_eps, epsm1 => con_epsm1, fv => con_fvirt
      implicit none
!c
      integer              k,k1,k2,km,i,im
      real(kind=kind_phys) pv,qma,slklcl,tdpd,thelcl,tlcl
      real(kind=kind_phys) tma,tvcld,tvenv
!c
      real(kind=kind_phys) prsl(im,km), prslk(im,km), tenv(im,km),       &   
     &                     qenv(im,km), tcld(im,km),  qcld(im,km)
      integer              klcl(im),    kbot(im),      ktop(im)
!  local arrays
      real(kind=kind_phys) slkma(im), thema(im)
!-----------------------------------------------------------------------
!  determine warmest potential wet-bulb temperature between k1 and k2.
!  compute its lifting condensation level.
!
      do i=1,im
        slkma(i) = 0.
        thema(i) = 0.
      enddo
      do k=k1,k2
        do i=1,im
          pv   = 1000.0 * prsl(i,k)*qenv(i,k)/(eps-epsm1*qenv(i,k))
          tdpd = tenv(i,k)-ftdp(pv)
          if(tdpd.gt.0.) then
            tlcl   = ftlcl(tenv(i,k),tdpd)
            slklcl = prslk(i,k)*tlcl/tenv(i,k)
          else
            tlcl   = tenv(i,k)
            slklcl = prslk(i,k)
          endif
          thelcl=fthe(tlcl,slklcl)
          if(thelcl.gt.thema(i)) then
            slkma(i) = slklcl
            thema(i) = thelcl
          endif
        enddo
      enddo
!-----------------------------------------------------------------------
!  set cloud temperatures and humidities wherever the parcel lifted up
!  the moist adiabat is buoyant with respect to the environment.
      do i=1,im
        klcl(i)=km+1
        kbot(i)=km+1
        ktop(i)=0
      enddo
      do k=1,km
        do i=1,im
          tcld(i,k)=0.
          qcld(i,k)=0.
        enddo
      enddo
      do k=k1,km
        do i=1,im
          if(prslk(i,k).le.slkma(i)) then
            klcl(i)=min(klcl(i),k)
            call stma(thema(i),prslk(i,k),tma,qma)
!           tma=ftma(thema(i),prslk(i,k),qma)
            tvcld=tma*(1.+fv*qma)
            tvenv=tenv(i,k)*(1.+fv*qenv(i,k))
            if(tvcld.gt.tvenv) then
              kbot(i)=min(kbot(i),k)
              ktop(i)=max(ktop(i),k)
              tcld(i,k)=tma-tenv(i,k)
              qcld(i,k)=qma-qenv(i,k)
            endif
          endif
        enddo
      enddo
!-----------------------------------------------------------------------
      return
      end
