!
!  progt2 is the second part of the soil model that is executed
!  after precipitation for the time step has been calculated
!
!fpp$ noconcur r
!fpp$ expand(funcdf,funckt,thsat)
      subroutine progt2(im,ix,km,rhscnpy,                                &       
     &                  rhsmc,ai,bi,ci,smc,slimsk,                       &
     &                  canopy,precip,runoff,snowmt,                     &
     &                  zsoil,soiltyp,sigmaf,delt)
!c
      use machine     , only : kind_phys
!     use machine_rad , only : kind_phys
      implicit none
      integer              km, im, ix
      real(kind=kind_phys) delt
      real(kind=kind_phys) rhscnpy(ix),  rhsmc(ix,km), ai(ix,km),        &
     &                     bi(ix,km),    ci(ix,km),    smc(ix,km),       &
     &                     slimsk(ix),   canopy(ix),   precip(ix),       &
     &                     runoff(ix),   snowmt(ix),   zsoil(ix,km),     &
     &                     sigmaf(ix)
      integer soiltyp(ix)
!
      integer              k, lond, i
      real(kind=kind_phys) cnpy(ix), prcp(ix),   tsat(ix),               &
     &                     inf(ix),  infmax(ix), smsoil(ix,km)
!
      real(kind=kind_phys) cc,    ctfil1, ctfil2, delt2,                 &
     &                     drip,  rffact, rhoh2o,                        &
     &                     rzero, scanop, tdif, thsat, ksat
!
      logical flag(ix)
!c
      parameter (scanop=.5, rhoh2o=1000.)
      parameter (ctfil1=.5, ctfil2=1.-ctfil1)
!     parameter (ctfil1=1., ctfil2=1.-ctfil1)
      parameter (rffact=.15)
!
!ifdef DBG
!     latd = 44
!endif
      lond = 353
      delt2 = delt * 2.
!
!  precipitation rate is needed in unit of kg m-2 s-1
!
      do i=1,im
        prcp(i) = rhoh2o * (precip(i)+snowmt(i)) / delt
        runoff(i) = 0.
        cnpy(i) = canopy(i)
      enddo
!ifdef DBG
!      if(lat.eq.latd) then
!        print *, ' before runoff cal, rhsmc =', rhsmc(1)
!      endif
!endif
!
!  update canopy water content
!
      do i=1,im
        if(slimsk(i).eq.1.) then
          rhscnpy(i) = rhscnpy(i) + sigmaf(i) * prcp(i)
          canopy(i) = canopy(i) + delt * rhscnpy(i)
          canopy(i) = max(canopy(i),0.)
          prcp(i) = prcp(i) * (1. - sigmaf(i))
          if(canopy(i).gt.scanop) then
            drip = canopy(i) - scanop
            canopy(i) = scanop
            prcp(i) = prcp(i) + drip / delt
          endif
!
!  calculate infiltration rate
!
          inf(i) = prcp(i)
          tsat(i) = thsat(soiltyp(i))
!         dsat = funcdf(tsat(i),soiltyp(i))
!         ksat = funckt(tsat(i),soiltyp(i))
!         infmax(i) = -dsat * (tsat(i) - smc(i,1))
!    &                / (.5 * zsoil(i,1))
!    &                + ksat
          infmax(i) = (-zsoil(i,1)) *                                    &   
     &                ((tsat(i) - smc(i,1)) / delt - rhsmc(i,1))         &
     &                * rhoh2o
          infmax(i) = max(rffact*infmax(i),0.)
!         if(smc(i,1).ge.tsat(i)) infmax(i) = ksat
!         if(smc(i,1).ge.tsat(i)) infmax(i) = zsoil(i,1) * rhsmc(i,1)
          if(inf(i).gt.infmax(i)) then
            runoff(i) = inf(i) - infmax(i)
            inf(i) = infmax(i)
          endif
          inf(i) = inf(i) / rhoh2o
          rhsmc(i,1) = rhsmc(i,1) - inf(i) / zsoil(i,1)
        endif
      enddo
!!
!ifdef DBG
!      if(lat.eq.latd) then
!        print *, ' prcp(i), infmax(i), runoff =', prcp(i),infmax(i),runoff
!        print *, ' smsoil =', smc(1), smc(2)
!        print *, ' rhsmc =', rhsmc(1)
!      endif
!endif
!
!  we currently ignore the effect of rain on sea ice
!
      do i=1,im
        flag(i) = slimsk(i).eq.1.
      enddo
!!
!
!  solve the tri-diagonal matrix
!
      do k = 1, km
        do i=1,im
          if(flag(i))  then
            rhsmc(i,k) = rhsmc(i,k) * delt2
            ai(i,k) = ai(i,k) * delt2
            bi(i,k) = 1. + bi(i,k) * delt2
            ci(i,k) = ci(i,k) * delt2
          endif
        enddo
      enddo
!  forward elimination
      do i=1,im
        if(flag(i)) then
          ci(i,1) = -ci(i,1) / bi(i,1)
          rhsmc(i,1) = rhsmc(i,1) / bi(i,1)
        endif
      enddo
      do k = 2, km
        do i=1,im
          if(flag(i)) then
            cc = 1. / (bi(i,k) + ai(i,k) * ci(i,k-1))
            ci(i,k) = -ci(i,k) * cc
            rhsmc(i,k)=(rhsmc(i,k)-ai(i,k)*rhsmc(i,k-1))*cc
          endif
        enddo
      enddo
!  backward substituttion
      do i=1,im
        if(flag(i)) then
          ci(i,km) = rhsmc(i,km)
        endif
      enddo
!!
      do k = km-1, 1
        do i=1,im
          if(flag(i)) then
            ci(i,k) = ci(i,k) * ci(i,k+1) + rhsmc(i,k)
          endif
        enddo
      enddo
 100  continue
!
!  update soil moisture
!
      do k = 1, km
        do i=1,im
          if(flag(i)) then
            smsoil(i,k) = smc(i,k) + ci(i,k)
            smsoil(i,k) = max(smsoil(i,k),0.)
            tdif = max(smsoil(i,k) - tsat(i),0.)
            runoff(i) = runoff(i) - rhoh2o * tdif * zsoil(i,k) / delt
            smsoil(i,k) = smsoil(i,k) - tdif
          endif
        enddo
      enddo
      do k = 1, km
        do i=1,im
          if(flag(i)) then
            smc(i,k) = ctfil1 * smsoil(i,k) + ctfil2 * smc(i,k)
          endif
        enddo
      enddo
!       if(flag(i)) then
!         canopy(i) = ctfil1 * canopy(i) + ctfil2 * cnpy(i)
!       endif
!     i = 1
!     print *, ' smc'
!     print 6000, smc(1), smc(2)
!6000 format(2(f8.5,','))
      return
      end
