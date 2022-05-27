      subroutine conrad(nfile,rco2, l,lp1,lp1v,nbly, nblw               &
     &,           sgtmp, co21d, co22d, co21d3, co21d7                   &
     &,           source, dsrce, me)
!fpp$ noconcur r
!    *******************************************************************
!    *                           c o n r a d                           *
!    *    read co2 transmission data from unit(nfile)for new vertical  *
!    *      coordinate tests      ...                                  *
!    *    these arrays used to be in block data    ...k.campana-mar 90 *
!    *******************************************************************
!
      use machine , only : kind_rad,kind_io4
      implicit none
!     include 'mpi_inc.h'
!
      integer nfile, l, lp1, lp1v, nbly, nblw, me
      real (kind=kind_rad) sgtmp(lp1,2),   co21d(l,6), co22d(lp1,lp1,6) & 
     &,                     co21d3(lp1,6),  co21d7(lp1,6)               &
     &,                     source(28,nbly),dsrce(28,nbly), rco2
!
      real (kind=kind_io4) sgtmp4(lp1,2),co21d4(l,6),co22d4(lp1,lp1,6)  &
     &,                   co21d34(lp1,6), co21d74(lp1,6), rco24
!
      integer i, j, kk
!
!                 co2 data tables for users vertical coordinate
!
!   the following module blocks contain pretabulated co2 transmission
!       functions, evaluated using the methods of fels and
!       schwarzkopf (1981) and schwarzkopf and fels (1985),
!-----  the 2-dimensional arrays are
!                    co2 transmission functions and their derivatives
!        from 109-level line-by-line calculations made using the 1982
!        mcclatchy tape (12511 lines),consolidated,interpolated
!        to the nmc mrf vertical coordinatte,and re-consolidated to a
!        200 cm-1 bandwidth. the interpolation method is described in
!        schwarzkopf and fels (j.g.r.,1985).
!-----  the 1-dim arrays are
!                  co2 transmission functions and their derivatives
!          for tau(i,i+1),i=1,l,
!            where the values are not obtained by quadrature,but are the
!            actual transmissivities,etc,between a pair of pressures.
!          these used only for nearby layer calculations including qh2o.
!-----  the weighting function gtemp=p(k)**0.2*(1.+p(k)/30000.)**0.8/
!         1013250.,where p(k)=pressure,nmc mrf(new)  l18 data levels for
!         pstar=1013250.
!-----  stemp is us standard atmospheres,1976,at data pressure levels
!        using nmc mrf sigmas,where pstar=1013.25 mb (ptz program)
!====>   begin here to get constants for radiation package
!
      rewind nfile
!       read in pre-computed co2 transmission data....
!
      read(nfile) (sgtmp4(i,1),i=1,lp1)
      read(nfile) (sgtmp4(i,2),i=1,lp1)
      do kk=1,6
        read(nfile) (co21d4(i,kk),i=1,l)
      enddo
      do kk=1,6
        read(nfile) ((co22d4(i,j,kk),i=1,lp1),j=1,lp1)
      enddo
      do kk=1,6
        read(nfile) (co21d34(i,kk),i=1,lp1)
      enddo
      do kk=1,6
        read(nfile) (co21d74(i,kk),i=1,lp1)
      enddo
!
!  read co2 concentration in ppm (defaulted in gradfs if missing)
        read(nfile,end=31) rco24
   31   continue
!
      sgtmp  = sgtmp4
      co21d  = co21d4
      co22d  = co22d4
      co21d3 = co21d34
      co21d7 = co21d74
      rco2   = rco24
!
      if (me.eq.0) print *,'co2 concentration is ',rco2
      rewind nfile
!
!     print 66,nfile
!  66 format(1h ,'----read co2 transmission functions from unit ',i2)
!
!......    define tables for lw radiation
!
      call lwtable(lp1,lp1v, source,dsrce)
!
      return
      end
