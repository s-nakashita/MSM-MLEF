!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! rsmkenprof is to read all r_kenfxx files and re-arrange variables of both profile c
! and surface according to eta bufr table and write into a binary file that is      c
! similar to what eta prof.f procuces so that eta bufr table and eta sndpst.f       c
! can be used to further create eta-like bufr file                                  c
!                                                                                   c
! author: binbin zhou                                                               c
!         july, 2003                                                                c
!                                                                                   c
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc  
!j
      subroutine rsmkenprof
!j
      use machine , only : kind_io4
#include <paramodel.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
#endif
#include <rscomgpd.h>
!j
      parameter(nlevel=levr, mvar=rmlvark, nfcsts= 64)  
      integer n1,n2,n3,npoints,itnums, nrkenp
!
! nvrken--number of variables
! nptken--maximum number of stations
! nfcsts--maximum number of number of forecast time steps (from f00, f01, f02, ....f63)
!
      character*80 r_kenf,r_kenbfr 
!
      dimension idate(4)
      real(kind=kind_io4) rsmdata(nvrken,nptken,nfcsts)
!
       character*3,save:: fxx(22)
       integer,save :: ftt(22)
!
        data (fxx(i), i=1,22)                                            &  
     &  /'f00', 'f03', 'f06', 'f09', 'f12', 'f15', 'f18',                &
     &   'f21', 'f24', 'f27', 'f30', 'f33', 'f36', 'f39',                &
     &   'f42', 'f45', 'f48', 'f51', 'f54', 'f57', 'f60', 'f63'/
!     
        data (ftt(i), i=1,22)                                            &
     &  /00, 03, 06, 09, 12, 15, 18, 21, 24, 27, 30, 33,                 &
     &   36, 39, 42, 45, 48, 51, 54, 57, 60, 63/
!     
        nrkenp=10
        nunit=40   
!     
#ifdef MP
        if (mype.eq.master) then
#endif
        do  ntime=2,(nfcsts-1)/3+1
!hmhj     r_kenf='r_ken'//fxx(ntime)
          write(r_kenf,"(a,a)") 'r_ken',fxx(ntime)
          open(unit=nrkenp,file=r_kenf,                                  &  
     &           status='old',form='unformatted')
          call rkenread(nrkenp,ntime,idate,rsmdata)
          close(nrkenp)
        enddo
!
        call rsm2eta(nunit,rsmdata,idate)
#ifdef MP
        endif
#endif
!     
        return
        end
!=======================================================
      subroutine rkenread(nrkenp,ntime,idate,rsmdata)
!
!  input:  nrkenp, ntime
!  output: idate, rsmdata
!
      use machine , only : kind_io4
#include <paramodel.h>
#include <rscomgpd.h>
!
      parameter(nlevel=levr, mvar=rmlvark, nfcsts= 64)
      integer n1,n2,n3,npoints,itnums, nrkenp
!
      character*8 label(4)
      character*80 r_kenf
!
      dimension idate(4),si(nlevel+1),sl(nlevel)
      real(kind=kind_io4) fhours,sis(nlevel+1),sls(nlevel)               &  
     &         ,temp(nvrken,nptken,nstken)                               &
     &         ,gpdlats(nptken),gpdlons(nptken),gpdgzss(nptken)          &
     &         ,gpmlats(nptken),gpmlons(nptken),gpmgzss(nptken)          &
     &         ,rsmdata(nvrken,nptken,nfcsts)
!
       common/x/n1,n2,n3,npoints,itnums
       common/y/gpdlats,gpdlons,gpdgzss
       common/z/gpmlats,gpmlons,gpmgzss
!
        read(nrkenp) label
!     
        fhours=fhour
        read(nrkenp) fhours,idate,sis,sls
!
        read(nrkenp) n1,n2,n3,npoints,itnums
        read(nrkenp)(gpdnam(n),                                          &
     &               gpdlats(n),gpdlons(n),gpdgzss(n),                   &  
     &               gpmlats(n),gpmlons(n),gpmgzss(n),n=1,npoints)
!     
        do j=1,npoints
         read(nrkenp)   ((temp(i,j,k),k=1,itnums),i=1,n1)
        enddo
!     
        do j=1,npoints
         do i=1,n1
          do k=1,itnums
           if(ntime.eq.2) then
            rsmdata(i,j,k)=temp(i,j,k)
           else
            rsmdata(i,j,(ntime-2)*3+1+k)=temp(i,j,k)
           end if
          enddo
         enddo
        enddo   
!
      return
      end
!=================================================
      subroutine rsm2eta(nunit, rsmdata, idate)
!
!  input: nunit, rsmdata, idate
!  output: none, but a eta-like binary file rsm2eta.tm00
!          is written out 
!
      use machine , only : kind_io4
#include <paramodel.h>
#include <rscomgpd.h>
!
       parameter(nlevel=levr, mvar=rmlvark, msvar=54) !mvar=13 same as eta, msvar=54 is rsm surface variables
       parameter( nfcsts= 64)
       parameter(lrecpr=4*(8 + 9 + mvar*nlevel + msvar ))
       real(kind=kind_io4) rsmdata(nvrken,nptken,nfcsts)
       real       etadata(nvrken,nptken,nfcsts)       
       real(kind=kind_io4) gpdlats(nptken),gpdlons(nptken)
       real(kind=kind_io4) gpdgzss(nptken)
       dimension idate(4)
       character*8 cistat
       common/y/gpdlats,gpdlons,gpdgzss
!
        open(unit=nunit,file='rsm2eta.tm00', access='direct',            &  
     &       recl=lrecpr,iostat=ier)
!     
        do  k=1,nfcsts            !64 forecast time steps
         do  j=1,nptken           !1300 stations
          etadata(1,j,k)=gpdlats(j)                   !latitude
          etadata(2,j,k)=gpdlons(j)                   !longitude
          etadata(3,j,k)=rsmdata(16,j,k)              !height (m)
          etadata(4,j,k)=float(nlevel)                !profile level number   
          etadata(5,j,k)=float(mvar)                  !number of profile variable
          etadata(6,j,k)=float(msvar)                 !number of surface variable
          etadata(7,j,k)=9+float(mvar*nlevel+msvar)   !actual length of etadata 
          etadata(8,j,k)=999.   
          etadata(9,j,k)=999.
!     
          do i=1,nlevel
           etadata(9+0*nlevel+i,j,k)=rsmdata(i+80+7*nlevel,j,k)*100  !10-37:p profile (pa) 
           etadata(9+1*nlevel+i,j,k)=rsmdata(i+80+2*nlevel,j,k)      !38-65:t profile (k)   
           etadata(9+2*nlevel+i,j,k)=rsmdata(i+80+0*nlevel,j,k)      !66-93:u profile (m/sec) 
           etadata(9+3*nlevel+i,j,k)=rsmdata(i+80+1*nlevel,j,k)      !94-121:v profile (m/sec) 
           etadata(9+4*nlevel+i,j,k)=rsmdata(i+80+3*nlevel,j,k)      !122-149:q profile (kg/kg) 
           etadata(9+5*nlevel+i,j,k)=rsmdata(i+80+6*nlevel,j,k)*1000 !150-177:omega prof(pa/sec) 
           etadata(9+6*nlevel+i,j,k)=1.e10                           !178-205:cloud water mixing ratio(>0 cloud,<0 ice)
           etadata(9+7*nlevel+i,j,k)=1.e10                           !206-233:heating rate due to convection, missing
           etadata(9+8*nlevel+i,j,k)=1.e10                           !234-261:heating rate due to stable latent,missing
           etadata(9+9*nlevel+i,j,k)=1.e10                           !262-289:heating rate due to short wave,missing
           etadata(9+10*nlevel+i,j,k)=1.e10                          !290-317:heating rate due to long-wave,missing
           etadata(9+11*nlevel+i,j,k)=1.e10                          !318-345:cloud cover fraction at each level,missing 
           etadata(9+12*nlevel+i,j,k)=1.e10                          !346-373:tke, missing
          enddo
!     
          len=9+mvar*nlevel                         !len=9+13*28=373
!     
          etadata(len+1,j,k)= 1.e10                 !374:sea level presure, missing   
          etadata(len+2,j,k)= rsmdata(4,j,k)*100    !375:surface presure (mb->pa)     
          etadata(len+3,j,k)= rsmdata(5,j,k)        !376:skin temperature (k)         
          etadata(len+4,j,k)= rsmdata(14,j,k)       !377:1-hr min t at surface(k)     
          etadata(len+5,j,k)= rsmdata(15,j,k)       !378:1-hr max t at surface(k)      
          etadata(len+6,j,k)= 1.e10                 !379:soil moisture available, missing
          etadata(len+7,j,k)= rsmdata(11,j,k)       !380:1-hr accumulated rain at grid (mm)
          etadata(len+8,j,k)= rsmdata(12,j,k)       !381:1-hr accumulated convective rain (mm)
          etadata(len+9,j,k)= rsmdata(17,j,k)       !382:1-hr averaged latent heat flux (w/m2)
          etadata(len+10,j,k)=1.e10                 !383:1-hr averaged potential latent heat flux, missing
          etadata(len+11,j,k)=rsmdata(18,j,k)       !384:1-hr averaged sensible heat flux (w/m2)
          etadata(len+12,j,k)=1.e10                 !385:1-hr averaged sub-surface heat flux, missing
          etadata(len+13,j,k)=1.e10                 !386:1-hr averaged flux of snow phase change heat, missing
          etadata(len+14,j,k)=rsmdata(45,j,k)       !387:1-hr averaged shortwave down flux at sfc (w/m2, +down,-up)
          etadata(len+15,j,k)=rsmdata(46,j,k)       !388:1-hr averaged shortwave upward flux at sfc (w/m2, +down,-up)
          etadata(len+16,j,k)=rsmdata(47,j,k)       !389:1-hr averaged longwave down flux at sfc (w/m2, +down,-up)
          etadata(len+17,j,k)=rsmdata(48,j,k)       !390:1-hr averaged longwave upward flux at sfc (w/m2, +down,-up) 
          etadata(len+18,j,k)=rsmdata(49,j,k)       !391:1-hr averaged longwave net flux at top (w/m2, +down,-up)
          etadata(len+19,j,k)=rsmdata(50,j,k)       !392:1-hr averaged shortwave net flux at top (w/m2, +down,-up)
          etadata(len+20,j,k)=rsmdata(10,j,k)       !393:accumulated snow depth (mm), note: diff from eta where is 1-hr
          etadata(len+21,j,k)=(rsmdata(9,j,k)*0.1                        &  !394:total soil moisture (average) (kg/kg)
     &           +rsmdata(57,j,k)*1.9)/(0.1+1.9)
          etadata(len+22,j,k)=1.e10                 !395:snow water equavalent, missing
          etadata(len+23,j,k)=rsmdata(53,j,k)       !396:snow melt (mm) since begin, diff from eta where is 1-hr value
          etadata(len+24,j,k)=rsmdata(54,j,k)*3600  !397:surface runoff(mm/hr) (non-accum, different from eta)
          etadata(len+25,j,k)=1.e10                 !398:1-hr accumulated baseflow-groundwater runoff, missing
          etadata(len+26,j,k)=rsmdata(8,j,k)        !399:deep soil temperature (k)
          etadata(len+27,j,k)=rsmdata(22,j,k)*0.01  !400:roughness (cm->meter)
          etadata(len+28,j,k)=rsmdata(34,j,k)       !401:u at 10 m (m/sec)
          etadata(len+29,j,k)=rsmdata(35,j,k)       !402:v at 10 m (m/sec)
          etadata(len+30,j,k)=1.e10                 !403:potential temperature at 10 m, missing
          etadata(len+31,j,k)=1.e10                 !404:q at 10 m (kg/kg), missing
          etadata(len+32,j,k)=rsmdata(30,j,k)       !405:t at 2 m (k)
          etadata(len+33,j,k)=rsmdata(31,j,k)       !406:q at 2 m (kg/kg)
          etadata(len+34,j,k)=rsmdata(23,j,k)       !407:momentum exchange coef(none), eta: (m/s)
          etadata(len+35,j,k)=rsmdata(55,j,k)*100   !408:vegetation cover(%)
          etadata(len+36,j,k)=rsmdata(32,j,k)       !409:canopy water (kg/kg)
          etadata(len+37,j,k)=rsmdata(9,j,k)        !410:soil moisture in layer 1 (kg/kg)
          etadata(len+38,j,k)=rsmdata(57,j,k)       !411:soil moisture in layer 2 (kg/kg)
          etadata(len+39,j,k)=rsmdata(7,j,k)        !412:soil temperature in layer 1 (k)
          etadata(len+40,j,k)=rsmdata(8,j,k)        !413:soil temperature in layer 2 (k)
          if(rsmdata(3,j,k).eq.0) then
            etadata(len+41,j,k)=1                   !414:land/sea mask. eta and rsm are reversed
          else                                      !only 1 and 0 used in eta
            etadata(len+41,j,k)=0             
          end if    
          etadata(len+42,j,k)=rsmdata(27,j,k)*100   !415:amount of lower cloud (%)
          etadata(len+43,j,k)=rsmdata(26,j,k)*100   !416:amount of mid cloud (%)
          etadata(len+44,j,k)=rsmdata(25,j,k)*100   !417:amount of high cloud (%)
          etadata(len+45,j,k)=1.e10                 !418:snow ratio from explicity cloud scheme, missing
!     
          etadata(len+46,j,k)=1.e10                 !419:snow precip type,          will do in sndpst.f
          etadata(len+47,j,k)=1.e10                 !420:ice precip type,           will do in sndpst.f
          etadata(len+48,j,k)=1.e10                 !421:frozing rain  precip type, will do in sndpst.f
          etadata(len+49,j,k)=1.e10                 !422:rain type,                 will do in sndpst.f
          etadata(len+50,j,k)=1.e10                 !423:u of storm motion,         will do in sndpst.f
          etadata(len+51,j,k)=1.e10                 !424:v of storm motion,         will do in sndpst.f
          etadata(len+52,j,k)=1.e10                 !426:storm relative helicity,   will do in sndpst.f
          etadata(len+53,j,k)=rsmdata(36,j,k)*100   !425:cloud base presure(mb->pa)
          etadata(len+54,j,k)=1.e10                 !427:horizontal visibility,     will do in sndpst.f      
                                                    !note: cloud base presure is computed in the rsm model
                                                    !while not in eta model, there is no space for it in eta case,    
                                                    !so 9 space from 419 - 427 are declared here instead of in
                                                    !sndpst.f for easy coding
          nntot=len+54
          nrec=j+(k-1)*nptken
          idstat=nint(rsmdata(37,j,k))  
          cistat=gpdnam(j)
          ifcst=(k-1)*3600
          write(nunit,rec=nrec) idate, ifcst,idstat,cistat,              &   
     &                       (etadata(n,j,k),n=1,nntot)
!     
        enddo   
       enddo
!     
          close(nunit)
!     
          return
          end          
