      program mainrinp
!
! interpolate global to regional by spherical transform in dynamic grid
! or interpolate coarse regional to regional 
! by bi-linear interpolation in physic grids.
!
      use funcphys
!ccj
#include <paramodel.h>
#include <rscomcpu.h>
!
#include <rscomver.h>
#include <rscomloc.h>
!
      logical         newsig,newmtn,newhor,sig2rg,sfc2rg,pgb2rg
      logical         newsst
      logical         moddate
      integer ivs
      integer igribversion,iqvar
      real plevels(levs)
      namelist/namrin/newsst,newsig,newmtn,newhor,sig2rg,sfc2rg,          &
     &                percmtn,ivs,igribversion,pgb2rg,iqvar,moddate
      data igribversion/1/
      data iqvar/1/
      data pgb2rg/.false./
      data newsig/.false./
      data newmtn/.false./
      data newhor/.false./
      data sig2rg/.false./
      data sfc2rg/.false./
      data percmtn/0.0/
      data newsst/.false./
      data moddate/.false./
      data nsigi,nsfci,nrslm,nrorg,nsig,npgb,nplevels                     &
     &    /   11,   12,   13,   14,  15,  16,     17/
      data nrsigio,nrsfcio,nrsigo,nrsfco                                  &
     &    /     51,     52,    61,    62/
      namelist /namloc/                                                  &
     & rproj,rtruth,rorient,rdelx,rdely,rcenlat,rcenlon,rlftgrd,rbtmgrd  &
     &,cproj,ctruth,corient,cdelx,cdely,ccenlat,ccenlon,clftgrd,cbtmgrd  &
     &,clat1,clat2,clon1,clon2
      data ivs/1997/
! enter
!-opn call w3tagb('rsmihi  ',1998,0000,0000,'np20   ')
!
      print *,'in mainrinp'
      call gncpus(ncpus)
      print *,'after mainrinp'
!
      print *,'read namelist'
      read(5,namrin)
      write(6,namrin)
      read(5,namloc)
      write(6,namloc)
!
!
      call gfuncphys
!
      if( sig2rg ) then
#ifdef G2R
              print *,'call g2rinp'
              call g2rinp(nsigi,nrsigio,ivs,idvc)
              print *,'after call g2rinp'
#endif
#ifdef C2R
              call c2rinp(nsigi,nrsigio,0,0,nrslm)
#endif
      endif
      if ((ivs.gt.2000).and.(idvc.ge.2)) newsig=.true.
!
      if( sfc2rg ) then
#ifdef G2R
              print *,'call s2rinp'
              if (ivs.le.2000) then
                 call s2rinp(nsfci,nrsfcio,nrslm)
              else
                 call s2rinp_gfs(nsfci,nrsfcio,nrslm,ivs)
              endif
              print *,'after call s2rinp'
#endif
#ifdef C2R
              call c2rinp(nsigi,0,nsfci,nrsfcio,nrslm)
#endif
      endif
!
      if ( pgb2rg ) then
              write(*,*) 'levs=',levs
              read(nplevels,*) plevels
              call l2rinp(npgb,nrsigio,nrsfcio,nrslm,igribversion,       &
     &                    plevels,iqvar)
      endif
!
      if( newsig .or. newmtn .or. newhor ) then
              print *,'call rchgr'
         if ( pgb2rg ) then
         call rchgr(nrsigio,nrsfcio,nrslm,nrorg,nrsigo,nrsfco,nsig,      &
     &           newsig,newmtn,newhor,pgb2rg,pgb2rg,pgb2rg,percmtn,      &
     &           newsst,moddate)
         else
         call rchgr(nrsigio,nrsfcio,nrslm,nrorg,nrsigo,nrsfco,nsig,      &
     &           newsig,newmtn,newhor,sig2rg,sfc2rg,pgb2rg,percmtn,      &
     &           newsst,moddate)
         endif
              print *,'after call rchgr'
      endif
! exit
!-opn call w3tage('rsmihi  ')
!
      stop
      end
