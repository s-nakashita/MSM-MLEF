      program mainrmtn
!c
      use physcons, rerth => con_rerth, pi => con_pi
!
#include <paramodel.h>
#include <rscomloc.h>
#ifdef GTOPO30
      real mtnresx
#else
      integer mtnresx
#endif
!
      parameter(imn=360*60/mtnres,jmn=180*60/mtnres,mtnvar=nmtvr)
!
#ifdef GTOPO30
      character*120 condir
#endif
!
      namelist /namloc/                                                      &
     & rproj,rtruth,rorient,rdelx,rdely,rcenlat,rcenlon,rlftgrd,rbtmgrd      &
     &,cproj,ctruth,corient,cdelx,cdely,ccenlat,ccenlon,clftgrd,cbtmgrd      &
     &,clat1,clat2,clon1,clon2
!
#ifdef GTOPO30
      namelist /namcondir/ condir
#endif
!
! define regional grid from stdin and save to comloc
!
      read(5,namloc)
#ifdef GTOPO30
      read(5,namcondir)
#endif
!
! check the orography data
!
      mtnresx = 360./float(imn) * 60.
      print *,'++++  orography data consistency checking +++ '
      print *,'          input data = ',mtnresx,' (min)'
      print *,'          in program = ',mtnres,' (min)'
      if(mtnresx.ne.mtnres) then
         print *,' !!!!!! orography data mismached !!!!!! '
         print *,' !!!!!! check mtnres in script '
         call abort
      endif
!
! check the orography resolution on the rsm grid
!
      if (rproj.eq.4) then
         rdelx=rerth*rdelx*pi/180.0
         rdely=rerth*rdely*pi/180.0
      endif
      delxl = 2.*pi*rerth/float(imn)
      ratio = rdelx / (delxl)
      print *,'++++  orography data resolution checking +++ '
      print *,'      rsm_dx   = ',rdelx,' (m)'
      print *,'      mtn_dx   = ', delxl,' (m)'
      print *,'      ratio (rsm_dx/mtn_dx)   = ',  ratio
      print *,'!!!!! ratio greater than 3 is recommended !!!! '
      if(ratio.lt.3.) then
         print *,'!!!!!! increase of mtn data resolution is desirable '
         print *,'!!!!!! but keep running '
      endif
      print *,'++++  optimal lat-lon grid resolution checking +++ '
      llfacx = nint(max(ratio/3.,1.))*3
      iml = imn/llfacx
      jml = jmn/llfacx
      print *,'!!!!!! nominal lat-lon grid resolution is ',              &
     &       llfacx*mtnres, ' (min)'
      print *,'!!!!!! (iml,jml) is ',iml,jml
      print *,'++++  number of mountain variance file  =  ',mtnvar
      if(mtnvar.gt.1 ) then
         print *,'!!!!!! enhanced surface gwd scheme is applied'
      else
         print *,'!!!!!! standard upper-level gwd scheme is applied'
      endif
!
#ifdef GTOPO30
      call rtersub(iml,jml,condir)
#else
      call rtersub(iml,jml)
#endif
      print *,' !!!! Normal exit !!!!'
      stop
      end
