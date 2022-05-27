      program prepare_terrain
!
! henry juang for dimensionless terrain preparation done on April 2007
!
      use comrloc
!
      character*120 condir     
!
      namelist /namloc/							&
        rproj,rtruth,rorient,rdelx,rdely,rcenlat,rcenlon,rlftgrd,rbtmgrd
      namelist /namdim/ im,jm,condir,mtnvar,mtnres
!
! define regional grid from stdin and save to comloc
!
      read(5,namloc)
      write(*,namloc)
      read(5,namdim)
      write(*,namdim)
!
! approimated wave number, only for smooth purpose.
!
      iw = im/3 * 2 + 1
      jw = iw*jm/(im*2) * 2 + 1
      lngrd = im*jm
      lnwav = iw*jw
!
! check the orography data
!
      imn = 360*60*60/mtnres
      jmn = 180*60*60/mtnres
!
! check the orography resolution on the rsm grid
!
      pi = 2.0 * asin(1.0)
      rerth = 6371220.0
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
      print *,'!!!!!! nominal lat-lon grid resolution is ',		&
             llfacx*mtnres, ' (min)'
      print *,'!!!!!! (iml,jml) is ',iml,jml
      print *,'++++  number of mountain variance file  =  ',mtnvar
      if(mtnvar.gt.1 ) then
         print *,'!!!!!! enhanced surface gwd scheme is applied'
      else
         print *,'!!!!!! standard upper-level gwd scheme is applied'
      endif
!
      call terrain_sub(iml,jml,mtnres,condir)
!
      print *,' Normal exit!!!! '
!
      stop
      end
