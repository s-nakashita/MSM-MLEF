!
!***********************************************************************
!
      subroutine prtime(id,munth,iyear,ihr,xmin,jd,fjd,                  &   
     &                  dlt,alf,r1,slag,solc)
!c
      use machine , only :kind_rad,kind_phys
      use physcons, pi => con_pi
      implicit none
!c
      character*4 munth
      integer id,iyear,ihr,jd
      integer ltd,ltm,ihalp,iyy
      integer sign,sigb,dsig
      real (kind=kind_rad) xmin,fjd,dlt,alf,r1,slag,solc
      real (kind=kind_rad) degrad,hpi,zero,six,sixty,q22855
      real (kind=kind_rad) dltd,dltm,dlts,halp,ymin,asec,eqt,eqsec
!
      parameter (degrad=180.e0/pi,hpi=0.5e0*pi)
      data       sign/1h-/,      sigb/1h /
      data zero,six,sixty,q22855/0.0,6.0,60.0,228.55735/
      save sign,sigb,zero,six,sixty,q22855
!
      dltd=degrad*dlt
      ltd=dltd
      dltm=sixty*(abs(dltd)-abs(float(ltd)))
      ltm=dltm
      dlts=sixty*(dltm-float(ltm))
      dsig=sigb
      if((dltd.lt.zero).and.(ltd.eq.0)) dsig=sign
      halp=six*alf/hpi
      ihalp=halp
      ymin=abs(halp-float(ihalp))*sixty
      iyy=ymin
      asec=(ymin-float(iyy))*sixty
      eqt=q22855*slag
      eqsec=sixty*eqt
      print 1004,    id,munth,iyear,ihr,xmin,jd,fjd,r1,halp,ihalp,       &    
     &       iyy,asec,dltd,dsig,ltd,ltm,dlts,eqt,eqsec,slag,solc            
 1004 format('0 forecast date',9x,i3,a5,i6,' at',i3,' hrs',f6.2,         &
     &       '  mins, julian day',12x,i8,2x,'plus',f11.6,                &
     &       '  radius vector',9x,f10.7,                                 &
     &       '  right ascension of sun',f12.7,' hrs, or',i4,' hrs',i4,   &
     &                                 ' mins',f6.1,                     &
     &       '   secs, declination of the sun',f12.7,' degs, or',a2,i3,  &
     &                                 ' degs',i4,' mins',f6.1,          &
     &       '   secs, equation of time',6x,f12.7,' mins, or',f10.2,     &
     &                 ' secs, or',f9.6,                                 &
     &       '  radians solar constant',8x,f12.7)
      return
      end
