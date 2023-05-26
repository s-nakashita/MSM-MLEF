!
!***********************************************************************
!
      subroutine solar(jd,fjd,r,dlt,alp,slag,sdec,cdec)
!
!
!    *******************************************************************
!    *                            s o l a r                            *
!... *  patterned after original gfdl code---                          *
!... *     but no calculation of latitude mean cos solar zenith angle..*
!... *     zenith angle calculations done in sr   zenith in this case..*
!... *  hr angle,mean cosz,and mean tauda calc removed--k.a.c. mar 89  *
!    *  updates by hualu pan to limit iterations in newton method and  *
!    *  also ccr reduced from(1.3e-7)--both to avoid nonconvergence in *
!    *  nmc s half precision version of gfdl s code   ----  fall 1988  *
!    *******************************************************************
!
!.....solar computes radius vector, declination and right ascension of
!.....sun, equation of time
!
      use machine , only :kind_rad,kind_phys
      use physcons, pi => con_pi
      implicit none
!c
      real(kind=kind_rad),save :: cyear,svt6,ccr,tpp
      integer,save:: jdor
                                   data                                   &  
     &   cyear/365.25/,      ccr/1.3e-6/
!
!.....tpp = days between epoch and perihelion passage of 1900
!.....svt6 = days between perihelion passage and march equinox of 1900
!.....jdor = jd of epoch which is january 0, 1900 at 12 hours ut
!
                                   data                                   &  
     &   tpp/1.55/,          svt6/78.035/,       jdor/2415020/
!
!    *******************************************************************
      

      real(kind=kind_rad) tpi,hpi,rad
      parameter (tpi=2.0*pi,hpi=0.5*pi,rad=180.0/pi)
      integer jd,jdoe,iter
      real(kind=kind_rad) fjd,r,dlt,alp,slag,sdec,cdec
      real(kind=kind_rad) dat,t,year,tyear,ec,angin,ador,deleqn
      real(kind=kind_rad) sni,tini,er,qq,e,ep,cd,he,eq,date
      real(kind=kind_rad) em,cr,w,tst,sun
!
      dat=float(jd-jdor)-tpp+fjd
!    computes time in julian centuries after epoch
      t=float(jd-jdor)/36525.e0
!    computes length of anomalistic and tropical years (minus 365 days)
 
      year=.25964134e0+.304e-5*t
      tyear=.24219879e0-.614e-5*t
!    computes orbit eccentricity and angle of earth inclination from t
      ec=.01675104e0-(.418e-4+.126e-6*t)*t
      angin=23.452294e0-(.0130125e0+.164e-5*t)*t
      ador=jdor
      jdoe=ador+(svt6*cyear)/(year-tyear)
!    deleqn=updated svt6 for current date
      deleqn=float(jdoe-jd)*(year-tyear)/cyear
      year=year+365.e0
      sni=sin(angin/rad)
      tini=1.e0/tan(angin/rad)
      er=sqrt((1.e0+ec)/(1.e0-ec))
      qq=deleqn*tpi/year
!    determine true anomaly at equinox
      e=1.e0
      iter = 0
 32   ep=e-(e-ec*sin(e)-qq)/(1.e0-ec*cos(e))
      cd=abs(e-ep)
      e=ep
      iter = iter + 1
      if(iter.gt.10) then
        write(6,*) ' iteration count for loop 32 =', iter
        write(6,*) ' e, ep, cd =', e, ep, cd
      endif
      if(iter.gt.10) goto 1032
      if(cd.gt.ccr) go to 32
 1032 continue
      he=.5e0*e
      eq=2.e0*atan(er*tan(he))
!    date=days since last perihelion passage
      date = mod(dat,year)
!    solve orbit equations by newton method
      em=tpi*date/year
      e=1.e0
      iter = 0
 31   ep=e-(e-ec*sin(e)-em)/(1.e0-ec*cos(e))
      cr=abs(e-ep)
      e=ep
      iter = iter + 1
      if(iter.gt.10) then
        write(6,*) ' iteration count for loop 31 =', iter
      endif
      if(iter.gt.10) goto 1031
      if(cr.gt.ccr) go to 31
 1031 continue
      r=1.e0-ec*cos(e)
      he=.5e0*e
      w=2.e0*atan(er*tan(he))
!>yh  sind=sni*sin(w-eq)
!>yh  dlt=asin(sind)
      sdec=sni*sin(w-eq)
      cdec=sqrt(1.e0 - sdec*sdec)
      dlt=asin(sdec)
      alp=asin(tan(dlt)*tini)
      tst=cos(w-eq)
      if(tst.lt.0.e0) alp=pi-alp
      if(alp.lt.0.e0) alp=alp+tpi
      sun=tpi*(date-deleqn)/year
      if(sun.lt.0.e0) sun=sun+tpi
      slag=sun-alp-.03255e0
      return
      end
