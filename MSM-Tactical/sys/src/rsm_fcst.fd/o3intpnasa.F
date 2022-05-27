!
!***********************************************************************
!
      subroutine o3intpnasa(fhour,idate,o3clim,pstr,o3out)
!     ********************************************************
!     *  computes o3 climo from 12 month dataset, linearly   *
!     *   interpolated to day,mon of the fcst.  then create  *
!     *   a 5 deg array from the 10 deg climatology...for    *
!     *   ease when doing a latitudinal interpolation        *
!     *  thanks to s moorthi for new o3 climo...kac  dec 1996*
!     * input:                                               *
!     *   idate=nmc date-time                                *
!     *   fhour=forecast hour                                *
!     *   o3clim=10-deg o3 climo for each month(np->spole)   *
!     * output :                                             *
!     *   o3out=5-deg o3 climo for forecast date(np->spole)  *
!     ********************************************************
! geos ozone data
      use machine , only :kind_rad
      implicit none
!c
      integer loz,jmr,jmout
      parameter (jmr=18,loz=17,jmout=37)
!
      integer days(12),idate(4)
      integer ida,imo,numdyz,imo1,jday,nmdtot,ndayr,mday
      integer monl,monc,monr,midl,midc,midr,jmr1,j1,j2
      integer l,j,ken
      
      real(kind=kind_rad) fhour
      real(kind=kind_rad) o3clim(jmr,loz,12),o3tmp(jmr,loz)
      real(kind=kind_rad) o3out(jmout,loz),pstr(loz)
      real(kind=kind_rad) difl,difr,delday
!
!ccc      common /o3nasaclim/o3clim,pstr,o3out 
!      data  days/31,28,31,30,31,30,31,31,30,31,30,31/
!      save days
       days(1)=31
       days(2)=28
       days(3)=31
       days(4)=30
       days(5)=31
       days(6)=30
       days(7)=31
       days(8)=31
       days(9)=30
       days(10)=31
       days(11)=30
       days(12)=31

!        print *,'fhour=',fhour,'idate=',idate
!...     begin here  ......
      ida=idate(3)
      imo=idate(2)
!...   find current day and month, initial values in ida,imo!
!       will not worry about leap year, since it will take a
!       120-year (what?) forecast to be off by 1 month.  if this
!       is deemed a problem, need to redo this calculation.
!
      if (fhour.ge.24.) then
!... number of days into the forecast
       numdyz=fhour/24.0 + 0.01
!... get day-of-year, remember climate runs are for years
       imo1=imo-1
       jday = ida
       if (imo1.gt.0) then
        jday=0
        do 7 ken=1,imo1
         jday=jday+days(ken)
    7   continue
        jday=jday+ida
       end if
       nmdtot = jday+numdyz
       ndayr = mod(nmdtot,365)
       if (ndayr.eq.0) ndayr=365
!... now get month from day-of-year
       mday=0
       do 8 ken=1,11
        mday=mday+days(ken)
        imo=ken
        if (ndayr.le.mday) then
         ida=ndayr-(mday-days(imo))
         go to 9
        end if
    8  continue
       imo=12
    9  continue
!cc    print 66,fhour,numdyz,jday,nmdtot,ndayr
   66  format(' sbuvo3 climo hr=',f10.1,                                 &   
     &        ' numdyz,jday,nmdtot,ndayr=',4i8)
      end if
!
!...   do a linear interpolation in time, where we assume that
!       the ozone data is valid for mid-month
!      monl is the preceeding month, monc for current mo, and
!      monr is the future month..
      monl=imo-1
      monc=imo
      monr=imo+1
      if (monl.lt.1) monl=12
      if (monr.gt.12) monr=1
!...    difl=number of days beteen mid-months of the current and
!            preceeding mo, difr=same for current and future mo..
!...    dell=number of days between current day and mon,
!       delr=same for current day and next month.
!       sign convention as if we were using day of year calculations.
      midl=days(monl)/2
      midc=days(monc)/2
      midr=days(monr)/2
      difl=-(days(monl)-midl+midc)
      difr= (days(monc)-midc+midr)
      delday=ida-midc
!      print *,'monl=',monl,'monc=',monc,'monr=',monr,'dellay=',delday,difl
      if (ida.gt.midc) then
       do 60 j=1,jmr
        do 60 l=1,loz
          o3tmp(j,l)=o3clim(j,l,monc) +                                  &
     &        (o3clim(j,l,monr)-o3clim(j,l,monc)) * delday/difr
   60  continue
      else if (ida.lt.midc) then
       do 65 j=1,jmr
        do 65 l=1,loz
          o3tmp(j,l)=o3clim(j,l,monc) +                                  &    
     &        (o3clim(j,l,monl)-o3clim(j,l,monc)) * delday/difl
   65  continue
      else if (ida.eq.midc) then
       do 70 j=1,jmr
        do 70 l=1,loz
          o3tmp(j,l)=o3clim(j,l,monc)
   70  continue
      end if
!cselaprint 200,imo,ida
!...   linearly interpolate to 5 deg zonal means
      jmr1=jmr-1
      do 80 j=1,jmr1
       j1=j*2
       j2=j1+1
       do 80 l=1,loz
        o3out(j1,l)=o3tmp(j,l)
        o3out(j2,l)=0.5*(o3tmp(j,l)+o3tmp(j+1,l))
   80 continue
      do 85 l=1,loz
       o3out(1,l)=o3tmp(1,l)
       o3out(jmout-1,l)=o3tmp(jmr,l)
       o3out(jmout,l)=o3tmp(jmr,l)
   85 continue
  200 format(1h1,'from o3intpnasa ozone climatology for mont,day=',2i4)       
      return
      end
