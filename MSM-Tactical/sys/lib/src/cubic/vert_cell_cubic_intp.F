      subroutine vert_cell_cubic_intp(pp,qq,pn,qn,lons,levs,rc)
!
! mass conservation in vertical interpolation: interpolate a group
! of grid point  coordiante call pp at interface with quantity qq at
! cell averaged to a group of new grid point coordinate call pn at
! interface with quantity qn at cell average with cubic spline.
! in vertical with mass conservation is under the condition that
! pp(1)=pn(1), pp(levs+1)=pn(levs+1)
!
! pp    pressure at interfac level as input
! qq    quantity at layer as input
! pn    pressure at interface of new grid structure as input
! qn    quantity at layer as output
! levs  numer of verical layers
! rc    rerurn code; o:success, 1:error in end point, 2:error in p index
!
! author : henry.juang@noaa.gov
!
      implicit none
!
      real      pp(lons,levs+1)
      real      qq(lons,levs)
      real      pn(lons,levs+1)
      real      qn(lons,levs)
      integer   lons,levs
      integer   rc
!
      real      mass(0:levs+1),qi(levs+1),mi(levs+1)
      real      mmi(levs),mpi(levs)
      real      hh(levs),alpha,betta,gamma,hhp2,hhm2
      real      dqi,dqimax,dqimin,dqmono(levs)
      real      tl,tl2,tl3,tl4,qql,dql
      real      th,th2,th3,th4,qqh,dqh
      real      dpp,dpq
      integer   i,k, kl, kh, kk, kkl, kkh
!
      do i=1,lons
!
      rc = 1
      if( pp(i,1).ne.pn(i,1) .or. pp(i,levs+1).ne.pn(i,levs+1) ) return
!
! prepare thickness for uniform grid
!
      do k=1,levs
        hh(k) = pp(i,k+1)-pp(i,k)
      enddo
!
! prepare location with monotonic concerns
!
      mass(1:levs)=qq(i,1:levs)
      mass(0)=(3.*hh(1)+hh(2))*mass(1)-2.*hh(1)*mass(2)
      mass(0)=mass(0)/(hh(1)+hh(2))
!     mass(0)=mass(1)
      mass(levs+1)=(3.*hh(levs)+hh(levs-1))*mass(levs)
     &             -2.*hh(levs)*mass(levs-1)
      mass(levs+1)=mass(levs+1)/(hh(levs)+hh(levs-1))
!     mass(levs+1)=mass(levs)
      do k=1,levs
        dqi = 0.25*(mass(k+1)-mass(k-1))
        dqimax = max(mass(k-1),mass(k),mass(k+1)) - mass(k)
        dqimin = mass(k) - min(mass(k-1),mass(k),mass(k+1))
        dqmono(k) = sign( min( abs(dqimin), dqimin, dqimax ), dqi)
      enddo
!
! compute value at interface with momotone
!
      do k=2,levs
!       qi(k)=0.5*(mass(k-1)+mass(k))+(dqmono(k-1)-dqmono(k))/3.0
        qi(k)=(mass(k-1)*hh(k)+mass(k)*hh(k-1))/(hh(k)+hh(k-1))
     &       +(dqmono(k-1)-dqmono(k))/3.0
      enddo
!
! prepare derivative with mass conservation concerns
!
      do k=3,levs-1
        alpha=1./hh(k)+1./hh(k-1)
        hhp2 = hh(k)**2
        hhm2 = hh(k-1)**2
        mi(k) = 4.*(mass(k)/hhp2-mass(k-1)/hhm2) 
     &          - (qi(k+1)+3.*qi(k))/hhp2
     &          + (qi(k-1)+3.*qi(k))/hhm2
        mi(k) = mi(k) / alpha
      enddo
!
! --  bottom natural condition: 2nd derivative=0
!
      alpha=1./hh(1)/hh(1)
      betta=1./hh(1)/hh(2)
      gamma=1./hh(2)/hh(2)
      qi(1)=(4.*alpha+8.*betta)*mass(1)+4.*gamma*mass(2)
     &     -(2.*alpha+5.*betta+3.*gamma)*qi(2)-gamma*qi(3)
      qi(1)=qi(1)/(2.*alpha+3.*betta)
      mi(2)=(5.*qi(2)+3.*qi(1)-8.*mass(1))/hh(1)
      mi(1)=mi(2)+(12.*mass(1)-6.*(qi(1)+qi(2)))/hh(1)
!
! --  upper natural condition: 2nd derivative=0
      alpha=1./hh(levs)/hh(levs)
      betta=1./hh(levs)/hh(levs-1)
      gamma=1./hh(levs-1)/hh(levs-1)
      qi(levs+1)=(4.*alpha+8.*betta)*mass(levs)+4.*gamma*mass(levs-1)
     &     -(2.*alpha+5.*betta+3.*gamma)*qi(levs)-gamma*qi(levs-1)
      qi(levs+1)=qi(levs+1)/(2.*alpha+3.*betta)
      mi(levs)=(8.*mass(levs)-5.*qi(levs)-3.*qi(levs+1))/hh(levs)
      mi(levs+1)=mi(levs)-(12.*mass(levs)
     &                   -6.*(qi(levs)+qi(levs+1)))/hh(levs)
!
! modify to local consistency
!     do k=1,levs
!       if( mi(k).eq.mi(k+1) ) then
!         alpha=mass(k)/(0.5*(qi(k)+qi(k+1)))
!         qi(k)=qi(k)*alpha
!         qi(k+1)=qi(k+1)*alpha
!       endif
!     enddo
      do k=1,levs
        mmi(k)=mi(k)
        mpi(k)=mi(k+1)
!       if( mi(k).ne.mi(k+1) ) then
!         alpha=12./hh(k)*(0.5*(qi(k)+qi(k+1))-mass(k))
!         alpha=alpha/(mi(k+1)-mi(k))
!         mmi(k)=mmi(k)*alpha
!         mpi(k)=mpi(k)*alpha
!       endif
      enddo
!         
!
      do k=levs+1,2,-1
        print *,' i=',k,' qi=',qi(k),' mi=',mi(k)
        print *,'     ',' qm=',mass(k-1)
      enddo
      print *,' i=  1     qi=',qi(1),' mi=',mi(1)
!
! start interpolation by integral of cubic spline
!
      kkl = 1
      do k=1,levs
        kl = k
        kh = k + 1
! find kkh
        do kk=kkl+1,levs+1
!         if( pn(i,kh).ge.pp(i,kk) ) then
          if( -pn(i,kh).ge.-pp(i,kk) ) then
            kkh = kk-1
            go to 100
          endif
        enddo
! mass interpolate
 100    if( kkh.eq.kkl ) then
          tl=(pn(i,kl)-pp(i,kkl))/hh(kkl)
          th=(pn(i,kh)-pp(i,kkl))/hh(kkl)
          tl2=tl*tl
          tl3=tl2*tl
          tl4=tl3*tl
          th2=th*th
          th3=th2*th
          th4=th3*th
          qqh=(th4/2.-th3+th)*qi(kkl)
     &       +(th4/4.-2.*th3/3.+th2/2.)*hh(kkl)*mmi(kkl)
     &       +(-th4/2.+th3)*qi(kkl+1)
     &       +(th4/4.-th3/3.)*hh(kkl)*mpi(kkl)
          qql=(tl4/2.-tl3+tl)*qi(kkl)
     &       +(tl4/4.-2.*tl3/3.+tl2/2.)*hh(kkl)*mmi(kkl)
     &       +(-tl4/2.+tl3)*qi(kkl+1)
     &       +(tl4/4.-tl3/3.)*hh(kkl)*mpi(kkl)
          qn(i,k) = (qqh-qql)/(th-tl)
          print *,' con1: kl kh  kkl kkh ',kl,kh,kkl,kkh
          print *,' con1: pkl pkh  pkkl pkkh ',pn(i,kl),pn(i,kh)
     &           , pp(i,kkl),pp(i,kkh)
          print *,' con1: qq tl th qqh qql ',qq(i,k),tl,th,qqh,qql
          print *,' compare qn and qq ',qn(i,k),qq(i,kkl)
        else if( kkh.gt.kkl ) then
          tl=(pn(i,kl)-pp(i,kkl))/hh(kkl)
          tl2=tl*tl
          tl3=tl2*tl
          tl4=tl3*tl
          qql=(tl4/2.-tl3+tl)*qi(kkl)
     &       +(tl4/4.-2*tl3/3.+tl2/2.)*hh(kkl)*mmi(kkl)
     &       +(-tl4/2.+tl3)*qi(kkl+1)
     &       +(tl4/4.-tl3/3.)*hh(kkl)*mpi(kkl)
          dql = qq(i,kkl)-qql
          print *,' con2: kl kh  kkl kkh ',kl,kh,kkl,kkh
          print *,' con2: pkl pkh  pkkl pkkh ',pn(i,kl),pn(i,kh)
     &           , pp(i,kkl),pp(i,kkh)
          print *,' con2a: qq tl qql dql ',qq(i,kkl),tl,qql,dql
          th=(pn(i,kh)-pp(i,kkh))/hh(kkh)
          th2=th*th
          th3=th2*th
          th4=th3*th
          dqh=(th4/2.-th3+th)*qi(kkh)
     &       +(th4/4.-2*th3/3.+th2/2.)*hh(kkh)*mmi(kkh)
     &       +(-th4/2.+th3)*qi(kkh+1)
     &       +(th4/4.-th3/3.)*hh(kkh)*mpi(kkh)
          print *,' con2b: qq th dqh ',qq(i,kkh),th,dqh
          dpp  = (1.-tl)*hh(kkl) + th*hh(kkh)
          dpq  = dql*hh(kkl) + dqh*hh(kkh)
          if( kkh-kkl.gt.1 ) then
            do kk=kkl+1,kkh-1
              dpp = dpp + hh(kk)
              dpq = dpq + qq(i,kk)*hh(kk)
              print *,' con2c: qq ',qq(i,kk)
            enddo
          endif
          qn(i,k) = dpq / dpp
          print *,' dpq dpp ',dpq,dpp
          print *,' compare qn and qq ',qn(i,k),qq(i,kkh)
        else
          rc=2
          return
        endif
! next one
        print *,' ----------------------------------------------------'
        kkl = kkh
      enddo
!
      enddo
!
      rc = 0
      return
      end
