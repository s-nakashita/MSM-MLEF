!ccccccccccccccccccccccccccccccccccccccccc
#include <define.h>
#ifdef GTOPO30
      subroutine rtersub(iml,jml,condir)
#else
      subroutine rtersub(iml,jml)
#endif
!ccccccccccccccccccccccccccccccccccccccccc
      use machine , only : kind_io4
#include <paramodel.h>
#include <rscompln.h>
!
      parameter(im=igrd1,jm=jgrd1,iw=iwav1,jw=jwav1)
      parameter(mtnvar=nmtvr)
!
#ifdef GTOPO30
      character*120 condir
#endif
!
      dimension flat(im*jm),flon(im*jm),gclat(jml)                       &                   
     &         ,n00(im*jm),n01(im*jm),n10(im*jm),n11(im*jm)              &
     &         ,d00(im*jm),d01(im*jm),d10(im*jm),d11(im*jm)
      dimension slm(im*jm),oro(im*jm),ors(im*jm),oss(im*jm)
      dimension var(im*jm),vas(im*jm)
      dimension var4(im*jm),oa(im*jm,4),ol(im*jm,4)
      dimension theta(im*jm),gamma(im*jm)
      dimension sigma(im*jm),elvmax(im*jm)
      dimension wave(iw*jw),grids(im*2),grid2(im),smooth(iw*jw)
      dimension tmp(jm),tmp2(im,jm)
      real colrad,wgt,wgtcs,rcs2
      dimension colrad(jml/2),wgt(jml/2),wgtcs(jml/2)
      dimension rcs2(jml/2)
!
#ifndef GTOPO30
      real, allocatable :: olll(:,:),elvv(:)     ! SCC 05/25/05
#else
      real, allocatable :: olll(:,:)
#endif
!
      real(kind=kind_io4) hprimes(im*jm,14),tmps(im*jm)
      real starttime, endtime
!
      equivalence (slm(1),flat(1))
      equivalence (oro(1),flon(1))
      equivalence (ors(1),d00(1))
      equivalence (oss(1),d01(1))
      equivalence (vas(1),d10(1))
!
      im2 = im*2
      jm2 = jm/2
      imm2=(im-1)*2
!
!- open(unit=11,form='formatted',err=900) ! average
!- open(unit=12,form='formatted',err=900) ! variance
!- open(unit=13,form='formatted',err=900) ! maximum
!- open(unit=14,form='formatted',err=900) ! sea-land-lake-mask
!
      print *,'+++++ define model grid lat/lon +++++'
      call setlatlon(flat,flon,delx,dely,dlamda0)
!      print *,'flat1=',(flat(i),i=1,20)
!      print *,'flatjm=',(flat(i),i=im*(jm-1)+1,im*(jm-1)+20)
!     
!  define interpolation from nominal ll grid to model grid
!
!      call glats(jml/2,colrad,wgt,wgtcs,rcs2)
!      do 7 j=1,jml/2
!      gclat(j) = colrad(j)
!      jj = jml - j + 1
!      gclat(jj) = pi - colrad(j)
! 7    continue
!      call g2rini(gclat,flat,flon,iml,jml,
!     1        n00,n01,n10,n11,d00,d01,d10,d11)
       call defg2r(flat,flon,iml,jml,lngrd,                              & 
     &        n00,n01,n10,n11,d00,d01,d10,d11)
      print *,'after g2rini'
      print *,'n00=',(n00(i),i=1,10)
      print *,'n01=',(n01(i),i=1,10)
      print *,'n10=',(n10(i),i=1,10)
      print *,'n11=',(n11(i),i=1,10)
      print *,'d00=',(d00(i),i=1,10)
      print *,'d01=',(d01(i),i=1,10)
      print *,'d10=',(d10(i),i=1,10)
      print *,'d11=',(d11(i),i=1,10)

      print *,'n00=',(n00(i),i=im*(jm-1)+1,im*(jm-1)+10)
      print *,'n01=',(n01(i),i=im*(jm-1)+1,im*(jm-1)+10)
      print *,'n10=',(n10(i),i=im*(jm-1)+1,im*(jm-1)+10)
      print *,'n11=',(n11(i),i=im*(jm-1)+1,im*(jm-1)+10)
      print *,'d00=',(d00(i),i=im*(jm-1)+1,im*(jm-1)+10)
      print *,'d01=',(d01(i),i=im*(jm-1)+1,im*(jm-1)+10)
      print *,'d10=',(d10(i),i=im*(jm-1)+1,im*(jm-1)+10)
      print *,'d11=',(d11(i),i=im*(jm-1)+1,im*(jm-1)+10)
!
!     compute mountain data : oro slm var oc
!
!      print *,'before rmakemt'
#ifndef GTOPO30
      allocate (olll(iml*jml,4),elvv(iml*jml))      ! SCC 05/25/05
      call rmakemt(olll(1,1),olll(1,2),olll(1,3),olll(1,4),iml,jml)
#else
      allocate (olll(iml*jml,4))      ! SCC 05/25/05
!      if(flon(1).eq.flon(im+1)) then
!        flat_s=flat(1)
!        flat_n=flat(im+1)
!        flon_w=flon(1)
!        flon_e=flon(im)
!      else
!        if((flat(1)+flat(1+im))/2.ge.0) then
!          flat_s=flat(1)
!          flat_n=flat(im+im/2)
!          flon_w=flon(im+1)
!          flon_e=flon(im*2)
!        else
!          flat_s=flat(im/2)
!          flat_n=flat(im+1)
!          flon_w=flon(1)
!          flon_e=flon(im)
!        endif
!      endif
      flat_s=minval(flat)
      flat_n=maxval(flat)
      flon_w=minval(flon)
      flon_e=maxval(flon)
      call rmakemt(olll(1,1),olll(1,2),olll(1,3),olll(1,4),iml,jml,        &
     &             flat_s,flat_n,flon_w,flon_e,condir)
#endif
      do n=1,iml*jml
      if( olll(n,2).gt.1.0 ) olll(n,2)=1.0
      enddo
!
      call maxmin(olll(1,1),iml*jml,1,1,1,'in rtersub, olll1-var')
      call maxmin(olll(1,2),iml*jml,1,1,1,'in rtersub, olll2-slm')
      call maxmin(olll(1,3),iml*jml,1,1,1,'in rtersub, olll3-var')
      call maxmin(olll(1,4),iml*jml,1,1,1,'in rtersub, olll4-var4')

      do n = 1,lngrd
        oro(n)   = olll(n00(n),1) * d00(n)                               &        
     &           + olll(n10(n),1) * d10(n)                               &
     &           + olll(n11(n),1) * d11(n)                               &
     &           + olll(n01(n),1) * d01(n)
        slm(n)   = olll(n00(n),2) * d00(n)                               &
     &           + olll(n10(n),2) * d10(n)                               &
     &           + olll(n11(n),2) * d11(n)                               &
     &           + olll(n01(n),2) * d01(n)
        var(n)   = olll(n00(n),3) * d00(n)                               &
     &           + olll(n10(n),3) * d10(n)                               &
     &           + olll(n11(n),3) * d11(n)                               &
     &           + olll(n01(n),3) * d01(n)
        var4(n)  = olll(n00(n),4) * d00(n)                               &
     &           + olll(n10(n),4) * d10(n)                               &
     &           + olll(n11(n),4) * d11(n)                               &
     &           + olll(n01(n),4) * d01(n)
      enddo
      call maxmin(oro,lngrd,1,1,1,'in rtersub, oro')
      call maxmin(slm,lngrd,1,1,1,'in rtersub, slm')
      call maxmin(var,lngrd,1,1,1,'in rtersub, var')
      call maxmin(var4,lngrd,1,1,1,'in rtersub, var4')
!
      do n = 1,lngrd
        slm(n) = min(1.0,float(nint(slm(n))))
        if(slm(n).eq.0.) then
           var(n)  = 0.
           var4(n) = 0.
        endif
      enddo
!
      print *,'after var,var4'
#ifndef GTOPO30
!
! ===  compute mtn principal coord htensr: theta,gamma,sigma
!
      call rmakepc(olll(1,1),olll(1,2),olll(1,3),iml,jml)
      do n = 1,lngrd
        theta(n) = olll(n00(n),1) * d00(n)                                &
     &           + olll(n10(n),1) * d10(n)                                &
     &           + olll(n11(n),1) * d11(n)                                &
     &           + olll(n01(n),1) * d01(n)                                
        gamma(n) = olll(n00(n),2) * d00(n)                                &
     &           + olll(n10(n),2) * d10(n)                                &
     &           + olll(n11(n),2) * d11(n)                                &
     &           + olll(n01(n),2) * d01(n)
        sigma(n) = olll(n00(n),3) * d00(n)                                &
     &           + olll(n10(n),3) * d10(n)                                &
     &           + olll(n11(n),3) * d11(n)                                &
     &           + olll(n01(n),3) * d01(n)
      enddo
      call maxmin(theta,lngrd,1,1,1,'  theta')
      call maxmin(gamma,lngrd,1,1,1,'  gamma')
      call maxmin(sigma,lngrd,1,1,1,'  sigma')
!
      do n = 1,lngrd
        if(slm(n).eq.0.) then
           theta(n) = 0.
           gamma(n) = 0.
           sigma(n) = 0.
        endif
      enddo

!   compute subgridscale mountain data : oa ol
!
      call rmakeoal(olll(1,3),olll,iml,jml)
!
      do k = 1,4
        do n = 1,lngrd
          if(slm(n).eq.1.) then
             ol(n,k)   = olll(n00(n),k) * d00(n)                         &
     &                 + olll(n10(n),k) * d10(n)                         &
     &                 + olll(n11(n),k) * d11(n)                         &
     &                 + olll(n01(n),k) * d01(n)
          else
             ol(n,k)   = 0.0
          endif
        enddo
      enddo
!
      print *,'after oal'
      call rmakeoao(olll,elvv,iml,jml)
!
      do k = 1,4
        do n = 1,lngrd
          if(slm(n).eq.1.) then
             oa(n,k)   = olll(n00(n),k) * d00(n)                         &
     &                 + olll(n10(n),k) * d10(n)                         &
     &                 + olll(n11(n),k) * d11(n)                         &
     &                 + olll(n01(n),k) * d01(n)
          else
             oa(n,k)   = 0.0
          endif
        enddo
      enddo
      print *,'after,im=',im,'jm=',jm,'iw=',iw,'jw=',jw
!
      do n = 1,lngrd
         elvmax(n) = elvv(n00(n)) * d00(n)                               &
     &             + elvv(n10(n)) * d10(n)                               &
     &             + elvv(n11(n)) * d11(n)                               &
     &             + elvv(n01(n)) * d01(n)
      enddo
      call maxmin(oa,lngrd,4,1,4,'  oa')
      call maxmin(ol,lngrd,4,1,4,'  ol')
      call maxmin(elvmax,lngrd,1,1,1,'  elvmax1')
!
        maxc3 = 0
        maxc4 = 0
        maxc5 = 0
        maxc6 = 0
        maxc7 = 0
        maxc8 = 0
      do n = 1,lngrd
         if (elvmax(n) .gt. 3000.) maxc3 = maxc3 +1
         if (elvmax(n) .gt. 4000.) maxc4 = maxc4 +1
         if (elvmax(n) .gt. 5000.) maxc5 = maxc5 +1
         if (elvmax(n) .gt. 6000.) maxc6 = maxc6 +1
         if (elvmax(n) .gt. 7000.) maxc7 = maxc7 +1
         if (elvmax(n) .gt. 8000.) maxc8 = maxc8 +1
      enddo
      print *,' maxc3-8km:',maxc3,maxc4,maxc5,maxc6,maxc7,maxc8
       print *,' ===> replacing elvmax with elvmax-oro <=== '
       print *,' ===> if elvmax<oro replace with proxy <=== '
       print *,' ===> the sum of mean orog (oro) and std dev <=== '
      do n = 1,lngrd
        if (elvmax(n) .lt. oro(n) ) then
!---  subtracting off oro leaves std dev (this should never happen)
       elvmax(n) = max(  2. * var(n),0.)
        else
       elvmax(n) = max( elvmax(n) - oro(n),0.)
        endif
      enddo
      call maxmin(elvmax,lngrd,1,1,1,'  elvmax2')
        maxc3 = 0
        maxc4 = 0
        maxc5 = 0
        maxc6 = 0
        maxc7 = 0
        maxc8 = 0
      do n = 1,lngrd
         if (elvmax(n) .gt. 3000.) maxc3 = maxc3 +1
         if (elvmax(n) .gt. 4000.) maxc4 = maxc4 +1
         if (elvmax(n) .gt. 5000.) maxc5 = maxc5 +1
         if (elvmax(n) .gt. 6000.) maxc6 = maxc6 +1
         if (elvmax(n) .gt. 7000.) maxc7 = maxc7 +1
         if (elvmax(n) .gt. 8000.) maxc8 = maxc8 +1
      enddo
      print *,' after maxc 3-6 km:',maxc3,maxc4,maxc5,maxc6
!
!     do n = 1,lngrd
!       if(slm(n).eq.0.) then
!          elvmax(n) = 0.
!       endif
!     enddo
!
      deallocate (olll,elvv)         ! SCC 05/25/05
#else
      deallocate (olll)         ! SCC 05/25/05
#endif
      call ful2sgl(slm,tmps,lngrd)
      write(51)       (tmps(i) ,i=1,lngrd)
      print *,'after write slm'
!
!  spectral smoothing and lnczs filter for oro
!
      print *,' +++++ do fourier(cos,cos) transform ++++'
      wave = 0.0
!
      call rftini

      oss=oro
      call maxmin(oss,lngrd,1,1,1,'before smooth, oss')

      call ffacosx1(oss,jm,1,1,1)
      call maxmin(oss,lngrd,1,1,1,'before ffccosy1, oss')
      call ffacosy1(oss,wave,1,im,iw,1,1)
       call maxmin(wave,lnwav,1,1,1,'before smooth, wave')

      call lnczs(wave,smooth,iw,jw,order)
      call maxmin(wave,lnwav,1,1,1,'after smooth, wave')

      call ffscosy1(wave,oss,1,iw,im,1,1)
      call maxmin(oss,lngrd,1,1,1,'after ffscosy1, oss')
      call ffscosx1(oss,jm,1,1,1)
!
      call maxmin(oss,lngrd,1,1,1,'after smooth, oss')
!
!      call ful2sgl(oro,tmps,lngrd)
!     write(52)       (tmps(i) ,i=1,lngrd)
#ifndef GTOPO30
      if(mtnvar.gt.1) then
       call ful2sgl(var    ,hprimes(1,1),lngrd)
       call ful2sgl(var4   ,hprimes(1,2),lngrd)
       call ful2sgl(oa,hprimes(1,3),lngrd*4)
       call ful2sgl(ol,hprimes(1,7),lngrd*4)
       call ful2sgl(theta,hprimes(1,11),lngrd)
       call ful2sgl(gamma,hprimes(1,12),lngrd)
       call ful2sgl(sigma,hprimes(1,13),lngrd)
       call ful2sgl(elvmax,hprimes(1,14),lngrd)
       write(53)       hprimes(:,1:mtnvar)
      print *,'after write var10'
      else
#endif
       call ful2sgl(var,tmps,lngrd)
       write(53)       (tmps(i),i=1,lngrd)
#ifndef GTOPO30
      endif
#endif
      call ful2sgl(oss,tmps,lngrd)
      write(55)       (tmps(i) ,i=1,lngrd)
      print *,'after write oss'
      print *,'normal end of rmtn'

      return
      end
