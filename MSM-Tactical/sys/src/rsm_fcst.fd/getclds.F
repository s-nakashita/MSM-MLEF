      subroutine getclds(im,     ix,    levs,  tgrs,  qgrs,             &
     &                   vvel,   rh,    qst,                            &
     &                   slmsk,  rlon,  rlat,                           &
     &                   cvr,    cvtr,  cvbr,   rhcl,                   &
     &                   istrat, prsi,  prsl,   cldtot, cldcnv,         &
     &                   cldsa,  mbota, mtopa,  clw,    ncld)
!
      use machine , only : kind_phys,kind_rad
      use physcons, pi => con_pi
      implicit none
!     include 'constant.h'
!
      integer mcld,nseal,nbin,nlon,nlat
!bl ...     4 tuned cld/rh curves..bl,l,m,h  ...  kac apr96
      parameter (mcld=4,nseal=2,nbin=100,nlon=2,nlat=4)
!
      integer im, ix, i
      integer k,iregnh,isla,kc,lo,nbi,kla,ilsea,ilongt,klo,ikn,ilft,    &
     &        irgt,incv,itop,ibtc,levs,ncld,istrat
      integer mtopa(ix,3),mbota(ix,3)
      integer invr,ivva
!   
!
      real (kind=kind_rad) prsi(ix,levs+1), prsl(ix,levs)
      real (kind=kind_rad) tgrs(ix,levs),   qgrs(ix,levs),              &
     &                     vvel(im,levs),   rh(im,levs)
      real (kind=kind_rad) clw(im,levs),    cldsa(im,5),                &
     &                     qst(im,levs),                                &
     &                     cldtot(im,levs), cldcnv(im,levs)
!     real (kind=kind_rad) pgr,        cvr,        cvtr,       cvbr
      real (kind=kind_rad) cvr(im),    cvtr(im),   cvbr(im)             &
     &,                    slmsk(im),  rlon(im),   rlat(im)
!....
!tune
!...  array added for rh-cl calculation
!     indices for lon,lat,cld type(l,m,h), land/sea respectively
!     nlon=1-2, for eastern and western hemispheres
!     nlat=1-4, for 60n-30n,30n-equ,equ-30s,30s-60s
!     land/sea=1-2 for land(and seaice),sea
!
      real (kind=kind_rad) rhcl (nbin,nlon,nlat,mcld,nseal)
      real (kind=kind_rad) rhcla(nbin,nlon,     mcld,nseal)
      real (kind=kind_rad) rhcld(im,nbin,mcld)
      real (kind=kind_rad) xlabdy(3),xlobdy(3)
!...   xlabdy = lat bndry between tuning regions,+/- xlim for transition
!.     xlobdy = lon bndry between tuning regions
!
      real (kind=kind_rad)  tem, xlatdg, xlondg                         &
     &,                     xlnn, xlss,rhmax,xlim,xrgt,xlft,diflo
!
      save invr,ivva,rhmax,xlabdy,xlobdy,xlim
!
      data xlabdy / 30.e0 , 0.e0 , -30.e0 /
      data xlobdy / 0.e0 , 180.e0 , 360.e0 /
      data xlim / 5.e0 /
!tune
!..... initial rh crit. set 1 for ocean, set 2 for land.
!      invr=0 no lapse rate inversion type cld, =1 wiht it
!      ivva=0 no vertical velocity adj. for low cld, =1 with adj.
      data rhmax/1.00e0/, invr/1/, ivva/1/
!     data istrat,ibl,iconv,iemis,ithk/1,1,1,0,1/
!
!tune
      if (ncld .eq. 0) then    ! do the following only for rh clouds!
        tem    = 180.0 / pi
        do i=1,im
          xlatdg = rlat(i) * tem
          xlondg = rlon(i) * tem
          iregnh = 4
!....  get rh-cld relation for this lat
          do k=1,3
            if (xlatdg .gt. xlabdy(k)) then
              iregnh = k
              go to 215
             end if
          enddo
  215     continue
          do 230 isla=1,nseal
           do 230 kc=1,mcld
            do 230 lo=1,nlon
             do 230 nbi=1,nbin
              rhcla(nbi,lo,kc,isla) = rhcl(nbi,lo,iregnh,kc,isla)
  230     continue
!.....    linear transition between latitudinal regions...
          do 240 kla=1,3
           xlnn = xlabdy(kla)+xlim
           xlss = xlabdy(kla)-xlim
           if (xlatdg.lt.xlnn.and.xlatdg.gt.xlss) then
            do 235 isla=1,nseal
             do 235 kc=1,mcld
              do 235 lo=1,nlon
               do 235 nbi=1,nbin
                rhcla (nbi,lo,kc,isla) =                                &
     &           (rhcl(nbi,lo,kla,kc,isla)-rhcl(nbi,lo,kla+1,kc,isla))  &
     &        * (xlatdg-xlss)/(xlnn-xlss) + rhcl(nbi,lo,kla+1,kc,isla)
  235       continue
           end if
  240     continue
!...   get rh-cld relationship for each grid point, interpolating
!       longitudinally between regions if necessary..
          ilsea = 1
          if (slmsk(i).lt.1.e0) then
            ilsea = 2               !...  open ocean point....
          end if
!...  which hemisphere (e,w)
          ilongt = 1
          if (xlondg.gt.180.e0) ilongt = 2
          do 246 k=1,mcld
            do 241 nbi=1,nbin
              rhcld(i,nbi,k) = rhcla(nbi,ilongt,k,ilsea)
  241       continue
          ikn = 0
          do 243 klo=1,3
            diflo = abs(xlondg-xlobdy(klo))
            if (diflo.lt.xlim) then
              ikn = klo
              go to 244
             end if
  243     continue
          go to 246
  244     continue
          ilft = ilongt
          irgt = ilft + 1
          if (irgt.gt.nlon) irgt = 1
          xlft = xlobdy(ikn) - xlim
          xrgt = xlobdy(ikn) + xlim
          do 245 nbi=1,nbin
            rhcld (i,nbi,k) =                                           &
     &        (rhcla(nbi,ilft,k,ilsea)-rhcla(nbi,irgt,k,ilsea))         &
     &         * (xlondg-xrgt)/(xlft-xrgt)+rhcla(nbi,irgt,k,ilsea)
  245     continue
  246     continue
        enddo
      endif
!tune
!...      vertical motion (cb/sec) in vvel
!...      get model diagnosed clds
!
      call cldjms(ix, im, levs, nbin, mcld,                             &
     &            qgrs, tgrs, vvel, rh, qst,                            &
     &            cvr,  cvtr,  cvbr, prsi, prsl, slmsk,                 &
     &            cldsa, mtopa, mbota, cldtot, cldcnv,                  &
     &            ivva,invr,rlat,rhcld,istrat,clw,ncld)
!
      return
      end
