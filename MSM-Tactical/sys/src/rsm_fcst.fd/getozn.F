      subroutine getozn(lent, lm, o3b, k1, k2, fac, prslk, iflip        &
     &,                 xlat,ko3)
!
!     this code is written by shrinivas moorthi
!
      use machine , only : kind_phys,kind_rad,kind_io4
      use funcphys , only : fpkap
      use physcons, rocp => con_rocp, pi => con_pi
      implicit none
!     include 'constant.h'
!
      integer jmr, blte, dlte, loz
!  4x5 ozone data
!     parameter (jmr=45, blte=-86.0, dlte=4.0)
! geos ozone data
      parameter (jmr=18, blte=-85.0, dlte=10.0, loz=17)
!
      real (kind=kind_rad) p00, daylen
      parameter (p00=1000.0)
      logical geosoz
      parameter (geosoz=.true., daylen=86400.0)
!
      integer lent, lm, k1, k2, iflip, ko3
!
!     locals
!
      real (kind=kind_rad) o3r(jmr,loz,12), o3b(lent,lm)                &
     &,                    prslk(lent,lm),  o3i(lent,loz), pkstr(loz)   &
     &,                    pstr(loz),       xlat(lent)                  &
     &,                    wk1(lent)
!
      integer imond(12), ilat(jmr,12)
      real (kind=kind_io4) pstr4(loz), o3clim4(jmr,loz,12)
!
      logical     first
      data  first/.true./
      data pkstr/loz*0.0/, pstr/loz*0.0/
!     data o3z/jmlz13*0.0/
!
      real (kind=kind_rad) tem, tem1, tem2, tem3, tem4                  &
     &,                    temp, rdg, fac, deglat, elte
!
      save first, pkstr, pstr, o3r, elte
!
      integer i, j, k, l, nm, j1, j2, ll
!
!     if(me .eq. 0) write (6,989) jdat(1),jdat(2),jdat(3),jdat(5)
! 989 format(' updating ozone for ', i4,i3,i3,i3)
!
      if (first) then
         rewind ko3
         elte = blte + (jmr-1) * dlte
         do l=1,loz
           read (ko3,15) pstr4(l)
         enddo
         pstr = pstr4
         do nm=1,12
           do j=1,jmr
             read (ko3,19) imond(nm),ilat(j,nm),                        &
     &                     (o3clim4(j,l,nm), l=1,10)
             read (ko3,20) (o3clim4(j,l,nm), l=11,loz)
           enddo
         enddo
         o3r = o3clim4
         do  nm=1,12
           do l=1,loz
             do j=1,jmr
               o3r(j,l,nm) = o3r(j,l,nm) * 1.655e-6
             enddo
           enddo
         enddo
!
   15    format(f10.3)
   19    format(i2,i4,10f6.2)
   20    format(6x,10f6.2)
!
         print *,' found ozone data for levels pstr=',(pstr(l),l=1,loz)
!        print *,' o3=',(o3r(15,l,1),l=1,loz)
!
         do l=1,loz
           pkstr(l) = fpkap(pstr(l)*100.0)
!          pkstr(l) = (pstr(l)/p00) ** rocp
         enddo
!
         first  = .false.
      endif
!
      rdg = 180.0 / pi
      do i=1,lent
        deglat = xlat(i)*rdg
        if (deglat .gt. blte .and. deglat .lt. elte) then
          tem1 = (deglat - blte)/dlte + 1
          j1   = tem1
          j2   = j1 + 1
          tem1 = tem1 - j1
        elseif (deglat .le. blte) then
          j1 = 1
          j2 = 1
          tem1 = 1.0
        elseif (deglat .ge. elte) then
          j1 = jmr
          j2 = jmr
          tem1 = 1.0
        endif
        tem2 = 1.0 - tem1
        do j=1,loz
          tem3     = tem2*o3r(j1,j,k1) + tem1*o3r(j2,j,k1)
          tem4     = tem2*o3r(j1,j,k2) + tem1*o3r(j2,j,k2)
          o3i(i,j) = tem4*fac          + tem3*(1.0-fac)
        enddo
      enddo
!     do i=1,lent
!       pik(i) = (ps(i)*10.0/p00) ** rocp
!     enddo
!
      do l=1,lm
        ll = l
        if (iflip .eq. 1) ll = lm + 1 -l
        do i=1,lent
          wk1(i) = prslk(i,ll)
        enddo
        do k=1,loz-1
          temp = 1.0 / (pkstr(k+1) - pkstr(k))
          do i=1,lent
            if (wk1(i).gt.pkstr(k) .and. wk1(i).le.pkstr(k+1)) then
              tem      = (pkstr(k+1) - wk1(i)) * temp
              o3b(i,l) = tem * o3i(i,k) + (1.0 - tem) * o3i(i,k+1)
            endif
          enddo
        enddo
        do i=1,lent
          if (wk1(i) .gt. pkstr(loz)) o3b(i,l) = o3i(i,loz)
          if (wk1(i) .lt. pkstr(1))   o3b(i,l) = o3i(i,1)
        enddo
      enddo
!
      return
      end
