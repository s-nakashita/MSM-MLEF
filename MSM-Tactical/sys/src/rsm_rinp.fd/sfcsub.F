      module sfccyc_module
      implicit none
      save
!
!  grib code for each parameter - used in subroutines sfccycle and setrmsk.
!
      integer kpdtsf,kpdwet,kpdsno,kpdzor,kpdais,kpdtg3,kpdplr,kpdgla,      &
     &        kpdmxi,kpdscv,kpdsmc,kpdoro,kpdmsk,kpdstc,kpdacn,kpdveg,      &
     &        kpdvet,kpdsot                                                 &
!clu [+1l] add kpd() for vmn, vmx, slp, abs
     &,       kpdvmn,kpdvmx,kpdslp,kpdabs                                   &
!cggg snow mods start  add snow depth
     &,       kpdsnd
!cggg snow mods end
      parameter(kpdtsf=11,  kpdwet=86, kpdsno=65,  kpdzor=83,               &
!    &          kpdalb=84,  kpdais=91, kpdtg3=11,  kpdplr=224,
     &          kpdais=91,  kpdtg3=11, kpdplr=224,                          &
     &          kpdgla=238, kpdmxi=91, kpdscv=238, kpdsmc=144,              &
     &          kpdoro=8,   kpdmsk=81, kpdstc=11,  kpdacn=91, kpdveg=87,    &
!clu [+1l] add kpd() for vmn, vmx, slp, abs
     &          kpdvmn=255, kpdvmx=255,kpdslp=236, kpdabs=255,              &
     &          kpdvet=225, kpdsot=230,                                     &
!cggg snow mods start
     &          kpdsnd=66 )
!cggg snow mods end
!
      integer, parameter :: kpdalb(4)=(/212,215,213,216/)                   &
     &,                     kpdalf(2)=(/214,217/)
!
      end module sfccyc_module
      subroutine getarea(kgds,dlat,dlon,rslat,rnlat,wlon,elon,ijordr        &
     &,                  me)
      use machine , only : kind_io8,kind_io4
      implicit none
      integer j,me,kgds11
      real (kind=kind_io8) f0lon,f0lat,elon,dlon,dlat,rslat,wlon,rnlat
!
!  get area of the grib record
!
      integer kgds(22)
      logical ijordr
!
      if (me .eq. 0) then
       write(6,*) ' kgds( 1-12)=',(kgds(j),j= 1,12)
       write(6,*) ' kgds(13-22)=',(kgds(j),j=13,22)
      endif
!
      if(kgds(1).eq.0) then                      !  lat/lon grid
!
        if (me .eq. 0) write(6,*) 'lat/lon grid'
        dlat   = float(kgds(10)) * 0.001
        dlon   = float(kgds( 9)) * 0.001
        f0lon  = float(kgds(5))  * 0.001
        f0lat  = float(kgds(4))  * 0.001
        kgds11 = kgds(11)
        if(kgds11.ge.128) then
          wlon = f0lon - dlon*(kgds(2)-1)
          elon = f0lon
          if(dlon*kgds(2).gt.359.99) then
            wlon =f0lon - dlon*kgds(2)
          endif
          dlon   = -dlon
          kgds11 = kgds11 - 128
        else
          wlon = f0lon
          elon = f0lon + dlon*(kgds(2)-1)
          if(dlon*kgds(2).gt.359.99) then
            elon = f0lon + dlon*kgds(2)
          endif
        endif
        if(kgds11.ge.64) then
          rnlat  = f0lat + dlat*(kgds(3)-1)
          rslat  = f0lat
          kgds11 = kgds11 - 64
        else
          rnlat = f0lat
          rslat = f0lat - dlat*(kgds(3)-1)
          dlat  = -dlat
        endif
        if(kgds11.ge.32) then
          ijordr = .false.
        else
          ijordr = .true.
        endif

        if(wlon.gt.180.) wlon = wlon - 360.
        if(elon.gt.180.) elon = elon - 360.
        wlon  = nint(wlon*1000.)  * 0.001
        elon  = nint(elon*1000.)  * 0.001
        rslat = nint(rslat*1000.) * 0.001
        rnlat = nint(rnlat*1000.) * 0.001
        return
!
      elseif(kgds(1).eq.1) then                  !  mercator projection
        write(6,*) 'mercator grid'
        write(6,*) 'cannot process'
        call abort
!
      elseif(kgds(1).eq.2) then                  !  gnomonic projection
        write(6,*) 'gnomonic grid'
        write(6,*) 'error!! gnomonic projection not coded'
        call abort
!
      elseif(kgds(1).eq.3) then                  !  lambert conformal
        write(6,*) 'lambert conformal'
        write(6,*) 'cannot process'
        call abort
      elseif(kgds(1).eq.4) then                  !  gaussian grid
!
        if (me .eq. 0) write(6,*) 'gaussian grid'
        dlat   = 99.
        dlon   = float(kgds( 9)) / 1000.0
        f0lon  = float(kgds(5))  / 1000.0
        f0lat  = 99.
        kgds11 = kgds(11)
        if(kgds11.ge.128) then
          wlon = f0lon
          elon = f0lon
          if(dlon*kgds(2).gt.359.99) then
            wlon = f0lon - dlon*kgds(2)
          endif
          dlon   = -dlon
          kgds11 = kgds11-128
        else
          wlon = f0lon
          elon = f0lon + dlon*(kgds(2)-1)
          if(dlon*kgds(2).gt.359.99) then
            elon = f0lon + dlon*kgds(2)
          endif
        endif
        if(kgds11.ge.64) then
          rnlat  = 99.
          rslat  = 99.
          kgds11 = kgds11 - 64
        else
          rnlat = 99.
          rslat = 99.
          dlat  = -99.
        endif
        if(kgds11.ge.32) then
          ijordr = .false.
        else
          ijordr = .true.
        endif
        return
!
      elseif(kgds(1).eq.5) then                  !  polar strereographic
        write(6,*) 'polar stereographic grid'
        write(6,*) 'cannot process'
        call abort
        return
!
      elseif(kgds(1).eq.13) then                 !  oblique lambert conformal
        write(6,*) 'oblique lambert conformal grid'
        write(6,*) 'cannot process'
        call abort
!
      elseif(kgds(1).eq.50) then                 !  spherical coefficient
        write(6,*) 'spherical coefficient'
        write(6,*) 'cannot process'
        call abort
        return
!
      elseif(kgds(1).eq.90) then                 !  space view perspective
!                                                  (orthographic grid)
        write(6,*) 'space view perspective grid'
        write(6,*) 'cannot process'
        call abort
        return
!
      else                                       !  unknown projection.  abort.
        write(6,*) 'error!! unknown map projection'
        write(6,*) 'kgds(1)=',kgds(1)
        print *,'error!! unknown map projection'
        print *,'kgds(1)=',kgds(1)
        call abort
      endif
!
      return
      end
      subroutine subst(data,imax,jmax,ijmax,dlon,dlat,ijordr,work)
      use machine , only : kind_io8,kind_io4
      implicit none
      integer j,ij,i,ijo,ji,jmax,imax,ijmax
      real (kind=kind_io8) dlat,dlon
!
      logical ijordr
!
      real (kind=kind_io8) data(ijmax), work(ijmax)
!
      if(.not.ijordr.or.                                                        &
     &  (ijordr.and.(dlat.gt.0..or.dlon.lt.0.))) then
        if(.not.ijordr) then
          ij=0
          do j=1,jmax
            do i=1,imax
              ij       = (j-1)*imax+i
              ji       = (i-1)*jmax+j
              work(ij) = data(ji)
            enddo
          enddo
        else
          do j=1,jmax
            do i=1,imax
              ij       = (j-1)*imax+i
              work(ij) = data(ij)
            enddo
          enddo
        endif
        do j=1,jmax
          do i=1,imax
            if(dlat.gt.0..and.dlon.gt.0.) then
              ij = imax*jmax - imax*j + i
            elseif(dlat.gt.0..and.dlon.lt.0.) then
              ij = imax*jmax - (j-1)*imax - imax + i - 1
            elseif(dlat.lt.0..and.dlon.lt.0.) then
              ij = imax*(j-1) + imax - i + 1
            endif
            ijo      = (j-1)*imax + i
            data(ij) = work(ijo)
          enddo
        enddo
      endif
      return
      end
      subroutine la2ga(regin,imxin,jmxin,rinlon,rinlat,rlon,rlat,inttyp,         &
     &                 gauout,len,lmask,rslmsk,slmask                            &
     &,                outlat, outlon,me)
      use machine , only : kind_io8,kind_io4
      implicit none
      real (kind=kind_io8) wei4,wei3,wei2,sum2,sum1,sum3,wei1,sum4,              &
     &                     wsum,tem,wsumiv,sums,sumn,wi2j2,x,y,wi1j1,            &
     &                     wi1j2,wi2j1,rlat,rlon,aphi,                           &
     &                     rnume,alamd,denom
      integer jy,ifill,ix,len,inttyp,me,i,j,jmxin,imxin,jq,jx,j1,j2,             &
     &        ii,i1,i2,kmami,it
      integer nx,kxs,kxt,imxnx
!
!  interpolation from lat/lon or gaussian grid to other lat/lon grid
!
      real (kind=kind_io8) outlon(len),outlat(len),gauout(len),                  &
     &                     slmask(len)
      real (kind=kind_io8) regin (imxin,jmxin),rslmsk(imxin,jmxin)
!
      real (kind=kind_io8)    rinlat(jmxin),  rinlon(imxin)
      integer iindx1(len),    iindx2(len)
      integer jindx1(len),    jindx2(len)
      real (kind=kind_io8)    ddx(len),       ddy(len),   wrk(len)
!
      logical lmask
!
      logical first
      integer   num_threads
      data first /.true./
      save num_threads, first
!
      integer len_thread_m, len_thread, i1_t, i2_t
!
      if (first) then
#ifdef IBMSP
         num_threads    = num_parthds()
#else
         num_threads    = 1
#endif
         first = .false.
      endif
!
!
!     if(me .eq. 0) then
!     print *,'rlon=',rlon,' me=',me
!     print *,'rlat=',rlat,' me=',me,' imxin=',imxin,' jmxin=',jmxin
!     endif
!
!     do j=1,jmxin
!       if(rlat.gt.0.) then
!         rinlat(j) = rlat - float(j-1)*dlain
!       else
!         rinlat(j) = rlat + float(j-1)*dlain
!       endif
!     enddo
!
!     if (me .eq. 0) then
!       print *,'rinlat='
!       print *,(rinlat(j),j=1,jmxin)
!       print *,'rinlon='
!       print *,(rinlon(i),i=1,imxin)
!
!       print *,'outlat='
!       print *,(outlat(j),j=1,len)
!       print *,(outlon(j),j=1,len)
!     endif
!
!     do i=1,imxin
!       rinlon(i) = rlon + float(i-1)*dloin
!     enddo
!
!     print *,'rinlon='
!     print *,(rinlon(i),i=1,imxin)
!
      len_thread_m  = (len+num_threads-1) / num_threads
!
!!$omp parallel do private(i1_t,i2_t,len_thread,it,i,ii,i1,i2)
!!$omp+private(j,j1,j2,jq,ix,jy,nx,kxs,kxt,imxnx,kmami)
!!$omp+private(alamd,denom,rnume,aphi,x,y,wsum,wsumiv,sum1,sum2)
!!$omp+private(sum3,sum4,wi1j1,wi2j1,wi1j2,wi2j2,wei1,wei2,wei3,wei4)
!!$omp+private(sumn,sums)
!!$omp+shared(imxin,jmxin,ifill)
!!$omp+shared(outlon,outlat,wrk,iindx1,rinlon,jindx1,rinlat,ddx,ddy)
!!$omp+shared(rlon,rlat,regin,gauout)
!
      do it=1,num_threads   ! start of threaded loop ...................
        i1_t       = (it-1)*len_thread_m+1
        i2_t       = min(i1_t+len_thread_m-1,len)
        len_thread = i2_t-i1_t+1
!
!       find i-index for interpolation
!
        do i=i1_t, i2_t
          alamd = outlon(i)
          if (alamd .lt. rlon)   alamd = alamd + 360.0
          if (alamd .gt. 360.0+rlon) alamd = alamd - 360.0
          wrk(i)    = alamd
          iindx1(i) = imxin
        enddo
        do i=i1_t,i2_t
          do ii=1,imxin
            if(wrk(i) .ge. rinlon(ii)) iindx1(i) = ii
          enddo
        enddo
        do i=i1_t,i2_t
          i1 = iindx1(i)
          if (i1 .lt. 1) i1 = imxin
          i2 = i1 + 1
          if (i2 .gt. imxin) i2 = 1
          iindx1(i) = i1
          iindx2(i) = i2
          denom     = rinlon(i2) - rinlon(i1)
          if(denom.lt.0.) denom = denom + 360.
          rnume = wrk(i) - rinlon(i1)
          if(rnume.lt.0.) rnume = rnume + 360.
          ddx(i) = rnume / denom
        enddo
!
!  find j-index for interplation
!
        if(rlat.gt.0.) then
          do j=i1_t,i2_t
            jindx1(j)=0
          enddo
          do jx=1,jmxin
            do j=i1_t,i2_t
              if(outlat(j).le.rinlat(jx)) jindx1(j) = jx
            enddo
          enddo
          do j=i1_t,i2_t
            jq = jindx1(j)
            aphi=outlat(j)
            if(jq.ge.1 .and. jq .lt. jmxin) then
              j2=jq+1
              j1=jq
             ddy(j)=(aphi-rinlat(j1))/(rinlat(j2)-rinlat(j1))
            elseif (jq .eq. 0) then
              j2=1
              j1=1
              if(abs(90.-rinlat(j1)).gt.0.001) then
                ddy(j)=(aphi-rinlat(j1))/(90.-rinlat(j1))
              else
                ddy(j)=0.0
              endif
            else
              j2=jmxin
              j1=jmxin
              if(abs(-90.-rinlat(j1)).gt.0.001) then
                ddy(j)=(aphi-rinlat(j1))/(-90.-rinlat(j1))
              else
                ddy(j)=0.0
              endif
            endif
            jindx1(j)=j1
            jindx2(j)=j2
          enddo
        else
          do j=i1_t,i2_t
            jindx1(j) = jmxin+1
          enddo
          do jx=jmxin,1,-1
            do j=i1_t,i2_t
              if(outlat(j).le.rinlat(jx)) jindx1(j) = jx
            enddo
          enddo
          do j=i1_t,i2_t
            jq = jindx1(j)
            aphi=outlat(j)
            if(jq.gt.1 .and. jq .le. jmxin) then
              j2=jq
              j1=jq-1
              ddy(j)=(aphi-rinlat(j1))/(rinlat(j2)-rinlat(j1))
            elseif (jq .eq. 1) then
              j2=1
              j1=1
              if(abs(-90.-rinlat(j1)).gt.0.001) then
                 ddy(j)=(aphi-rinlat(j1))/(-90.-rinlat(j1))
              else
                 ddy(j)=0.0
              endif
            else
              j2=jmxin
              j1=jmxin
              if(abs(90.-rinlat(j1)).gt.0.001) then
                 ddy(j)=(aphi-rinlat(j1))/(90.-rinlat(j1))
              else
                 ddy(j)=0.0
              endif
            endif
            jindx1(j)=j1
            jindx2(j)=j2
          enddo
        endif
!
!     if (me .eq. 0 .and. inttyp .eq. 1) then
!       print *,'la2ga'
!       print *,'iindx1'
!       print *,(iindx1(n),n=1,len)
!       print *,'iindx2'
!       print *,(iindx2(n),n=1,len)
!       print *,'jindx1'
!       print *,(jindx1(n),n=1,len)
!       print *,'jindx2'
!       print *,(jindx2(n),n=1,len)
!       print *,'ddy'
!       print *,(ddy(n),n=1,len)
!       print *,'ddx'
!       print *,(ddx(n),n=1,len)
!     endif
!
        sum1 = 0.
        sum2 = 0.
        sum3 = 0.
        sum4 = 0.
        if (lmask) then
          wei1 = 0.
          wei2 = 0.
          wei3 = 0.
          wei4 = 0.
          do i=1,imxin
            sum1 = sum1 + regin(i,1) * rslmsk(i,1)
            sum2 = sum2 + regin(i,jmxin) * rslmsk(i,jmxin)
            wei1 = wei1 + rslmsk(i,1)
            wei2 = wei2 + rslmsk(i,jmxin)
!
            sum3 = sum3 + regin(i,1) * (1.0-rslmsk(i,1))
            sum4 = sum4 + regin(i,jmxin) * (1.0-rslmsk(i,jmxin))
            wei3 = wei3 + (1.0-rslmsk(i,1))
            wei4 = wei4 + (1.0-rslmsk(i,jmxin))
          enddo
!
          if(wei1.gt.0.) then
            sum1 = sum1 / wei1
          else
            sum1 = 0.
          endif
          if(wei2.gt.0.) then
            sum2 = sum2 / wei2
          else
            sum2 = 0.
          endif
          if(wei3.gt.0.) then
            sum3 = sum3 / wei3
          else
            sum3 = 0.
          endif
          if(wei4.gt.0.) then
            sum4 = sum4 / wei4
          else
            sum4 = 0.
          endif
        else
          do i=1,imxin
            sum1 = sum1 + regin(i,1)
            sum2 = sum2 + regin(i,jmxin)
          enddo
          sum1 = sum1 / imxin
          sum2 = sum2 / imxin
          sum3 = sum1
          sum4 = sum2
        endif
!
!     print *,' sum1=',sum1,' sum2=',sum2
!    *,' sum3=',sum3,' sum4=',sum4
!     print *,' rslmsk=',(rslmsk(i,1),i=1,imxin)
!     print *,' slmask=',(slmask(i),i=1,imxout)
!    *,' j1=',jindx1(1),' j2=',jindx2(1)
!
!
!  inttyp=1  take the closest point value
!
        if(inttyp.eq.1) then

          do i=i1_t,i2_t
            jy = jindx1(i)
            if(ddy(i) .ge. 0.5) jy = jindx2(i)
            ix = iindx1(i)
            if(ddx(i) .ge. 0.5) ix = iindx2(i)
!
!cggg start
!
            if (.not. lmask) then

              gauout(i) = regin(ix,jy)

            else

              if(slmask(i).eq.rslmsk(ix,jy)) then

                gauout(i) = regin(ix,jy)

              else

                i1 = ix
                j1 = jy

! spiral around until matching mask is found.
                do nx=1,jmxin*imxin/2
                  kxs=sqrt(4*nx-2.5)
                  kxt=nx-int(kxs**2/4+1)
                  select case(mod(kxs,4))
                  case(1)
                    ix=i1-kxs/4+kxt
                    jx=j1-kxs/4
                  case(2)
                    ix=i1+1+kxs/4
                    jx=j1-kxs/4+kxt
                  case(3)
                    ix=i1+1+kxs/4-kxt
                    jx=j1+1+kxs/4
                  case default
                    ix=i1-kxs/4
                    jx=j1+kxs/4-kxt
                  end select
                  if(jx.lt.1) then
                    ix=ix+imxin/2
                    jx=2-jx
                  elseif(jx.gt.jmxin) then
                    ix=ix+imxin/2
                    jx=2*jmxin-jx
                  endif
                  ix=modulo(ix-1,imxin)+1
                  if(slmask(i).eq.rslmsk(ix,jx)) then
                    gauout(i) = regin(ix,jx)
                    go to 81
                  endif
                enddo

!cggg here, set the gauout value to be 0, and let's sarah's land
!cggg routine assign a default.

              print*,'no matching mask found ',i,i1,j1,ix,jx
              print*,'set to default value.'
              gauout(i) = 0.0


   81  continue

              end if

            end if

!cggg end

          enddo
          kmami=1
          if (me .eq. 0) call maxmin(gauout(i1_t),len_thread,kmami,         &
     &                        1,1,'gauout')
          cycle
        endif  ! nearest neighbor interpolation

!
!  quasi-bilinear interpolation
!
        ifill = 0
        imxnx = 0
        do i=i1_t,i2_t
          y  = ddy(i)
          j1 = jindx1(i)
          j2 = jindx2(i)
          x  = ddx(i)
          i1 = iindx1(i)
          i2 = iindx2(i)
!
          wi1j1 = (1.-x) * (1.-y)
          wi2j1 =     x  *( 1.-y)
          wi1j2 = (1.-x) *      y
          wi2j2 =     x  *      y
!
          tem = 4.*slmask(i) - rslmsk(i1,j1) - rslmsk(i2,j1)                 &
     &                       - rslmsk(i1,j2) - rslmsk(i2,j2)
          if(lmask .and. abs(tem) .gt. 0.01) then
            if(slmask(i).eq.1.) then
                wi1j1 = wi1j1 * rslmsk(i1,j1)
                wi2j1 = wi2j1 * rslmsk(i2,j1)
                wi1j2 = wi1j2 * rslmsk(i1,j2)
                wi2j2 = wi2j2 * rslmsk(i2,j2)
            else
                wi1j1 = wi1j1 * (1.0-rslmsk(i1,j1))
                wi2j1 = wi2j1 * (1.0-rslmsk(i2,j1))
                wi1j2 = wi1j2 * (1.0-rslmsk(i1,j2))
                wi2j2 = wi2j2 * (1.0-rslmsk(i2,j2))
            endif
          endif
!
          wsum   = wi1j1 + wi2j1 + wi1j2 + wi2j2
          wrk(i) = wsum
          if(wsum.ne.0.) then
            wsumiv = 1./wsum
!
            if(j1.ne.j2) then
              gauout(i) = (wi1j1*regin(i1,j1) + wi2j1*regin(i2,j1) +          &
     &                     wi1j2*regin(i1,j2) + wi2j2*regin(i2,j2))           &
     &                  *wsumiv
            else
!
              if (rlat .gt. 0.0) then
                if (slmask(i) .eq. 1.0) then
                  sumn = sum1
                  sums = sum2
                else
                  sumn = sum3
                  sums = sum4
                endif
                if( j1 .eq. 1) then
                  gauout(i) = (wi1j1*sumn        +wi2j1*sumn        +          &
     &                         wi1j2*regin(i1,j2)+wi2j2*regin(i2,j2))          &
     &                      * wsumiv
                elseif (j1 .eq. jmxin) then
                  gauout(i) = (wi1j1*regin(i1,j1)+wi2j1*regin(i2,j1)+          &
     &                         wi1j2*sums        +wi2j2*sums        )          &
     &                      * wsumiv
                endif
!     print *,' slmask=',slmask(i),' sums=',sums,' sumn=',sumn
!    &,' regin=',regin(i1,j2),regin(i2,j2),' j1=',j1,' j2=',j2
!    &,' wij=',wi1j1, wi2j1, wi1j2, wi2j2,wsumiv
              else
                if (slmask(i) .eq. 1.0) then
                  sums = sum1
                  sumn = sum2
                else
                  sums = sum3
                  sumn = sum4
                endif
                if( j1 .eq. 1) then
                  gauout(i) = (wi1j1*regin(i1,j1)+wi2j1*regin(i2,j1)+          &
     &                         wi1j2*sums        +wi2j2*sums        )          &
     &                      * wsumiv
                elseif (j1 .eq. jmxin) then
                  gauout(i) = (wi1j1*sumn        +wi2j1*sumn        +          &
     &                         wi1j2*regin(i1,j2)+wi2j2*regin(i2,j2))          &
     &                      * wsumiv
                endif
              endif
            endif            ! if j1 .ne. j2
          endif
        enddo
        do i=i1_t,i2_t
          j1 = jindx1(i)
          j2 = jindx2(i)
          i1 = iindx1(i)
          i2 = iindx2(i)
          if(wrk(i) .eq. 0.0) then
            if(.not.lmask) then
              write(6,*) ' la2ga called with lmask=.true. but bad',            &
     &                   ' rslmsk or slmask given'
              call abort
            endif
            ifill = ifill + 1
            if(ifill.le.2) then
              if (me .eq. 0) then
                write(6,*) 'i1,i2,j1,j2=',i1,i2,j1,j2
                write(6,*) 'rslmsk=',rslmsk(i1,j1),rslmsk(i1,j2),               &
     &                               rslmsk(i2,j1),rslmsk(i2,j2)
!               write(6,*) 'i,j=',i,j,' slmask(i)=',slmask(i)
                write(6,*) 'i=',i,' slmask(i)=',slmask(i)                       &
     &,         ' outlon=',outlon(i),' outlat=',outlat(i)
              endif
            endif
! spiral around until matching mask is found.
            do nx=1,jmxin*imxin/2
              kxs=sqrt(4*nx-2.5)
              kxt=nx-int(kxs**2/4+1)
              select case(mod(kxs,4))
              case(1)
                ix=i1-kxs/4+kxt
                jx=j1-kxs/4
              case(2)
                ix=i1+1+kxs/4
                jx=j1-kxs/4+kxt
              case(3)
                ix=i1+1+kxs/4-kxt
                jx=j1+1+kxs/4
              case default
                ix=i1-kxs/4
                jx=j1+kxs/4-kxt
              end select
              if(jx.lt.1) then
                ix=ix+imxin/2
                jx=2-jx
              elseif(jx.gt.jmxin) then
                ix=ix+imxin/2
                jx=2*jmxin-jx
              endif
              ix=modulo(ix-1,imxin)+1
              if(slmask(i).eq.rslmsk(ix,jx)) then
                gauout(i) = regin(ix,jx)
                imxnx=max(imxnx,nx)
                go to 71
              endif
            enddo
!
            write(6,*) ' error!!! no filling value found in la2ga'
            call abort
!
   71       continue
          endif
!
        enddo
      enddo            ! end of threaded loop ...................
!!$omp end parallel do
!
      if(ifill.gt.1) then
        if (me .eq. 0) then
        write(6,*) ' unable to interpolate.  filled with nearest',               &
     &             ' point value at ',ifill,' points  imxnx=',imxnx
        endif
      endif
!
      kmami=1
      if (me .eq. 0) call maxmin(gauout,len,kmami,1,1,'gauout')
!
      return
      end subroutine la2ga
      subroutine rof01(aisfld,len,op,crit)
      use machine , only : kind_io8,kind_io4
      implicit none
      integer i,len
      real (kind=kind_io8) aisfld(len),crit
      character*2 op
!
      if(op.eq.'ge') then
        do i=1,len
          if(aisfld(i).ge.crit) then
            aisfld(i)=1.
          else
            aisfld(i)=0.
          endif
        enddo
      elseif(op.eq.'gt') then
        do i=1,len
          if(aisfld(i).gt.crit) then
            aisfld(i)=1.
          else
            aisfld(i)=0.
          endif
        enddo
      elseif(op.eq.'le') then
        do i=1,len
          if(aisfld(i).le.crit) then
            aisfld(i)=1.
          else
            aisfld(i)=0.
          endif
        enddo
      elseif(op.eq.'lt') then
        do i=1,len
          if(aisfld(i).lt.crit) then
            aisfld(i)=1.
          else
            aisfld(i)=0.
          endif
        enddo
      else
        write(6,*) ' illegal operator in rof01.  op=',op
        call abort
      endif
!
      return
      end
      subroutine setrmsk(kpds5,slmask,igaul,jgaul,wlon,rnlat,               &
     &                   data,imax,jmax,rlnout,rltout,lmask,rslmsk          &
!    &                   data,imax,jmax,dlon,dlat,lmask,rslmsk
!cggg     &,                  gaus,blno, blto, kgds1)
     &,                  gaus,blno, blto, kgds1, kpds4, lbms)  
      use machine , only : kind_io8,kind_io4
      use sfccyc_module
      implicit none
      real (kind=kind_io8) blno,blto,wlon,rnlat,crit,data_max
!     real (kind=kind_io8) blno,dlat,dlon,blto,wlon,rnlat,crit
      integer i,j,ijmax,jgaul,igaul,kpds5,jmax,imax, kgds1, kspla
!cggg
      integer, intent(in)   :: kpds4
      logical*1, intent(in) :: lbms(imax,jmax)
      real*4                :: dummy(imax,jmax)

      real (kind=kind_io8)    slmask(igaul,jgaul)
      real (kind=kind_io8)    data(imax,jmax),rslmsk(imax,jmax)             &
     &,                       rlnout(imax), rltout(jmax)
      real (kind=kind_io8)    a(jmax), w(jmax), radi, dlat, dlon
      logical lmask, gaus
!
!     integer kpdalb(4), kpdalf(2)
!     data kpdalb/212,215,213,216/, kpdalf/214,217/
!     save kpdalb, kpdalf
!
!     set the longitude and latitudes for the grib file
!
      if (kgds1 .eq. 4) then         ! grib file on gaussian grid
        kspla=4
        call splat(kspla, jmax, a, w)
!
        radi = 180.0 / (4.*atan(1.))
        do  j=1,jmax
          rltout(j) = acos(a(j)) * radi
        enddo
!
        if (rnlat .gt. 0.0) then
          do j=1,jmax
            rltout(j) = 90. - rltout(j)
          enddo
        else
          do j=1,jmax
            rltout(j) = -90. + rltout(j)
          enddo
        endif
      elseif (kgds1 .eq. 0) then     ! grib file on lat/lon grid
        dlat = -(rnlat+rnlat) / float(jmax-1)
        do j=1,jmax
         rltout(j) = rnlat + (j-1) * dlat
        enddo
      else                           ! grib file on some other grid
        call abort
      endif
      dlon = 360.0 / imax
      do i=1,imax
        rlnout(i) = wlon + (i-1)*dlon
      enddo
!
!
      ijmax  = imax*jmax
      rslmsk = 0.
!
!  surface temperature
!
      if(kpds5.eq.kpdtsf) then
!       lmask=.false.
        call ga2la(slmask,igaul,jgaul,rslmsk,imax,jmax,wlon,rnlat             &
     &,            rlnout, rltout, gaus, blno, blto)
!    &,            dlon, dlat, gaus, blno, blto)
        crit=0.5
        call rof01(rslmsk,ijmax,'ge',crit)
        lmask=.true.
!
!  bucket soil wetness
!
      elseif(kpds5.eq.kpdwet) then
        call ga2la(slmask,igaul,jgaul,rslmsk,imax,jmax,wlon,rnlat             &
     &,            rlnout, rltout, gaus, blno, blto)
!    &,            dlon, dlat, gaus, blno, blto)
        crit=0.5
        call rof01(rslmsk,ijmax,'ge',crit)
        lmask=.true.
!       write(6,*) 'wet rslmsk'
!       znnt=1.
!       call nntprt(rslmsk,ijmax,znnt)
!
!  snow depth
!
!cggg snow mods start
      elseif(kpds5.eq.kpdsnd) then
        print*,'get mask for snow depth data'
        if(kpds4 == 192) then  ! use the bitmap
          print*,'use bitmap for snow'
          rslmsk = 0.
          do j = 1, jmax
            do i = 1, imax
              if (lbms(i,j)) then
                rslmsk(i,j) = 1.
              end if
            enddo
          enddo
          lmask=.true.
        else
          lmask=.false.
        end if
!cggg snow mods end
!
! snow liq equivalent depth
!
      elseif(kpds5.eq.kpdsno) then
        call ga2la(slmask,igaul,jgaul,rslmsk,imax,jmax,wlon,rnlat             &
     &,            rlnout, rltout, gaus, blno, blto)
!    &,            dlon, dlat, gaus, blno, blto)
        crit=0.5
        call rof01(rslmsk,ijmax,'ge',crit)
        lmask=.true.
!       write(6,*) 'sno rslmsk'
!       znnt=1.
!       call nntprt(rslmsk,ijmax,znnt)
!
!  soil moisture
!
      elseif(kpds5.eq.kpdsmc) then
        call ga2la(slmask,igaul,jgaul,rslmsk,imax,jmax,wlon,rnlat             &
     &,            rlnout, rltout, gaus, blno, blto)
!    &,            dlon, dlat, gaus, blno, blto)
        crit=0.5
        call rof01(rslmsk,ijmax,'ge',crit)
        lmask=.true.
!
!  surface roughness
!
      elseif(kpds5.eq.kpdzor) then
        do j=1,jmax
          do i=1,imax
            rslmsk(i,j)=data(i,j)
          enddo
        enddo
        crit=9.9
        call rof01(rslmsk,ijmax,'lt',crit)
        lmask=.true.
!       write(6,*) 'zor rslmsk'
!       znnt=1.
!       call nntprt(rslmsk,ijmax,znnt)
!
!  albedo
!
!     elseif(kpds5.eq.kpdalb) then
!       do j=1,jmax
!         do i=1,imax
!           rslmsk(i,j)=data(i,j)
!         enddo
!       enddo
!       crit=99.
!       call rof01(rslmsk,ijmax,'lt',crit)
!       lmask=.true.
!       write(6,*) 'alb rslmsk'
!       znnt=1.
!       call nntprt(rslmsk,ijmax,znnt)
!
!  albedo
!
! change by zyf
! interpolate albedo land-sea seperately
      elseif(kpds5.eq.kpdalb(1)) then
!        rslmsk=data
!        crit=0.
!        call rof01(rslmsk,ijmax,'gt',crit)
!        lmask=.true.
       lmask=.false.
      elseif(kpds5.eq.kpdalb(2)) then
!        rslmsk=data
!        crit=0.
!        call rof01(rslmsk,ijmax,'gt',crit)
!        lmask=.true.
       lmask=.false.
      elseif(kpds5.eq.kpdalb(3)) then
!        rslmsk=data
!        crit=0.
!        call rof01(rslmsk,ijmax,'gt',crit)
!        lmask=.true.
       lmask=.false.
      elseif(kpds5.eq.kpdalb(4)) then
!        rslmsk=data
!        crit=0.
!        call rof01(rslmsk,ijmax,'gt',crit)
!        lmask=.true.
       lmask=.false.
!       write(6,*) 'alb rslmsk'
!       znnt=1.
!       call nntprt(rslmsk,ijmax,znnt)
!
!  vegetation fraction for albedo
!
      elseif(kpds5.eq.kpdalf(1)) then
!       rslmsk=data
!       crit=0.
!       call rof01(rslmsk,ijmax,'gt',crit)
!       lmask=.true.
        lmask=.false.
      elseif(kpds5.eq.kpdalf(2)) then
!       rslmsk=data
!       crit=0.
!       call rof01(rslmsk,ijmax,'gt',crit)
!       lmask=.true.
        lmask=.false.
!
!  sea ice
!
      elseif(kpds5.eq.kpdais) then
        lmask=.false.
!       call ga2la(slmask,igaul,jgaul,rslmsk,imax,jmax,wlon,rnlat
!    &,            dlon, dlat, gaus, blno, blto)
!       crit=0.5
!       call rof01(rslmsk,ijmax,'ge',crit)
!
        data_max = 0.0
        do j=1,jmax
          do i=1,imax
              rslmsk(i,j) = data(i,j)
              data_max= max(data_max,data(i,j))
          enddo
        enddo
        crit=1.0
        if (data_max .gt. crit) then
          call rof01(rslmsk,ijmax,'gt',crit)
          lmask=.true.
        else
          lmask=.false.
        endif
!       write(6,*) 'acn rslmsk'
!       znnt=1.
!       call nntprt(rslmsk,ijmax,znnt)
!
!  deep soil temperature
!
      elseif(kpds5.eq.kpdtg3) then
        lmask=.false.
!       call ga2la(slmask,igaul,jgaul,rslmsk,imax,jmax,wlon,rnlat
!    &,            rlnout, rltout, gaus, blno, blto)
!    &,            dlon, dlat, gaus, blno, blto)
!       crit=0.5
!       call rof01(rslmsk,ijmax,'ge',crit)
!       lmask=.true.
!
!  plant resistance
!
!     elseif(kpds5.eq.kpdplr) then
!       call ga2la(slmask,igaul,jgaul,rslmsk,imax,jmax,wlon,rnlat
!    &,            rlnout, rltout, gaus, blno, blto)
!    &,            dlon, dlat, gaus, blno, blto)
!       crit=0.5
!       call rof01(rslmsk,ijmax,'ge',crit)
!       lmask=.true.
!
!       write(6,*) 'plr rslmsk'
!       znnt=1.
!       call nntprt(rslmsk,ijmax,znnt)
!
!  glacier points
!
      elseif(kpds5.eq.kpdgla) then
        lmask=.false.
!
!  max ice extent
!
      elseif(kpds5.eq.kpdmxi) then
        lmask=.false.
!
!  snow cover
!
      elseif(kpds5.eq.kpdscv) then
        call ga2la(slmask,igaul,jgaul,rslmsk,imax,jmax,wlon,rnlat             &
     &,            rlnout, rltout, gaus, blno, blto)
!    &,            dlon, dlat, gaus, blno, blto)
        crit=0.5
        call rof01(rslmsk,ijmax,'ge',crit)
        lmask=.true.
!       write(6,*) 'scv rslmsk'
!       znnt=1.
!       call nntprt(rslmsk,ijmax,znnt)
!
!  sea ice concentration
!
      elseif(kpds5.eq.kpdacn) then
        lmask=.false.
        call ga2la(slmask,igaul,jgaul,rslmsk,imax,jmax,wlon,rnlat             &
     &,            rlnout, rltout, gaus, blno, blto)
!    &,            dlon, dlat, gaus, blno, blto)
        crit=0.5
        call rof01(rslmsk,ijmax,'ge',crit)
        lmask=.true.
!       write(6,*) 'acn rslmsk'
!       znnt=1.
!       call nntprt(rslmsk,ijmax,znnt)
!
!  vegetation cover
!
      elseif(kpds5.eq.kpdveg) then

!cggg
        if (kpds4 == 192) then  ! use the bitmap

!         print*,'use bitmap for greenness'

          rslmsk = 0.
          do j = 1, jmax
            do i = 1, imax
              if (lbms(i,j)) then
                rslmsk(i,jmax-j+1) = 1.  ! need to flip grid in n/s direction
              end if
            enddo
          enddo

          lmask = .true.

        else  ! no bitmap, set mask the old way.

          call ga2la(slmask,igaul,jgaul,rslmsk,imax,jmax,wlon,rnlat             &
     &,              rlnout, rltout, gaus, blno, blto)
          crit=0.5
          call rof01(rslmsk,ijmax,'ge',crit)
          lmask=.true.

       end if
!
!  soil type
!
      elseif(kpds5.eq.kpdsot) then
!       call ga2la(slmask,igaul,jgaul,rslmsk,imax,jmax,wlon,rnlat
!    &,            rlnout, rltout, gaus, blno, blto)
!    &,            dlon, dlat, gaus, blno, blto)


!cggg soil type is zero over water, use this to get a bitmap.

        do j = 1, jmax
          do i = 1, imax
            rslmsk(i,j) = data(i,j)
          enddo
        enddo

        crit=0.1
        call rof01(rslmsk,ijmax,'gt',crit)
        lmask=.true.

!
!  vegetation type
!
      elseif(kpds5.eq.kpdvet) then
!       call ga2la(slmask,igaul,jgaul,rslmsk,imax,jmax,wlon,rnlat
!    &,            rlnout, rltout, gaus, blno, blto)
!    &,            dlon, dlat, gaus, blno, blto)


!cggg veg type is zero over water, use this to get a bitmap.

        do j = 1, jmax
          do i = 1, imax
            rslmsk(i,j) = data(i,j)
          enddo
        enddo

        crit=0.1
        call rof01(rslmsk,ijmax,'gt',crit)
        lmask=.true.
!
!      these are for four new data type added by clu -- not sure its correct!
!
      elseif(kpds5.eq.kpdvmn) then
!
!cggg  greenness is zero over water, use this to get a bitmap.
!
        do j = 1, jmax
          do i = 1, imax
            rslmsk(i,j) = data(i,j)
          enddo
        enddo
!
        crit=0.1
        call rof01(rslmsk,ijmax,'gt',crit)
        lmask=.true.
!cggg        lmask=.false.
!
      elseif(kpds5.eq.kpdvmx) then
!
!cggg  greenness is zero over water, use this to get a bitmap.
!
        do j = 1, jmax
          do i = 1, imax
            rslmsk(i,j) = data(i,j)
          enddo
        enddo
!
        crit=0.1
        call rof01(rslmsk,ijmax,'gt',crit)
        lmask=.true.
!cggg        lmask=.false.
!
      elseif(kpds5.eq.kpdslp) then
!
!cggg slope type is zero over water, use this to get a bitmap.
!
        do j = 1, jmax
          do i = 1, imax
            rslmsk(i,j) = data(i,j)
          enddo
        enddo
!
        crit=0.1
        call rof01(rslmsk,ijmax,'gt',crit)
        lmask=.true.
!cggg        lmask=.false.
!
      elseif(kpds5.eq.kpdabs) then
!
!cggg snow albedo is zero over water, use this to get a bitmap.
!
        do j = 1, jmax
          do i = 1, imax
            rslmsk(i,j) = data(i,j)
          enddo
        enddo
!
        crit=0.1
        call rof01(rslmsk,ijmax,'gt',crit)
        lmask=.true.
!cggg        lmask=.false.
!
      endif
!
      return
      end
      subroutine ga2la(gauin,imxin,jmxin,regout,imxout,jmxout,             &
     &                 wlon,rnlat,rlnout,rltout,gaus,blno, blto)
      use machine , only : kind_io8,kind_io4
      implicit none
      integer i1,i2,j2,ishft,i,jj,j1,jtem,jmxout,imxin,jmxin,imxout,       &
     &        j,iret
      real (kind=kind_io8) alamd,dxin,aphi,x,sum1,sum2,y,dlati,wlon,       &
     &                     rnlat,dxout,dphi,dlat,facns,tem,blno,           &
     &                     blto
!
!  interpolation from lat/lon grid to other lat/lon grid
!
      real (kind=kind_io8) gauin (imxin,jmxin), regout(imxout,jmxout)      &
     &,                    rlnout(imxout), rltout(jmxout)
      logical gaus
!
      real, allocatable :: gaul(:)
      real (kind=kind_io8) ddx(imxout),ddy(jmxout)
      integer iindx1(imxout), iindx2(imxout),                              &
     &        jindx1(jmxout), jindx2(jmxout)
      integer jmxsav,n,kspla
      data    jmxsav/0/
      save    jmxsav, gaul, dlati
      real (kind=kind_io8) radi
      real (kind=kind_io8) a(jmxin), w(jmxin)
!
!
      logical first
      integer   num_threads
      data first /.true./
      save num_threads, first
!
      integer len_thread_m, j1_t, j2_t, it
!
      if (first) then
#ifdef IBMSP
         num_threads    = num_parthds()
#else
         num_threads    = 1
#endif
         first = .false.
      endif
!
      if (jmxin .ne. jmxsav) then
        if (jmxsav .gt. 0) deallocate (gaul, stat=iret)
        allocate (gaul(jmxin))
        jmxsav = jmxin
        if (gaus) then
!jfe      call gaulat(gaul,jmxin)
!jfe
!
          kspla=4
          call splat(kspla, jmxin, a, w)
!
          radi = 180.0 / (4.*atan(1.))
          do  n=1,jmxin
            gaul(n) = acos(a(n)) * radi
          enddo
!jfe
          do j=1,jmxin
            gaul(j) = 90. - gaul(j)
          enddo
        else
          dlat = -2*blto / float(jmxin-1)
          dlati = 1 / dlat
          do j=1,jmxin
           gaul(j) = blto + (j-1) * dlat
          enddo
        endif
      endif
!
!
      dxin  = 360. / float(imxin )
!
      do i=1,imxout
        alamd = rlnout(i)
        i1     = floor((alamd-blno)/dxin) + 1
        ddx(i) = (alamd-blno)/dxin-(i1-1)
        iindx1(i) = modulo(i1-1,imxin) + 1
        iindx2(i) = modulo(i1  ,imxin) + 1
      enddo
!
!
      len_thread_m  = (jmxout+num_threads-1) / num_threads
!
      if (gaus) then
!
!!$omp parallel do private(j1_t,j2_t,it,j1,j2,jj)
!!$omp+private(aphi)
!!$omp+shared(num_threads,len_thread_m)
!!$omp+shared(jmxin,jmxout,gaul,rltout,jindx1,ddy)
!
        do it=1,num_threads   ! start of threaded loop ...................
          j1_t       = (it-1)*len_thread_m+1
          j2_t       = min(j1_t+len_thread_m-1,jmxout)
!
          j2=1
          do 40 j=j1_t,j2_t
            aphi=rltout(j)
            do 50 jj=1,jmxin
              if(aphi.lt.gaul(jj)) go to 50
              j2=jj
              go to 42
   50       continue
   42       continue
            if(j2.gt.2) go to 43
            j1=1
            j2=2
            go to 44
   43       continue
            if(j2.le.jmxin) go to 45
            j1=jmxin-1
            j2=jmxin
            go to 44
   45       continue
            j1=j2-1
   44       continue
            jindx1(j)=j1
            jindx2(j)=j2
            ddy(j)=(aphi-gaul(j1))/(gaul(j2)-gaul(j1))
   40     continue
        enddo             ! end of threaded loop ...................
!!$omp   end parallel do
!
      else
!!$omp parallel do private(j1_t,j2_t,it,j1,j2,jtem)
!!$omp+private(aphi)
!!$omp+shared(num_threads,len_thread_m)
!!$omp+shared(jmxin,jmxout,gaul,rltout,jindx1,ddy,dlati,blto)
!
        do it=1,num_threads   ! start of threaded loop ...................
          j1_t       = (it-1)*len_thread_m+1
          j2_t       = min(j1_t+len_thread_m-1,jmxout)
!
          j2=1
          do 400 j=j1_t,j2_t
            aphi=rltout(j)
            jtem = (aphi - blto) * dlati + 1
            if (jtem .ge. 1 .and. jtem .lt. jmxin) then
              j1 = jtem
              j2 = j1 + 1
              ddy(j)=(aphi-gaul(j1))/(gaul(j2)-gaul(j1))
            elseif (jtem .eq. jmxin) then
              j1 = jmxin
              j2 = jmxin
              ddy(j)=1.0
            else
              j1 = 1
              j2 = 1
              ddy(j)=1.0
            endif
!
            jindx1(j) = j1
            jindx2(j) = j2
  400     continue
        enddo             ! end of threaded loop ...................
!!$omp   end parallel do
      endif
!
!     write(6,*) 'ga2la'
!     write(6,*) 'iindx1'
!     write(6,*) (iindx1(n),n=1,imxout)
!     write(6,*) 'iindx2'
!     write(6,*) (iindx2(n),n=1,imxout)
!     write(6,*) 'jindx1'
!     write(6,*) (jindx1(n),n=1,jmxout)
!     write(6,*) 'jindx2'
!     write(6,*) (jindx2(n),n=1,jmxout)
!     write(6,*) 'ddy'
!     write(6,*) (ddy(n),n=1,jmxout)
!     write(6,*) 'ddx'
!     write(6,*) (ddx(n),n=1,jmxout)
!
!
!!$omp parallel do private(j1_t,j2_t,it,i,i1,i2)
!!$omp+private(j,j1,j2,x,y)
!!$omp+shared(num_threads,len_thread_m)
!!$omp+shared(imxout,iindx1,jindx1,ddx,ddy,gauin,regout)
!
      do it=1,num_threads   ! start of threaded loop ...................
        j1_t       = (it-1)*len_thread_m+1
        j2_t       = min(j1_t+len_thread_m-1,jmxout)
!
        do  j=j1_t,j2_t
          y  = ddy(j)
          j1 = jindx1(j)
          j2 = jindx2(j)
          do i=1,imxout
            x  = ddx(i)
            i1 = iindx1(i)
            i2 = iindx2(i)
            regout(i,j) = (1.-x)*((1.-y)*gauin(i1,j1) + y*gauin(i1,j2))      &
     &                  +     x *((1.-y)*gauin(i2,j1) + y*gauin(i2,j2))
          enddo
        enddo
      enddo             ! end of threaded loop ...................
!!$omp end parallel do
!
      sum1 = 0.
      sum2 = 0.
      do i=1,imxin
        sum1 = sum1 + gauin(i,1)
        sum2 = sum2 + gauin(i,jmxin)
      enddo
      sum1 = sum1 / float(imxin)
      sum2 = sum2 / float(imxin)
!
      if (gaus) then
        if (rnlat .gt. 0.0) then
          do i=1,imxout
            regout(i,     1) = sum1
            regout(i,jmxout) = sum2
          enddo
        else
          do i=1,imxout
            regout(i,     1) = sum2
            regout(i,jmxout) = sum1
          enddo
        endif
      else
        if (blto .lt. 0.0) then
          if (rnlat .gt. 0.0) then
            do i=1,imxout
              regout(i,     1) = sum2
              regout(i,jmxout) = sum1
            enddo
          else
            do i=1,imxout
              regout(i,     1) = sum1
              regout(i,jmxout) = sum2
            enddo
          endif
        else
          if (rnlat .lt. 0.0) then
            do i=1,imxout
              regout(i,     1) = sum2
              regout(i,jmxout) = sum1
            enddo
          else
            do i=1,imxout
              regout(i,     1) = sum1
              regout(i,jmxout) = sum2
            enddo
          endif
        endif
      endif
!
      return
      end
      subroutine clima(iy,im,id,ih,fh,len,lsoil,slmask,outlat,outlon,      &
     &                 fntg3c,fnzorc,fnalbc,fnvegc,fnvetc,fnsotc,fnmskh,   &
     &                 tg3clm,zorclm,albclm,vegclm,vetclm,sotclm,alfclm)
!
      use machine , only : kind_io8,kind_io4
      use sfccyc_module
      implicit none
      real (kind=kind_io8) rjday,wei1x,wei2x,rjdayh,wei2m,wei1m,wei1s,     &
     &                     wei2s,fh,stcmon1s,blto,blno,deltsfc,rjdayh2
      integer jdoy,jday,jh,jdow,mmm,mmp,mm,iret,monend,i,k,jm,jd,iy4,      &
     &        jy,mon1,is2,isx,kpd9,is1,l,nn,mon2,mon,is,                   &
     &        im,id,                                                       &
     &        lugb,iy,len,lsoil,ih,iprnt,me,m1,m2,k1,k2,                   &
     &        jmsk,imsk,j
!clu [+1l] add kpd() for vmn, vmx, slp, abs
!
      character*500 fntsfc,fnwetc,fnsnoc,fnzorc,fnalbc,fnaisc,             &
     &              fntg3c,fnscvc,fnsmcc,fnstcc,fnacnc,fnvegc,             &
     &              fnvetc,fnsotc,fnmskh                                   &
!clu [+1l] add fn()c for vmn, vmx, slp, abs
     &,            fnvmnc,fnvmxc,fnslpc,fnabsc
      real (kind=kind_io8) tsfclm(len),tsfcl2(len),                        &
     &     wetclm(len),snoclm(len),                                        &
     &     zorclm(len),albclm(len,4),aisclm(len),                          &
     &     tg3clm(len),acnclm(len),                                        &
     &     cvclm (len),cvbclm(len),cvtclm(len),                            &
     &     cnpclm(len),                                                    &
     &     smcclm(len,lsoil),stcclm(len,lsoil),                            &
     &     sliclm(len),scvclm(len),vegclm(len),                            &
     &     vetclm(len),sotclm(len),alfclm(len,2)                           &
!clu [+1l] add ()cm for vmn, vmx, slp, abs
     &,    vmnclm(len),vmxclm(len),slpclm(len),absclm(len)              
      real (kind=kind_io8) slmskh(2500*1250)
      real (kind=kind_io8) outlat(len), outlon(len)
!
      real (kind=kind_io8) slmask(len), tsfcl0(len)
!    
      real (kind=kind_io8) zorlmx,zorlmn,zoromx,zoromn,zorsmx,             &
     &                     zorsmn,zorimx,zorimn,zorjmx,zorjmn,             &
     &                     alblmx,alblmn,albomx,albomn,albsmx,             &
     &                     albsmn,albimx,albimn,albjmx,albjmn

      parameter(zorlmx=300.,zorlmn=2.,zoromx=1.0,zoromn=1.e-05,            &
     &          zorsmx=300.,zorsmn=2.,zorimx=1.0,zorimn=1.0,               &
     &          zorjmx=1.0,zorjmn=1.0)
      parameter(alblmx=0.80,alblmn=0.06,albomx=0.06,albomn=0.06,           &
     &          albsmx=0.80,albsmn=0.06,albimx=0.80,albimn=0.06,           &
     &          albjmx=0.80,albjmn=0.06)

!
      logical lanom, gaus, first
!
! dayhf : julian day of the middle of each month
!
      real (kind=kind_io8) dayhf(13)
      data dayhf/ 15.5, 45.0, 74.5,105.0,135.5,166.0,                      &
     &           196.5,227.5,258.0,288.5,319.0,349.5,380.5/
!
      real (kind=kind_io8) fha(5)
      integer ida(8),jda(8)
!
      real (kind=kind_io8), allocatable :: tsf(:,:),sno(:,:),              &
     &                     zor(:,:),wet(:,:),                              &
     &                     ais(:,:), acn(:,:),   scv(:,:), smc(:,:,:),     &
     &                     tg3(:),   alb(:,:,:), alf(:,:),                 &
     &                     vet(:),   sot(:),     tsf2(:),                  &
     &                     veg(:,:), stc(:,:,:)                            &
!clu [+1l] add vmn, vmx, slp, abs
     &,                    vmn(:), vmx(:),  slp(:), abs(:)
!
      integer mon1s, mon2s, sea1s, sea2s, sea1, sea2
      data first/.true./
      data mon1s/0/, mon2s/0/, sea1s/0/, sea2s/0/
!
      save first, tsf, sno, zor, wet,  ais, acn, scv, smc, tg3,            &
     &     alb,   alf, vet, sot, tsf2, veg, stc                            &
!clu [+1l] add vmn, vmx, slp, abs
     &,    vmn,   vmx, slp, abs,                                           &
     &     mon1s, mon2s, sea1s, sea2s, dayhf, k1, k2, m1, m2
!
      logical lprnt
!
      fntsfc=' '
      fnwetc=' '
      fnsnoc=' '
      fnaisc=' '
      fnscvc=' '
      fnsmcc=' '
      fnstcc=' '
      fnacnc=' '
      fnvmnc=' ' 
      fnvmxc=' '
      fnslpc=' '
      fnabsc=' '
!
! move from sfccycle
      lugb=101
      me=0
      call hmskrd(lugb,imsk,jmsk,fnmskh,                                   &
     &              kpdmsk,slmskh,gaus,blno,blto,me)
!
      do i=1,len
        tsfclm(i) = 0.0
        tsfcl2(i) = 0.0
        snoclm(i) = 0.0
        wetclm(i) = 0.0
        zorclm(i) = 0.0
        aisclm(i) = 0.0
        tg3clm(i) = 0.0
        acnclm(i) = 0.0
        cvclm(i)  = 0.0
        cvbclm(i) = 0.0
        cvtclm(i) = 0.0
        cnpclm(i) = 0.0
        sliclm(i) = 0.0
        scvclm(i) = 0.0
!clu [+4l]  add ()clm for vmn, vmx, slp, abs
        vmnclm(i) = 0.0
        vmxclm(i) = 0.0
        slpclm(i) = 0.0
        absclm(i) = 0.0
      enddo
      do k=1,lsoil
        do i=1,len
          smcclm(i,k) = 0.0
          stcclm(i,k) = 0.0
        enddo
      enddo
      do k=1,4
        do i=1,len
          albclm(i,k) = 0.0
        enddo
      enddo
      do k=1,2
        do i=1,len
          alfclm(i,k) = 0.0
        enddo
      enddo
!
      iret   = 0
      monend = 9999
!
      if (first) then
!
!    allocate variables to be saved
!
       allocate (tsf(len,2), sno(len,2),      zor(len,2),                 &
     &           wet(len,2), ais(len,2),      acn(len,2),                 &
     &           scv(len,2), smc(len,lsoil,2),                            &
     &           tg3(len),   alb(len,4,2),    alf(len,2),                 &
     &           vet(len),   sot(len), tsf2(len),                         &
!clu [+1l] add vmn, vmx, slp, abs
     &           vmn(len),   vmx(len), slp(len), abs(len),                &
     &           veg(len,2), stc(len,lsoil,2))
!
!     get tsf climatology for the begining of the forecast
!
        if (fh .gt. 0.0) then
          iy4=iy
          if(iy.lt.101) iy4=1900+iy4
          fha=0
          ida=0
          jda=0
!         fha(2)=nint(fh)
          ida(1)=iy
          ida(2)=im
          ida(3)=id
          ida(5)=ih
          call w3movdat(fha,ida,jda)
          jy=jda(1)
          jm=jda(2)
          jd=jda(3)
          jh=jda(5)
          if (me .eq. 0) write(6,*) ' forecast jy,jm,jd,jh',              &
     &                   jy,jm,jd,jh
          jdow = 0
          jdoy = 0
          jday = 0
          call w3doxdat(jda,jdow,jdoy,jday)
          rjday=jdoy+jda(5)/24.
          if(rjday.lt.dayhf(1)) rjday=rjday+365.
!
          if (me .eq. 0) write(6,*) 'forecast jy,jm,jd,jh=',jy,jm,jd,jh
!
!         for monthly mean climatology
!
          monend = 12
          do mm=1,monend
            mmm=mm
            mmp=mm+1
            if(rjday.ge.dayhf(mmm).and.rjday.lt.dayhf(mmp)) then
               mon1=mmm
               mon2=mmp
               go to 10
            endif
          enddo
          print *,'wrong rjday',rjday
          call abort
   10     continue
          wei1m = (dayhf(mon2)-rjday)/(dayhf(mon2)-dayhf(mon1))
          wei2m = (rjday-dayhf(mon1))/(dayhf(mon2)-dayhf(mon1))
          if(mon2.eq.13) mon2=1
          if (me .eq. 0) print *,'rjday,mon1,mon2,wei1m,wei2m=',         &
     &                   rjday,mon1,mon2,wei1m,wei2m
!
!       read monthly mean climatology of tsf
!
          do nn=1,2
            mon = mon1
            if (nn .eq. 2) mon = mon2
!!!            call fixrdc(lugb,fntsfc,kpdtsf,mon,slmask,                   &
!!!     &                 tsf(1,nn),len,iret                                &
!!!     &,                imsk, jmsk, slmskh, gaus,blno, blto               &
!!!     &,                outlat, outlon, me)
          enddo
!
!  tsf at the begining of forecast i.e. fh=0
!
          do i=1,len
            tsfcl0(i) = wei1m * tsf(i,1) + wei2m * tsf(i,2)
          enddo
        endif
      endif
!
!  compute current jy,jm,jd,jh of forecast and the day of the year
!
      iy4=iy
      if(iy.lt.101) iy4=1900+iy4
      fha    = 0
      ida    = 0
      jda    = 0
      fha(2) = nint(fh)
      ida(1) = iy
      ida(2) = im
      ida(3) = id
      ida(5) = ih
      call w3movdat(fha,ida,jda)
      jy     = jda(1)
      jm     = jda(2)
      jd     = jda(3)
      jh     = jda(5)
      if (me .eq. 0) write(6,*) ' forecast jy,jm,jd,jh,rjday=',          &
     &               jy,jm,jd,jh,rjday
      jdow   = 0
      jdoy   = 0
      jday   = 0
      call w3doxdat(jda,jdow,jdoy,jday)
      rjday  = jdoy+jda(5)/24.
      if(rjday.lt.dayhf(1)) rjday=rjday+365.
!
      if (me .eq. 0) write(6,*) 'forecast jy,jm,jd,jh=',jy,jm,jd,jh
!
!     for monthly mean climatology
!
      monend = 12
      do mm=1,monend
         mmm=mm
         mmp=mm+1
         if(rjday.ge.dayhf(mmm).and.rjday.lt.dayhf(mmp)) then
            mon1=mmm
            mon2=mmp
            go to 20
         endif
      enddo
      print *,'wrong rjday',rjday
      call abort
   20 continue
      wei1m=(dayhf(mon2)-rjday)/(dayhf(mon2)-dayhf(mon1))
      wei2m=(rjday-dayhf(mon1))/(dayhf(mon2)-dayhf(mon1))
      if(mon2.eq.13) mon2=1
      if (me .eq. 0) print *,'rjday,mon1,mon2,wei1m,wei2m=',             &
     &               rjday,mon1,mon2,wei1m,wei2m
!
!     for seasonal mean climatology
!
      monend = 4
      is     = im/3 + 1
      if (is.eq.5) is = 1
      do mm=1,monend
        mmm = mm*3 - 2
        mmp = (mm+1)*3 - 2
        if(rjday.ge.dayhf(mmm).and.rjday.lt.dayhf(mmp)) then
          sea1 = mmm
          sea2 = mmp
          go to 30
        endif
      enddo
      print *,'wrong rjday',rjday
      call abort
   30 continue
      wei1s = (dayhf(sea2)-rjday)/(dayhf(sea2)-dayhf(sea1))
      wei2s = (rjday-dayhf(sea1))/(dayhf(sea2)-dayhf(sea1))
      if(sea2.eq.13) sea2=1
      if (me .eq. 0) print *,'rjday,sea1,sea2,wei1s,wei2s=',            &
     &               rjday,sea1,sea2,wei1s,wei2s
!
!  start reading in climatology and interpolate to the date
!
      if (first) then
!
!     annual mean climatology
!
!  fraction of vegetation field for albedo --  there are two
!  fraction fields in this version: strong zeneith angle dependent
!  and weak zeneith angle dependent
!
        kpd9 = -1
!jfe
        alf=0.
!jfe
        call fixrdc(lugb,fnalbc,kpdalf(1),kpd9,slmask,                  &
     &              alf,len,iret                                        &
     &,             imsk, jmsk, slmskh, gaus,blno, blto                 &
     &,             outlat, outlon, me)
        do i = 1, len
          if(slmask(i).eq.1.) then
            alf(i,2) = 100. - alf(i,1)
          endif
        enddo
!
!  deep soil temperature
!
        if(fntg3c(1:8).ne.'        ') then
          call fixrdc(lugb,fntg3c,kpdtg3,kpd9,slmask,                   &
     &                tg3,len,iret                                      &
     &,               imsk, jmsk, slmskh, gaus,blno, blto               &
     &,               outlat, outlon, me)
        endif
!
!  vegetation type
!
        if(fnvetc(1:8).ne.'        ') then
          call fixrdc(lugb,fnvetc,kpdvet,kpd9,slmask,                   &
     &                vet,len,iret                                      &
     &,               imsk, jmsk, slmskh, gaus,blno, blto               &
     &,               outlat, outlon, me)
          if (me .eq. 0) write(6,*) 'climatological vegetation',        &
     &                              ' type read in.'
        endif
!
!  soil type
!
        if(fnsotc(1:8).ne.'        ') then
          call fixrdc(lugb,fnsotc,kpdsot,kpd9,slmask,                   &
     &                sot,len,iret                                      &
     &,               imsk, jmsk, slmskh, gaus,blno, blto               &
     &,               outlat, outlon, me)
          if (me .eq. 0) write(6,*) 'climatological soil type read in.'
        endif

!clu ----------------------------------------------------------------------
!
!  min vegetation cover
!
        if(fnvmnc(1:8).ne.'        ') then
          call fixrdc(lugb,fnvmnc,kpdvmn,kpd9,slmask,                   &
     &                vmn,len,iret                                      &
     &,               imsk, jmsk, slmskh, gaus,blno, blto               &
     &,               outlat, outlon, me)
          if (me .eq. 0) write(6,*) 'climatological shdmin read in.'
        endif
!
!  max vegetation cover
!
        if(fnvmxc(1:8).ne.'        ') then
          call fixrdc(lugb,fnvmxc,kpdvmx,kpd9,slmask,                   &
     &                vmx,len,iret                                      &
     &,               imsk, jmsk, slmskh, gaus,blno, blto               &
     &,               outlat, outlon, me)
          if (me .eq. 0) write(6,*) 'climatological shdmax read in.'
        endif
!
!  slope type
!
        if(fnslpc(1:8).ne.'        ') then
          call fixrdc(lugb,fnslpc,kpdslp,kpd9,slmask,                   &
     &                slp,len,iret                                      &
     &,               imsk, jmsk, slmskh, gaus,blno, blto               &
     &,               outlat, outlon, me)
          if (me .eq. 0) write(6,*) 'climatological slope read in.'
        endif
!
!  max snow albeod
!
        if(fnabsc(1:8).ne.'        ') then
          call fixrdc(lugb,fnabsc,kpdabs,kpd9,slmask,                   &
     &                abs,len,iret                                      &
     &,               imsk, jmsk, slmskh, gaus,blno, blto               &
     &,               outlat, outlon, me)
          if (me .eq. 0) write(6,*) 'climatological snoalb read in.'
        endif
!clu ----------------------------------------------------------------------
!
        is1 = sea1/3 + 1
        is2 = sea2/3 + 1
        if (is1 .eq. 5) is1 = 1
        if (is2 .eq. 5) is2 = 1
        do nn=1,2
!
!     seasonal mean climatology
          if(nn.eq.1) then
             isx=is1
          else
             isx=is2
          endif
          if(isx.eq.1) kpd9 = 12
          if(isx.eq.2) kpd9 = 3
          if(isx.eq.3) kpd9 = 6
          if(isx.eq.4) kpd9 = 9
!
!     seasonal mean climatology
!
!  albedo
!  there are four albedo fields in this version:
!  two for strong zeneith angle dependent (visible and near ir)
!  and two for weak zeneith angle dependent (vis ans nir)
!
          do k = 1, 4
            call fixrdc(lugb,fnalbc,kpdalb(k),kpd9,slmask,              &
     &                  alb(1,k,nn),len,iret                            &
     &,                 imsk, jmsk, slmskh, gaus,blno, blto             &
     &,                 outlat, outlon, me)
          enddo
!
!     monthly mean climatology
!
          mon = mon1
          if (nn .eq. 2) mon = mon2
!     if(lprnt) print *,' mon1=',mon1,' mon2=',mon2
!
!  tsf at the current time t
!
!!!          call fixrdc(lugb,fntsfc,kpdtsf,mon,slmask,                   &
!!!     &               tsf(1,nn),len,iret                                &
!!!     &,              imsk, jmsk, slmskh, gaus,blno, blto               &
!!!     &,              outlat, outlon, me)
!     if(lprnt) print *,' tsf=',tsf(iprnt,nn),' nn=',nn
!
!  tsf...at time t-deltsfc
!
!     fh2 = fh - deltsfc
!     if (fh2 .gt. 0.0) then
!       call fixrd(lugb,fntsfc,kpdtsf,lclim,slmask,                    
!    &             iy,im,id,ih,fh2,tsfcl2,len,iret
!    &,            imsk, jmsk, slmskh, gaus,blno, blto
!    &,            outlat, outlon, me)
!     else
!       do i=1,len
!         tsfcl2(i) = tsfclm(i)
!       enddo
!     endif
!
!  soil wetness
!
          if(fnwetc(1:8).ne.'        ') then
            call fixrdc(lugb,fnwetc,kpdwet,mon,slmask,                  &
     &                  wet(1,nn),len,iret                              &
     &,                 imsk, jmsk, slmskh, gaus,blno, blto             &
     &,                 outlat, outlon, me)
          elseif(fnsmcc(1:8).ne.'        ') then
            call fixrdc(lugb,fnsmcc,kpdsmc,mon,slmask,                  &
     &                  smc(1,lsoil,nn),len,iret                        &
     &,                 imsk, jmsk, slmskh, gaus,blno, blto             &
     &,                 outlat, outlon, me)
            do l=1,lsoil-1
              do i = 1, len
               smc(i,l,nn) = smc(i,lsoil,nn)
              enddo
            enddo
!
!       call fixrd(lugb,fnsmcc,kpdsmc,lclim,slmask,                    
!    &             iy,im,id,ih,fh,smcclm(1,1),len,iret) 
!    &,            imsk, jmsk, slmskh, gaus,blno, blto
!    &,            outlat, outlon, me)
!       call fixrd(lugb,fnsmcc,kpdsmc,lclim,slmask,
!    &             iy,im,id,ih,fh,smcclm(1,2),len,iret)
!    &,            imsk, jmsk, slmskh, gaus,blno, blto
!    &,            outlat, outlon, me)
          else
            write(6,*) 'climatological soil wetness file not given'
!!!            call abort
          endif
!
!  soil temperature
!
          if(fnstcc(1:8).ne.'        ') then
            call fixrdc(lugb,fnstcc,kpdstc,mon,slmask,                  &
     &                  stc(1,lsoil,nn),len,iret                        &
     &,                 imsk, jmsk, slmskh, gaus,blno, blto             &
     &,                 outlat, outlon, me)
            do l=1,lsoil-1
              do i = 1, len
               stc(i,l,nn) = stc(i,lsoil,nn)
              enddo
            enddo
          endif
!
!  sea ice
!
          if(fnacnc(1:8).ne.'        ') then
            call fixrdc(lugb,fnacnc,kpdacn,mon,slmask,                  &
     &                  acn(1,nn),len,iret                              &
     &,                 imsk, jmsk, slmskh, gaus,blno, blto             &
     &,                 outlat, outlon, me)
          elseif(fnaisc(1:8).ne.'        ') then
            call fixrdc(lugb,fnaisc,kpdais,mon,slmask,                  &
     &                  ais(1,nn),len,iret                              &
     &,                 imsk, jmsk, slmskh, gaus,blno, blto             &
     &,                 outlat, outlon, me)
          else
            write(6,*) 'climatological ice cover file not given'
!!!            call abort
          endif
!
!  snow depth
!
!!!          call fixrdc(lugb,fnsnoc,kpdsno,mon,slmask,                  &
!!!     &                sno(1,nn),len,iret                              &
!!!     &,               imsk, jmsk, slmskh, gaus,blno, blto             &
!!!     &,               outlat, outlon, me)
!
!  snow cover
!
          if(fnscvc(1:8).ne.'        ') then
            call fixrdc(lugb,fnscvc,kpdscv,mon,slmask,                  &
     &                  scv(1,nn),len,iret                              &
     &,                 imsk, jmsk, slmskh, gaus,blno, blto             &
     &,                 outlat, outlon, me)
            write(6,*) 'climatological snow cover read in.'
          endif
!
!  surface roughness
!
          call fixrdc(lugb,fnzorc,kpdzor,mon,slmask,                    &
     &                zor(1,nn),len,iret                                &
     &,               imsk, jmsk, slmskh, gaus,blno, blto               &
     &,               outlat, outlon, me)
!
!  minimum stomatal resistance
!
!     call fixrd(lugb,fnplrc,kpdplr,lclim,slmask,                  
!    &           iy,im,id,ih,fh,plrclm,len,iret
!    &,          imsk, jmsk, slmskh, gaus,blno, blto
!    &,            outlat, outlon, me)
!
!
          do i = 1, len
!                           set clouds climatology to zero
            cvclm (i) = 0.
            cvbclm(i) = 0.
            cvtclm(i) = 0.
!
            cnpclm(i) = 0.  !set canopy water content climatology to zero
          enddo
!
!  layer soil temperature
!
!     elseif(fnstcc(1:8).ne.'        ') then
!       call fixrd(lugb,fnstcc,kpdstc,lclim,slmask,
!    &             iy,im,id,ih,fh,stcclm(1,1),len,iret
!    &,            imsk, jmsk, slmskh, gaus,blno, blto
!    &,            outlat, outlon, me)
!       call fixrd(lugb,fnstcc,kpdstc,lclim,slmask,
!    &             iy,im,id,ih,fh,stcclm(1,2),len,iret
!    &,            imsk, jmsk, slmskh, gaus,blno, blto
!    &,            outlat, outlon, me)
!     else
!       write(6,*) 'climatological soil temp file not given'
!       call abort
!     endif
!
!  vegetation cover
!
          if(fnvegc(1:8).ne.'        ') then
            call fixrdc(lugb,fnvegc,kpdveg,mon,slmask,                  &
     &                  veg(1,nn),len,iret                              &
     &,                 imsk, jmsk, slmskh, gaus,blno, blto             &
     &,                 outlat, outlon, me)
            if (me .eq. 0) write(6,*) 'climatological vegetation',      &
     &                                ' cover read in for mon=',mon
          endif
!     write(0,*) ' endding veg cover read'
!
!         if (lanom) then             !  tsf at forecast hour=0.
!           call fixrd(lugb,fntsfc,kpdtsf,lclim,slmask,               
!    &                 iy,im,id,ih,0.,tsfcl0,len,iret           
!    &,                imsk, jmsk, slmskh, gaus,blno, blto
!    &,                outlat, outlon, me)
!         endif
        enddo
!
      mon1s = mon1 ; mon2s = mon2 ; sea1s = sea1 ; sea2s = sea2
!
      if (me .eq. 0) print *,' mon1s=',mon1s,' mon2s=',mon2s            &
     &,' sea1s=',sea1s,' sea2s=',sea2s
!
        k1  = 1 ; k2 = 2
        m1  = 1 ; m2 = 2
!
        first = .false.
      endif
!
!     to get tsf climatology at the previous call to sfccycle
!
      rjdayh = rjday - deltsfc/24.0
!     if(lprnt) print *,' rjdayh=',rjdayh,' mon1=',mon1,' mon2='
!    &,mon2,' mon1s=',mon1s,' mon2s=',mon2s,' k1=',k1,' k2=',k2
      if (rjdayh .ge. dayhf(mon1)) then
        if (mon2 .eq. 1) mon2 = 13
        wei1x = (dayhf(mon2)-rjdayh)/(dayhf(mon2)-dayhf(mon1))
        wei2x = 1.0 - wei1x
        if (mon2 .eq. 13) mon2 = 1
      else
        rjdayh2 = rjdayh
        if (rjdayh .lt. dayhf(1)) rjdayh2 = rjdayh2 + 365.0
        if (mon1s .eq. mon1) then
          mon1s = mon1 - 1
          if (mon1s .eq. 0) mon1s = 12
          k2  = k1
          k1  = mod(k2,2) + 1
          mon = mon1s
!!!          call fixrdc(lugb,fntsfc,kpdtsf,mon,slmask,                  &
!!!     &               tsf(1,k1),len,iret                               &
!!!     &,              imsk, jmsk, slmskh, gaus,blno, blto              &
!!!     &,              outlat, outlon, me)
        endif
        mon2s = mon1s + 1
!       if (mon2s .eq. 1) mon2s = 13
        wei1x = (dayhf(mon2s)-rjdayh2)/(dayhf(mon2s)-dayhf(mon1s))
        wei2x = 1.0 - wei1x
        if (mon2s .eq. 13) mon2s = 1
        do i=1,len
          tsf2(i) = wei1x * tsf(i,k1) + wei2x * tsf(i,k2)
        enddo
      endif
!
      if (sea1 .ne. sea1s) then
         sea1s = sea1
         sea2s = sea2
         m1    = mod(m1,2) + 1
         m2    = mod(m1,2) + 1
!
!     seasonal mean climatology
!
         isx   = sea2/3 + 1
         if (isx .eq. 5) isx = 1
         if(isx.eq.1) kpd9 = 12
         if(isx.eq.2) kpd9 = 3
         if(isx.eq.3) kpd9 = 6
         if(isx.eq.4) kpd9 = 9
!
!  albedo
!  there are four albedo fields in this version:
!  two for strong zeneith angle dependent (visible and near ir)
!  and two for weak zeneith angle dependent (vis ans nir)
!
          do k = 1, 4
            call fixrdc(lugb,fnalbc,kpdalb(k),kpd9,slmask,              &
     &                  alb(1,k,m2),len,iret                            &
     &,                 imsk, jmsk, slmskh, gaus,blno, blto             &
     &,                 outlat, outlon, me)
          enddo
      endif
      if (mon1 .ne. mon1s) then
         mon1s = mon1
         mon2s = mon2
         k1    = mod(k1,2) + 1
         k2    = mod(k1,2) + 1
!
!     monthly mean climatology
!
          mon = mon2
          nn  = k2
!
!  tsf at the current time t
!
!!!          call fixrdc(lugb,fntsfc,kpdtsf,mon,slmask,                 &
!!!     &               tsf(1,nn),len,iret                              &
!!!     &,              imsk, jmsk, slmskh, gaus,blno, blto             &
!!!     &,              outlat, outlon, me)
!
!  soil wetness
!
          if(fnwetc(1:8).ne.'        ') then
            call fixrdc(lugb,fnwetc,kpdwet,mon,slmask,                   &
     &                  wet(1,nn),len,iret                               &
     &,                 imsk, jmsk, slmskh, gaus,blno, blto              &
     &,                 outlat, outlon, me)
          elseif(fnsmcc(1:8).ne.'        ') then
            call fixrdc(lugb,fnsmcc,kpdsmc,mon,slmask,                   &
     &                  smc(1,lsoil,nn),len,iret                         &
     &,                 imsk, jmsk, slmskh, gaus,blno, blto              &
     &,                 outlat, outlon, me)
            do l=1,lsoil-1
              do i = 1, len
               smc(i,l,nn) = smc(i,lsoil,nn)
              enddo
            enddo
!
!       call fixrd(lugb,fnsmcc,kpdsmc,lclim,slmask,
!    &             iy,im,id,ih,fh,smcclm(1,1),len,iret)
!    &,            imsk, jmsk, slmskh, gaus,blno, blto
!    &,            outlat, outlon, me)
!       call fixrd(lugb,fnsmcc,kpdsmc,lclim,slmask,
!    &             iy,im,id,ih,fh,smcclm(1,2),len,iret)
!    &,            imsk, jmsk, slmskh, gaus,blno, blto
!    &,            outlat, outlon, me)
          else
            write(6,*) 'climatological soil wetness file not given'
!!!            call abort
          endif
!
!  sea ice
!
          if(fnacnc(1:8).ne.'        ') then
            call fixrdc(lugb,fnacnc,kpdacn,mon,slmask,                   &
     &                  acn(1,nn),len,iret                               &
     &,                 imsk, jmsk, slmskh, gaus,blno, blto              &
     &,                 outlat, outlon, me)
          elseif(fnaisc(1:8).ne.'        ') then
            call fixrdc(lugb,fnaisc,kpdais,mon,slmask,                   &
     &                  ais(1,nn),len,iret                               &
     &,                 imsk, jmsk, slmskh, gaus,blno, blto              &
     &,                 outlat, outlon, me)
          else
            write(6,*) 'climatological ice cover file not given'
!!!            call abort
          endif
!
!  snow depth
!
!!!          call fixrdc(lugb,fnsnoc,kpdsno,mon,slmask,                 &
!!!     &                sno(1,nn),len,iret                             &
!!!     &,               imsk, jmsk, slmskh, gaus,blno, blto            &
!!!     &,               outlat, outlon, me)
!
!  snow cover
!
          if(fnscvc(1:8).ne.'        ') then
            call fixrdc(lugb,fnscvc,kpdscv,mon,slmask,                   &
     &                  scv(1,nn),len,iret                               &
     &,                 imsk, jmsk, slmskh, gaus,blno, blto              &
     &,                 outlat, outlon, me)
            write(6,*) 'climatological snow cover read in.'
          endif
!
!  surface roughness
!
          call fixrdc(lugb,fnzorc,kpdzor,mon,slmask,                     &
     &                zor(1,nn),len,iret                                 &
     &,               imsk, jmsk, slmskh, gaus,blno, blto                &
     &,               outlat, outlon, me)
!
!  vegetation cover
!
          if(fnvegc(1:8).ne.'        ') then 
            call fixrdc(lugb,fnvegc,kpdveg,mon,slmask,                   &
     &                  veg(1,nn),len,iret                               &
     &,                 imsk, jmsk, slmskh, gaus,blno, blto              &
     &,                 outlat, outlon, me)
!           if (me .eq. 0) write(6,*) 'climatological vegetation',
!    &                                ' cover read in for mon=',mon
          endif
!
!  minimum stomatal resistance
!
!     call fixrd(lugb,fnplrc,kpdplr,lclim,slmask,
!    &           iy,im,id,ih,fh,plrclm,len,iret
!    &,          imsk, jmsk, slmskh, gaus,blno, blto
!    &,            outlat, outlon, me)
!
      endif
!
!     now perform the time interpolation
!
      do i=1,len
        tsfclm(i) = wei1m * tsf(i,k1) + wei2m * tsf(i,k2)
        snoclm(i) = wei1m * sno(i,k1) + wei2m * sno(i,k2)
        zorclm(i) = wei1m * zor(i,k1) + wei2m * zor(i,k2)
        cvclm(i)  = 0.0
        cvbclm(i) = 0.0
        cvtclm(i) = 0.0
        cnpclm(i) = 0.0
        tsfcl2(i) = tsf2(i)
      enddo
!     if(lprnt) print *,' tsfclm=',tsfclm(iprnt),' wei1m=',wei1m
!    &,' wei2m=',wei2m,' tsfk12=',tsf(iprnt,k1),tsf(iprnt,k2)
!
      if (fh .eq. 0.0) then
        do i=1,len
          tsfcl0(i) = tsfclm(i)
        enddo
      endif
      if (rjdayh .ge. dayhf(mon1)) then
        do i=1,len
          tsf2(i)   = wei1x * tsf(i,k1) + wei2x * tsf(i,k2)
          tsfcl2(i) = tsf2(i)
        enddo
      endif
!     if(lprnt) print *,' tsf2=',tsf2(iprnt),' wei1x=',wei1x
!    &,' wei2x=',wei2x,' tsfk12=',tsf(iprnt,k1),tsf(iprnt,k2)
!    &,' mon1s=',mon1s,' mon2s=',mon2s
!    &,' slmask=',slmask(iprnt)
!
      if(fnacnc(1:8).ne.'        ') then
        do i=1,len
          acnclm(i) = wei1m * acn(i,k1) + wei2m * acn(i,k2)
        enddo
      elseif(fnaisc(1:8).ne.'        ') then
        do i=1,len
          aisclm(i) = wei1m * ais(i,k1) + wei2m * ais(i,k2)
        enddo
      endif
!
      if(fnwetc(1:8).ne.'        ') then
        do i=1,len
          wetclm(i) = wei1m * wet(i,k1) + wei2m * wet(i,k2)
        enddo
      elseif(fnsmcc(1:8).ne.'        ') then
        do k=1,lsoil
          do i=1,len
            smcclm(i,k) = wei1m * smc(i,k,k1) + wei2m * smc(i,k,k2)
          enddo
        enddo
      endif
!
      if(fnscvc(1:8).ne.'        ') then
        do i=1,len
          scvclm(i) = wei1m * scv(i,k1) + wei2m * scv(i,k2)
        enddo
      endif
!
      if(fntg3c(1:8).ne.'        ') then
        do i=1,len
          tg3clm(i) =         tg3(i)
        enddo
      elseif(fnstcc(1:8).ne.'        ') then
        do k=1,lsoil
          do i=1,len
            stcclm(i,k) = wei1m * stc(i,k,k1) + wei2m * stc(i,k,k2)
          enddo
        enddo
      endif
!
      if(fnvegc(1:8).ne.'        ') then
        do i=1,len
          vegclm(i) = wei1m * veg(i,k1) + wei2m * veg(i,k2)
        enddo
      endif
!
      if(fnvetc(1:8).ne.'        ') then
        do i=1,len
          vetclm(i) =         vet(i)
        enddo
      endif
!
      if(fnsotc(1:8).ne.'        ') then
        do i=1,len
          sotclm(i) =         sot(i)
        enddo
      endif


!clu ----------------------------------------------------------------------
!
      if(fnvmnc(1:8).ne.'        ') then
        do i=1,len
          vmnclm(i) =         vmn(i)
        enddo
      endif
!
      if(fnvmxc(1:8).ne.'        ') then
        do i=1,len
          vmxclm(i) =         vmx(i)
        enddo
      endif
!
      if(fnslpc(1:8).ne.'        ') then
        do i=1,len
          slpclm(i) =         slp(i)
        enddo
      endif
!
      if(fnabsc(1:8).ne.'        ') then
        do i=1,len
          absclm(i) =         abs(i)
        enddo
      endif
!clu ----------------------------------------------------------------------
!
      do k=1,4
        do i=1,len
          albclm(i,k) = wei1s * alb(i,k,m1) + wei2s * alb(i,k,m2)
        enddo
      enddo
!
      do k=1,2
        do i=1,len
          alfclm(i,k) = alf(i,k)
        enddo
      enddo
!
!  end of climatology reads
!
!
      albclm=albclm*0.01
      alfclm=alfclm*0.01
      zorclm=zorclm*100
      do i=1,len
         if (slmask(i).eq.1) then
            if (zorclm(i).gt.zorlmx) zorclm(i)=zorlmx
            if (zorclm(i).lt.zorlmn) zorclm(i)=zorlmn
            do j=1,4
            if (albclm(i,j).gt.alblmx) albclm(i,j)=alblmx
            if (albclm(i,j).lt.alblmn) albclm(i,j)=alblmn
            enddo
         else
            if (zorclm(i).gt.zoromx) zorclm(i)=zoromx
            if (zorclm(i).lt.zoromn) zorclm(i)=zoromn
            do j=1,4
            if (albclm(i,j).gt.albomx) albclm(i,j)=albomx
            if (albclm(i,j).lt.albomn) albclm(i,j)=albomn
            enddo
         endif
      enddo
      return
      end
      subroutine fixrdc(lugb,fngrib,kpds5,mon,slmask,                   &
     &                 gdata,len,iret                                   &
     &,                imsk, jmsk, slmskh, gaus,blno, blto              &
     &,                outlat, outlon, me)
      use machine , only : kind_io8,kind_io4
      implicit none
      integer mdata,imax,jmax,ijmax,i,j,n,jret,inttyp,iret,imsk,        &
     &        jmsk,len,lugb,kpds5,mon,lskip,lgrib,ndata,lugi,me,kmami
      real (kind=kind_io8) wlon,elon,rnlat,dlat,dlon,rslat,blno,blto
!
!   read in grib climatology files and interpolate to the input
!   grid.  grib files should allow all the necessary parameters
!   to be extracted from the description records.
!
!
!clu [-1l/+1l] increase the dimension size
!clu  parameter(mdata=2048*1024)
      parameter(mdata=2500*1250)
!
      character*500 fngrib
!     character*500 fngrib, asgnstr
!
      real (kind=kind_io8) slmskh(imsk,jmsk)
!
      real (kind=kind_io8) gdata(len), slmask(len)
      real (kind=kind_io8) data(mdata),work(mdata),rslmsk(mdata)
      real (kind=kind_io8) data4(mdata)
      real (kind=kind_io8), allocatable :: rlngrb(:), rltgrb(:)
!
      logical lmask, yr2kc, gaus, ijordr
      logical*1 lbms(mdata)
!
      integer kpds(1000),kgds(1000)
      integer jpds(1000),jgds(1000), kpds0(1000)
      real (kind=kind_io8) outlat(len), outlon(len)
!
!     integer imax_sv, jmax_sv, wlon_sv, rnlat_sv, kpds1_sv
!     date imax_sv/0/, jmax_sv/0/, wlon_sv/999.0/, rnlat_sv/999.0/
!    &,    kpds1_sv/-1/
!     save imax_sv, jmax_sv, wlon_sv, rnlat_sv, kpds1_sv
!    &,    rlngrb, rltgrb
!
      iret   = 0
!
      if (me .eq. 0) write(6,*) ' in fixrdc for mon=',mon                &
     &,' fngrib=',fngrib
!
      close(lugb)
      call baopenr(lugb,fngrib,iret)
      if (iret .ne. 0) then
        write(6,*) ' error in opening file ',fngrib(1:50)
        print *,'error in opening file ',fngrib(1:50)
        call abort
      endif
      if (me .eq. 0) write(6,*) ' file ',fngrib(1:50),                   &
     &             ' opened. unit=',lugb
!
      lugi = 0
!
      lskip   = -1
      jpds    = -1
      jgds    = -1
      jpds(5) = kpds5
      kpds    = jpds
      call getgbh(lugb,lugi,lskip,jpds,jgds,lgrib,ndata,                 &
     &            lskip,kpds,kgds,iret)
      if (me .eq. 0) then
      write(6,*) ' first grib record.'
      write(6,*) ' kpds( 1-10)=',(kpds(j),j= 1,10)
      write(6,*) ' kpds(11-20)=',(kpds(j),j=11,20)
      write(6,*) ' kpds(21-  )=',(kpds(j),j=21,22)
      endif
      yr2kc     = (kpds(8) / 100) .gt. 0
      kpds0     = jpds
      kpds0(4)  = -1
      kpds0(18) = -1
      if(lgrib.eq.0) then
        write(6,*) ' error in getgbh.  field not found.'
        call abort
      endif
!
!   handling climatology file
!
      lskip   = -1
      n       = 0
      jpds    = kpds0
      jpds(9) = mon
      if(jpds(9).eq.13) jpds(9) = 1
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,            &
     &          kpds,kgds,lbms,data4,jret)
      if (me .eq. 0) write(6,*) ' input grib file dates=',               &
     &              (kpds(i),i=8,11)
      if(jret.eq.0) then
         if(ndata.eq.0) then
           write(6,*) ' error in getgb'
           write(6,*) ' kpds=',kpds
           write(6,*) ' kgds=',kgds
           call abort
         endif
         imax=kgds(2)
         jmax=kgds(3)
         ijmax=imax*jmax
         data(1:ijmax)=data4(1:ijmax)
         if (me .eq. 0) write(6,*) 'imax,jmax,ijmax=',imax,jmax,ijmax
       else
         write(6,*) ' error in getgb'
         call abort
       endif
!
      if (me .eq. 0) then
      write(6,*) ' maxmin of input as is'
      kmami=1
      call maxmin(data,ijmax,kmami,1,1,'data')
      endif
!
      call getarea(kgds,dlat,dlon,rslat,rnlat,wlon,elon,ijordr,me)
      if (me .eq. 0) then
      write(6,*) 'imax,jmax,ijmax,dlon,dlat,ijordr,wlon,rnlat='
      write(6,*)  imax,jmax,ijmax,dlon,dlat,ijordr,wlon,rnlat
      endif
      call subst(data,imax,jmax,ijmax,dlon,dlat,ijordr,work)
!
!   first get slmask over input grid
!
        allocate (rlngrb(imax), rltgrb(jmax))
        call setrmsk(kpds5,slmskh,imsk,jmsk,wlon,rnlat,                  &
     &               data,imax,jmax,rlngrb,rltgrb,lmask,rslmsk           &
!    &               data,imax,jmax,abs(dlon),abs(dlat),lmask,rslmsk
!cggg
     &,                  gaus,blno, blto, kgds(1), kpds(4), lbms)
!       write(6,*) ' kpds5=',kpds5,' lmask=',lmask
!
                         inttyp = 0
        if(kpds5.eq.225) inttyp = 1
        if(kpds5.eq.230) inttyp = 1
!clu [+1l] add slope (=236)
        if(kpds5.eq.236) inttyp = 1  
        if (me .eq. 0) then
        if(inttyp.eq.1) print *, ' nearest grid point used'              &
     &,   ' kpds5=',kpds5, ' lmask = ',lmask
        endif
!
        call la2ga(data,imax,jmax,rlngrb,rltgrb,wlon,rnlat,inttyp,       &
     &             gdata,len,lmask,rslmsk,slmask                         &
     &,            outlat, outlon,me)
!
        deallocate (rlngrb, stat=iret)
        deallocate (rltgrb, stat=iret)
      call baclose(lugb,iret)
!
      return
      end
      subroutine splat(idrt,jmax,slat,wlat)
!$$$  subprogram documentation block
!
! subprogram:  splat      compute latitude functions
!   prgmmr: iredell       org: w/nmc23       date: 96-02-20
!
! abstract: computes cosines of colatitude and gaussian weights
!           for one of the following specific global sets of latitudes.
!             gaussian latitudes (idrt=4)
!             equally-spaced latitudes including poles (idrt=0)
!             equally-spaced latitudes excluding poles (idrt=256)
!           the gaussian latitudes are located at the zeroes of the
!           legendre polynomial of the given order.  these latitudes
!           are efficient for reversible transforms from spectral space.
!           (about twice as many equally-spaced latitudes are needed.)
!           the weights for the equally-spaced latitudes are based on
!           ellsaesser (jam,1966).  (no weight is given the pole point.)
!           note that when analyzing grid to spectral in latitude pairs,
!           if an equator point exists, its weight should be halved.
!           this version invokes the ibm essl matrix solver.
!
! program history log:
!   96-02-20  iredell
!   97-10-20  iredell  adjust precision
!   98-06-11  iredell  generalize precision using fortran 90 intrinsic
! 1998-12-03  iredell  generalize precision further
! 1998-12-03  iredell  use blas calls
!
! usage:    call splat(idrt,jmax,slat,wlat)
!
!   input argument list:
!     idrt     - integer grid identifier
!                (idrt=4 for gaussian grid,
!                 idrt=0 for equally-spaced grid including poles,
!                 idrt=256 for equally-spaced grid excluding poles)
!     jmax     - integer number of latitudes.
!
!   output argument list:
!     slat     - real (jmax) sines of latitude.
!     wlat     - real (jmax) gaussian weights.
!
! subprograms called:
!   dgef         matrix factorization
!   dges         matrix solver
!
! attributes:
!   language: fortran 90
!
!$$$
      use machine , only : kind_io8,kind_io4
      implicit none
      real (kind=kind_io8) pi,r,c
      integer jz,j,n,idrt,jmax,jh
      real (kind=kind_io8) slat(jmax),wlat(jmax)
      integer,parameter:: kd=selected_real_kind(15,45)
      real(kind=kd):: pk(jmax/2),pkm1(jmax/2),pkm2(jmax/2)
      real(kind=kd):: slatd(jmax/2),sp,spmax,eps=10.*epsilon(sp)
      parameter(jz=50)
      real (kind=kind_io8) bz(jz)
      data bz        / 2.4048255577,  5.5200781103,                        &
     &  8.6537279129, 11.7915344391, 14.9309177086, 18.0710639679,         &
     & 21.2116366299, 24.3524715308, 27.4934791320, 30.6346064684,         &
     & 33.7758202136, 36.9170983537, 40.0584257646, 43.1997917132,         &
     & 46.3411883717, 49.4826098974, 52.6240518411, 55.7655107550,         &
     & 58.9069839261, 62.0484691902, 65.1899648002, 68.3314693299,         &
     & 71.4729816036, 74.6145006437, 77.7560256304, 80.8975558711,         &
     & 84.0390907769, 87.1806298436, 90.3221726372, 93.4637187819,         &
     & 96.6052679510, 99.7468198587, 102.888374254, 106.029930916,         &
     & 109.171489649, 112.313050280, 115.454612653, 118.596176630,         &
     & 121.737742088, 124.879308913, 128.020877005, 131.162446275,         &
     & 134.304016638, 137.445588020, 140.587160352, 143.728733573,         &
     & 146.870307625, 150.011882457, 153.153458019, 156.295034268 /
      real (kind=kind_io8):: dlt,d1=1.
      real (kind=kind_io8) awork((jmax+1)/2-1,((jmax+1)/2)-1),             &
     &      bwork(((jmax+1)/2))
      real (kind=kind_io8) awork1((jmax+1)/2,((jmax+1)/2))
      integer(4):: jhe,jho,j0=0
      integer(4) ipvt((jmax+1)/2)
      parameter(pi=3.14159265358979,c=(1.-(2./pi)**2)*0.25)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  gaussian latitudes
      if(idrt.eq.4) then
        jh=jmax/2
        jhe=(jmax+1)/2
        r=1./sqrt((jmax+0.5)**2+c)
        do j=1,min(jh,jz)
          slatd(j)=cos(bz(j)*r)
        enddo
        do j=jz+1,jh
          slatd(j)=cos((bz(jz)+(j-jz)*pi)*r)
        enddo
        spmax=1.
        do while(spmax.gt.eps)
          spmax=0.
          do j=1,jh
            pkm1(j)=1.
            pk(j)=slatd(j)
          enddo
          do n=2,jmax
            do j=1,jh
              pkm2(j)=pkm1(j)
              pkm1(j)=pk(j)
              pk(j)=((2*n-1)*slatd(j)*pkm1(j)-(n-1)*pkm2(j))/n
            enddo
          enddo
          do j=1,jh
            sp=pk(j)*(1.-slatd(j)**2)/(jmax*(pkm1(j)-slatd(j)*pk(j)))
            slatd(j)=slatd(j)-sp
            spmax=max(spmax,abs(sp))
          enddo
        enddo
!dir$ ivdep
        do j=1,jh
          slat(j)=slatd(j)
          wlat(j)=(2.*(1.-slatd(j)**2))/(jmax*pkm1(j))**2
          slat(jmax+1-j)=-slat(j)
          wlat(jmax+1-j)=wlat(j)
        enddo
        if(jhe.gt.jh) then
          slat(jhe)=0.
          wlat(jhe)=2./jmax**2
          do n=2,jmax,2
            wlat(jhe)=wlat(jhe)*n**2/(n-1)**2
          enddo
        endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  equally-spaced latitudes including poles
!mi      elseif(idrt.eq.0) then
!mi        jh=jmax/2
!mi        jhe=(jmax+1)/2
!mi        jho=jhe-1
!mi        dlt=pi/(jmax-1)
!mi        slat(1)=1.
!mi        do j=2,jh
!mi          slat(j)=cos((j-1)*dlt)
!mi        enddo
!mi        do js=1,jho
!mi          do j=1,jho
!mi            awork(js,j)=cos(2*(js-1)*j*dlt)
!mi          enddo
!mi        enddo
!mi        do js=1,jho
!mi          bwork(js)=-d1/(4*(js-1)**2-1)
!mi        enddo
!mi!cjfe   call dgef(awork,jhe,jho,ipvt)
!mi!cjfe   call dges(awork,jhe,jho,ipvt,bwork,j0)
!mi        call dgesv(jho,1,awork,jho,ipvt,bwork,jho,info)
!mi        wlat(1)=0.
!mi        do j=1,jho
!mi          wlat(j+1)=bwork(j)
!mi        enddo
!micdir$ ivdep
!mi        do j=1,jh
!mi          slat(jmax+1-j)=-slat(j)
!mi          wlat(jmax+1-j)=wlat(j)
!mi        enddo
!mi        if(jhe.gt.jh) then
!mi          slat(jhe)=0.
!mi          wlat(jhe)=2.*wlat(jhe)
!mi        endif
!mic - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!mic  equally-spaced latitudes excluding poles
!mi      elseif(idrt.eq.256) then
!mi        jh=jmax/2
!mi        jhe=(jmax+1)/2
!mi        jho=jhe
!mi        dlt=pi/jmax
!mi        slat(1)=1.
!mi        do j=1,jh
!mi          slat(j)=cos((j-0.5)*dlt)
!mi        enddo
!mi        do js=1,jho
!mi          do j=1,jho
!mi            awork1(js,j)=cos(2*(js-1)*(j-0.5)*dlt)
!mi          enddo
!mi        enddo
!mi        do js=1,jho
!mi          bwork(js)=-d1/(4*(js-1)**2-1)
!mi        enddo
!mi!cjfe   call dgef(awork1,jhe,jho,ipvt)
!mi!cjfe   call dges(awork1,jhe,jho,ipvt,bwork,j0)
!mi        call dgesv(jho,1,awork1,jho,ipvt,bwork,1,info)
!mi        wlat(1)=0.
!mi        do j=1,jho
!mi          wlat(j)=bwork(j)
!mi        enddo
!micdir$ ivdep
!mi        do j=1,jh
!mi          slat(jmax+1-j)=-slat(j)
!mi          wlat(jmax+1-j)=wlat(j)
!mi        enddo
!mi        if(jhe.gt.jh) then
!mi          slat(jhe)=0.
!mi          wlat(jhe)=2.*wlat(jhe)
!mi        endif
      endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      return
      end
      subroutine hmskrd(lugb,imsk,jmsk,fnmskh,                          &
     &                  kpds5,slmskh,gausm,blnmsk,bltmsk,me)
      use machine , only : kind_io8,kind_io4
      implicit none
      integer kpds5,me,i,imsk,jmsk,lugb,mdata
!
      character*500 fnmskh
!clu [-1l/+1l] increase the dimension size
!clu  parameter(mdata=2048*1024)
      parameter(mdata=2500*1250)
!
      real (kind=kind_io8) slmskh(mdata)
      logical gausm
      real (kind=kind_io8) blnmsk,bltmsk
!

!clu [-2l/+2l] increase the dimension size
!clu  imsk = 2048
!clu  jmsk = 1024
      imsk = 2500
      jmsk = 1250
      call fixrdg(lugb,imsk,jmsk,fnmskh,                                &
     &            kpds5,slmskh,gausm,blnmsk,bltmsk,me)
      do i=1,imsk*jmsk
         slmskh(i) = nint(slmskh(i))
      enddo
!
      return
      end
      subroutine fixrdg(lugb,idim,jdim,fngrib,                          &
     &                  kpds5,gdata,gaus,blno,blto,me)
      use machine , only : kind_io8,kind_io4
      implicit none
      integer lgrib,n,lskip,jret,j,ndata,lugi,jdim,idim,lugb,           &
     &        iret, me,kpds5,mdata,kdata,i
!
      character*500 fngrib
!clu [-1l/+1l] increase the dimension size
!clu  parameter(mdata=2048*1024)
      parameter(mdata=2500*1250)
!
      real (kind=kind_io8) gdata(idim*jdim)
      logical gaus
      real (kind=kind_io8) blno,blto
      real(kind=kind_io8) data4(idim*jdim)
!
      logical*1 lbms(mdata)
!
      integer kpds(200),kgds(200)
      integer jpds(200),jgds(200), kpds0(200)
!
!     if(me .eq. 0) then
!     write(6,*) ' '
!     write(6,*) '************************************************'
!     endif
!
      close(lugb)
      call baopenr(lugb,fngrib,iret)
      if (iret .ne. 0) then
        write(6,*) ' error in opening file ',trim(fngrib)
        print *,'error in opening file ',trim(fngrib)
        call abort
      endif
      if (me .eq. 0) write(6,*) ' file ',trim(fngrib),                  &
     &              ' opened. unit=',lugb
      lugi    = 0
      lskip   = -1
      n       = 0
      jpds    = -1
      jgds    = -1
      jpds(5) = kpds5
      kpds    = jpds
!
      call getgbh(lugb,lugi,lskip,jpds,jgds,lgrib,ndata,                 &
     &            lskip,kpds,kgds,iret)
!
      if(me .eq. 0) then
        write(6,*) ' first grib record.'
        write(6,*) ' kpds( 1-10)=',(kpds(j),j= 1,10)
        write(6,*) ' kpds(11-20)=',(kpds(j),j=11,20)
        write(6,*) ' kpds(21-  )=',(kpds(j),j=21,22)
      endif
!
      kpds0=jpds
      kpds0(4)=-1
      kpds0(18)=-1
      if(lgrib.eq.0) then
        write(6,*) ' error in getgbh.  field not found.'
        call abort
      endif
      jpds = kpds0
      lskip = -1
      kdata=idim*jdim
      call getgb(lugb,lugi,kdata,lskip,jpds,jgds,ndata,lskip,           &
     &                kpds,kgds,lbms,data4,jret)
!
      if(jret.eq.0) then
        if(ndata.eq.0) then
          write(6,*) ' error in getgb'
          write(6,*) ' kpds=',kpds
          write(6,*) ' kgds=',kgds
          call abort
        endif
        idim=kgds(2)
        jdim=kgds(3)
        gaus=kgds(1).eq.4
        blno=kgds(5)*1.d-3
        blto=kgds(4)*1.d-3
        gdata(1:idim*jdim)=data4(1:idim*jdim)
        if (me .eq. 0) write(6,*) 'idim,jdim=',idim,jdim                &
     &,                ' gaus=',gaus,' blno=',blno,' blto=',blto
      else
        write(6,*) ' error in getgb : jret=',jret
        write(6,*) ' kpds(13)=',kpds(13),' kpds(15)=',kpds(15)
        call abort
      endif
!
      return
      end

