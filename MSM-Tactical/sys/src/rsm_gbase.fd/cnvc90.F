      subroutine cnvc90(clstp,im,ix,rn,kbot,ktop,km,prsi,               &
     &                   acv,acvb,acvt,cv,cvb,cvt)
!c
      use machine, only :kind_phys
!     use machine_rad, only :kind_phys
      implicit none
      integer              i,ibot,im,itop,km,lc,lz,n,ncc,ix
      real(kind=kind_phys) ah,cc1,cc2,clstp,cvb0,p1,p2,rkbot,rktop,val
!c
      integer              kbot(ix),ktop(ix)
      real(kind=kind_phys) rn(ix),  acv(ix), acvb(ix), acvt(ix),        &
     &                     cv(ix),  cvb(ix), cvt(ix)
      real(kind=kind_phys) prsi(ix,km+1)
      integer              nmd(ix)
      real(kind=kind_phys) pmd(ix)
      parameter(ncc=9)
      real(kind=kind_phys) cc(ncc),p(ncc)
      data cc/0.,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8/
      data p/.14,.31,.70,1.6,3.4,7.7,17.,38.,85./
      data cvb0/100./
      save cc,p,cvb0
!
      lz=0
      lc=0
      if(clstp.ge.1000.) lz=1
      if(clstp.ge.1100..or.(clstp.lt.1000..and.clstp.ge.100.)) lc=1
      ah=mod(clstp,100.)
      if(lz.ne.0) then
        do i=1,im
          acv(i)  = 0.
          acvb(i) = cvb0
          acvt(i) = 0.
        enddo
      endif
      if(lc.ne.0) then
        do i=1,im
          if(rn(i).gt.0.) then
            acv(i)  = acv(i)+rn(i)
            acvb(i) = min(acvb(i),float(kbot(i)))
            acvt(i) = max(acvt(i),float(ktop(i)))
          endif
        enddo
      endif
      if(ah.gt.0.01.and.ah.lt.99.99) then
        do i=1,im
          if(acv(i).gt.0.) then
!           cvb(i) = acvb(i)
!           cvt(i) = acvt(i)
!....   convert cvt and cvb to pressures
            itop   = nint(acvt(i))
            cvt(i) = prsi(i,itop+1)
            ibot   = nint(acvb(i))
            cvb(i) = prsi(i,ibot)
          else
!           cvb(i) = cvb0
            cvb(i) = 0.
            cvt(i) = 0.
          endif
          pmd(i)   = acv(i)*(24.e+3/ah)
          nmd(i)   = 0
        enddo
        do n=1,ncc
          do i=1,im
            if(pmd(i).gt.p(n)) nmd(i) = n
          enddo
        enddo
        do i=1,im
          if(nmd(i).eq.0) then
            cv(i)  = 0.
!           cvb(i) = cvb0
            cvb(i) = 0.
            cvt(i) = 0.
          elseif(nmd(i).eq.ncc) then
            cv(i)  = cc(ncc)
          else
            cc1    = cc(nmd(i))
            cc2    = cc(nmd(i)+1)
            p1     = p(nmd(i))
            p2     = p(nmd(i)+1)
            cv(i)  = cc1 + (cc2-cc1)*(pmd(i)-p1)/(p2-p1)
          endif
        enddo
      endif
      return
      end
