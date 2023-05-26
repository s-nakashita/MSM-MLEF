      subroutine clo89(cldfac,camt,nclds,kbtm,ktop                      &
     &,                l, lp1, imax, nvect)
!
!
!     subroutine clo88 computes cloud transmission functions for the
!  longwave code,using code written by bert katz (301-763-8161).
!  and modified by dan schwarzkopf in december,1988.
!                inputs:          (module block)
!      camt,ktop,kbtm,nclds         radisw
!                output:
!      cldfac                       cldcom
!
!          called by:      radmn or model routine
!          calls    :
!
!
      use machine , only : kind_rad
      implicit none
!
      integer imax,kp,k,lp1,l,nvect
      integer nclds(imax),ktop(imax,lp1),kbtm(imax,lp1)
      real (kind=kind_rad) camt(imax,lp1),cldfac(imax,lp1,lp1)          &
     &,                     cldrow(lp1)
!
       real (kind=kind_rad) cldipt(lp1,lp1,nvect)
!
      real (kind=kind_rad) xcld
      integer iq, itop, jtop, ip, ir, i, j, k1, k2, kt, nc, kb
!
      do iq=1,imax,nvect
!
         itop = iq + (nvect-1)
         if (itop .gt. imax) itop = imax
         jtop = itop - iq + 1
!
         do ip=1,jtop
           ir = iq + ip - 1
           if (nclds(ir).eq.0) then
             do j=1,lp1
               do i=1,lp1
                 cldipt(i,j,ip) = 1.
               enddo
             enddo
           endif
           if (nclds(ir).ge.1) then
             xcld = 1.-camt(ir,2)
             k1   = ktop(ir,2) + 1
             k2   = kbtm(ir,2)
             do j=1,lp1
               cldrow(j) = 1.
             enddo
             do j=1,k2
               cldrow(j) = xcld
             enddo
             kb = max(k1,k2+1)
             do k=kb,lp1
               do kp=1,lp1
                 cldipt(kp,k,ip) = cldrow(kp)
               enddo
             enddo
             do j=1,lp1
               cldrow(j) = 1.
             enddo
             do j=k1,lp1
               cldrow(j) = xcld
             enddo
             kt = min(k1-1,k2)
             do k=1,kt
               do kp=1,lp1
                 cldipt(kp,k,ip) = cldrow(kp)
               enddo
             enddo
             if (k2+1 .le. k1-1) then
               do j=k2+1,k1-1
                 do i=1,lp1
                   cldipt(i,j,ip) = 1.
                 enddo
               enddo
             else if(k1.le.k2) then
               do j=k1,k2
                 do i=1,lp1
                   cldipt(i,j,ip) = xcld
                 enddo
               enddo
             endif
           endif
           if (nclds(ir).ge.2) then
             do nc=2,nclds(ir)
               xcld = 1. - camt(ir,nc+1)
               k1   = ktop(ir,nc+1)+1
               k2   = kbtm(ir,nc+1)
               do j=1,lp1
                 cldrow(j) = 1.
               enddo
               do j=1,k2
                 cldrow(j) = xcld
               enddo
               kb = max(k1,k2+1)
               do k=kb,lp1
                 do kp=1,lp1
                    cldipt(kp,k,ip) = cldipt(kp,k,ip)*cldrow(kp)
!                   cldfip(kp,k)    = cldrow(kp)
                 enddo
               enddo
               do j=1,lp1
                 cldrow(j) = 1.
               enddo
               do j=k1,lp1
                 cldrow(j) = xcld
               enddo
               kt = min(k1-1,k2)
               do k=1,kt
                 do kp=1,lp1
                   cldipt(kp,k,ip) = cldipt(kp,k,ip)*cldrow(kp)
!                  cldfip(kp,k)    = cldrow(kp)
                 enddo
               enddo
!              if(k2+1.le.k1-1) then
!                do j=k2+1,k1-1
!                  do i=1,lp1
!                    cldipt(i,j,ip) = 1.
!                  enddo
!                enddo
               if (k1 .le. k2) then
                 do j=k1,k2
                   do i=1,lp1
                     cldipt(i,j,ip) = cldipt(i,j,ip)*xcld
                   enddo
                 enddo
               endif
!              do j=1,lp1
!                do i=1,lp1
!                  cldipt(i,j,ip) = cldipt(i,j,ip)*cldfip(i,j)
!                enddo
!              enddo
             enddo
           endif
!
         enddo
!
         do j=1,lp1
           do i=1,lp1
             do ip=1,jtop
               ir             = iq + ip - 1
               cldfac(ir,i,j) = cldipt(i,j,ip)
             enddo
           enddo
         enddo
!
      enddo
!
      return
      end
