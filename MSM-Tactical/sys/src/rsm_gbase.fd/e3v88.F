!     subroutine e3v88 computes nearby layer transmissivities for
!  h2o using a table lookup of the pre-computed e3 function
! ( described in ref. (4)).
!         inputs:                 (module blocks,args.)
!       tv,av                      argument list
!       em3                        tabcom
!          outputs:
!       emv                        argument list
!
!       called by  :    fst88
!       calls      :    alog10h,alog10v
!
      subroutine e3v88(emv,tv,av, llp1, imax, em3v)
!fpp$ noconcur r
!
      use machine , only : kind_rad
      use hcon
      implicit none
!
      integer  llp1,imax
!
!    dimensions of arrays in argument list
      real (kind=kind_rad) emv(imax,llp1), tv(imax,llp1)                &
     &,                     av(imax,llp1),em3v(5040)
!
      real (kind=kind_rad) fxo, tmp3, dt, fyo, du, ww1, ww2
      integer k, i, it
!
!---the following loop replaces a double loop over i (1-imax) and
!   k (1-llp1)
!
      do k=1,llp1
        do i=1,imax
          fxo  = aint(tv(i,k)*hp1)
          tmp3 = log10(av(i,k)) + h16e1
          dt   = tv(i,k) - ten*fxo
          fyo  = aint(tmp3*ten)
          du   = tmp3 - hp1*fyo
!---obtain index for table lookup; this value will have to be
!   decremented by 9 to account for table temps starting at 100k.
          it   = fxo + fyo*h28e1
          ww1  = ten - dt
          ww2  = hp1 - du
          emv(i,k) = ww2 * (ww1*em3v(it-9)  + dt*em3v(it-8))            &
     &             + du  * (ww1*em3v(it+19) + dt*em3v(it+20))
        enddo
      enddo
!
      return
      end
