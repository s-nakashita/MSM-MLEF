!fpp$ noconcur r
      subroutine derivs(syn,dyn,rcs2)
      use physcons, rerth => con_rerth
!ccj
#include <paramodel.h>
!cc
!.................................................................
! syn(1, 0*levs+0*levh+1)  ze
! syn(1, 1*levs+0*levh+1)  di
! syn(1, 2*levs+0*levh+1)  te
! syn(1, 3*levs+0*levh+1)  rq
! syn(1, 3*levs+1*levh+1)  dpdlam
! syn(1, 3*levs+1*levh+2)  dpdphi
! syn(1, 3*levs+1*levh+3)  uln
! syn(1, 4*levs+1*levh+3)  vln
!.................................................................
! dyn(1, 0*levs+0*levh+1)  d(t)/d(phi)
! dyn(1, 1*levs+0*levh+1)  d(rq)/d(phi)
! dyn(1, 1*levs+1*levh+1)  d(t)/d(lam)
! dyn(1, 2*levs+1*levh+1)  d(rq)/d(lam)
! dyn(1, 2*levs+2*levh+1)  d(u)/d(lam)
! dyn(1, 3*levs+2*levh+1)  d(v)/d(lam)
! dyn(1, 4*levs+2*levh+1)  d(u)/d(phi)
! dyn(1, 5*levs+2*levh+1)  d(v)/d(phi)
!.................................................................
      parameter(lots    =5*levs+1*levh+2,lotst=2*levs+1,                &
     &          ksz     =0*levs+0*levh+1,                               &
     &          ksd     =1*levs+0*levh+1,                               &
     &          kst     =2*levs+0*levh+1,                               &
     &          ksr     =3*levs+0*levh+1,                               &
     &          ksplam  =3*levs+1*levh+1,                               &
     &          kspphi  =3*levs+1*levh+2,kstb=3*levs+1*levh+2,          &
     &          ksu     =3*levs+1*levh+3,                               &
     &          ksv     =4*levs+1*levh+3)
      parameter(lotd    =6*levs+2*levh,                                 &
     &          kdtphi  =0*levs+0*levh+1,                               &
     &          kdrphi  =1*levs+0*levh+1,                               &
     &          kdtlam  =1*levs+1*levh+1,                               &
     &          kdrlam  =2*levs+1*levh+1,                               &
     &          kdulam  =2*levs+2*levh+1,                               &
     &          kdvlam  =3*levs+2*levh+1,                               &
     &          kduphi  =4*levs+2*levh+1,                               &
     &          kdvphi  =5*levs+2*levh+1)
!...
       dimension rl(jcap1),rlcs2(jcap1),
     1 syn(lonf22,lots),dyn(lonf22,lotd) 
!.................................................................
        do  ll=1,jcap1
          rl(ll)=float(ll-1)/rerth
        enddo
!
      do i=1,jcap1
       rlcs2(i)=rl(i)*rcs2
      enddo
!
!   calculate t rq u v zonal derivs. by multiplication with i*l
!   note rlcs2=rcs2*l/rerth
!
      do k=1,levs
       do i=1,jcap1
!   d(t)/d(lam)
        dyn(2*i-1,kdtlam-1+k)= -syn(2*i  ,kst-1+k)*rlcs2(i)
        dyn(2*i  ,kdtlam-1+k)=  syn(2*i-1,kst-1+k)*rlcs2(i)
        dyn(lonf+2*i-1,kdtlam-1+k)= -syn(lonf+2*i  ,kst-1+k)*rlcs2(i)
        dyn(lonf+2*i  ,kdtlam-1+k)=  syn(lonf+2*i-1,kst-1+k)*rlcs2(i)
!   d(u)/d(lam)
        dyn(2*i-1,kdulam-1+k)= -syn(2*i  ,ksu-1+k)*rlcs2(i)
        dyn(2*i  ,kdulam-1+k)=  syn(2*i-1,ksu-1+k)*rlcs2(i)
        dyn(lonf+2*i-1,kdulam-1+k)= -syn(lonf+2*i  ,ksu-1+k)*rlcs2(i)
        dyn(lonf+2*i  ,kdulam-1+k)=  syn(lonf+2*i-1,ksu-1+k)*rlcs2(i)
!   d(v)/d(lam)
        dyn(2*i-1,kdvlam-1+k)= -syn(2*i  ,ksv-1+k)*rlcs2(i)
        dyn(2*i  ,kdvlam-1+k)=  syn(2*i-1,ksv-1+k)*rlcs2(i)
        dyn(lonf+2*i-1,kdvlam-1+k)= -syn(lonf+2*i  ,ksv-1+k)*rlcs2(i)
        dyn(lonf+2*i  ,kdvlam-1+k)=  syn(lonf+2*i-1,ksv-1+k)*rlcs2(i)
       enddo
      enddo
!
      do k=1,levh
       do i=1,jcap1
!   d(rq)/d(lam)
        dyn(2*i-1,kdrlam-1+k)= -syn(2*i  ,ksr-1+k)*rlcs2(i)
        dyn(2*i  ,kdrlam-1+k)=  syn(2*i-1,ksr-1+k)*rlcs2(i)
        dyn(lonf+2*i-1,kdrlam-1+k)= -syn(lonf+2*i  ,ksr-1+k)*rlcs2(i)
        dyn(lonf+2*i  ,kdrlam-1+k)=  syn(lonf+2*i-1,ksr-1+k)*rlcs2(i)
       enddo
      enddo
!
      call fti_lonf(syn,dummy,(5*levs+levh+2)*2,1)
!
!   d(t)/d(phi)  d(rq)/d(phi) in s. hemi.
      do k=1,levs
       do i=1,lonf
        dyn(i+lonf,kdtphi-1+k)=-dyn(i+lonf,kdtphi-1+k)
       enddo
      enddo
      do k=1,levh
       do i=1,lonf
        dyn(i+lonf,kdrphi-1+k)=-dyn(i+lonf,kdrphi-1+k)
       enddo
      enddo

!   synthesize temp.    meridional and zonal derivatives
!   synthesize moisture meridional and zonal derivatives
!   synthesize u and v            zonal      derivatives
!
      call fti_lonf(dyn,dummy,(4*levs+2*levh)*2,1)
!
! calculate grid meridional derivatives of u and v.
!
!  cos*d(u)/d(theta)= d(v)/d(lam)-a*zeta*cos**2
!  cos*d(v)/d(theta)=-d(u)/d(lam)+a*divr*cos**2
!
      do k=1,levs
       do j=1,lonf2
        dyn(j,kduphi-1+k)= dyn(j,kdvlam-1+k)-syn(j,ksz-1+k)
        dyn(j,kdvphi-1+k)=-dyn(j,kdulam-1+k)+syn(j,ksd-1+k)
       enddo
      enddo
!
      return
      end
