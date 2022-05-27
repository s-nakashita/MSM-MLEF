!     parameter settings for the longwave and shortwave radiation code:
!          l      =  no. vertical levels (also levr) in model
!
      integer l,lp1,lp1v,ll,llp1,nvect

      parameter (l=levr, lp1=l+1, lp1v=lp1*(1+2*l/2))
      parameter (ll=l+l, llp1=ll+1)
      parameter (nvect=1)
!
!     lw_type is an integer which determines which longwave radiation
!             package to call.  lw_type=0 calls gfdl longwave.
!                               lw_type=1 calls rrtm
!
      integer   lw_type
      parameter (lw_type=1)
