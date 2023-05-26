      module module_rascnv
!
      use machine , only : kind_phys
      implicit none

!
      logical revap, cumfrc
      logical wrkfun, calkbl, crtfun, updret, botop
      real(kind=kind_phys) frac, crtmsf, max_neg_bouy, rhfacs, rhfacl   &
     &,                    face, delx,   ddfac
      parameter (frac=0.5, crtmsf=0.0)
!     parameter (max_neg_bouy=0.25, revap=.true., cumfrc=.true.)
!     parameter (max_neg_bouy=0.20, revap=.true., cumfrc=.true.)
      parameter (max_neg_bouy=0.15, revap=.true., cumfrc=.true.)
!     parameter (max_neg_bouy=0.10, revap=.true., cumfrc=.true.)
!     parameter (max_neg_bouy=0.15, revap=.false., cumfrc=.false.)
      parameter (wrkfun = .false.,  updret = .false.)
      parameter (crtfun = .true.,   calkbl = .true., botop=.true.)
!
!     parameter (rhfacs=0.70, rhfacl=0.70)
!     parameter (rhfacs=0.75, rhfacl=0.75)
      parameter (rhfacs=0.80, rhfacl=0.80)
!     parameter (rhfacs=0.85, rhfacl=0.85)
      parameter (face=5.0, delx=10000.0, ddfac=face*delx*0.001)
!
      end module module_rascnv
