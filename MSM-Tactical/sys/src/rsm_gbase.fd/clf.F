      function clf(prate)
!
      use machine , only : kind_phys
      implicit none
      real(kind=kind_phys) prate, clf
!
      real (kind=kind_phys), parameter :: ccf1=0.30, ccf2=0.09          &
     &,                                   ccf3=0.04, ccf4=0.01          &
     &,                                   pr1=1.0,   pr2=5.0            &
     &,                                   pr3=20.0
!
      if (prate .lt. pr1) then
        clf = ccf1
      elseif (prate .lt. pr2) then
        clf = ccf2
      elseif (prate .lt. pr3) then
        clf = ccf3
      else
        clf = ccf4
      endif
!
      return
      end
