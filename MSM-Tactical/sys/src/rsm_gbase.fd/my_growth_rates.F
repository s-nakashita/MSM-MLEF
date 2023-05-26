!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!--- sets up lookup table for calculating initial ice crystal growth ---
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
      subroutine my_growth_rates (dtph)
!
!--- below are tabulated values for the predicted mass of ice crystals
!    after 600 s of growth in water saturated conditions, based on 
!    calculations from miller and young (jas, 1979).  these values are
!    crudely estimated from tabulated curves at 600 s from fig. 6.9 of
!    young (1993).  values at temperatures colder than -27c were 
!    assumed to be invariant with temperature.  
!
!--- used to normalize miller & young (1979) calculations of ice growth
!    over large time steps using their tabulated values at 600 s.
!    assumes 3d growth with time**1.5 following eq. (6.3) in young (1993).
!
      integer, parameter :: my_t1=1, my_t2=35
      common /cmy600/ my_growth(my_t1:my_t2)
      real my_growth, my_600(my_t1:my_t2)
      save my_600
!
      data my_600 /                                                      &   
     & 5.5e-8, 1.4e-7, 2.8e-7, 6.e-7, 3.3e-6,                            & !  -1 to  -5 deg c     
     & 2.e-6, 9.e-7, 8.8e-7, 8.2e-7, 9.4e-7,                             & !  -6 to -10 deg c
     & 1.2e-6, 1.85e-6, 5.5e-6, 1.5e-5, 1.7e-5,                          & ! -11 to -15 deg c
     & 1.5e-5, 1.e-5, 3.4e-6, 1.85e-6, 1.35e-6,                          & ! -16 to -20 deg c
     & 1.05e-6, 1.e-6, 9.5e-7, 9.0e-7, 9.5e-7,                           & ! -21 to -25 deg c
     & 9.5e-7, 9.e-7, 9.e-7, 9.e-7, 9.e-7,                               & ! -26 to -30 deg c
     & 9.e-7, 9.e-7, 9.e-7, 9.e-7, 9.e-7 /                                 ! -31 to -35 deg c
!
!-----------------------------------------------------------------------
!
      dt_ice=(dtph/600.)**1.5
      my_growth=dt_ice*my_600
!
!-----------------------------------------------------------------------
!
      return
      end
