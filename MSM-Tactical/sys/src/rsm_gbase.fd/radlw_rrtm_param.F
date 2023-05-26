!!!!!  ==========================================================  !!!!!
!!!!!              rrtm radiation package description              !!!!!
!!!!!  ==========================================================  !!!!!
!                                                                      !
!    the rrtm package includes three parts:                            !
!                                                                      !
!       'radlw_rrtm_param.f'                                           !
!       'radlw_rrtm_datatb.f'                                          !
!       'radlw_rrtm_main.f'                                            !
!                                                                      !
!    the 'radlw_rrtm_param.f' contains:                                !
!                                                                      !
!       'module_radlw_parameters'  -- band parameters set up           !
!       'module_radlw_cntr_para'   -- control parameters set up        !
!                                                                      !
!    the 'radlw_rrtm_datatb.f' contains:                               !
!                                                                      !
!       'module_radlw_aerosols'    -- aerosols data tables (not yet!)  !
!       'module_radlw_avplank'     -- plank flux data                  !
!       'module_radlw_cldprlw'     -- cloud property coefficients      !
!       'module_radlw_kgbnn'       -- absorption coeffients for 16     !
!                                     bands, where nn = 01-16          !
!                                                                      !
!    the 'radlw_rrtm_main.f' contains the main module:                 !
!                                                                      !
!       'module_radlw_main'                                            !
!                                                                      !
!    in the main module 'module_radlw_main' there are only two         !
!    externally callable subroutines:                                  !
!                                                                      !
!                                                                      !
!       'lwrad'     -- main rrtm lw radiation routine                  !
!       'rlwinit'   -- to initialize rrtm lw radiation                 !
!                                                                      !
!    all the lw radiation subprograms become contained subprograms     !
!    in module 'module_radlw_rrtm' and many of them are not directly   !
!    accessable from places outside the module.                        !
!                                                                      !
!    exterior modules referenced:                                      !
!                                                                      !
!       'module machine'                    in 'machine.f'             !
!       'module physcons'                   in 'physcons.f'            !
!aer    'module module_aerosols'            in 'rad_aerosols.f'        !
!                                                                      !
!    compilation sequence is:                                          !
!                                                                      !
!       'radlw_rrtm_param,f'                                           !
!       'radlw_rrtm_datatb,f'                                          !
!       'radlw_rrtm_main.f'                                            !
!                                                                      !
!    and all should be put in front of routines that use lw modules    !
!                                                                      !
!!!!!  ==========================================================  !!!!!
!!!!!                       end descriptions                       !!!!!
!!!!!  ==========================================================  !!!!!



!========================================!
      module module_radlw_parameters     !
!........................................!
!
!  ---  parameter constants for lw band structures
!
      implicit none
!
      integer  :: nbands, ngpt, n5000, n200, maxgas, maxxsec, nplnk
      parameter (nbands=16, ngpt=140, n5000=5000, n200=200, nplnk=181)
      parameter (maxgas=6, maxxsec=4)

!  ---  number of g-point in each band
      integer  :: ng01, ng02, ng03, ng04, ng05, ng06, ng07, ng08,       &
     &            ng09, ng10, ng11, ng12, ng13, ng14, ng15, ng16
      parameter (ng01=08, ng02=14, ng03=16, ng04=14, ng05=16, ng06=08,  &
     &           ng07=12, ng08=08, ng09=12, ng10=06, ng11=08, ng12=08,  &
     &           ng13=04, ng14=02, ng15=02, ng16=02)

!  ---  begining index of each band
      integer  :: ns01, ns02, ns03, ns04, ns05, ns06, ns07, ns08,       &
     &            ns09, ns10, ns11, ns12, ns13, ns14, ns15, ns16
      parameter (ns01=00, ns02=08, ns03=22, ns04=38, ns05=52, ns06=68,  &
     &           ns07=76, ns08=88, ns09=96, ns10=108, ns11=114,         &
     &           ns12=122, ns13=130, ns14=134, ns15=136, ns16=138)

!........................................!
      end module module_radlw_parameters !
!========================================!



!========================================!
      module module_radlw_cntr_para      !
!........................................!
!
        implicit   none
!
        integer :: ilwrate, iaerlw, icfclw, iflagliq, iflagice

!
!  ---  set up control parameters for lw radiation
!
        parameter ( ilwrate=1 )     !===> ... lw heating rate unit selection
                        !(default)  ! =1: output in k/day
                                    ! =2: output in k/second

        parameter ( iaerlw=0 )      !===> ... control flag for aerosols (not yet)
                        !(default)  ! =0: do not include aerosol effect
                                    ! =1: include aerosol effect

        parameter ( icfclw=0  )     !===> ... control flag for cfc gases
                                    ! =0: do not include cfc gases
                        !(default)  ! =1: include all cfc gases

        parameter ( iflagliq=3 )    !===> ... liq-cloud optical properties contrl flag
                                    ! =0: input cloud opt depth, ignor iflagice setting
                                    ! =1: input cwp,cip, (ccm2 method) ignor iflagice setting
                                    ! =2: input cwp rew, ccm3 method for liquid clouds
                        !(default)  ! =3: input cwp rew, hu and stamnes(1993) method for liq cld

        parameter ( iflagice=1 )    !===> ... ice-cloud optical properties contrl flag
                                    !         only used when iflagliq .ge. 2, else is ignored
                                    ! =0: input cip rei, ccm3 method for ice clouds
                        !(default)  ! =1: input cip rei, ebert and curry(1997) for ice clouds
                                    ! =2: input cip rei, streamer (1996) for ice clouds

!
!........................................!
      end module module_radlw_cntr_para  !
!========================================!
