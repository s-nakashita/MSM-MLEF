#ifdef MP
#define IGRD1S igrd1p
#define JGRD1S jgrd1p
#define LNWAVS lnwavp
#else
#define IGRD1S igrd1
#define JGRD1S jgrd1
#define LNWAVS lnwav
#endif
!.................................................................
!......begin regional spectral common......................
! ... fillowing common has to be in order with the call of sums in
!     rloopa, rloopb, rloopr
       common /rcomspc/ 						&
     &    z(LNWAVS)							&
     & ,  y(LNWAVS,levr)						&
     & , rt(LNWAVS,levr,ntotal)						&
     & , di(LNWAVS,levr)						&
     & ,  q(LNWAVS)							&
     & , te(LNWAVS,levr)						&
     & , rq(LNWAVS,levr,ntotal)						&
     & ,tem(LNWAVS,levr)						&
     & , qm(LNWAVS)							&
     & , rm(LNWAVS,levr,ntotal)						&
     & , gz(LNWAVS)							&
     & ,  x(LNWAVS,levr)						&
     & ,dpdlam(LNWAVS)							&
     & , uu(LNWAVS,levr)						&
     & ,uum(LNWAVS,levr)						&
     & ,  w(LNWAVS,levr)						&
     & ,dpdphi(LNWAVS)							&
     & , vv(LNWAVS,levr)						&
     & ,vvm(LNWAVS,levr)
!
#ifdef MP
       common /rcomfp/							&
     &       za(llwavp)							&
     & ,     ya(llwavp,levrp)						&
     & ,    rta(llwavp,levrp,ntotal)					&
     & ,    dia(llwavp,levrp)						&
     & ,     qa(llwavp)							&
     & ,    tea(llwavp,levrp)						&
     & , rqa(llwavp,levrp,ntotal)					&
     & ,tema(llwavp,levrp)						&
     & , qma(llwavp)							&
     & , rma(llwavp,levrp,ntotal)					&
     & , gza(llwavp)							&
     & ,  xa(llwavp,levrp)						&
     & ,dpdlama(llwavp)							&
     & ,    uua(llwavp,levrp)						&
     & ,uuma(llwavp,levrp)						&
     & ,  wa(llwavp,levrp)						&
     & ,dpdphia(llwavp)							&
     & , vva(llwavp,levrp)						&
     & ,vvma(llwavp,levrp)
#endif
!
! ----------------------------- end rcomspc
!ccj
!c regional grid common of fields updated by evaporation/condensation
!
       common /rcomgsc/phy_f3d(IGRD1S,JGRD1S,levr,num_p3d), 		&
     &                 phy_f2d(IGRD1S,JGRD1S,num_p2d)       
!cc
