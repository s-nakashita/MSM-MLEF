#ifdef MPIO
      real*4             sig_recn,sfc_recn,flx_recn
      integer (kind=mpi_offset_kind) sig_disp,sfc_disp,flx_disp
<<<<<<< .mine
      integer            kgz,kq,kte,kuu,kvv,krq,ktt,kpp,kww              &    
=======
      integer            kgz,kq,kte,kuu,kvv,krq,ktt,kpp,kww              &  
>>>>>>> .r631
     &                  ,kfm2,kfm2x,kfm2y,kflon,kflat
      common /comrmpio1/ sig_recn,sfc_recn,flx_recn
      common /comrmpio2/ sig_disp,sfc_disp,flx_disp 
      common /comrmpio3/ kgz,kq,kte,kuu,kvv,krq,ktt,kpp,kww              &  
     &                  ,kfm2,kfm2x,kfm2y,kflon,kflat,ktotal
#endif
