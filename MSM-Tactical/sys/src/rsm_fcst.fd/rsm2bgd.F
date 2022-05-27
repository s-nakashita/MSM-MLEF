      subroutine rsm2bgd(gz,q,te,uu,vv,rq)
!$$$  subprogram documentation block                                            
!                .      .    .                                       .          
! subprogram:  gsm2rsm                                                          
!   prgmmr:  hann-ming henry juang      org: w/nmc20    date: 92-02-06          
!                                                                               
! abstract:  do global to regional grids transformation.                        
!                                                                               
! program history log:                                                          
!                                                                               
! usage:    call gsm2rsm(gz,q,te,di,ze,rq)                                      
!   input argument list:                                                        
!     gz        - global coefficient of terrain height.                                
!     q         - global coefficient of log surface pressure.                          
!     te        - global coefficient of virtual temperature.                           
!     di        - global coefficient of divergence.                                    
!     ze        - global coefficient of vorticity.                                     
!     rq        - global coefficient of specific humidity.                             
!                                                                               
!   output argument list:                                                       
!                                                                               
!   common block:                                                               
!       rscomltb                                                                      
!       regin1                                                                        
!       repro                                                                         
!                                                                               
!   input files: none                                                           
!                                                                               
!   output files: none                                                          
!                                                                               
!   subprograms called:                                                         
!       dztouv  maxmin  pln2i  sums2i  sumtop  ftilonf_  fplfml                       
!       sphpt1                                                                        
!                                                                               
!   remark: none                                                                
!                                                                               
! attributes:                                                                   
!   language: fortran 77.                                                       
!   machine:  cray ymp.                                                         
!                                                                               
!$$$                                                                            
!.....
!ccj
#include <paramodel.h>
#include <rscomltb.h>
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
#endif
!cc
!...sof include..........................................
!...
      dimension gz(clngrd), q(clngrd)                                    & 
     &  , te(clngrd,levr), rq(clngrd,levr,ntotal)                        &
     &  , uu(clngrd,levr), vv(clngrd,levr)
!
!.................................................................
      save numprt
      data numprt/0/
!.................................................................
      call cgtobg(gz,gzb,1)
      call cgtobg( q,qltb,1)
      call cgtobg(te,teltb,levr)
      do nc=1,ntotal
       call cgtobg(rq(1,1,nc),rqltb(1,1,nc),levr)
      enddo
      call cgtobg(uu,uultb,levr)
      call cgtobg(vv,vvltb,levr)
!      call maxmin(uultb,lngrdb,levr,1,levr,'uu base')
!      call maxmin(vvltb,lngrdb,levr,1,levr,'vv base')
!
! rotate u v to new projection
!
#ifdef ORIGIN_THREAD
!$doacross share(uultb,vvltb,bcsln,bsnln,lngrdb),local(n,hold,k)
#endif
#ifdef CRAY_THREAD
!mic$ do all                                                                    
!mic$1 shared(uultb,vvltb,bcsln,bsnln,lngrdb)                                   
!mic$1 private(n,hold,k)                                                        
#endif
#ifdef OPENMP
!$omp parallel do private(n,hold,k)
#endif
      do k=1,levr
        do n=1,lngrdb
          hold = uultb(n,k)
          uultb(n,k) = hold * bcsln(n) - vvltb(n,k) * bsnln(n)
          vvltb(n,k) = hold * bsnln(n) + vvltb(n,k) * bcsln(n)
        enddo
      enddo
!
      if( numprt .lt. 0 ) then
#ifdef MP
      if (mype .eq. master) then
#endif
!
!     call maxmin(gzb,lngrdb,1,1,1,'gz base')
!     call maxmin( qltb,lngrdb,1,1,1,' q base')
!     call maxmin(teltb,lngrdb,levr,1,levr,'te base')
!     call maxmin(rqltb(1,1,1),lngrdb,levr,1,levr,'rq base')
!     if(ntoz.eq.1)                                                      &  
!    & call maxmin(rqltb(1,1,2),lngrdb,levr,1,levr,'oz base')
!     if(ncld.eq.1)                                                      &  
!    & call maxmin(rqltb(1,1,3),lngrdb,levr,1,levr,'cw base')
!     call maxmin(uultb,lngrdb,levr,1,levr,'uu base')
!     call maxmin(uu,clngrd,levr,1,levr,'cuu base')
!     call maxmin(vvltb,lngrdb,levr,1,levr,'vv base')
!     call maxmin(vv,clngrd,levr,1,levr,'cvv base')
!     call maxmin(bcsln,lngrdb,1,1,1,'bcsln base')
!     call maxmin(bsnln,lngrdb,1,1,1,'bsnln base')
!
#ifdef MP
      endif
#endif
      numprt = numprt + 1
      endif
! .......................................................
      return
      end
