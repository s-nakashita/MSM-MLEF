#define DCRFT
#undef FFT_SGIMATH
#undef FFTW
#undef RFFTMLT
      subroutine ftilonf (a, b, lot, lcapf, lonff, lat, isign,          &
     &                    latg, lonf, lonf22                            &
#ifdef MP
     &                   ,lonfp                                         &
#endif
     &                    )
#ifdef CRAY_THREAD
!fpp$ noconcur r                                                                
#endif
#define LATS 1
#define LATX 1
#ifdef MP
#define NVECT lonfp*2
#else
#define NVECT 64
#endif
      parameter (lotmin=NVECT,lotmax=NVECT,ncpu=1)                            
!                                                                               
#define DEFAULT
#ifdef DCRFT
#undef DEFAULT
      parameter (jumph=(lonf+2)/2,jumpdim=jumph*2)
      dimension table(42000)
      common /comlonf/crscale(LATS),rcscale(LATS)
#endif
#ifdef FFT_SGIMATH
#undef DEFAULT
      parameter (jumpdim=lonf + 3)                                             
      common /comlonf/scale(LATS),trig(lonf+15,LATS)                        
#endif
#ifdef FFTW
#undef DEFAULT
      parameter (jumpdim=lonf + 3)                                             
      common /comlonf/ scale(LATS)
      common /comlonfi/iplan_c_to_r(LATS),iplan_r_to_c(LATS)
      dimension ak(jumpdim,lotmax)
#endif
#ifdef DEFAULT
      parameter (jumpdim=lonf + 3)                                             
      dimension work(lonf,lotmax,2)
      common /comlonf/ifax(20,LATS),trigs(lonf,2,LATS)                        
#endif
      dimension al(jumpdim,lotmax)
      dimension a(lonf22,lot)                                                  
      dimension b(lonf22,lot)                                                  
!                                                                               
      jump=jumpdim                                                              
      jcapf=lcapf-1
      twoj1=2*(jcapf+1)
!
      if (isign .eq.  1)  then                                                  
!                                                                               
!  multiple fast fourier transform - synthesis.  isign=1                        
!  good for zonal wave number jcap.                                            
!                                                                               
!     dimension a(lonf,lot)                                                    
!                                                                               
!   input - lot sets of complex coefficients in                                 
!           a(1,j), a(2,j), ..., a(jcap1*2,j), j=1,...,lot.                    
!           a(jcap1*2+1,j), ..., a(lonf,j), j=1,...,lot are not set           
!           before call fftlonf.                                                
!                                                                               
!  output - lot sets of grid values in                                          
!           a(1,j), a(2,j), ..., a(lonf,j), j=1,...,lot.                       
!                                                                               
!
      nlot=max0(lot/ncpu,lotmin)                                                
      nlot=min0(nlot    ,lotmax)                                                
!
      do 460 i=1,lot,nlot                                                       
        lots = min0(nlot, lot-i+1)                                                
        do 360 j=i,i+lots-1                                                       
          jj=j-i+1
          k=(j+1)/2                                                                 
          loff=mod(j+1,2)*lonf                                                     
#ifdef FFTW
          al(1,jj)=a(1+loff,k)
          do l=2,jcapf+1
            al(l        ,jj)=a(2*l-1+loff,k)
            al(lonff+2-l,jj)=a(2*l  +loff,k)
          enddo
          do l=jcapf+2,lonff-jcapf
            al(l,jj)=0.0
          enddo
          do l=lonff+1,jump
            al(l,jj)=0.0
          enddo
#else
          do 320 l=1,twoj1                                                         
            al(l,jj) = a(l+loff,k)                                          
320       continue                                                                  
          do 340 l=twoj1 + 1 , jump
            al(l,jj) = 0.0                                                  
340       continue                                                                  
#endif
360     continue                                                                  
!                                                                               
!     call  fft for systhesis.                                              
!                                                                               
#define DEFAULT
#ifdef DCRFT
#undef DEFAULT
      call dcrft(1,al,jumph,al,jump,lonff,lots,                         &
     &           -1,crscale(LATX),table,22000,table(22001),20000)
      call dcrft(0,al,jumph,al,jump,lonff,lots,                         &
     &           -1,crscale(LATX),table,22000,table(22001),20000)
#endif
#ifdef FFT_SGIMATH
#undef DEFAULT
       call zdfftm1du(-1,lonff,lots,al,1,jump,trig(1,LATX))                    
#endif
#ifdef FFTW
#undef DEFAULT
        call rfftw_f77(iplan_c_to_r(LATX),lots,al,1,jump,ak,1,jump)
#endif
#ifdef RFFTMLT
#undef DEFAULT
        call rfftmlt(al,work,trigs(1,1,LATX),ifax(1,LATX),              &
     &                                  1,jump,lonff,lots,1)                    
#endif
#ifdef DEFAULT
        call fft99m (al,work,trigs(1,1,LATX),ifax(1,LATX),              &
     &                                  1,jump,lonff,lots,1)                      
#endif
!                                                                               
#ifdef FFTW
#define ALK ak
#else
#define ALK al
#endif
        do 440 j=i,i+lots-1                                                       
          k=(j+1)/2                                                                 
          loff=mod(j+1,2)*lonff                                                     
          do 420 l=1,lonff                                                          
            b(l+loff,k) = ALK(l,j-i+1)                                          
420       continue                                                                  
440     continue                                                                  
460   continue                                                                  
!
      endif                                                                     
!                                                                               
!                                                                               
      if (isign .eq. -1)  then                                                  
!                                                                               
!  multiple fast fourier transform - analysis.  isign=-1                        
!  good for zonal wave number jcap.                                            
!                                                                               
!     dimension a(lonf,lot), b(lonf,lot)                                      
!                                                                               
!   input - lot sets of grid values in                                          
!           a(1,j), a(2,j), ..., a(lonf,j), j=1,...,lot.                       
!           a array is not changed by sr fftlonf.                               
!                                                                               
!  output - lot sets of complex coefficients in                                 
!           b(1,j), b(2,j), ..., b(jcap1*2,j), j=1,...,lot.                    
!           b(jcap1*2+1,j), ..., b(lonf,j), j=1,...,lot are not set.          
!                                                                               
      nlot=max0(lot/ncpu,lotmin)                                                
      nlot=min0(nlot    ,lotmax)                                                
      do 660 i=1,lot,nlot                                                       
        lots = min0(nlot, lot-i+1)                                                
        do 560 j=i,i+lots-1                                                       
          k=(j+1)/2                                                                 
          loff=mod(j+1,2)*lonff                                                     
          do 520 l=1,lonff                                                          
             al(l,j-i+1) = a(l+loff,k)                                          
520       continue                                                                  
560     continue                                                                  
!                                                                               
!     call fft for analysis.                                               
!
#define DEFAULT
#ifdef DCRFT
#undef DEFAULT
        call drcft(1,al,jump,al,jumph,lonff,lots,                         &
     &             1,rcscale(LATX),table,22000,table(22001),20000)
        call drcft(0,al,jump,al,jumph,lonff,lots,                         &
     &             1,rcscale(LATX),table,22000,table(22001),20000)
#endif
#ifdef FFT_SGIMATH
#undef DEFAULT
        call dzfftm1du(1,lonff,lots,al,1,jump,trig(1,LATX))                    
        call dscalm1d(lonff,lots,scale(LATX),al,1,jump)
#endif
#ifdef FFTW
#undef DEFAULT
        call rfftw_f77(iplan_r_to_c(LATX),lots,al,1,jump,ak,1,jump)
#endif
#ifdef RFFTMLT
#undef DEFAULT
        call rfftmlt(al,work,trigs(1,1,LATX),ifax(1,LATX),              &
     &                                  1,jump,lonff,lots,-1)                   
#endif
#ifdef DEFAULT
        call fft99m (al,work,trigs(1,1,LATX),ifax(1,LATX),              &
     &                                  1,jump,lonff,lots,-1)                     
#endif
!                                                                               
        do 640 j=i,i+lots-1                                                       
          k=(j+1)/2                                                                 
          loff=mod(j+1,2)*lonf                                                     
#ifdef FFTW
          jj=j-i+1
          b(1+loff,k)      =ak(1,jj)*scale
          b(2+loff,k)      =0.0
          do l=2,jcapf+1
            b(2*l-1+loff,k)=ak(l        ,jj)*scale
            b(2*l  +loff,k)=ak(lonff+2-l,jj)*scale
          enddo
#else
          do 620 l=1,twoj1                                                         
             b(l+loff,k) = al(l,j-i+1)                                          
620       continue                                                                  
#endif
640     continue                                                                  
660   continue                                                                  
!
      endif                                                                     
!                                                                               
      return                                                                    
      end                                                                       
