       program mainrsmf_mrg
!....
! add option to run from global as well as regional files
!....
      use machine , only : kind_io4
      use cnttime
!ccj
#include <paramodel.h>
!
#include <rscomfc.h>
#include <rscomio.h>
!
#ifdef MP
#include <npesi.h>
#include <comrmpi.h>
#ifdef G2R
#include <commpi.h>
#include <commpildef.h>
#endif
#endif
#include <rscomgpd.h>
!
      real(kind=kind_io4) fhours
!
      character cfhour*16,cform*40,filename*100
!..........................................
#ifdef MP
      call rmpinit(ncoli,nrowi,strwtime)
#else
#ifdef IBMSP
      strwtime=timef()
#endif
#endif
!cc
!..........................................
!  
#ifdef MP
#ifdef G2R
       call mpdimset
#endif
       call rmpdimset
#endif
!.........................................
!c
#ifdef NONHYD
      if(mype.eq.master) print 100
100     format (1h0,'ncep nonhydrostatic mesoscale spectral model',      &
     &        '-current version created may 2004')
#else
      if(mype.eq.master) print 100
100   format (1h0,'ncep regional spectral model',                        &  
     &        '-current version created february 2003')
#endif
!jun
#ifdef CNT
       call cnttime_init()
#endif
!jun
      call rgetcon_mrg
#ifdef MP
!tst      if (mype.eq.master) then
#endif
!      print*,'mype=',mype,'at mainrsmf after rgetcon_mrg,rsfcsec=',      &
!     &     rsfcsec
#ifdef MP
!tst      endif
#endif

!
! rsm: run regional model only.
#ifdef MP
         if (mype.eq.master) then
#endif
#ifdef G2R
         print *,'rsm: run regional only with global files'
#endif
#ifdef C2R
         print *,'rsm: run regional with regional files'
#endif
#ifdef MP
         endif
#endif
#ifdef MP
      if (mype.eq.master) then
#endif
      rewind nrsmi1
      read(nrsmi1)
      read(nrsmi1) fhours
      fhour=fhours
      rewind nrsmi1
      kh=nint(fhour)
!     print*,'at mainrsmf',' fhour= ',fhour,' kh= ',kh
      ndig=max(log10(kh+0.5)+1.,2.)
      write(cform,'("(i",i1,".",i1,")")') ndig,ndig
      write(cfhour,cform) kh
!hmhj open(unit=n1,file='rb_sigf'//cfhour                              & 
!hmhj&    ,status='old',form='unformatted')
      write(filename,"(a,a)") 'rb_sigf',cfhour
      open(unit=n1,file=filename                                       & 
     &    ,status='old',form='unformatted')
#ifdef MP
      endif
#endif
#ifdef MP
      if (mype.eq.master) then
#endif
      print*,'at mainrsmf before rsmini,rsfcsec=',rsfcsec
#ifdef MP
      endif
#endif
      call rsmini_mpi(n1)
#ifdef MP
      if (mype.eq.master) then
#endif
      print*,'at mainrsmf after rsmini'
      close(n1)
#ifdef MP
      endif
#endif
! time loop
!jw for test
      maxstp=nint(fhseg/fhbas)
      do n=1,maxstp
#ifdef G2R
#ifdef MP
        ijmaxf=lnt22
        ijmaxp=lnt22p
#else
        ijmaxf=lnt22
        ijmaxp=lnt22
#endif
        kmax=levs
#endif
#ifdef C2R
        ijmaxf=clngrd
        ijmaxp=clngrd
        kmax=levr
#endif

        fhnxt=phour+fhbas
#ifdef MP
        if (mype.eq.master) then
#endif
        kh=nint(fhnxt)
        ndig=max(log10(kh+0.5)+1.,2.)
        write(cform,'("(i",i1,".",i1,")")') ndig,ndig
        write(cfhour,cform) kh
!hmhj   open(unit=n2,file='rb_sigf'//cfhour                              &
!hmhj&      ,status='old',form='unformatted')
        write(filename,"(a,a)") 'rb_sigf',cfhour
        open(unit=n2,file=filename                                       & 
     &      ,status='old',form='unformatted')
#ifdef MP
        endif
#endif
!
        call cread(n2,ijmaxf,ijmaxp,kmax,                                &
#ifdef MP
     &             mype,master,                                          &
#ifdef G2R
     &             jcap,lwvdef,                                          &
#endif
#endif
     &             label,gfhour,idate,sl,si,                             &
     &             gz,q,te,du,zv,rq                                      &
#ifdef C2R
     &            ,fm2,fm2x,fm2y,flat,flon                               &  
#endif
     &            ,ntotal,ntoz,ncld,ncldb)
#ifdef MP
      if (mype.eq.master) then
#endif
        close(n2)
#ifdef MP
        endif
#endif
!c      n2=n2+1
!c      fhnxt=phour+fhbas
        call rsmsmf(fhnxt,gz,q,te,du,zv,rq)
      enddo

#ifdef RKN
#ifdef MP
      call rmpkencollect(itnum,nvrken,nptken,nstken,svdata)
#endif
#endif
      call rsmsav

#ifdef RKN
#ifdef MP
      if (mype.eq.master) then
#endif
!j      call rsmkenprof    !add by binbin to convert to eta bufr
#ifdef MP
      endif
#endif
#endif
!
!jun
#ifdef CNT
       call cnttime_final()
#endif
!jun
#ifdef MP
      call rmpfine(endwtime)
      print *,' MPI wall time is ',endwtime-strwtime
#else
#ifdef IBMSP
      endwtime=timef()
      print *,' SGL wall time is ',(endwtime-strwtime)/1000.
#endif
#endif
!c
      stop
      end
