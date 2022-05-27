      program mainmove
!
! move regional domain from one location to another location with
! overlay. The overlay points are updated with forecast values.
!
! forecast restart files at old location in units 11, 12, and 13
! analyzed restart files at new location in units 21, 22, and 23
! update overlay points for new location with old location values
! updated  restart files at new location in units 51, 52, and 53
!
      use machine, only : kind_io4

#include <paramodel.h>
!
      parameter(levmax=100)
      character*8 label(4)
      integer idate(4)
      real(kind=kind_io4) fhours,sis(levr+1),sls(levr)
      real(kind=kind_io4) dummy(2*levmax+1-levr-1-levr)                 &
     &         ,ext(512-(6+2*levmax))
      real(kind=kind_io4) ogs(lngrd), ogls(lngrd,lsoil)                 &
     &         ,og2s(lngrd,2), og4s(lngrd,4)                            &
     &         ,rgs(lngrd),  rgls(lngrd,lsoil)                          &
     &         ,rg2s(lngrd,2),  rg4s(lngrd,4)                           &
     &         ,f3do(lngrd,levr),  f3dn(lngrd,levr)                     &
     &         ,flato(lngrd),flono(lngrd)                               &
     &         ,flatn(lngrd),flonn(lngrd)                               &
     &         ,fmap(lngrd,4)                                           &
     &         ,tmpo(lngrd),tmpb(lngrd),tmpn(lngrd)
      integer map(lngrd),imap(lngrd)

      data no_sigi,no_sigitdt,no_sfci  /     11,     12,    13/
      data nb_sigi,nb_sigitdt,nb_sfci  /     21,     22,    23/
      data nn_sigi,nn_sigitdt,nn_sfci  /     31,     32,    33/
      data nr_sigi,nr_sigitdt,nr_sfci  /     51,     52,    53/
!
      print *,'***** start of mainmove *****'
      print *,' im jm ijm ',igrd1,jgrd1,lngrd
      im = igrd1
      jm = jgrd1
      ijm = im*jm
      km = levr
!
! found the mapover index and nearest point
! skip record of lab, fhours, gz, q, (u, v, t with levr)
!                (q with levr*ntotal), f2, fmx, fmy
!
      rewind no_sigi
      rewind nn_sigi
      iskip=4+levr*(3+ntotal)+3
#ifdef NONHYD
      iskip=iskip+levr*3+1
#endif
      do i=1,iskip
        read(no_sigi)
        read(nn_sigi)
      enddo
      read(no_sigi) flato
      read(no_sigi) flono
      print *,' old domain lat ',minval(flato),maxval(flato)
      print *,' old domain lon ',minval(flono),maxval(flono)
      read(nn_sigi) flatn
      read(nn_sigi) flonn
      print *,' new domain lat ',minval(flatn),maxval(flatn)
      print *,' new domain lon ',minval(flonn),maxval(flonn)
!
      call rmapindx(igrd1,jgrd1,lngrd,                                  &
     &              flato,flono,flatn,flonn,map,imap,fmap)
!
! --------
! r_sigit
! --------
!
      rewind no_sigi
      rewind nb_sigi
      rewind nn_sigi
      rewind nr_sigi

      read(nn_sigi) label
      read(nb_sigi) label
      read(no_sigi) label
      write(nr_sigi) label
      print *,' new domain label t ',label

      read(nn_sigi) fhours,idate,(sis(k),k=1,km+1),(sls(k),k=1,km)       &
     &         ,dummy,ext
      read(nb_sigi) fhours,idate,(sis(k),k=1,km+1),(sls(k),k=1,km)       &
     &         ,dummy,ext
      read(no_sigi) fhours,idate,(sis(k),k=1,km+1),(sls(k),k=1,km)       &
     &         ,dummy,ext
      write(nr_sigi) fhours,idate,(sis(k),k=1,km+1),(sls(k),k=1,km)      &
     &         ,dummy,ext
      print *,' new domain fhour idate t ',fhours,idate

      do i=1,2+levr*(3+ntotal)
        read(nn_sigi) tmpn
        read(nb_sigi) tmpb
        read(no_sigi) tmpo
        call rmappert(igrd1,jgrd1,lngrd,                                 &
     &                tmpo,tmpb,tmpn,map,imap,fmap)
        write(nr_sigi) tmpn
      enddo
#ifdef NONHYD
      do i=1,levr*3+1
        read(nn_sigi) tmpn
        read(nb_sigi) tmpb
        read(no_sigi) tmpo
        call rmappert(igrd1,jgrd1,lngrd,                                 &
     &                tmpo,tmpb,tmpn,map,imap,fmap)
        write(nr_sigi) tmpn
      enddo
#endif
      do i=1,5
        read(no_sigi) tmpo
        read(nn_sigi) tmpn
        write(nr_sigi) tmpn
      enddo
! ----- for extra
      tmpb=0.0
      do m=1,3
        do k=1,levr
          read(no_sigi,end=111) tmpo
          tmpn=0.0
          call rmappert(igrd1,jgrd1,lngrd,                               &
     &                tmpo,tmpb,tmpn,map,imap,fmap)
          write(nr_sigi) tmpn
        enddo
      enddo
!
! --------------------
!     r_sigitdt
! --------------------
!
 111  continue
      rewind no_sigitdt
      rewind nb_sigitdt
      rewind nn_sigitdt
      rewind nr_sigitdt

      read(nn_sigitdt) label
      read(nb_sigitdt) label
      read(no_sigitdt) label
      write(nr_sigitdt) label
      print *,' new domain label t+dt ',label

      read(nn_sigitdt) fhours,idate,(sis(k),k=1,km+1),(sls(k),k=1,km)       &
     &         ,dummy,ext
      read(nb_sigitdt) fhours,idate,(sis(k),k=1,km+1),(sls(k),k=1,km)       &
     &         ,dummy,ext
      read(no_sigitdt) fhours,idate,(sis(k),k=1,km+1),(sls(k),k=1,km)       &
     &         ,dummy,ext
      write(nr_sigitdt) fhours,idate,(sis(k),k=1,km+1),(sls(k),k=1,km)      &
     &         ,dummy,ext
      print *,' new domain fhour idate t+dt ',fhours,idate

      do i=1,2+levr*(3+ntotal)
        read(nn_sigitdt) tmpn
        read(nb_sigitdt) tmpb
        read(no_sigitdt) tmpo
        call rmappert(igrd1,jgrd1,lngrd,                                 &
     &                tmpo,tmpb,tmpn,map,imap,fmap)
        write(nr_sigitdt) tmpn
      enddo
#ifdef NONHYD
      do i=1,levr*3+1
        read(nn_sigitdt) tmpn
        read(nb_sigitdt) tmpb
        read(no_sigitdt) tmpo
        call rmappert(igrd1,jgrd1,lngrd,                                 &
     &                tmpo,tmpb,tmpn,map,imap,fmap)
        write(nr_sigitdt) tmpn
      enddo
#endif
      do i=1,5
        read(no_sigitdt) tmpo
        read(nn_sigitdt) tmpn
        write(nr_sigitdt) tmpn
      enddo
! ----- for extra
      tmpb=0.0
      do m=1,num_p3d
        read(no_sigitdt,end=222) ((f3do(ij,k),ij=1,lngrd),k=1,levr)
        do k=1,levr
          tmpn=0.0
          tmpo(:)=f3do(:,k)
          call rmappert(igrd1,jgrd1,lngrd,                               &
     &                tmpo,tmpb,tmpn,map,imap,fmap)
          f3dn(:,k) = tmpn(:)
        enddo
        write(nr_sigitdt) ((f3dn(ij,k),ij=1,lngrd),k=1,levr)
      enddo
      do m=1,num_p2d
        write(no_sigitdt) tmpo
        tmpn = 0.0
        call rmappert(igrd1,jgrd1,lngrd,                               &
     &                tmpo,tmpb,tmpn,map,imap,fmap)
        write(nr_sigitdt) tmpn
      enddo
!
!
! --------------------
! last for r_sfci
! --------------------
!
 222  continue
      rewind no_sfci
      rewind nn_sfci
      rewind nr_sfci
!  1 label 
      read(nn_sfci) label
      read(no_sfci) label
      write(nr_sfci) label
      print *,' sfc label ',label
!  2 hour idate
      read(nn_sfci) fhours,idate,im,jm,version
      read(no_sfci) fhours,idate,im,jm,version
      write(nr_sfci) fhours,idate,im,jm,version
      print *,' sfc fhour idate ',fhours,idate
!  3 (tsea)
      read(nn_sfci) rgs
      read(no_sfci) ogs
      call rmapover(ogs,rgs,map,imap,lngrd,1)
      write(nr_sfci) rgs
      print *,' sfc tsea ',minval(rgs),maxval(rgs)
! 4 (smc) soil moisture content
      read(nn_sfci) rgls
      read(no_sfci) ogls
      call rmapover(ogls,rgls,map,imap,lngrd,lsoil)
      write(nr_sfci) rgls
      print *,' sfc smc ',minval(rgls),maxval(rgls)
! 5 (sheleg) snow depth
      read(nn_sfci) rgs
      read(no_sfci) ogs
      call rmapover(ogs,rgs,map,imap,lngrd,1)
      write(nr_sfci) rgs
      print *,' sfc sheleg ',minval(rgs),maxval(rgs)
! 6 (stc) soil temperature content stc 6 
      read(nn_sfci) rgls
      read(no_sfci) ogls
      call rmapover(ogls,rgls,map,imap,lngrd,lsoil)
      write(nr_sfci) rgls
      print *,' sfc stc ',minval(rgls),maxval(rgls)
! 7 (tg3) deep soil temperature tg3 7
      read(nn_sfci) rgs
      read(no_sfci) ogs
      call rmapover(ogs,rgs,map,imap,lngrd,1)
      write(nr_sfci) rgs
      print *,' sfc tg3 ',minval(rgs),maxval(rgs)
!  8 (z0) roughness z0 8
      read(nn_sfci) rgs
      read(no_sfci) ogs
      call rmapover(ogs,rgs,map,imap,lngrd,1)
      write(nr_sfci) rgs
      print *,' sfc z0 ',minval(rgs),maxval(rgs)
!  9 (cv) cloud amount cv 9
      read(nn_sfci) rgs
      read(no_sfci) ogs
      call rmapover(ogs,rgs,map,imap,lngrd,1)
      write(nr_sfci) rgs
      print *,' sfc cv ',minval(rgs),maxval(rgs)
! 10 (cb) cloud base cb 10
      read(nn_sfci) rgs
      read(no_sfci) ogs
      call rmapover(ogs,rgs,map,imap,lngrd,1)
      write(nr_sfci) rgs
      print *,' sfc cvb',minval(rgs),maxval(rgs)
! 11 (ct) cloud top ct 11
      read(nn_sfci) rgs
      read(no_sfci) ogs
      call rmapover(ogs,rgs,map,imap,lngrd,1)
      write(nr_sfci) rgs
      print *,' sfc cvt',minval(rgs),maxval(rgs)
! 12 (albedo) 
      read(nn_sfci) rg4s
      read(no_sfci) og4s
      call rmapover(og4s,rg4s,map,imap,lngrd,4)
      write(nr_sfci) rg4s
      print *,' sfc albedo',minval(rg4s),maxval(rg4s)
! 13 (lsmk) sea land mask 13
      read(nn_sfci) rgs
      read(no_sfci) ogs
      call rmapover(ogs,rgs,map,imap,lngrd,1)
      write(nr_sfci) rgs
      print *,' sfc lsmk',minval(rgs),maxval(rgs)
! 14 (vfrac) vegitation fraction 14
      read(nn_sfci) rgs
      read(no_sfci) ogs
      call rmapover(ogs,rgs,map,imap,lngrd,1)
      write(nr_sfci) rgs
      print *,' sfc vfrac',minval(rgs),maxval(rgs)
! 15 (canopy) vegitation canopy
      read(nn_sfci) rgs
      read(no_sfci) ogs
      call rmapover(ogs,rgs,map,imap,lngrd,1)
      write(nr_sfci) rgs
      print *,' sfc canopy',minval(rgs),maxval(rgs)
! 16 (f10m)
      read(nn_sfci) rgs
      read(no_sfci) ogs
      call rmapover(ogs,rgs,map,imap,lngrd,1)
      write(nr_sfci) rgs
      print *,' sfc f10m',minval(rgs),maxval(rgs)
! 17 (vtype) vegitation type
      read(nn_sfci) rgs
      read(no_sfci) ogs
      call rmapover(ogs,rgs,map,imap,lngrd,1)
      write(nr_sfci) rgs
      print *,' sfc vtype',minval(rgs),maxval(rgs)
! 18 (stype) soil type
      read(nn_sfci) rgs
      read(no_sfci) ogs
      call rmapover(ogs,rgs,map,imap,lngrd,1)
      write(nr_sfci) rgs
      print *,' sfc stype',minval(rgs),maxval(rgs)
! 19 (fac) zenith angle dependent factor 
      read(nn_sfci) rg2s
      read(no_sfci) og2s
      call rmapover(og2s,rg2s,map,imap,lngrd,2)
      write(nr_sfci) rg2s
      print *,' sfc fac',minval(rg2s),maxval(rg2s)
! 20 (uustar)
      read(nn_sfci) rgs
      read(no_sfci) ogs
      call rmapover(ogs,rgs,map,imap,lngrd,1)
      write(nr_sfci) rgs
      print *,' sfc uustar',minval(rgs),maxval(rgs)
! 21 (ffmm) momentum exchange similarity function
      read(nn_sfci) rgs
      read(no_sfci) ogs
      call rmapover(ogs,rgs,map,imap,lngrd,1)
      write(nr_sfci) rgs
      print *,' sfc ffmm',minval(rgs),maxval(rgs)
! 22 (ffhh) heat exchange similarity function
      read(nn_sfci) rgs
      read(no_sfci) ogs
      call rmapover(ogs,rgs,map,imap,lngrd,1)
      write(nr_sfci) rgs
      print *,' sfc ffhh',minval(rgs),maxval(rgs)

      print *,' ****** normal end of mainmove ***** '
      stop
      end
