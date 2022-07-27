program read_flx
!
! main program : read_flx   read flux file and convert into 
!                           readable file with GrADS
! history:
! 22-07-27 create
! 
! namelist:
!
! input :
! unit 11 sigma file (sequential, with header)
! unit 12 flux file (sequential, with header)
! 
! output:
! unit 51 flux file (direct, no header)
!
  implicit none
  integer, parameter :: isig=11, iunit=12, ounit=51, cunit=61
  integer, parameter :: iprs=1, itemp=11, iznlw=33, imerw=34, isphum=51, ipwat=54, &
  &                     ipcpr=59, isnowd=65, icldf=71, iccldf=72, islmsk=81, izorl=83, &
  &                     ialbdo=84, isoilm=144, icemsk=91, ilhflx=121, ishflx=122, izws=124, &
  &                     imws=125, ighflx=155, iuswfc=160, idswfc=161, iulwfc=162, idlwfc=163, &
  &                     inswfc=164, inlwfc=165, idswvb=166, idswvd=167, idswnb=168, idswnd=169, &
  &                     itmx=15, itmn=16, irnof=90, iep=145, iqmx=118, iqmn=119, icldwk=14, &
  &                     izgw=147, imgw=148, ihpbl=221, idswf=204, idlwf=205, iuswf=211, iulwf=212, &
  &                     icpcpr=214
  integer, parameter :: isfc=1, itoa=8, ielev=105, isglev=107, idbls=111, i2dbls=112, icolmn=200, &
  &                     ilcbl=212, ilctl=213, ilclyr=214, imcbl=222, imctl=223, imclyr=224, &
  &                     ihcbl=232, ihctl=233, ihclyr=234
  integer, parameter :: nfld=16
  integer, parameter :: lsoil=2, lflux=27
  integer, dimension(nfld) :: ipur, itlr
  data ipur/iulwf, iuswf, iuswf, idswf, icldf, iprs, iprs, itemp, icldf, iprs, iprs, itemp,&
          & icldf, iprs, iprs, itemp/
  data itlr/itoa, itoa, isfc, isfc, ihclyr, ihctl, ihcbl, ihctl, imclyr, imctl, imcbl, imctl,&
          & ilclyr, ilctl, ilcbl, ilctl/ 
  character(len=8) :: label(4)
  integer :: idate(4), iymdh
  integer :: iyr, imo, ida, ihr
  integer :: ids(255), iens(5), idstmp
  integer :: iret
  logical, allocatable :: lbm(:)
  real, allocatable :: fluxf(:,:)
  ! for sigma file header
  integer, parameter :: levmax=100, nwext=512-(6+2*levmax)
  real(kind=4) :: fhour, si(levmax+1), sl(levmax), sisl(2*levmax+1), &
 &                ext(nwext) 
  integer :: igrd1, jgrd1
  ! components of flux file
  integer :: maxbit, iptv, icen, igen, ibm, il1k, il2k, ip1, ip2, ina, inm, &
  &          icen2, inst, iavg, iacc, ifhour, ifday, ifhr, ithr, ilpds, idrt,&
  &          igrd2, jgrd2
  real(kind=4) :: colat, rlat1, rlon1, rlat2, rlon2, delx, dely, ortru, proj
  integer :: nflds, nwf, irec
  integer :: itype, ilev, itime
  integer :: i,j,k,l,k4
  real(kind=4), allocatable :: sfld(:), dfld(:,:,:)
  real(kind=4), parameter :: rd=2.8705e2, rv=4.6150e2, fvirt=rv/rd-1.0
  real(kind=4), parameter :: pi=3.141592, rad2deg=180.0/pi
  integer,allocatable :: iparam(:)
  character(100) :: lname(255)
  
  print *, 'read and extract header record'
  iret = 0
  rewind(isig)
! read label
  read(isig) label
  print '(4a8)', label
! read header
  read(isig) fhour, idate, sisl, ext
  iymdh = idate(4)*1000000+idate(2)*10000+idate(3)*100+idate(1)
  print *, 'posting date ', iymdh, '+', nint(fhour)
  igrd1 = int(ext(3)); jgrd1 = int(ext(4))
  print *, 'igrd1, jgrd1'
  print '(2i4)', igrd1, jgrd1
  nwf = igrd1*jgrd1
  print *, 'nwf', nwf
  allocate( sfld(nwf) )
  allocate( lbm(nwf) )
  nflds=56
  allocate( dfld(igrd1,jgrd1,nflds) )
  allocate( iparam(nflds) )
!  allocate( clat(jgrd1),clon(igrd1), factor(igrd1,jgrd1,levs) )
  print *, 'start reading and writing data'
  rewind(iunit)
  ! dusfc
  l=1
  print *, 'itype ',izws,' ilev ',isfc
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'lbm ',lbm(1:min(10,nwf))
  print *,'idrt ',idrt,' igrd ',igrd2,' jgrd ',jgrd2
  print *,'maxbit ',maxbit,' colat ',colat
  print *,'ilpds ',ilpds,' iptv ',iptv,' icen ',icen,' igen ',igen
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'iyr ',iyr,' imo ',imo,' ida ',ida,' ihr ',ihr
  print *,'ifhour ',ifhour,' ifhr ',ifhr,' ithr ',ithr
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  print *,'rlat1 ',rlat1,' rlon1 ',rlon1,' rlat2 ',rlat2,' rlon2 ',rlon2
  print *,'delx ',delx,' dely ',dely
  print *,'ortru ',ortru,' proj ',proj
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read dusfc ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! dvsfc
  l=l+1
  print *, 'itype ',imws,' ilev ',isfc
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read dvsfc ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! dtsfc
  l=l+1
  print *, 'itype ',ishflx,' ilev ',isfc
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read dtsfc ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! dqsfc
  l=l+1
  print *, 'itype ',ilhflx,' ilev ',isfc
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read dqsfc ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! tsea
  l=l+1
  print *, 'itype ',itemp,' ilev ',isfc
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read tsea ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! smc(1)
  l=l+1
  print *, 'itype ',isoilm,' ilev ',i2dbls
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read smc(:,1) ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! smc(2)
  l=l+1
  print *, 'itype ',isoilm,' ilev ',i2dbls
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read smc(:,2) ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! stc(1)
  l=l+1
  print *, 'itype ',itemp,' ilev ',i2dbls
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read stc(:,1) ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! stc(2)
  l=l+1
  print *, 'itype ',itemp,' ilev ',i2dbls
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read stc(:,2) ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! sheleg
  l=l+1
  print *, 'itype ',isnowd,' ilev ',isfc
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read sheleg ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! dlwsfc
  l=l+1
  print *, 'itype ',idlwf,' ilev ',isfc
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read dlwsfc ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! ulwsfc
  l=l+1
  print *, 'itype ',iulwf,' ilev ',isfc
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read ulwsfc ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! raw fluxes
  do k=1,4
    l=l+1
    print *, 'itype ',ipur(k),' ilev ',itlr(k)
    read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
    &           ilpds,iptv,icen,igen,&
    &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
    &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
    &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
    ids(itype)=idstmp
  iparam(l)=itype
    print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
    print *,'il1k ',il1k,' il2k ',il2k
    print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
    print *,'ids ',ids(itype),' iens ',iens
    do j=1,jgrd1
      do i=1,igrd1
        dfld(i,j,l) = sfld(i+(j-1)*igrd1)
      end do
    end do 
    print *,l,'read raw flux ',k, dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  end do
  ! fixed fluxes for approx diurnal cycle
  do k=5,7
    l=l+1
    k4=4+(k-5)*4
    print *, 'itype ',ipur(k4+1),' ilev ',itlr(k4+1)
    read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
    &           ilpds,iptv,icen,igen,&
    &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
    &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
    &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
    ids(itype)=idstmp
  iparam(l)=itype
    print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
    print *,'il1k ',il1k,' il2k ',il2k
    print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
    print *,'ids ',ids(itype),' iens ',iens
    do j=1,jgrd1
      do i=1,igrd1
        dfld(i,j,l) = sfld(i+(j-1)*igrd1)
      end do
    end do 
    print *,l,'read fixed flux ',k4+1, dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
    l=l+1
    print *, 'itype ',ipur(k4+2),' ilev ',itlr(k4+2)
    read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
    &           ilpds,iptv,icen,igen,&
    &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
    &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
    &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
    ids(itype)=idstmp
  iparam(l)=itype
    print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
    print *,'il1k ',il1k,' il2k ',il2k
    print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
    print *,'ids ',ids(itype),' iens ',iens
    do j=1,jgrd1
      do i=1,igrd1
        dfld(i,j,l) = sfld(i+(j-1)*igrd1)
      end do
    end do 
    print *,l,'read fixed flux ',k4+2, dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
    l=l+1
    print *, 'itype ',ipur(k4+3),' ilev ',itlr(k4+3)
    read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
    &           ilpds,iptv,icen,igen,&
    &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
    &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
    &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
    ids(itype)=idstmp
  iparam(l)=itype
    print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
    print *,'il1k ',il1k,' il2k ',il2k
    print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
    print *,'ids ',ids(itype),' iens ',iens
    do j=1,jgrd1
      do i=1,igrd1
        dfld(i,j,l) = sfld(i+(j-1)*igrd1)
      end do
    end do 
    print *,l,'read fixed flux ',k4+3, dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
    l=l+1
    print *, 'itype ',ipur(k4+4),' ilev ',itlr(k4+4)
    read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
    &           ilpds,iptv,icen,igen,&
    &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
    &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
    &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
    ids(itype)=idstmp
  iparam(l)=itype
    print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
    print *,'il1k ',il1k,' il2k ',il2k
    print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
    print *,'ids ',ids(itype),' iens ',iens
    do j=1,jgrd1
      do i=1,igrd1
        dfld(i,j,l) = sfld(i+(j-1)*igrd1)
      end do
    end do 
    print *,l,'read fixed flux ',k4+4, dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  end do 
  ! geshem
  l=l+1
  print *, 'itype ',ipcpr,' ilev ',isfc
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read geshem ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! bengsh
  l=l+1
  print *, 'itype ',icpcpr,' ilev ',isfc
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read bengsh ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! gflux
  l=l+1
  print *, 'itype ',ighflx,' ilev ',isfc
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read gflux ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! slmsk
  l=l+1
  print *, 'itype ',islmsk,' ilev ',isfc
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read slmsk ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! cemsk
  l=l+1
  print *, 'itype ',icemsk,' ilev ',isfc
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read cemsk ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! u10
  l=l+1
  print *, 'itype ',iznlw,' ilev ',ielev
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read u10 ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! v10
  l=l+1
  print *, 'itype ',imerw,' ilev ',ielev
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read v10 ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! t2
  l=l+1
  print *, 'itype ',itemp,' ilev ',ielev
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read t2 ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! q2
  l=l+1
  print *, 'itype ',isphum,' ilev ',ielev
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read q2 ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! psurf
  l=l+1
  print *, 'itype ',iprs,' ilev ',isfc
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read psurf ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! t2max
  l=l+1
  print *, 'itype ',itmx,' ilev ',ielev
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read t2max ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! q2max
  l=l+1
  print *, 'itype ',iqmx,' ilev ',ielev
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read q2max ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! t2min
  l=l+1
  print *, 'itype ',itmn,' ilev ',ielev
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read t2min ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! q2min
  l=l+1
  print *, 'itype ',iqmn,' ilev ',ielev
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read q2min ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! runoff
  l=l+1
  print *, 'itype ',irnof,' ilev ',isfc
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read runoff ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! ep
  l=l+1
  print *, 'itype ',iep,' ilev ',isfc
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read ep ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! cldwrk
  l=l+1
  print *, 'itype ',icldwk,' ilev ',icolmn
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read cldwrk ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! dugwd
  l=l+1
  print *, 'itype ',izgw,' ilev ',isfc
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read dugwd ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! dvgwd
  l=l+1
  print *, 'itype ',imgw,' ilev ',isfc
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read dvgwd ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! hpbl
  l=l+1
  print *, 'itype ',ihpbl,' ilev ',isfc
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read hpbl ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! pwat
  l=l+1
  print *, 'itype ',ipwat,' ilev ',icolmn
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read pwat ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! albedo(percent)
  l=l+1
  print *, 'itype ',ialbdo,' ilev ',isfc
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read albedo ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! cldf
  l=l+1
  print *, 'itype ',icldf,' ilev ',icolmn
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read cldf ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! wvuflx
  l=l+1
  print *, 'itype ',242,' ilev ',icolmn
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
!  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',idstmp,' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read wvuflx ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! wvvflx
  l=l+1
  print *, 'itype ',243,' ilev ',icolmn
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
!  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',idstmp,' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read wvvflx ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! srunoff
  l=l+1
  print *, 'itype ',235,' ilev ',isfc
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
!  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',idstmp,' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read srunoff ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! soilm
  l=l+1
  print *, 'itype ',86,' ilev ',i2dbls
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  iparam(l)=itype
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read soilm ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  ! snwdph
  l=l+1
  print *, 'itype ',66,' ilev ',isfc
  read(iunit) sfld,lbm,idrt,igrd2,jgrd2,maxbit,colat, &
  &           ilpds,iptv,icen,igen,&
  &           ibm,itype,ilev,il1k,il2k,iyr,imo,ida,ihr,&
  &           ifhour,ifhr,ithr,itime,ina,inm,icen2,idstmp,iens,&
  &           rlat1,rlon1,rlat2,rlon2,delx,dely,ortru,proj
  ids(itype)=idstmp
  print *,'ibm ',ibm,' itype ',itype,' ilev ',ilev
  print *,'il1k ',il1k,' il2k ',il2k
  print *,'itime ',itime,' ina ',ina,' inm ',inm,' icen2 ',icen2
  print *,'ids ',ids(itype),' iens ',iens
  do j=1,jgrd1
    do i=1,igrd1
      dfld(i,j,l) = sfld(i+(j-1)*igrd1)
    end do
  end do 
  print *,l,'read snwdph ', dfld(1,1,l), maxval(dfld(:,:,l)), minval(dfld(:,:,l))
  print *,'end reading flux file'
  print *, 'start write output'
  ! open output
  open(ounit, FORM='unformatted', access='direct',&
&      convert='big_endian', recl=4*nwf)
  irec=1
  !dusfc
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !dvsfc
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !dtsfc
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !dqsfc
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !tsea
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !smc
  do k=1,lsoil
    write(ounit, rec=irec) dfld(:,:,irec)
    irec=irec+1
  end do
  !stc
  do k=1,lsoil
    write(ounit, rec=irec) dfld(:,:,irec)
    irec=irec+1
  end do
  !sheleg
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !dlwsfc
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !ulwsfc
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !raw flux
  do k=1,4
    write(ounit, rec=irec) dfld(:,:,irec)
    irec=irec+1
  end do
  !fixed flux
  do k=5,7
    do l=1,4
      write(ounit, rec=irec) dfld(:,:,irec)
      irec=irec+1
    end do
  end do
  !geshem
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !bengsh
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !gflux
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !slmsk
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !cemsk
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !u10
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !v10
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !t2
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !q2
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !psurf
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !t2max
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !q2max
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !t2min
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !q2min
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !runoff
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !ep
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !cldwrk
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !dugwd
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !dvgwd
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !hpbl
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !pwat
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !albedo
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !cldf
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !wvuflx
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !wvvflx
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !srunoff
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !soilm
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  !snwdph
  write(ounit, rec=irec) dfld(:,:,irec)
  irec=irec+1
  close(ounit)
  print *, 'end write output'
  print *, 'generate control file'
  call genctl(cunit,idate,fhour)
  stop
contains
  subroutine genctl(nctl,idate,fhour)
    implicit none
    integer, intent(in) :: nctl
    integer, intent(in) :: idate(4)
    real(kind=4), intent(in) :: fhour
    integer :: i,j
    integer :: ihr,idy,imo,iyr
    integer :: days(12),daysl(12)
    data days/31,28,31,30,31,30,31,31,30,31,30,31/
    data daysl/31,29,31,30,31,30,31,31,30,31,30,31/ !leap year
    character(len=2) hour, day
    character(len=3) mon(12)
    data mon/'JAN','FEB','MAR','APR','MAY','JUN',&
&            'JUL','AUG','SEP','OCT','NOV','DEC'/

    write(nctl,112) lsoil
 112  format('zdef',I5,' linear 1 1')
    ihr = idate(1) + nint(fhour)
    idy = idate(3)
    imo = idate(2)
    iyr = idate(4)
    do while (ihr .ge. 24) 
      idy=idy+1
      ihr=ihr-24
    end do
    if (mod(iyr,4).eq.0) then
      if(idy.gt.daysl(imo)) then
        imo=imo+1
        idy=idy-daysl(imo-1)
      end if
    else
      if(idy.gt.days(imo)) then
        imo=imo+1
        idy=idy-days(imo-1)
      end if
    end if
    if (imo.gt.12) then
      iyr=iyr+1
      imo=imo-12
    end if
    write(hour,'(i2.2)') ihr
    write(day, '(i2.2)') idy
    write(nctl,114) hour,day,mon(imo),iyr
 114 format('tdef 1 linear ',A2,'Z',A2,A3,I4,'   1hr')
    write(nctl,'(a)') 'vars 54'
    write(nctl,'(a)') 'dusfc 0 99 u wind stress (N/m2)'
    write(nctl,'(a)') 'dvsfc 0 99 v wind stress (N/m2)'
    write(nctl,'(a)') 'shflx 0 99 sensible heat flux (W/m2)'
    write(nctl,'(a)') 'lhflx 0 99 latent heat flux (W/m2)'
    write(nctl,'(a)') 'tsea 0 99 land and sea surface temperature (K)'
    write(nctl,'(a,i2,a)') 'smc ',lsoil,' 99 two layers of soil moisture contents (0.47 - 0.1)'
    write(nctl,'(a,i2,a)') 'stc ',lsoil,' 99 two layers of soil temperature (K)'
    write(nctl,'(a)') 'sheleg 0 99 water equivalent of snow depth (kg/m2)'
    write(nctl,'(a)') 'dlwsfc 0 99 downward longwave radiative flux at surface (W/m2)'
    write(nctl,'(a)') 'ulwsfc 0 99 upward longwave radiative flux at surface (W/m2)'
    write(nctl,'(a)') 'ulwtoa 0 99 upward longwave radiative flux at top of atmosphere (W/m2)'
    write(nctl,'(a)') 'uswtoa 0 99 upward shortwave radiative flux at top of atmosphere (W/m2)'
    write(nctl,'(a)') 'uswsfc 0 99 upward shortwave radiative flux at surface (W/m2)'
    write(nctl,'(a)') 'dswsfc 0 99 upward shortwave radiative flux at surface (W/m2)'
    write(nctl,'(a)') 'hclc 0 99 high cloud cover (percent)'
    write(nctl,'(a)') 'hcltp 0 99 high cloud top pressure (pa)'
    write(nctl,'(a)') 'hclbp 0 99 high cloud bottom pressure (pa)'
    write(nctl,'(a)') 'hcltt 0 99 high cloud top temperature (K)'
    write(nctl,'(a)') 'mclc 0 99 middle cloud cover (percent)'
    write(nctl,'(a)') 'mcltp 0 99 middle cloud top pressure (pa)'
    write(nctl,'(a)') 'mclbp 0 99 middle cloud bottom pressure (pa)'
    write(nctl,'(a)') 'mclt 0 99 middle cloud top temperature (K)'
    write(nctl,'(a)') 'lclc 0 99 low cloud cover (percent)'
    write(nctl,'(a)') 'lcltp 0 99 low cloud top pressure (pa)'
    write(nctl,'(a)') 'lclbp 0 99 low cloud bottom pressure (pa)'
    write(nctl,'(a)') 'lclt 0 99 low cloud top temperature (K)'
    write(nctl,'(a)') 'pcpr 0 99 precipitation rate (kg/m2/s)'
    write(nctl,'(a)') 'cpcpr 0 99 convective precipitation rate (kg/m2/s)'
    write(nctl,'(a)') 'ghflx 0 99 ground head flux (W/m2)'
    write(nctl,'(a)') 'slmsk 0 99 sea-land mask'
    write(nctl,'(a)') 'cemsk 0 99 ice concentration'
    write(nctl,'(a)') 'u10 0 99 10-m zonal wind (m/s)'
    write(nctl,'(a)') 'v10 0 99 10-m meridional wind (m/s)'
    write(nctl,'(a)') 't2 0 99 2-m temperature (K)'
    write(nctl,'(a)') 'q2 0 99 2-m specific humidity (kg/kg)'
    write(nctl,'(a)') 'psfc 0 99 surface pressure (pa)'
    write(nctl,'(a)') 't2max 0 99 2-m maximum temperature (K)'
    write(nctl,'(a)') 'q2max 0 99 2-m maximum specific humidity (kg/kg)'
    write(nctl,'(a)') 't2min 0 99 2-m minimum temperature (K)'
    write(nctl,'(a)') 'q2min 0 99 2-m minimum specific humidity (kg/kg)'
    write(nctl,'(a)') 'runoff 0 99 runoff (kg/m2)'
    write(nctl,'(a)') 'ep 0 99 potential evaporation rate (W/m2)'
    write(nctl,'(a)') 'cldwrk 0 99 cloud work function (J/kg)'
    write(nctl,'(a)') 'dugwd 0 99 u gravity wave stress (N/m2)'
    write(nctl,'(a)') 'dvgwd 0 99 v gravity wave stress (N/m2)'
    write(nctl,'(a)') 'hpbl 0 99 planetary boundary layer height (m)'
    write(nctl,'(a)') 'tpw 0 99 precipitable water (kg/m2)'
    write(nctl,'(a)') 'albedo 0 99 albedo (percent)'
    write(nctl,'(a)') 'tclc 0 99 total cloud cover (percent)'
    write(nctl,'(a)') 'wvuflx 0 99 water vapor zonal flux ()' 
    write(nctl,'(a)') 'wvvflx 0 99 water vapor meridional flux ()' 
    write(nctl,'(a)') 'srunoff 0 99 storm surface runoff (kg/m2)'
    write(nctl,'(a)') 'soilm 0 99 soil wetness between two layers (kg/m2)'
    write(nctl,'(a)') 'snwdph 0 99 snow depth (m)'
    write(nctl,'(a)') 'endvars'
    return
  end subroutine
end 
