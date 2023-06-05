program test_get_cc
    use kind_module
    use rsmcom_module, only: set_rsmparm, igrd1, jgrd1, nlev, &
    & nproj, fhour, idate, nonhyd, rlon, rlat, sig, sigh, filesuffix, clean_rsmparm
    use read_module, only: read_sig, read_sfc, nfldsfc !, read_flx
    implicit none
    character(len=4), parameter :: cfile='init'
    character(len=12) :: filename
    real(kind=dp), allocatable :: t(:,:,:), q(:,:,:), cw(:,:,:), w(:,:,:)
    real(kind=dp), allocatable :: f_ice(:,:,:), f_rain(:,:,:)
    real(kind=dp), allocatable :: clw(:,:,:), ciw(:,:,:), crw(:,:,:)
    real(kind=dp), allocatable :: ps(:,:)
    real(kind=dp), allocatable :: prsl(:,:,:), prsi(:,:,:)
    real(kind=dp), allocatable :: slmsk(:,:), cv(:,:), cvt(:,:), cvb(:,:)
    real(kind=dp), allocatable :: cldtot(:,:,:), cldcnv(:,:,:), cldsa(:,:,:)
    real(kind=dp), allocatable :: dfld(:,:,:), dummap(:,:,:), dumlat(:), dumlon(:)
    real(kind=sp), allocatable :: buf4(:,:)
    integer :: n,nflds,icld
    integer :: i,j,k
    
    ! initialize
    call set_rsmparm(cfile)
    allocate( ps(igrd1,jgrd1) )
    allocate( t(igrd1,jgrd1,nlev) )
    allocate( q(igrd1,jgrd1,nlev) )
    allocate( cw(igrd1,jgrd1,nlev) )
    allocate( clw(igrd1,jgrd1,nlev) )
    allocate( ciw(igrd1,jgrd1,nlev) )
    allocate( crw(igrd1,jgrd1,nlev) )
    allocate( f_ice(igrd1,jgrd1,nlev) )
    allocate( f_rain(igrd1,jgrd1,nlev) )
    allocate( w(igrd1,jgrd1,nlev+1) )
    allocate( prsl(igrd1,jgrd1,nlev) )
    allocate( prsi(igrd1,jgrd1,nlev+1) )
    allocate( slmsk(igrd1,jgrd1) )
    allocate( cv(igrd1,jgrd1) )
    allocate( cvt(igrd1,jgrd1) )
    allocate( cvb(igrd1,jgrd1) )

    ! read field
    n=11
    !! sig
    nflds = 2 + nlev*9 + 1
    if (fhour > 0) then
      icld=1
      nflds = nflds + 3*nlev ! phys_3d
    end if
    allocate( dfld(igrd1,jgrd1,nflds) )
    allocate( dummap(igrd1,jgrd1,3) )
    allocate( dumlon(igrd1), dumlat(jgrd1) )
    filename = cfile//'.sig'//filesuffix
    open(n,file=filename,&
    & access='sequential',form='unformatted',&
    & action='read')
    call read_sig(n,igrd1,jgrd1,nlev,nflds,nonhyd,icld,fhour,sig,&
    & dfld,dummap,dumlat,dumlon,convert=.true.)
    close(n)
    ps(:,:) = dfld(:,:,2)
    if (nonhyd.eq.1) then
      i=7*nlev+2
    else
      i=2
    end if
    do k=1,nlev
      t(:,:,k) = dfld(:,:,i+k)
    end do
    i=3*nlev+2
    do k=1,nlev
      q(:,:,k) = dfld(:,:,i+k)
    end do
    i=5*nlev+2
    do k=1,nlev
      cw(:,:,k) = dfld(:,:,i+k)
    end do
    !i=6*nlev+2
    prsi(:,:,1) = ps * 1.0e-3 !Pa=>kPa
    do k=1,nlev
    !  prsl(:,:,k) = dfld(:,:,i+k) * 1.0e-3 !Pa=>kPa
      prsl(:,:,k) = ps(:,:) * sig(k) * 1.0e-3 !Pa=>kPa
      prsi(:,:,k+1) = ps(:,:) * sigh(k+1) * 1.0e-3 !Pa=>kPa
    end do
!    do k=1,nlev
!      prsi(:,:,k+1) = 0.5*(prsl(:,:,k)+prsl(:,:,k+1))
!    end do
    i=8*nlev+2
    do k=1,nlev+1
      w(:,:,k) = dfld(:,:,i+k)
    end do
    i=9*nlev+3
    clw=0.0
    ciw=0.0
    do k=1,nlev
      f_ice(:,:,k) = dfld(:,:,i+k)
      ciw(:,:,k) = f_ice(:,:,k)*cw(:,:,k)
      clw(:,:,k) = cw(:,:,k) - ciw(:,:,k)
    end do
    i=10*nlev+3
    crw=0.0
    do k=1,nlev
      f_rain(:,:,k) = dfld(:,:,i+k)
      crw(:,:,k) = f_rain(:,:,k)*clw(:,:,k)
    end do
    !!! check
    do k=1,nlev
      print *, k,'t max=',maxval(t(:,:,k)),' min=',minval(t(:,:,k))
      print *, k,'q max=',maxval(q(:,:,k)),' min=',minval(q(:,:,k))
      print *, k,'p max=',maxval(prsl(:,:,k)),' min=',minval(prsl(:,:,k))
      print *, k,'pi max=',maxval(prsi(:,:,k)),' min=',minval(prsi(:,:,k))
      print *, k,'w max=',maxval(w(:,:,k)),' min=',minval(w(:,:,k))
      print *, k,'f_ice max=',maxval(f_ice(:,:,k)),' min=',minval(f_ice(:,:,k))
      print *, k,'f_rain max=',maxval(f_rain(:,:,k)),' min=',minval(f_rain(:,:,k))
      print *, k,'ciw max=',maxval(ciw(:,:,k)),' min=',minval(ciw(:,:,k))
      print *, k,'clw max=',maxval(clw(:,:,k)),' min=',minval(clw(:,:,k))
      print *, k,'crw max=',maxval(crw(:,:,k)),' min=',minval(crw(:,:,k))
    end do
    k=nlev+1
    print *, k,'pi max=',maxval(prsi(:,:,k)),' min=',minval(prsi(:,:,k))
    print *, k,'w max=',maxval(w(:,:,k)),' min=',minval(w(:,:,k))
    deallocate( dfld )
    !! sfc
    allocate( dfld(igrd1,jgrd1,nfldsfc) )
    filename = cfile//'.sfc'//filesuffix
    open(n,file=filename,&
    & access='sequential',form='unformatted',&
    & action='read')
    call read_sfc(n,igrd1,jgrd1,dfld)
    close(n)
    cv(:,:) = dfld(:,:,9)
    cvb(:,:) = dfld(:,:,10)
    cvt(:,:) = dfld(:,:,11)
    slmsk(:,:) = dfld(:,:,16)
    !!! check
    print *, 'cv max=',maxval(cv),' min=',minval(cv)
    print *, 'cvt max=',maxval(cvt),' min=',minval(cvt)
    print *, 'cvb max=',maxval(cvb),' min=',minval(cvb)
    print *, 'slmsk max=',maxval(slmsk),' min=',minval(slmsk)

    ! get cc
    allocate( cldtot(igrd1,jgrd1,nlev) )
    allocate( cldcnv(igrd1,jgrd1,nlev) )
    allocate( cldsa(igrd1,jgrd1,5) )
    call get_cc(t,q,cw,w,slmsk,cv,cvt,cvb,prsi,prsl,&
    & cldtot,cldcnv,cldsa)
    !!! check
    do k=1,nlev
      print *, k,'cldtot max=',maxval(cldtot(:,:,k)),' min=',minval(cldtot(:,:,k))
      print *, k,'cldcnv max=',maxval(cldcnv(:,:,k)),' min=',minval(cldcnv(:,:,k))
    end do
    do k=1,5
      print *, k,'cldsa max=',maxval(cldsa(:,:,k)),' min=',minval(cldsa(:,:,k))
    end do

    ! writing output
    allocate( buf4(igrd1,jgrd1) )
    n=55
    filename = cfile//'.cld'//filesuffix
    open(n,file=filename,form='unformatted',access='direct',&
    & convert='big_endian',recl=4*igrd1*jgrd1)
    i=1
    do k=1,nlev
      buf4 = real(prsl(:,:,k)*1.0e3,kind=sp)
      write(n,rec=i) buf4
      i=i+1
    end do
    do k=1,nlev
      buf4 = real(ciw(:,:,k),kind=sp)
      write(n,rec=i) buf4
      i=i+1
    end do
    do k=1,nlev
      buf4 = real(clw(:,:,k),kind=sp)
      write(n,rec=i) buf4
      i=i+1
    end do
    do k=1,nlev
      buf4 = real(crw(:,:,k),kind=sp)
      write(n,rec=i) buf4
      i=i+1
    end do
    do k=1,nlev
      buf4 = real(cldtot(:,:,k),kind=sp)
      write(n,rec=i) buf4
      i=i+1
    end do
    do k=1,nlev
      buf4 = real(cldcnv(:,:,k),kind=sp)
      write(n,rec=i) buf4
      i=i+1
    end do
    do k=1,5
      buf4 = real(cldsa(:,:,k),kind=sp)
      write(n,rec=i) buf4
      i=i+1
    end do
    close(n)
    n=60
    filename = cfile//'.cld.ctl'
    open(n,file=filename)
    call genctl(n)

    call clean_rsmparm
contains
    subroutine genctl(nctl)
      implicit none
      integer, intent(in) :: nctl
      integer :: i,j
      integer :: ihr,idy,imo,iyr
      integer :: days(12),daysl(12)
      data days/31,28,31,30,31,30,31,31,30,31,30,31/
      data daysl/31,29,31,30,31,30,31,31,30,31,30,31/ !leap year
      character(len=2) hour, day
      character(len=3) mon(12)
      data mon/'JAN','FEB','MAR','APR','MAY','JUN',&
  &            'JUL','AUG','SEP','OCT','NOV','DEC'/
      integer :: ncmem
      character(len=256) :: cmem 
  
      write(nctl,'(4a)') 'dset ^',cfile,'.cld',filesuffix
      write(nctl,'(a)') 'options big_endian'
      write(nctl,'(a)') 'undef -9.99E+33'
      if (nproj .eq. 0) then
        !write(6,108) igrd1,rlon(1),rlon(2)-rlon(1)
        write(nctl,108) igrd1,rlon(1),rlon(2)-rlon(1)
   108  format('xdef',I5,' linear',2G14.6)
        !write(6,110) jgrd1
        write(nctl,110) jgrd1
   110  format('ydef',I5,' levels')
        !write(6,111) (rlat(j),j=1,jgrd1)
        write(nctl,111) (rlat(j),j=1,jgrd1)
   111  format(5G14.6)
      end if
      !write(6,112) nlev
      write(nctl,112) nlev
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
      !write(6,114) hour,day,mon(imo),iyr
      write(nctl,114) hour,day,mon(imo),iyr
   114 format('tdef 1 linear ',A2,'Z',A2,A3,I4,'   1hr')
      write(nctl,'(a)') 'vars 11'
      write(nctl,'(a,i2,a)') 'p ',nlev,' 99 pressure'
      write(nctl,'(a,i2,a)') 'ciw ',nlev,' 99 cloud ice water'
      write(nctl,'(a,i2,a)') 'clw ',nlev,' 99 cloud liquid water'
      write(nctl,'(a,i2,a)') 'crw ',nlev,' 99 cloud rain water'
      write(nctl,'(a,i2,a)') 'cldtot ',nlev,' 99 cloud fraction for stratiform'
      write(nctl,'(a,i2,a)') 'cldcnv ',nlev,' 99 cloud fraction for convective'
      write(nctl,'(a)') 'cldsa_low 1 99 low cloud fraction'
      write(nctl,'(a)') 'cldsa_mid 1 99 middle cloud fraction'
      write(nctl,'(a)') 'cldsa_high 1 99 high cloud fraction'
      write(nctl,'(a)') 'cldsa_tot 1 99 total cloud fraction'
      write(nctl,'(a)') 'cldsa_bl 1 99 bottom-low cloud fraction'
      write(nctl,'(a)') 'endvars'
      return
    end subroutine
end program
