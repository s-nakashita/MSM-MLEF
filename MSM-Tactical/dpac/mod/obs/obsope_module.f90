module obsope_module
!
! observation operators
!
! history:
! 22-12-05 create
!
  use kind_module
  use nml_module
  use co_module
  use rsmcom_module
  use corsm_module
  use func_module, only : calc_pfull, calc_td, calc_wd, calc_rh, prsadj
  use obs_module
  implicit none
  private
  integer, parameter :: stnout=7 ! debug

  public :: obsope_serial, obsope_parallel, obsope_update, monit_dep, monit_print, &
          & obsmake_cal
contains
!
! (parallel) main routine for observation operator
!
  subroutine obsope_parallel(obsin,obsout,gues3dc,gues2dc,gues3d,gues2d)
    implicit none
    type(obstype), intent(in) :: obsin(obsin_num)
    type(obstype2), intent(inout):: obsout
    real(kind=dp), intent(in) :: gues3dc(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d)
    real(kind=dp), intent(in) :: gues2dc(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,     nv2d)
    real(kind=dp), intent(in), optional :: gues3d (1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,member,nv3d)
    real(kind=dp), intent(in), optional :: gues2d (1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,     member,nv2d)
    integer :: nobsin
    integer :: nobsout
    integer :: iof
    character(len=filelenmax) :: guesf, outf
    real(kind=dp), allocatable :: v3d(:,:,:,:)
    real(kind=dp), allocatable :: v2d(:,:,  :)
    real(kind=dp), allocatable :: p_full(:,:,:)
    real(kind=dp) :: ri,rj,rk
    integer :: im, n, nn, img
    real(kind=dp), allocatable :: wk2d(:,:)[:]
    integer, allocatable :: iwk2d(:,:)[:]
    integer :: is,ie,js,je
    integer, dimension(1) :: kk
    integer :: k
!!! for single point observation
    logical :: single_use=.true.
    real(kind=dp) :: lonb,latb
    real(kind=dp) :: hxf,dep

    nobsin=0
    do iof=1,obsin_num
      nobsin=nobsin+obsin(iof)%nobs
    end do
    obsout%nobs = nobsin + obsout%nobs ! obsout%nobs:externally processed obs number
    if(obsout%nobs.le.0) then
      write(6,'(a)') 'no observation to be assimilated'
      return
    end if
    call obsout_allocate(obsout,member)

    if(debug_obs) open(stnout,file='station_synop.txt')
    allocate( v3d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d) )
    allocate( v2d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,     nv2d) )
    allocate( p_full(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev) )
    if(.not.mean) then
      im=0
    else
      im=1
    end if
    allocate( iwk2d(im:member,obsout%nobs)[*] )
    allocate( wk2d(im:member,obsout%nobs)[*] )
    iwk2d=0
    wk2d=0.0d0
    is=1
    ie=ni1+ighost
    if(nidom(myimage)==nisep) ie=ni1
    js=1
    je=nj1+jghost
    if(njdom(myimage)==njsep) je=nj1
    do while(im<=member)
      if(single_obs) single_use=.true.
      nobsout=0
      if(im.gt.0.and.present(gues3d)) then
        v3d=gues3d(:,:,:,im,:)
        v2d=gues2d(:,:,  im,:)
      else
        v3d=gues3dc
        v2d=gues2dc
      end if
      write(6,'(a,i4,a,2f10.2)')&
              'member ',im,' v3d(iv3d_t)= ',maxval(v3d(1:ni1,1:nj1,:,iv3d_t)),minval(v3d(1:ni1,1:nj1,:,iv3d_t))
      write(6,'(a,i4,a,2f10.2)')&
              'member ',im,' v2d(iv2d_ps)=',maxval(v2d(1:ni1,1:nj1,iv2d_ps)),minval(v2d(1:ni1,1:nj1,iv2d_ps))
      if(nonhyd.eq.1) then !non-hydrostatic
        p_full = v3d(:,:,:,iv3d_pn)
      else
        call calc_pfull(ni1max+2*ighost,nj1max+2*jghost,nlev,sig,v2d(:,:,iv2d_ps),p_full)
      end if
      write(6,'(a,i4,a,2f10.2)') 'member ',im,' p_full=',maxval(p_full(1:ni1,1:nj1,:)),minval(p_full(1:ni1,1:nj1,:))
      lonb=undef
      latb=undef
      do iof=1,obsin_num
        do n=1,obsin(iof)%nobs
          !!! single point observation
          if(single_obs.and.myimage==print_img) then
            if(single_use.and.(lonb.ne.undef).and.(latb.ne.undef)) then
              if(lonb.ne.obsin(iof)%lon(n).or.latb.ne.obsin(iof)%lat(n)) then
                write(6,'(a,i5,a,f10.2,a,f10.2)') &
                        'MYIMAGE ',myimage,' observation point : lon=',lonb,' lat=',latb
                single_use=.false.
                !exit
              end if
            end if
          end if
          nobsout=nobsout+1
          obsout%elem(nobsout) = obsin(iof)%elem(n)
          obsout%lon(nobsout)  = obsin(iof)%lon(n)
          obsout%lat(nobsout)  = obsin(iof)%lat(n)
          obsout%lev(nobsout)  = obsin(iof)%lev(n)
          obsout%dat(nobsout)  = obsin(iof)%dat(n)
          obsout%err(nobsout)  = obsin(iof)%err(n)
          obsout%dmin(nobsout) = obsin(iof)%dmin(n)
          !kk = minloc(abs(obsin(iof)%lev(n)-plevfix(:)))
          !k=kk(1)
          !obsout%err(nobsout)  = obserr(k,uid_obs(obsin(iof)%elem(n)))
          ! horizontal domain check
          if(    obsin(iof)%lon(n).le.rlon(1) &
             .or.obsin(iof)%lon(n).ge.rlon(nlon)&
             .or.obsin(iof)%lat(n).le.rlat(1) &
             .or.obsin(iof)%lat(n).ge.rlat(nlat)) then
            if(myimage.eq.print_img) &
            write(0,'(a,2(a,f8.2))') &
            & 'warning: observation is outside of the horizontal domain ', &
            & 'lon=',obsin(iof)%lon(n),' lat=',obsin(iof)%lat(n)
            iwk2d(im,nobsout)=iqc_out_h
            cycle
          end if
          ! search which image contains observation
          if(    obsin(iof)%lon(n).lt.myrlon(is)&
             .or.obsin(iof)%lon(n).ge.myrlon(ie)&
             .or.obsin(iof)%lat(n).lt.myrlat(js)&
             .or.obsin(iof)%lat(n).ge.myrlat(je)) then
          else !obsout%img == myimage
            call phys2ijk(p_full,obsin(iof)%elem(n),&
             &  obsin(iof)%lon(n),obsin(iof)%lat(n),obsin(iof)%lev(n), &
             &  ri,rj,rk,iwk2d(im,nobsout),.true.)
            if(iwk2d(im,nobsout).eq.iqc_good) then
              if(.not.luseobs(uid_obs(obsin(iof)%elem(n)))) then
                iwk2d(im,nobsout)=iqc_otype
              else
                if(debug_obs) then
                  if(obsin(iof)%elem(n)==id_ps_obs) then
                    print *, 'zobs   ',rk,' psobs ',obsin(iof)%dat(n)
                  else if(obsin(iof)%elem(n)==id_t2m_obs) then
                    print *, 'zobs   ',rk,' tobs ',obsin(iof)%dat(n)
                  end if
                end if
                call trans_xtoy(obsin(iof)%elem(n),ri,rj,rk,&
                 &  v3d,v2d,p_full,wk2d(im,nobsout)&
                 &  ,obsout%corr(nobsout))
!!debug
!              if(debug_obs) then
!                dep = obsin(iof)%dat(n) - wk2d(im,nobsout)
!                print *, obsin(iof)%elem(n),obsin(iof)%lon(n),obsin(iof)%lat(n),&
!                 &  obsin(iof)%lev(n)
!                print *,obsin(iof)%dat(n),wk2d(im,nobsout), dep
!              end if
!!debug
              end if !luseobs
              if(single_obs) then
                if(myimage/=print_img.or.(.not.single_use)) then
                  iwk2d(im,nobsout)=iqc_out_h
                  wk2d(im,nobsout)=0.0d0
                else
                  if(lonb.eq.undef.and.latb.eq.undef) then
                    lonb=obsin(iof)%lon(n)
                    latb=obsin(iof)%lat(n)
                    if(latb.lt.lats.or.latb.gt.latn.or.&
                     lonb.lt.lonw.or.lonb.gt.lone) then
                      lonb=undef; latb=undef
                      iwk2d(im,nobsout)=iqc_out_h
                      wk2d(im,nobsout)=0.0d0
                    end if
                  end if
                end if !single_img
              end if !single_obs
            end if !iqc_good
          end if !obsout%img==myimage
        end do ! n=1,obsin(iof)%nobs
      end do ! iof=1,obsin_num

      im=im+1
    end do !do while (im<=member)
    sync all
    ! broadcast hxf, qc
    if(myimage.eq.print_img) then
      do img=1,nimages
        if(myimage.eq.img) cycle
        do n=1,nobsout
          if(.not.mean) then
            im=0
          else
            im=1
          end if
          do while(im<=member) 
            wk2d(im,n)[myimage]=wk2d(im,n)[myimage]+wk2d(im,n)[img]
            iwk2d(im,n)[myimage]=max(iwk2d(im,n)[myimage],iwk2d(im,n)[img])
            im=im+1
          end do
        end do
      end do
      do img=1,nimages
        wk2d(:,:)[img] = wk2d(:,:)[myimage]
        iwk2d(:,:)[img] = iwk2d(:,:)[myimage]
      end do
    end if
    sync all
    do n=1,nobsout
      if(.not.mean) then
        obsout%hxf(n)=wk2d(0,n)
      end if
      if(member.gt.0) then
      do im=1,member
        obsout%hxe(im,n)=wk2d(im,n)
      end do
      end if
    end do
    if(obs_out) then
      if(.not.mean) then
        im=0
      else
        im=1
      end if
      im=im+myimage-1
      do while(im<=member)
        call file_member_replace(im,obsout_basename,outf)
        write(6,'(a,i4.4,2a)') 'MYIMAGE ',myimage,' is writing a file ',trim(outf)
        do n=1,nobsout
          obsout%qc(n) = iwk2d(im,n)
        end do
        if(debug_obs) then
          print '(10a10)','number','elem','lon','lat','lev','dat','err','dmin','hxf','qc'
          nn=0
          do n=1,nobsout
            if(obsout%qc(n)/=iqc_good) cycle
            nn=nn+1
            if(nn.gt.nobsmax) exit
            if(im.eq.0) then
              hxf=obsout%hxf(n)
            else
              hxf=obsout%hxe(im,n)
            end if
            print '(i10,a10,2f10.2,f10.1,4es10.2,i10)', &
         &  n,obelmlist(uid_obs(obsout%elem(n))),&
         &  obsout%lon(n),obsout%lat(n),obsout%lev(n),obsout%dat(n),&
         &  obsout%err(n),obsout%dmin(n), &
         &  hxf,obsout%qc(n)
          end do
        end if
        call write_obsout(outf,obsout,im)
        im=im+nimages
      end do
      sync all
    end if
    do n=1,nobsout
      obsout%qc(n) = maxval(iwk2d(:,n))
    end do
    ! broadcast image
    if(.not.mean) then
      im=0
    else
      im=1
    end if
    iwk2d=0
    iwk2d(im,1:nobsout) = obsout%img(1:nobsout)
    if(myimage.eq.print_img) then
      do img=1,nimages
        if(img.eq.myimage) cycle
        iwk2d(im,1:nobsout)[myimage]=iwk2d(im,1:nobsout)[myimage] + iwk2d(im,1:nobsout)[img]
      end do
      do img=1,nimages
        iwk2d(im,1:nobsout)[img] = iwk2d(im,1:nobsout)[myimage]
      end do
    end if
    sync all
    obsout%img(1:nobsout) = iwk2d(im,1:nobsout)
!    if(debug_obs) write(6,*) 'obs%img=',obsout%img(1:nobsout)
    deallocate( v3d,v2d )
    deallocate( wk2d,iwk2d )
    if(debug_obs) close(stnout)
    return
  end subroutine obsope_parallel
!
! (serial) main routine for observation operator
!
  subroutine obsope_serial(obsin,obsout)
    implicit none
    type(obstype), intent(in) :: obsin(obsin_num)
    type(obstype2), intent(inout):: obsout
    integer :: nobsin,nobsout
    integer :: iof
    character(len=filelenmax) :: guesf, outf
    real(kind=dp) :: v3dg(nlon,nlat,nlev,nv3d)
    real(kind=dp) :: v2dg(nlon,nlat,nv2d)
    real(kind=dp) :: p_full(nlon,nlat,nlev)
    real(kind=dp) :: ri,rj,rk
    integer :: im, n, nn
    integer, allocatable :: tmpqc(:)
    integer, dimension(1) :: kk
    integer :: k
!!! for single point observation
    real(kind=dp) :: lonb,latb
    real(kind=dp) :: hxf,dep

    nobsin=0
    do iof=1,obsin_num
      nobsin=nobsin+obsin(iof)%nobs
    end do
    obsout%nobs = nobsin + obsout%nobs !obsout%nobs : externally processes obs number
    if(obsout%nobs.le.0) then
      write(6,'(a)') 'no observation to be assimilated'
      return
    end if
    call obsout_allocate(obsout,member)
    allocate( tmpqc(nobsin) )
   
    do im=0,member
      nobsout=0
      nn=0
      call file_member_replace(im,fguess_basename,guesf)
      call read_restart(guesf,v3dg,v2dg)
!      if(im.eq.0) then !all members assumed to have the same pressure levels
        if(nonhyd.eq.1) then !non-hydrostatic
          p_full = v3dg(:,:,:,iv3d_pn)
        else
          call calc_pfull(nlon,nlat,nlev,sig,v2dg(:,:,iv2d_ps),p_full)
        end if
      write(6,'(a,i4,a,2f10.2)') 'member ',im,' p_full=',maxval(p_full),minval(p_full)
!      end if
      lonb=undef
      latb=undef
      do iof=1,obsin_num
        do n=1,obsin(iof)%nobs
!          if(nobsmax.gt.0.and.nn.ge.nobsmax) exit
          !!! single point observation
          if(single_obs) then
            if(lonb.ne.undef.and.latb.ne.undef) then
              if(lonb.ne.obsin(iof)%lon(n).or.latb.ne.obsin(iof)%lat(n)) then
                write(6,'(a,f10.2,a,f10.2)') 'observation point : lon=',lonb,' lat=',latb
                exit
              end if
            end if
          end if
          nobsout=nobsout+1
          obsout%elem(nobsout) = obsin(iof)%elem(n)
          obsout%lon(nobsout)  = obsin(iof)%lon(n)
          obsout%lat(nobsout)  = obsin(iof)%lat(n)
          obsout%lev(nobsout)  = obsin(iof)%lev(n)
          obsout%dat(nobsout)  = obsin(iof)%dat(n)
          obsout%err(nobsout)  = obsin(iof)%err(n)
          obsout%dmin(nobsout) = obsin(iof)%dmin(n)
          !kk = minloc(abs(obsin(iof)%lev(n)-plevfix(:)))
          !k=kk(1)
          !obsout%err(nobsout)  = obserr(k,uid_obs(obsin(iof)%elem(n)))
    ! (debug) check whether observation is 
    !  within prescribed horizontal domain or not
    if(     lonw.ne.0.0d0.and.lone.ne.0.0d0 &
       .and.lats.ne.0.0d0.and.latn.ne.0.0d0 &
       .and.(obsout%lon(nobsout).lt.lonw&
         .or.obsout%lon(nobsout).gt.lone&
         .or.obsout%lat(nobsout).lt.lats&
         .or.obsout%lat(nobsout).gt.latn)) then
        write(0,'(2a,4f8.2,2(a,f8.2))') &
        & 'warning: observation is outside of the prescribed horizontal domain ', &
        & 'lonw,lone,lats,latn=',lonw,lone,lats,latn,&
        & 'lon=',obsout%lon(nobsout),' lat=',obsout%lat(nobsout)
        obsout%qc(nobsout)=iqc_out_h
    else
          call phys2ijk(p_full,obsin(iof)%elem(n),&
             &  obsin(iof)%lon(n),obsin(iof)%lat(n),obsin(iof)%lev(n), &
             &  ri,rj,rk,obsout%qc(nobsout))
    end if
          if(obsout%qc(nobsout).eq.iqc_good) then
            if(.not.luseobs(uid_obs(obsin(iof)%elem(n)))) then
              obsout%qc(nobsout)=iqc_otype
            else
            nn=nn+1
            if(im.eq.0) then
              call trans_xtoy(obsin(iof)%elem(n),ri,rj,rk,&
               &  v3dg,v2dg,p_full,obsout%hxf(nobsout))
!!! debug
!              dep = obsin(iof)%dat(n) - obsout%hxf(nobsout)
!              print *, obsin(iof)%elem(n),obsin(iof)%lon(n),obsin(iof)%lat(n),&
!               &  obsin(iof)%lev(n)
!              print *,obsin(iof)%dat(n),obsout%hxf(nobsout), dep
!!! debug
            else
              call trans_xtoy(obsin(iof)%elem(n),ri,rj,rk,&
               &  v3dg,v2dg,p_full,obsout%hxe(im,nobsout))
!!! debug
!              dep = obsin(iof)%dat(n) - obsout%hxe(im,nobsout)
!              print *, obsin(iof)%elem(n),obsin(iof)%lon(n),obsin(iof)%lat(n),&
!               &  obsin(iof)%lev(n)
!              print *,obsin(iof)%dat(n),obsout%hxe(im,nobsout), dep
!!! debug
            end if
            end if !luseobs
            if(single_obs) then
              if(lonb.eq.undef.and.latb.eq.undef) then
                lonb=obsin(iof)%lon(n)
                latb=obsin(iof)%lat(n)
                if(latb.lt.lats.or.latb.gt.latn.or.&
                   lonb.lt.lonw.or.lonb.gt.lone) then
                  lonb=undef; latb=undef
                  obsout%qc(nobsout)=iqc_out_h
                end if
              end if
            end if
          end if !iqc_good
          if(single_obs.and.&
            obsout%qc(nobsout)/=iqc_good) then
            nobsout=nobsout-1
            cycle
          end if
          ! qc
          if (im.eq.0) then
            tmpqc(nobsout) = obsout%qc(nobsout)
          else
            tmpqc(nobsout) = max(tmpqc(nobsout),obsout%qc(nobsout))
          end if
        end do ! n=1,obsin(iof)%nobs
      end do ! iof=1,obsin_num
      obsout%nobs = nobsout
!      if(single_obs) then
      if(debug_obs) then
        print '(10a10)','number','elem','lon','lat','lev','dat','err','dmin','hxf','qc'
        nn=0
        do n=1,obsout%nobs
          if(obsout%qc(n)/=iqc_good) cycle
          nn=nn+1
          if(nobsmax.gt.0.and.nn.gt.nobsmax) exit
          if(im.eq.0) then
            hxf=obsout%hxf(n)
          else
            hxf=obsout%hxe(im,n)
          end if
          print '(i10,a10,2f10.2,f10.1,4es10.2,i10)', &
           &  n,obelmlist(uid_obs(obsout%elem(n))),&
           &  obsout%lon(n),obsout%lat(n),obsout%lev(n),obsout%dat(n),&
           &  obsout%err(n),obsout%dmin(n), &
           &  hxf,obsout%qc(n)
        end do
      end if
      if(obs_out) then
        call file_member_replace(im,obsout_basename,outf)
        call write_obsout(outf,obsout,im)
      end if
    end do ! while (im <= member)
    ! unifying qc
    obsout%qc(:) = tmpqc(:)
    return
  end subroutine obsope_serial
!
! (parallel) observation operator applied to updated model variables in DA
!
  subroutine obsope_update(obs,mem,v3d,v2d,p_full)
    implicit none
    type(obstype2), intent(inout):: obs
    integer,       intent(in) :: mem
    real(kind=dp), intent(in) :: v3d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d)
    real(kind=dp), intent(in) :: v2d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,     nv2d)
    real(kind=dp), intent(in) :: p_full(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev)
    integer :: nobsin,nobsout
    real(kind=dp) :: ri,rj,rk
    integer :: im, n, nn
!!! debug
!    integer :: nobsmax
!    real(kind=dp) :: dep
!    
!    nobsmax=100 !debug
!!! debug

    if(obs%nobs==0) then
      write(6,'(a)') 'no observation to be assimilated'
      return
    end if
   
    nobsout=0
    nn=0
    write(6,'(a,2f10.2)') 'p_full ',minval(p_full(1:ni1,1:nj1,:)),maxval(p_full(1:ni1,1:nj1,:))
    if(mem.eq.0) then
      obs%hxf(:) = 0.0d0
    else
      obs%hxe(mem,:) = 0.0d0
    end if
    do n=1,obs%nobs
      if(obs%img(n)/=myimage) cycle
      nobsout=nobsout+1
      call phys2ijk(p_full,obs%elem(n),&
         &  obs%lon(n),obs%lat(n),obs%lev(n), &
         &  ri,rj,rk,obs%qc(n),.true.)
      if(obs%qc(n).eq.iqc_good) then
        nn=nn+1
        if(mem.eq.0) then
          call trans_xtoy(obs%elem(n),ri,rj,rk,&
           &  v3d,v2d,p_full,obs%hxf(n))
!!! debug
!          dep = obs%dat(n) - obs%hxf(n)
!          print *, obs%elem(n),obs%lon(n),obs%lat(n),&
!           &  obs%lev(n)
!          print *,obs%dat(n),obs%hxf(n), dep
!!! debug
        else
          call trans_xtoy(obs%elem(n),ri,rj,rk,&
           &  v3d,v2d,p_full,obs%hxe(mem,n))
!!! debug
!          dep = obs%dat(n) - obs%hxe(im,n)
!          print *, obs%elem(n),obs%lon(n),obs%lat(n),&
!           &  obs%lev(n)
!              print *,obs%dat(n),obs%hxe(im,n), dep
!!! debug
        end if
      end if
    end do
    return
  end subroutine obsope_update
!
! (serial) create observations from reference state
!
  subroutine obsmake_cal(obsin,v3d,v2d,datatype,odate)
    use random_module, only: random_init, random_normal
    implicit none
    type(obstype), intent(inout) :: obsin(obsin_num)
    real(kind=dp), intent(in) :: v3d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d)
    real(kind=dp), intent(in) :: v2d(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,     nv2d)
    character(len=*), intent(in) :: datatype(:)
    character(len=*), intent(in) :: odate
    character(len=100) :: ofile
    integer :: nobsin,nobsout
    integer :: iof
    real(kind=dp), allocatable :: p_full(:,:,:)
    real(kind=dp) :: ri,rj,rk
    integer :: im, n, img
    integer :: tmpqc
    real(kind=dp), allocatable :: rand(:)
    real(kind=dp) :: tmplev, tmperr
    real(kind=dp), allocatable :: wk(:,:)[:]
    integer :: is,ie,js,je
    integer,dimension(1) :: kk
    integer :: k
!!! for single point observation
    real(kind=dp) :: lonb,latb
    real(kind=dp) :: hxf,dep

    call random_init

    nobsin=0
    do iof=1,obsin_num
      nobsin=nobsin+obsin(iof)%nobs
    end do
    if(nobsin.le.0) then
      write(6,'(a)') 'no observation to be assimilated'
      return
    end if
    print *, nobsin, ' observation will be created'
   
    allocate( p_full(1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev) )
    is=1
    ie=ni1+ighost
    if(nidom(myimage)==nisep) ie=ni1
    js=1
    je=nj1+jghost
    if(njdom(myimage)==njsep) je=nj1
    print *, myimage, myrlon(is), myrlon(ie), myrlat(js), myrlat(je)
    if(nonhyd.eq.1) then !non-hydrostatic
      p_full = v3d(:,:,:,iv3d_pn)
    else
      call calc_pfull(ni1max+2*ighost,nj1max+2*jghost,nlev,sig,v2d(:,:,iv2d_ps),p_full)
    end if
    write(6,'(a,2f10.2)') 'p_full=',maxval(p_full(1:ni1,1:nj1,:)),minval(p_full(1:ni1,1:nj1,:))

    do iof=1,obsin_num
      nobsin=obsin(iof)%nobs
      print *, nobsin
      allocate( rand(nobsin) )
      call random_normal(rand)
      allocate( wk(nobsin,3)[*] ) !level,dat,err
      wk=0.0d0
      do n=1,obsin(iof)%nobs
        ! horizontal domain check
        if(    obsin(iof)%lon(n).lt.rlon(1)&
           .or.obsin(iof)%lon(n).ge.rlon(nlon)&
           .or.obsin(iof)%lat(n).lt.rlat(1)&
           .or.obsin(iof)%lat(n).ge.rlat(nlat)) then
          if(myimage.eq.print_img) &
          write(0,'(a,2(a,f8.2))') &
          & 'warning: observation is outside of the horizontal domain ', &
          & 'lon=',obsin(iof)%lon(n),' lat=',obsin(iof)%lat(n)
          tmpqc=iqc_out_h
          cycle
        end if
        ! search which image contains observation
        if(    obsin(iof)%lon(n).lt.myrlon(is)&
           .or.obsin(iof)%lon(n).ge.myrlon(ie)&
           .or.obsin(iof)%lat(n).lt.myrlat(js)&
           .or.obsin(iof)%lat(n).ge.myrlat(je)) then
        else !myimage
!debug          print '(2I5,2F10.2)',n, obsin(iof)%elem(n),&
!debug                  obsin(iof)%lon(n), obsin(iof)%lat(n)
          tmplev  = 500.0d2 !Pa, dummy
          call phys2ijk(p_full,obsin(iof)%elem(n),&
           &  obsin(iof)%lon(n),obsin(iof)%lat(n),tmplev, &
           &  ri,rj,rk,tmpqc,.true.)
          ! determine obs level
          if(obsin(iof)%elem(n).lt.10000) then !upper
            rk=obsin(iof)%lev(n)
            call itpl_2d(p_full(:,:,nint(rk)),ri,rj,wk(n,1))
          !  kk = minloc(abs(wk(n,1)-plevfix))
          !  k = kk(1)
          else !synop
            call itpl_2d(v2d(:,:,iv2d_gz),ri,rj,wk(n,1))
            rk=wk(n,1)
          !  k=1
          end if
          !tmperr  = obserr(k,uid_obs(obsin(iof)%elem(n)))
          tmperr  = obsin(iof)%err(n)
          wk(n,3) = tmperr
          print '(3I6,3F10.2,ES10.2)', &
                  myimage, n, obsin(iof)%elem(n), ri,rj,rk,tmperr
!debug          print '(2I5,2F10.2)',n, obsin(iof)%elem(n), obsin(iof)%lev(n),wk(n,1)
          if(tmpqc.ne.iqc_good) then
            wk(n,2)=undef
          else
            if(.not.luseobs(uid_obs(obsin(iof)%elem(n)))) then
              wk(n,2)=undef
            else
              call trans_xtoy(obsin(iof)%elem(n),ri,rj,rk,&
                 &  v3d,v2d,p_full,wk(n,2))
              ! add error
              wk(n,2) = wk(n,2) + tmperr * rand(n)
              if(obsin(iof)%elem(n)==id_rh_obs) then
                if(wk(n,2)<0.0) wk(n,2)=1.0e-6
                if(wk(n,2)>1.0) wk(n,2)=1.0
              else if(obsin(iof)%elem(n)==id_q_obs) then
                if(wk(n,2)<0.0) wk(n,2)=1.0e-9
              end if
            end if !luseobs
          end if !iqc_good
!debug          print '(2I5,F10.2)',n, obsin(iof)%elem(n), wk(n,2)
        end if !myimage
      end do ! n=1,obsin(iof)%nobs
      sync all
      !! gather wk, monitor and write output
      if(myimage.eq.print_img) then
        do img=1,nimages
          if(myimage.eq.img) cycle
          do n=1,obsin(iof)%nobs
            wk(n,:)[myimage]=wk(n,:)[myimage]+wk(n,:)[img]
          end do
        end do
!        do img=1,nimages
!          wk(:,:)[img] = wk(:,:)[myimage]
!        end do
        do n=1,obsin(iof)%nobs
          obsin(iof)%lev(n)=wk(n,1)
          obsin(iof)%dat(n)=wk(n,2)
          obsin(iof)%err(n)=wk(n,3)
        end do
        call monit_obsin(obsin(iof)%nobs,obsin(iof)%elem,obsin(iof)%dat)
        ofile=trim(datatype(iof))//'.siml.'//odate
        print *, ofile
        call write_obs(ofile,obsin(iof))
      end if
      sync all
      deallocate( rand, wk )
    end do ! iof=1,obsin_num
    deallocate( p_full )
    return
  end subroutine obsmake_cal
!
!
! coordinate conversion
!
  subroutine phys2ijk(p_full,elm,rlon1,rlat1,rlev1,ri,rj,rk,qc,local)
    implicit none
    real(kind=dp),intent(in) :: p_full(:,:,:) !(nlon,nlat,nlev) or (1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev)
    integer,intent(in) :: elm
    real(kind=dp),intent(in) :: rlon1,rlat1
    real(kind=dp),intent(in) :: rlev1 !pressure level
    real(kind=dp),intent(out):: ri,rj,rk
    integer,      intent(out):: qc
    logical,optional,intent(in) :: local
    real(kind=dp) :: ai,aj,ak
    real(kind=dp),allocatable :: lnps(:,:) !(nlon,nlat) or (1-ighost:ni1max+ighost,1-jghost:nj1max+jghost)
    real(kind=dp) :: plev(nlev)
    real(kind=dp) :: ptmp
    logical :: local_
    integer :: igrdtmp,jgrdtmp
    integer :: i,j,k
    integer :: is,ie,js,je

    local_=.false.
    if(present(local)) local_=local

    qc=iqc_good
    
    if(local_) then
      allocate( lnps(1:ni1max+2*ighost,1:nj1max+2*jghost) )
      ! rlon1 -> ri
      is=1
      ie=ni1+ighost
      if(nidom(myimage)==nisep) ie=ni1
      do i=is,ie
        if(rlon1.lt.myrlon(i)) exit
      end do
      ai=(rlon1-myrlon(i-1))/(myrlon(i)-myrlon(i-1))
      ri=real(i-1,kind=dp)+ai
      ! rlat1 -> rj
      js=1
      je=nj1+jghost
      if(njdom(myimage)==njsep) je=nj1
      do j=js,je
        if(rlat1.lt.myrlat(j)) exit
      end do
      aj=(rlat1-myrlat(j-1))/(myrlat(j)-myrlat(j-1))
      rj=real(j-1,kind=dp)+aj
      ! check whether observation is within horizontal domain or not
      if(ri.lt.real(is-1).or.ri.gt.real(ie).or.rj.lt.real(js-1).or.rj.gt.real(je)) then
        write(0,'(a,4(a,f8.2))') &
        & 'warning: observation is outside of the horizontal domain ', &
        & 'lon=',rlon1,' lat=',rlat1,' ri=',ri,' rj=',rj
        qc=iqc_out_h
        return
      end if
!!DEBUG      write(6,'(6(a,f8.2))') &
!!DEBUG        & 'lon=',rlon1,' lat=',rlat1,' ri=',ri,' rj=',rj, ' rlon=',myrlon(i),' rlat=',myrlat(j)
      ri=ri+real(ighost,kind=dp)
      rj=rj+real(jghost,kind=dp)
    else
      allocate( lnps(nlon,nlat) )
      ! rlon1 -> ri
      do i=1,nlon
        if(rlon1.lt.rlon(i)) exit
      end do
      if(i.gt.nlon) then
        ri=real(nlon+1,kind=dp)
      else
        ai=(rlon1-rlon(i-1))/(rlon(i)-rlon(i-1))
        ri=real(i-1,kind=dp)+ai 
      end if
      ! rlat1 -> rj
      do j=1,nlat
        if(rlat1.lt.rlat(j)) exit
      end do
      if(j.gt.nlat) then
        rj=real(nlat+1,kind=dp)
      else
        aj=(rlat1-rlat(j-1))/(rlat(j)-rlat(j-1))
        rj=real(j-1,kind=dp)+aj
      end if
      ! check whether observation is within horizontal domain or not
      if(ri.lt.1.or.ri.gt.nlon.or.rj.lt.1.or.rj.gt.nlat) then
        write(0,'(a,4(a,f8.2))') &
        & 'warning: observation is outside of the horizontal domain ', &
        & 'lon=',rlon1,' lat=',rlat1,' ri=',ri,' rj=',rj
        qc=iqc_out_h
        return
      end if
!!DEBUG      write(6,'(6(a,f8.2))') &
!!DEBUG        & 'lon=',rlon1,' lat=',rlat1,' ri=',ri,' rj=',rj, ' rlon=',rlon(i),' rlat=',rlat(j)
    end if
    ! rlev1 -> rk
    if(elm.gt.9999) then !surface observation : rlev = height [m]
      rk=rlev1
    else !upper observation : rlev = pressure [Pa]
      !! for hydrometeor variables
      if(elm==id_q_obs.or.elm==id_rh_obs.or.elm==id_td_obs) then
        if(rlev1.lt.q_update_top) then
          write(0,'(2a,f8.1,a,i5)') &
          & 'warning: observation is too high for hydrometeors, ',&
          & 'lev=',rlev1,' elem=',elm
          qc=iqc_out_vhi
          return
        end if
      end if
      !! fixed level
      if(fixed_level) then
        do k=1,25
          if(rlev1.eq.plevfix(k)) exit
        end do
        if(k.gt.25) then
          write(0,'(2a,f8.1,a,i5)') &
          & 'warning: observation is not on the mandatory levels, ',&
          & 'lev=',rlev1,' elem=',elm
          qc=iqc_otype
          return
        end if
      end if
      ! horizontal interpolation
      do k=1,nlev
        i=ceiling(ri)
        j=ceiling(rj)
!!DEBUG        print *, i,j,p_full(i-1,j-1,k),p_full(i,j-1,k),p_full(i-1,j,k),p_full(i,j,k)
        lnps=0.0_dp
        lnps(i-1:i,j-1:j)=log(p_full(i-1:i,j-1:j,k))
!!DEBUG        print *, i,j,lnps(i-1,j-1),lnps(i,j-1),lnps(i-1,j),lnps(i,j)
        call itpl_2d(lnps,ri,rj,plev(k))
!        print *, exp(plev(k))
      end do
      ! find rk
      rk=log(rlev1)
!!DEBUG      if(local_) write(6,'(a,2f8.5,a,f8.5)') 'plev ',plev(1),plev(nlev),' rk ',rk 
      if(rk.gt.plev(1)) then
!        call itpl_2d(p_full(:,:,1),ri,rj,ptmp)
        ptmp=exp(plev(1))
        write(0,'(a,f8.1,a,f8.1,a,i5)') &
        & 'warning: observation is too low, pbtm=',ptmp,', lev=',rlev1,' elem=',elm
        qc=iqc_out_vlo
        return
      end if
      if(rk.lt.plev(nlev)) then
!        call itpl_2d(p_full(:,:,nlev),ri,rj,ptmp)
        ptmp=exp(plev(nlev))
        write(0,'(a,f8.1,a,f8.1,a,i5)') &
        & 'warning: observation is too high, ptop=',ptmp,', lev=',rlev1,' elem=',elm
        qc=iqc_out_vhi
        return
      end if
      do k=1,nlev
        if(plev(k).lt.rk) exit
      end do
      ak=(plev(k-1)-rk)/(plev(k-1)-plev(k))
      rk=real(k-1,kind=dp)+ak
    end if
    
!!DEBUG    if(local_) write(6,'(3(a,f8.1))') 'ri=',ri,' rj=',rj,' rk=',rk
    deallocate( lnps )
    return
  end subroutine phys2ijk
!
! model variables => observation
!
  subroutine trans_xtoy(elm,ri,rj,rk,v3d,v2d,p_full,yobs,corr)
    use phconst_module, only: grav,rd,lapse,fvirt
    implicit none
    integer, intent(in) :: elm
    real(kind=dp), intent(in) :: ri,rj,rk
    real(kind=dp), intent(in) :: v3d(:,:,:,:) !(nlon,nlat,nlev,nv3d) or (1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev,nv3d)
    real(kind=dp), intent(in) :: v2d(:,:,:)   !(nlon,nlat,nv2d) or (1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nv2d)
    real(kind=dp), intent(in) :: p_full(:,:,:) !(nlon,nlat,nlev) or (1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev)
    real(kind=dp), intent(out):: yobs
    real(kind=dp), optional, intent(out) :: corr !bias correction
    
    real(kind=dp) :: t,q,p,gz
    real(kind=dp) :: fact, dtmp, z1
    real(kind=dp) :: u,v
    
    integer :: i,j,k
    integer :: is,ie,js,je,ks,ke

    if(present(corr)) then
      corr=0.0d0
    end if

    ie = ceiling( ri )
    is = ie - 1
    je = ceiling( rj )
    js = je - 1
    ke = ceiling( rk )
    ks = ke - 1

    select case( elm )
    case(id_u_obs) !U-wind
      call itpl_3d(v3d(:,:,:,iv3d_u),ri,rj,rk,yobs)
    case(id_v_obs) !V-wind
      call itpl_3d(v3d(:,:,:,iv3d_v),ri,rj,rk,yobs)
    case(id_t_obs) !Temperature
      call itpl_3d(v3d(:,:,:,iv3d_t),ri,rj,rk,yobs)
    case(id_q_obs) !Specific humidity
      call itpl_3d(v3d(:,:,:,iv3d_q),ri,rj,rk,yobs)
    case(id_td_obs) !Dewpoint Temperature
      call itpl_3d(v3d(:,:,:,iv3d_q),ri,rj,rk,q)
      call itpl_3d(p_full(:,:,:),ri,rj,rk,p)
      call calc_td(q,p,yobs) 
    case(id_rh_obs) !Relative humidity
      call itpl_3d(v3d(:,:,:,iv3d_t),ri,rj,rk,t)
      call itpl_3d(v3d(:,:,:,iv3d_q),ri,rj,rk,q)
      call itpl_3d(p_full(:,:,:),ri,rj,rk,p)
      call calc_rh(t,q,p,yobs) 
    case(id_ws_obs) !Wind Speed
      call itpl_3d(v3d(:,:,:,iv3d_u),ri,rj,rk,u)
      call itpl_3d(v3d(:,:,:,iv3d_v),ri,rj,rk,v)
      yobs = sqrt(u**2 + v**2)
    case(id_wd_obs) !Wind Direction
      call itpl_3d(v3d(:,:,:,iv3d_u),ri,rj,rk,u)
      call itpl_3d(v3d(:,:,:,iv3d_v),ri,rj,rk,v)
      call calc_wd(u,v,yobs)
    case(id_ps_obs) !Surface pressure
      call itpl_2d(v2d(:,:,iv2d_ps),ri,rj,yobs)
      call itpl_2d(v2d(:,:,iv2d_gz),ri,rj,gz) !surface elevation
      if(debug_obs) print *, 'zmodel ', gz,' psmodel ', yobs !debug
      call itpl_2d(v3d(:,:,1,iv3d_t),ri,rj,t)
      call itpl_2d(v3d(:,:,1,iv3d_q),ri,rj,q)
      !call itpl_2d(v2d(:,:,nv2d_sig+nv2d_sfc+iv2d_t2m),ri,rj,t)
      !call itpl_2d(v2d(:,:,nv2d_sig+nv2d_sfc+iv2d_q2m),ri,rj,q)
      if(present(corr)) corr=yobs
      call prsadj(yobs,rk-gz,t,q)
      if(present(corr)) then
        corr=yobs-corr
      end if
      if(debug_obs) then
        print *, 'zmodel ', gz,' psadj   ', yobs !debug
        fact=(1.0-(0.995)**(rd*lapse/grav))/lapse
        call itpl_2d(v2d(:,:,nv2d_sig+iv2d_tsfc),ri,rj,t)
        !call itpl_2d(v3d(:,:,1,iv3d_t),ri,rj,dtmp)
        !dtmp = dtmp - t !t1 - ts
        t = t*(1.0+fvirt*q) !tv
        z1 = gz + t * fact
        print *, 'z1     ',z1 !debug
        write(stnout,'(i5,4f9.3)') elm,ri,rj,rk,gz
      end if
    case(id_t2m_obs) !Surface temparature
      call itpl_2d(v2d(:,:,nv2d_sig+nv2d_sfc+iv2d_t2m),ri,rj,yobs)
      call itpl_2d(v2d(:,:,iv2d_gz),ri,rj,gz) !surface elevation
      if(debug_obs) print *, 'zmodel ', gz,' tmodel ', yobs !debug
      if(present(corr)) corr=yobs
      yobs = yobs - lapse*(rk-gz)
      if(debug_obs) then
        print *, 'zmodel ', gz,' tadj ', yobs !debug
        write(stnout,'(i5,4f9.3)') elm,ri,rj,rk,gz
      end if
      if(present(corr)) then
        corr=yobs-corr
      end if
    end select
    return

  end subroutine trans_xtoy
!
! 2-dimensional interpolation
!
  subroutine itpl_2d(v2d,ri,rj,vout)
    implicit none
    real(kind=dp), intent(in) :: v2d(:,:) !(nlon,nlat) or (1-ighost:ni1max+ighost,1-jghost:nj1max+jghost)
    real(kind=dp), intent(in) :: ri, rj
    real(kind=dp), intent(out):: vout
    real(kind=dp) :: ai, aj
    integer :: i,j

    i = ceiling(ri)
    ai = ri - real(i-1,kind=dp)
    j = ceiling(rj)
    aj = rj - real(j-1,kind=dp)
!    print *, i,j,v2d(i-1,j-1),v2d(i,j-1),v2d(i-1,j),v2d(i,j)

    vout = v2d(i-1,j-1) * (1.0 - ai) * (1.0 - aj) &
       & + v2d(i  ,j-1) *        ai  * (1.0 - aj) &
       & + v2d(i-1,j  ) * (1.0 - ai) *        aj  &
       & + v2d(i  ,j  ) *        ai  *        aj
    return
  end subroutine itpl_2d
!
! 3-dimensional interpolation
!
  subroutine itpl_3d(v3d,ri,rj,rk,vout)
    implicit none
    real(kind=dp), intent(in) :: v3d(:,:,:) !(nlon,nlat,nlev) or (1-ighost:ni1max+ighost,1-jghost:nj1max+jghost,nlev)
    real(kind=dp), intent(in) :: ri,rj,rk
    real(kind=dp), intent(out):: vout
    real(kind=dp) :: ai,aj,ak
    integer :: i,j,k

    i = ceiling(ri)
    ai = ri - real(i-1,kind=dp)
    j = ceiling(rj)
    aj = rj - real(j-1,kind=dp)
    k = ceiling(rk)
    ak = rk - real(k-1,kind=dp)
    
    if(k.eq.1) then
    vout = v3d(i-1,j-1,k) * (1.0 - ai) * (1.0 - aj) &
       & + v3d(i  ,j-1,k) *        ai  * (1.0 - aj) &
       & + v3d(i-1,j  ,k) * (1.0 - ai) *        aj  &
       & + v3d(i  ,j  ,k) *        ai  *        aj  
    else
    vout = v3d(i-1,j-1,k-1) * (1.0 - ai) * (1.0 - aj) * (1.0 - ak) &
       & + v3d(i  ,j-1,k-1) *        ai  * (1.0 - aj) * (1.0 - ak) &
       & + v3d(i-1,j  ,k-1) * (1.0 - ai) *        aj  * (1.0 - ak) &
       & + v3d(i  ,j  ,k-1) *        ai  *        aj  * (1.0 - ak) &
       & + v3d(i-1,j-1,k  ) * (1.0 - ai) * (1.0 - aj) *        ak  &
       & + v3d(i  ,j-1,k  ) *        ai  * (1.0 - aj) *        ak  &
       & + v3d(i-1,j  ,k  ) * (1.0 - ai) *        aj  *        ak  &
       & + v3d(i  ,j  ,k  ) *        ai  *        aj  *        ak
    end if
    return
  end subroutine itpl_3d
!
! Monitor departure
!
  subroutine monit_dep(nobsall,elem,dep,qc,nobs,bias,rmse,nqc)
    use phconst_module, only: pi, rad2deg
    implicit none
    integer, intent(in) :: nobsall
    integer, intent(in) :: elem(nobsall)
    real(kind=dp), intent(in) :: dep(nobsall)
    integer, intent(in) :: qc(nobsall)
    integer, intent(out) :: nobs(nobstype)
    real(kind=dp), intent(out) :: bias(nobstype)
    real(kind=dp), intent(out) :: rmse(nobstype)
    integer, intent(out) :: nqc(nobstype,nqctype)
    real(kind=dp) :: dep1
    integer :: n,i

    nobs(:) = 0
    bias(:) = 0.0_dp
    rmse(:) = 0.0_dp
    nqc(:,:)= 0
    do n=1,nobsall
      i = uid_obs(elem(n))
      nobs(i)=nobs(i)+1
      select case(qc(n))  
      case(iqc_gross_err)
        nqc(i,2)=nqc(i,2)+1
      case(iqc_out_vhi)
        nqc(i,3)=nqc(i,3)+1
      case(iqc_out_vlo)
        nqc(i,4)=nqc(i,4)+1
      case(iqc_out_h)
        nqc(i,5)=nqc(i,5)+1
      case(iqc_otype)
        nqc(i,6)=nqc(i,6)+1
      case(iqc_good)
        nqc(i,1)=nqc(i,1)+1
        dep1 = dep(n)
        if(elem(n)==id_wd_obs) then !wind direction
          if(abs(dep1).gt.180.0d0) then
            dep1=dep1-sign(360.0d0,dep1)
          end if
        end if
        bias(i)=bias(i)+dep1
        rmse(i)=rmse(i)+dep1**2
      end select
    end do
    do i=1,nobstype
      if(nqc(i,1).eq.0) then
        bias(i)=undef
        rmse(i)=undef
      else
        bias(i)=bias(i)/real(nqc(i,1),kind=dp)
        rmse(i)=sqrt(rmse(i)/real(nqc(i,1),kind=dp))
      end if
    end do
    return
  end subroutine monit_dep
!
! print monitor
!
  subroutine monit_print(nobs,bias,rmse,nqc)
    implicit none
    integer, intent(in) :: nobs(nobstype)
    real(kind=dp), intent(in) :: bias(nobstype)
    real(kind=dp), intent(in) :: rmse(nobstype)
    integer, intent(in) :: nqc(nobstype,nqctype)

    character(len=12) :: var_show(nobstype)
    character(len=12) :: nobs_show(nobstype)
    character(len=12) :: bias_show(nobstype)
    character(len=12) :: rmse_show(nobstype)
    character(len=12) :: nqc_show(nobstype,nqctype)

    integer :: i,iqc,n
    character(len=4) :: nstr
    character(len=12) :: tmpstr(nobstype), tmpstr2(nobstype)

    n=0
    do i=1,nobstype
      n=n+1
      write(var_show(n),'(a12)') obelmlist(i)
      write(nobs_show(n),'(i12)') nobs(i)
      if(nqc(i,1).gt.0) then
        write(bias_show(n),'(es12.3)') bias(i)
        write(rmse_show(n),'(es12.3)') rmse(i)
      else
        write(bias_show(n),'(a12)') 'N/A'
        write(rmse_show(n),'(a12)') 'N/A'
      end if
      do iqc=1,nqctype
        write(nqc_show(n,iqc),'(i12)') nqc(n,iqc)
      end do
    end do
    write(nstr,'(i4)') n
    tmpstr(1:n) = '============'
    tmpstr2(1:n) = '------------'

    write(6,'(a,'//trim(nstr)//'a)') '======',tmpstr(1:n)
    write(6,'(6x,'//trim(nstr)//'a)') var_show(1:n)
    write(6,'(a,'//trim(nstr)//'a)') '------',tmpstr2(1:n)
    write(6,'(a,'//trim(nstr)//'a)') 'BIAS  ',bias_show(1:n)
    write(6,'(a,'//trim(nstr)//'a)') 'RMSE  ',rmse_show(1:n)
    write(6,'(a,'//trim(nstr)//'a)') 'NUMBER',nobs_show(1:n)
    do iqc=1,nqctype
    write(6,'(a,'//trim(nstr)//'a)') qctype(iqc),nqc_show(1:n,iqc)
    end do
    write(6,'(a,'//trim(nstr)//'a)') '======',tmpstr(1:n)
  end subroutine monit_print
end module obsope_module
