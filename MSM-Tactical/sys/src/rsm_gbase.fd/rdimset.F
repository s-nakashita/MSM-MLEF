      subroutine rdimset(iwav1,jwav1,levr,igrd1,jgrd1,                   & 
     &                  npes,ncol,nrow,                                  &
     &                  levstr,levlen,levmax,                            &
     &                  lonstr,lonlen,lonmax,                            &
     &                  latstr,latlen,latmax,                            &
     &                  lwvstr,lwvlen,lwvmax,                            &
     &                  lntstr,lntlen,lntmax,                            &
     &                  lnpstr,lnplen,lnpmax)
!$$$  subprogram documentation block
! subprogram: 	rdimset
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract: preset all starting point and length for 
!           all pe for global spectral model.
!
! program history log:
!    99-06-27  henry juang 	finish entire test for gsm
!
! usage:	call rdimset(iwav1,levs,igrd1,jgrd1,
!    *                  npes,ncol,nrow,
!    *                  levstr,levlen,levmax,
!    *                  lonstr,lonlen,lonmax,
!    *                  latstr,latlen,latmax,
!    *                  lwvstr,lwvlen,lwvmax,
!    *                  lntstr,lntlen,lntmax,
!    *                  lnpstr,lnplen,lnpmax)
!
!    input argument lists:
!	iwav1	- integer spectral wavenumber
!	levr	- integer vertical layer number
!	igrd1	- integer gaussian grid for longitude
!	jgrd1	- integer gaussian grid for latitude
!	npes	- integer number of pe used: npes=ncol*nrow
!	ncol	- integer number of column
!	nrow	- integer number of nrow
!
!    output argument list:
!	lev*	- integer (npes) related to layers for each pe
!	lon*	- integer (npes) related to longitude for each pe
!	lat*	- integer (npes) related to latitude for each pe
!	lnt*	- integer (npes) related to npes cut of spectral
!	lnp*	- integer (npes) related to nrow cut of spectral
!	lwv*	- integer (npes) related to group of spectral in l
!	*str	- integer (npes) related to each starting point
!	*len	- integer (npes) related to each length
!	*max	- integer related to maximal length of the kind
! 
! subprograms called:
!   equdiv	- to compute about equal number of subgroup by division
!
! attributes:
!    language: fortran 90
!$$$
!
!      implicit none
      integer iwav1,levr,igrd1,jgrd1,npes,ncol,nrow                      &  
     &       ,nr,nc,nn,jcaprm,nremain                                    &
     &       ,lnp,lx,lh,n                                                &
     &       ,levmax,lonmax,latmax,lwvmax,lntmax,lnpmax                  &
     &       ,levpnt,lonpnt,latpnt,lwvpnt,lntpnt,lnppnt                  &
     &       ,levstr,lonstr,latstr,lwvstr,lntstr,lnpstr                  &
     &       ,levlen,lonlen,latlen,lwvlen,lntlen,lnplen
      dimension                                                          &
     &        levstr(0:npes-1),levlen(0:npes-1)                          &
     &       ,lonstr(0:npes-1),lonlen(0:npes-1)                          &
     &       ,latstr(0:npes-1),latlen(0:npes-1)                          &
     &       ,lwvstr(0:npes-1),lwvlen(0:npes-1)                          &
     &       ,lntstr(0:npes-1),lntlen(0:npes-1)                          &
     &       ,lnpstr(0:npes-1),lnplen(0:npes-1)
      integer, allocatable ::                                            &
     &          levdis(:),londis(:),lntdis(:)
      integer, allocatable ::                                            &
     &          lwvdis(:),latdis(:),lnpdis(:)
!
      allocate (levdis(ncol))
      allocate (londis(ncol))
      allocate (lntdis(ncol))
      allocate (lwvdis(nrow))
      allocate (lnpdis(nrow))
      allocate (latdis(nrow))
!
      call equdiv(levr ,ncol,levdis)
      call equdiv(iwav1 ,nrow,lwvdis)
!
      do nr=1,nrow	
        lnpdis(nr)=lwvdis(nr)*jwav1
      enddo                  
!
      call equdiv(igrd1 ,ncol,londis)
      call equdiv(jgrd1 ,nrow,latdis)
!
      levmax=0
      lonmax=0
      latmax=0
      lwvmax=0
      lntmax=0
      lnpmax=0
!
      latpnt=1
      lwvpnt=1
      lntpnt=1
      lnppnt=1
      n=0
!
      do nr=1,nrow
!
        levpnt=1
        lonpnt=1
        call equdiv(lnpdis(nr),ncol,lntdis)
!
         do nc=1,ncol
!
          levstr(n)=levpnt
          levlen(n)=levdis(nc)
          levpnt=levpnt+levdis(nc)
          levmax=max(levmax,levlen(n))
!
          lonstr(n)=lonpnt
          lonlen(n)=londis(nc)
          lonpnt=lonpnt+londis(nc)
          lonmax=max(lonmax,lonlen(n))
!
          lntstr(n)=lntpnt
          lntlen(n)=lntdis(nc)
          lntpnt=lntpnt+lntdis(nc)
          lntmax=max(lntmax,lntlen(n))
!
          latstr(n)=latpnt
          latlen(n)=latdis(nr)
          latmax=max(latmax,latlen(n))
!
          lwvstr(n)=lwvpnt
          lwvlen(n)=lwvdis(nr)
          lwvmax=max(lwvmax,lwvlen(n))
!
          lnpstr(n)=lnppnt
          lnplen(n)=lnpdis(nr)
          lnpmax=max(lnpmax,lnplen(n))
!
          n=n+1
!
        enddo
!
        if( nr.lt.nrow ) then
          latpnt=latpnt+latdis(nr)
          lwvpnt=lwvpnt+lwvdis(nr)
          lnppnt=lnppnt+lnpdis(nr)
        endif
!
      enddo
!
      do n=0,npes-1
        lwvstr(n)=lwvstr(n)-1
        lnpstr(n)=lnpstr(n)-1
        lntstr(n)=lntstr(n)-1
      enddo
!
      levmax=levmax
      lonmax=lonmax
      latmax=latmax
      lwvmax=lwvmax	! consider as iwv1p
      lntmax=lntmax
      lnpmax=lnpmax
!
      deallocate (levdis)
      deallocate (londis)
      deallocate (lntdis)
      deallocate (lwvdis)
      deallocate (lnpdis)
      deallocate (latdis)
!
      return
      end

