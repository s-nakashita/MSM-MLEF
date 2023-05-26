      subroutine rwrtbgt(fhour,idate)
#include <paramodel.h>
#include <rscombgt.h>
#ifdef A
      parameter(nprnt=3,npdim=nprnt*nprnt,nskip=igrd1+5)
      character*2 nvar(5)
      dimension idate(4),ivar(5),lvar(5),
     &          prtbgt(5,npdim,levr,20)
       save ivar,nvar,lvar
      data ivar/5*0/
      data nvar/'tt','qq','uu','vv','ps'/
      data lvar/4*levr,1/
#endif
#ifdef T
      ivar(1)=nt+1
#endif
#ifdef Q
      ivar(2)=nq+1
#endif
#ifdef U
      ivar(3)=nu+1
#endif
#ifdef V
      ivar(4)=nv+1
#endif
#ifdef P
      ivar(5)=np+1
#endif
!
#ifdef A
      print *,' enter rwrtbgt fhour=',fhour,' idate=',idate
     &       ,' ndst=',ndst
#endif
!  compute mean
!
#ifdef A
      write(nbgt,'(a16)') 'rsm model budget'
      write(nbgt,'(f8.0,9i8)') fhour,idate,(ivar(n),n=1,5)
      rewind junit1
      do numvar=1,5
        if( ivar(numvar).ne.0 ) then
          print *,' output budget for ',nvar(numvar)
          do n=1,ivar(numvar)
            do k=1,lvar(numvar)
              read(junit1) (tmpbgt(i,k),i=1,lngrd)
            enddo
            do k=1,lvar(numvar)
              do i=1,lngrd
                tmpbgt(i,k)=tmpbgt(i,k)/float(ndst)
              enddo
            enddo
            do k=1,lvar(numvar)
              write(nbgt,'(1x,6e13.6)') (tmpbgt(i,k),i=1,lngrd)
              do i=1,npdim
                ii=1+(i-1)*nskip
                prtbgt(numvar,i,k,n)=tmpbgt(ii,k)
              enddo
            enddo
          enddo
        endif
#endif
!
! ------ print out -----
! t
#ifdef T
        if( numvar.eq.1 .and. ivar(numvar).ne.0 ) then
          do k=1,levr
            write(6,
     &      "(/,2x,'level = ',i3,2x,'hour = ',f7.1,/)") k,fhour
            write(6,1)
    1       format                                                       &  
     &      (3x,'( i, j)',3x,                                            &
     &       'hadv t  ','vadv t  ',' lnps   ',' vdif t ',' hdif t ',     &
     &       ' conv   ','shalcnv ',' lrgscl ',' shwrad ',                &
     &       ' lhwrad ','   tv   ',' impadj ',                           &
     &       ' latbnd ','spectrun','mdl dt  ',' sumrhs ')
            do i=1,npdim
              ii=mod(1+(i-1)*nskip,igrd1)
              jj=(1+(i-1)*nskip)/igrd1+1
              sumrhs=0
              do n=1,nt-1
                sumrhs=sumrhs+prtbgt(1,i,k,n)
              enddo
              write(6,                                                   &  
     &        "(3x,'(',i2,',',i2,')',1x,nt (6pf8.1),7pf8.1)")            &
     &        ii,jj,(prtbgt(1,i,k,n),n=1,nt),sumrhs
            enddo
          enddo
        endif
#endif
! q
#ifdef Q
        if( numvar.eq.2 .and. ivar(numvar).ne.0 ) then
          do k=1,levr
            write(6,                                                     &
     &      "(/,2x,'level = ',i3,2x,'hour = ',f7.1,/)") k,fhour  
            write(6,2)
    2       format                                                       &
     &      (3x,'( i, j)',3x,                                            &
     &      'hadv q  ',' vadv q ',                                       &
     &      'vdiff   ','hdiff   ','convect ','shacnv  ','lrg-cnv ',      &
     &      ' latbnd ','spectrun',' mdl ddq',' sumrhs ')
            do i=1,npdim
              ii=mod(1+(i-1)*nskip,igrd1)
              jj=(1+(i-1)*nskip)/igrd1+1
              sumrhs=0
              do n=1,nq-1
                sumrhs=sumrhs+prtbgt(2,i,k,n)
              enddo
              write(6,                                                   & 
     &        "(3x,'(',i2,',',i2,')',1x,nq (11pf8.1),7pf8.1)")           &
     &        ii,jj,(prtbgt(2,i,k,n),n=1,nq),sumrhs
            enddo
          enddo
        endif
#endif
! u
#ifdef U
        if( numvar.eq.3 .and. ivar(numvar).ne.0 ) then
          do k=1,levr
            write(6,                                                     & 
     &      "(/,2x,'level = ',i3,2x,'hour = ',f7.1,/)") k,fhour
            write(6,3)
    3       format                                                       &
     &      (3x,'( i, j)',3x,                                            &
     &      'hadv u  ',' vadv u ','corilis ','pr grad ',                 &
     &      'vdiff   ','hdiff   ','grwdrag ','imp adj  ',                &
     &      ' latbnd ','spectrun',' mdl ddu',' sumrhs ')
            do i=1,npdim
              ii=mod(1+(i-1)*nskip,igrd1)
              jj=(1+(i-1)*nskip)/igrd1+1
              sumrhs=0
              do n=1,nu-1
                sumrhs=sumrhs+prtbgt(3,i,k,n)
              enddo
              write(6,                                                   &  
     &        "(3x,'(',i2,',',i2,')',1x,nu (7pf8.1),7pf8.1)")            &
     &        ii,jj,(prtbgt(3,i,k,n),n=1,nu),sumrhs
            enddo
          enddo
        endif
#endif
! v
#ifdef V
        if( numvar.eq.4 .and. ivar(numvar).ne.0 ) then
          do k=1,levr
            write(6,                                                     &   
     &      "(/,2x,'level = ',i3,2x,'hour = ',f7.1,/)") k,fhour          &
            write(6,4)
    4        format                                                      &
     &      (3x,'( i, j)',3x,                                            &
     &      'hadv v  ',' vadv v ','corilis ','pr grad ',                 &
     &      'vdiff   ','hdiff   ','grwdrag ','imp adj  ',                &
     &      ' latbnd ','spectrun',' mdl ddv',' sumrhs ')
            do i=1,npdim
              ii=mod(1+(i-1)*nskip,igrd1)
              jj=(1+(i-1)*nskip)/igrd1+1
              sumrhs=0
              do n=1,nv-1
                sumrhs=sumrhs+prtbgt(4,i,k,n)
              enddo
              write(6,                                                   & 
     &        "(3x,'(',i2,',',i2,')',1x,nv (7pf8.1),7pf8.1)")            &
     &        ii,jj,(prtbgt(4,i,k,n),n=1,nv),sumrhs
            enddo
          enddo
        endif
#endif
! p
#ifdef P
        if( numvar.eq.5 .and. ivar(numvar).ne.0 ) then
          write(6,                                                       &   
     &    "(/,2x,'level = ',i3,2x,'hour = ',f7.1,/)") k,fhour
          write(6,5)
    5        format                                                      &
     &    (3x,'( i, j)',3x,                                              &
     &    'hadv p  ',' div p  ',                                         &
     &    'imp adj  ',                                                   &
     &    ' latbnd ','spectrun',' mdl ddp',' sumrhs ')
          do i=1,npdim
            ii=mod(1+(i-1)*nskip,igrd1)
            jj=(1+(i-1)*nskip)/igrd1+1
        sumrhs=0
            do n=1,np-1
              sumrhs=sumrhs+prtbgt(5,i,1,n)
            enddo
            write(6,                                                     &   
     &      "(3x,'(',i2,',',i2,')',1x,np (7pf8.1),7pf8.1)")              &
     &      ii,jj,(prtbgt(5,i,1,n),n=1,np),sumrhs
          enddo
        endif
#endif
!
!
#ifdef A
      enddo
#endif
!
#ifdef A
      return
#endif
      end
