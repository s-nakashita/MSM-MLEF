        program plot_sig
!
! this program rotatee initial condition w,r,t, center of domain
! with a constant angular velocity so like a solid state rotation
! but with fluid dynamics by partial differential equation
!
!      dh/dt + u dh/dx + v dh/dy = 0
! 
! where d is partial derivative, h is tracer, u and v are static winds.
!
! The states to rotate with three time levels are hp,hc,hf  for h at
! past, current, and future.
! The steady flow are by us and vs.
! The temporary (transition) state will have subflix with t.
!
      implicit none
!
! --- arrays ---
      real(4), allocatable :: us(:)
      real(8), allocatable :: uu(:)
!
! --- namelist ---
      integer	imax,jmax,jcap,lnt22,idate(4)
! --- others ---
      integer   i,j,k,step,all_step,out_step,method,ini
      real(4)   fhours,amax,amin
      character	chr*4
       imax=192
       jmax=94
       jcap=62
       lnt22=(jcap+2)*(jcap+1)


      allocate(us(lnt22))
      print *,' after allocation '

       open ( 65,file='sigf2007030700', form="unformatted")
       read(65)
       print *,'read label'
       read(65)fhours,(idate(i),i=1,4)
       print *,'read fhour=',fhours,'idate=',(idate(i),i=1,4)
       do i=1,1000
        print *,'i=',i
        read(65,end=999)(us(j),j=1,lnt22)
        amax=us(1)
        amin=us(1)
        do j=1,lnt22
          if(amax.gt.us(j)) amax=us(j) 
          if(amin.lt.us(j)) amin=us(j) 
        enddo
        print *,'us ',i,' has max=',amax,' min=',amin
!        if ( i.eq.1) then
!         call output_grads(lnt22,1,us,'first','uus 0 99 static u ')
!        else
!      call output_grads(imax,jmax,us,'contd',chr//' 0 99 integrated h')
!        endif
       enddo
 999   continue
!      call output_grads(imax,jmax,us,'lastd',chr//' 0 99 integrated h')

       close(65)
        stop
        end
