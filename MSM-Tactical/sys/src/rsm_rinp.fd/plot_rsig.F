        program plot_rsig
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
      real(4), allocatable :: us(:,:)
!
! --- namelist ---
      integer	imax,jmax
! --- others ---
      integer   i,j,k,step,all_step,out_step,method,ini
      real	filta,filtb,time
      character	chr*4,string*100
       imax=361
       jmax=265


      allocate(us(imax,jmax))
      print *,' after allocation '

       open ( 65,file='r_sig_f00', form="unformatted")
       read(65)
       read(65)
       do i=1,1000
        read(65,end=999)((us(k,j),k=1,imax),j=1,jmax)
        write(chr,'(a1,i3.3)') 'h',i
        call maxmin(us,imax*jmax,1,1,1,chr)
        if ( i.eq.1) then
         call output_grads(imax,jmax,us,'first','uus 0 99 static u ')
        else
         write(string,"(a,a)") chr,' 0 99 integrated h'
         call output_grads(imax,jmax,us,'contd',string)
        endif
       enddo
 999   continue
       write(string,"(a,a)") chr,' 0 99 integrated h'
       call output_grads(imax,jmax,us,'lastd',string)

       close(65)
        stop
        end
