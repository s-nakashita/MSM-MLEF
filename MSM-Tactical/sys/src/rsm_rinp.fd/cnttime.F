#include <define.h>
       module cnttime
                                                                                
       use istack

       implicit none
       integer (kind=4) :: number_of_timer,counter=0
       integer (kind=4) :: imype
                                                                                
       type recd_type
           character (len=20) :: name
           integer (kind=8) :: step
#ifdef IBMSP
           real (kind=16) :: tsum
           real (kind=16) ::  stime
#else
           real (kind=8) :: tsum
           real (kind=8) ::  stime
#endif
       end type recd_type
       type(recd_type), dimension(:), allocatable :: timer
                                                                                
       type(stack) int_stack
       namelist/namcnttime/ number_of_timer
                                                                                
!       public timer
       private timer, counter, cnttime_print
       public cnttime_init,cnttime_on, cnttime_off,cnttime_final
                                                                                
!following does not work on mpxlf90 compiler -- 20060609
!#define MP

!#ifdef MP
!#include "comrmpi.h"
!#endif

       contains
                                                                                
       subroutine cnttime_init
!initialize cnttime paras

!          read(5,namcnttime)
!          write(6,namcnttime)
!try simple for test
          number_of_timer=100
          allocate(timer(number_of_timer))
                                                                                
          int_stack=istack_new()
                                                                                
       end subroutine cnttime_init

       subroutine cnttime_search (iname, icount, found)
          character (len=*) :: iname
          integer (kind=4) :: icount,i
          logical :: found
 
!simple way
!   found=any(timer(1:size(timer))%name==iname)
          icount=0
          found=.false.
          do i=1,number_of_timer
             if (timer(i)%name == iname ) then
                found=.true.
                icount=i
                exit
             end if
         enddo
       end subroutine cnttime_search

       subroutine cnttime_on(iname)
! start cnt procedure computing time
          character (len=*) :: iname
          integer (kind=4) :: itimer=0
          integer (kind=4),dimension(1:8) :: tvalue
          real (kind=8) timef
          logical :: found
                                                                                
          call cnttime_search(iname,itimer, found)
          if ( found ) then
            timer(itimer)%step=timer(itimer)%step + 1
            int_stack=istack_push(int_stack,itimer)
          else
            counter=counter + 1
            itimer=counter
            timer(itimer)=recd_type(iname,1,0.,0.)
            int_stack=istack_push(int_stack,itimer)
          end if

!compute julian date?
#ifdef IBMSP
          timer(itimer)%stime =timef()
#endif

       end subroutine cnttime_on
                                                                                
       subroutine cnttime_off(iname)
                                                                                
          character (len=*) :: iname
#ifdef IBMSP
          real (kind=16) :: etime=0
#else
          real (kind=8) :: etime=0
#endif
          integer (kind=4) :: itimer=0
          integer (kind=4),dimension(1:8) :: tvalue
          real (kind=8) timef
                                                                                
#ifdef IBMSP
          etime =timef()
#endif
                                                                                
          itimer=istack_peek(int_stack)
          int_stack=istack_pop(int_stack)
          if ( timer(itimer)%name == iname ) then
            timer(itimer)%tsum=timer(itimer)%tsum+                      &
     &            (etime-timer(itimer)%stime)/1000.
            timer(itimer)%stime=0.
          else 
            print *,"ERROR in cnttime, wrong nesting"
            stop
          endif
       end subroutine cnttime_off
                                                                                
       subroutine cnttime_print(iname)
! print tsum for iname
          character (len=*) :: iname
          integer (kind=4) :: itimer=0
          logical :: found
                                                                                
          call cnttime_search(iname,itimer, found)
          if ( found ) then
            print *,"(a,a,a,f8.1,a,i8)","cnttime for ",iname," tsum=",  &
     &           timer(itimer)%tsum, "step=", timer(itimer)%step
          else
            print *,"no ",iname, "is found in time counter!"
          end if
       end subroutine cnttime_print
                                                                                
       subroutine cnttime_final

       integer (kind=4) :: err, ierr
       integer (kind=4) :: i,j

#ifdef MP
       real (kind=16) :: tmmax, tmmin
       real (kind=16), dimension(:), allocatable :: trecv 

       allocate(trecv(npes))
#endif

! print out timer 
       do i =1,counter
#ifdef MP
          call mpi_gather(timer(i)%tsum,1,MPI_REAL16,                      &
     &                    trecv,1,MPI_REAL16,0,MPI_COMM_WORLD, ierr)
          if (mype == 0) then
              tmmax = maxval(trecv,MASK=trecv .lt. trecv(1))
              tmmin = minval(trecv,MASK=trecv .lt. trecv(1))
              print *,"(a,a,a,f8.1,a,f8.1,a,f8.1,a,i8)",                   &
     &          "cnttime for ",timer(i)%name,                              &  
     &          "  mype=1,tsum=",timer(i)%tsum," max tsum=",               &
                tmmax, " min tsum=", tmmin,  " step=", timer(i)%step
           endif
#else        
          print *,"(a,a,a,f8.1,a,i8)","cnttime for ",                      &
     &             timer(i)%name," tsum=",timer(i)%tsum, "step=",          &
     &             timer(i)%step
#endif
       end do

#ifdef MP
       deallocate(trecv)
#endif
!deallocate cnttime paras
          err=istack_deallocate(int_stack)
          deallocate(timer)
                                                                                
       end subroutine cnttime_final
                                                                                
       end module cnttime
