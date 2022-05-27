#define MPIREAL mpi_real4
      include 'mpif.h'
      integer npes,ncol,nrow,mype,master,msgtag	                        & 
     &       ,myrow,mycol,comm_row,comm_column                          & 
     &       ,levstr,levlen,lonstr,lonlen,latstr,latlen	                & 
     &       ,lwvstr,lwvlen,lntstr,lntlen,lnpstr,lnplen
      integer kind_mpi
      parameter(kind_mpi=4)
      common /comrmpi/ npes,ncol,nrow,mype,master,msgtag                & 
     &               ,myrow,mycol,comm_row,comm_column                  & 
     &               ,levstr(0:npesi-1),levlen(0:npesi-1)               & 
     &               ,lonstr(0:npesi-1),lonlen(0:npesi-1)               & 
     &               ,latstr(0:npesi-1),latlen(0:npesi-1)               & 
     &               ,lwvstr(0:npesi-1),lwvlen(0:npesi-1)               & 
     &               ,lntstr(0:npesi-1),lntlen(0:npesi-1)               & 
     &               ,lnpstr(0:npesi-1),lnplen(0:npesi-1)
