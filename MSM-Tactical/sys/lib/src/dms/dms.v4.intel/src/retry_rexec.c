/*---------------------------------------------------------------------------*/
/*
  FILE     : retry_rexec.c
  PURPOSE  : Provide retry capability for rexec and rcmd API.
  USAGE    : Replace origional rexec/rcmd with retry_rexec/retry_rcmd.
             by "#define rexec retry_rexec" and "#define rcmd retry_rcmd".
  HISTORY  :
    May-27-1999   C.P.DZEN   Origional
*/
/*---------------------------------------------------------------------------*/
int retry_rexec
(char **ahost,u_short inport,char *user,char *passwd,char *cmd,int *fd2p)
{ 
  int sleep_time=1;
  int socket_fd;

  while( sleep_time < 256 )
  { socket_fd = rexec(ahost,inport,user,passwd,cmd,fd2p);
    if( socket_fd > 0 ) return(socket_fd);
    printf("!!! Wait %d seconds for next rexec try !!!\n",sleep_time);
    sleep(sleep_time);
    sleep_time *= 2;
  }
  return(-1);
}
/*---------------------------------------------------------------------------*/
int retry_rcmd
(char **ahost,u_short inport,char *locuser,char *remuser,char *cmd,int *fd2p)
{ 
  int sleep_time=1;
  int socket_fd;

  while( sleep_time < 256 )
  { socket_fd = rcmd(ahost,inport,locuser,remuser,cmd,fd2p);
    if( socket_fd > 0 ) return(socket_fd);
    printf("!!! Wait %d seconds for next rcmd try !!!\n",sleep_time);
    sleep(sleep_time);
    sleep_time *= 2;
  }
  return(-1);
}
/*---------------------------------------------------------------------------*/
#include "retry_rexec.h"
/*---------------------------------------------------------------------------*/
