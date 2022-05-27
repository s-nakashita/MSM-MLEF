/*---------------------------------------------------------------------------*/
/*
  FILE     : retry_rexec.h
  PURPOSE  : Provide retry capability for rexec and rcmd API.
  USAGE    : Replace origional rexec/rcmd with retry_rexec/retry_rcmd.
             by "#define rexec retry_rexec" and "#define rcmd retry_rcmd".
  HISTORY  :
    May-27-1999   C.P.DZEN   Origional
*/
/*---------------------------------------------------------------------------*/
int retry_rexec
(char **ahost,u_short inport,char *user,char *passwd,char *cmd,int *fd2p);
/*---------------------------------------------------------------------------*/
int retry_rcmd
(char **ahost,u_short inport,char *locuser,char *remuser,char *cmd,int *fd2p);
/*---------------------------------------------------------------------------*/
#define  rexec    retry_rexec
#define  rcmd     retry_rcmd 
/*---------------------------------------------------------------------------*/
