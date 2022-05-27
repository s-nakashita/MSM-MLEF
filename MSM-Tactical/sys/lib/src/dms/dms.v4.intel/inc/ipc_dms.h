#ifndef _IPC_DMS_H_
#define _IPC_DMS_H_
/*---------------------------------------------------------------------------*\
 *                                                                           *
 * Program File : ipc_dms.h                                                  *
 * PURPOSE : Header for DMS module to support IPC platform                   *
 *                                                                           *
\*---------------------------------------------------------------------------*/

/* ------------------------------------------------------------------------- */
typedef struct ipc_struct
{ char  rhost[32];
  char  ruser[32];
  long  pid;
  int   ipcfd;
  int   rmtfd;
} IPC ;
/* ------------------------------------------------------------------------- */
enum DMS_IPC_COMMAND
{ DMS_IPC_COMMAND_MIN=0x0,
  IPC_INIT,  IPC_EXIT,
  IPC_DBCRT, IPC_DBDLT,  IPC_FLST,
  IPC_FCHK,  IPC_FROG,  IPC_FCRT,  IPC_FDLT,  IPC_FOPN,
  IPC_RGET,  IPC_RPUT,  IPC_RDLT,  IPC_RLST,  IPC_FCLS,
  IPC_OPTN,
  IPC_TEST,
  DMS_IPC_COMMAND_MAX
};
/* ------------------------------------------------------------------------- */
int ipc_init(void (*sig_handler)());
int ipc_exit(int exitno);
/* ------------------------------------------------------------------------- */
int ipc_dbcrt(char *db_name,char *cfgstr);
int ipc_dbdlt(char *db_name);
int ipc_flst(char *db_name,DMST *flstbuf,int *flstnum);
/* ------------------------------------------------------------------------- */
int ipc_fchk(char *dmsfile);
int ipc_frog(char *dmsfile);
int ipc_fcrt(char *dmsfile,char *cfgstr);
int ipc_fdlt(char *dmsfile);
int ipc_fopn(char *dmsfile,u_int access_mode,DMS *dmsp);
/* ------------------------------------------------------------------------- */
int ipc_rget(DMS *dmsp,DMST *keyp,DMST *datp);
int ipc_rput(DMS *dmsp,DMST *keyp,DMST *datp);
int ipc_rdlt(DMS *dmsp,DMST *keyp);
int ipc_rlst(DMS *dmsp,DMST *keyp,char *keybuf,int *keynum);
int ipc_fcls(DMS *dmsp);
/* ------------------------------------------------------------------------- */
int ipc_conf(DMS *dmsp);
/* ------------------------------------------------------------------------- */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* ------------------------------------------------------------------------- */
#endif
