#ifndef _NET_DMS_H_
#define _NET_DMS_H_
/*---------------------------------------------------------------------------*\
 *                                                                           *
 * Program File : net_dms.h                                                  *
 * PURPOSE : Header for DMS module to support NET platform                   *
 *                                                                           *
\*---------------------------------------------------------------------------*/

/* ------------------------------------------------------------------------- */
typedef struct net_struct
{ char  rhost[32];
  char  ruser[32];
  int   netfd;
  int   rmtfd;
} NET ;
/* ------------------------------------------------------------------------- */
enum DMS_NET_COMMAND
{ DMS_NET_COMMAND_MIN=0x0,
  NET_INIT,  NET_EXIT,
  NET_DBCRT, NET_DBDLT,  NET_FLST,
  NET_FCHK,  NET_FROG,  NET_FCRT,  NET_FDLT,  NET_FOPN,
  NET_RGET,  NET_RPUT,  NET_RDLT,  NET_RLST,  NET_FCLS,
  NET_OPTN,
  NET_TEST,
  DMS_NET_COMMAND_MAX
};
/* ------------------------------------------------------------------------- */
int net_init(void (*sig_handler)());
int net_exit(int exitno);
/* ------------------------------------------------------------------------- */
int net_dbcrt(char *db_name,char *cfgstr);
int net_dbdlt(char *db_name);
int net_flst(char *db_name,DMST *flstbuf,int *flstnum);
/* ------------------------------------------------------------------------- */
int net_fchk(char *dmsfile);
int net_frog(char *dmsfile);
int net_fcrt(char *dmsfile,char *cfgstr);
int net_fdlt(char *dmsfile);
int net_fopn(char *dmsfile,u_int access_mode,DMS *dmsp);
/* ------------------------------------------------------------------------- */
int net_rget(DMS *dmsp,DMST *keyp,DMST *datp);
int net_rput(DMS *dmsp,DMST *keyp,DMST *datp);
int net_rdlt(DMS *dmsp,DMST *keyp);
int net_rlst(DMS *dmsp,DMST *keyp,char *keybuf,int *keynum);
int net_fcls(DMS *dmsp);
/* ------------------------------------------------------------------------- */
int net_conf(DMS *dmsp);
/* ------------------------------------------------------------------------- */
/* +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* ------------------------------------------------------------------------- */
#endif
