/*---------------------------------------------------------------------------*\
 *                                                                           *
 * FILE    : ufs_dms.h                                                       *
 * PURPOSE : Header for DMS module to support UFS platform                   *
 *                                                                           *
\*---------------------------------------------------------------------------*/
#define     MAX_LOCK_RETRY   5
/* -------------------------------------------------------------------- */
int ufs_get_configure(char *dmsfile,UFS *ufs);
int ufs_set_configure(char *dmsfile,UFS *ufs);
/* -------------------------------------------------------------------- */
int ufs_init(void (*sig_handler)());
int ufs_exit(int exitno);
/* -------------------------------------------------------------------- */
int ufs_dbcrt(char *db_name,char *cfgstr);
int ufs_dbdlt(char *db_name);
int ufs_flst(char *db_name,DMST *flstbuf,int *flstnum);
/* -------------------------------------------------------------------- */
int ufs_fchk(char *dmsfile);
int ufs_frog(char *dmsfile);
int ufs_fcrt(char *dmsfile,char *cfgstr);
int ufs_fdlt(char *dmsf_name);
int ufs_fopn(char *dmsfile,u_int access_mode,DMS *dmsp);
/* -------------------------------------------------------------------- */
int ufs_rget(DMS *dmsp,DMST *keyp,DMST *datp);
int ufs_rput(DMS *dmsp,DMST *keyp,DMST *datp);
int ufs_rlst(DMS *dmsp,DMST *keyp,char *keybuf,int *keynum);
int ufs_rdlt(DMS *dmsp,DMST *keyp);
int ufs_fcls(DMS *dmsp);
/* -------------------------------------------------------------------- */
int ufs_conf(DMS *dmsp);
/* -------------------------------------------------------------------- */
int ufs_ckcfg(char *cfgname,char *mode,char *cfgstr);
/* -------------------------------------------------------------------- */

