/*---------------------------------------------------------------------*\
 *                                                                     *
 *  Program file: dms_api.h                                            *
 *  Purpose: To judge which platform, ufs, gdb or net, shall be used.  *
 *                                                                     *
\*---------------------------------------------------------------------*/

/* -------------------------------------------------------------------- */
int get_pltype(char *phyname,int *pltype,u_int level);
/* -------------------------------------------------------------------- */
int dms_init(void (*sig_handler)());
int dms_exit(int exitno);
/* -------------------------------------------------------------------- */
int dms_dbcrt(char *dbname,char *plt,char *phyname,char *cfgstr);
int dms_dbdlt(char *dbname,char *phyname);
int dms_flst(char *db_name,DMST *flstbuf,int *flstnum,char *physical);
/* -------------------------------------------------------------------- */
int dms_fchk(char *dmsfile,char* physical_name);
int dms_frog(char *dmsfile,char* physical_name);
int dms_fcrt(char *dmsfile,char *phyname,char *cfgstr);
int dms_fdlt(char *dmsfile,char *phyname);
int dms_fopn(char *dmsfile,u_int filemode,DMS *dmsp);
/* -------------------------------------------------------------------- */
int dms_rget(DMS *dmsp,DMST *keyp,DMST *data);
int dms_rput(DMS *dmsp,DMST *keyp,DMST *datp);
int dms_rlst(DMS *dmsp,DMST *keyset,char *keybuf,int *keynum);
int dms_rdlt(DMS *dmsp,DMST *keyp);
int dms_fcls(DMS *dmsp);
/* -------------------------------------------------------------------- */
int dms_conf(CONF_ENUM conftype,char *argument);
/* -------------------------------------------------------------------- */

