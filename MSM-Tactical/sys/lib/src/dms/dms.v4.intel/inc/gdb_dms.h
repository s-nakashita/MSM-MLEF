/*---------------------------------------------------------------------------*\
 *                                                                           *
 * Program File : gdb_api.h                                                  *
 * PURPOSE : Header for DMS module to support GDB platform                   *
 *                                                                           *
\*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/
int gdb_init(void (*sig_handler)());
int gdb_exit(int exitno);
/*---------------------------------------------------------------------------*/
int gdb_dbcrt(char *db_name,char *cfgstr);
int gdb_dbdlt(char *db_name);
int gdb_flst(char *db_name,DMST *flstbuf,int *flstnum);
/*---------------------------------------------------------------------------*/
int gdb_fchk(char *physical_name);
int gdb_frog(char *physical_name);
int gdb_fcrt(char *physical_name,char *cfgstr);
int gdb_fdlt(char *dmsf_name);
int gdb_fopn(char *physical_name,u_int access_mode,DMS *dmsp);
/*---------------------------------------------------------------------------*/
int gdb_rget(DMS *dmsp,DMST *keyp,DMST *datp);
int gdb_rput(DMS *dmsp,DMST *keyp,DMST *datp);
int gdb_rlst(DMS *dmsp,DMST *keyp,char *keybuf,int *keynum);
int gdb_rdlt(DMS *dmsp,DMST *keyp);
int gdb_fcls(DMS *dmsp);
/*---------------------------------------------------------------------------*/
int gdb_conf(DMS *dmsp);
/*---------------------------------------------------------------------------*/
int gdb_ckcfg(char *cfgname,char *mode,char *cfgstr);
/*---------------------------------------------------------------------------*/

