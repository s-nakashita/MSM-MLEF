#ifndef _DMS_LIB_H_
#define _DMS_LIB_H_
/*---------------------------------------------------------------------------*\
 *                                                                           *
 *  FILE    : dms_lib.h                                                      *
 *  Purpose : Header file for dms_lib.c                                      *
 *                                                                           *
\*---------------------------------------------------------------------------*/

#include <stdio.h>
/*---------------------------------------------------------------------------*/
/* #define  SERVER "/nice/shelly/dms_new/HP/bin/dms_svr" */
#define  SERVER "/package/dms/dms.v4/dms_net_svr" 
#define  FPATH  "/package/dms/bin/dmshost"     
/*---------------------------------------------------------------------------*/
typedef struct objnode { int use; char *key; void *ptr; } OBJNODE ;
typedef struct objlist { int maxsz,cursz; OBJNODE *nodes; } OBJLIST ;
/*---------------------------------------------------------------------------*/
char *GETDMSPATH();
void uppercase(char *s);
void lowercase(char *s);
void checkname(char *s);
void checkkey(char *s);
char *cutspace(char *s);
/*---------------------------------------------------------------------------*/
void resp_ioerr(int errnum);
int  get_string(int x,char *bp);
int  resp_val(long lval);
int  rcv_buff(int x, char *buff,int nbytes);
int  resp_buff(int x, char *buff,int nbytes);
/*---------------------------------------------------------------------------*/
void checksocket(int size,int option);
int  getbuf(DMST *ptr,int memsz);
/*---------------------------------------------------------------------------*/
int  checkformat(char *s);
int  dbplatform(char *dmsfile);
/*---------------------------------------------------------------------------*/
int  validuser(char *cfgdir,char *rhost,char *ruser,char *dmsfile);
/*---------------------------------------------------------------------------*/
int  chk_filename(char *dmsfile,char *dmsdb,char *logical_name);
char *get_dmsname(char *logical_name);
/*---------------------------------------------------------------------------*/
void adtfflush(FILE *out,char *pout,int number,int keylen);
/*---------------------------------------------------------------------------*/
void invsort(char *pout,int number,int keylen);
/*---------------------------------------------------------------------------*/
int  local_info(char local_host[],char local_user[]);
/*---------------------------------------------------------------------------*/
void *obj_dupx(void *obj_ptr,int obj_siz);
int   obj_init(OBJLIST *listp);
int   obj_walk(OBJLIST *listp,int (*func)(OBJNODE *) );
int   obj_indx(OBJLIST *listp,char *obj_key);
void *obj_ptrx(OBJLIST *listp,char *obj_key);
int   obj_addx(OBJLIST *listp,char *obj_key,void *obj_ptr,int obj_siz);
int   obj_delt(OBJLIST *listp,char *obj_key);
int   obj_free(OBJLIST *listp);
/*---------------------------------------------------------------------------*/
int  get_connect(char *obj,char *host,char *user,char *psw,int *pid);
char *getdmspass(char *host,char *user);
char *ngetdmspass(char *host,char *user);
char *ngetsvrpath(char *host);
/*---------------------------------------------------------------------------*/
char *getcfgname(char *dmsname);
/*---------------------------------------------------------------------------*/
int  read_maplist(char *mapfile,int  mapuse,int mapklen1,int mapklen2);
/*---------------------------------------------------------------------------*/
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
/*---------------------------------------------------------------------------*/
#endif
