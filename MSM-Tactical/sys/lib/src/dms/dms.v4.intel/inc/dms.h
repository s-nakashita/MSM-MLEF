/*---------------------------------------------------------------------------*\
 *                                                                           *
 * dms.h  -  The include file for DMS users.                                 *
 *                                                                           *
 *  This file is part of DMS, the CWB's grid data management system.         *
 *                                                                           *
 *  DMS is free software; you can redistribute it and/or modify              *
 *  it under the terms of the GNU General Public License as published by     *
 *  the Free Software Foundation; either version 2, or (at your option)      *
 *  any later version.                                                       *
 *                                                                           *
 *  DMS is distributed in the hope that it will be useful,                   *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *
 *  GNU General Public License for more details.                             *
 *                                                                           *
 *  You may contact the author by:                                           *
 *    e-mail:  dbadm@cwb.gov.tw                                              *
 *     phone:  (02)2349-1281                                                 *
 *                                                                           *
\*---------------------------------------------------------------------------*/

/* Protection for multiple includes. */
#ifndef _DMS_H_

#define _DMS_H_

/****************************************
#if defined(__uxp__) || defined(__ia64)
****************************************/
#ifndef Full_Precision

#if defined(__uxp__) || defined(HPIA64) || defined(LX64) || defined(LXSGI) || defined(AIX5)
#define Full_Precision
#endif

#endif

#ifdef __alpha
typedef unsigned int u_int;
#else
#include <sys/types.h>
#endif

#define D_LEVEL 1
#define F_LEVEL 2

/* if NEW_PROTOCOL define then use new protocol, otherwise use old protocol. */ 
/* #define NEW_PROTOCOL  */

#define MAX_DMS_FNAME    256
#define MAX_DMS_STRL     256
#define KEYCNT  3
#define MAXFILE 20
#define MAXKEYL 64
#define MAXBUFFER	153600 /* 15K=153600 */
#define max(a,b)        (a<b ? b : a)
#define min(a,b)        (a<b ? a : b)

/* DMS platform type */
typedef enum 
{
   DMS_UFS,      /* UNIX File System    */
   DMS_GDB,      /* GDBM File System    */
   DMS_IPC,      /* IPC File System     */
   DMS_NET,      /* Network File System */
   DMS_UNKNOWN   /* Unknown File System */
} PLT_ENUM;

/* typedef for dms */
struct dms_file;        typedef struct dms_file DMSFILE; 
struct dms_t;           typedef struct dms_t DMST;
struct dms_conf;        typedef struct dms_conf DMSCONF;
struct dms_type;        typedef struct dms_type DMS;
struct dms_plt;         typedef struct dms_plt DMS_PLT;
struct dms_key;         typedef struct dms_key DMS_KEY;
struct opn_file;        typedef struct opn_file OPNFILE;
struct ufs_st; 		typedef struct ufs_st UFS;
struct map_list;        typedef struct map_list MAPLIST;
struct map_st;          typedef struct map_st DMSMAP;
typedef  DMS  *DMSP;

struct dms_file 
{
   char name[128];
   u_int size;
};

struct dms_t 
{
   int size;
   char *data;
};

typedef enum 
{
   S_LOG=1,
   G_LOG,
   S_KEYLEN,
   G_KEYLEN,
   S_CONFIRM,
   G_CONFIRM,
   S_RORDER,
   G_RORDER,
   S_WORDER,
   G_WORDER,
   S_MAPFILE,
   G_MAPFILE,
   S_UNKNOWN
} CONF_ENUM;

/* DMS Option */
struct dms_conf 
{
   u_int log;
   u_int keylen;
   u_int rorder[KEYCNT];
   u_int worder[KEYCNT];
   u_int mapcnt;
   char  mapfile[KEYCNT][256];
};

/* Platform dependent API */
struct dms_plt 
{
   /* Platform name */
   char pltname[8];
   /* Platform type : 1 -- physical type, 0 -- virtual type */
   int plttype;
   /* Database Level API */
   int (*dbcrt)(char *dbname,char *cfgstr);
   int (*dbdlt)(char *dbname);
   /* File Level API */
   int (*fcrt)(char *dmsfile,char *cfgstr);
   int (*fdlt)(char *dmsfile);
   int (*fopn)(char *dmsfile,u_int access_mode,DMS *dmsp);
   int (*fcls)(DMS *dmsp);
   int (*flst)(char *dbname,DMST *flstbuf,int *number);
   int (*frog)(char *dmsfile);
   int (*fchk)(char *dmsfile);
   /* Record Level API */
   int (*rput)(DMS *dmsp,DMST *keyp,DMST *datp);
   int (*rget)(DMS *dmsp,DMST *keyp,DMST *datp);
   int (*rdlt)(DMS *dmsp,DMST *keyp);
   int (*rlst)(DMS *dmsp,DMST *keyp,char *buffer,int *keynum);
   /* Other API */
   int (*conf)(DMS *dmsp);
   int (*init)(void (*sig_handler)());
   int (*exit)(int exitno);
};

/* DMS access method description structure. */
struct dms_type 
{
   PLT_ENUM pltype;
   char     phyname[MAX_DMS_FNAME];    /* physical filename(absolute path)  */
   char     lgcname[MAX_DMS_FNAME];    /* logical filename(users define)    */
   void     *lrucache;                 /* LRU cache structure pointer       */
   void     *internal;                 /* internal information for platform */

#define ODMS_CREATE 0x00001  /* open for create */ 
#define ODMS_RDONLY 0x00002  /* open for read only */ 
#define ODMS_RDWR   0x00004  /* open for read and write */
   u_int    access;
   int      keylen;
   /* DMS API for Users */
   int (*fcls)(DMS *dmsp);
   int (*rput)(DMS *dmsp,DMST *keyp,DMST *datp);
   int (*rget)(DMS *dmsp,DMST *keyp,DMST *datp);
   int (*rdlt)(DMS *dmsp,DMST *keyp);
   int (*rlst)(DMS *dmsp,DMST *keyp,char *buffer,int *keynum);
   int (*conf)(DMS *dmsp);
   int (*init)(void (*sig_handler)());
   int (*exit)(int exitno);
};

struct dms_key
{
  int  keylen;          /* key length              */
  int  height;          /* height offset           */
  int  field;           /* field id offset         */
  int  tau;             /* TAU offset              */
  int  flap;            /* flap id offset          */
  int  time;            /* date-time-group offset  */
  int  type;            /* variable type offset    */
  int  size;            /* data size offset        */
  int  height_len;      /* length of the height    */
  int  field_len;       /* length of the field id  */
  int  tau_len;         /* length of the TAU       */
  int  flap_len;        /* length of he Flap Id    */
  int  time_len;        /* length of the DTG       */
  int  type_len;        /* length of the Type      */
  int  size_len;        /* length of the Data Size */
  int  keylevel;        /* key level              */ 
  char *keymask;        /* key mask string        */
  char *ordermask;      /* mask order string      */
}; 

struct opn_file 
{
   char dmsfile[128];
   DMS  *dmsp;
};

struct ufs_st
{ int  keylen;
  char keymask[MAXKEYL];
  char ordermask[MAXKEYL];
  int  keylvl;
  int  orderlvl;
};

struct map_list
{ 
  char  keymap1[MAXKEYL];
  char  keymap2[MAXKEYL];
};

struct map_st
{ 
  int keymaplen1;
  int keymaplen2;
  int mapline;
  MAPLIST *maplist;
};

/************ change by Shelly Hsu, 2006/06/08 ***********
#ifdef __uxp__
**********************************************************/
#if defined(__uxp__) || defined(AIX5)
#define FORMAT "FUJI"
#endif
/************
#ifdef __ia64
*************/
#ifdef HPIA64
#define FORMAT "IA64"
#endif
#ifdef __sgi
#define FORMAT "SGI"
#endif
#ifdef cray
#define FORMAT "CRAY"
#endif
#ifdef ultrix
#define FORMAT "DEC"
#endif
#ifdef  __alpha
#define FORMAT "ALPHA"
#endif

#ifdef LX64
#define FORMAT "LX64"
#else
   #ifdef  LINUX
   #define FORMAT "LINX"
   #endif
#endif

#ifdef LXSGI
#define FORMAT "LXSGI"
#endif
#ifndef FORMAT
#define FORMAT "IEEE"
#endif

#define F_FOPN  0x0001;
#define F_FCLS  0x0002;
#define INITDMST(dmst) { dmst.size=0;dmst.data=NULL; }
#define FREEDMST(dmst) { if (dmst.size != 0) free(dmst.data);INITDMST(dmst); }

#define KEYLEN    "KEYLEN"
#define KEYMASK   "KEYMASK"
#define ORDERMASK "ORDERMASK"

#define MAXSTR 128
#define MAXOPN 64

#endif /* _DMS_H_ */

